{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}

-- | Translation from typechecked 'AST' to 'Term', while accumulating an
-- execution graph to be used during symbolic analysis and model reporting.
module Pact.Analyze.Translate where

import qualified Algebra.Graph              as Alga
import           Control.Applicative        (Alternative (empty))
import           Control.Lens               (Lens', at, cons, makeLenses, snoc,
                                             to, toListOf, use, view, zoom,
                                             (%=), (%~), (+~), (.=), (.~),
                                             (<>~), (?=), (^.), (<&>), _1, (^..))
import           Control.Monad              (join, replicateM, (>=>))
import           Control.Monad.Except       (Except, MonadError, throwError)
import           Control.Monad.Reader       (MonadReader (local),
                                             ReaderT (runReaderT))
import           Control.Monad.State.Strict (MonadState, StateT, evalStateT,
                                             modify', runStateT)
import           Data.Foldable              (foldl', for_, foldlM)
import           Data.List                  (sort)
import qualified Data.Map                   as Map
import           Data.Map.Strict            (Map)
import           Data.Maybe                 (fromMaybe, isNothing)
import           Data.Proxy                 (Proxy)
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Thyme                 (parseTime)
import           Data.Traversable           (for)
import           Data.Type.Equality         ((:~:) (Refl))
import           GHC.Natural                (Natural)
import           GHC.TypeLits
import           System.Locale              (defaultTimeLocale)

import           Pact.Types.Lang            (Info, Literal (..), Type
  (TyFun, TyPrim, TySchema, TyUser, TyVar), SchemaPartial(PartialSchema))
import qualified Pact.Types.Lang            as Pact
import           Pact.Types.Persistence     (WriteType)
import           Pact.Types.Typecheck       (AST, Named (Named), Node, aId,
                                             aNode, aTy, tiName, _aTy)
import qualified Pact.Types.Typecheck       as Pact
import           Pact.Types.Util            (tShow)

import           Pact.Analyze.Feature       hiding (TyFun, TyVar, Var, col,
                                             list, obj, str, time)
import           Pact.Analyze.Patterns
import           Pact.Analyze.Types
import           Pact.Analyze.Util

-- * Translation types

data TranslateFailure = TranslateFailure
  { _translateFailureInfo :: Info
  , _translateFailure     :: TranslateFailureNoLoc
  } deriving Show

data TranslateFailureNoLoc
  = BranchesDifferentTypes EType EType
  | NonStringLitInBinding (AST Node)
  | EmptyBody
  | MalformedArithOp Text [AST Node]
  | MalformedLogicalOp Text [AST Node]
  | MalformedComparison Text [AST Node]
  | NotConvertibleToSchema (Pact.Type Pact.UserType)
  | TypeMismatch EType EType
  | UnexpectedNode (AST Node)
  | UnexpectedPactNode (AST Node)
  | MissingConcreteType (Pact.Type Pact.UserType)
  | MonadFailure String
  | NonStaticColumns (AST Node)
  | BadNegationType (AST Node)
  | BadTimeType (AST Node)
  | FailedVarLookup Text
  | NoKeys (AST Node)
  | NoReadMsg (AST Node)
  | DeprecatedList Node
  | SimpleTypeRequired
  | TypeError Node
  | FreeVarInvariantViolation Text
  | UnhandledType Node (Pact.Type Pact.UserType)
  | SortLiteralObjError String (Existential (Core Term))
  | CapabilityNotFound CapName
  | BadPartialBind EType [String]
  | UnexpectedDefaultReadType EType EType
  deriving (Eq, Show)

describeTranslateFailureNoLoc :: TranslateFailureNoLoc -> Text
describeTranslateFailureNoLoc = \case
  BranchesDifferentTypes t1 t2 -> "two branches unexpectedly have different types: (" <> tShow t1 <> ") vs (" <> tShow t2 <> ")"
  NonStringLitInBinding ast -> "We only support analysis of binding forms (bind / with-read) binding string literals. Instead we found " <> tShow ast
  EmptyBody -> "can't translate an empty body"
  MalformedArithOp op args -> "Unsupported arithmetic op " <> op <> " with args " <> tShow args
  MalformedLogicalOp op args -> "Unsupported logical op " <> op <> " with args " <> tShow args
  MalformedComparison op args -> "Unsupported comparison op " <> op <> " with args " <> tShow args
  NotConvertibleToSchema ty -> "Expected a schema, but found " <> tShow ty
  TypeMismatch ty1 ty2 -> "Type mismatch: (" <> tShow ty1 <> ") vs (" <> tShow ty2 <> ")"
  -- Uncomment for debugging
  -- UnexpectedNode ast -> "Unexpected node in translation: " <> tShow ast
  UnexpectedNode _ast -> "Analysis doesn't support this construct yet"
  UnexpectedPactNode ast -> "Unexpected node in translation of a pact: " <> tShow ast
  MissingConcreteType ty -> "The typechecker should always produce a concrete type, but we found " <> tShow ty
  MonadFailure str -> "Translation failure: " <> T.pack str
  NonStaticColumns col -> "We require all column (field) names to be concrete in order to do analysis. We found " <> tShow col
  BadNegationType node -> "Invalid: negation of a non-integer / decimal: " <> tShow node
  BadTimeType node -> "Invalid: days / hours / minutes applied to non-integer / decimal: " <> tShow node
  FailedVarLookup varName -> "Failed to look up a variable (" <> varName <> "). This likely means the variable wasn't properly bound."
  NoKeys _node  -> "`keys` is not yet supported"
  NoReadMsg _ -> "`read-msg` is not yet supported"
  DeprecatedList node -> "Analysis doesn't support the deprecated `list` function -- please update to literal list syntax: " <> tShow node
  SimpleTypeRequired -> "Lists are currently limited to holding simply-typed objects"
  TypeError node -> "\"impossible\" post-typechecker type error in node: " <> tShow node
  FreeVarInvariantViolation msg -> msg
  UnhandledType node ty -> "Found a type we don't know how to translate yet: " <> tShow ty <> " at node: " <> tShow node
  SortLiteralObjError msg tm -> T.pack $ msg ++ show tm
  CapabilityNotFound (CapName cn) -> "Found a reference to capability that does not exist: " <> T.pack cn
  BadPartialBind (EType objTy) columnNames -> "Couldn't extract fields " <> tShow columnNames <> " from object of type " <> tShow objTy
  UnexpectedDefaultReadType (EType expected) (EType received) -> "Bad type in a `with-default-read`: we expected " <> tShow expected <> " (the type of the provided default), but saw " <> tShow received <> " (based on the fields that were bound)."

data TranslateEnv
  = TranslateEnv
    { _teInfo           :: Info
    , _teCapabilities   :: Map CapName Capability
    , _teNodeVars       :: Map Node (Munged, VarId)
    , _teRecoverability :: Recoverability
    , _teScopesEntered  :: Natural

    -- How to generate the next tag and vertex ids. Usually this is via @genId@
    -- (see @mkTranslateEnv@) but in testing these return a constant @0@.
    , _teGenTagId       :: forall m. MonadState TagId  m => m TagId
    , _teGenVertex      :: forall m. MonadState Vertex m => m Vertex
    }

mkTranslateEnv :: Info -> [Capability] -> [Arg] -> TranslateEnv
mkTranslateEnv info caps args
  = TranslateEnv info caps' nodeVars mempty 0 (genId id) (genId id)
  where
    -- NOTE: like in Check's moduleFunChecks, this assumes that toplevel
    -- function arguments are the only variables for which we do not use munged
    -- names:
    nodeVars = foldl'
      (\m (Arg nm vid node _ety) ->
        Map.insert node (coerceUnmungedToMunged nm, vid) m)
      Map.empty
      args

    caps' = Map.fromList $ caps <&> \c@(Capability _ capName) -> (capName, c)

    coerceUnmungedToMunged :: Unmunged -> Munged
    coerceUnmungedToMunged (Unmunged nm) = Munged nm

data TranslateState
  = TranslateState
    { _tsNextTagId     :: TagId
    , _tsNextVarId     :: VarId
    , _tsNextGuard     :: Guard
    , _tsGraph         :: Alga.Graph Vertex
      -- ^ The execution graph we've built so far. This is expanded upon as we
      -- translate an entire function.
    , _tsPathHead      :: Vertex
      -- ^ The "latest" vertex/current path of the graph. This starts out as
      -- the single initial vertex. it splits into two if we hit a conditional,
      -- and rejoins afterwards.
    , _tsNextVertex    :: Vertex
    , _tsEdgeEvents    :: Map Edge [TraceEvent]
      -- ^ Events added to each new 'Edge' upon creating a new 'Vertex' which
      -- closes/completes the 'Edge'.
    , _tsPendingEvents :: SnocList TraceEvent
      -- ^ Events being accumulated until the creation of the next 'Vertex'.
    , _tsCurrentPath   :: Path
      -- ^ Path to be associated with the 'Edge' formed by the creation of the
      -- next 'Vertex'.
    , _tsPathEdges     :: Map Path [Edge]
      -- ^ Graph edges corresponding to a given execution "path".
      --
      -- 'TraceSubpathStart's are emitted once for each path: at the start of
      -- an execution trace, and at the beginning of either side of a
      -- conditional. After a conditional, we resume the path from before the
      -- conditional. Either side of a conditional will contain a minimum of
      -- two edges: splitting away from the other branch, and then rejoining
      -- back to the other branch at the join point. The following program:
      --
      --     (defun test ()
      --       (if true 1 2))
      --
      -- will result in the following diagram, with six total edges (where /,
      -- \, and - are edges):
      --        .
      --       / \
      --     ->   ->
      --       \./
      --
      -- The initial edge leading into the conditional, two for each branch,
      -- and a final edge after the two branches have rejoined one another. We
      -- must have two edges for each branch so that we can unambiguously talk
      -- about either branch in our graph representation, where we only one
      -- permit one edge in a given direction between two vertices.
      --
      -- Also note that in the presence of nested conditionals, these
      -- "branch-out" and "rejoin" edges will not be contiguous in the graph on
      -- the side of the outer conditional which contains the nested
      -- conditional:
      --       ......
      --     _/  .   \_
      --      \ / \ _/
      --        \./
      --
      -- We track all of the edges for each path so that we can determine the
      -- subset of edges on the graph that form the upper bound for the edges
      -- that are reached during a particular execution trace. We say "upper
      -- bound" here because some traces will not execute entirely to the end
      -- of the program due to the use of e.g. @enforce@ and @enforce-keyset@.
      --
      -- There's one more scenario where we start subpaths: for each of the
      -- "cases" of an @enforce-one@. Here's an example with three cases:
      --
      --     (enforce-one [case-1 case-2 case-3])
      --
      --     \____        <- case-1 runs, always
      --      \___ ._     <- case-2 runs if case-1 fails
      --       \__        <- case-3 runs if case-2 fails
      --
      -- The \ edges correspond to the execution of each case. The _ edges
      -- correspond to successful exit early due to the lack of a failure.
      -- These three "success" edges all join together at the same vertex.
    , _tsFoundVars     :: [(VarId, Text, EType)]

      -- Vars representing nondeterministic choice between two branches. These
      -- are used for continuing on or rolling back in evaluation of pacts.
    , _tsStepChoices   :: [VarId]
    }

makeLenses ''TranslateFailure
makeLenses ''TranslateEnv
makeLenses ''TranslateState

instance HasVarId TranslateState where
  varId = tsNextVarId

class HasInfo e where
  envInfo :: Lens' e Info

instance HasInfo Info where
  envInfo = id

instance HasInfo TranslateEnv where
  envInfo = teInfo

newtype TranslateM a
  = TranslateM
    { unTranslateM :: ReaderT TranslateEnv
                        (StateT TranslateState
                          (Except TranslateFailure))
                        a
    }
  deriving (Functor, Applicative, Monad, MonadReader TranslateEnv,
    MonadState TranslateState, MonadError TranslateFailure)

failing :: String -> TranslateM a
failing s = do
  info <- view envInfo
  throwError (TranslateFailure info (MonadFailure s))

-- * Translation

emit :: TraceEvent -> TranslateM ()
emit event = modify' $ tsPendingEvents %~ flip snoc event

-- | Call when entering a node to set the current context
withNodeContext :: Node -> TranslateM a -> TranslateM a
withNodeContext node = local (envInfo .~ nodeToInfo node)

-- | Call when entering an ast node to set the current context
withAstContext :: AST Node -> TranslateM a -> TranslateM a
withAstContext ast = local (envInfo .~ astToInfo ast)

withNestedRecoverability :: Recoverability -> TranslateM a -> TranslateM a
withNestedRecoverability r = local $ teRecoverability <>~ r

-- | Enter a pact step or rollback.
--
-- This differs from 'withNewScope' by not tracing a return value from the
-- action (since steps don't return anything). When we're translating a step in
-- isolation, for a single step analysis, we bind variables that come from the
-- enclosing @defpact@, otherwise (when doing a whole-pact analysis), the
-- pact-level variables will already be in scope. The 'ScopeType' should be
-- 'StepScope' or 'RollbackScope'.
withNewStep :: ScopeType -> [Located Binding] -> TranslateM a -> TranslateM a
withNewStep scopeType bindings act = local (teScopesEntered +~ 1) $ do
  tid <- genTagId
  depth <- view teScopesEntered
  emit $ TracePushScope depth scopeType bindings
  res <- act
  emit $ TracePopScope depth scopeType tid $ EType SStr
  pure res

-- | Enter a new scope, binding variables.
withNewScope
  :: ScopeType
  -> [Located Binding]
  -> TranslateM ETerm
  -> TranslateM ETerm
withNewScope scopeType bindings act = local (teScopesEntered +~ 1) $ do
  tid <- genTagId
  depth <- view teScopesEntered
  emit $ TracePushScope depth scopeType bindings
  res <- mapExistential (Return tid) <$> act
  let ty = existentialType res
  emit $ TracePopScope depth scopeType tid ty
  pure res

genTagId :: TranslateM TagId
genTagId = TranslateM $ zoom tsNextTagId $ join $ view teGenTagId

nodeInfo :: Node -> Info
nodeInfo node = node ^. aId . Pact.tiInfo

startSubpath :: Path -> TranslateM ()
startSubpath p = do
  tsCurrentPath .= p
  emit $ TraceSubpathStart p

startNewSubpath :: TranslateM Path
startNewSubpath = do
  p <- Path <$> genTagId
  startSubpath p
  pure p

tagDbAccess
  :: (TableName -> ESchema -> Located TagId -> TraceEvent)
  -> TableName
  -> Node
  -> ESchema
  -> TranslateM TagId
tagDbAccess mkEvent tname node schema = do
  tid <- genTagId
  emit $ mkEvent tname schema (Located (nodeInfo node) tid)
  pure tid

tagRead :: TableName -> Node -> ESchema -> TranslateM TagId
tagRead = tagDbAccess TraceRead

tagWrite :: WriteType -> TableName -> Node -> ESchema -> TranslateM TagId
tagWrite = tagDbAccess . TraceWrite

tagYield :: EType -> TranslateM TagId
tagYield ety = do
  tid <- genTagId
  emit $ TraceYield ety tid
  pure tid

tagResume :: EType -> TranslateM TagId
tagResume ety = do
  tid <- genTagId
  emit $ TraceResume ety tid
  pure tid

tagAssert :: Node -> TranslateM TagId
tagAssert node = do
  tid <- genTagId
  recov <- view teRecoverability
  emit $ TraceAssert recov $ Located (nodeInfo node) tid
  pure tid

tagGuard :: Node -> TranslateM TagId
tagGuard node = do
  tid <- genTagId
  recov <- view teRecoverability
  emit $ TraceGuard recov $ Located (nodeInfo node) tid
  pure tid

-- Note: uses left-biased union to prefer new vars
withNodeVars :: [Named Node] -> [Located Binding] -> TranslateM a -> TranslateM a
withNodeVars namedNodes bindings = local (teNodeVars %~ Map.union nodeVars)
  where
    nodeVars :: Map Node (Munged, VarId)
    nodeVars = Map.fromList
      [ (node, (munged, vid))
      | ((Named _ node _), Located _ (Binding vid _ munged _))
          <- zip namedNodes bindings
      ]

maybeTranslateUserType
  :: Maybe (Set Text)
  -- ^ Set of keys to keep (if this is a partial schema)
  -> Pact.UserType
  -> Maybe QType
maybeTranslateUserType _ (Pact.Schema _ _ [] _) = Just $ EType $ mkSObject (SingList SNil)
maybeTranslateUserType restrictKeys (Pact.Schema a b (Pact.Arg name ty _:tys) c) = do
  subTy@(EType (SObject tys'))
    <- maybeTranslateUserType restrictKeys $ Pact.Schema a b tys c
  EType ty' <- maybeTranslateType ty

  let keepThisKey = case restrictKeys of
        Nothing  -> True
        Just set -> name `Set.member` set

  if keepThisKey
  then

    withSing ty' $ withTypeable ty' $
      case someSymbolVal (T.unpack name) of
        SomeSymbol (_ :: Proxy sym) ->
          -- we can use @SObjectUnsafe@ here instead of @mkSObject@ because we
          -- @insert@ the new column.
          Just $ EType $ SObjectUnsafe $ SingList $
            insert (SSymbol @sym) ty' (UnSingList tys')

  else Just subTy

maybeTranslateUserType' :: Pact.UserType -> Maybe EType
maybeTranslateUserType' = maybeTranslateUserType Nothing >=> downcastQType

maybeTranslateType :: Pact.Type Pact.UserType -> Maybe EType
maybeTranslateType = maybeTranslateType' Nothing >=> downcastQType

-- A helper to translate types that doesn't know how to handle user types
-- itself
maybeTranslateType'
  :: Maybe (Set Text)
  -- ^ Set of keys to keep (if this is a partial schema)
  -> Pact.Type Pact.UserType
  -> Maybe QType
maybeTranslateType' restrictKeys = \case
  -- Note: TyUser vs TySchema
  --
  -- A user type holds a schema type (@TyUser TySchema{}@). A schema type can
  -- be @TyTable@, @TyObject@, or @TyBinding@.
  TyUser a -> maybeTranslateUserType restrictKeys a

  TySchema Pact.TyTable _ _ -> pure QTable
  TySchema _ ty' (PartialSchema keys)
    -> maybeTranslateType' (Just keys) ty'
  TySchema _ ty' _
    -> maybeTranslateType' Nothing ty'

  TyPrim Pact.TyBool        -> pure $ EType SBool
  TyPrim Pact.TyDecimal     -> pure $ EType SDecimal
  TyPrim Pact.TyInteger     -> pure $ EType SInteger
  TyPrim Pact.TyString      -> pure $ EType SStr
  TyPrim Pact.TyTime        -> pure $ EType STime
  TyPrim (Pact.TyGuard _)   -> pure $ EType SGuard

  TyVar (Pact.SchemaVar (Pact.TypeVarName "table")) -> pure QTable

  -- Pretend any and an unknown var are the same -- we can't analyze either of
  -- them.
  -- TODO(joel): revisit this assumption
  TyVar _                                           -> pure $ EType SAny
  Pact.TyAny                                        -> pure $ EType SAny

  Pact.TyList a -> do
    ty <- maybeTranslateType' Nothing a
    case ty of
      EType ty' -> pure $ EType $ SList ty'
      _         -> empty

  TyFun _             -> empty

throwError'
  :: (MonadError TranslateFailure m, MonadReader r m, HasInfo r)
  => TranslateFailureNoLoc -> m a
throwError' err = do
  info <- view envInfo
  throwError $ TranslateFailure info err

-- | Generates a new 'Vertex'. Does *not* add this new 'Vertex' to the graph.
genVertex :: TranslateM Vertex
genVertex = TranslateM $ zoom tsNextVertex $ join $ view teGenVertex

-- | Flushes-out events accumulated for the current edge of the execution path.
flushEvents :: TranslateM [TraceEvent]
flushEvents = use tsPendingEvents >>= \case
  ConsList pathEvents -> do
    tsPendingEvents .= mempty
    pure pathEvents
  _ -> failing "Pattern match failure"

addPathEdge :: Path -> Edge -> TranslateM ()
addPathEdge path e =
  tsPathEdges.at path %= pure . cons e . fromMaybe []

extendPathTo :: Vertex -> TranslateM ()
extendPathTo toV = do
  path <- use tsCurrentPath
  v    <- use tsPathHead
  tsGraph %= Alga.overlay (Alga.edge v toV)
  edgeTrace <- flushEvents
  let e = (v, toV)
  tsEdgeEvents.at e ?= edgeTrace
  addPathEdge path e
  tsPathHead .= toV

-- | Extends the previous path head to a new 'Vertex', flushing accumulated
-- events to 'tsEdgeEvents'.
extendPath :: TranslateM Vertex
extendPath = do
  v' <- genVertex
  extendPathTo v'
  pure v'

-- | Extends multiple separate paths to a single join point. Assumes that each
-- 'Vertex' was created via 'extendPath' before invocation, and thus
-- 'tsPendingEvents' is currently empty.
joinPaths :: [(Vertex, Path)] -> TranslateM ()
joinPaths branches = do
  let vs = map fst branches
  v' <- genVertex
  tsPathHead .= v'
  tsGraph %= Alga.overlay (Alga.vertices vs `Alga.connect` pure v')
  for_ branches $ \(v, path) -> do
    isNewPath <- use $ tsPathEdges.at path.to isNothing
    let rejoinEdge = (v, v')
    tsEdgeEvents.at rejoinEdge ?= [TraceSubpathStart path | isNewPath]
    addPathEdge path rejoinEdge

-- | When we bind a subset of fields in a type, we need to determine the sub-
-- schema type.
typeOfPartialBind
  :: SingTy ('TyObject schema) -> [(Named Node, AST Node)] -> TranslateM EType
typeOfPartialBind objTy bindings = do
  columnNames <- for bindings $ \case
    (_, Pact.Prim _ (Pact.PrimLit (LString name))) -> pure $ T.unpack name
    (_, binding) -> throwError' $ NonStaticColumns binding

  -- The object keys are sorted so we must sort our binding names to match them
  -- in the correct order
  typeOfPartialBind' objTy (sort columnNames)
    ?? BadPartialBind (EType objTy) columnNames

typeOfPartialBind'
  :: SingTy ('TyObject schema) -> [String] -> Maybe EType
typeOfPartialBind' SObjectNil []
  = Just $ EType SObjectNil
typeOfPartialBind' SObjectNil _
  = Nothing
typeOfPartialBind' SObjectCons{} []
  = Just $ EType SObjectNil
typeOfPartialBind' (SObjectCons k v schema') (name:names)
  | symbolVal k == name
  = do EType (SObject subschema)
         <- typeOfPartialBind' (SObjectUnsafe schema') names
       pure $ EType $ SObjectCons k v subschema
  | otherwise
  = typeOfPartialBind' (SObjectUnsafe schema') (name:names)
typeOfPartialBind' _ _
  = vacuousMatch "pattern synonyms lead to an erroneous warning here"

translateType
  :: (MonadError TranslateFailure m, MonadReader r m, HasInfo r)
  => Node -> m EType
translateType node = case _aTy node of
  (maybeTranslateType -> Just ety) -> pure ety
  ty                               -> throwError' $ UnhandledType node ty

translateArg
  :: ( MonadState s m
     , HasVarId s
     , MonadReader r m
     , HasInfo r
     , MonadError TranslateFailure m
     )
  => Named Node
  -> m Arg
translateArg (Named nm node _) = do
  vid <- genVarId
  ety <- translateType node
  pure (Arg (Unmunged nm) vid node ety)

translateBinding :: Named Node -> TranslateM (Located Binding)
translateBinding (Named unmunged' node _) = do
  vid <- genVarId
  let munged = node ^. aId.tiName.to Munged
      info   = node ^. aId . Pact.tiInfo
  varType <- translateType node
  pure $ Located info $ Binding vid (Unmunged unmunged') munged varType

translateBody :: [AST Node] -> TranslateM ETerm
translateBody = \case
  []       -> do
    info <- view envInfo
    throwError $ TranslateFailure info EmptyBody
  [ast]    -> translateNode ast
  ast:asts -> do
    someExpr      <- translateNode ast
    Some ty exprs <- translateBody asts
    pure $ Some ty $ Sequence someExpr exprs

-- Pact translation:
--
-- We translate steps in the order they'll be eventually evaluated, meaning we
-- first translate each step, then each rollback. In the diagram we translate
-- s1, s2, s3, r2, r1. Note that the c edges have no computation / translation.
--
--      src          sink
--       +      /---->+
--       |      |     ^
--    s1 |      |     |
--       |      | r1  |
--       v  c2  |     |
--       +----->+     |
--       |      ^     |
--    s2 |      |     |
--       |      | r2  |
--       v  c3  |     |
--       +----->+     |
--       |            |
--    s3 |            |
--       |            |
--       v            |
--       +------------/
--
-- This first diagram is for a pact with the following structure:
--
--     (defpact
--       (step-with-rollback s1 r1)
--       (step-with-rollback s2 r2)
--       (step s3))
--
-- Note the numbering scheme:
-- * steps are numbered how you might expect
-- * rollbacks are numbered to match their corresponding step
-- * cancels are numbered to match the step they're an alternative to
--
-- This means that if there are n step edges, there are:
-- * n-1 cancel edges
-- * from 0 to n-1 (inclusive) rollback edges
--
-- Furthermore, there are:
-- * n vertices following a step (including the sink)
-- * from 0 to n-1 (inclusive) vertices preceding a rollback
--
-- What if we drop the first rollback?
--
--     (defpact
--       (step s1)
--       (step-with-rollback s2 r2)
--       (step s3))
--
-- The corresponding graph ends up looking like:
--
--      src    c2    sink
--       +    /------>+
--       |   /       /^
--    s1 |  /       / |
--       | /       /  |
--       v/       /   |
--       +       /    |
--       |      /     |
--    s2 |      |     |
--       |      | r2  |
--       v  c3  |     |
--       +----->+     |
--       |            |
--    s3 |            |
--       |            |
--       v            |
--       +------------/
translatePact :: [AST Node] -> TranslateM [PactStep]
translatePact nodes = do
  preStepsPath    <- use tsCurrentPath
  protoSteps      <- go True nodes
  postSnVertex    <- use tsPathHead
  snPath          <- use tsCurrentPath
  postLastCancelV <- genVertex

  (sinkV, cancels, rollbacks) <- foldlM
    (\(rightV, cancels, rollbacks) (_step, leftV, mRollback) -> do
      tsPathHead .= leftV
      emit TraceReset
      cancel <- (,) <$> startNewSubpath <*> genVarId
      emit $ TraceCancel $ _pathTag $ fst cancel
      extendPathTo rightV
      case mRollback of
        Nothing
          -> pure (rightV, cancel : cancels, Nothing : rollbacks)
        Just rollbackA -> withNewStep RollbackScope [] $ do
          rollback <- (,)
            <$> startNewSubpath
            <*> translateNode rollbackA
          postRollback <- extendPath
          pure (postRollback, cancel : cancels, Just rollback : rollbacks)
      )
    (postLastCancelV, [], [])
    -- all but the last step, in reverse order
    (tail $ reverse protoSteps)

  let steps = zipWith3
        (\(Step exec p e _ _) mCancel mRb -> Step exec p e mCancel mRb)
        (protoSteps ^.. traverse . _1)
        (Nothing : fmap Just cancels)
        (rollbacks <> [Nothing])

  tsStepChoices .= fmap snd cancels

  -- connect sink
  tsPathHead    .= postSnVertex
  tsCurrentPath .= snPath
  extendPathTo sinkV

  -- restore initial path
  tsCurrentPath .= preStepsPath

  pure steps

  where
    -- We don't generate a cancel var on the first step but we do for all
    -- subsequent steps.
    go firstStep = \case
      []       -> pure []
      ast:asts -> (:) <$> translateStep firstStep ast <*> go False asts

translateStep
  :: Bool -> AST Node -> TranslateM (PactStep, Vertex, Maybe (AST Node))
translateStep firstStep = \case
  AST_Step _node entity exec rollback _yr -> withNewStep StepScope [] $ do
    p <- if firstStep then use tsCurrentPath else startNewSubpath
    mEntity <- for entity $ \tm -> translateNode tm >>= \case
      Some SStr entity' -> pure entity'
      _ -> failing "Pattern match failure"
    Some ty exec' <- translateNode exec
    postVertex    <- extendPath
    pure (Step (exec' , ty) p mEntity Nothing Nothing, postVertex, rollback)
  astNode -> throwError' $ UnexpectedPactNode astNode

lookupCapability :: CapName -> TranslateM Capability
lookupCapability capName = do
  mCap <- view $ teCapabilities.at capName
  case mCap of
    Just cap -> pure cap
    Nothing  -> throwError' $ CapabilityNotFound capName

withTranslatedBindings
  :: [(Named Node, AST Node)]
  -> ([Located Binding] -> TranslateM ETerm)
  -> TranslateM ETerm
withTranslatedBindings (unzip -> (bindingAs, rhsAs)) k = do
  bindingTs <- traverse translateBinding bindingAs
  rhsETs <- traverse translateNode rhsAs

  let -- Wrap the 'Term' body of clauses in a 'Let' for each of the bindings
      wrapWithLets :: Term a -> Term a
      wrapWithLets tm = foldr
        (\(rhsET, Located _ (Binding vid _ (Munged munged) _)) body' ->
          Let munged vid rhsET body')
        tm
        (zip rhsETs bindingTs)

  fmap (mapExistential wrapWithLets) $
    withNodeVars bindingAs bindingTs $
      k bindingTs

translateObjBinding
  :: [(Named Node, AST Node)]
  -> SingTy ('TyObject m)
  -> [AST Node]
  -> ETerm
  -> TranslateM ETerm
translateObjBinding pairs schema bodyA rhsT = do
  let bindingAs = map fst pairs
  bindingTs <- traverse translateBinding bindingAs
  cols <- for pairs $ \case
    (_, AST_StringLit colName) ->
      pure $ T.unpack colName
    (Named _ node _, x) ->
      withNodeContext node $ throwError' $ NonStringLitInBinding x

  -- We create one synthetic binding for the object, which then only the column
  -- bindings use.
  objBindingId <- genVarId
  let objVar = CoreTerm $ Var objBindingId "binding"

  --
  -- TODO: we might want to only create a single Let here, with a binding for
  --       each column. at the moment we create a new Let for each binding. a
  --       single let would be slightly more elegant for generating assertions
  --       for tags during evaluation as well. with this current elaboration,
  --       we would generate @n@ extra/unnecessary assertions for @n@ columns
  --       (because we currently generate @n+1@ lets -- one synthetic binding
  --       for the object, and one for each column.
  --

  let wrapWithLets :: Term a -> Term a
      wrapWithLets innerBody = Let "binding" objBindingId rhsT $
        -- NOTE: *left* fold for proper shadowing/overlapping name semantics:
        foldl'
          (\body ( colName
                 , Located _ (Binding vid _ (Munged varName) (EType ty))
                 ) ->
            let colTerm = StrLit @Term colName
                rhs = Some ty $ CoreTerm $ ObjAt schema colTerm objVar
            in Let varName vid rhs body)
          innerBody
          (zip cols bindingTs)

  fmap (mapExistential wrapWithLets) $
    withNewScope ObjectScope bindingTs $
      withNodeVars bindingAs bindingTs $
        translateBody bodyA

pattern EmptyList :: Term ('TyList a)
pattern EmptyList = CoreTerm (Lit [])

translateNamedGuard :: AST Node -> TranslateM ETerm
translateNamedGuard strA = translateNode strA >>= \case
  Some SStr strT -> do
    tid <- tagGuard $ strA ^. aNode
    return $ Some SBool $ Enforce Nothing $ GuardPasses tid $ MkKsRefGuard strT
  _ -> failing "Pattern match failure"

translateGuard :: AST Node -> TranslateM ETerm
translateGuard guardA = translateNode guardA >>= \case
  Some SGuard guardT -> do
    tid <- tagGuard $ guardA ^. aNode
    return $ Some SBool $ Enforce Nothing $ GuardPasses tid guardT
  _ -> failing "Pattern match failure"

translateCapabilityApp
  :: Pact.ModuleName
  -> CapName
  -> [(Named Node, AST Node)]
  -> [AST Node]
  -> TranslateM ETerm
translateCapabilityApp modName capName bindingsA appBodyA = do
  cap <- lookupCapability capName
  withTranslatedBindings bindingsA $ \bindingTs -> do
    withNewScope (CapabilityScope modName capName) bindingTs $ do
      let vids = toListOf (traverse.located.bVid) bindingTs
      fmap (mapExistential $ Granting cap vids) $
        translateBody appBodyA

genGuard :: TranslateM Guard
genGuard = do
  g <- use tsNextGuard
  tsNextGuard %= succ
  pure g

translateNode :: AST Node -> TranslateM ETerm
translateNode astNode = withAstContext astNode $ case astNode of
  AST_Let bindings body ->
    withTranslatedBindings bindings $ \bindingTs -> do
      withNewScope LetScope bindingTs $
        translateBody body

  AST_InlinedApp modName funName bindings body -> do
    withTranslatedBindings bindings $ \bindingTs -> do
      withNewScope (FunctionScope modName funName) bindingTs $
        translateBody body

  AST_Var node -> do
    mVar     <- view $ teNodeVars.at node
    translateType node >>= \case
      EType ty -> do
        (Munged varName, vid) <- case mVar of
          Just x  -> pure x
          Nothing -> do
            vid <- genVarId
            let tcName = node ^. aId . tiName
            tsFoundVars %= cons (vid, tcName, EType ty)
            pure (Munged tcName, vid)
        pure $ Some ty $ CoreTerm $ Var vid varName

  -- Int
  AST_NegativeLit l -> case l of
    LInteger i -> pure $ Some SInteger $ inject @(Numerical Term) $
      IntUnaryArithOp Negate $ Lit' i
    LDecimal d -> pure $ Some SDecimal $ inject @(Numerical Term) $
      DecUnaryArithOp Negate $ Lit' $ fromPact decimalIso d
    _          -> throwError' $ BadNegationType astNode

  AST_Lit l -> case l of
    LInteger i -> pure $ Some SInteger $ Lit' i
    LBool b    -> pure $ Some SBool    $ Lit' b
    LString s  -> pure $ Some SStr     $ Lit' $ Str $ T.unpack s
    LDecimal d -> pure $ Some SDecimal $ Lit' $ fromPact decimalIso d
    LTime t    -> pure $ Some STime    $ Lit' $ fromPact timeIso t

  AST_NegativeVar node -> view (teNodeVars.at node) >>= \case
    Just (Munged name, vid) -> translateType node >>= \case
      EType ty -> do
        case ty of
          SInteger     -> pure $ Some SInteger $ inject $ IntUnaryArithOp Negate $
            CoreTerm $ Var vid name
          SDecimal -> pure $ Some SDecimal $ inject $ DecUnaryArithOp Negate $
            CoreTerm $ Var vid name
          _        -> throwError' $ BadNegationType astNode
    _ -> failing "Pattern match failure"

  AST_Format formatStr vars -> translateNode formatStr >>= \case
    Some SStr formatStr' -> do
      vars' <- for vars translateNode
      pure $ Some SStr $ Format formatStr' vars'
    _ -> failing "Pattern match failure"

  AST_FormatTime formatStr time -> translateNode formatStr >>= \case
    Some SStr formatStr' -> translateNode time >>= \case
      Some STime time' -> pure $ Some SStr $ FormatTime formatStr' time'
      _ -> failing "Pattern match failure"
    _ -> failing "Pattern match failure"

  AST_ParseTime formatStr timeStr -> translateNode formatStr >>= \case
    Some SStr formatStr' -> translateNode timeStr >>= \case
      Some SStr timeStr' -> pure $ Some STime $ ParseTime (Just formatStr') timeStr'
      _ -> failing "Pattern match failure"
    _ -> failing "Pattern match failure"

  AST_Time timeStr -> translateNode timeStr >>= \case
    Some SStr timeStr' -> pure $ Some STime $ ParseTime Nothing timeStr'
    _ -> failing "Pattern match failure"

  AST_Hash val -> do
    val' <- translateNode val
    pure $ Some SStr $ Hash val'

  AST_ReadKeyset nameA -> translateNode nameA >>= \case
    Some SStr nameT -> return $ Some SGuard $ ReadKeySet nameT
    _ -> failing "Pattern match failure"

  AST_ReadDecimal nameA -> translateNode nameA >>= \case
    Some SStr nameT -> return $ Some SDecimal $ ReadDecimal nameT
    _ -> failing "Pattern match failure"

  AST_ReadInteger nameA -> translateNode nameA >>= \case
    Some SStr nameT -> return $ Some SInteger $ ReadInteger nameT
    _ -> failing "Pattern match failure"

  AST_ReadString nameA -> translateNode nameA >>= \case
    Some SStr nameT -> return $ Some SStr $ ReadString nameT
    _ -> failing "Pattern match failure"

  AST_ReadMsg _ -> throwError' $ NoReadMsg astNode

  AST_PactId -> pure $ Some SStr PactId

  AST_ChainData node -> translateType node >>= \case
    EType objTy@(SObject _schema) -> pure $ Some objTy $ ChainData objTy
    _ -> failing "Pattern match failure"

  AST_KeysetRefGuard strA -> translateNode strA >>= \case
    Some SStr strT -> pure $ Some SGuard $ MkKsRefGuard strT
    _ -> failing "Pattern match failure"

  AST_CreatePactGuard strA -> translateNode strA >>= \case
    Some SStr strT -> pure $ Some SGuard $ MkPactGuard strT
    _ -> failing "Pattern match failure"

  AST_CreateUserGuard (AST_InlinedApp modName funName bindings appBodyA) -> do
    guard <- genGuard
    body <- withTranslatedBindings bindings $ \bindingTs ->
      withNewScope (FunctionScope modName funName) bindingTs $
        translateBody appBodyA
    pure $ Some SGuard $ MkUserGuard guard body

  AST_CreateModuleGuard strA -> translateNode strA >>= \case
    Some SStr strT -> pure $ Some SGuard $ MkModuleGuard strT
    _ -> failing "Pattern match failure"

  AST_Enforce _ cond -> translateNode cond >>= \case
    Some SBool condTerm -> do
      tid <- tagAssert $ cond ^. aNode
      pure $ Some SBool $ Enforce (Just tid) condTerm
    _ -> failing "Pattern match failure"

  AST_EnforceGuard_Str strA -> translateNamedGuard strA

  AST_EnforceGuard_Guard guardA -> translateGuard guardA

  AST_EnforceKeyset_Str strA -> translateNamedGuard strA

  AST_EnforceKeyset_Guard guardA -> translateGuard guardA

  AST_EnforceOne node [] -> do
    -- we just emit an event equivalent to one for `(enforce false)` in this
    -- case:
    tid <- tagAssert node
    return $ Some SBool $ EnforceOne $ Left tid

  AST_EnforceOne _ casesA@(_:_) -> do
    let n = length casesA -- invariant: n > 0
        genPath = Path <$> genTagId
    preEnforcePath <- use tsCurrentPath
    -- Generate failure and success paths for each case. We generate and tag
    -- the final failure path, but don't include it in the graph.
    pathPairs <- replicateM n ((,) <$> genPath <*> genPath)

    let (failurePaths, successPaths) = unzip pathPairs
        -- we don't start a new path for the first case -- we *always* run it:
        newPaths :: [Maybe Path]
        newPaths = Nothing : fmap Just (take (pred n) failurePaths)

        recovs :: [Recoverability]
        recovs = (Recoverable <$> take (pred n) failurePaths)
              ++ [Unrecoverable]

    (terms, vertices) <- fmap unzip $
      for (zip3 casesA newPaths recovs) $ \(caseA, mNewPath, recov) -> do
        maybe (pure ()) startSubpath mNewPath
        withNestedRecoverability recov (translateNode caseA) >>= \case
          Some SBool caseT -> do
            postVertex <- extendPath
            pure (caseT, postVertex)
          _ -> failing "Pattern match failure"

    joinPaths $ zip vertices successPaths
    tsCurrentPath .= preEnforcePath
    return $ Some SBool $ EnforceOne $ Right $ zip pathPairs terms

  AST_Days days -> do
    Some daysTy days' <- translateNode days
    case daysTy of
      SInteger -> pure $ Some SInteger $ inject $ IntArithOp Mul (60 * 60 * 24) days'
      SDecimal -> pure $ Some SDecimal $ inject $ DecArithOp Mul (60 * 60 * 24) days'
      _        -> throwError' $ BadTimeType astNode

  AST_Hours hours -> do
    Some hoursTy hours' <- translateNode hours
    case hoursTy of
      SInteger -> pure $ Some SInteger $ inject $ IntArithOp Mul (60 * 60) hours'
      SDecimal -> pure $ Some SDecimal $ inject $ DecArithOp Mul (60 * 60) hours'
      _        -> throwError' $ BadTimeType astNode

  AST_Minutes minutes -> do
    Some minutesTy minutes' <- translateNode minutes
    case minutesTy of
      SInteger -> pure $ Some SInteger $ inject $ IntArithOp Mul 60 minutes'
      SDecimal -> pure $ Some SDecimal $ inject $ DecArithOp Mul 60 minutes'
      _        -> throwError' $ BadTimeType astNode

  AST_NFun _node "time" [AST_Lit (LString timeLit)]
    | Just timeLit'
      <- parseTime defaultTimeLocale Pact.simpleISO8601 (T.unpack timeLit)
    -> pure $ Some STime $ Lit' $ fromPact timeIso timeLit'

  AST_NFun_Basic SModulus [a, b] -> translateNode a >>= \case
    Some SInteger a' -> translateNode b >>= \case
      Some SInteger b' -> pure (Some SInteger (inject $ ModOp a' b'))
      _ -> failing "Pattern match failure"
    _ -> failing "Pattern match failure"

  AST_NFun_Basic fn@(toOp comparisonOpP -> Just op) args@[a, b] -> do
    aT <- translateNode a
    bT <- translateNode b
    case (aT, bT) of

      (Some (SList SAny) EmptyList, Some (SList ty) lst) -> do
        op' <- toOp eqNeqP fn ?? MalformedComparison fn args
        pure $ Some SBool $ CoreTerm $ ListEqNeq ty op' EmptyList lst

      (Some (SList ty) lst, Some (SList SAny) EmptyList) -> do
        op' <- toOp eqNeqP fn ?? MalformedComparison fn args
        pure $ Some SBool $ CoreTerm $ ListEqNeq ty op' lst EmptyList

      (Some (SList ta) a', Some (SList tb) b') -> do
        Refl <- singEq ta tb ?? TypeMismatch (EType ta) (EType tb)
        op'  <- toOp eqNeqP fn ?? MalformedComparison fn args
        pure $ Some SBool $ inject $ ListEqNeq ta op' a' b'

      (Some ta@SObjectUnsafe{} a', Some tb@SObjectUnsafe{} b') -> do
        op' <- toOp eqNeqP fn ?? MalformedComparison fn args
        pure $ Some SBool $ inject $ ObjectEqNeq ta tb op' a' b'

      (Some ta a', Some tb b') -> do
        Refl <- singEq ta tb ?? TypeMismatch (EType ta) (EType tb)
        pure $ Some SBool $ inject $ Comparison ta op a' b'

  AST_NFun_Basic fn@(toOp comparisonOpP -> Just _) args
    -> throwError' $ MalformedComparison fn args

  -- logical: not, and, or

  AST_NFun_Basic SLogicalNegation [a] -> translateNode a >>= \case
    Some SBool a' -> pure $ Some SBool $ inject $ Logical NotOp [a']
    _ -> failing "Pattern match failure"

  AST_NFun_Basic fn args@[a, b]
    | fn == SLogicalConjunction || fn == SLogicalDisjunction -> do
      Some tyA a' <- translateNode a
      Some tyB b' <- translateNode b
      case (tyA, tyB) of
        (SBool, SBool) -> case fn of
          SLogicalConjunction -> pure $
            Some SBool $ inject $ Logical AndOp [a', b']
          SLogicalDisjunction -> pure $
            Some SBool $ inject $ Logical OrOp [a', b']
          _ -> error "impossible"
        _ -> throwError' $ MalformedLogicalOp fn args

  AST_NFun_Basic fn@(toOp logicalOpP -> Just _) args
    -> throwError' $ MalformedLogicalOp fn args

  -- arithmetic

  AST_NFun_Basic fn@(toOp roundingLikeOpP -> Just op) args@[a, b] -> do
      Some tyA a' <- translateNode a
      Some tyB b' <- translateNode b
      case (tyA, tyB, op) of
        (SDecimal, SInteger, Round)   -> pure $
          Some SDecimal $ inject $ RoundingLikeOp2 op a' b'
        (SDecimal, SInteger, Ceiling) -> pure $
          Some SDecimal $ inject $ RoundingLikeOp2 op a' b'
        (SDecimal, SInteger, Floor)   -> pure $
          Some SDecimal $ inject $ RoundingLikeOp2 op a' b'
        _ -> throwError' $ MalformedArithOp fn args

  AST_NFun_Basic fn@(toOp roundingLikeOpP -> Just op) args@[a] -> do
      Some ty a' <- translateNode a
      case ty of
        SDecimal -> pure $ Some SInteger $ inject $ RoundingLikeOp1 op a'
        _        -> throwError' $ MalformedArithOp fn args

  AST_NFun_Basic fn@(toOp unaryArithOpP -> Just op) args@[a] -> do
      Some ty a' <- translateNode a
      case ty of
        SInteger -> pure $ Some SInteger $ inject $ IntUnaryArithOp op a'
        SDecimal -> pure $ Some SDecimal $ inject $ DecUnaryArithOp op a'
        _        -> throwError' $ MalformedArithOp fn args

  --
  -- NOTE: We don't use a feature symbol here because + is overloaded across
  -- multiple (3) features.
  --
  AST_NFun node fn@"+" args@[a, b] -> translateType node >>= \case
    EType retTy -> do
      aT <- translateNode a
      bT <- translateNode b
      case (aT, bT) of
        (Some ty1@SObjectUnsafe{} o1, Some ty2@SObjectUnsafe{} o2) -> do
          -- Feature 3: object merge
          pure $ Some retTy $ inject $ ObjMerge ty1 ty2 o1 o2

        (Some (SList tyA) a', Some (SList tyB) b') -> do
          Refl <- singEq tyA tyB ?? MalformedArithOp fn args
          -- Feature 4: list concatenation
          pure $ Some (SList tyA) $ inject $ ListConcat tyA a' b'

        (Some tyA a', Some tyB b') ->
          case (tyA, tyB) of
            -- Feature 1: string concatenation
            (SStr, SStr)         -> pure $ Some SStr $ inject $ StrConcat a' b'
            -- Feature 2: arithmetic addition
            (SInteger, SInteger) -> pure $ Some SInteger $ inject $ IntArithOp Add a' b'
            (SDecimal, SDecimal) -> pure $ Some SDecimal $ inject $ DecArithOp Add a' b'
            (SInteger, SDecimal) -> pure $ Some SDecimal $ inject $ IntDecArithOp Add a' b'
            (SDecimal, SInteger) -> pure $ Some SDecimal $ inject $ DecIntArithOp Add a' b'
            _ -> throwError' $ MalformedArithOp fn args

  AST_NFun_Basic fn@(toOp arithOpP -> Just op) args@[a, b] -> do
      Some tyA a' <- translateNode a
      Some tyB b' <- translateNode b
      case (tyA, tyB) of
        (SInteger, SInteger)         -> pure $
          Some SInteger $ inject $ IntArithOp op a' b'
        (SDecimal, SDecimal) -> pure $
          Some SDecimal $ inject $ DecArithOp op a' b'
        (SInteger, SDecimal)     -> pure $
          Some SDecimal $ inject $ IntDecArithOp op a' b'
        (SDecimal, SInteger)     -> pure $
          Some SDecimal $ inject $ DecIntArithOp op a' b'
        _ -> throwError' $ MalformedArithOp fn args

  AST_NFun_Basic fn@(toOp arithOpP -> Just _) args
    -> throwError' $ MalformedArithOp fn args
  --
  -- NOTE: We don't use a feature symbol here because `length` is overloaded
  -- across multiple (3) features.
  --
  AST_NFun node "length" [a] -> do
    Some ty a' <- translateNode a
    case ty of
      SStr ->
        pure $ Some SInteger $ CoreTerm $ StrLength a'
      SList sty ->
        pure $ Some SInteger $ CoreTerm $ ListLength sty a'
      SObjectUnsafe _ ->
        pure $ Some SInteger $ CoreTerm $ ObjLength ty a'
      _ ->
        throwError' $ TypeError node

  AST_NFun node (toOp writeTypeP -> Just writeType) [ShortTableName tn, row, obj] -> translateNode row >>= \case
    Some SStr row' -> translateNode obj >>= \case
      Some objTy@(SObject schema) obj' -> do
        let tname = TableName (T.unpack tn)
        tid                              <- tagWrite writeType tname node (ESchema schema)
        pure $ Some SStr $
          Write objTy writeType tid tname row' obj'
      _ -> failing "Pattern match failure"
    _ -> failing "Pattern match failure"

  AST_If _ cond tBranch fBranch -> translateNode cond >>= \case
    Some SBool cond' -> do
      preBranchPath <- use tsCurrentPath
      postTest      <- extendPath

      truePath  <- startNewSubpath
      Some ta a <- translateNode tBranch
      postTrue  <- extendPath

      tsPathHead .= postTest -- reset to before true branch

      falsePath <- startNewSubpath
      Some tb b <- translateNode fBranch
      postFalse <- extendPath

      joinPaths [(postTrue, truePath), (postFalse, falsePath)]
      tsCurrentPath .= preBranchPath -- reset to before conditional
      Refl <- singEq ta tb ?? BranchesDifferentTypes (EType ta) (EType tb)
      pure $ Some ta $ IfThenElse ta cond' (truePath, a) (falsePath, b)
    _ -> failing "Pattern match failure"

  AST_NFun _node SStringToInteger [s] -> translateNode s >>= \case
    Some SStr s' -> pure $ Some SInteger $ CoreTerm $ StrToInt s'
    _ -> failing "Pattern match failure"

  AST_NFun _node SStringToInteger [b, s] -> translateNode b >>= \case
    Some SInteger b' -> translateNode s >>= \case
      Some SStr s' -> pure $ Some SInteger $ CoreTerm $ StrToIntBase b' s'
      _ -> failing "Pattern match failure"
    _ -> failing "Pattern match failure"

  AST_NFun _node "pact-version" [] -> pure $ Some SStr PactVersion

  AST_WithRead node table key bindings schemaNode body -> translateType schemaNode >>= \case
    EType rowTy@SObject{} -> translateNode key >>= \case
      Some SStr key' -> typeOfPartialBind rowTy bindings >>= \case
        EType partialReadTy@(SObject schema) -> do
          let tname = TableName (T.unpack table)
          tid <- tagRead tname node (ESchema schema)
          let readT = Some partialReadTy $ Read rowTy partialReadTy Nothing tid tname key'

          withNodeContext node $
            translateObjBinding bindings partialReadTy body readT
        _ -> failing "Pattern match failure"
      _ -> failing "Pattern match failure"
    _ -> failing "Pattern match failure"

  AST_WithDefaultRead node table key bindings schemaNode defaultNode body -> translateType schemaNode >>= \case
    EType rowTy@SObject{} -> translateNode key >>= \case
      Some SStr key' -> translateNode defaultNode >>= \case
        Some defTy@SObject{} def -> typeOfPartialBind rowTy bindings >>= \case
          EType subsetTy@(SObject schema) -> do
            let tname = TableName (T.unpack table)
            tid <- tagRead tname node (ESchema schema)

            -- Expect the bound type to equal the provided default object type
            case singEq subsetTy defTy of
              Nothing -> throwError' $
                UnexpectedDefaultReadType (EType defTy) (EType subsetTy)
              Just Refl -> do
                let readT = Some subsetTy $
                      Read rowTy subsetTy (Just def) tid (TableName (T.unpack table)) key'
                withNodeContext node $ translateObjBinding bindings defTy body readT
          _ -> failing "Pattern match failure"
        _ -> failing "Pattern match failure"
      _ -> failing "Pattern match failure"
    _ -> failing "Pattern match failure"

  AST_Bind node objectA bindings schemaNode body -> translateType schemaNode >>= \case
    EType objTy@SObject{} -> typeOfPartialBind objTy bindings >>= \case
      EType partialReadTy@SObject{} -> do
        objectT <- translateNode objectA
        withNodeContext node $
          translateObjBinding bindings partialReadTy body objectT
      _ -> failing "Pattern match failure"
    _ -> failing "Pattern match failure"

  AST_WithCapability (AST_InlinedApp modName funName bindings appBodyA) withBodyA -> do
    let capName = CapName $ T.unpack funName
    appET <- translateCapabilityApp modName capName bindings appBodyA
    Some ty withBodyT <- translateBody withBodyA
    pure $ Some ty $ WithCapability appET withBodyT

  AST_RequireCapability node (AST_InlinedApp _ funName bindings _) -> do
    let capName = CapName $ T.unpack funName
    cap <- lookupCapability capName
    withTranslatedBindings bindings $ \bindingTs -> do
      let vars = (\b -> (_mungedName . _bmName $ b, _bVid b)) . _located <$>
            bindingTs
      recov <- view teRecoverability
      tid <- genTagId
      emit $ TraceRequireGrant recov capName bindingTs $ Located (nodeInfo node) tid
      pure $ Some SBool $ Enforce Nothing $ HasGrant tid cap vars

  AST_ComposeCapability (AST_InlinedApp modName funName bindings appBodyA) ->
    translateCapabilityApp modName (CapName $ T.unpack funName) bindings appBodyA

  AST_AddTime time seconds
    | seconds ^. aNode . aTy == TyPrim Pact.TyInteger ||
      seconds ^. aNode . aTy == TyPrim Pact.TyDecimal -> translateNode time >>= \case
        Some STime time' -> do
          Some ty seconds' <- translateNode seconds

          case ty of
            SInteger ->
              pure $ Some STime $ CoreTerm $ IntAddTime time' seconds'
            SDecimal ->
              pure $ Some STime $ CoreTerm $ DecAddTime time' seconds'
            _ -> failing $
              "Unexpected type for seconds in add-time " ++ show ty
        _ -> failing "Pattern match failure"

  AST_Read node table key -> translateNode key >>= \case
    Some SStr key' -> translateType node >>= \case
      EType objTy@(SObject schema) -> do
        let tname = TableName (T.unpack table)
        tid <- tagRead tname node (ESchema schema)
        pure $ Some objTy $ Read objTy objTy Nothing tid tname key'
      _ -> failing "Pattern match failure"
    _ -> failing "Pattern match failure"

  -- Note: this won't match if the columns are not a list literal
  AST_ReadCols node table key columns -> translateNode key >>= \case
    Some SStr key' -> translateType node >>= \case
      -- this object type contains all the fields in the schema
      EType tableObjTy@(SObject tableSchema) -> do

        litColumns <- for columns $ \case
          AST_Lit (LString col) -> pure $ T.unpack col
          bad                   -> throwError' $ NonStaticColumns bad

        let columnSet = Set.fromList litColumns

            -- the filtered schema contains only the columns we want
            eFilteredSchema = foldrSingList
              (ESchema (SingList SNil))
              (\k ty (ESchema (SingList subSchema)) ->
                if symbolVal k `Set.member` columnSet
                then ESchema $ SingList (SCons k ty subSchema)
                else ESchema (SingList subSchema))
              tableSchema

        let tname = TableName (T.unpack table)

        case eFilteredSchema of
          ESchema filteredSchema -> do
            let filteredObjTy = mkSObject filteredSchema
            tid <- tagRead tname node (ESchema tableSchema)
            pure $ Some filteredObjTy $
              CoreTerm $ ObjTake tableObjTy
                (CoreTerm (LiteralList SStr (CoreTerm . Lit . Str <$> litColumns)))
                (Read tableObjTy tableObjTy Nothing tid tname key')
      _ -> failing "Pattern match failure"
    _ -> failing "Pattern match failure"

  AST_At node index obj -> do
    obj' <- translateNode obj
    translateType node >>= \case
      EType ty -> case obj' of
        Some objTy@SObjectUnsafe{} obj'' -> translateNode index >>= \case
          Some SStr colName -> pure $ Some ty $ CoreTerm $ ObjAt objTy colName obj''
          _ -> failing "Pattern match failure"
        Some (SList listOfTy) list -> translateNode index >>= \case
          Some SInteger index' -> pure $ Some listOfTy $ CoreTerm $ ListAt listOfTy index' list
          _ -> failing "Pattern match failure"
        _ -> throwError' $ TypeError node

  AST_Obj _node (Pact.ObjectMap kvs) -> do
    kvs' <- for (Map.toList kvs) $ \(Pact.FieldKey k, v) -> do
      v' <- translateNode v
      pure (k, v')
    mkLiteralObject (fmap throwError' . SortLiteralObjError) kvs' >>= \case
      Some objTy'@SObject{} litObj -> pure $ Some objTy' $ CoreTerm litObj
      _ -> failing "Pattern match failure"

  AST_NFun node "list" _ -> throwError' $ DeprecatedList node

  AST_List node elems -> do
    elems' <- traverse translateNode elems
    Some ty litList <- mkLiteralList elems' ?? TypeError node
    pure $ Some ty $ CoreTerm litList

  AST_Contains node val collection -> do
    Some needleTy needle <- translateNode val
    collection'          <- translateNode collection
    case collection' of
      Some SStr haystack -> case needleTy of
        SStr -> pure $ Some SBool $ CoreTerm $ StrContains needle haystack
        _    -> throwError' $ TypeError node
      Some (SList ty) haystack -> do
        Refl <- singEq needleTy ty ?? TypeError node
        pure $ Some SBool $ CoreTerm $ ListContains ty needle haystack
      Some objTy@SObject{} obj -> case needleTy of
        SStr -> pure $ Some SBool $ CoreTerm $ ObjContains objTy needle obj
        _    -> throwError' $ TypeError node
      Some _ _ -> throwError' $ TypeError node

  AST_Reverse _node list -> translateNode list >>= \case
    Some ty'@(SList elemTy) list' -> pure $ Some ty' $ CoreTerm $ ListReverse elemTy list'
    _ -> failing "Pattern match failure"

  AST_Sort _node list -> translateNode list >>= \case
    Some ty'@(SList elemTy) list' -> pure $ Some ty' $ CoreTerm $ ListSort elemTy list'
    _ -> failing "Pattern match failure"

  AST_Drop node numOrKeys list -> do
    elist       <- translateNode list
    translateType node >>= \case
      EType retTy -> case elist of
        Some ty'@(SList elemTy) list' -> translateNode numOrKeys >>= \case
          Some SInteger num -> pure $ Some ty' $ CoreTerm $ ListDrop elemTy num list'
          _ -> failing "Pattern match failure"
        Some objTy@SObject{} obj -> case retTy of
          SObject{} -> translateNode numOrKeys >>= \case
            Some (SList SStr) keys -> pure $ Some retTy $ CoreTerm $ ObjDrop objTy keys obj
            _ -> failing "Pattern match failure"
          _ -> throwError' $ TypeError node
        _ -> throwError' $ TypeError node

  AST_Take node numOrKeys list -> do
    elist       <- translateNode list
    translateType node >>= \case
      EType retTy -> case elist of
        Some ty'@(SList elemTy) list' -> translateNode numOrKeys >>= \case
          Some SInteger num -> pure $ Some ty' $ CoreTerm $ ListTake elemTy num list'
          _ -> failing "Pattern match failure"
        Some objTy@SObject{} obj -> case retTy of
          SObject{} -> translateNode numOrKeys >>= \case
            Some (SList SStr) keys -> pure $ Some retTy $ CoreTerm $ ObjTake objTy keys obj
            _ -> failing "Pattern match failure"
          _ -> throwError' $ TypeError node
        _ -> throwError' $ TypeError node

  AST_MakeList _node num a -> translateNode num >>= \case
    Some SInteger num' -> do
      Some ty       a'   <- translateNode a
      pure $ Some (SList ty) $ CoreTerm $ MakeList ty num' a'
    _ -> failing "Pattern match failure"

  AST_NFun _node SIdentity [a] -> do
    Some tya a' <- translateNode a
    pure $ Some tya $ CoreTerm $ Identity tya a'

  AST_NFun _node SConstantly [ a, b ] -> do
    Some tya a' <- translateNode a
    Some tyb b' <- translateNode b
    pure $ Some tya $ CoreTerm $ Constantly tyb a' b'

  AST_NFun _node SCompose [ f, g, a ] -> do
    Some tya a' <- translateNode a
    avarLst     <- use tsFoundVars
    tsFoundVars .= []

    Some tyb f'  <- translateNode f
    (avid, _, _) <- captureOneFreeVar

    Some tyc g'  <- translateNode g
    (bvid, _, _) <- captureOneFreeVar

    -- important: we captured a, so we need to leave it free (by restoring
    -- tsFoundVars)
    tsFoundVars .= avarLst

    pure $ Some tyc $ CoreTerm $
      Compose tya tyb tyc a' (Open avid "a" f') (Open bvid "b" g')

  AST_NFun node SMap [ fun, l ] -> do
    expectNoFreeVars
    Some bTy fun' <- translateNode fun
    captureOneFreeVar >>= \case
      (vid, varName, EType aType) -> translateNode l >>= \case
        Some (SList listTy) l' -> do
          Refl <- singEq listTy aType ?? TypeError node
          pure $ Some (SList bTy) $ CoreTerm $
            ListMap aType bTy (Open vid varName fun') l'
        _ -> failing "Pattern match failure"

  AST_NFun node SFilter [ fun, l ] -> do
    expectNoFreeVars
    translateNode fun >>= \case
      Some SBool fun' -> captureOneFreeVar >>= \case
        (vid, varName, EType aType) -> translateNode l >>= \case
          Some (SList listTy) l' -> do
            Refl <- singEq listTy aType ?? TypeError node
            pure $ Some (SList aType) $ CoreTerm $
              ListFilter aType (Open vid varName fun') l'
          _ -> failing "Pattern match failure"
      _ -> failing "Pattern match failure"

  AST_NFun node SFold [ fun, a, l ] -> do
    expectNoFreeVars
    Some funTy fun' <- translateNode fun

    -- Note: The order of these variables is important. `a` should be the first
    -- variable we encounter when traversing `fun` (and `b` the second) because
    -- `a` is the first argument and `b` is the second.
    --
    -- TODO(joel): this doesn't seem to follow
    --
    -- `a` encountered first, `b` will be consed on top of it, resulting in the
    -- variables coming out backwards.
    captureTwoFreeVars >>= \case
      [ (vidb, varNameb, EType tyb), (vida, varNamea, EType tya) ] -> do
        Some aTy' a' <- translateNode a
        translateNode l >>= \case
          Some (SList listTy) l' -> do
            Refl <- singEq aTy' tya   ?? TypeError node
            Refl <- singEq aTy' funTy ?? TypeError node
            Refl <- singEq listTy tyb ?? TypeError node
            pure $ Some tya $ CoreTerm $
              ListFold tya tyb (Open vida varNamea (Open vidb varNameb fun')) a' l'
          _ -> failing "Pattern match failure"
      _ -> failing "Pattern match failure"

  AST_NFun _ name [ f, g, a ]
    | name == SAndQ || name == SOrQ -> do
    expectNoFreeVars
    translateNode f >>= \case
      Some SBool f' -> do
        (fvid, fvarName, _) <- captureOneFreeVar
        translateNode g >>= \case
          Some SBool g' -> do
            (gvid, gvarName, _) <- captureOneFreeVar
            Some aTy' a' <- translateNode a
            pure $ Some SBool $ CoreTerm $ (if name == "and?" then AndQ else OrQ)
              aTy' (Open fvid fvarName f') (Open gvid gvarName g') a'
          _ -> failing "Pattern match failure"
      _ -> failing "Pattern match failure"

  AST_NFun _ SWhere [ field, fun, obj ] -> translateNode field >>= \case
    Some SStr field' -> do
      expectNoFreeVars
      translateNode fun >>= \case
        Some SBool fun' -> captureOneFreeVar >>= \case
          (vid, varName, EType freeTy) -> translateNode obj >>= \case
            Some objTy@SObject{} obj' -> pure $ Some SBool $ CoreTerm $
              Where objTy freeTy field' (Open vid varName fun') obj'
            _ -> failing "Pattern match failure"
        _ -> failing "Pattern match failure"
    _ -> failing "Pattern match failure"

  AST_NFun _ STypeof [tm] -> do
    Some ty tm' <- translateNode tm
    pure $ Some SStr $ CoreTerm $ Typeof ty tm'

  -- NOTE: we ignore the optional target chain during analysis, for now at
  -- least.
  AST_NFun node "yield" (obj : _optionalTargetChain) -> do
    Some objTy obj' <- translateNode obj
    ety <- translateType node
    tid <- tagYield ety
    pure $ Some objTy $ Yield tid obj'

  -- Translate into a resume-and-bind
  AST_Resume node bindings schemaNode body -> translateType schemaNode >>= \case
    EType objTy@SObject{} -> typeOfPartialBind objTy bindings >>= \case
      ety@(EType partialReadTy@SObject{}) -> do
        tid <- tagResume ety
        withNodeContext node $ translateObjBinding bindings partialReadTy body $
          Some partialReadTy $ Resume tid
      _ -> failing "Pattern match failure"
    _ -> failing "Pattern match failure"

  AST_NFun _ "keys" [_] -> throwError' $ NoKeys astNode

  AST_NFun _node (toOp bitwiseOpP -> Just op) args -> do
    args' <- for args $ \arg -> translateNode arg >>= \case
      Some SInteger arg' -> pure arg'
      _ -> failing "Pattern match failure"
    pure $ Some SInteger $ inject @(Numerical Term) $
      BitwiseOp op args'

  _ -> throwError' $ UnexpectedNode astNode

captureOneFreeVar :: TranslateM (VarId, Text, EType)
captureOneFreeVar = do
  vs <- use tsFoundVars
  tsFoundVars .= []
  case vs of
    [v] -> pure v
    _   -> throwError' $ FreeVarInvariantViolation $
      "unexpected vars found: " <> tShow vs

captureTwoFreeVars :: TranslateM [(VarId, Text, EType)]
captureTwoFreeVars = do
  vs <- use tsFoundVars
  tsFoundVars .= []
  case vs of
    [_, _] -> pure vs
    _      -> throwError' $ FreeVarInvariantViolation $
      "unexpected vars found: " <> tShow vs

expectNoFreeVars :: TranslateM ()
expectNoFreeVars = do
  vars <- use tsFoundVars
  case vars of
    [] -> pure ()
    _  -> throwError' $ FreeVarInvariantViolation
      "invariant violation: free variable unexpectedly found"

mkExecutionGraph :: Vertex -> Path -> TranslateState -> ExecutionGraph
mkExecutionGraph vertex0 rootPath st = ExecutionGraph
    vertex0
    rootPath
    (_tsGraph st)
    (_tsEdgeEvents st)
    (_tsPathEdges st)

runTranslation
  :: Pact.ModuleName
  -> Text
  -> Info
  -> [Capability]
  -> [Named Node]
  -> [AST Node]
  -> CheckableType
  -> Except TranslateFailure ([Arg], [VarId], ETerm, ExecutionGraph)
runTranslation modName funName info caps pactArgs body checkType = do
    (args, translationVid)     <- runArgsTranslation
    (tm, (stepChoices, graph)) <- runBodyTranslation args translationVid
    pure (args, stepChoices, tm, graph)

  where
    runArgsTranslation :: Except TranslateFailure ([Arg], VarId)
    runArgsTranslation = runStateT
      (runReaderT (traverse translateArg pactArgs) info)
      (VarId 1)

    argToBinding :: Arg -> Located Binding
    argToBinding (Arg unmunged vid node ety) =
      Located (node ^. aId . Pact.tiInfo) $
        Binding vid unmunged (node ^. aId.tiName.to Munged) ety

    runBodyTranslation
      :: [Arg]
      -> VarId
      -> Except TranslateFailure (ETerm, ([VarId], ExecutionGraph))
    runBodyTranslation args nextVarId =
      let vertex0    = 0
          nextVertex = succ vertex0
          path0      = Path 0
          nextTagId  = succ $ _pathTag path0
          nextGuard  = Guard 0
          graph0     = pure vertex0
          state0     = TranslateState nextTagId nextVarId nextGuard graph0
            vertex0 nextVertex Map.empty mempty path0 Map.empty [] []
          translation = do
            -- For our toplevel 'FunctionScope', we reuse variables we've
            -- already generated during argument translation:
            let bindingTs = fmap argToBinding args

            res <- case checkType of
              CheckDefun ->
                withNewScope (FunctionScope modName funName) bindingTs $
                  translateBody body
              CheckDefpact ->
                withNewScope (PactScope modName funName) bindingTs $
                  Some SStr . Pact <$> translatePact body
              CheckPactStep ->
                withNewStep StepScope bindingTs $
                  translateBody body
              CheckDefconst
                -> error "invariant violation: this cannot be a constant"
            _ <- extendPath -- form final edge for any remaining events
            pure res

          handleState translateState =
            ( _tsStepChoices translateState
            , mkExecutionGraph vertex0 path0 translateState
            )
      in fmap (fmap handleState) $ flip runStateT state0 $
           runReaderT (unTranslateM translation) (mkTranslateEnv info caps args)

-- | Translate a node ignoring the execution graph. This is useful in cases
-- where we don't show an execution trace. Those two places (currently) are:
-- * Translating `defconst`s for use in properties. This is for use only in
-- properties, as opposed to in execution.
-- * Translating terms for property testing. Here we don't show a trace -- we
-- just test that pact and analysis come to the same result.
--
-- TODO: is it possible for certain effects to work in `defconst` that we're
--       not permitting here? e.g. in the pact repl, it seems that at least
--       `read-keyset` works.
--
translateNodeNoGraph :: AST Node -> Except TranslateFailure ETerm
translateNodeNoGraph node =
  let vertex0        = 0
      nextVertex     = succ vertex0
      path0          = Path 0
      nextTagId      = succ $ _pathTag path0
      nextGuard      = Guard 0
      graph0         = pure vertex0
      translateState = TranslateState nextTagId 0 nextGuard graph0 vertex0
        nextVertex Map.empty mempty path0 Map.empty [] []

      translateEnv = TranslateEnv dummyInfo Map.empty Map.empty mempty 0
        (pure 0) (pure 0)

  in (`evalStateT` translateState) $
       (`runReaderT` translateEnv) $
         unTranslateM $ translateNode node

-- | Throw a translation failure when Nothing
(??)
  :: (MonadError TranslateFailure m, MonadReader r m, HasInfo r)
  => Maybe a -> TranslateFailureNoLoc -> m a
Just a  ?? _   = pure a
Nothing ?? err = throwError' err
infix 0 ??
