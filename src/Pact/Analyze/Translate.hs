{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MonadFailDesugaring        #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE ViewPatterns               #-}

module Pact.Analyze.Translate where

import           Control.Applicative        (Alternative(empty, (<|>)))
import           Control.Lens               (at, cons, makeLenses, makePrisms,
                                             view, (%~), (<&>), (?~), (^.),
                                             (^?), (.~), _1, _2)
import           Control.Monad              (MonadPlus (mzero), (>=>))
import           Control.Monad.Except       (Except, MonadError, throwError)
import           Control.Monad.Fail         (MonadFail (fail))
import           Control.Monad.Reader       (MonadReader (local),
                                             ReaderT (runReaderT))
import           Control.Monad.State.Strict (MonadState, StateT, modify',
                                             runStateT)
import           Data.Foldable              (foldl')
import qualified Data.Map                   as Map
import           Data.Map.Strict            (Map)
import           Data.Monoid                ((<>))
import qualified Data.Set                   as Set
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Thyme                 (parseTime)
import           Data.Traversable           (for)
import           Data.Type.Equality         ((:~:) (Refl))
import           Pact.Types.Lang            (Info, Literal (..), PrimType (..),
                                             Type (..))
import qualified Pact.Types.Lang            as Pact
import           Pact.Types.Typecheck       (AST, Named (Named), Node, aId,
                                             aNode, aTy, tiName, _aTy)
import qualified Pact.Types.Typecheck       as Pact
import           Pact.Types.Util            (tShow)
import           System.Locale              (defaultTimeLocale)

import           Pact.Analyze.Patterns
import           Pact.Analyze.Term
import           Pact.Analyze.Types
import           Pact.Analyze.Util

data TranslateFailure = TranslateFailure
  { _translateFailureInfo :: !Info
  , _translateFailure     :: !TranslateFailureNoLoc
  }

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
  | MissingConcreteType (Pact.Type Pact.UserType)
  | AlternativeFailures [TranslateFailureNoLoc]
  | MonadFailure String
  | NonStaticColumns (AST Node)
  | BadNegationType (AST Node)
  | BadTimeType (AST Node)
  | NonConstKey (AST Node)
  | FailedVarLookup Text
  -- For cases we don't handle yet:
  | UnhandledType (Pact.Type Pact.UserType)
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
  UnexpectedNode ast -> "Unexpected node in translation: " <> tShow ast
  MissingConcreteType ty -> "The typechecker should always produce a concrete type, but we found " <> tShow ty
  AlternativeFailures failures -> "Multiple failures: " <> T.unlines (mappend "  " . describeTranslateFailureNoLoc <$> failures)
  MonadFailure str -> "Translation failure: " <> T.pack str
  NonStaticColumns col -> "When reading only certain columns we require all columns to be concrete in order to do analysis. We found " <> tShow col
  BadNegationType node -> "Invalid: negation of a non-integer / decimal: " <> tShow node
  BadTimeType node -> "Invalid: days / hours / minutes applied to non-integer / decimal: " <> tShow node
  NonConstKey k -> "Pact can currently only analyze constant keys in objects. Found " <> tShow k
  FailedVarLookup varName -> "Failed to look up a variable (" <> varName <> "). This likely means the variable wasn't properly bound."
  UnhandledType ty -> "Found a type we don't know how to translate yet: " <> tShow ty

instance Monoid TranslateFailureNoLoc where
  mempty = AlternativeFailures []
  mappend (AlternativeFailures xs) (AlternativeFailures ys)
    = AlternativeFailures (xs `mappend` ys)
  mappend (AlternativeFailures xs) x = AlternativeFailures (x:xs)
  mappend x (AlternativeFailures xs) = AlternativeFailures (xs <> [x])
  mappend x y = AlternativeFailures [x, y]

instance Monoid TranslateFailure where
  mempty = TranslateFailure dummyInfo mempty
  TranslateFailure info1 x `mappend` TranslateFailure info2 y
    -- Note this instance is a bit odd, but `max` will find us a non-empty info
    -- if it exists (I think this is the only lawful way to do this).
    = TranslateFailure (max info1 info2) (x <> y)

mkTranslateEnv :: [Arg] -> Map Node (Text, VarId)
mkTranslateEnv = foldl'
  (\m (nm, vid, node, _ety) -> Map.insert node (nm, vid) m)
  Map.empty

data TagAllocation
  = AllocReadTag (Located (TagId, Schema))
  | AllocWriteTag (Located (TagId, Schema))
  | AllocAuthTag (Located TagId)
  | AllocVarTag (Located (VarId, Text, EType))
  deriving Show

data TranslateState
  = TranslateState
    { _tsTagAllocs :: [TagAllocation] -- "strict" WriterT isn't; so we use state
    , _tsNextTagId :: TagId
    , _tsNextVarId :: VarId
    }

makePrisms ''TagAllocation
makeLenses ''TranslateFailure
makeLenses ''TranslateState

instance HasVarId TranslateState where
  varId = tsNextVarId

newtype TranslateM a
  = TranslateM
    { unTranslateM :: ReaderT (Info, Map Node (Text, VarId))
                        (StateT TranslateState
                          (Except TranslateFailure))
                        a
    }
  deriving (Functor, Applicative, Monad, Alternative, MonadPlus,
    MonadReader (Info, Map Node (Text, VarId)), MonadState TranslateState,
    MonadError TranslateFailure)

instance MonadFail TranslateM where
  fail s = do
    info <- view _1
    throwError (TranslateFailure info (MonadFailure s))

-- | Call when entering a node to set the current context
nodeContext :: Node -> TranslateM a -> TranslateM a
nodeContext node = local (_1 .~ nodeToInfo node)

-- | Call when entering an ast node to set the current context
astContext :: AST Node -> TranslateM a -> TranslateM a
astContext ast = local (_1 .~ astToInfo ast)

writeTagAlloc :: TagAllocation -> TranslateM ()
writeTagAlloc tagAlloc = modify' $ tsTagAllocs %~ cons tagAlloc

genTagId :: TranslateM TagId
genTagId = genId tsNextTagId

tagDbAccess
  :: (Located (TagId, Schema) -> TagAllocation)
  -> Node
  -> Schema
  -> TranslateM TagId
tagDbAccess mkTagAlloc node schema = do
  tid <- genTagId
  let info = node ^. aId . Pact.tiInfo
  writeTagAlloc $ mkTagAlloc $ Located info (tid, schema)
  pure tid

tagRead :: Node -> Schema -> TranslateM TagId
tagRead = tagDbAccess AllocReadTag

tagWrite :: Node -> Schema -> TranslateM TagId
tagWrite = tagDbAccess AllocWriteTag

tagAuth :: Node -> TranslateM TagId
tagAuth node = do
  tid <- genTagId
  let info = node ^. aId . Pact.tiInfo
  writeTagAlloc $ AllocAuthTag $ Located info tid
  pure tid

tagVarBinding :: Info -> Text -> EType -> VarId -> TranslateM ()
tagVarBinding info nm ety vid = writeTagAlloc $
  AllocVarTag (Located info (vid, nm, ety))

withNewVarId :: Node -> Text -> (VarId -> TranslateM a) -> TranslateM a
withNewVarId varNode varName action = do
  vid <- genVarId
  local (_2 . at varNode ?~ (varName, vid)) (action vid)

-- Map.union is left-biased. The more explicit name makes this extra clear.
unionPreferring :: Ord k => Map k v -> Map k v -> Map k v
unionPreferring = Map.union

maybeTranslateType :: Pact.Type Pact.UserType -> Maybe EType
maybeTranslateType =
  let handleUserType :: Pact.UserType -> Maybe QType
      handleUserType (Pact.Schema _ _ fields _) =
        fmap (EObjectTy . Schema) $ sequence $ Map.fromList $ fields <&>
          \(Pact.Arg name ty _info) -> (name, maybeTranslateType ty)

  in maybeTranslateType' handleUserType >=> downcastQType

-- A helper to translate types that doesn't know how to handle user types
-- itself
maybeTranslateType'
  :: Alternative f
  => (a -> f QType)
  -> Pact.Type a
  -> f QType
maybeTranslateType' f = \case
  TyUser a         -> f a

  -- TODO(joel): understand the difference between the TyUser and TySchema cases
  TySchema Pact.TyTable _ -> pure QTable
  TySchema _ ty'   -> maybeTranslateType' f ty'

  TyPrim TyBool    -> pure $ EType TBool
  TyPrim TyDecimal -> pure $ EType TDecimal
  TyPrim TyInteger -> pure $ EType TInt
  TyPrim TyString  -> pure $ EType TStr
  TyPrim TyTime    -> pure $ EType TTime
  TyPrim TyKeySet  -> pure $ EType TKeySet

  -- Pretend any and an unknown var are the same -- we can't analyze either of
  -- them.
  -- TODO(joel): revisit this assumption
  TyVar (Pact.SchemaVar (Pact.TypeVarName "table")) -> pure QTable
  TyVar _                                           -> pure $ EType TAny
  TyAny                                             -> pure $ EType TAny

  --
  -- TODO: handle these:
  --
  TyPrim TyValue   -> empty
  TyList _         -> empty
  TyFun _          -> empty

throwError'
  :: (MonadError TranslateFailure m, MonadReader (Info, b) m)
  => TranslateFailureNoLoc -> m a
throwError' err = do
  info <- view _1
  throwError $ TranslateFailure info err

translateType
  :: (MonadError TranslateFailure m, MonadReader (Info, b) m)
  => Pact.Type Pact.UserType -> m EType
translateType = \case
  (maybeTranslateType -> Just ety) -> pure ety
  ty                               -> throwError' $ UnhandledType ty

translateArg
  :: (MonadState s m, HasVarId s, MonadError TranslateFailure m,
      MonadReader (Info, b) m)
  => Named Node
  -> m Arg
translateArg (Named nm node _) = do
  vid <- genVarId
  ety <- translateType (_aTy node)
  pure (nm, vid, node, ety)

translateSchema :: Pact.Type Pact.UserType -> TranslateM Schema
translateSchema ty = do
  ty' <- translateType ty
  case ty' of
    EType _primTy    -> throwError' $ NotConvertibleToSchema ty
    EObjectTy schema -> pure schema

translateBody :: [AST Node] -> TranslateM ETerm
translateBody = \case
  []       -> do
    info <- view _1
    throwError $ TranslateFailure info EmptyBody
  [ast]    -> translateNode ast
  ast:asts -> do
    ast'  <- translateNode ast
    asts' <- translateBody asts
    pure $ case asts' of
      ETerm   astsT ty -> ETerm   (Sequence ast' astsT) ty
      EObject astsO ty -> EObject (Sequence ast' astsO) ty

translateObjBinding
  :: [(Named Node, AST Node)]
  -> Schema
  -> [AST Node]
  -> ETerm
  -> TranslateM ETerm
translateObjBinding bindingsA schema bodyA rhsT = do
  (bindings :: [(String, EType, (Node, Text, VarId))]) <- for bindingsA $
    \(Named unmungedVarName varNode _, colAst) -> do
      let varName = varNode ^. aId.tiName
          varInfo = varNode ^. aId . Pact.tiInfo
      varType <- translateType (_aTy varNode)
      vid     <- genVarId
      tagVarBinding varInfo unmungedVarName varType vid
      case colAst of
        AST_StringLit colName ->
          pure (T.unpack colName, varType, (varNode, varName, vid))
        _ -> nodeContext varNode $ throwError' $ NonStringLitInBinding colAst

  bindingId <- genVarId
  let freshVar = Var "binding" bindingId

  let translateLet :: Term a -> Term a
      translateLet innerBody = Let "binding" bindingId rhsT $
        -- NOTE: *left* fold for proper shadowing/overlapping name semantics:
        foldl'
          (\body (colName, varType, (_varNode, varName, vid)) ->
            let colTerm = lit colName
            in Let varName vid
              (case varType of
                 EType ty ->
                   ETerm   (At schema colTerm freshVar varType) ty
                 EObjectTy sch ->
                   EObject (At schema colTerm freshVar varType) sch)
              body)
          innerBody
          bindings

      nodeToNameVid = Map.fromList $
        (\(_, _, (node', name, vid)) -> (node', (name, vid))) <$> bindings

  fmap (mapETerm translateLet) $
    local (_2 %~ unionPreferring nodeToNameVid) $
      translateBody bodyA

translateNode :: AST Node -> TranslateM ETerm
translateNode astNode = astContext astNode $ case astNode of
  AST_Let _ [] body -> translateBody body

  AST_Let node ((Named unmungedVarName varNode _, rhsNode):bindingsRest) body -> do
    rhsETerm <- translateNode rhsNode
    let varName = varNode ^. aId.tiName
    withNewVarId varNode varName $ \vid -> do
      --
      -- TODO: do we only want to allow subsequent bindings to reference
      --       earlier ones if we know it's let* rather than let? or has this
      --       been enforced by earlier stages for us?
      --

      let varInfo = varNode ^. aId . Pact.tiInfo
          varType = etermEType rhsETerm

      tagVarBinding varInfo unmungedVarName varType vid

      body' <- translateNode $ AST_Let node bindingsRest body
      pure $ case body' of
        ETerm   bodyTm bodyTy -> ETerm   (Let varName vid rhsETerm bodyTm) bodyTy
        EObject bodyTm bodyTy -> EObject (Let varName vid rhsETerm bodyTm) bodyTy

  AST_InlinedApp body -> translateBody body

  AST_Var node -> do
    Just (varName, vid) <- view (_2 . at node)
    ty      <- translateType (_aTy node)
    pure $ case ty of
      EType ty'        -> ETerm (Var varName vid) ty'
      EObjectTy schema -> EObject (Var varName vid) schema

  -- Int
  AST_NegativeLit l -> case l of
    LInteger i -> pure $ ETerm (IntUnaryArithOp Negate (lit i)) TInt
    LDecimal d -> pure $ ETerm (DecUnaryArithOp Negate (lit (mkDecimal d))) TDecimal
    _          -> throwError' $ BadNegationType astNode

  AST_Lit l -> case l of
    LInteger i -> pure $ ETerm (lit i) TInt
    LBool b    -> pure $ ETerm (lit b) TBool
    LString s  -> pure $ ETerm (lit $ T.unpack s) TStr
    LDecimal d -> pure $ ETerm (lit (mkDecimal d)) TDecimal
    LTime t    -> pure $ ETerm (lit (mkTime t)) TTime

  AST_NegativeVar node -> do
    Just (name, vid) <- view (_2 . at node)
    EType ty <- translateType (_aTy node)
    case ty of
      TInt     -> pure (ETerm (IntUnaryArithOp Negate (Var name vid)) TInt)
      TDecimal -> pure (ETerm (DecUnaryArithOp Negate (Var name vid)) TDecimal)
      _        -> throwError' $ BadNegationType astNode

  AST_Enforce _ cond -> do
    ETerm condTerm TBool <- translateNode cond
    pure $ ETerm (Enforce condTerm) TBool

  AST_Format formatStr vars -> do
    ETerm formatStr' TStr <- translateNode formatStr
    vars' <- for vars translateNode
    pure $ ETerm (Format formatStr' vars') TStr

  AST_FormatTime formatStr time -> do
    ETerm formatStr' TStr <- translateNode formatStr
    ETerm time' TTime     <- translateNode time
    pure $ ETerm (FormatTime formatStr' time') TStr

  AST_ParseTime formatStr timeStr -> do
    ETerm formatStr' TStr <- translateNode formatStr
    ETerm timeStr' TStr   <- translateNode timeStr
    pure $ ETerm (ParseTime (Just formatStr') timeStr') TTime

  AST_Time timeStr -> do
    ETerm timeStr' TStr <- translateNode timeStr
    pure $ ETerm (ParseTime Nothing timeStr') TTime

  AST_Hash val -> do
    val' <- translateNode val
    pure $ ETerm (Hash val') TStr

  AST_ReadKeyset nameA -> do
    ETerm nameT TStr <- translateNode nameA
    return $ ETerm (ReadKeySet nameT) TKeySet

  AST_EnforceKeyset ksA
    | ksA ^? aNode.aTy == Just (TyPrim TyString)
    -> do
      ETerm ksnT TStr <- translateNode ksA
      tid <- tagAuth $ ksA ^. aNode
      return $ ETerm (Enforce (NameAuthorized tid ksnT)) TBool

  AST_EnforceKeyset ksA
    | ksA ^? aNode.aTy == Just (TyPrim TyKeySet)
    -> do
      ETerm ksT TKeySet <- translateNode ksA
      tid <- tagAuth $ ksA ^. aNode
      return $ ETerm (Enforce (KsAuthorized tid ksT)) TBool

  AST_Days days -> do
    ETerm days' daysTy <- translateNode days
    case daysTy of
      TInt     -> pure $ ETerm (IntArithOp Mul (60 * 60 * 24) days') TInt
      TDecimal -> pure $ ETerm (DecArithOp Mul (60 * 60 * 24) days') TDecimal
      _        -> throwError' $ BadTimeType astNode

  AST_Hours hours -> do
    ETerm hours' hoursTy <- translateNode hours
    case hoursTy of
      TInt     -> pure $ ETerm (IntArithOp Mul (60 * 60) hours') TInt
      TDecimal -> pure $ ETerm (DecArithOp Mul (60 * 60) hours') TDecimal
      _        -> throwError' $ BadTimeType astNode

  AST_Minutes minutes -> do
    ETerm minutes' minutesTy <- translateNode minutes
    case minutesTy of
      TInt     -> pure $ ETerm (IntArithOp Mul 60 minutes') TInt
      TDecimal -> pure $ ETerm (DecArithOp Mul 60 minutes') TDecimal
      _        -> throwError' $ BadTimeType astNode

  AST_NFun _node "time" [AST_Lit (LString timeLit)]
    | Just timeLit'
      <- parseTime defaultTimeLocale Pact.simpleISO8601 (T.unpack timeLit)
    -> pure $ ETerm (lit (mkTime timeLit')) TTime

  AST_NFun_Basic fn args ->
    let mkComparison :: TranslateM ETerm
        mkComparison = case args of
          [a, b] -> do
            ETerm a' ta <- translateNode a
            ETerm b' tb <- translateNode b
            op <- case fn of
              ">"  -> pure Gt
              "<"  -> pure Lt
              ">=" -> pure Gte
              "<=" -> pure Lte
              "="  -> pure Eq
              "!=" -> pure Neq
              _    -> throwError' $ MalformedComparison fn args
            case typeEq ta tb of
              Just Refl -> pure $ ETerm (Comparison op a' b') TBool
              _         -> throwError' (TypeMismatch (EType ta) (EType tb))
          _ -> throwError' $ MalformedComparison fn args

        mkLogical :: TranslateM ETerm
        mkLogical = case args of
          [a] -> do
            ETerm a' TBool <- translateNode a
            case fn of
              "not" -> pure $ ETerm (Logical NotOp [a']) TBool
              _     -> throwError' $ MalformedComparison fn args
          [a, b] -> do
            ETerm a' TBool <- translateNode a
            ETerm b' TBool <- translateNode b
            case fn of
              "and" -> pure $ ETerm (Logical AndOp [a', b']) TBool
              "or"  -> pure $ ETerm (Logical OrOp [a', b']) TBool
              _     -> throwError' $ MalformedLogicalOp fn args
          _ -> throwError' $ MalformedLogicalOp fn args

        mkArith :: TranslateM ETerm
        mkArith = case args of
          [a, b] -> do
            ETerm a' tyA <- translateNode a
            ETerm b' tyB <- translateNode b
            if
              | fn `Set.member` Set.fromList ["+", "-", "*", "/", "^"]
                -> let opFromName = \case
                         ("+" :: Text) -> Add
                         "-"           -> Sub
                         "*"           -> Mul
                         "/"           -> Div
                         "^"           -> Pow
                         "log"         -> Log
                         _             -> error "impossible"
                 in case (tyA, tyB) of
                   (TInt, TInt)         -> pure $
                     ETerm (IntArithOp (opFromName fn) a' b') TInt
                   (TDecimal, TDecimal) -> pure $
                     ETerm (DecArithOp (opFromName fn) a' b') TDecimal
                   (TInt, TDecimal)     -> pure $
                     ETerm (IntDecArithOp (opFromName fn) a' b') TDecimal
                   (TDecimal, TInt)     -> pure $
                     ETerm (DecIntArithOp (opFromName fn) a' b') TDecimal
                   _ -> throwError' $ MalformedArithOp fn args
              | otherwise -> case (tyA, tyB, fn) of
                (TDecimal, TInt, "round")   -> pure $
                  ETerm (RoundingLikeOp2 Round a' b') TDecimal
                (TDecimal, TInt, "ceiling") -> pure $
                  ETerm (RoundingLikeOp2 Ceiling a' b') TDecimal
                (TDecimal, TInt, "floor")   -> pure $
                  ETerm (RoundingLikeOp2 Floor a' b') TDecimal
                _ -> throwError' $ MalformedArithOp fn args
          [a] -> do
            ETerm a' ty <- translateNode a
            case (fn, ty) of
              ("-",    TInt) -> pure $ ETerm (IntUnaryArithOp Negate a') TInt
              ("sqrt", TInt) -> pure $ ETerm (IntUnaryArithOp Sqrt a') TInt
              ("ln",   TInt) -> pure $ ETerm (IntUnaryArithOp Ln a') TInt
              ("exp",  TInt) -> pure $ ETerm (IntUnaryArithOp Exp a') TInt
              ("abs",  TInt) -> pure $ ETerm (IntUnaryArithOp Abs a') TInt

              ("-",    TDecimal) -> pure $ ETerm (DecUnaryArithOp Negate a') TDecimal
              ("sqrt", TDecimal) -> pure $ ETerm (DecUnaryArithOp Sqrt a') TDecimal
              ("ln",   TDecimal) -> pure $ ETerm (DecUnaryArithOp Ln a') TDecimal
              ("exp",  TDecimal) -> pure $ ETerm (DecUnaryArithOp Exp a') TDecimal
              ("abs",  TDecimal) -> pure $ ETerm (DecUnaryArithOp Abs a') TDecimal

              ("round",   TDecimal) -> pure $ ETerm (RoundingLikeOp1 Round a') TInt
              ("ceiling", TDecimal) -> pure $ ETerm (RoundingLikeOp1 Ceiling a') TInt
              ("floor",   TDecimal) -> pure $ ETerm (RoundingLikeOp1 Floor a') TInt
              _         -> throwError' $ MalformedArithOp fn args
          _ -> throwError' $ MalformedArithOp fn args

        mkConcat :: TranslateM ETerm
        mkConcat = case (fn, args) of
          ("+", [a, b]) -> do
            ETerm a' TStr <- translateNode a
            ETerm b' TStr <- translateNode b
            pure (ETerm (Concat a' b') TStr)
          _ -> mzero

        mkMod :: TranslateM ETerm
        mkMod = case (fn, args) of
          ("mod", [a, b]) -> do
            ETerm a' TInt <- translateNode a
            ETerm b' TInt <- translateNode b
            pure (ETerm (ModOp a' b') TInt)
          _ -> mzero

    in mkMod <|> mkArith <|> mkComparison <|> mkLogical <|> mkConcat

  AST_NFun node name [ShortTableName tn, row, obj]
    | name `elem` ["insert", "update", "write"] -> do
    ETerm row' TStr <- translateNode row
    EObject obj' schema <- translateNode obj
    tid <- tagWrite node schema
    pure $ ETerm (Write tid (TableName (T.unpack tn)) row' obj') TStr

  AST_If _ cond tBranch fBranch -> do
    ETerm cond' TBool <- translateNode cond
    ETerm a ta <- translateNode tBranch
    ETerm b tb <- translateNode fBranch
    case typeEq ta tb of
      Just Refl -> pure $ ETerm (IfThenElse cond' a b) ta
      _         -> throwError' (BranchesDifferentTypes (EType ta) (EType tb))

  AST_NFun _node "pact-version" [] -> pure $ ETerm PactVersion TStr

  AST_WithRead node table key bindings schemaNode body -> do
    schema          <- translateSchema (_aTy schemaNode)
    ETerm key' TStr <- translateNode key
    tid             <- tagRead node schema
    let readT = EObject (Read tid (TableName (T.unpack table)) schema key') schema
    nodeContext node $
      translateObjBinding bindings schema body readT

  AST_Bind node objectA bindings schemaNode body -> do
    schema  <- translateSchema (_aTy schemaNode)
    objectT <- translateNode objectA
    nodeContext node $
      translateObjBinding bindings schema body objectT

  -- Time
  -- Tricky: seconds could be either integer or decimal
  AST_AddTime time seconds
    | seconds ^. aNode . aTy == TyPrim TyInteger ||
      seconds ^. aNode . aTy == TyPrim TyDecimal -> do
      ETerm time' TTime <- translateNode time
      seconds'          <- translateNode seconds
      pure (ETerm (AddTime time' seconds') TTime)

  AST_Read node table key -> do
    ETerm key' TStr <- translateNode key
    schema <- translateSchema (_aTy node)
    tid <- tagRead node schema
    pure (EObject (Read tid (TableName (T.unpack table)) schema key') schema)

  -- Note: this won't match if the columns are not a list literal
  AST_ReadCols node table key columns -> do
    ETerm key' TStr <- translateNode key
    (Schema fields) <- translateSchema (_aTy node)
    columns' <- fmap Set.fromList $ for columns $ \case
      AST_Lit (LString col) -> pure col
      bad                   -> throwError' (NonStaticColumns bad)
    let schema = Schema $
          Map.filterWithKey (\k _ -> k `Set.member` columns') fields

    tid <- tagRead node schema
    pure $ EObject
      (Read tid (TableName (T.unpack table)) schema key')
      schema

  AST_At node colName obj -> do
    EObject obj' schema <- translateNode obj
    ETerm colName' TStr <- translateNode colName
    ty <- translateType (_aTy node)
    pure $ case ty of
      EType ty'         -> ETerm   (At schema colName' obj' ty) ty'
      EObjectTy schema' -> EObject (At schema colName' obj' ty) schema'

  AST_Obj node kvs -> do
    kvs' <- for kvs $ \(k, v) -> do
      k' <- case k of
        AST_Lit (LString t) -> pure t
        -- TODO: support non-const keys
        _                   -> throwError' $ NonConstKey k
      ty <- translateType $ v ^. aNode . aTy
      v' <- translateNode v
      pure (k', (ty, v'))
    schema <- translateSchema (_aTy node)
    pure $ EObject (LiteralObject $ Map.fromList kvs') schema

  --
  -- TODO: more cases.
  --

  ast -> throwError' $ UnexpectedNode ast

runTranslation
  :: Info
  -> [Named Node]
  -> [AST Node]
  -> Except TranslateFailure ([Arg], ETerm, [TagAllocation])
runTranslation info pactArgs body = do
    (args, translationVid) <- runArgsTranslation
    (tm, tagAllocs) <- runBodyTranslation args translationVid
    pure (args, tm, tagAllocs)

  where
    runArgsTranslation :: Except TranslateFailure ([Arg], VarId)
    runArgsTranslation =
      -- Note we add () as a second value in the reader context because some
      -- methods require a reader in a pair.
      runStateT
        (runReaderT (traverse translateArg pactArgs) (info, ()))
        (VarId 1)

    runBodyTranslation
      :: [Arg] -> VarId -> Except TranslateFailure (ETerm, [TagAllocation])
    runBodyTranslation args nextVarId = fmap (fmap _tsTagAllocs) $
      (flip runStateT (TranslateState [] 0 nextVarId) $
        (runReaderT
          (unTranslateM (translateBody body))
          (info, mkTranslateEnv args)))
