{-# language ConstraintKinds    #-}
{-# language DeriveAnyClass     #-}
{-# language DeriveDataTypeable #-}
{-# language DeriveGeneric      #-}
{-# language FlexibleContexts   #-}
{-# language FlexibleInstances  #-}
{-# language GADTs              #-}
{-# language LambdaCase         #-}
{-# language MultiWayIf         #-}
{-# language OverloadedStrings  #-}
{-# language PatternSynonyms    #-}
{-# language Rank2Types         #-}
{-# language RecordWildCards    #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneDeriving #-}
{-# language TemplateHaskell    #-}
{-# language TypeApplications   #-}
{-# language ViewPatterns       #-}
{-# language TupleSections      #-}
{-# language MultiParamTypeClasses #-}
{-# language UndecidableInstances #-}
{-# language InstanceSigs #-}

module Pact.Analyze.Types
  ( runCompiler
  , runCompilerDebug
  , runCompilerTest
  , runTest
  , analyzeFunction
  , AnalyzeState(..)
  , inferFun
  , Check(..)
  , DomainProperty(..)
  , Property(..)
  ) where

import Control.Arrow ((>>>))
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Except (ExceptT(..), Except, runExcept, throwError, MonadError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
import Control.Monad.State.Strict (runStateT)
import Control.Monad.Trans.RWS.Strict
import Control.Lens hiding (op, (.>))
import Data.Data
import qualified Data.Decimal as Decimal
import Data.Text (Text)
import Data.Thyme
import Pact.Typechecker hiding (debug)
import Pact.Types.Lang hiding (Term)
import Pact.Types.Runtime hiding (Term)
import Pact.Types.Typecheck hiding (Var)
import qualified Pact.Types.Typecheck as TC
import Pact.Types.Version (pactVersion)
import Pact.Repl
import Data.Aeson hiding (Object, Success, (.=))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HM
import Data.Default (def)
import Data.Traversable (for)
import GHC.Generics
import Data.SBV hiding (Satisfiable, Unsatisfiable, Unknown, ProofError, name)
import qualified Data.SBV as SBV
import qualified Data.SBV.Internals as SBVI
import qualified Data.Text as T

import Debug.Trace

-- | Low-level, untyped variable.
data AVar = AVar SBVI.SVal
  deriving (Eq, Show)

unsafeCastAVar :: AVar -> SBV a
unsafeCastAVar (AVar sval) = SBVI.SBV sval

mkAVar :: SBV a -> AVar
mkAVar (SBVI.SBV sval) = AVar sval

data AnalyzeEnv = AnalyzeEnv
  { _scope     :: Map Text AVar      -- used with 'local' in a stack fashion
  , _nameAuths :: SArray String Bool -- read-only
  } deriving Show
makeLenses ''AnalyzeEnv

newtype AnalyzeLog
  = AnalyzeLog ()

instance Monoid AnalyzeLog where
  mempty = AnalyzeLog ()
  mappend _ _ = AnalyzeLog ()

instance Mergeable AnalyzeLog where
  --
  -- NOTE: If we change the underlying representation of AnalyzeLog to a list,
  -- the default Mergeable instance for this will have the wrong semantics, as
  -- it requires that lists have the same length. We more likely want to use
  -- monoidal semantics for anything we log:
  --
  symbolicMerge _f _t = mappend

-- Checking state that is split before, and merged after, conditionals.
data LatticeAnalyzeState
  = LatticeAnalyzeState
    { _lasSucceeds :: SBool
    }
  deriving (Show, Eq, Generic, Mergeable)
makeLenses ''LatticeAnalyzeState

-- Checking state that is transferred through every computation, in-order.
newtype GlobalAnalyzeState
  = GlobalAnalyzeState ()
  deriving (Show, Eq)
makeLenses ''GlobalAnalyzeState

data AnalyzeState
  = AnalyzeState
    { _latticeState :: LatticeAnalyzeState
    , _globalState  :: GlobalAnalyzeState
    }
  deriving (Show, Eq)
makeLenses ''AnalyzeState

instance Mergeable AnalyzeState where
  -- NOTE: We discard the left global state because this is out-of-date and was
  -- already fed to the right computation -- we use the updated right global
  -- state.
  symbolicMerge force test (AnalyzeState lls _) (AnalyzeState rls rgs) =
    AnalyzeState (symbolicMerge force test lls rls) rgs

initialAnalyzeState :: AnalyzeState
initialAnalyzeState = AnalyzeState
  { _latticeState = LatticeAnalyzeState true
  , _globalState  = GlobalAnalyzeState ()
  }

succeeds :: Lens' AnalyzeState SBool
succeeds = latticeState.lasSucceeds

data AnalyzeFailure
  = MalformedArithOp ArithOp [Term Integer]
  | UnsupportedArithOp ArithOp
  | MalformedComparison Text [AST Node]
  | MalformedLogicalOp Text [AST Node]
  -- | Some translator received a node it didn't expect
  | UnexpectedNode String (AST Node)
  -- | 'translateBody' expects at least one node in a function body.
  | EmptyBody
  -- | A node we have a good reason not to handle
  | UnhandledTerm String (Term Int64)
  deriving Show

type M = RWST AnalyzeEnv AnalyzeLog AnalyzeState (Except AnalyzeFailure)

instance (Mergeable w, Mergeable a) => Mergeable (RWST r w AnalyzeState (Except e) a) where
  symbolicMerge force test left right = RWST $ \r s -> ExceptT $ Identity $
    --
    -- We explicitly propagate only the "global" portion of the state from the
    -- left to the right computation. And then the only lattice state, and not
    -- global state, is merged (per AnalyzeState's Mergeable instance.)
    --
    -- If either side fails, the entire merged computation fails.
    --
    let run act = runExcept . runRWST act r
    in do
      lTup <- run left s
      let gs = lTup ^. _2.globalState
      rTup <- run right $ s & globalState .~ gs
      return $ symbolicMerge force test lTup rTup

comparisonOperators, logicalOperators, numericalOperators :: Set Text
comparisonOperators = Set.fromList [">", "<", ">=", "<=", "=", "!="]
logicalOperators    = Set.fromList ["and", "or", "not"]
numericalOperators  = Set.fromList ["+", "-", "*", "/", "abs", "^"]

isComparison, isLogical, isNumerical :: Text -> Bool
isComparison s = Set.member s comparisonOperators
isLogical    s = Set.member s logicalOperators
isNumerical  s = Set.member s numericalOperators

ofBasicOperators :: Text -> Either Text Text
ofBasicOperators s = if isBasic then Right s else Left s
  where
    isBasic = Set.member s
      (comparisonOperators <> logicalOperators <> numericalOperators)

-- helper patterns
pattern NativeFunc :: forall a. Text -> Fun a
pattern NativeFunc f <- (FNative _ f _ _)

-- compileNode's Patterns

pattern AST_Var :: forall a. a -> AST a
pattern AST_Var var <- (TC.Var var)

pattern AST_Lit :: forall a. Literal -> AST a
pattern AST_Lit lit <- (Prim _ (PrimLit lit))

pattern AST_NegativeVar :: forall a. a -> AST a
pattern AST_NegativeVar var <- (App _ (NativeFunc "-") [AST_Var var])

pattern AST_NegativeLit :: forall a. Literal -> AST a
pattern AST_NegativeLit lit <- (App _ (NativeFunc "-") [AST_Lit lit])

pattern AST_NFun :: forall a. a -> Text -> [AST a] -> AST a
pattern AST_NFun node fn args <- (App node (NativeFunc fn) args)

pattern AST_NFun_Basic :: forall a. Text -> [AST a] -> AST a
pattern AST_NFun_Basic fn args <-
  AST_NFun _ (ofBasicOperators -> Right fn) args

pattern AST_If :: forall a. a -> AST a -> AST a -> AST a -> AST a
pattern AST_If node cond then' else' <-
  App node (NativeFunc "if") [cond, then', else']

pattern AST_Enforce :: forall a. a -> AST a -> Text -> AST a
pattern AST_Enforce node cond msg <-
  App node (NativeFunc "enforce") [cond, AST_Lit (LString msg)]

pattern AST_EnforceKeyset :: forall a. AST a -> AST a
pattern AST_EnforceKeyset ks <-
  App _node (NativeFunc "enforce-keyset") [ks] -- can be string or object

-- Unsupported currently

pattern AST_AddTime :: forall a. AST a -> AST a -> AST a
pattern AST_AddTime time seconds <- (App _ (NativeFunc "add-time") [time, seconds])

pattern AST_Days :: forall a. AST a -> AST a
pattern AST_Days days <- (App _ (NativeFunc "days") [days])

data ArithOp
  = Sub -- "-" Integer / Decimal
  | Add -- "+"
  | Mul -- "*"
  | Div -- "/"

  | Pow -- "^"
  | Sqrt -- "sqrt"
  | Mod -- "mod"
  | Log -- "log"
  | Ln -- "ln"

  | Exp -- "exp"
  | Abs -- "abs"
  | Round -- "round"
  | Ceiling -- "ceiling"
  | Floor -- "floor"

  -- Extras not necessarily in pact
  -- @Signum@ because we want (Term Integer) to be a Num instance
  | Signum
  -- @Negate@ because @-@ is overloaded in pact to mean "minus" or "negate"
  | Negate
  deriving (Show, Eq, Ord)

unsupportedArithOps :: Set ArithOp
unsupportedArithOps = Set.fromList
  -- TODO(joel) support Exp with svExp?
  [Pow, Sqrt, Log, Ln, Exp, Round, Ceiling, Floor]

data LogicalOp = AndOp | OrOp | NotOp
  deriving (Show, Eq)

data ComparisonOp = Gt | Lt | Gte | Lte | Eq | Neq
  deriving (Show, Eq)

data Term ret where
  IfThenElse     ::                        Term Bool    -> Term a   -> Term a -> Term a
  Enforce        ::                        Term Bool    ->                       Term Bool
  Sequence       :: (Show b, SymWord b) => Term b       -> Term a   ->           Term a
  Literal        ::                        SBV a        ->                       Term a
  --
  -- TODO: change read/write to use strings -> objects
  --
  Read           ::                        Term Integer ->                     Term a
  Write          :: (Show a)            => Term Integer -> Term a ->           Term ()
  Let            :: (Show a)            => String       -> Term a -> Term b -> Term b
  Var            ::                        Text         ->                     Term a
  Arith          ::                        ArithOp      -> [Term Integer]   -> Term Integer
  Comparison     :: (Show a, SymWord a) => ComparisonOp -> Term a -> Term a -> Term Bool
  Logical        :: LogicalOp -> [Term a] -> Term a
  AddTimeInt     ::                        Term Time    -> Term Integer     -> Term Time
  AddTimeDec     ::                        Term Time    -> Term Decimal     -> Term Time
  NameAuthorized ::                        Term String  ->                     Term Bool

  Concat         ::                        Term String  -> Term String      -> Term String
  PactVersion    ::                                                            Term String

  WithRead       :: (Show a) => Text -> Term String -> [Text] -> Term a -> Term a

  --
  -- TODO: figure out the object representation we use here:
  --
  -- ObjAuthorized  ::                     Term Obj     ->                     Term Bool
  --
  -- TODO: we will also want to handle cases where load a keyset object by its
  -- name, and then use the object: e.g.:
  --
  --   (defconst ADMIN_KEYSET (read-keyset "accounts-admin-keyset"))
  --
  --  and then ADMIN_KEYSET is used in the code
  --

deriving instance Show a => (Show (Term a))

instance Num (Term Integer) where
  fromInteger = Literal . fromInteger
  x + y = Arith Add [x, y]
  x * y = Arith Mul [x, y]
  abs x = Arith Abs [x]
  signum x = Arith Signum [x]
  negate x = Arith Negate [x]

data DomainProperty where
  TableWrite       :: TableName  ->             DomainProperty -- anything in table is written
  TableRead        :: TableName  ->             DomainProperty -- anything in table is read
  ColumnWrite      :: TableName  -> ColumnId -> DomainProperty -- particular column is written
  --
  CellIncrease     :: TableName  -> ColumnId -> DomainProperty -- any cell at all in col increases
  ColumnConserves  :: TableName  -> ColumnId -> DomainProperty -- sum of all changes in col is 0
  --
  KsNameAuthorized :: KeySetName ->             DomainProperty -- keyset authorized by name
  Abort            ::                           DomainProperty
  Success          ::                           DomainProperty
  --
  -- TODO: row-level keyset enforcement seems like it needs some form of
  --       unification, so that using a variable we can connect >1 domain
  --       property?
  --
  --       e.g.: forall row. RowWrite("balances", r) `Implies` RowKsEnforced(r)
  --
  --       RowKsEnforced  :: RowUid    ->            DomainProperty
  --       RowWrite       :: TableName -> RowUid  -> DomainProperty
  --
  -- TODO: Add DomainProperty/ies for constraining function arguments
  --       - e.g.: x > 10 `Implies` table_write(t0)
  --
  -- TODO: possibly allow use of input as parameter to domain properties
  --       - e.g.: column_increases_by(t0, x)     [where x is function input]
  --

data Property where
  Implies :: Property       -> Property -> Property
  Not     :: Property       ->             Property
  And     :: Property       -> Property -> Property
  Or      :: Property       -> Property -> Property
  Occurs  :: DomainProperty ->             Property

data Check where
  Satisfiable :: Property -> Check
  Valid       :: Property -> Check

newtype AstNodeOf a
  = AstNodeOf
    { unAstNodeOf :: AST Node }

type TranslateM = ReaderT (Map Node Text) (Except AnalyzeFailure)

data K a = K
  !(AstNodeOf Bool    -> a)
  !(AstNodeOf Decimal -> a)
  !(AstNodeOf Integer -> a)
  !(AstNodeOf String  -> a)
  !(AstNodeOf Time    -> a)

instance Functor K where
  fmap f (K b d i s t) = K (f . b) (f . d) (f . i) (f . s) (f . t)

-- newtype TransT b m a = TransT { unTransT :: K b -> m a }

-- instance Functor m => Functor (TransT b m) where
--   fmap f (TransT cont) = TransT $ fmap f . cont

-- instance Applicative m => Applicative (TransT b m) where
--   TransT fab <*> TransT fa = TransT $ \k -> fab k <*> fa k

-- instance Monad m => Monad (TransT b m) where
--   TransT ma >>= famb = TransT $ \k -> do
--     a <- ma k
--     unTransT (famb a) k

-- instance MonadError e m => MonadError e (TransT b m) where

-- handleB :: Applicative m => AstNodeOf Bool -> TransT a m a
-- handleB x = TransT $ \(K hb _hd _hi _hs _ht) -> pure (hb x)

-- handleD :: Applicative m => AstNodeOf Decimal -> TransT a m a
-- handleD x = TransT $ \(K _hb hd _hi _hs _ht) -> pure (hd x)

-- handleI :: Applicative m => AstNodeOf Integer -> TransT a m a
-- handleI x = TransT $ \(K _hb _hd hi _hs _ht) -> pure (hi x)

-- handleS :: Applicative m => AstNodeOf String -> TransT a m a
-- handleS x = TransT $ \(K _hb _hd _hi hs _ht) -> pure (hs x)

-- handleT :: Applicative m => AstNodeOf Time -> TransT a m a
-- handleT x = TransT $ \(K _hb _hd _hi _hs ht) -> pure (ht x)

translateNodeInt :: AstNodeOf Integer -> TranslateM (Term Integer)
translateNodeInt = unAstNodeOf >>> \case
  AST_NegativeLit (LInteger i)  -> Arith Negate . pure <$> pure (Literal (literal i))
  AST_Lit         (LInteger i)  -> pure (Literal (literal i))
  AST_NegativeVar n -> do
    name <- view (ix n)
    pure $ Arith Negate [Var name]
  AST_Var         n -> Var <$> view (ix n)
  AST_Days days -> do
    days' <- translateNodeInt (AstNodeOf days)
    pure $ Arith Mul [60 * 60 * 24, days']
  AST_AddTime time seconds
    | seconds ^. aNode . aTy == TyPrim TyInteger -> undefined

  -- TODO(joel): do this for decimal (etc) as well
  AST_NFun_Basic fn args -> case (fn, args) of
    ("-", [a, b]) -> do
      a' <- translateNodeInt (AstNodeOf a)
      b' <- translateNodeInt (AstNodeOf b)
      pure $ Arith Sub [a', b']
    ("-", [a]) -> Arith Negate . pure <$> translateNodeInt (AstNodeOf a)
    ("+", [a, b]) -> do
      a' <- translateNodeInt (AstNodeOf a)
      b' <- translateNodeInt (AstNodeOf b)
      pure $ Arith Add [a', b']
    ("*", [a, b]) -> do
      a' <- translateNodeInt (AstNodeOf a)
      b' <- translateNodeInt (AstNodeOf b)
      pure $ Arith Mul [a', b']
    ("/", [a, b]) -> do
      a' <- translateNodeInt (AstNodeOf a)
      b' <- translateNodeInt (AstNodeOf b)
      pure $ Arith Div [a', b']
    ("^", [a, b]) -> do
      a' <- translateNodeInt (AstNodeOf a)
      b' <- translateNodeInt (AstNodeOf b)
      pure $ Arith Pow [a', b']
    ("sqrt", [a]) -> Arith Sqrt . pure <$> translateNodeInt (AstNodeOf a)
    ("mod", [a, b]) -> do
      a' <- translateNodeInt (AstNodeOf a)
      b' <- translateNodeInt (AstNodeOf b)
      pure $ Arith Mod [a', b']
    ("log", [a, b]) -> do
      a' <- translateNodeInt (AstNodeOf a)
      b' <- translateNodeInt (AstNodeOf b)
      pure $ Arith Log [a', b']
    ("ln", [a]) -> Arith Ln . pure <$> translateNodeInt (AstNodeOf a)
    ("exp", [a]) -> Arith Pow . pure <$> translateNodeInt (AstNodeOf a)
    ("abs", [a]) -> Arith Abs . pure <$> translateNodeInt (AstNodeOf a)

    ("round", [a]) -> Arith Round . pure <$> translateNodeInt (AstNodeOf a)
    ("ceiling", [a]) -> Arith Ceiling . pure <$> translateNodeInt (AstNodeOf a)
    ("floor", [a]) -> Arith Floor . pure <$> translateNodeInt (AstNodeOf a)

  ast -> throwError $ UnexpectedNode "translateNodeInt" ast

translateNodeBool :: AstNodeOf Bool -> TranslateM (Term Bool)
translateNodeBool = unAstNodeOf >>> \case
  AST_Lit (LBool b)     -> pure (Literal (literal b))
  AST_Var n -> Var <$> view (ix n)

  AST_Enforce _ cond _msg -> do
    condTerm <- translateNodeBool (AstNodeOf cond)
    return $ Enforce condTerm

  AST_EnforceKeyset ks
    | ks ^? aNode.aTy == Just (TyPrim TyString)
    -> do
      ksNameTerm <- translateNodeStr (AstNodeOf ks)
      return $ Enforce $ NameAuthorized ksNameTerm

  AST_NFun_Basic fn args -> do
    let mkComparison
          :: (Show b, SymWord b)
          => (AstNodeOf b -> TranslateM (Term b))
          -> TranslateM (Term Bool)
        mkComparison translate = case (fn, args) of
          -- TODO: this could compare integer, decimal, string, or time. use the node
          -- to decide which to dispatch to
          (">", [a, b]) -> Comparison Gt
            <$> translate (AstNodeOf a)
            <*> translate (AstNodeOf b)
          ("<", [a, b]) -> Comparison Lt
            <$> translate (AstNodeOf a)
            <*> translate (AstNodeOf b)
          ("<=", [a, b]) -> Comparison Lte
            <$> translate (AstNodeOf a)
            <*> translate (AstNodeOf b)
          (">=", [a, b]) -> Comparison Gte
            <$> translate (AstNodeOf a)
            <*> translate (AstNodeOf b)
          ("=", [a, b]) -> Comparison Eq
            <$> translate (AstNodeOf a)
            <*> translate (AstNodeOf b)
          ("!=", [a, b]) -> Comparison Neq
            <$> translate (AstNodeOf a)
            <*> translate (AstNodeOf b)
          _ -> throwError $ MalformedComparison fn args

        mkLogical :: TranslateM (Term Bool)
        mkLogical = case (fn, args) of
          ("and", [a, b]) -> do
            a' <- translateNodeBool (AstNodeOf a)
            b' <- translateNodeBool (AstNodeOf b)
            pure $ Logical AndOp [a', b']
          ("or", [a, b]) -> do
            a' <- translateNodeBool (AstNodeOf a)
            b' <- translateNodeBool (AstNodeOf b)
            pure $ Logical OrOp [a', b']
          ("not", [a]) -> do
            a' <- translateNodeBool (AstNodeOf a)
            pure $ Logical NotOp [a']
          _ -> throwError $ MalformedLogicalOp fn args

    if
      -- integer, decimal, string, and time are all comparable. Use the type of
      -- the first argument to decide which to use.
      | isComparison fn -> case args ^? ix 0 . aNode . aTy of
        Just (TyPrim TyInteger) -> mkComparison translateNodeInt
        Just (TyPrim TyDecimal) -> mkComparison translateNodeDecimal
        Just (TyPrim TyString) -> mkComparison translateNodeStr
        Just (TyPrim TyTime) -> mkComparison translateNodeTime
        _ -> throwError $ MalformedComparison fn args
      | isLogical fn -> mkLogical

  AST_If _ cond tBranch fBranch -> IfThenElse
    <$> translateNodeBool (AstNodeOf cond)
    <*> translateNodeBool (AstNodeOf tBranch)
    <*> translateNodeBool (AstNodeOf fBranch)

  ast -> throwError $ UnexpectedNode "translateNodeBool" ast

translateNodeStr :: AstNodeOf String -> TranslateM (Term String)
translateNodeStr = unAstNodeOf >>> \case
  AST_Lit (LString t) -> pure $ Literal $ literal $ T.unpack t
  AST_NFun_Basic "+" [a, b] -> Concat
    <$> translateNodeStr (AstNodeOf a)
    <*> translateNodeStr (AstNodeOf b)

  AST_NFun_Basic "pact-version" [] -> pure PactVersion
  --
  -- TODO: more cases.
  --

  AST_WithRead _node table key bindings body -> do
    traceShowM bindings
    let bindings' = flip map bindings $ \(Named name _ _, _var) -> name
    WithRead table
      <$> translateNodeStr (AstNodeOf key)
      <*> pure bindings'
      <*> translateBody undefined (AstNodeOf <$> body)

  ast -> throwError $ UnexpectedNode "translateNodeStr" ast

pattern AST_WithRead :: Node
                     -> Text
                     -> AST Node
                     -> [(Named Node, AST Node)]
                     -> [AST Node]
                     -> AST Node
pattern AST_WithRead node table key bindings body <-
  (App node
       (NativeFuncSpecial "with-read" (AST_Binding _ bindings body))
       [RawTableName table, key])

pattern AST_Binding :: forall a. a -> [(Named a, AST a)] -> [AST a] -> AST a
pattern AST_Binding node' bindings' bdy' <- (Binding node' bindings' bdy' _)

pattern RawTableName :: Text -> AST Node
pattern RawTableName t <- (Table (Node (TcId _ t _) _))

pattern NativeFuncSpecial :: forall a. Text -> AST a -> Fun a
pattern NativeFuncSpecial f bdy <- (FNative _ f _ (Just (_,SBinding bdy)))

-- Pact uses Data.Decimal which is arbitrary-precision
data Decimal = Decimal
  { decimalPlaces :: !Word8
  , decimalMantissa :: !Integer
  } deriving (Show, Read, Eq, Ord, Data, HasKind, SymWord)

mkDecimal :: Decimal.Decimal -> Decimal
mkDecimal (Decimal.Decimal places mantissa) = Decimal places mantissa

sDecimal :: String -> Symbolic (SBV Decimal)
sDecimal = symbolic

translateNodeDecimal :: AstNodeOf Decimal -> TranslateM (Term Decimal)
translateNodeDecimal = unAstNodeOf >>> \case
  AST_Lit (LDecimal d) -> pure (Literal (literal (mkDecimal d)))
  AST_Var n            -> Var <$> view (ix n)
  n                    -> throwError $ UnexpectedNode "translateNodeDecimal" n

type Time = Int64

mkTime :: UTCTime -> Time
mkTime utct = utct ^. _utctDayTime . microseconds

translateNodeTime :: AstNodeOf Time -> TranslateM (Term Time)
translateNodeTime = unAstNodeOf >>> \case
  -- Tricky: seconds could be either integer or decimal
  AST_AddTime time seconds
    | seconds ^. aNode . aTy == TyPrim TyInteger -> AddTimeInt
      <$> translateNodeTime (AstNodeOf time)
      <*> translateNodeInt (AstNodeOf seconds)
    | seconds ^. aNode . aTy == TyPrim TyDecimal -> AddTimeDec
      <$> translateNodeTime (AstNodeOf time)
      <*> translateNodeDecimal (AstNodeOf seconds)

  AST_Lit (LTime t) -> pure (Literal (literal (mkTime t)))
  AST_Var n -> Var <$> view (ix n)

  n -> throwError $ UnexpectedNode "translateNodeTime" n

allocateArgs :: [(Text, Type UserType)] -> Symbolic (Map Text AVar)
allocateArgs argTys = fmap Map.fromList $ for argTys $ \(name, ty) -> do
  let name' = T.unpack name
  var <- case ty of
    TyPrim TyInteger -> mkAVar <$> sInteger name'
    TyPrim TyBool    -> mkAVar <$> sBool name'
    TyPrim TyDecimal -> mkAVar <$> sDecimal name'
    TyPrim TyTime    -> mkAVar <$> sInt64 name'
    TyPrim TyString  -> mkAVar <$> sString name'
  pure (name, var)

namedAuth :: SString -> M SBool
namedAuth str = do
  arr <- view nameAuths
  pure $ readArray arr str

evalTerm :: (Show a, SymWord a) => Term a -> M (SBV a)
evalTerm = \case
  IfThenElse cond then' else' -> do
    testPasses <- evalTerm cond
    ite testPasses (evalTerm then') (evalTerm else')

  Enforce cond -> do
    cond <- evalTerm cond
    succeeds %= (&&& cond)
    pure true

  Sequence a b -> evalTerm a *> evalTerm b

  Literal a -> pure a

  Var name -> do
    -- Assume the term is well-scoped after typechecking
    Just val <- view (scope . at name)
    -- Assume the variable is well-typed after typechecking
    pure $ unsafeCastAVar val

  Arith op args ->
    if op `Set.member` unsupportedArithOps
    then throwError $ UnsupportedArithOp op
    else do

            args' <- forM args evalTerm
            case (op, args') of
              (Sub, [x, y]) -> pure $ x - y
              (Add, [x, y]) -> pure $ x + y
              (Mul, [x, y]) -> pure $ x * y
              (Div, [x, y]) -> pure $ x `sDiv` y
              (Mod, [x, y]) -> pure $ x `sMod` y
              (Abs, [x])    -> pure $ abs x
              (Signum, [x]) -> pure $ signum x
              (Negate, [x]) -> pure $ negate x
              _             -> throwError $ MalformedArithOp op args

  Comparison op x y -> do
    x' <- evalTerm x
    y' <- evalTerm y
    pure $ case op of
      Gt  -> x' .> y'
      Lt  -> x' .< y'
      Gte -> x' .>= y'
      Lte -> x' .<= y'
      Eq  -> x' .== y'
      Neq -> x' ./= y'

  AddTimeInt time secs -> do
    time' <- evalTerm time
    secs' <- evalTerm secs
    pure $ time' + sFromIntegral secs'

  n@(AddTimeDec _ _) -> throwError $ UnhandledTerm
    "We don't support adding decimals to time yet"
    n

  NameAuthorized str -> namedAuth =<< evalTerm str

  Concat str1 str2 -> (.++) <$> evalTerm str1 <*> evalTerm str2

  PactVersion -> pure $ literal $ T.unpack pactVersion

evalDomainProperty :: DomainProperty -> M SBool
evalDomainProperty Success = use succeeds
evalDomainProperty Abort = bnot <$> evalDomainProperty Success
evalDomainProperty (KsNameAuthorized (KeySetName n)) =
  namedAuth $ literal $ T.unpack n
-- evalDomainProperty (TableRead tn) = _todoRead
-- evalDomainProperty (TableWrite tn) = _todoWrite
-- evalDomainProperty (ColumnConserves tableName colName)
-- evalDomainProperty (CellIncrease tableName colName)

evalProperty :: Property -> M SBool
evalProperty (p1 `Implies` p2) = do
  b1 <- evalProperty p1
  b2 <- evalProperty p2
  pure $ b1 ==> b2
evalProperty (p1 `And` p2) = do
  b1 <- evalProperty p1
  b2 <- evalProperty p2
  pure $ b1 &&& b2
evalProperty (p1 `Or` p2) = do
  b1 <- evalProperty p1
  b2 <- evalProperty p2
  pure $ b1 ||| b2
evalProperty (Not p) = bnot <$> evalProperty p
evalProperty (Occurs dp) = evalDomainProperty dp

analyzeFunction
  :: TopLevel Node
  -> Check
  -> IO CheckResult
analyzeFunction (TopFun (FDefun _ _ ty@(FunType argTys retTy) args body' _)) check =
  let argNodes :: [Node]
      argNodes = _nnNamed <$> args

      -- extract the typechecker's name for a node, eg "analyze-tests.layup_x".
      nodeNames :: [Text]
      nodeNames = _tiName . _aId <$> argNodes

      nodeNames' :: Map Node Text
      nodeNames' = Map.fromList $ zip argNodes nodeNames

      argTys :: [(Text, Type UserType)]
      argTys = zip nodeNames (_aTy <$> argNodes)

  in case retTy of
       TyPrim TyString  ->
         analyzeFunction' translateNodeStr check body' argTys nodeNames'
       TyPrim TyInteger ->
         analyzeFunction' translateNodeInt check body' argTys nodeNames'
       TyPrim TyBool    ->
         analyzeFunction' translateNodeBool check body' argTys nodeNames'
       TyPrim TyDecimal ->
         analyzeFunction' translateNodeDecimal check body' argTys nodeNames'
       TyPrim TyTime ->
         analyzeFunction' translateNodeTime check body' argTys nodeNames'
       _ -> error $ "can't analyze ret of: " ++ show retTy

analyzeFunction _ _ = pure $ Left $ CodeCompilationFailed "Top-Level Function analysis can only work on User defined functions (i.e. FDefun)"

checkProperty :: Check -> Property
checkProperty (Satisfiable p) = p
checkProperty (Valid p) = p

data CheckFailure
  = Invalid SBVI.SMTModel
  | Unsatisfiable
  | Unknown String -- reason
  | SatExtensionField SBVI.SMTModel
  | ProofError [String]
  | TypecheckFailure (Set TC.Failure)
  | AnalyzeFailure AnalyzeFailure
  --
  -- TODO: maybe remove this constructor from from CheckFailure.
  --
  | CodeCompilationFailed String
  deriving (Show)

data CheckSuccess
  = SatisfiedProperty SBVI.SMTModel
  | ProvedTheorem
  deriving (Show)

type CheckResult
  = Either CheckFailure CheckSuccess

-- This does not use the underlying property -- this merely dispatches to
-- sat/prove appropriately, and accordingly translates sat/unsat to
-- semantically-meaningful results.
runCheck :: Provable a => Check -> a -> IO CheckResult
runCheck (Satisfiable _prop) provable = do
  (SatResult smtRes) <- sat provable
  pure $ case smtRes of
    SBV.Unsatisfiable _config -> Left Unsatisfiable
    SBV.Satisfiable _config model -> Right $ SatisfiedProperty model
    SBV.SatExtField _config model -> Left $ SatExtensionField model
    SBV.Unknown _config reason -> Left $ Unknown reason
    SBV.ProofError _config strs -> Left $ ProofError strs
runCheck (Valid _prop) provable = do
  (ThmResult smtRes) <- prove provable
  pure $ case smtRes of
    SBV.Unsatisfiable _config -> Right ProvedTheorem
    SBV.Satisfiable _config model -> Left $ Invalid model
    SBV.SatExtField _config model -> Left $ SatExtensionField model
    SBV.Unknown _config reason -> Left $ Unknown reason
    SBV.ProofError _config strs -> Left $ ProofError strs

translateBody
  :: (Show a, SymWord a)
  => K (Term a)
  -> [AstNodeOf a]
  -> TranslateM (Term a)
translateBody _k ast
  | length ast >= 1 = undefined
  | otherwise = throwError EmptyBody

analyzeFunction'
  :: forall a.
     (Show a, SymWord a)
  => (AstNodeOf a -> TranslateM (Term a))
  -> Check
  -> [AST Node]
  -> [(Text, Type UserType)]
  -> Map Node Text
  -> IO CheckResult
analyzeFunction' translator check body argTys nodeNames =
  case runExcept
           (runReaderT
             (translateBody (undefined :: K (Term a)) (AstNodeOf <$> body))
             nodeNames) of
    Left reason -> pure $ Left $ AnalyzeFailure reason
    Right body'' -> do

      compileFailureVar <- newEmptyMVar
      checkResult <- runCheck check $ do
        scope0 <- allocateArgs argTys
        nameAuths <- newArray "nameAuthorizations"

        let prop   = checkProperty check
            env0   = AnalyzeEnv scope0 nameAuths
            state0 = initialAnalyzeState
            action = evalTerm body''
                  *> evalProperty prop

        case runExcept $ runRWST action env0 state0 of
          Left cf -> do
            liftIO $ putMVar compileFailureVar cf
            pure false
          Right (propResult, _env, _log) ->
            pure propResult

      mVarVal <- tryTakeMVar compileFailureVar
      pure $ case mVarVal of
        Nothing -> checkResult
        Just cf -> Left (AnalyzeFailure cf)

rsModuleData :: ModuleName -> Lens' ReplState (Maybe ModuleData)
rsModuleData mn = rEnv . eeRefStore . rsModules . at mn

loadModule :: FilePath -> ModuleName -> IO ModuleData
loadModule fp mn = do
  -- XXX(joel): I don't think we should execScript' here
  (r,s) <- execScript' (Script False fp) fp
  either (die def) (const (return ())) r
  case view (rsModuleData mn) s of
    Just m -> return m
    Nothing -> die def $ "Module not found: " ++ show (fp,mn)

loadFun :: FilePath -> ModuleName -> Text -> IO Ref
loadFun fp mn fn = loadModule fp mn >>= \(_,m) -> case HM.lookup fn m of
  Nothing -> die def $ "Function not found: " ++ show (fp,mn,fn)
  Just f -> return f

inferFun :: Bool -> FilePath -> ModuleName -> Text -> IO (TopLevel Node, TcState)
inferFun dbg fp mn fn = loadFun fp mn fn >>= \r -> runTC 0 dbg (typecheckTopLevel r)

runCompiler :: String -> Text -> Text -> Check -> IO ()
runCompiler = runCompilerDebug False

runCompilerDebug :: Bool -> String -> Text -> Text -> Check -> IO ()
runCompilerDebug dbg replPath' modName' funcName' check = do
  (fun, tcState) <- inferFun dbg replPath' (ModuleName modName') funcName'
  let failures = tcState ^. tcFailures
  if Set.null failures
  then do
    r <- analyzeFunction fun check
    putStrLn $ case r of
      Left err  -> show err
      Right res -> show res
  else putStrLn $ "typechecking failed: " ++ show failures

failedTcOrAnalyze :: TcState -> TopLevel Node -> Check -> IO CheckResult
failedTcOrAnalyze tcState fun check =
    if Set.null failures
    then analyzeFunction fun check
    else pure $ Left $ TypecheckFailure failures
  where
    failures = tcState ^. tcFailures

runCompilerTest :: String -> Text -> Text -> Check -> IO CheckResult
runCompilerTest replPath modName funcName check = do
  (fun, tcState) <- inferFun False replPath (ModuleName modName) funcName
  failedTcOrAnalyze tcState fun check

runTest :: Text -> Check -> IO CheckResult
runTest code check = do
  replState0 <- initReplState StringEval
  (eTerm, replState) <- runStateT (evalRepl' $ T.unpack code) replState0
  case eTerm of
    Left err ->
      pure $ Left $ CodeCompilationFailed err
    Right _t ->
      case view (rsModuleData "test") replState of
        Nothing ->
          pure $ Left $ CodeCompilationFailed "expected module 'test'"
        Just (_mod, modRefs) ->
          case HM.lookup "test" modRefs of
            Nothing ->
              pure $ Left $ CodeCompilationFailed "expected function 'test'"
            Just ref -> do
              (fun, tcState) <- runTC 0 False $ typecheckTopLevel ref
              failedTcOrAnalyze tcState fun check

