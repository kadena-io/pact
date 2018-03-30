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

import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Except (ExceptT(..), Except, runExcept, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
import Control.Monad.State.Strict (runStateT)
import Control.Monad.Trans.RWS.Strict
import Control.Lens hiding (op, (.>), (...))
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
  | UnexpectedNode (AST Node)
  -- | 'translateBody' expects at least one node in a function body.
  | EmptyBody
  -- | A node we have a good reason not to handle
  | UnhandledTerm String (Term Int64)
  | UnexpectedNode'
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
  -- TODO: do we need a noop to handle a sequence of one expression?
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

type TranslateM = ReaderT (Map Node Text) (Except AnalyzeFailure)

data K a = K
  !(Term Bool    -> a)
  !(Term Decimal -> a)
  !(Term Integer -> a)
  !(Term String  -> a)
  !(Term Time    -> a)

instance Functor K where
  fmap f (K b d i s t) = K (f . b) (f . d) (f . i) (f . s) (f . t)

preMap :: (forall a. Term a -> Term a) -> K b -> K b
preMap f (K fb fd fi fs ft) = K (fb . f) (fd . f) (fi . f) (fs . f) (ft . f)

uniformK :: (forall a. (Show a, SymWord a) => Term a -> b) -> K b
uniformK f = K f f f f f

kExpectInt :: K (Maybe (Term Integer))
kExpectInt = K
  (const Nothing)
  (const Nothing)
  pure
  (const Nothing)
  (const Nothing)

kApplyInt :: K a -> Term Integer -> a
kApplyInt (K _ _ fi _ _) i = fi i

kExpectBool :: K (Maybe (Term Bool))
kExpectBool = K
  pure
  (const Nothing)
  (const Nothing)
  (const Nothing)
  (const Nothing)

kApplyBool :: K a -> Term Bool -> a
kApplyBool (K fb _ _ _ _) b = fb b

kExpectStr :: K (Maybe (Term String))
kExpectStr = K
  (const Nothing)
  (const Nothing)
  (const Nothing)
  pure
  (const Nothing)

kApplyStr :: K a -> Term String -> a
kApplyStr (K _ _ _ fs _) s = fs s

kExpectDecimal :: K (Maybe (Term Decimal))
kExpectDecimal = K
  (const Nothing)
  pure
  (const Nothing)
  (const Nothing)
  (const Nothing)

kApplyDecimal :: K a -> Term Decimal -> a
kApplyDecimal (K _ fd _ _ _) d = fd d

mkTime :: UTCTime -> Time
mkTime utct = utct ^. _utctDayTime . microseconds

kExpectTime :: K (Maybe (Term Time))
kExpectTime = K
  (const Nothing)
  (const Nothing)
  (const Nothing)
  (const Nothing)
  pure

kApplyTime :: K a -> Term Time -> a
kApplyTime (K _ _ _ _ ft) t = ft t

infixr 9 ...
(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(...) = ((.) . (.))

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

type Time = Int64

-- Pact uses Data.Decimal which is arbitrary-precision
data Decimal = Decimal
  { decimalPlaces :: !Word8
  , decimalMantissa :: !Integer
  } deriving (Show, Read, Eq, Ord, Data, HasKind, SymWord)

mkDecimal :: Decimal.Decimal -> Decimal
mkDecimal (Decimal.Decimal places mantissa) = Decimal places mantissa

sDecimal :: String -> Symbolic (SBV Decimal)
sDecimal = symbolic

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

  in analyzeFunction' check body' argTys nodeNames'

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
  :: forall a. (Show a, SymWord a)
  => K (Maybe (Term a))
  -> [AST Node]
  -> TranslateM (Term a)
translateBody _k [] = throwError EmptyBody
translateBody k [ast] = translateNode' k ast
translateBody k (ast:asts) = join $ flip translateNode ast $ uniformK
  (\a -> translateBody (preMap (Sequence a) k) asts)

-- TODO: can we get rid of translateNode'
translateNode' :: forall a. K (Maybe a) -> AST Node -> TranslateM a
translateNode' k node = do
  x <- translateNode k node
  case x of
    Nothing -> throwError UnexpectedNode'
    Just x' -> pure x'

translateNode :: forall a. K a -> AST Node -> TranslateM a
translateNode k = \case

  -- Int
  AST_NegativeLit (LInteger i)  -> kApplyInt k . Arith Negate . pure <$>
    pure (Literal (literal i))
  AST_Lit         (LInteger i)  -> pure (kApplyInt k (Literal (literal i)))
  AST_NegativeVar n -> do
    name <- view (ix n)
    pure $ kApplyInt k $ Arith Negate [Var name]
  AST_Var         n -> kApplyInt k . Var <$> view (ix n)
  AST_Days days -> do
    days' <- translateNode' kExpectInt days
    pure $ kApplyInt k $ Arith Mul [60 * 60 * 24, days']
  AST_AddTime time seconds
    | seconds ^. aNode . aTy == TyPrim TyInteger -> undefined

  -- TODO(joel): do this for decimal (etc) as well
  AST_NFun_Basic fn args -> fmap (kApplyInt k) $ case (fn, args) of
    ("-", [a, b]) -> do
      a' <- translateNode' kExpectInt a
      b' <- translateNode' kExpectInt b
      pure $ Arith Sub [a', b']
    ("-", [a]) -> Arith Negate . pure <$> translateNode' kExpectInt a
    ("+", [a, b]) -> do
      a' <- translateNode' kExpectInt a
      b' <- translateNode' kExpectInt b
      pure $ Arith Add [a', b']
    ("*", [a, b]) -> do
      a' <- translateNode' kExpectInt a
      b' <- translateNode' kExpectInt b
      pure $ Arith Mul [a', b']
    ("/", [a, b]) -> do
      a' <- translateNode' kExpectInt a
      b' <- translateNode' kExpectInt b
      pure $ Arith Div [a', b']
    ("^", [a, b]) -> do
      a' <- translateNode' kExpectInt a
      b' <- translateNode' kExpectInt b
      pure $ Arith Pow [a', b']
    ("sqrt", [a]) -> Arith Sqrt . pure <$> translateNode' kExpectInt a
    ("mod", [a, b]) -> do
      a' <- translateNode' kExpectInt a
      b' <- translateNode' kExpectInt b
      pure $ Arith Mod [a', b']
    ("log", [a, b]) -> do
      a' <- translateNode' kExpectInt a
      b' <- translateNode' kExpectInt b
      pure $ Arith Log [a', b']
    ("ln", [a]) -> Arith Ln . pure <$> translateNode' kExpectInt a
    ("exp", [a]) -> Arith Pow . pure <$> translateNode' kExpectInt a
    ("abs", [a]) -> Arith Abs . pure <$> translateNode' kExpectInt a

    ("round", [a]) -> Arith Round . pure <$> translateNode' kExpectInt a
    ("ceiling", [a]) -> Arith Ceiling . pure <$> translateNode' kExpectInt a
    ("floor", [a]) -> Arith Floor . pure <$> translateNode' kExpectInt a

  -- Bool
  AST_Lit (LBool b)     -> pure (kApplyBool k (Literal (literal b)))
  AST_Var n -> kApplyBool k . Var <$> view (ix n)

  AST_Enforce _ cond _msg -> do
    condTerm <- translateNode' kExpectBool cond
    return $ kApplyBool k $ Enforce condTerm

  AST_EnforceKeyset ks
    | ks ^? aNode.aTy == Just (TyPrim TyString)
    -> do
      ksNameTerm <- translateNode' kExpectStr ks
      return $ kApplyBool k $ Enforce $ NameAuthorized ksNameTerm

  AST_NFun_Basic fn args -> do
    let mkComparison
          :: (Show b, SymWord b)
          => (AST Node -> TranslateM (Term b))
          -> TranslateM a
        mkComparison translate = fmap (kApplyBool k) $ case (fn, args) of
          -- TODO: this could compare integer, decimal, string, or time. use the node
          -- to decide which to dispatch to
          (">", [a, b]) -> Comparison Gt
            <$> translate a
            <*> translate b
          ("<", [a, b]) -> Comparison Lt
            <$> translate a
            <*> translate b
          ("<=", [a, b]) -> Comparison Lte
            <$> translate a
            <*> translate b
          (">=", [a, b]) -> Comparison Gte
            <$> translate a
            <*> translate b
          ("=", [a, b]) -> Comparison Eq
            <$> translate a
            <*> translate b
          ("!=", [a, b]) -> Comparison Neq
            <$> translate a
            <*> translate b
          _ -> throwError $ MalformedComparison fn args

        mkLogical :: TranslateM a
        mkLogical = fmap (kApplyBool k) $ case (fn, args) of
          ("and", [a, b]) -> do
            a' <- translateNode' kExpectBool a
            b' <- translateNode' kExpectBool b
            pure $ Logical AndOp [a', b']
          ("or", [a, b]) -> do
            a' <- translateNode' kExpectBool a
            b' <- translateNode' kExpectBool b
            pure $ Logical OrOp [a', b']
          ("not", [a]) -> do
            a' <- translateNode' kExpectBool a
            pure $ Logical NotOp [a']
          _ -> throwError $ MalformedLogicalOp fn args

    if
      -- integer, decimal, string, and time are all comparable. Use the type of
      -- the first argument to decide which to use.
      | isComparison fn -> case args ^? ix 0 . aNode . aTy of
        Just (TyPrim TyInteger) -> mkComparison (translateNode' kExpectInt)
        Just (TyPrim TyDecimal) -> mkComparison (translateNode' kExpectDecimal)
        Just (TyPrim TyString)  -> mkComparison (translateNode' kExpectStr)
        Just (TyPrim TyTime)    -> mkComparison (translateNode' kExpectTime)
        _ -> throwError $ MalformedComparison fn args
      | isLogical fn -> mkLogical

  AST_If _ cond tBranch fBranch -> do
    ite <- IfThenElse
      <$> translateNode' kExpectBool cond
      <*> translateNode' kExpectBool tBranch
      <*> translateNode' kExpectBool fBranch
    pure $ kApplyBool k ite

  -- String
  AST_Lit (LString t) -> pure $ kApplyStr k $ Literal $ literal $ T.unpack t
  AST_NFun_Basic "+" [a, b] -> kApplyStr k ... Concat
    <$> translateNode' kExpectStr a
    <*> translateNode' kExpectStr b

  AST_NFun_Basic "pact-version" [] -> pure $ kApplyStr k PactVersion

  AST_WithRead _node table key bindings body -> do
    traceShowM bindings
    let bindings' = flip map bindings $ \(Named name _ _, _var) -> name
    -- TODO: this should not be specialized to just String
    body' <- translateBody kExpectStr body
    key' <- translateNode' kExpectStr key
    let withRead = WithRead table key' bindings' body'
    pure $ kApplyStr k withRead

  -- Decimal
  AST_Lit (LDecimal d) -> pure (kApplyDecimal k (Literal (literal (mkDecimal d))))

  -- Time
  -- Tricky: seconds could be either integer or decimal
  AST_AddTime time seconds
    | seconds ^. aNode . aTy == TyPrim TyInteger -> kApplyTime k ... AddTimeInt
      <$> translateNode' kExpectTime time
      <*> translateNode' kExpectInt seconds
    | seconds ^. aNode . aTy == TyPrim TyDecimal -> kApplyTime k ... AddTimeDec
      <$> translateNode' kExpectTime time
      <*> translateNode' kExpectDecimal seconds

  AST_Lit (LTime t) -> pure (kApplyTime k (Literal (literal (mkTime t))))
  AST_Var n -> kApplyTime k . Var <$> view (ix n)
  AST_Var n            -> kApplyDecimal k . Var <$> view (ix n)

  --
  -- TODO: more cases.
  --

  ast -> throwError (UnexpectedNode ast)

analyzeFunction'
  :: Check
  -> [AST Node]
  -> [(Text, Type UserType)]
  -> Map Node Text
  -> IO CheckResult
analyzeFunction' check body argTys nodeNames =
  case runExcept
           (runReaderT
             -- XXX generalize to any type
             (translateBody kExpectInt body)
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

