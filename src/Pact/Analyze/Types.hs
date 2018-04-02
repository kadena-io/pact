{-# language DeriveAnyClass      #-}
{-# language DeriveDataTypeable  #-}
{-# language DeriveGeneric       #-}
{-# language FlexibleContexts    #-}
{-# language FlexibleInstances   #-}
{-# language GADTs               #-}
{-# language LambdaCase          #-}
{-# language MultiWayIf          #-}
{-# language OverloadedStrings   #-}
{-# language PatternSynonyms     #-}
{-# language Rank2Types          #-}
{-# language RecordWildCards     #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneDeriving  #-}
{-# language TemplateHaskell     #-}
{-# language TypeApplications    #-}
{-# language ViewPatterns        #-}
{-# language TupleSections       #-}

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
import Control.Monad.Trans.RWS.Strict (RWST(..))
import Control.Lens hiding (op, (.>), (...))
import Data.Data
import qualified Data.Decimal as Decimal
import Data.Text (Text)
import Data.Thyme
import Data.String (IsString(..))
import Pact.Typechecker hiding (debug)
import Pact.Types.Lang hiding (Term, TableName)
import qualified Pact.Types.Lang as Lang
import Pact.Types.Runtime hiding (Term, WriteType(..), TableName)
import Pact.Types.Typecheck hiding (Var)
import qualified Pact.Types.Typecheck as TC
import Pact.Types.Version (pactVersion)
import Pact.Repl
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

newtype TableName
  = TableName Text
  deriving (Eq, Ord, Read, Data, Show)

deriving instance HasKind TableName
deriving instance SymWord TableName

instance IsString TableName where
  fromString = TableName . T.pack

-- Checking state that is split before, and merged after, conditionals.
data LatticeAnalyzeState
  = LatticeAnalyzeState
    { _lasSucceeds     :: SBool
    , _lasTableRead    :: SFunArray TableName Bool
    , _lasTableWritten :: SFunArray TableName Bool
    }
  deriving (Show, Generic, Mergeable)
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
  deriving (Show)
makeLenses ''AnalyzeState

instance Mergeable AnalyzeState where
  -- NOTE: We discard the left global state because this is out-of-date and was
  -- already fed to the right computation -- we use the updated right global
  -- state.
  symbolicMerge force test (AnalyzeState lls _) (AnalyzeState rls rgs) =
    AnalyzeState (symbolicMerge force test lls rls) rgs

initialAnalyzeState :: AnalyzeState
initialAnalyzeState = AnalyzeState
  { _latticeState = LatticeAnalyzeState
      { _lasSucceeds     = true
      , _lasTableRead    = mkSFunArray $ const false
      , _lasTableWritten = mkSFunArray $ const false
      }
  , _globalState  = GlobalAnalyzeState ()
  }

succeeds :: Lens' AnalyzeState SBool
succeeds = latticeState.lasSucceeds

symArrayAt
  :: forall array k v
   . (SymWord k, SymWord v, SymArray array)
  => SBV k -> Lens' (array k v) (SBV v)
symArrayAt symKey = lens getter setter
  where
    getter :: array k v -> SBV v
    getter arr = readArray arr symKey

    setter :: array k v -> SBV v -> array k v
    setter arr = writeArray arr symKey

tableRead :: TableName -> Lens' AnalyzeState SBool
tableRead tn = latticeState.lasTableRead.symArrayAt (literal tn)

tableWritten :: TableName -> Lens' AnalyzeState SBool
tableWritten tn = latticeState.lasTableWritten.symArrayAt (literal tn)

data AnalyzeFailure
  = MalformedArithOpExec ArithOp [Term Integer]
  | UnsupportedArithOp ArithOp
  | MalformedComparison Text [AST Node]
  | MalformedLogicalOp Text [AST Node]
  | MalformedLogicalOpExec LogicalOp [Term Bool]
  | MalformedArithOp Text [AST Node]
  -- | Some translator received a node it didn't expect
  | UnexpectedNode String (AST Node)
  -- | 'translateBody' expects at least one node in a function body.
  | EmptyBody
  -- | A node we have a good reason not to handle
  | UnhandledTerm String (Term Int64)
  deriving Show

type AnalyzeM = RWST AnalyzeEnv AnalyzeLog AnalyzeState (Except AnalyzeFailure)

instance (Mergeable a) => Mergeable (AnalyzeM a) where
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

comparisonOperators, logicalOperators, arithOperators :: Set Text
comparisonOperators = Set.fromList [">", "<", ">=", "<=", "=", "!="]
logicalOperators    = Set.fromList ["and", "or", "not"]
arithOperators      = Set.fromList
  ["+", "-", "*", "/", "abs", "^", "sqrt", "mod", "log", "ln", "exp", "abs",
  "round", "ceiling", "floor"]

isComparison, isLogical, isArith :: Text -> Bool
isComparison s = Set.member s comparisonOperators
isLogical    s = Set.member s logicalOperators
isArith      s = Set.member s arithOperators

ofBasicOperators :: Text -> Either Text Text
ofBasicOperators s = if isBasic then Right s else Left s
  where
    isBasic = Set.member s
      (comparisonOperators <> logicalOperators <> arithOperators)

-- helper patterns
pattern NativeFunc :: forall a. Text -> Fun a
pattern NativeFunc f <- (FNative _ f _ _)

-- compileNode's Patterns

pattern AST_Let :: forall a. a -> [(Named a, AST a)] -> [AST a] -> AST a
pattern AST_Let node bindings body = Binding node bindings body BindLet

pattern AST_Bind :: forall a. a -> [(Named a, AST a)] -> a -> [AST a] -> AST a
pattern AST_Bind node bindings schema body <- Binding node bindings body (BindSchema schema)

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
  IfThenElse     ::                        Term Bool    -> Term a         -> Term a -> Term a
  Enforce        ::                        Term Bool    ->                             Term Bool
  -- TODO: do we need a noop to handle a sequence of one expression?
  Sequence       :: (Show b, SymWord b) => Term b       -> Term a         ->           Term a
  Literal        ::                        SBV a        ->                             Term a
  -- TODO: Read should return an object, probably parameterized by a schema:
  Read           ::                        TableName    -> Term String    ->           Term ()
  -- TODO: Write should take an obj after tn and row term but still return Term String like pact:
  --         the object should likewise probably be parameterized by a schema.
  Write          ::                        TableName    -> Term String    ->           Term String
  Let            :: (Show a, SymWord a) => Text         -> Term a         -> Term b -> Term b
  -- TODO: not sure if we need a separate `Bind` ctor for object binding. try
  --       just using Let+At first.
  Var            ::                        Text         ->                             Term a
  Arith          ::                        ArithOp      -> [Term Integer] ->           Term Integer
  Comparison     :: (Show a, SymWord a) => ComparisonOp -> Term a         -> Term a -> Term Bool
  Logical        ::                        LogicalOp    -> [Term Bool]    ->           Term Bool
  AddTimeInt     ::                        Term Time    -> Term Integer   ->           Term Time
  AddTimeDec     ::                        Term Time    -> Term Decimal   ->           Term Time
  NameAuthorized ::                        Term String  ->                             Term Bool
  Concat         ::                        Term String  -> Term String    ->           Term String
  PactVersion    ::                                                                    Term String

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

--
-- TODO: this should probably have its own TranslateFailure type?
--
type TranslateM = ReaderT (Map Node Text) (Except AnalyzeFailure)

data K a = K
  !(Term Bool    -> a)
  !(Term Decimal -> a)
  !(Term Integer -> a)
  !(Term String  -> a)
  !(Term Time    -> a)

instance Functor K where
  fmap f (K b d i s t) = K (f . b) (f . d) (f . i) (f . s) (f . t)

kApplyUniform :: forall a. Node -> K a -> (forall b. Term b) -> a
kApplyUniform node (K fb fd fi fs ft) tm = case node ^. aTy of
  TyPrim TyBool    -> fb tm
  TyPrim TyDecimal -> fd tm
  TyPrim TyInteger -> fi tm
  TyPrim TyString  -> fs tm
  TyPrim TyTime    -> ft tm

  -- TODO
  TyPrim TyValue   -> error "unimplemented type analysis"
  TyPrim TyKeySet  -> error "unimplemented type analysis"
  TyAny            -> error "unimplemented type analysis"
  TyVar _v         -> error "unimplemented type analysis"
  TyList _         -> error "unimplemented type analysis"
  TySchema _ _     -> error "unimplemented type analysis"
  TyFun _          -> error "unimplemented type analysis"
  TyUser _         -> error "unimplemented type analysis"

preMap :: (forall a. Term a -> Term a) -> K b -> K b
preMap f (K fb fd fi fs ft) = K (fb . f) (fd . f) (fi . f) (fs . f) (ft . f)

uniformK :: (forall a. (Show a, SymWord a) => Term a -> b) -> K b
uniformK f = K f f f f f

kExpectInt :: K (Either String (Term Integer))
kExpectInt = K
  (const (Left "expecting int"))
  (const (Left "expecting int"))
  pure
  (const (Left "expecting int"))
  (const (Left "expecting int"))

kApplyInt :: K a -> Term Integer -> a
kApplyInt (K _ _ fi _ _) i = fi i

kExpectBool :: K (Either String (Term Bool))
kExpectBool = K
  pure
  (const (Left "expecting bool"))
  (const (Left "expecting bool"))
  (const (Left "expecting bool"))
  (const (Left "expecting bool"))

kApplyBool :: K a -> Term Bool -> a
kApplyBool (K fb _ _ _ _) b = fb b

kExpectStr :: K (Either String (Term String))
kExpectStr = K
  (const (Left "expecting string"))
  (const (Left "expecting string"))
  (const (Left "expecting string"))
  pure
  (const (Left "expecting string"))

kApplyStr :: K a -> Term String -> a
kApplyStr (K _ _ _ fs _) s = fs s

kExpectDecimal :: K (Either String (Term Decimal))
kExpectDecimal = K
  (const (Left "expecting decimal"))
  pure
  (const (Left "expecting decimal"))
  (const (Left "expecting decimal"))
  (const (Left "expecting decimal"))

kApplyDecimal :: K a -> Term Decimal -> a
kApplyDecimal (K _ fd _ _ _) d = fd d

mkTime :: UTCTime -> Time
mkTime utct = utct ^. _utctDayTime . microseconds

kExpectTime :: K (Either String (Term Time))
kExpectTime = K
  (const (Left "expecting time"))
  (const (Left "expecting time"))
  (const (Left "expecting time"))
  (const (Left "expecting time"))
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
pattern RawTableName t <- (Table (Node (TcId _ t _) _) _)

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

    -- TODO
    TyPrim TyValue   -> error "unimplemented type analysis"
    TyPrim TyKeySet  -> error "unimplemented type analysis"
    TyAny            -> error "unimplemented type analysis"
    TyVar _v         -> error "unimplemented type analysis"
    TyList _         -> error "unimplemented type analysis"
    TySchema _ _     -> error "unimplemented type analysis"
    TyFun _          -> error "unimplemented type analysis"
    TyUser _         -> error "unimplemented type analysis"
  pure (name, var)

namedAuth :: SString -> AnalyzeM SBool
namedAuth str = do
  arr <- view nameAuths
  pure $ readArray arr str

evalTerm :: (Show a, SymWord a) => Term a -> AnalyzeM (SBV a)
evalTerm = \case
  IfThenElse cond then' else' -> do
    testPasses <- evalTerm cond
    ite testPasses (evalTerm then') (evalTerm else')

  Enforce cond -> do
    cond' <- evalTerm cond
    succeeds %= (&&& cond')
    pure true

  Sequence a b -> evalTerm a *> evalTerm b

  Literal a -> pure a

  Read tn rowId -> do
    rId <- evalTerm rowId -- TODO: use this
    tableRead tn .= true
    pure $ literal () -- TODO: this should instead return a symbolic object

  --
  -- TODO: we might want to eventually support checking each of the semantics
  -- of Pact.Types.Runtime's WriteType.
  --
  Write tn rowId {- obj -} -> do
    tableWritten tn .= true
    --
    -- TODO: make a constant on the pact side that this uses:
    --
    pure $ literal "Write succeeded"

  Let name rhs body -> do
    val <- evalTerm rhs
    local (scope.at name ?~ mkAVar val) $
      evalTerm body

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
              _             -> throwError $ MalformedArithOpExec op args

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

  Logical op args -> do
    args' <- forM args evalTerm
    case (op, args') of
      (AndOp, [a, b]) -> pure $ a &&& b
      (OrOp, [a, b])  -> pure $ a ||| b
      (NotOp, [a])    -> pure $ bnot a
      _               -> throwError $ MalformedLogicalOpExec op args

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

evalDomainProperty :: DomainProperty -> AnalyzeM SBool
evalDomainProperty Success = use succeeds
evalDomainProperty Abort = bnot <$> evalDomainProperty Success
evalDomainProperty (KsNameAuthorized (KeySetName n)) =
  namedAuth $ literal $ T.unpack n
evalDomainProperty (TableRead tn) = use $ tableRead tn
evalDomainProperty (TableWrite tn) = use $ tableWritten tn
-- evalDomainProperty (ColumnConserves tableName colName)
-- evalDomainProperty (CellIncrease tableName colName)

evalProperty :: Property -> AnalyzeM SBool
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

tcName :: Node -> Text
tcName = _tiName . _aId

analyzeFunction
  :: TopLevel Node
  -> Check
  -> IO CheckResult
analyzeFunction (TopFun (FDefun _ _ (FunType _ retTy) args body' _)) check =
  let argNodes :: [Node]
      argNodes = _nnNamed <$> args

      -- extract the typechecker's name for a node, eg "analyze-tests.layup_x".
      nodeNames :: [Text]
      nodeNames = tcName <$> argNodes

      nodeNames' :: Map Node Text
      nodeNames' = Map.fromList $ zip argNodes nodeNames

      argTys :: [(Text, Type UserType)]
      argTys = zip nodeNames (_aTy <$> argNodes)

  in case retTy of
      TyPrim TyBool    -> analyzeFunction' check kExpectBool body' argTys nodeNames'
      TyPrim TyDecimal -> analyzeFunction' check kExpectDecimal body' argTys nodeNames'
      TyPrim TyInteger -> analyzeFunction' check kExpectInt body' argTys nodeNames'
      TyPrim TyString  -> analyzeFunction' check kExpectStr body' argTys nodeNames'
      TyPrim TyTime    -> analyzeFunction' check kExpectTime body' argTys nodeNames'

      -- TODO
      TyPrim TyValue   -> error "unimplemented type analysis"
      TyPrim TyKeySet  -> error "unimplemented type analysis"
      TyAny            -> error "unimplemented type analysis"
      TyVar _v         -> error "unimplemented type analysis"
      TyList _         -> error "unimplemented type analysis"
      TySchema _ _     -> error "unimplemented type analysis"
      TyFun _          -> error "unimplemented type analysis"
      TyUser _         -> error "unimplemented type analysis"

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
  => K (Either String (Term a))
  -> [AST Node]
  -> TranslateM (Term a)
translateBody _k [] = throwError EmptyBody
translateBody k [ast] = translateNode' k ast
translateBody k (ast:asts) = join $ flip translateNode ast $ uniformK
  (\a -> translateBody (preMap (Sequence a) k) asts)

-- TODO: can we get rid of translateNode'?
translateNode' :: forall a. K (Either String a) -> AST Node -> TranslateM a
translateNode' k node = do
  x <- translateNode k node
  case x of
    Left e   -> throwError (UnexpectedNode e node)
    Right x' -> pure x'

translateNode :: forall a. K a -> AST Node -> TranslateM a
translateNode k = \case
  -- AST_Let _ [] body ->
  --   translateBody kExpectInt body
  --
  -- AST_Let n ((Named _ varNode _, rhs):bindingsRest) body -> do
  --   val <- _translateBasedOnType varNode rhs
  --   let varName = tcName varNode
  --   local (at varNode ?~ varName) $ do
  --     --
  --     -- TODO: do we only want to allow subsequent bindings to reference
  --     --       earlier ones if we know it's let* rather than let? or has this
  --     --       been enforced by earlier stages for us?
  --     --
  --     rest <- translateNode kExpectInt $ AST_Let n bindingsRest body
  --     return $ Let varName val rest

  AST_Var node -> do
    varName <- view (ix node)
    pure $ kApplyUniform node k (Var varName)

  -- Int
  AST_NegativeLit (LInteger i)  -> kApplyInt k . Arith Negate . pure <$>
    pure (Literal (literal i))
  AST_Lit         (LInteger i)  -> pure (kApplyInt k (Literal (literal i)))
  AST_NegativeVar n -> do
    name <- view (ix n)
    pure $ kApplyInt k $ Arith Negate [Var name]
  AST_Days days -> do
    days' <- translateNode' kExpectInt days
    pure $ kApplyInt k $ Arith Mul [60 * 60 * 24, days']

  -- Bool
  AST_Lit (LBool b)     -> pure (kApplyBool k (Literal (literal b)))

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

        -- TODO(joel): do this for decimal (etc) as well
        mkArith :: TranslateM a
        mkArith = fmap (kApplyInt k) $ case (fn, args) of
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
          _ -> throwError $ MalformedArithOp fn args

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
      | isArith   fn -> mkArith

  AST_NFun _node name [Table _tnode (Lang.TableName tn), row, _obj]
    | elem name ["insert", "update", "write"]
    -> kApplyStr k . Write (TableName tn) <$> translateNode' kExpectStr row

  AST_If node cond tBranch fBranch -> case node ^. aTy of
    -- TODO(joel): express this more succinctly

    TyPrim TyBool    -> do
      ite' <- IfThenElse
        <$> translateNode' kExpectBool cond
        <*> translateNode' kExpectBool tBranch
        <*> translateNode' kExpectBool fBranch
      pure $ kApplyBool k ite'

    TyPrim TyDecimal -> do
      ite' <- IfThenElse
        <$> translateNode' kExpectBool cond
        <*> translateNode' kExpectDecimal tBranch
        <*> translateNode' kExpectDecimal fBranch
      pure $ kApplyDecimal k ite'

    TyPrim TyInteger -> do
      ite' <- IfThenElse
        <$> translateNode' kExpectBool cond
        <*> translateNode' kExpectInt tBranch
        <*> translateNode' kExpectInt fBranch
      pure $ kApplyInt k ite'

    TyPrim TyString  -> do
      ite' <- IfThenElse
        <$> translateNode' kExpectBool cond
        <*> translateNode' kExpectStr tBranch
        <*> translateNode' kExpectStr fBranch
      pure $ kApplyStr k ite'

    TyPrim TyTime    -> do
      ite' <- IfThenElse
        <$> translateNode' kExpectBool cond
        <*> translateNode' kExpectTime tBranch
        <*> translateNode' kExpectTime fBranch
      pure $ kApplyTime k ite'

    -- TODO
    _ -> error "unimplemented type analysis"

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

  --
  -- TODO: more cases.
  --

  ast -> throwError (UnexpectedNode "translateNode" ast)

analyzeFunction'
  :: (Show a, SymWord a)
  => Check
  -> K (Either String (Term a))
  -> [AST Node]
  -> [(Text, Type UserType)]
  -> Map Node Text
  -> IO CheckResult
analyzeFunction' check expectation body argTys nodeNames =
  case runExcept
           (runReaderT
             (translateBody expectation body)
             nodeNames) of
    Left reason -> pure $ Left $ AnalyzeFailure reason
    Right body'' -> do

      compileFailureVar <- newEmptyMVar
      checkResult <- runCheck check $ do
        scope0 <- allocateArgs argTys
        nameAuths' <- newArray "nameAuthorizations"

        let prop   = checkProperty check
            env0   = AnalyzeEnv scope0 nameAuths'
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

