{-# language DeriveAnyClass      #-}
{-# language DeriveDataTypeable  #-}
{-# language DeriveGeneric       #-}
{-# language FlexibleContexts    #-}
{-# language FlexibleInstances   #-}
{-# language GADTs               #-}
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

module Pact.Analyze.Types where

import Control.Monad.Except (ExceptT(..), Except, runExcept)
import Control.Monad.Reader
import Control.Monad.Trans.RWS.Strict (RWST(..))
import Control.Lens hiding (op, (.>), (...))
import Data.Data
import qualified Data.Decimal as Decimal
import Data.Map.Strict (Map)
import Data.SBV hiding (Satisfiable, Unsatisfiable, Unknown, ProofError, name)
import qualified Data.SBV.Internals as SBVI
import qualified Data.Text as T
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (IsString(..))
import Data.Text (Text)
import Data.Thyme
import GHC.Generics
import Pact.Types.Lang hiding (Term, TableName)
import Pact.Types.Runtime hiding (Term, WriteType(..), TableName)
import Pact.Types.Typecheck hiding (Var, UserType)
import qualified Pact.Types.Typecheck as TC

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

newtype ColumnName
  = ColumnName Text
  deriving (Eq, Ord, Read, Data, Show)

deriving instance HasKind ColumnName
deriving instance SymWord ColumnName

instance IsString ColumnName where
  fromString = ColumnName . T.pack

deriving instance HasKind (TableName, ColumnName)
deriving instance SymWord (TableName, ColumnName)

-- Checking state that is split before, and merged after, conditionals.
data LatticeAnalyzeState
  = LatticeAnalyzeState
    { _lasSucceeds     :: SBool
    , _lasTableRead    :: SFunArray TableName Bool
    , _lasTableWritten :: SFunArray TableName Bool
    , _lasColumnDelta  :: SFunArray (TableName, ColumnName) Integer
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
      , _lasColumnDelta  = mkSFunArray $ const 0
      }
  , _globalState  = GlobalAnalyzeState ()
  }

succeeds :: Lens' AnalyzeState SBool
succeeds = latticeState.lasSucceeds

data UserType = UserType
  deriving (Eq, Ord, Read, Data, Show)

deriving instance HasKind UserType
deriving instance SymWord UserType

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

columnDelta :: TableName -> ColumnName -> Lens' AnalyzeState SInteger
columnDelta tn cn = latticeState.lasColumnDelta.symArrayAt (literal (tn, cn))

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

  WithRead       :: (Show a) => TableName -> Term String -> [Text] -> Term a -> Term a
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

type Time = Int64

mkTime :: UTCTime -> Time
mkTime utct = utct ^. _utctDayTime . microseconds

-- Pact uses Data.Decimal which is arbitrary-precision
data Decimal = Decimal
  { decimalPlaces :: !Word8
  , decimalMantissa :: !Integer
  } deriving (Show, Read, Eq, Ord, Data, HasKind, SymWord)

mkDecimal :: Decimal.Decimal -> Decimal
mkDecimal (Decimal.Decimal places mantissa) = Decimal places mantissa

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
