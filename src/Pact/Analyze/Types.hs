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
{-# language StandaloneDeriving #-}
{-# language TemplateHaskell    #-}
{-# language TypeApplications   #-}
{-# language ViewPatterns       #-}

module Pact.Analyze.Types
  ( runCompiler
  , runCompilerDebug
  , runCompilerTest
  , analyzeFunction
  , CheckState(..)
  , SmtCompilerException(..)
  , inferFun
  ) where

import Control.Arrow ((>>>))
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Except (ExceptT(..), Except, runExcept, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
import Control.Monad.Trans.RWS.Strict
import Control.Exception
import Control.Lens hiding (op, (.>))
import Data.Data
import qualified Data.Decimal as Decimal
import Data.Thyme
import Pact.Typechecker hiding (debug)
import Pact.Types.Lang hiding (Term)
import Pact.Types.Runtime hiding (Term)
import Pact.Types.Typecheck hiding (Var)
import qualified Pact.Types.Typecheck as TC
import Pact.Repl
import Data.Aeson hiding (Object, (.=))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HM
import Data.Default
import GHC.Generics
import Data.SBV hiding (name)
import qualified Data.SBV.Internals as SBVI
import Data.Thyme.Clock.POSIX
import qualified Data.Text as T

-- import SmtLib.Syntax.Syntax
-- import qualified SmtLib.Syntax.Syntax as Smt
-- import qualified SmtLib.Syntax.ShowSL as SmtShow
-- import qualified SmtLib.Parsers.CommandsParsers as SmtParser

import qualified Text.Parsec as Parsec
import Text.Parsec hiding (parse, try)

-- !!! Orphan needed for dev, delete when finished
-- instance ToJSON Node where
--   toJSON = toJSON . show

-- TODO: get rid of SmtCompilerException

data SmtCompilerException =
  SmtCompilerException
  { _smteSrc :: String
  , _smteErr :: String }
  deriving (Eq)

instance Show SmtCompilerException where
  show (SmtCompilerException src err) = src ++ ": " ++ err

instance Exception SmtCompilerException

data SymType = SymInteger
  | SymDecimal
  | SymBool
  | SymString
  | SymTime
  deriving (Show, Eq, Ord, Generic, ToJSON)

-- data TableAccess =
--   TableRead |
--   TableWrite
--   deriving (Show, Eq, Generic, ToJSON)

-- | Low-level, untyped variable.
data AVar = AVar SBVI.SVal
  deriving (Eq, Show)

unsafeCastAVar :: AVar -> SBV a
unsafeCastAVar (AVar sval) = SBVI.SBV sval

mkAVar :: SBV a -> AVar
mkAVar (SBVI.SBV sval) = AVar sval

data CheckEnv = CheckEnv
  { _scope :: Map Text AVar
  } deriving Show
makeLenses ''CheckEnv

newtype CheckLog
  = CheckLog ()

instance Mergeable CheckLog where
  --
  -- NOTE: If we change the underlying representation of CheckLog to a list,
  -- the default Mergeable instance for this will have the wrong semantics, as
  -- it requires that lists have the same length. We more likely want to use
  -- monoidal semantics for anything we log:
  --
  symbolicMerge f t (CheckLog log1) (CheckLog log2) = CheckLog $ log1 <> log2

instance Monoid CheckLog where
  mempty = CheckLog ()
  mappend _ _ = CheckLog ()

-- Checking state that is split before, and merged after, conditionals.
data LatticeCheckState
  = LatticeCheckState
    { _lcsSucceeds :: SBool
    }
  deriving (Show, Eq, Generic, Mergeable)
makeLenses ''LatticeCheckState

-- Checking state that is transferred through every computation, in-order.
newtype GlobalCheckState
  = GlobalCheckState ()
  deriving (Show, Eq)
makeLenses ''GlobalCheckState

data CheckState
  = CheckState
    { _latticeState :: LatticeCheckState
    , _globalState  :: GlobalCheckState
    }
  deriving (Show, Eq)
makeLenses ''CheckState

instance Mergeable CheckState where
  -- NOTE: We discard the left global state because this is out-of-date and was
  -- already fed to the right computation -- we use the updated right global
  -- state.
  symbolicMerge force test (CheckState lls _) (CheckState rls rgs) =
    CheckState (symbolicMerge force test lls rls) rgs

initialCheckState :: CheckState
initialCheckState = CheckState
  { _latticeState = LatticeCheckState true
  , _globalState  = GlobalCheckState ()
  }

succeeds :: Lens' CheckState SBool
succeeds = latticeState.lcsSucceeds

data CompileFailure
  = MalformedArithOp ArithOp [Term Integer]
  | UnsupportedArithOp ArithOp
  | MalformedComparison
  deriving Show

type M = RWST CheckEnv CheckLog CheckState (Except CompileFailure)

instance (Mergeable w, Mergeable a) => Mergeable (RWST r w CheckState (Except e) a) where
  symbolicMerge force test left right = RWST $ \r s -> ExceptT $ Identity $
    --
    -- We explicitly propagate only the "global" portion of the state from the
    -- left to the right computation. And then the only lattice state, and not
    -- global state, is merged (per CheckState's Mergeable instance.)
    --
    -- If either side fails, the entire merged computation fails.
    --
    let run act = runExcept . runRWST act r
    in do
      lTup <- run left s
      let gs = lTup ^. _2.globalState
      rTup <- run right $ s & globalState .~ gs
      return $ symbolicMerge force test lTup rTup

isUnsupportedOperator :: Text -> Maybe Text
isUnsupportedOperator s = if isUnsupported then Just s else Nothing
  where
    isUnsupported = Set.member s $ Set.fromList ["log", "ln", "ceiling", "floor", "mod"]

isInsertOrUpdate :: Fun Node -> Maybe Text
isInsertOrUpdate (NativeFunc "insert") = Just "insert"
isInsertOrUpdate (NativeFunc "update") = Just "update"
isInsertOrUpdate _ = Nothing

ofPrimType :: Node -> Maybe PrimType
ofPrimType (Node _ (TyPrim ty)) = Just ty
ofPrimType _ = Nothing

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


{-
convertType :: Node -> SymType
convertType (OfPrimType TyInteger) = SymInteger
convertType (OfPrimType TyBool) = SymBool
convertType (OfPrimType TyDecimal) = SymDecimal
convertType (OfPrimType TyString) = SymString
convertType (OfPrimType TyTime) = SymTime
convertType n = throw $ SmtCompilerException "convertType" $ "node's type is not supported: " ++ show n

symTypeToSortId :: SymType -> Sort
symTypeToSortId SymInteger = SortId $ ISymbol "Int"
symTypeToSortId SymBool = SortId $ ISymbol "Bool"
symTypeToSortId SymDecimal = SortId $ ISymbol "Real"
symTypeToSortId SymString = SortId $ ISymbol "String"
symTypeToSortId SymTime = SortId $ ISymbol "Real"
-}

-- | Everything is obvious except for time, which works like this:
-- `\(LTime (t :: UTCTime)) -> (init $ show (t ^. posixTime)` :: Smt.Decimal)
-- Decimals (which are actually Reals) are strings in the SMT parser (dunno why)
-- so we convert to POSIX time and show, getting `<some numbers>.<some more numbers>s` and use init to remove the trailing `s`
-- literalToTerm :: Literal -> Term a
-- literalToTerm = Value . \case
--   LBool v    -> literal v
--   LString v  -> literal (show v)
--   LInteger v -> literal v
--   LDecimal v -> literal (show v)
--   LTime t    -> literal $ init $ show (t ^. posixTime)

-- helper patterns
pattern OfPrimType :: PrimType -> Node
pattern OfPrimType pType <- (ofPrimType -> Just pType)

pattern RawTableName :: Text -> AST Node
pattern RawTableName t <- (Table (Node (TcId _ t _) _))

pattern NativeFunc :: forall a. Text -> Fun a
pattern NativeFunc f <- (FNative _ f _ _)

pattern UserFunc :: forall a. Text -> [Named a] -> [AST a] -> Fun a
pattern UserFunc name' args bdy <- (FDefun _ name' _ args bdy _)

pattern NativeFuncSpecial :: forall a. Text -> AST a -> Fun a
pattern NativeFuncSpecial f bdy <- (FNative _ f _ (Just (_,SBinding bdy)))

-- compileNode's Patterns

pattern AST_Var :: forall a. a -> AST a
pattern AST_Var var <- (TC.Var var)

pattern AST_Lit :: forall a. Literal -> AST a
pattern AST_Lit lit <- (Prim _ (PrimLit lit))

pattern AST_NegativeVar :: forall a. a -> AST a
pattern AST_NegativeVar var' <- (App _ (NativeFunc "-") [AST_Var var'])

pattern AST_NegativeLit :: forall a. Literal -> AST a
pattern AST_NegativeLit lit' <- (App _ (NativeFunc "-") [AST_Lit lit'])

pattern AST_NFun :: forall a. a -> Text -> [AST a] -> AST a
pattern AST_NFun node' fn' args' <- (App node' (NativeFunc fn') args')

pattern AST_NFun_Basic :: forall a. Text -> [AST a] -> AST a
pattern AST_NFun_Basic fn' args' <-
  AST_NFun _ (ofBasicOperators -> Right fn') args'

pattern AST_UFun :: forall a. Text -> a -> [AST a] -> [AST a] -> AST a
pattern AST_UFun name' node' bdy' args' <-
  (App node' (UserFunc name' _ bdy') args')

pattern AST_Enforce :: forall a. a -> AST a -> Text -> AST a
pattern AST_Enforce node' app' msg' <-
  (App node' (NativeFunc "enforce") [app', AST_Lit (LString msg')])

pattern AST_If :: forall a. a -> AST a -> AST a -> AST a -> AST a
pattern AST_If node' cond' ifTrue' ifFalse' <-
  (App node' (NativeFunc "if") [cond', ifTrue', ifFalse'])

pattern AST_EnforceKeyset :: forall a. a -> Text -> AST a
pattern AST_EnforceKeyset node' keyset' <-
  (App node' (NativeFunc "enforce-keyset") [AST_Lit (LString keyset')])

pattern AST_Binding :: forall a. a -> [(Named a, AST a)] -> [AST a] -> AST a
pattern AST_Binding node' bindings' bdy' <- (Binding node' bindings' bdy' _)

pattern AST_WithRead :: Node
                     -> Text
                     -> AST Node
                     -> [(Named Node, AST Node)]
                     -> [AST Node]
                     -> AST Node
pattern AST_WithRead node' table' key' bindings' bdy' <-
  (App node'
       (NativeFuncSpecial "with-read" (AST_Binding _ bindings' bdy'))
       [RawTableName table', key'])

pattern AST_Obj :: forall a. a -> [(AST a, AST a)] -> AST a
pattern AST_Obj objNode kvs <- (Object objNode kvs)

pattern AST_InsertOrUpdate node' fnName' table' key' tcId' kvs' <-
  (App node' (isInsertOrUpdate -> (Just fnName')) [RawTableName table', key', AST_Obj (Node tcId' _) kvs'])

pattern AST_InsertOrUpdate :: Node
                           -> Text
                           -> Text
                           -> AST Node
                           -> TcId
                           -> [(AST Node, AST Node)]
                           -> AST Node

pattern AST_Format :: forall a. a -> Text -> [AST a] -> AST a
pattern AST_Format node' fmtStr' args' <-
  (App node' (NativeFunc "format") (AST_Lit (LString fmtStr'):args'))

-- Unsupported currently

pattern AST_Read :: forall a. AST a
pattern AST_Read <- (App _ (NativeFunc "read") _)

pattern AST_AddTime :: forall a. AST a -> AST a -> AST a
pattern AST_AddTime time seconds <- (App _ (NativeFunc "add-time") [time, seconds])

pattern AST_Days :: forall a. AST a -> AST a
pattern AST_Days days <- (App _ (NativeFunc "days") [days])

pattern AST_Bind :: forall a. AST a
pattern AST_Bind <- (App _ (NativeFuncSpecial "bind" _) _)

pattern AST_UnsupportedOp :: forall a. Text -> AST a
pattern AST_UnsupportedOp s <-
  (App _ (NativeFunc (isUnsupportedOperator -> Just s)) _ )

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

class Storable a where

data Term ret where
  IfThenElse     ::                        Term Bool    -> Term a -> Term a -> Term a
  Enforce        ::                        Term Bool    -> String ->           Term ()
  Sequence       :: (Show b, SymWord b) => Term b       -> Term a ->           Term a
  Literal        ::                        SBV a        ->                     Term a
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
  NameAuthorized ::                        KeySetName   ->                     Term Bool
  --
  -- TODO: figure out the object representation we use here:
  --
  -- ObjAuthorized  ::                     Term Obj     ->                     Term Bool

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

type TranslateM = ReaderT (Map Node Text) Maybe

translateBody
  :: (Show a, SymWord a)
  => (AstNodeOf a -> TranslateM (Term a))
  -> [AstNodeOf a]
  -> TranslateM (Term a)
translateBody translator ast
  | length ast >= 1 = do
    ast' <- mapM translator ast
    pure $ foldr1 Sequence ast'
  | otherwise = lift Nothing

translateBodyUnit :: [AstNodeOf ()] -> TranslateM (Term ())
translateBodyUnit = translateBody translateNodeUnit

translateBodyBool :: [AstNodeOf Bool] -> TranslateM (Term Bool)
translateBodyBool = translateBody translateNodeBool

translateBodyInt :: [AstNodeOf Integer] -> TranslateM (Term Integer)
translateBodyInt = translateBody translateNodeInt

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

translateNodeBool :: AstNodeOf Bool -> TranslateM (Term Bool)
translateNodeBool (AstNodeOf node) = case node of
  AST_Lit (LBool b)     -> pure (Literal (literal b))
  AST_Var n -> Var <$> view (ix n)

  AST_NFun_Basic fn args -> do
    let mkComparison
          :: (Show a, SymWord a)
          => (AstNodeOf a -> TranslateM (Term a))
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
          _ -> lift Nothing -- throwError MalformedComparison

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

    if
      -- integer, decimal, string, and time are all comparable. Use the type of
      -- the first argument to decide which to use.
      | isComparison fn -> case args ^? ix 0 . aNode . aTy of
        Just (TyPrim TyInteger) -> mkComparison translateNodeInt
        Just (TyPrim TyDecimal) -> mkComparison translateNodeDecimal
        -- Just (TyPrim TyString) -> mkComparison translateNodeString
        Just (TyPrim TyTime) -> mkComparison translateNodeTime
        _ -> lift Nothing -- throwError MalformedComparison
      | isLogical fn -> mkLogical

  AST_If _ cond tBranch fBranch -> IfThenElse
    <$> translateNodeBool (AstNodeOf cond)
    <*> translateNodeBool (AstNodeOf tBranch)
    <*> translateNodeBool (AstNodeOf fBranch)

-- Pact uses Data.Decimal which is arbitrary-precision
data Decimal = Decimal
  { decimalPlaces :: !Word8
  , decimalMantissa :: !Integer
  } deriving (Show, Read, Eq, Ord, Data, HasKind, SymWord)

mkDecimal :: Decimal.Decimal -> Decimal
mkDecimal (Decimal.Decimal places mantissa) = Decimal places mantissa

translateNodeDecimal :: AstNodeOf Decimal -> TranslateM (Term Decimal)
translateNodeDecimal = unAstNodeOf >>> \case
  AST_Lit (LDecimal d) -> pure (Literal (literal (mkDecimal d)))
  AST_Var n -> Var <$> view (ix n)

-- translateNodeString

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

translateNodeUnit :: AstNodeOf () -> TranslateM (Term ())
translateNodeUnit (AstNodeOf node) = case node of
  AST_If _ cond tBranch fBranch -> IfThenElse
    <$> translateNodeBool (AstNodeOf cond)
    <*> translateNodeUnit (AstNodeOf tBranch)
    <*> translateNodeUnit (AstNodeOf fBranch)

translateNode :: AstNodeOf a -> TranslateM (Term a)
translateNode = unAstNodeOf >>> \case
  -- App (Node _id (TyPrim TyBool)) _ _ -> translateNodeBool

  AST_Lit lit                                         -> undefined
  AST_Var var                                         -> undefined
  AST_Binding node' bindings' body'                   -> undefined
  AST_If node' cond' ifTrue' ifFalse'                 -> undefined
  AST_Read                                            -> undefined
  AST_Days _                                          -> undefined
  AST_Bind                                            -> undefined
  AST_UnsupportedOp s                                 -> lift Nothing
  AST_NFun_Basic fn args                              -> undefined
  -- AST_Enforce _node' app' msg'                        -> do
  --   app''  <- translateNodeBool (AstNodeOf app')
  --   pure $ Enforce app'' (T.unpack msg')
  AST_EnforceKeyset node' ks                          -> undefined
  AST_WithRead node' table' key' bindings' body'      -> undefined
  AST_InsertOrUpdate node' fn' table' key' tcId' kvs' -> undefined
  AST_Format node' fmtStr' args'                      -> undefined
  AST_UFun name' node' body' _args'                   -> undefined

processArgs :: [(Text, Type UserType)] -> Symbolic (Map Text AVar)
processArgs argTys = fmap Map.fromList $ forM argTys $ \(name, ty) -> do
  let name' = T.unpack name
  var <- case ty of
    TyPrim TyInteger -> mkAVar <$> sInteger name'
    TyPrim TyBool    -> mkAVar <$> sBool name'
  pure (name, var)


evalTerm :: (Show a, SymWord a) => Term a -> M (SBV a)
evalTerm = \case
  IfThenElse cond then' else' -> do
    testPasses <- evalTerm cond
    ite testPasses (evalTerm then') (evalTerm else')

  Enforce cond _msg -> do
    cond <- evalTerm cond
    succeeds %= (&&& cond)
    pure $ literal ()

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

  -- AddTimeDec time secs -> do
  --   time' <- evalTerm time
  --   secs' <- evalTerm secs
  --   pure $ time' + sFromIntegral secs'

analyzeFunction
  :: TopLevel Node
  -> IO (Either SmtCompilerException ThmResult)
analyzeFunction (TopFun (FDefun _ _ ty@(FunType argTys retTy) args body' _)) =
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
       TyPrim TyInteger ->
         analyzeFunction' translateBodyInt (.== 1) body' argTys nodeNames'
       TyPrim TyBool    ->
         analyzeFunction' translateBodyBool (.== true) body' argTys nodeNames'

analyzeFunction _ = pure $ Left $ SmtCompilerException "analyzeFunction" "Top-Level Function analysis can only work on User defined functions (i.e. FDefun)"

analyzeFunction'
  :: (Show a, SymWord a)
  => ([AstNodeOf a] -> TranslateM (Term a))
  -> (SBV a -> SBV Bool)
  -> [AST Node]
  -> [(Text, Type UserType)]
  -> Map Node Text
  -> IO (Either SmtCompilerException ThmResult)
analyzeFunction' translator p body' argTys nodeNames =
  -- TODO what type should this be?
  case runReaderT (translator (AstNodeOf <$> body')) nodeNames of
    Nothing -> pure $ Left $ SmtCompilerException "analyzeFunction" "could not translate node"
    Just body'' -> do
      compileFailureVar <- newEmptyMVar
      thmResult <- prove $ do
        scope0 <- processArgs argTys

        let action = evalTerm body''
            env0   = CheckEnv scope0
            state0 = initialCheckState

        case runExcept $ runRWST action env0 state0 of
          Left cf -> do
            liftIO $ putMVar compileFailureVar cf
            pure false
          Right (res, cstate, _log) -> pure (p res)

      mVarVal <- tryTakeMVar compileFailureVar
      case mVarVal of
        Nothing -> pure $ Right thmResult
        -- TODO: return the failure instead of showing it
        Just cf -> pure $ Left $ SmtCompilerException "analyzeFunction" (show cf)

loadModule :: FilePath -> ModuleName -> IO ModuleData
loadModule fp mn = do
  -- XXX(joel): I don't think we should execScript' here
  (r,s) <- execScript' (Script False fp) fp
  either (die def) (const (return ())) r
  case view (rEnv . eeRefStore . rsModules . at mn) s of
    Just m -> return m
    Nothing -> die def $ "Module not found: " ++ show (fp,mn)

loadFun :: FilePath -> ModuleName -> Text -> IO Ref
loadFun fp mn fn = loadModule fp mn >>= \(_,m) -> case HM.lookup fn m of
  Nothing -> die def $ "Function not found: " ++ show (fp,mn,fn)
  Just f -> return f

inferFun :: Bool -> FilePath -> ModuleName -> Text -> IO (TopLevel Node, TcState)
inferFun dbg fp mn fn = loadFun fp mn fn >>= \r -> runTC 0 dbg (typecheckTopLevel r)

runCompiler :: String -> Text -> Text -> IO ()
runCompiler = runCompilerDebug False

runCompilerDebug :: Bool -> String -> Text -> Text -> IO ()
runCompilerDebug dbg replPath' modName' funcName' = do
  f <- fst <$> inferFun dbg replPath' (ModuleName modName') funcName'
  r <- analyzeFunction f
  putStrLn $ case r of
    Left err  -> show err
    Right res -> show res

runCompilerTest :: String -> Text -> Text -> IO (Either SmtCompilerException ThmResult)
runCompilerTest replPath modName funcName = do
  f <- fst <$> inferFun False replPath (ModuleName modName) funcName
  analyzeFunction f
