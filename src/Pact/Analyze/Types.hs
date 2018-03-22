{-# language DeriveAnyClass     #-}
{-# language DeriveDataTypeable #-}
{-# language DeriveGeneric      #-}
{-# language RecordWildCards    #-}
{-# language FlexibleContexts   #-}
{-# language LambdaCase         #-}
{-# language OverloadedStrings  #-}
{-# language PatternSynonyms    #-}
{-# language TemplateHaskell    #-}
{-# language TypeApplications   #-}
{-# language ViewPatterns       #-}
{-# language Rank2Types         #-}
{-# language GADTs              #-}
{-# language StandaloneDeriving #-}
{-# language FlexibleInstances  #-}

module Pact.Analyze.Types
  ( runCompiler
  , runCompilerDebug
  , runCompilerTest
  , analyzeFunction
  , TableAccess(..)
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
import Pact.Typechecker hiding (debug)
import Pact.Types.Lang hiding (Term)
import Pact.Types.Runtime hiding (Term)
import Pact.Types.Typecheck hiding (Var)
import qualified Pact.Types.Typecheck as TC
import Pact.Repl
import Data.Aeson hiding (Object, (.=))
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

data TableAccess =
  TableRead |
  TableWrite
  deriving (Show, Eq, Generic, ToJSON)

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
  = MalformedArithmeticOp ArithOp [Term Integer]
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

ofBasicOperators :: Text -> Either Text Text
ofBasicOperators s = if isBasic then Right s else Left s
  where
    isBasic     = isCmp || isLogical || isNumerical
    -- FIXME: inefficient
    isCmp       = Set.member s $ Set.fromList [">", "<", ">=", "<=", "=", "!="]
    isLogical   = Set.member s $ Set.fromList ["and", "or", "not"]
    isNumerical = Set.member s $ Set.fromList ["+", "-", "*", "/", "abs", "^"]

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

data ArithOp = Add | Mul | Abs | Signum | Negate
  deriving (Show, Eq)

data ComparisonOp = Gt | Lt | Gte | Lte | Eq | Neq
  deriving (Show, Eq)

class Storable a where

data Term ret where
  IfThenElse ::    Term Bool -> Term a -> Term a -> Term a
  Enforce    ::              Term Bool -> String -> Term ()
  Sequence   ::   (Show b, SymWord b) =>  Term b -> Term a -> Term a
  Literal    ::                            SBV a -> Term a
  Read       ::                     Term Integer -> Term a
  Let        ::   Show a =>   String  -> Term a -> Term b -> Term b
  Var        ::                           Text -> Term a
  Write      ::   Show a =>        Term Integer -> Term a -> Term ()
  Throw      ::                           String -> Term ()
  Arith      ::        ArithOp -> [Term Integer] -> Term Integer
  Comparison :: (Show a, SymWord a) => ComparisonOp -> Term a -> Term a -> Term Bool

deriving instance Show a => (Show (Term a))

instance Num (Term Integer) where
  fromInteger = Literal . fromInteger
  x + y = Arith Add [x, y]
  x * y = Arith Mul [x, y]
  abs x = Arith Abs [x]
  signum x = Arith Signum [x]
  negate x = Arith Negate [x]

newtype AstNodeOf a = AstNodeOf { unAstNodeOf :: AST Node }

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

    -- integer, decimal, string, and time are all comparable. Use the type of
    -- the first argument to decide which to use.
    case args ^? ix 0 . aNode . aTy of
      Just (TyPrim TyInteger) -> mkComparison translateNodeInt
      Just (TyPrim TyDecimal) -> mkComparison translateNodeDecimal
      -- Just (TyPrim TyString) -> mkComparison translateNodeString
      Just (TyPrim TyTime) -> mkComparison translateNodeTime
      _ -> lift Nothing -- throwError MalformedComparison

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
  AST_Lit (LDecimal d) ->
    pure (Literal (literal (mkDecimal d)))
  AST_Var n -> Var <$> view (ix n)

-- translateNodeString

-- I copied this from Data.SBV.Examples.Puzzles.U2Bridge but it will definitely
-- not work in practice.
type Time = Word32

translateNodeTime :: AstNodeOf Time -> TranslateM (Term Time)
translateNodeTime = unAstNodeOf >>> \case
  -- Tricky: seconds could be either integer or decimal
  AST_AddTime time seconds
    | seconds ^. aNode . aTy == TyPrim TyTime -> undefined

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
  AST_AddTime _ _                                     -> undefined
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

declareTopLevelVars :: M ()
-- TODO(joel)
declareTopLevelVars = pure ()

symbolicEval :: (Show a, SymWord a) => Term a -> M (SBV a)
symbolicEval = \case
  IfThenElse cond then' else' -> do
    testPasses <- symbolicEval cond
    ite testPasses (symbolicEval then') (symbolicEval else')

  Enforce cond _msg -> do
    cond <- symbolicEval cond
    succeeds %= (&&& cond)
    pure $ literal ()

  Sequence a b -> symbolicEval a *> symbolicEval b

  Literal a -> pure a

  Var name -> do
    -- Assume the term is well-scoped after typechecking
    Just val <- view (scope . at name)
    -- Assume the variable is well-typed after typechecking
    pure $ unsafeCastAVar val

  Arith op args -> do
    args' <- forM args symbolicEval
    case (op, args') of
      (Add, [x, y]) -> pure $ x + y
      (Mul, [x, y]) -> pure $ x * y
      (Abs, [x])    -> pure $ abs x
      (Signum, [x]) -> pure $ signum x
      (Negate, [x]) -> pure $ negate x
      _             -> throwError $ MalformedArithmeticOp op args

  Comparison op x y -> do
    x' <- symbolicEval x
    y' <- symbolicEval y
    pure $ case op of
      Gt  -> x' .> y'
      Lt  -> x' .< y'
      Gte -> x' .>= y'
      Lte -> x' .<= y'
      Eq  -> x' .== y'
      Neq -> x' ./= y'

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
        argVars <- forM argTys $ \(name, ty) -> do
          let name' = T.unpack name
          var <- case ty of
            TyPrim TyInteger -> mkAVar <$> sInteger name'
            TyPrim TyBool    -> mkAVar <$> sBool name'
          pure (name, var)

        let action = declareTopLevelVars >> symbolicEval body''
            env0   = CheckEnv (Map.fromList argVars)
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
