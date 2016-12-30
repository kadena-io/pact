{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

module Pact.Analyze.Types where

import Control.Monad.Trans.RWS.Strict
import Control.Lens
import Pact.Typecheck
import Pact.Types
import Data.Either
import Data.Decimal
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>),(<$$>))
import Text.Show.Prettyprint (prettyShow, prettyPrint)

data SymType = SymInteger
  | SymDecimal
  | SymBool
  deriving (Show, Eq, Ord)

newtype SymName = SymName { unSymName :: String } deriving (Show, Eq)
newtype Smt2Cmd = Smt2Cmd { unSmt2Cmd :: String } deriving (Show, Eq)

data TrackingStatus = Tracked
  | Untracked {_tWhy :: String}
  | LostTrack {_tWhy :: String}
  deriving (Show, Eq)

data SymVar = SymVar
  { _svName :: SymName
  , _svType :: Maybe SymType
  , _svTracked :: TrackingStatus
  } deriving (Show, Eq)
makeLenses ''SymVar

data ProverCtx = ProverCtx
  { _pcInputArgs :: Map TcId SymVar
  } deriving (Show, Eq)
makeLenses ''ProverCtx

data ProverState = ProverState
  { _psVars :: Map TcId SymVar
  , _psPrevSMT :: [Smt2Cmd]
  , _psNodeSMT :: [Smt2Cmd]
  } deriving (Show, Eq)
makeLenses ''ProverState

data SymAst =
  Branch
    { _brRegularLeaf :: SymAst
    , _brNegatedLeaf :: SymAst
    , _saProverState :: ProverState } |
  ConstrainedLeaf
    { _saProverState :: ProverState } |
  ErrorLeaf
    { _elWhy :: String
    , _saProverState :: ProverState }

type PactAnalysis a = RWST ProverCtx () ProverState IO a

data LitVal =
    LVDecimal Decimal
  | LVBool Bool
  | LVInteger Integer

pattern NativeFunc f <- (FNative _ f _ _)

pattern Args_Var var <- [(Var _ var)]
pattern Args_Lit lit <- [(Lit _ _ (LVLit (lit)))]
pattern Args_Var_Var var1 var2 <- [(Var _ var1),(Var _ var2)]
pattern Args_Var_Lit var lit <- [(Var _ var),(Lit _ _ (LVLit (lit)))]
pattern Args_Lit_Var lit var <- [(Lit _ _ (LVLit (lit))),(Var _ var)]
pattern Args_App_Lit_Var app' lit var <- [app',(Lit _ _ (LVLit (lit))),(Var _ var)]
pattern Args_App_Lit_Lit app' lit1 lit2 <- [app',(Lit _ _ (LVLit (lit1))),(Lit _ _ (LVLit (lit2)))]

pattern NativeFunc_Var_Lit_Int f var' val' <- (App _ (NativeFunc f) (Args_Var_Lit var' (LInteger val')))
pattern IF_App_Lit_Lit app' lit1 lit2 <- (App _ (NativeFunc "if") (Args_App_Lit_Lit app' lit1 lit2))

cmpOperators :: Set String
cmpOperators = Set.fromList [">", "<", ">=", "<=", "="]

analyze :: AST (TcId,Type) -> PactAnalysis SymAst
analyze (NativeFunc_Var_Lit_Int f v@(name', _) l)
-- Comparison Operators
  | Set.member f cmpOperators = do
      updatePrevCommands
      updateTrackedVars v
      state' <- get
      sv <- getSymVar name'
      if isTracked sv
      then do
        (regular', negated') <- return $! createCmpAssertions f (unSymName (view svName sv)) (show l)
        return $ Branch
          { _brRegularLeaf = ConstrainedLeaf (state' { _psNodeSMT = [regular'] })
          , _brNegatedLeaf = ConstrainedLeaf (state' { _psNodeSMT = [negated'] })
          , _saProverState = state' }
      else return $ ErrorLeaf
          { _elWhy = "Variable " ++ show (view svName sv) ++ " is not tracked"
          , _saProverState = state' }

createCmpAssertions :: String -> String -> String -> (Smt2Cmd, Smt2Cmd)
createCmpAssertions f a b = (regular', negated')
  where
    regular' = Smt2Cmd $ "(assert (> " ++ a ++ " " ++ b ++ "))"
    negated' = Smt2Cmd $ "(assert (not (> " ++ a ++ " " ++ b ++ ")))"

updateTrackedVars :: (TcId,Type) -> PactAnalysis ()
updateTrackedVars (name', type') = do
  isInputArg <- Map.member name' <$> view pcInputArgs
  alreadyTracked <- Map.member name' <$> use psVars
  if isInputArg || alreadyTracked
    then return ()
    else do
      psVars %= Map.insert name' (constructSymVar (name', type'))

updatePrevCommands :: PactAnalysis ()
updatePrevCommands = do
  lastCmds <- use psNodeSMT
  psPrevSMT %= (\prevCmds -> prevCmds ++ lastCmds)
  psNodeSMT .= []

convertName :: TcId -> SymName
convertName = SymName . show

convertType :: Type -> Maybe SymType
convertType TyInteger = Just SymInteger
convertType TyBool = Just SymBool
convertType TyDecimal = Just SymDecimal
convertType _ = Nothing

constructSymVar :: (TcId, Type) -> SymVar
constructSymVar (name', type') = newVar
  where
    convType = convertType type'
    convName = convertName name'
    trackingStatus = case convType of
      Nothing -> Untracked "Unsupported Type"
      Just _ -> Tracked
    newVar = SymVar { _svName = convName, _svType = convType, _svTracked = trackingStatus}

getSymVar :: TcId -> PactAnalysis SymVar
getSymVar name' = do
  sv1 <- Map.lookup name' <$> view pcInputArgs
  case sv1 of
    Just sv' -> return sv'
    Nothing -> do
      sv2 <- Map.lookup name' <$> use psVars
      case sv2 of
        Just sv' -> return sv'
        Nothing -> do
          reader' <- ask
          state' <- get
          error $ "unable to lookup variable: " ++ show name'
            ++ "\n### context ###\n" ++ show reader'
            ++ "\n### state ###\n" ++ show state'

isTracked :: SymVar -> Bool
isTracked (SymVar _ _ Tracked) = True
isTracked _ = False
-- (module analyze-tests 'analyze-admin-keyset
--   (defun gt-ten (a:integer)
--     (if (> a 10) "more than ten" "less than ten")
--   )
-- )
--
-- FDefun
--   { _fInfo = (defun gt-ten (a:integer)
--   , _fName = "analyze-tests.gt-ten"
--   , _fType = (a:integer) -> <a>
--   , _fArgs = [ (analyze-tests.gt-ten_a0,integer) ]
--   , _fBody = [
--       App { _aId = appNif1
--           , _aAppFun = FNative { _fInfo = ""
--                                , _fName = "if"
--                                , _fTypes = (cond:bool then:<a> else:<a>) -> <a> :| []
--                                , _fSpecial = Nothing
--                                }
--           , _aAppArgs = [
--               App { _aId = appN>2
--                   , _aAppFun = FNative { _fInfo = ""
--                                        , _fName = ">"
--                                        , _fTypes = (x:<a => (integer|decimal|string|time)> y:<a => (integer|decimal|string|time)>) -> bool :| []
--                                        , _fSpecial = Nothing}
--                   , _aAppArgs =
--                        [ Var { _aId = analyze-tests.gt-ten_a0
--                              , _aVar = (analyze-tests.gt-ten_a0,integer)}
--                        , Lit {_aId = integer3, _aLitType = integer, _aLitValue = LVLit 10}]}
--              , Lit { _aId = string4
--                    , _aLitType = string
--                    , _aLitValue = LVLit "more than ten"}
--              , Lit {_aId = string5
--                    , _aLitType = string
--                    , _aLitValue = LVLit "less than ten"}]}]}

_inferGtTen :: IO (Either (String, Fun TcId) (Fun (TcId, Type)), TcState)
_inferGtTen = _infer "examples/analyze-tests/analyze-tests.repl" "analyze-tests" "gt-ten"

getGtTenFun :: IO (Fun (TcId, Type))
getGtTenFun = do
  (Right f, _) <- _inferGtTen
  return f
