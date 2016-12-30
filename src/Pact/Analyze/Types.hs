{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Pact.Analyze.Types where

import Control.Monad.Trans.Reader
import Pact.Typecheck
import Pact.Types
import Data.Either
import Data.Decimal
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>),(<$$>))
import Text.Show.Prettyprint (prettyShow, prettyPrint)

data SymType = SymInteger
  | SymDecimal
  | SymBool
  deriving (Show, Eq, Ord)

data TrackingStatus = IsTracked
  | Untracked
  | LostTrack String deriving (Show, Eq)

data SymVar = SymVar
  { _svName :: String
  , _svType :: SymType
  , _svTracked :: TrackingStatus
  } deriving (Show, Eq)

data ProverCtx = ProverCtx
  { _pcVars :: Map TcId SymVar
  , _pcPrevSMT :: [String]
  , _pcNodeSMT :: [String]
  } deriving (Show, Eq)

data SymAst =
  Branch
    { _brLeft :: SymAst
    , _brRight :: SymAst
    , _saProverCtx :: ProverCtx } |
  ConstrainedLeaf
    { _saProverCtx :: ProverCtx } |
  ErrorLeaf
    { _elWhy :: String
    , _saProverCtx :: ProverCtx }

type PactAnalysis = Reader ProverCtx SymAst

_inferGtTen :: IO (Either (String, Fun TcId) (Fun (TcId, Type)), TcState)
_inferGtTen = _infer "examples/analyze-tests/analyze-tests.repl" "analyze-tests" "gt-ten"

getGtTenFun :: IO (Fun (TcId, Type))
getGtTenFun = do
  (Right f, _) <- _inferGtTen
  return f

newtype Smt2Cmd = Smt2Cmd { unSmt2Cmd :: String } deriving (Show)

data LitVal =
    LVDecimal Decimal
  | LVBool Bool
  | LVInteger Integer

-- data Var =
--   Var
--     { _vId :: TcId
--     , _vType :: Type
--     } |
--   Lit
--     { _vId :: TcId
--     , _vType :: Type
--     , _vVal :: LitVal
--     }

pattern NativeFunc f <- (FNative _ f _ _)

pattern Args_Var var <- [(Var _ var)]
pattern Args_Lit lit <- [(Lit _ _ (LVLit (lit)))]
pattern Args_Var_Var var1 var2 <- [(Var _ var1),(Var _ var2)]
pattern Args_Var_Lit var lit <- [(Var _ var),(Lit _ _ (LVLit (lit)))]
pattern Args_Lit_Var lit var <- [(Lit _ _ (LVLit (lit))),(Var _ var)]

pattern GT_Var_Lit_Int name' val' <- (App _ (NativeFunc ">") (Args_Var_Lit (name',_) (LInteger val')))
pattern LT_Var_Lit_Int name' val' <- (App _ (NativeFunc "<") (Args_Var_Lit (name',_) (LInteger val')))

pryAppart :: AST (TcId,Type) -> (TcId, Integer)
pryAppart (GT_Var_Lit_Int n v) = (n,v)
pryAppart (LT_Var_Lit_Int n v) = (n,v)

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
