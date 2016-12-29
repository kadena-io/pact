{-# LANGUAGE OverloadedStrings #-}

module Pact.Analyze.Types where

import Pact.Typecheck
import Pact.Types
import Data.Either
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>),(<$$>))

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



_inferGtTen :: IO (Either (String, Fun TcId) (Fun (TcId, Type)), TcState)
_inferGtTen = _infer "examples/analyze-tests/analyze-tests.repl" "analyze-tests" "gt-ten"

getGtTenFun :: IO (Fun (TcId, Type))
getGtTenFun = do
  (Right f, _) <- _inferGtTen
  return f

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
--                                , _fSpecial = Nothing}
--                                , _aAppArgs = [
--                                    App { _aId = appN>2
--                                        , _aAppFun = FNative { _fInfo = ""
--                                                             , _fName = ">"
--                                                             , _fTypes = (x:<a => (integer|decimal|string|time)> y:<a => (integer|decimal|string|time)>) -> bool :| []
--                                                             , _fSpecial = Nothing}
--                                        , _aAppArgs =
--                                             [ Var { _aId = analyze-tests.gt-ten_a0
--                                                 , _aVar = (analyze-tests.gt-ten_a0,integer)}
--                                             , Lit {_aId = integer3, _aLitType = integer, _aLitValue = LVLit 10}]}
--                                   , Lit { _aId = string4
--                                         , _aLitType = string
--                                         , _aLitValue = LVLit "more than ten"}
--                                   , Lit {_aId = string5
--                                         , _aLitType = string
--                                         , _aLitValue = LVLit "less than ten"}]}]}
