{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}
module Pact.Analyze.Eval.Invariant where

import           Control.Lens            (at, view)
import           Control.Monad.Except    (MonadError (throwError))
import           Control.Monad.Reader    (MonadReader, ReaderT)
import           Data.Map.Strict         (Map)

import           Pact.Analyze.Errors
import           Pact.Analyze.Eval.Core
import           Pact.Analyze.Types.Eval
import           Pact.Analyze.Types

-- TODO Change SVal to AVal (allowing objects), update analyzer
newtype InvariantCheck a = InvariantCheck
  { unInvariantCheck :: ReaderT
    (Located (Map VarId AVal))
    (Either AnalyzeFailure) a
  } deriving (Functor, Applicative, Monad, MonadError AnalyzeFailure,
    MonadReader (Located (Map VarId AVal)))

instance Analyzer InvariantCheck where
  type TermOf InvariantCheck = Invariant
  eval  (PureInvariant tm)   = evalCore tm
  evalO (PureInvariant tm)   = evalCoreO tm
  throwErrorNoLoc err = do
    info <- view location
    throwError $ AnalyzeFailure info err
  getVar vid = view (located . at vid)
