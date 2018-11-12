{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}
module Pact.Analyze.Eval.Invariant where

import           Control.Lens               (at, view, (%=))
import           Control.Monad.Except       (MonadError (throwError))
import           Control.Monad.Reader       (MonadReader, ReaderT)
import           Control.Monad.State.Strict (MonadState,
                                             StateT(StateT, runStateT))
import           Data.Map.Strict            (Map)
import           Data.SBV                   (Mergeable(symbolicMerge),
                                             bnot, (&&&))

import           Pact.Analyze.Errors
import           Pact.Analyze.Eval.Core
import           Pact.Analyze.Types
import           Pact.Analyze.Types.Eval

-- TODO Change SVal to AVal (allowing objects), update analyzer
newtype InvariantCheck a = InvariantCheck
  { unInvariantCheck :: StateT SymbolicSuccess
    (ReaderT
    (Located (Map VarId AVal))
    (Either AnalyzeFailure)) a
  } deriving (Functor, Applicative, Monad, MonadError AnalyzeFailure,
    MonadReader (Located (Map VarId AVal)), MonadState SymbolicSuccess)

instance (Mergeable a) => Mergeable (InvariantCheck a) where
  symbolicMerge force test left right = InvariantCheck $ StateT $ \s0 -> do
    (resL, sL) <- runStateT (unInvariantCheck left) s0
    (resR, sR) <- runStateT (unInvariantCheck right) s0
    pure ( symbolicMerge force test resL resR
         , symbolicMerge force test sL   sR
         )

instance Analyzer InvariantCheck where
  type TermOf InvariantCheck = Invariant
  eval  (CoreInvariant tm)   = evalCore tm
  evalO (CoreInvariant tm)   = evalCoreO tm
  throwErrorNoLoc err = do
    info <- view location
    throwError $ AnalyzeFailure info err
  getVar vid = view (located . at vid)
  markFailure b = id %= (&&& SymbolicSuccess (bnot b))
