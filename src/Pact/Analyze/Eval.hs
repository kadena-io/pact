{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Pact.Analyze.Eval
  ( module Pact.Analyze.Eval.Invariant
  , module Pact.Analyze.Eval.Numerical
  , module Pact.Analyze.Eval.Prop
  , module Pact.Analyze.Eval.Core
  , module Pact.Analyze.Eval.Term
  , module Pact.Analyze.Types.Eval
  , analyzeCheck
  , analyzeInvariants
  , runInvariantAnalysis
  , runPropertyAnalysis
  ) where


import           Control.Applicative         (ZipList (..))
import           Control.Lens                (view, (&), (.~), (<&>), (^.))
import           Control.Monad.Except        (ExceptT, throwError)
import           Control.Monad.Morph         (generalize, hoist)
import           Control.Monad.Reader        (runReaderT)
import           Control.Monad.RWS.Strict    (RWST (runRWST))
import           Control.Monad.State.Strict  (runStateT)
import           Control.Monad.Trans.Class   (lift)
import           Data.Functor.Identity       (Identity (Identity, runIdentity))
import           Data.Map.Strict             (Map)
import           Data.SBV                    (Boolean ((==>)), SBV, Symbolic,
                                              true)
import           Data.String                 (fromString)

import           Pact.Types.Lang             (Info)

import           Pact.Analyze.Errors
import           Pact.Analyze.Eval.Core
import           Pact.Analyze.Eval.Invariant
import           Pact.Analyze.Eval.Numerical
import           Pact.Analyze.Eval.Prop
import           Pact.Analyze.Eval.Term
import           Pact.Analyze.Types
import           Pact.Analyze.Types.Eval
import           Pact.Analyze.Util

analyzeCheck :: Check -> Query (S Bool)
analyzeCheck = \case
    PropertyHolds p -> assumingSuccess =<< evalProp p
    Valid p         -> evalProp p
    Satisfiable p   -> evalProp p

  where
    assumingSuccess :: S Bool -> Query (S Bool)
    assumingSuccess p = do
      success <- view (qeAnalyzeState.succeeds)
      pure $ success ==> p

-- | A convenience to treat a nested 'TableMap', '[]', and tuple as a single
-- functor instead of three.
newtype InvariantsF a = InvariantsF { unInvariantsF :: TableMap [Located a] }

instance Functor InvariantsF where
  fmap f (InvariantsF a) = InvariantsF ((fmap . fmap . fmap) f a)

analyzeInvariants :: Query (InvariantsF (S Bool))
analyzeInvariants = assumingSuccess =<< invariantsHold''
  where
    assumingSuccess :: InvariantsF (S Bool) -> Query (InvariantsF (S Bool))
    assumingSuccess ps = do
      success <- view (qeAnalyzeState.succeeds)
      pure $ (success ==>) <$> ps

    invariantsHold :: Query (TableMap (ZipList (Located (SBV Bool))))
    invariantsHold = view (qeAnalyzeState.maintainsInvariants)

    invariantsHold' :: Query (InvariantsF (SBV Bool))
    invariantsHold' = InvariantsF <$> (getZipList <$$> invariantsHold)

    invariantsHold'' :: Query (InvariantsF (S Bool))
    invariantsHold'' = sansProv <$$> invariantsHold'

-- | Helper to run either property or invariant analysis
runAnalysis'
  :: Functor f
  => Query (f (S Bool))
  -> [Table]
  -> Map VarId AVal
  -> ETerm
  -> ModelTags 'Symbolic
  -> Info
  -> ExceptT AnalyzeFailure Symbolic (f AnalysisResult)
runAnalysis' query tables args tm tags info = do
  let act    = evalETerm tm >>= \res -> tagResult res >> pure res
      state0 = mkInitialAnalyzeState tables

  aEnv <- case mkAnalyzeEnv tables args tags info of
    Just env -> pure env
    Nothing  -> throwError $ AnalyzeFailure info $ fromString
      "Unable to make analyze env (couldn't translate schema)"

  (funResult, state1, ()) <- hoist generalize $
    runRWST (runAnalyze act) aEnv state0

  lift $ runConstraints $ state1 ^. globalState.gasConstraints

  let cv0     = state0 ^. latticeState . lasExtra
      cv1     = state1 ^. latticeState . lasExtra
      state1' = state1 &  latticeState . lasExtra .~ ()
      qEnv    = mkQueryEnv aEnv state1' cv0 cv1 funResult
      ksProvs = state1 ^. globalState.gasKsProvenances

  (results, querySucceeds)
    <- runReaderT (runStateT (queryAction query) true) qEnv
  pure $ results <&> \prop -> AnalysisResult querySucceeds (_sSbv prop) ksProvs

runPropertyAnalysis
  :: Check
  -> [Table]
  -> Map VarId AVal
  -> ETerm
  -> ModelTags 'Symbolic
  -> Info
  -> ExceptT AnalyzeFailure Symbolic AnalysisResult
runPropertyAnalysis check tables args tm tags info =
  runIdentity <$>
    runAnalysis' (Identity <$> analyzeCheck check) tables args tm tags info

runInvariantAnalysis
  :: [Table]
  -> Map VarId AVal
  -> ETerm
  -> ModelTags 'Symbolic
  -> Info
  -> ExceptT AnalyzeFailure Symbolic (TableMap [Located AnalysisResult])
runInvariantAnalysis tables args tm tags info =
  unInvariantsF <$> runAnalysis' analyzeInvariants tables args tm tags info
