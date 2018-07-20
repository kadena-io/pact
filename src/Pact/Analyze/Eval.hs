{-# language LambdaCase #-}
module Pact.Analyze.Eval
  ( module Pact.Analyze.Eval.Invariant
  , module Pact.Analyze.Eval.Numerical
  , module Pact.Analyze.Eval.Prop
  , module Pact.Analyze.Eval.Pure
  , module Pact.Analyze.Eval.Term
  , module Pact.Analyze.Eval.Types
  , analyzeCheck
  , analyzeInvariants
  , runInvariantAnalysis
  , runPropertyAnalysis
  ) where


import           Control.Applicative         (ZipList (..))
import           Control.Lens                (view, (<&>), (^.))
import           Control.Monad.Except        (ExceptT)
import           Control.Monad.Morph         (generalize, hoist)
import           Control.Monad.Reader        (runReaderT)
import           Control.Monad.RWS.Strict    (RWST (runRWST))
import           Control.Monad.Trans.Class   (lift)
import           Data.Functor.Identity       (Identity (Identity, runIdentity))
import           Data.SBV                    (Boolean ((==>)), SBV, Symbolic)

import           Pact.Types.Lang             (Info)

import           Pact.Analyze.Errors
import           Pact.Analyze.Eval.Invariant
import           Pact.Analyze.Eval.Numerical
import           Pact.Analyze.Eval.Prop
import           Pact.Analyze.Eval.Pure
import           Pact.Analyze.Eval.Term
import           Pact.Analyze.Eval.Types
import           Pact.Analyze.Types
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
  -> ETerm
  -> ModelTags
  -> Info
  -> ExceptT AnalyzeFailure Symbolic (f AnalysisResult)
runAnalysis' query tables tm tags info = do
  let act    = evalETerm tm >>= \res -> tagResult res >> pure res
      aEnv   = mkAnalyzeEnv tables tags info
      state0 = mkInitialAnalyzeState tables

  (funResult, state1, ()) <- hoist generalize $
    runRWST (runAnalyze act) aEnv state0

  lift $ runConstraints $ state1 ^. globalState.gasConstraints

  let qEnv  = mkQueryEnv aEnv state1 funResult
      ksProvs = state1 ^. globalState.gasKsProvenances

  results <- runReaderT (queryAction query) qEnv
  pure $ results <&> \prop -> AnalysisResult (_sSbv prop) ksProvs

runPropertyAnalysis
  :: Check
  -> [Table]
  -> ETerm
  -> ModelTags
  -> Info
  -> ExceptT AnalyzeFailure Symbolic AnalysisResult
runPropertyAnalysis check tables tm tags info =
  runIdentity <$> runAnalysis' (Identity <$> analyzeCheck check) tables tm tags info

runInvariantAnalysis
  :: [Table]
  -> ETerm
  -> ModelTags
  -> Info
  -> ExceptT AnalyzeFailure Symbolic (TableMap [Located AnalysisResult])
runInvariantAnalysis tables tm tags info =
  unInvariantsF <$> runAnalysis' analyzeInvariants tables tm tags info
