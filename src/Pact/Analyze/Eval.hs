{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Toplevel functions for symbolic evaluation of pact programs with respect
-- to properties and invariants, primarily used by the high-level interface in
-- 'Pact.Analyze.Check'.
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
import           Data.SBV                    (SBV, Symbolic)
import qualified Data.SBV                    as SBV
import           Data.String                 (fromString)

import           Pact.Types.Lang             (Info, ModuleName)

import           Pact.Analyze.Alloc          (runAlloc)
import           Pact.Analyze.Errors
import           Pact.Analyze.Eval.Core
import           Pact.Analyze.Eval.Invariant
import           Pact.Analyze.Eval.Numerical
import           Pact.Analyze.Eval.Prop
import           Pact.Analyze.Eval.Term
import           Pact.Analyze.Types          hiding (Core(Identity))
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
      pure $ success .=> p

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
      pure $ (success .=>) <$> ps

    invariantsHold :: Query (TableMap (ZipList (Located (SBV Bool))))
    invariantsHold = view (qeAnalyzeState.maintainsInvariants)

    invariantsHold' :: Query (InvariantsF (SBV Bool))
    invariantsHold' = InvariantsF <$> (fmap getZipList <$> invariantsHold)

    invariantsHold'' :: Query (InvariantsF (S Bool))
    invariantsHold'' = fmap sansProv <$> invariantsHold'

-- | Helper to run either property or invariant analysis
runAnalysis'
  :: Functor f
  => ModuleName
  -> Query (f (S Bool))
  -> [Table]
  -> [Capability]
  -> Map VarId AVal
  -> ETerm
  -> Path
  -> ModelTags 'Symbolic
  -> Info
  -> ExceptT AnalyzeFailure Symbolic (f AnalysisResult)
runAnalysis' modName query tables caps args tm rootPath tags info = do
  let --
      --
      -- TODO: pass this in (from a previous analysis) when we analyze >1
      --       function calls. we will want to propagate previous db state too.
      --
      reg = mkRegistry
      --
      -- TODO: potentially pre-set this metadata in some circumstances once we
      --       add support for analyzing pacts:
      --
      pactMetadata = mkPactMetadata

  aEnv <- case mkAnalyzeEnv modName pactMetadata reg tables caps args tags info of
    Just env -> pure env
    Nothing  -> throwError $ AnalyzeFailure info $ fromString
      "Unable to make analyze env (couldn't translate schema)"

  let state0 = mkInitialAnalyzeState tables caps

      analysis = do
        tagSubpathStart rootPath sTrue
        res <- evalETerm tm
        tagResult res
        pure res

  (funResult, state1, ()) <- hoist generalize $
    runRWST (runAnalyze analysis) aEnv state0

  lift $ SBV.constrain $ _sSbv $ state1 ^. latticeState.lasConstraints

  let cv0     = state0 ^. latticeState . lasExtra
      cv1     = state1 ^. latticeState . lasExtra
      state1' = state1 &  latticeState . lasExtra .~ ()
      qEnv    = mkQueryEnv aEnv state1' cv0 cv1 funResult
      ksProvs = state1 ^. globalState.gasGuardProvenances

  (results, querySucceeds)
    <- hoist runAlloc $ runReaderT (runStateT (queryAction query) sTrue) qEnv
  pure $ results <&> \prop -> AnalysisResult querySucceeds (_sSbv prop) ksProvs

runPropertyAnalysis
  :: ModuleName
  -> Check
  -> [Table]
  -> [Capability]
  -> Map VarId AVal
  -> ETerm
  -> Path
  -> ModelTags 'Symbolic
  -> Info
  -> ExceptT AnalyzeFailure Symbolic AnalysisResult
runPropertyAnalysis modName check tables caps args tm rootPath tags info =
  runIdentity <$>
    runAnalysis' modName (Identity <$> analyzeCheck check) tables caps args tm
      rootPath tags info

runInvariantAnalysis
  :: ModuleName
  -> [Table]
  -> [Capability]
  -> Map VarId AVal
  -> ETerm
  -> Path
  -> ModelTags 'Symbolic
  -> Info
  -> ExceptT AnalyzeFailure Symbolic (TableMap [Located AnalysisResult])
runInvariantAnalysis modName tables caps args tm rootPath tags info =
  unInvariantsF <$>
    runAnalysis' modName analyzeInvariants tables caps args tm rootPath tags info
