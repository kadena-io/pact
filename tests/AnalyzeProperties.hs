{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module AnalyzeProperties where

import           Control.Monad               ((<=<))
import           Control.Monad.Catch         (catch)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Reader        (ReaderT (runReaderT))
import           Control.Monad.Trans.Class   (MonadTrans (lift))
import           Control.Monad.Trans.Maybe   (MaybeT (runMaybeT))
import           Data.Type.Equality          ((:~:) (Refl))
import           HaskellWorks.Hspec.Hedgehog
import           Hedgehog                    hiding (Update)
import           Numeric.Interval.Exception  (EmptyInterval)
import           Test.Hspec                  (Spec, describe, it, pending)

import           Pact.Analyze.Translate      (maybeTranslateType)
import           Pact.Analyze.Types          hiding (Object, Term)

import           Analyze.Eval
import           Analyze.Gen
import           Analyze.Translate


prop_evaluation :: Property
prop_evaluation = property $ do
  (etm@(ESimple ty _tm), gState) <- forAll genAnyTerm'
  evalEnv <- liftIO $ mkEvalEnv gState

  -- pact setup
  -- TODO: look at what this reads from gState. does it read the named things?
  let Just pactTm = runReaderT (toPactTm etm) (genEnv, gState)

  (do
      -- evaluate via pact, convert to analyze term
      mPactVal <- liftIO $ pactEval pactTm evalEnv
      ePactVal <- case mPactVal of
        UnexpectedErr err  -> footnote err >> failure
        Discard            -> discard
        EvalResult pactVal -> pure $ Right pactVal
        EvalErr err        -> pure $ Left err

      eAnalyzeVal <- liftIO $ analyzeEval etm gState

      case (ePactVal, eAnalyzeVal) of
        (Left _pactErr, Left _analyzeErr) -> success
        (Left pactErr, Right analyzeVal) -> do
          footnote $ "got failure from pact: " ++ pactErr
          footnote $ "got value from analyze: " ++ show analyzeVal
          failure
        (Right pactVal, Left analyzeErr) ->  do
          footnote $ "got value from pact: " ++ show pactVal
          footnote $ "got failure from analyze: " ++ analyzeErr
          failure

        (Right pactVal, Right analyzeVal) -> do
          Just (ESimple ty' (CoreTerm (Lit pactSval)))
            <- lift $ fromPactVal (EType ty) pactVal
          ESimple ty'' (CoreTerm (Lit sval')) <- pure $ analyzeVal

          -- compare results
          case typeEq ty' ty'' of
            Just Refl -> sval' === pactSval
            Nothing   -> EType ty' === EType ty'' -- this'll fail
    )
      -- TODO: is this even on the right block?
      -- see note [EmptyInterval]
      `catch` (\(_e :: EmptyInterval)  -> discard)

prop_round_trip_type :: Property
prop_round_trip_type = property $ do
  ety@(EType ty) <- forAll genType
  maybeTranslateType (reverseTranslateType ty) === Just ety

prop_round_trip_term :: Property
prop_round_trip_term = property $ (do
  (etm@(ESimple ty _tm), gState) <- forAll genAnyTerm'

  etm' <- lift $ runMaybeT $
    (toAnalyze (reverseTranslateType ty) <=< toPactTm' (genEnv, gState)) etm

  etm' === Just etm)
    `catch` (\(_e :: EmptyInterval)  -> discard)

spec :: Spec
spec = describe "analyze properties" $ do
  -- We first check that our translation of types works in both directions.
  -- This is a pre-requisite to...
  it "should round-trip types" $ require prop_round_trip_type

  -- We check that we can translate terms in both directions. This is a
  -- pre-requisite to...
  it "should round-trip terms" $ require prop_round_trip_term

  -- We should be able to evaluate a term both normally and symbolically, and
  -- get the same result in both places.
  it "should evaluate to the same" $ require prop_evaluation

  it "show round-trip userShow / parse" pending

  it "userShow should have the same result on both the pact and analyze side" pending

-- Usually we run via `spec`, but these are useful for running tests
-- sequentially (so logs from different threads don't clobber each other)
sequentialChecks :: IO Bool
sequentialChecks = checkSequential $ Group "checks"
  [ ("prop_round_trip_type", prop_round_trip_type)
  , ("prop_round_trip_term", prop_round_trip_term)
  , ("prop_evaluation", prop_evaluation)
  ]
