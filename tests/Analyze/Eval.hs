{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Analyze.Eval where

import           Bound                    (closed)
import           Control.DeepSeq
import           Control.Exception        (ArithException (DivideByZero), throw)
import           Control.Lens             hiding (op, (...))
import           Control.Monad.Catch      (catch)
import           Control.Monad.Except     (runExcept)
import           Control.Monad.IO.Class   (liftIO)
import           Control.Monad.Reader     (ReaderT (runReaderT))
import           Control.Monad.RWS.Strict (runRWST)
import           Data.Aeson               (Value (Object), toJSON)
import qualified Data.Default             as Default
import qualified Data.HashMap.Strict      as HM
import           Data.List                (isPrefixOf)
import qualified Data.Map.Strict          as Map
import           Data.SBV                 (literal, unliteral, writeArray)
import qualified Data.SBV.Internals       as SBVI
import qualified Data.Text                as T
import           Pact.Types.Pretty        (Doc, viaShow, renderCompactString')

import           Pact.Analyze.Eval        (lasSucceeds, latticeState,
                                           runAnalyze)
import           Pact.Analyze.Eval.Term   (evalETerm)
import           Pact.Analyze.Types       hiding (Object, Term)
import           Pact.Analyze.Types.Eval  (aeRegistry, aeTxMetadata,
                                           tmDecimals, tmIntegers, tmKeySets,
                                           mkAnalyzeEnv, mkPactMetadata,
                                           mkRegistry, mkInitialAnalyzeState,
                                           registryMap)
import           Pact.Analyze.Util        (dummyInfo)

import           Pact.Eval                (reduce)
import           Pact.Repl                (initPureEvalEnv)
import           Pact.Repl.Types          (LibState)
import           Pact.Types.Runtime       (EvalEnv, PactError (..),
                                           PactErrorType (EvalError), eeMsgBody,
                                           runEval)
import qualified Pact.Types.Term          as Pact

import           Analyze.Gen
import           Analyze.Translate

data EvalResult
  = EvalResult !(Pact.Term Pact.Ref)
  | Discard
  | EvalErr !Doc
  | UnexpectedErr !Doc

-- Evaluate a term via Pact
pactEval :: ETerm -> GenState -> IO EvalResult
pactEval etm gState = (do
    pactTm <- case runReaderT (toPactTm etm) (genEnv, gState) of
      Just tm -> pure tm
      Nothing -> error $ "failed to convert term to pact " ++ show etm
    evalEnv <- liftIO $ mkEvalEnv gState

    let evalState = Default.def
    -- evaluate via pact, convert to analyze term
    (pactVal, _) <- runEval evalState evalEnv (reduce pactTm)
    Just pactVal' <- pure $ closed pactVal

    -- Fully evaluate (via deepseq) before returning result so we are sure to
    -- catch any exceptions.
    pure $ EvalResult $ show pactVal' `deepseq` pactVal'
  )
    -- discard division by zero, on either the pact or analysis side
    --
    -- future work here is to make sure that if one side throws, the other
    -- does as well.
    `catch` (\(e :: ArithException) -> case e of
      DivideByZero -> pure $ EvalErr "division by zero"
      _            -> throw e
      )
    `catch` (\(pe@(PactError err _ _ msg) :: PactError) ->
      let msgStr = renderCompactString' msg in case err of
        EvalError ->
          if "Division by 0" `isPrefixOf` msgStr ||
             "Negative precision not allowed" `isPrefixOf` msgStr
          then pure Discard
          else pure $ EvalErr msg
        _ -> case msgStr of
          "(enforce)"     -> pure $ EvalErr msg
          "(enforce-one)" -> pure $ EvalErr msg
          ""              -> pure $ UnexpectedErr $ viaShow pe
          _               -> pure $ UnexpectedErr msg)

-- Evaluate a term symbolically
analyzeEval :: ETerm -> GenState -> IO (Either String ETerm)
analyzeEval etm@(Some ty _tm) gs = analyzeEval' etm ty gs

analyzeEval' :: ETerm -> SingTy a -> GenState -> IO (Either String ETerm)
analyzeEval' etm ty (GenState _ registryKSs txKSs txDecs txInts) = do
  -- analyze setup
  let tables = []
      caps   = []
      args   = Map.empty
      state0 = mkInitialAnalyzeState tables caps

      tags = ModelTags Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty
        -- this 'Located TVal' is never forced so we don't provide it
        (error "analyzeEval: Located TVal unexpectedly forced")
        Map.empty Map.empty

      modName = Pact.ModuleName "test" Nothing
      pactMd  = mkPactMetadata
      reg     = mkRegistry

  Just aEnv <- pure $ mkAnalyzeEnv modName pactMd reg tables caps args tags dummyInfo

  let writeArray' k v env = writeArray env k v

      --
      -- TODO: need to hook this up to authorized-by (NameAuthorized) support
      --
      withRegistryGuards = flip (foldr
          (\(k, v) -> aeRegistry.registryMap %~
            writeArray' (literal (RegistryName (T.pack k))) (literal v)))
        (Map.toList (fmap snd registryKSs))

      --
      -- TODO: handle aeKsAuths
      --
      withKsAuths = id

      withTxKeySets = flip (foldr
        (\(k, v) -> aeTxMetadata.tmKeySets %~
          writeArray' (literal (Str k)) (literal v)))
        (Map.toList (fmap snd txKSs))

      withDecimals = flip (foldr
          (\(k, v) -> aeTxMetadata.tmDecimals %~
            writeArray' (literal (Str k)) (literal v)))
        (Map.toList txDecs)

      withIntegers = flip (foldr
          (\(k, v) -> aeTxMetadata.tmIntegers %~
            writeArray' (literal (Str k)) (literal v)))
        (Map.toList txInts)

      --
      -- TODO: add read-msg support here
      --
      withStrings = id

  -- evaluate via analyze
  (analyzeVal, las)
    <- case runExcept $ runRWST (runAnalyze (evalETerm etm)) (aEnv & withRegistryGuards & withKsAuths & withTxKeySets & withDecimals & withIntegers & withStrings) state0 of
      Right (analyzeVal, las, ()) -> pure (analyzeVal, las)
      Left err                    -> error $ describeAnalyzeFailure err

  case unliteral (las ^. latticeState . lasSucceeds . _Wrapped') of
    Nothing -> pure $ Left $ "couldn't unliteral lasSucceeds"
    Just False -> pure $ Left "fails"
    Just True -> case analyzeVal of
      AVal _ sval -> pure $ case withSymVal ty (unliteral (SBVI.SBV sval)) of
        Just sval' -> Right $ Some ty $ CoreTerm $ Lit sval'
        Nothing    -> Left $ "couldn't unliteral: " ++ show sval
      _ -> pure $ Left $ "not AVAl: " ++ show analyzeVal

-- Generate a pact evaluation environment given the keysets and decimals used
-- in the generated term. This generates an environment with just keysets and
-- decimals.
mkEvalEnv :: GenState -> IO (EvalEnv LibState)
mkEvalEnv (GenState _ registryKSs txKSs txDecs txInts) = do
  evalEnv <- liftIO $ initPureEvalEnv Nothing
  let xformKsMap = HM.fromList
        . fmap (\(k, (pks, _ks)) -> (T.pack k, toJSON pks))
        . Map.toList

      registryKSs' = xformKsMap registryKSs
      txKSs' = xformKsMap txKSs
      txDecs' = HM.fromList
        $ fmap (\(k, v) -> (T.pack k, toJSON (show (toPact decimalIso v))))
        $ Map.toList txDecs
      txInts' = HM.fromList
        $ fmap (\(k, v) -> (T.pack k, toJSON v))
        $ Map.toList txInts
      body = Object $ HM.unions [registryKSs', txKSs', txDecs', txInts']
  pure $ evalEnv & eeMsgBody .~ body
