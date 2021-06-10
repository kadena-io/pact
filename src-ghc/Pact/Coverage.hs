{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Pact.Coverage
-- Copyright   :  (C) 2021 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Implement advice to provide LCOV coverage data.
--

module Pact.Coverage
  ( mkCoverageAdvice
  ) where

import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Control.Lens
import Data.Default
import Data.IORef
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Text.Encoding
import Text.Trifecta.Delta

import Pact.Coverage.Report
import Pact.Eval
import Pact.Types.Advice
import Pact.Types.Info
import Pact.Types.Pretty
import Pact.Types.Term
import Pact.Types.Runtime
import Pact.Repl
import Pact.Repl.Types


mkCoverageAdvice :: IO (IORef LcovReport,Advice)
mkCoverageAdvice = newIORef mempty >>= \r -> return (r,Advice $ cover r)

cover :: MonadIO m => IORef LcovReport -> Info -> AdviceContext -> m a -> m a
cover ref i ctx f = case _iInfo i of
    Just {} -> report (parseInf i) >> f
    _ -> f
  where

    report (fn,l) = liftIO $ modifyIORef ref (<> newRep)
      where
        newRep = case ctx of
          AdviceUser (fdef,_) -> fr <> mkFunLcov fdef
          AdviceModule m -> fr <> mkModuleLcov m
          _ -> fr
        fr = mkFileLcov fn mempty mempty $ lnReport l

    mkFileLcov fileName funReps brReps lnReps = LcovReport $ HM.singleton fileName
        (FileReport fileName funReps brReps lnReps)

    lnReport l = lnReport' l 1

    lnReport' l count = HM.singleton l (LineReport l count)

    lnReport'' hi count = lnReport' l count where (_,l) = parseInf (getInfo hi)

    mkFunLcov fdef = mkFileLcov fn (mkFunReport fdef 1 l) mempty $ lnReport l
      where
        (fn,l) = parseInf (_dInfo fdef)

    mkFunReport fdef count fl = HM.singleton dn $ FunctionReport fl dn count
      where
        dn = renderCompactText $ derefDef fdef

    mkModuleLcov md = mkFileLcov fn modFuns mempty $ (HM.union (lnReport l) modLines)
      where
        (fn,l) = parseInf i
        (modFuns,modLines) = mkModFuns md
        mkModFuns (MDInterface {},_) = mempty -- no need to track interface funs
        mkModFuns (MDModule {},bod) = case instantiate' bod of
          TList ds _ _ ->
            let modBody (fs,ls) (TDef d di) = (HM.union fs fm, HM.union ls lm)
                  where
                    fm = mkFunReport d 0 (snd (parseInf di))
                    lm = mkFunLines d di
                modBody r _ = r
            in V.foldl' modBody (mempty,mempty) ds
          _ -> (mempty,mempty)

    mkFunLines d di = HM.union (lnReport'' di 0) $ case instantiate' (_dDefBody d) of
          TList ds _ _ -> HM.unions $ (`map` V.toList ds) $ \t -> case t of
              TApp {} -> lnReport'' t 0
              _ -> mempty
          _ -> mempty

    parseInf (Info m) = case m of
      Just (_,Parsed (Directed fn l _ _ _) _) -> (T.unpack $ decodeUtf8 fn,succ $ fromIntegral l)
      Just (_,Parsed (Lines l _ _ _) _) -> nofile $ succ $ fromIntegral l
      _ -> nofile 0
      where nofile l = ("<interactive>",l)

-- _cover "examples/accounts/accounts.repl"
_cover :: FilePath -> IO ()
_cover fn = do
  (ref,adv) <- mkCoverageAdvice
  s <- set (rEnv . eeAdvice) adv <$> initReplState (Script False fn) Nothing
  void $! runStateT (useReplLib >> loadFile def fn) s
  readIORef ref >>= writeReport "cov.lcov"
