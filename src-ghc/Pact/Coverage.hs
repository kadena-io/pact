{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

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

import Bound
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Control.Lens (set)
import Data.Default
import Data.Foldable
import Data.IORef
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.Text.Encoding
import Text.Trifecta.Delta

import Pact.Coverage.Report
import Pact.Typechecker
import Pact.Types.Advice
import Pact.Types.Info
import Pact.Types.Pretty
import Pact.Types.Term hiding (App(..),Object(..),Step(..))
import Pact.Types.Typecheck
import Pact.Types.Runtime (ModuleData(..),eeAdvice)
import Pact.Repl
import Pact.Repl.Types


mkCoverageAdvice :: IO (IORef LcovReport,Advice)
mkCoverageAdvice = newIORef mempty >>= \r -> return (r,Advice $ cover r)

cover :: MonadIO m => IORef LcovReport -> Info -> AdviceContext r -> m (r,a) -> m a
cover ref i ctx f = case _iInfo i of
    Just {} -> do
      post <- report (parseInf i)
      (r,a) <- f
      post r
      return a
    _ -> fmap snd f
  where

    report (fn,l) = liftIO $ modifyIORef ref (<> newRep) >> return post
      where
        newRep = case ctx of
          AdviceUser (fdef,_) -> fr <> mkFunLcov fdef
          -- AdviceModule m -> fr <> mkModuleLcov m
          _ -> fr
        post = case ctx of
          AdviceModule m -> postModule m
          _ -> const $ return ()
        fr = mkFileLcov fn mempty mempty $ lnReport l

    mkFileLcov fileName funReps brReps lnReps = LcovReport $ HM.singleton fileName
        (FileReport fileName funReps brReps lnReps)

    lnReport l = lnReport' l 1

    lnReport' l count = HM.singleton l (LineReport l count)

    lnReport'' hi count = lnReport' l count where (_,l) = parseInf hi

    mkFunLcov fdef = mkFileLcov fn (mkFunReport fdef 1 l) mempty $ lnReport l
      where
        (fn,l) = parseInf (_dInfo fdef)

    mkFunReport fdef count fl = HM.singleton dn $ FunctionReport fl dn count
      where
        dn = renderCompactText $ derefDef fdef

    postModule :: MonadIO m => (ModuleDef (Term Name),Scope () Term Name) -> ModuleData Ref -> m ()
    postModule _ (ModuleData (MDModule _m) modDefs) = do
      ((modFuns,modLines),_) <- liftIO $ runTC 0 False $
        foldM walkDefs (mempty,mempty) (HM.elems modDefs)
      let (fn,_l) = parseInf i
          newRep = mkFileLcov fn modFuns mempty modLines
      liftIO $ modifyIORef ref (<> newRep)
    postModule _ _ = return ()

    walkDefs acc@(fs,ls) mem = case mem of
        Ref r -> do
          tl <- mkTop (fmap Left r)
          case tl of
            (TopFun FDefun {..} _) -> do
              let fm = HM.singleton dn $ FunctionReport l dn 0
                  (_fn,l) = parseInf _fInfo
                  dn = renderCompactText _fModule <> "." <> _fName
                  lns = HM.unions $ map astRep _fBody
              return (HM.union fs fm,HM.unions [ls,lns,lnReport'' _fInfo 0])
            _ -> return acc
        _ -> return acc

    astRep :: AST Node -> HM.HashMap Int LineReport
    astRep App {..} = HM.unions $ [lnReport'' _aNode 0,specials] ++ map astRep _aAppArgs
        where specials = case _aAppFun of
                (FNative _ _ _ (Just (_,SBinding bast))) -> astRep bast
                _ -> mempty
    astRep List {..} = HM.unions $ map astRep _aList
    astRep Object {..} = HM.unions $ map astRep $ toList _aObject
    astRep Step {..} = HM.union (astRep _aExec) $ maybe mempty astRep _aRollback
    astRep Binding {..} = HM.unions $ map (astRep . snd) _aBindings ++ map astRep _aBody
    astRep _ = mempty

parseInf :: HasInfo i => i -> (FilePath,Int)
parseInf inf = case getInfo inf of
  Info (Just (_,Parsed (Directed fn l _ _ _) _)) -> (T.unpack $ decodeUtf8 fn,succ $ fromIntegral l)
  Info (Just (_,Parsed (Lines l _ _ _) _)) -> nofile $ succ $ fromIntegral l
  _ -> nofile 0
  where nofile l = ("<interactive>",l)

-- _cover "examples/accounts/accounts.repl"
_cover :: FilePath -> IO ()
_cover fn = do
  (ref,adv) <- mkCoverageAdvice
  s <- set (rEnv . eeAdvice) adv <$> initReplState (Script False fn) Nothing
  void $! runStateT (useReplLib >> loadFile def fn) s
  readIORef ref >>= writeReport "cov.lcov"
