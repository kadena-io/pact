{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      :  Pact.Repl.Cli
-- Copyright   :  (C) 2020 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Client library
--

module Pact.Repl.Cli
  ( loadCli
  ) where


import Control.Lens
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Default
import qualified Data.HashMap.Strict as HM
import System.IO
-- import System.FilePath
import Text.Trifecta as TF hiding (line,err,try,newline)


import Pact.Compile
import Pact.Eval
import Pact.Native
-- intentionally hidden unused functions to prevent lib functions from consuming gas
import Pact.Native.Internal hiding (defRNative,defGasRNative,defNative)
import Pact.Parse
import Pact.Repl
import Pact.Repl.Lib
import Pact.Repl.Types
import Pact.Types.Runtime
import Pact.Types.PactValue
import Pact.Types.Pretty



cliDefs :: NativeModule
cliDefs = ("Cli",
     [
      defZNative "local" local'
      (funType a [("exec",a)])
      [LitExample "(local (+ 1 2))"]
      "Evaluate EXEC on server."
     ])
  where
       a = mkTyVar "a" []


local' :: ZNativeFun LibState
local' _i as = do
  s <- cliState "sender" (_PLiteral . _LString)
  r <- mapM go as
  return $ toTList TyAny def (tStr s:r)
  where
    go t = return $ tStr $ tShow $ getInfo t

cliState :: FieldKey -> Traversal' PactValue a -> Eval LibState a
cliState k p = do
  r <- fmap toPactValueLenient <$> evalPact' "(cli.cli-state)"
  case preview (_head . _PObject . to _objectMap . ix k . p) r of
    Just l -> return l
    Nothing -> evalError def $ "cliState query failed for " <> pretty k

evalPact' :: String -> Eval e [Term Name]
evalPact' cmd = case TF.parseString exprsOnly mempty cmd of
  TF.Success es -> mapM go es
  TF.Failure f -> evalError def $ unAnnotate $ _errDoc f
  where
    go e = case compile (mkStringInfo cmd) e of
      Right t -> eval t
      Left l -> evalError (peInfo l) (peDoc l)


loadCli :: Maybe FilePath -> Repl ()
loadCli _confm = do
  rEnv . eeRefStore . rsNatives %= HM.union (moduleToMap cliDefs)
  void $ loadFile def "cli/cli.repl"


_cli :: IO ()
_cli = do
  s <- initReplState Interactive Nothing
  void $ (`evalStateT` s) $ do
    useReplLib
    loadCli Nothing
    forever $ pipeLoop True stdin Nothing
