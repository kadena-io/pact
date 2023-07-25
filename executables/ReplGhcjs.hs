-- |
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
--

module Main where

import Control.Monad.State.Strict

import Data.Aeson hiding ((.=),Object)

import GHCJS.Marshal
import GHCJS.Foreign.Callback
import GHCJS.Types

import Prelude hiding (log)

import qualified Pact.JSON.Encode as J
import Pact.Repl
import Pact.Repl.Types

foreign import javascript unsafe "evalCmd = $1"
    evalCmd :: Callback (JSVal -> JSVal -> IO JSVal) -> IO ()

main :: IO ()
main = do
    callback <- syncCallback2' $ \log cmd -> do
      Just log' <- fromJSVal log
      Just cmd' <- fromJSVal cmd
      r <- evalString log' cmd'
      toJSVal_aeson r
    evalCmd callback

-- -------------------------------------------------------------------------- --
-- Utils

evalString :: Bool -> String -> IO Value
evalString showLog cmd = do
  (er,s) <- initReplState StringEval Nothing >>= runStateT (evalRepl' cmd)
  return $ J.object $ case (showLog,er) of
    (False,Right v) -> [ "success" J..= v]
    (True,Right _) -> ["success" J..= trim (_rOut s) ]
    (False,Left e) -> ["failure" J..= e ]
    (True,Left e) -> ["failure" J..= (_rOut s ++ e) ]

