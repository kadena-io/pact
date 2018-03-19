-- |
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
--

module Main where

import GHCJS.Marshal
import GHCJS.Foreign.Callback
import GHCJS.Types
import Prelude hiding (log)

import Pact.Repl (evalString)

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
