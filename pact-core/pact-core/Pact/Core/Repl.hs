{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad.IO.Class(MonadIO(..))
import Control.Monad.Catch
import System.Console.Haskeline
import qualified Data.Text as T

import Pact.Core.Compile

type ReplT a = InputT IO a

main :: IO ()
main = runInputT defaultSettings loop
  where
  catch' ma = catchAll ma (\e -> outputStrLn (show e) *> loop)
  loop = do
    minput <- getInputLine "pact>"
    case minput of
      Nothing -> outputStrLn "goodbye"
      Just input -> case T.strip (T.pack input) of
        "" -> loop
        ":quit" -> outputStrLn "goodbye"
        i | T.isPrefixOf ":load" i ->  do
          catch' (liftIO (_compileFile (T.unpack (T.drop 5 i))))
          loop
        i -> catch' (liftIO (_compile "repl" i) *> loop)
