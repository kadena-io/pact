{-# LANGUAGE OverloadedStrings #-}

module Pact.Analyze.Demo where

import qualified Data.Text as T
import Pact.Analyze.DSL
import System.Environment

runDemo :: IO ()
runDemo = do
  as <- getArgs
  let (path, modName, funName) = case as of
        [a, b, c] -> (a, T.pack b, T.pack c)
        _ -> ("examples/analyze-tests/analyze-tests.repl", "analyze-tests", "pay")
  _compileTests False path modName funName
