{-# LANGUAGE OverloadedStrings #-}

module Pact.Analyze.Demo where

import qualified Data.Text as T
-- import Pact.Analyze.DSL
import Pact.Analyze.Types
import System.Environment

runDemo :: IO ()
runDemo = do
  as <- getArgs
  let (path, modName, funName) = case as of
        [a, b, c] -> (a, T.pack b, T.pack c)
        _ -> ("examples/analyze-tests/analyze-tests.repl", "analyze-tests", "layup")
  putStrLn "demo"

  -- _compileTests False path modName funName
  let prop = Satisfiable $ Occurs Success
  runCompiler path modName funName prop
