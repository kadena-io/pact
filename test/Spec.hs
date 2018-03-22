{-# language OverloadedStrings #-}

import EasyTest
import Pact.Analyze.Types

suite :: Test ()
suite = tests
  [ do result <- io $ runCompilerTest
         "examples/analyze-tests/analyze-tests.repl" "analyze-tests" "layup"
       expectRight result
       pure ()
  ]

main :: IO ()
main = run suite
