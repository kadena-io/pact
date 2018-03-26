{-# language OverloadedStrings #-}

import EasyTest
import Pact.Analyze.Types

suite :: Test ()
suite = tests
  [ do result <- io $ runCompilerTest
         "examples/analyze-tests/analyze-tests.repl" "analyze-tests" "layup" $
           Valid $ Occurs Success
       expectRight result
       pure ()
  ]

main :: IO ()
main = run suite
