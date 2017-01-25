
module Pact.Analyze.Demo where

import Pact.Analyze.DSL

runDemo :: IO ()
runDemo = _compileTests False "examples/analyze-tests/analyze-tests.repl" "analyze-tests" "pay"
