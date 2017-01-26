
module Pact.Analyze.Demo where

import Pact.Analyze.DSL
import System.Environment

runDemo :: IO ()
runDemo = do
  as <- getArgs
  let (fp,m,f) = case as of
        [a,b,c] -> (a,b,c)
        _ -> ( "examples/analyze-tests/analyze-tests.repl","analyze-tests","pay")
  _compileTests False fp m f
