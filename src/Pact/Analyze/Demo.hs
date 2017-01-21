
module Pact.Analyze.Demo where

import Pact.Analyze.Types
import Pact.Analyze.DSL


runDemo :: IO ()
runDemo = do
  f <- _getSampFunc "pay"
  putStrLn "\n# --------------------- #"
  putStrLn "# Compiling to SMT-LIB2 #"
  putStrLn "# --------------------- #\n"
  ps <- analyzeFunction f
  case ps of
    Left err -> putStrLn $ show err
    Right ps' -> do
      ppSymAst ps'
      putStrLn "\n# ----------------------------- #"
      putStrLn "# Rendering Formal Verification #"
      putStrLn "# ----------------------------- #\n"
      either putStrLn (putStrLn . unlines) =<< analyzeAndRenderTests f
