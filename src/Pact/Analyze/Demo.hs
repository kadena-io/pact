
module Pact.Analyze.Demo where

import Pact.Analyze.Types


demoTest :: ProveProperty
demoTest = ProveProperty "analyze-tests.accounts" "balance" [ColumnRange ">=" 0, ConservesMass]

runDemo :: IO ()
runDemo = do
  putStrLn "# ----------------- #"
  putStrLn "# Running TypeCheck #"
  putStrLn "# ----------------- #\n"
  f <- _getSampFunc "pay-with-let"
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
      prettyPrintProveProperty ps' demoTest
