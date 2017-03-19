{-# LANGUAGE OverloadedStrings #-}
module Pact.Bench where

import Criterion.Main

import qualified Data.Attoparsec.Text as APT
import Pact.Compile
import Data.Text (pack)


longStr :: Int -> String
longStr n = "\"" ++ take n (cycle "abcdefghijklmnopqrstuvwxyz") ++ "\""

exps :: [Either (String,String) String]
exps = [
  Left ("longStr 10",longStr 10),
  Left ("longStr 100", longStr 100),
  Left ("longStr 1000", longStr 1000),
  Right "(+ 1 2)",
  Right "(+ 1 (+ 1 2))",
  Right "(demo.transfer \"Acct1\" \"Acct2\" 1.00)",
  Right "(+ 1 2) (foo.bar true -23.345875)"
  ]

benchParse :: String -> (String -> a) -> (a -> b) -> Benchmark
benchParse n f p =
  let conv (Left (m,t)) = (m,f t)
      conv (Right t) = (t,f t)
  in bgroup ("parser-" ++ n) $ (`map` (conv <$> exps)) $ \(bname,ex) -> bench bname $ whnf p ex

parseAttoText :: Benchmark
parseAttoText = benchParse "attoText" pack $ \e -> case APT.parseOnly exprs e of
      Right s -> s
      Left er -> error $ "Pact parse failed: " ++ er

main :: IO ()
main = defaultMain [
  parseAttoText
  ]
