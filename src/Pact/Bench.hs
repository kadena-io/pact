{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Pact.Bench where

import Criterion.Main

import qualified Data.Attoparsec.Text as APT
import Pact.Parse
import Pact.Compile
import Pact.Types.Lang
import Control.Exception
import Control.Arrow
import Pact.Server.PactService
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Pact.Types.Command
import Control.DeepSeq
import Data.Aeson
import Pact.Types.Crypto
import Pact.Types.RPC
import Pact.Types.Util


longStr :: Int -> Text
longStr n = pack $ "\"" ++ take n (cycle "abcdefghijklmnopqrstuvwxyz") ++ "\""

exps :: [(String,Text)]
exps = map (either id (unpack &&& id)) [
  Left ("longStr 10",longStr 10),
  Left ("longStr 100", longStr 100),
  Left ("longStr 1000", longStr 1000),
  Right "(+ 1 2)",
  Right "(+ 1 (+ 1 2))",
  Right "(demo.transfer \"Acct1\" \"Acct2\" 1.00)",
  Right "(+ 1 2) (foo.bar true -23.345875)"
  ]

conv :: (t -> t1) -> Either (t, t) t -> (t, t1)
conv f (Left (m,t)) = (m,f t)
conv f (Right t) = (t,f t)

benchParse :: Benchmark
benchParse = bgroup "parse" $ (`map` exps) $
             \(bname,ex) -> bench bname $ (`whnf` ex) $ \e -> case APT.parseOnly exprs e of
               Right s -> s
               Left er -> error $ "Pact parse failed: " ++ er

benchCompile :: [(String,[Exp])] -> Benchmark
benchCompile es = bgroup "compile" $ (`map` es) $
  \(bname,exs) -> bench bname $ nf (map (either (error . show) show . compile mkEmptyInfo)) exs

benchVerify :: [(String,Command ByteString)] -> Benchmark
benchVerify cs = bgroup "verify" $ (`map` cs) $
  \(bname,c) -> bench bname $ nf verifyCommand c

eitherDie :: Either String a -> IO a
eitherDie = either (throwIO . userError) (return $!)

main :: IO ()
main = do
  !pub <- eitherDie $ fromText' "0c99d911059580819c6f39ca5c203364a20dbf0a02b0b415f8ce7b48ba3a5bad"
  !priv <- eitherDie $ fromText' "6c938ed95a8abf99f34a1b5edd376f790a2ea8952413526af91b4c3eb0331b3c"
  !parsedExps <- mapM (mapM (eitherDie . APT.parseOnly exprs)) exps
  let !cmds = force $ (`fmap` exps) $ fmap $ \t -> mkCommand' [(ED25519,pub,priv)]
              (toStrict $ encode (Payload (Exec (ExecMsg t Null)) "nonce"))
  defaultMain [
    benchParse,
    benchCompile parsedExps,
    benchVerify cmds
    ]
