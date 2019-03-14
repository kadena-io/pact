{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Pact.Bench where

import Criterion.Main

import Pact.Parse
import Pact.Compile
import Pact.Types.Lang
import Control.Exception
import Control.Arrow
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Pact.Types.Command
import Control.DeepSeq
import Data.Aeson
import Pact.Types.Crypto
import Pact.Types.RPC
import Pact.Types.Runtime
import Pact.Interpreter
import qualified Data.Set as S
import Data.Default
import Pact.Types.Logger
import System.CPUTime
import Pact.MockDb
import qualified Data.Map.Strict as M
import qualified Crypto.Hash as H
import Pact.Persist.MockPersist
import Pact.Persist
import Unsafe.Coerce
import Pact.Gas

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

benchParse :: Benchmark
benchParse = bgroup "parse" $ (`map` exps) $
             \(bname,ex) -> bench bname $ (`whnf` ex) $ \e -> case parseExprs e of
               Right s -> s
               Left er -> error $ "Pact parse failed: " ++ er

benchCompile :: [(String,[Exp Parsed])] -> Benchmark
benchCompile es = bgroup "compile" $ (`map` es) $
  \(bname,exs) -> bench bname $ nf (map (either (error . show) show . compile mkEmptyInfo)) exs

benchVerify :: [(String,Command ByteString)] -> Benchmark
benchVerify cs = bgroup "verify" $ (`map` cs) $
  \(bname,c) ->
    bench bname $
      nf (verifyCommand :: Command ByteString -> ProcessedCommand () ParsedCode) c

eitherDie :: Either String a -> IO a
eitherDie = either (throwIO . userError) (return $!)

entity :: Maybe EntityName
entity = Just $ EntityName "entity"

loadBenchModule :: PactDbEnv e -> IO RefStore
loadBenchModule db = do
  m <- pack <$> readFile "tests/bench/bench.pact"
  pc <- parseCode m
  let md = MsgData S.empty
           (object ["keyset" .= object ["keys" .= ["benchadmin"::Text], "pred" .= (">"::Text)]])
           Nothing
           (initialHashTx H.Blake2b_512)
  let e = setupEvalEnv db entity (Transactional 1) md initRefStore
          freeGasEnv permissiveNamespacePolicy noSPVSupport
  _erRefStore <$> evalExec e pc

parseCode :: Text -> IO ParsedCode
parseCode m = ParsedCode m <$> eitherDie (parseExprs m)

benchNFIO :: NFData a => String -> IO a -> Benchmark
benchNFIO bname = bench bname . nfIO

runPactExec :: PactDbEnv e -> RefStore -> ParsedCode -> IO Value
runPactExec dbEnv refStore pc = do
  t <- Transactional . fromIntegral <$> getCPUTime
  let e = setupEvalEnv dbEnv entity t (initMsgData (initialHashTx H.Blake2b_512))
          refStore freeGasEnv permissiveNamespacePolicy noSPVSupport
  toJSON . _erOutput <$> evalExec e pc

benchKeySet :: KeySet
benchKeySet = KeySet [PublicKey "benchadmin"] (Name ">" def)

acctRow :: Columns Persistable
acctRow = Columns $ M.fromList [("balance",PLiteral (LDecimal 100.0))]

benchRead :: Domain k v -> k -> Method () (Maybe v)
benchRead KeySets _ = rc (Just benchKeySet)
benchRead UserTables {} _ = rc (Just acctRow)
benchRead _ _ = rc Nothing

benchReadValue :: Table k -> k -> Persist () (Maybe v)
benchReadValue (DataTable t) _k
  | t == "SYS_keysets" = rcp $ Just (unsafeCoerce benchKeySet)
  | t == "USER_bench_bench-accounts" = rcp $ Just (unsafeCoerce acctRow)
  | t == "SYS_modules" = rcp Nothing
  | otherwise = error (show t)
benchReadValue (TxTable _t) _k = rcp Nothing


mkBenchCmd :: [SomeKeyPair] -> (String, Text) -> IO (String, Command ByteString)
mkBenchCmd kps (str, t) = do
  cmd <- mkCommand' kps (toStrict $ encode (Payload (Exec (ExecMsg t Null)) "nonce" ()))
  return (str, cmd)


main :: IO ()
main = do
  !pub <- eitherDie $ parseB16TextOnly "0c99d911059580819c6f39ca5c203364a20dbf0a02b0b415f8ce7b48ba3a5bad"
  !priv <- eitherDie $ parseB16TextOnly "6c938ed95a8abf99f34a1b5edd376f790a2ea8952413526af91b4c3eb0331b3c"
  !keyPair <- eitherDie $ importKeyPair defaultScheme (Just $ PubBS pub) (PrivBS priv)
  !parsedExps <- force <$> mapM (mapM (eitherDie . parseExprs)) exps
  !pureDb <- mkPureEnv neverLog
  initSchema pureDb
  !refStore <- loadBenchModule pureDb
  !benchCmd <- parseCode "(bench.bench)"
  print =<< runPactExec pureDb refStore benchCmd
  !mockDb <- mkMockEnv def { mockRead = MockRead benchRead }
  !mdbRS <- loadBenchModule mockDb
  print =<< runPactExec mockDb mdbRS benchCmd
  !mockPersistDb <- mkMockPersistEnv neverLog def { mockReadValue = MockReadValue benchReadValue }
  !mpdbRS <- loadBenchModule mockPersistDb
  print =<< runPactExec mockPersistDb mpdbRS benchCmd
  cmds_ <- traverse (mkBenchCmd [keyPair]) exps
  !cmds <- return $!! cmds_


  defaultMain [
    benchParse,
    benchCompile parsedExps,
    benchVerify cmds,
    benchNFIO "puredb" (runPactExec pureDb refStore benchCmd),
    benchNFIO "mockdb" (runPactExec mockDb mdbRS benchCmd),
    benchNFIO "mockpersist" (runPactExec mockPersistDb mpdbRS benchCmd)
    ]
