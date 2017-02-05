{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Pact.Server.Command where

import Control.Concurrent
import Data.Default
import Data.Aeson as A
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict,fromStrict)

import qualified Data.ByteString.Base16 as B16
import Data.Serialize as SZ hiding (get,put)
import Control.Monad.Reader
import Control.Exception.Safe
import Control.Applicative
import Control.Lens hiding ((.=))
import qualified Data.Set as S
import Data.Maybe
import qualified Text.Trifecta as TF
import qualified Data.Attoparsec.Text as AP
import Control.Monad.Except
import Data.Text (Text,unpack)
import Prelude hiding (log,exp)
import qualified Data.HashMap.Strict as HM
import Text.PrettyPrint.ANSI.Leijen (renderCompact,displayS)
import System.Directory

import Crypto.Ed25519.Pure (valid)

import Pact.Types hiding (PublicKey)
import qualified Pact.Types as Pact
import Pact.Pure
import Pact.Eval
import Pact.Compile as Pact
import Pact.Repl

import Pact.Server.Types
import Pact.Server.SQLite as PactSL

type PactMVars = (DBVar,MVar CommandState)

initCommandLayer :: CommandConfig -> IO (ApplyFn,ApplyLocal)
initCommandLayer config = do
  let klog s = _ccDebugFn config ("[Pact] " ++ s)
  mvars <- case _ccDbFile config of
    Nothing -> do
      klog "Initializing pure pact"
      ee <- initEvalEnv def puredb
      rv <- newMVar (CommandState $ _eeRefStore ee)
      return (PureVar $ _eePactDbVar ee,rv)
    Just f -> do
      klog "Initializing pact SQLLite"
      dbExists <- doesFileExist f
      if dbExists
        then klog "Deleting Existing Pact DB File" >> removeFile f
        else klog "No Pact DB File Found"
      p <- (\a -> a { _log = \m s -> klog $ m ++ ": " ++ show s }) <$> initPSL f
      ee <- initEvalEnv p psl
      rv <- newMVar (CommandState $ _eeRefStore ee)
      let v = _eePactDbVar ee
      klog "Creating Pact Schema"
      createSchema v
      return (PSLVar v,rv)
  return (applyTransactional config mvars,applyLocal config mvars)


applyTransactional :: CommandConfig -> PactMVars -> LogEntry -> IO CommandResult
applyTransactional config (dbv,cv) le = do
  let logIndex = _leLogIndex le
  r <- tryAny (runCommand
               (CommandEnv config (Transactional $ fromIntegral logIndex) dbv cv)
               (applyLogEntry le))
  case r of
    Right cr -> return cr
    Left e -> return $ jsonResult $
               CommandError "Transaction execution failed" (Just $ show e)

jsonResult :: ToJSON a => a -> CommandResult
jsonResult = CommandResult . toStrict . A.encode

applyLocal :: CommandConfig -> PactMVars -> ByteString -> IO CommandResult
applyLocal config (dbv,cv) bs = do
  r <- tryAny (runCommand
               (CommandEnv config Local dbv cv)
               (applyPactMessage bs))
  case r of
    Right cr -> return cr
    Left e ->
        return $ jsonResult $
               CommandError "Local execution failed" (Just $ show e)

applyPreprocessedCommand :: PreprocessedCommand  -> CommandM CommandResult
applyPreprocessedCommand PreprocessedPublicCommand{..} = do
    let
        cmd = _leCommand e
        bs = unCommandEntry $ _cmdEntry cmd
    cmsg :: CommandMessage <- either (throwCmdEx . ("applyLogEntry: deserialize failed: " ++ ) . show) return $
            SZ.decode bs
    case cmsg of
      PublicMessage m -> do
                    pk <- case firstOf (cmdProvenance.pDig.digPubkey) cmd of
                            Nothing -> return [] -- really an error but this is still toy code
                            Just k -> return [Pact.PublicKey (B16.encode $ exportPublic k)]
                    applyRPC pk m
      PrivateMessage ct mt m -> applyPrivate ct mt m

applyPactMessage :: ByteString -> CommandM CommandResult
applyPactMessage m = do
  pmsg <- either (throwCmdEx . ("applyPactMessage: deserialize failed: " ++ ) . show) return $
          SZ.decode m
  pk <- validateSig pmsg
  applyRPC [pk] (_pmEnvelope pmsg)

applyRPC :: [Pact.PublicKey] -> ByteString -> CommandM CommandResult
applyRPC ks m =
  case A.eitherDecode (fromStrict m) of
      Right PactEnvelope {..} ->
          case _pePayload of
            (Exec pm) -> applyExec pm ks
            (Continuation ym) -> applyContinuation ym ks
            (Multisig mm) -> applyMultisig mm ks
      Left err -> throwCmdEx $ "RPC deserialize failed: " ++ show err ++ show m

validateSig :: PactMessage -> CommandM Pact.PublicKey
validateSig (PactMessage payload key sig hsh) = do
  if hsh /= hash payload
  then throwCmdEx "Hash verification failure"
  else if valid payload key sig
       then return (Pact.PublicKey (exportPublic key)) -- TODO turn off with compile flags?
       else throwCmdEx "Signature verification failure"

parse :: ExecutionMode -> Text -> CommandM [Exp]
parse (Transactional _) code =
    case AP.parseOnly Pact.exprs code of
      Right s -> return s
      Left e -> throwCmdEx $ "Pact parse failed: " ++ e
parse Local code =
    case TF.parseString Pact.exprs mempty (unpack code) of
      TF.Success s -> return s
      TF.Failure f -> throwCmdEx $ "Pact parse failed: " ++
                      displayS (renderCompact (TF._errDoc f)) ""


applyExec :: ExecMsg -> [Pact.PublicKey] -> CommandM CommandResult
applyExec (ExecMsg code edata) ks = do
  CommandEnv {..} <- ask
  exps <- parse _ceMode code
  when (null exps) $ throwCmdEx "No expressions found"
  terms <- forM exps $ \exp -> case compile exp of
            Right r -> return r
            Left (i,e) -> throwCmdEx $ "Pact compile failed: " ++ show i ++ ": " ++ show e
  (CommandState refStore) <- liftIO $ readMVar _ceState
  let evalEnv :: PactDb e -> MVar e -> EvalEnv e
      evalEnv pdb mv = EvalEnv {
                  _eeRefStore = refStore
                , _eeMsgSigs = S.fromList ks
                , _eeMsgBody = edata
                , _eeTxId = fromMaybe 0 $ firstOf emTxId _ceMode
                , _eeEntity = _entName $ _ccEntity $ _ceConfig
                , _eePactStep = Nothing
                , _eePactDb = pdb
                , _eePactDbVar = mv
                }
      runP (PureVar mv) = runEval def (evalEnv puredb mv) (execTerms _ceMode terms)
      runP (PSLVar mv) = runEval def (evalEnv psl mv) (execTerms _ceMode terms)
  (r,rEvalState') <- liftIO $ runP _ceDBVar
  case r of
    Right t -> do
           when (_ceMode /= Local) $ liftIO $ modifyMVar_ _ceState $ \rs ->
             return $ over (csRefStore.rsModules)
                      (HM.union (HM.fromList (_rsNew (_evalRefs rEvalState')))) rs
           return $ jsonResult $ CommandSuccess t -- TODO Yield handling
    Left e -> throwCmdEx $ "Exec failed: " ++ show e

execTerms :: ExecutionMode -> [Term Name] -> Eval e (Term Name)
execTerms mode terms = do
  evalBeginTx
  er <- catchError
        (last <$> mapM eval terms)
        (\e -> evalRollbackTx >> throwError e)
  case mode of
    Transactional _ -> void evalCommitTx
    Local -> evalRollbackTx
  return er


applyContinuation :: ContMsg -> [Pact.PublicKey] -> CommandM CommandResult
applyContinuation _ _ = throwCmdEx "Continuation not supported"

applyMultisig :: MultisigMsg -> [Pact.PublicKey] -> CommandM CommandResult
applyMultisig _ _ = throwCmdEx "Multisig not supported"

applyPrivate :: SessionCipherType -> MessageTags -> ByteString -> CommandM a
applyPrivate _ _ _ = throwCmdEx "Private messages not supported"

_pk :: PublicKey
_pk = fromJust $ importPublic $ fst $ B16.decode "06f1ade90e5637a3392dbd7aa01486d4ac597dbf7707dfb12f94f9b9d69fcf0f"
_sk :: PrivateKey
_sk = fromJust $ importPrivate $ fst $ B16.decode "2ca45751578698d73759b44feeea38391cd4136bb8265cd3a36f488cbadf8eb7"

_config :: CommandConfig
_config = CommandConfig (EntityInfo "me") Nothing putStrLn
{-
_localRPC :: ToRPC a => a -> IO ByteString
_localRPC rpc = do
  (_,runl) <- initCommandLayer _config
  let p = mkPactMessage _sk _pk (toRPC rpc)
  unCommandResult <$> runl (SZ.encode p)

_publicRPC :: ToRPC a => a -> LogIndex -> IO ByteString
_publicRPC rpc li = do
  (runt,_) <- initCommandLayer _config
  let p = mkPactMessage _sk _pk (toRPC rpc)
      pm = PublicMessage (SZ.encode p)
      le = LogEntry 0 li (Command (CommandEntry (SZ.encode pm))
                          (Alias "")
                          0 Valid NewMsg) ""
  unCommandResult <$> runt le
-}

mkRPC :: ToRPC a => a ->  CommandEntry
mkRPC = CommandEntry . SZ.encode . PublicMessage . toStrict . A.encode . A.toJSON . toRPC

mkSimplePact :: Text -> CommandEntry
mkSimplePact = mkRPC . (`ExecMsg` A.Null)

mkTestPact :: CommandEntry
mkTestPact = mkSimplePact "(demo.transfer \"Acct1\" \"Acct2\" 1.0)"



--mkTestSigned :: IO ()
--mkTestSigned = do
--  (Right (msg :: PactRPC)) <- eitherDecode <$> BSL.readFile "tests/exec1.json"
--  let env@PactEnvelope {..} = PactEnvelope msg "a" "rid"
--  let (pm@PactMessage {..}) = mkPactMessage' _sk _pk  (BSL.toStrict $ A.encode env)
--      ce = CommandEntry $! SZ.encode $! PublicMessage $! _pmEnvelope
--      rpc = mkCmdRpc ce _peAlias "rid" (Digest _peAlias _pmSig _pmKey CMD $ hash $ SZ.encode $ CMDWire (ce,_peAlias,"rid"))
--      Right (c :: Command) = fromWire Nothing def rpc
--      cmdbrpc = mkCmdBatchRPC [rpc] (Digest _peAlias _pmSig _pmKey CMDB $ hash $ SZ.encode $ [rpc])
--      Right (cb :: CommandBatch) = fromWire Nothing def cmdbrpc
--  BSL.writeFile "tests/exec1-signed.json" $ encodePretty pm
--  (Just pm') <- A.decode <$> BSL.readFile "tests/exec1-signed.json"
--  print (pm == pm')
--  print c
--  print cb
