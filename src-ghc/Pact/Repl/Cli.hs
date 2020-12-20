{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      :  Pact.Repl.Cli
-- Copyright   :  (C) 2020 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Client library
--

module Pact.Repl.Cli
  ( loadCli
  ) where


import Control.Lens
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Aeson
import Data.Default
import qualified Data.HashMap.Strict as HM
import Data.Text (Text,unpack,intercalate)
import Data.Thyme.Time.Core
import Data.Thyme.Clock
import qualified Data.Vector as V
import Network.Connection
import Network.HTTP.Client hiding (responseBody)
import Network.HTTP.Client.TLS
import Servant.Client.Core
import Servant.Client
import Servant.Server
import System.IO
-- import System.FilePath
import Text.Trifecta as TF hiding (line,err,try,newline)

import Pact.ApiReq
import Pact.Compile
import Pact.Eval
import Pact.Native
-- intentionally hidden unused functions to prevent lib functions from consuming gas
import Pact.Native.Internal hiding (defRNative,defGasRNative,defNative)
import Pact.Parse
import Pact.Repl
import Pact.Repl.Lib
import Pact.Repl.Types
import Pact.Types.API
import Pact.Types.Command
import Pact.Types.Runtime
import Pact.Types.PactValue
import Pact.Types.Pretty
import Pact.Server.API



cliDefs :: NativeModule
cliDefs = ("Cli",
     [
      defZNative "local" local'
      (funType a [("exec",a)])
      [LitExample "(local (+ 1 2))"]
      "Evaluate EXEC on server."
      ,
      defZNative "add-cap" addCap
      (funType tTyString [("cap",TyFun $ funType' tTyBool []),("signers",TyList (tTyString))])
      [LitExample "(add-cap (coin.TRANSFER \"alice\" \"bob\" 10.0) [\"alice\"])"]
      "Add signer capability CAP for SIGNERS"
     ])
  where
       a = mkTyVar "a" []


local' :: ZNativeFun LibState
local' i as = do
  let code = intercalate " " $ map (tShow . getInfo) as
  cmd <- buildCmd i code
  env <- buildEndpoint i
  r <- sendLocal i env cmd
  case _pactResult (_crResult r) of
    Left e -> throwM e
    Right v -> return $ fromPactValue v


infoCode :: HasInfo i => i -> Term n
infoCode = tStr . tShow . getInfo

addCap :: ZNativeFun LibState
addCap _i [TApp (App n as _) _,signers] = do
  let name = infoCode n
  as' <- mapM reduce as
  signers' <- reduce signers
  eval (TApp (App (TVar (QName (QualifiedName "cli" "add-cap1" def)) def)
               [ name
               , TList (V.fromList as') TyAny def
               , signers' ]
               def) def)

addCap i as = argsError' i as

cliState :: HasInfo i => i -> FieldKey -> Fold PactValue a -> Eval LibState a
cliState i k p = evalExpect i (asString k) (_head . _PObject . to _objectMap . ix k . p) "(cli.cli-state)"

buildEndpoint :: HasInfo i => i -> Eval LibState ClientEnv
buildEndpoint i = do
  cid <- cliState i "chain-id" (_PLiteral . _LInteger)
  let pactCid = -1-- <- evalExpect1 i "integer" (_PLiteral . _LInteger) "PACT_CHAIN_ID"
  let isPact = cid == pactCid
  host <- cliState i "host" (_PLiteral . _LString)
  nw <- cliState i "network-id" (_PLiteral . _LString)
  --let url | isPact = "http://" <> host <> "/"
   --       | otherwise
  let url = "https://" <> host <> "/chainweb/0.0/" <> nw <> "/chain/" <> tShow cid <> "/pact"
  burl <- parseBaseUrl (unpack url)
  -- mgr <- liftIO $ newManager defaultManagerSettings
  liftIO $ getClientEnv burl

buildCmd :: HasInfo i => i -> Text -> Eval LibState (Command Text)
buildCmd i cmd = do
  ttl <- cliState i "ttl" (_PLiteral . _LInteger)
  nw <- NetworkId <$> cliState i "network-id" (_PLiteral . _LString)
  ctime <- cliState i "creation-time" (_PLiteral . _LTime)
  nonce <- cliState i "nonce" (_PLiteral . _LString)
  cid <- cliState i "chain-id" (_PLiteral . _LInteger)
  sender <- cliState i "sender" (_PLiteral . _LString)
  gasLimit <- view (eeGasEnv . geGasLimit)
  gasPrice <- view (eeGasEnv . geGasPrice)
  md <- view eeMsgBody
  let toCT = TxCreationTime . fromIntegral . (`div` 1000000) . toMicroseconds . utcTimeToPOSIXSeconds
  liftIO $ mkExec cmd md
    (PublicMeta (ChainId (tShow cid))
     sender gasLimit gasPrice (fromIntegral ttl)
     (toCT ctime))
    []
    (Just nw)
    (Just nonce)

getClientEnv :: BaseUrl -> IO ClientEnv
getClientEnv url = flip mkClientEnv url <$> newTlsManagerWith mgrSettings
    where
      mgrSettings = mkManagerSettings
       (TLSSettingsSimple True False False)
       Nothing

evalExpect1 :: HasInfo i => i -> Text -> Fold PactValue a -> String -> Eval LibState a
evalExpect1 i msg f = evalExpect i msg (_head . f)

evalExpect :: HasInfo i => i -> Text -> Fold [PactValue] a -> String -> Eval LibState a
evalExpect i msg f cmd = do
  r <- evalPactValue cmd
  case preview f r of
    Nothing -> evalError' i $ "Expected " <> pretty msg <> " for command " <> pretty cmd
    Just v -> return v

evalPactValue :: String -> Eval e [PactValue]
evalPactValue = fmap (fmap toPactValueLenient) . evalPact'

evalPact' :: String -> Eval e [Term Name]
evalPact' cmd = case TF.parseString exprsOnly mempty cmd of
  TF.Success es -> mapM go es
  TF.Failure f -> evalError def $ unAnnotate $ _errDoc f
  where
    go e = case compile (mkStringInfo cmd) e of
      Right t -> eval t
      Left l -> evalError (peInfo l) (peDoc l)


loadCli :: Maybe FilePath -> Repl ()
loadCli _confm = do
  rEnv . eeRefStore . rsNatives %= HM.union (moduleToMap cliDefs)
  void $ loadFile def "cli/cli.repl"


_cli :: IO ()
_cli = do
  s <- initReplState Interactive Nothing
  void $ (`evalStateT` s) $ do
    useReplLib
    loadCli Nothing
    forever $ pipeLoop True stdin Nothing



send :: HasInfo i => i -> ClientEnv -> SubmitBatch -> Eval e RequestKeys
send i env sb =
  liftIO (runClientM (sendClient sb) env) >>= eitherDie i

sendLocal :: HasInfo i => i -> ClientEnv -> Command Text -> Eval e (CommandResult Hash)
sendLocal i env cmd =
  liftIO (runClientM (localClient cmd) env) >>= eitherDie i

eitherDie :: HasInfo i => Pretty a => i -> Either a b -> Eval e b
eitherDie i = either (evalError' i . pretty) return
