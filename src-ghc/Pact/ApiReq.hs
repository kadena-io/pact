{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- |
-- Module      :  Pact.ApiReq
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--

module Pact.ApiReq
    (
     ApiKeyPair(..)
    ,ApiReq(..)
    ,apiReq
    ,mkApiReq
    ,mkExec
    ,mkCont
    ,mkKeyPairs
    ) where

import Control.Monad.Catch
import Control.Monad.State.Strict
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Default (def)
import Data.List
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe
import Data.Text (Text, pack)
import Data.Text.Encoding
import Data.Thyme.Clock
import qualified Data.Yaml as Y
import GHC.Generics
import Prelude
import System.Directory
import System.FilePath

import Pact.Types.API
import Pact.Types.Capability
import Pact.Types.Command
import Pact.Types.Crypto
import Pact.Types.RPC
import Pact.Types.Runtime hiding (PublicKey)
import Pact.Types.SPV

data ApiKeyPair = ApiKeyPair {
  _akpSecret :: PrivateKeyBS,
  _akpPublic :: Maybe PublicKeyBS,
  _akpAddress :: Maybe Text,
  _akpScheme :: Maybe PPKScheme,
  _akpCaps :: Maybe [SigCapability]
  } deriving (Eq, Show, Generic)
instance ToJSON ApiKeyPair where toJSON = lensyToJSON 4
instance FromJSON ApiKeyPair where parseJSON = lensyParseJSON 4


data ApiReq = ApiReq {
  _ylType :: Maybe String,
  _ylPactTxHash :: Maybe Hash,
  _ylStep :: Maybe Int,
  _ylRollback :: Maybe Bool,
  _ylData :: Maybe Value,
  _ylProof :: Maybe ContProof,
  _ylDataFile :: Maybe FilePath,
  _ylCode :: Maybe String,
  _ylCodeFile :: Maybe FilePath,
  _ylKeyPairs :: [ApiKeyPair],
  _ylNonce :: Maybe String,
  _ylPublicMeta :: Maybe PublicMeta,
  _ylNetworkId :: Maybe NetworkId
  } deriving (Eq,Show,Generic)
instance ToJSON ApiReq where toJSON = lensyToJSON 3
instance FromJSON ApiReq where parseJSON = lensyParseJSON 3

apiReq :: FilePath -> Bool -> IO ()
apiReq fp local = do
  (_,exec) <- mkApiReq fp
  if local then
    BSL.putStrLn $ encode exec
    else
    BSL.putStrLn $ encode $ SubmitBatch $ exec :| []
  return ()

mkApiReq :: FilePath -> IO ((ApiReq,String,Value,PublicMeta),Command Text)
mkApiReq fp = do
  ar@ApiReq {..} <- either (dieAR . show) return =<<
                 liftIO (Y.decodeFileEither fp)
  kps <- mkKeyPairs _ylKeyPairs
  case _ylType of
    Just "exec" -> mkApiReqExec ar kps fp
    Just "cont" -> mkApiReqCont ar kps fp
    Nothing -> mkApiReqExec ar kps fp -- Default
    _ -> dieAR "Expected a valid message type: either 'exec' or 'cont'"



mkApiReqExec :: ApiReq -> [SomeKeyPairCaps] -> FilePath -> IO ((ApiReq,String,Value,PublicMeta),Command Text)
mkApiReqExec ar@ApiReq{..} kps fp = do
  (code,cdata) <- withCurrentDirectory (takeDirectory fp) $ do
    code <- case (_ylCodeFile,_ylCode) of
      (Nothing,Just c) -> return c
      (Just f,Nothing) -> liftIO (readFile f)
      _ -> dieAR "Expected either a 'code' or 'codeFile' entry"
    cdata <- case (_ylDataFile,_ylData) of
      (Nothing,Just v) -> return v -- either (\e -> dieAR $ "Data decode failed: " ++ show e) return $ eitherDecode (BSL.pack v)
      (Just f,Nothing) -> liftIO (BSL.readFile f) >>=
                          either (\e -> dieAR $ "Data file load failed: " ++ show e) return .
                          eitherDecode
      (Nothing,Nothing) -> return Null
      _ -> dieAR "Expected either a 'data' or 'dataFile' entry, or neither"
    return (code,cdata)
  let pubMeta = fromMaybe def _ylPublicMeta
  cmd <- mkExec code cdata pubMeta kps _ylNetworkId _ylNonce
  return ((ar,code,cdata,pubMeta), cmd)

-- | Construct an Exec request message
--
mkExec
  :: String
    -- ^ code
  -> Value
    -- ^ optional environment data
  -> PublicMeta
    -- ^ public metadata
  -> [SomeKeyPairCaps]
    -- ^ signing keypairs + caplists
  -> Maybe NetworkId
    -- ^ optional 'NetworkId'
  -> Maybe String
    -- ^ optional nonce
  -> IO (Command Text)
mkExec code mdata pubMeta kps nid ridm = do
  rid <- maybe (show <$> getCurrentTime) return ridm
  cmd <- mkCommand
         kps
         pubMeta
         (pack $ show rid)
         nid
         (Exec (ExecMsg (pack code) mdata))
  return $ decodeUtf8 <$> cmd


mkApiReqCont :: ApiReq -> [SomeKeyPairCaps] -> FilePath -> IO ((ApiReq,String,Value,PublicMeta),Command Text)
mkApiReqCont ar@ApiReq{..} kps fp = do
  apiPactId <- case _ylPactTxHash of
    Just t -> return t
    Nothing -> dieAR "Expected a 'pactTxHash' entry"

  step <- case _ylStep of
    Just s -> return s
    Nothing -> dieAR "Expected a 'step' entry"

  rollback <- case _ylRollback of
    Just r -> return r
    Nothing -> dieAR "Expected a 'rollback' entry"

  cdata <- withCurrentDirectory (takeDirectory fp) $ do
    case (_ylDataFile,_ylData) of
      (Nothing,Just v) -> return v -- either (\e -> dieAR $ "Data decode failed: " ++ show e) return $ eitherDecode (BSL.pack v)
      (Just f,Nothing) -> liftIO (BSL.readFile f) >>=
                          either (\e -> dieAR $ "Data file load failed: " ++ show e) return .
                          eitherDecode
      (Nothing,Nothing) -> return Null
      _ -> dieAR "Expected either a 'data' or 'dataFile' entry, or neither"
  let pubMeta = fromMaybe def _ylPublicMeta
      pactId = toPactId apiPactId
  cmd <- mkCont pactId step rollback cdata pubMeta kps _ylNonce _ylProof _ylNetworkId
  return ((ar,"",cdata,pubMeta), cmd)

-- | Construct a Cont request message
--
mkCont
  :: PactId
    -- ^ pact tx hash of the continuation
  -> Int
    -- ^ cont step
  -> Bool
    -- ^ has rollback?
  -> Value
    -- ^ environment data
  -> PublicMeta
    -- ^ command public metadata
  -> [SomeKeyPairCaps]
    -- ^ signing keypairs
  -> Maybe String
    -- ^ optional nonce
  -> Maybe ContProof
    -- ^ optional continuation proof (required for cross-chain)
  -> Maybe NetworkId
    -- ^ optional network id
  -> IO (Command Text)
mkCont txid step rollback mdata pubMeta kps ridm proof nid = do
  rid <- maybe (show <$> getCurrentTime) return ridm
  cmd <- mkCommand
         kps
         pubMeta
         (pack $ show rid)
         nid
         (Continuation (ContMsg txid step rollback mdata proof) :: (PactRPC ContMsg))
  return $ decodeUtf8 <$> cmd



mkKeyPairs :: [ApiKeyPair] -> IO [SomeKeyPairCaps]
mkKeyPairs keyPairs = traverse mkPair keyPairs
  where importValidKeyPair ApiKeyPair{..} = fmap (,maybe [] id _akpCaps) $
          case _akpScheme of
            Nothing -> importKeyPair defaultScheme _akpPublic _akpSecret
            Just ppk -> importKeyPair (toScheme ppk) _akpPublic _akpSecret

        mkPair akp = case _akpAddress akp of
          Nothing -> either dieAR return (importValidKeyPair akp)
          Just addrT -> do
            addrBS <- either dieAR return (parseB16TextOnly addrT)
            kp     <- either dieAR return (importValidKeyPair akp)

            -- Enforces that user provided address matches the address derived from the Public Key
            -- for transparency and a better user experience. User provided address not used except
            -- for this purpose.

            case (addrBS, formatPublicKey (fst kp)) of
              (expectAddr, actualAddr)
                | expectAddr == actualAddr  -> return kp
                | otherwise                 -> dieAR $ "Address provided "
                                               ++ show (toB16Text expectAddr)
                                               ++ " does not match actual Address "
                                               ++ show (toB16Text actualAddr)

dieAR :: String -> IO a
dieAR errMsg = throwM . userError $ intercalate "\n" $
  ["Failure reading request yaml. Valid fields are: "
  ,"Common fields:"
  ,"  type: 'exec' or 'cont', indicating Exec or Cont message type"
  ,"  data|dataFile: (optional) JSON transaction data, as yaml (data) or a yaml file (dataFile)"
  ,"  keyPairs: list of key pairs for signing (use pact -g to generate): ["
  ,"    public: base 16 public key"
  ,"    secret: base 16 secret key"
  ,"    address: (required for ETH) base 16 address"
  ,"    scheme: optional, 'ETH' or 'ED25519', default ED25519"
  ,"    caps: capability list as strings, in form \"[(module.CAP param1 param2)]\""
  ,"    ] "
  ,"  nonce: (optional) request nonce, defaults to current time"
  ,"  publicMeta: (optional) data for public-chain execution: ["
  ,"    chainId: chain or shard identifier"
  ,"    sender: gas-paying sender account"
  ,"    gasLimit: integer gas max limit"
  ,"    gasPrice: decimal gas unit price"
  ,"    ttl: TTL value in seconds"
  ,"    creationTime: epoch time integer value in seconds"
  ,"Exec-only fields:"
  ,"  code|codeFile: [exec only] Pact code, as a string (code) or file path (codeFile)"
  ,"Cont-only fields:"
  ,"  pactTxHash: pact ID to continue"
  ,"  step: step index to continue"
  ,"  rollback: rollback/cancel flag"
  ,"  proof: platform-specific continuation proof data"
  ,"Error message: " ++ errMsg
  ]
