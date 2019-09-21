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
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
-- |
-- Module      :  Pact.ApiReq
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--

module Pact.ApiReq
( ApiKeyPair(..)
, ApiReq(..)
, apiReq
, mkApiReq
, mkExec
, mkCont
, mkKeyPairs
) where

import Control.Monad.Catch
import Control.Monad.State.Strict

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Default (def)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding
import Data.Thyme.Clock
import qualified Data.Yaml as Y

import GHC.Generics

import System.Directory
import System.FilePath

import NeatInterpolation

import Pact.Types.API
import Pact.Types.Command
import Pact.Types.Crypto
import Pact.Types.RPC
import Pact.Types.Runtime hiding (PublicKey)
import Pact.Types.SPV


data ApiKeyPair = ApiKeyPair
  { _akpSecret :: PrivateKeyBS
  , _akpPublic :: Maybe PublicKeyBS
  , _akpAddress :: Maybe Text
  , _akpScheme :: Maybe PPKScheme
  } deriving (Eq, Show, Generic)
instance ToJSON ApiKeyPair where toJSON = lensyToJSON 4
instance FromJSON ApiKeyPair where parseJSON = lensyParseJSON 4


data ApiReq = ApiReq
  { _ylType :: Maybe String
  , _ylPactTxHash :: Maybe Hash
  , _ylStep :: Maybe Int
  , _ylRollback :: Maybe Bool
  , _ylData :: Maybe Value
  , _ylProof :: Maybe ContProof
  , _ylDataFile :: Maybe FilePath
  , _ylCode :: Maybe String
  , _ylCodeFile :: Maybe FilePath
  , _ylKeyPairs :: [ApiKeyPair]
  , _ylNonce :: Maybe String
  , _ylPublicMeta :: Maybe PublicMeta
  } deriving (Eq,Show,Generic)
instance ToJSON ApiReq where toJSON = lensyToJSON 3
instance FromJSON ApiReq where parseJSON = lensyParseJSON 3

apiReq :: FilePath -> Bool -> IO ()
apiReq fp local = do
  (_,exec) <- mkApiReq fp
  if local
    then BSL.putStrLn $ encode exec
    else BSL.putStrLn $ encode $ SubmitBatch $ pure exec
  return ()

mkApiReq :: FilePath -> IO ((ApiReq,String,Value,PublicMeta),Command Text)
mkApiReq fp = do
  ar@ApiReq {..} <- liftIO (Y.decodeFileEither fp) >>= \case
    Left e -> dieYaml $ show e
    Right t -> return t
  kps <- mkKeyPairs _ylKeyPairs
  case _ylType of
    Just "exec" -> mkApiReqExec ar kps fp
    Just "cont" -> mkApiReqCont ar kps fp
    Nothing -> mkApiReqExec ar kps fp -- Default
    _ -> dieYaml "Expected a valid message type: either 'exec' or 'cont'"



mkApiReqExec :: ApiReq -> [SomeKeyPair] -> FilePath -> IO ((ApiReq,String,Value,PublicMeta),Command Text)
mkApiReqExec ar@ApiReq{..} kps fp = do
  (code,cdata) <- withCurrentDirectory (takeDirectory fp) $ do
    code <- case (_ylCodeFile,_ylCode) of
      (Nothing,Just c) -> return c
      (Just f,Nothing) -> liftIO (readFile f)
      _ -> dieExec "Expected either a 'code' or 'codeFile' entry"
    cdata <- case (_ylDataFile,_ylData) of
      (Nothing, Just v) -> return v -- either (\e -> dieAR $ "Data decode failed: " ++ show e) return $ eitherDecode (BSL.pack v)
      (Just f, Nothing) -> liftIO (BSL.readFile f) >>= \r -> case eitherDecode r of
        Left e -> dieExec $ "Data file load failed: " ++ show e
        Right t -> return t
      (Nothing, Nothing) -> return Null
      _ -> dieExec "Expected either a 'data' or 'dataFile' entry, or neither"
    return (code, cdata)

  let pubMeta = fromMaybe def _ylPublicMeta

  exec <- mkExec code cdata pubMeta kps _ylNonce
  return ((ar, code, cdata, pubMeta), exec)

mkExec :: String -> Value -> PublicMeta -> [SomeKeyPair] -> Maybe String -> IO (Command Text)
mkExec code mdata pubMeta kps ridm = do
  rid <- maybe (show <$> getCurrentTime) return ridm
  cmd <- mkCommand kps pubMeta (pack $ show rid) $ Exec (ExecMsg (pack code) mdata)
  return $ decodeUtf8 <$> cmd


mkApiReqCont :: ApiReq -> [SomeKeyPair] -> FilePath -> IO ((ApiReq,String,Value,PublicMeta),Command Text)
mkApiReqCont ar@ApiReq{..} kps fp = do
  apiPactId <- case _ylPactTxHash of
    Just t -> return t
    Nothing -> dieCont "Expected a 'pactTxHash' entry"

  step <- case _ylStep of
    Just s -> return s
    Nothing -> dieCont "Expected a 'step' entry"

  rollback <- case _ylRollback of
    Just r -> return r
    Nothing -> dieCont "Expected a 'rollback' entry"

  cdata <- withCurrentDirectory (takeDirectory fp) $ do
    case (_ylDataFile,_ylData) of
      (Nothing, Just v) -> return v -- either (\e -> dieAR $ "Data decode failed: " ++ show e) return $ eitherDecode (BSL.pack v)
      (Just f, Nothing) -> liftIO (BSL.readFile f) >>= \r -> case eitherDecode r of
        Left e -> dieCont $ "Data file load failed: " ++ show e
        Right t -> return t
      (Nothing, Nothing) -> return Null
      _ -> dieCont "Expected either a 'data' or 'dataFile' entry, or neither"

  let pubMeta = fromMaybe def _ylPublicMeta
      pactId = toPactId apiPactId

  cont <- mkCont pactId step rollback cdata pubMeta kps _ylNonce _ylProof
  return ((ar, "", cdata, pubMeta), cont)

mkCont
    :: PactId
    -> Int
    -> Bool
    -> Value
    -> PublicMeta
    -> [SomeKeyPair]
    -> Maybe String
    -> Maybe ContProof
    -> IO (Command Text)
mkCont txid step rollback mdata pubMeta kps ridm proof = do
  rid <- maybe (show <$> getCurrentTime) return ridm
  cc <- mkCommand kps pubMeta (pack $ show rid)
        $ (Continuation (ContMsg txid step rollback mdata proof) :: (PactRPC ContMsg))=
  let c = fmap decodeUtf8 cc
  return c


mkKeyPairs :: [ApiKeyPair] -> IO [SomeKeyPair]
mkKeyPairs keyPairs = traverse mkPair keyPairs
  where
    isValidKeyPair ApiKeyPair{..} = case _akpScheme of
      Nothing -> importKeyPair defaultScheme _akpPublic _akpSecret
      Just ppk -> importKeyPair (toScheme ppk) _akpPublic _akpSecret

    mkPair akp = case _akpAddress akp of
      Nothing -> either dieYaml return (isValidKeyPair akp)
      Just addrT -> do
        addrBS <- either dieYaml return (parseB16TextOnly addrT)
        kp     <- either dieYaml return (isValidKeyPair akp)

        -- Enforces that user provided address matches the address derived from the Public Key
        -- for transparency and a better user experience. User provided address not used except
        -- for this purpose.

        case (addrBS, formatPublicKey kp) of
          (expectAddr, actualAddr)
            | expectAddr == actualAddr  -> return kp
            | otherwise -> dieYaml
              $ "Address provided "
              ++ show (toB16Text expectAddr)
              ++ " does not match actual Address "
              ++ show (toB16Text actualAddr)


dieYaml :: String -> IO a
dieYaml err = throwM
  $ userError
  $ "Failure reading request yaml - invalid YAML format.\n"
  <>
  <> "\nError Message: "
  <> err

dieExec :: String -> IO a
dieExec err = throwM
    $ userError
    $ "Invalid 'exec' YAML format. Yaml file keys:\n"
    <> msg
    <> "\nError message: "
    <> err
  where
    msg = unpack
      [text|
        code: Transaction code
        codeFile: Transaction code file
        data: JSON transaction data
        dataFile: JSON transaction data file
        keyPairs: list of key pairs for signing (use pact -g to generate):
          [ public: base 16 public key
            secret: base 16 secret key
          ]
        nonce: optional request nonce, will use current time if not provided
        publicMeta:
        chainId: string chain id of the chain of execution
        sender: string denoting the sender of the transaction
        gasLimit: integer gas limit
        gasPrice: decimal gas price
        ttl: integer time-to-live value
        creationTime: integer tx execution time after offset
        type: exec
       |]

dieCont :: String -> IO a
dieCont err = throwM
    $ userError
    $ "Invalid 'cont' YAML format. Yaml file keys:\n"
    <> msg
    <> "\nError message: "
    <> err
  where
    msg = unpack
      [text|
        pactTxHash: integer transaction id of pact
        step: integer next step of a pact
        rollback: boolean for rollingback a pact
        proof: string spv proof of continuation (optional, cross-chain only)
        data: JSON transaction data
        dataFile: JSON transaction data file
        keyPairs: list of key pairs for signing (use pact -g to generate):
         [ public: string base 16 public key
           secret: string base 16 secret key
         ]
        publicMeta:
        chainId: string chain id of the chain of execution
        sender: string denoting the sender of the transaction
        gasLimit: integer gas limit
        gasPrice: decimal gas price
        ttl: integer time-to-live value
        creationTime: integer tx execution time after offset
        nonce: optional request nonce, will use current time if not provided
        type: cont
       |]
