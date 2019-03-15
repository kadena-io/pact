{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

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

import Control.Monad.State.Strict
import Control.Monad.Catch
import Data.List
import Prelude
import System.Directory
import System.FilePath

import Data.Aeson
import GHC.Generics
import qualified Data.Yaml as Y
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Text (Text,pack)
import Data.Text.Encoding
import Data.Thyme.Clock
import Data.Default (def)

import Pact.Types.Crypto
import Pact.Types.Util
import Pact.Types.Command
import Pact.Types.RPC
import Pact.Types.Runtime hiding (PublicKey)
import Pact.Types.API

data ApiKeyPair = ApiKeyPair {
  _akpSecret  :: PrivateKeyBS,
  _akpPublic  :: Maybe PublicKeyBS,
  _akpAddress :: Maybe Text,
  _akpScheme  :: Maybe PPKScheme
  } deriving (Eq, Show, Generic)
instance ToJSON ApiKeyPair where toJSON = lensyToJSON 4
instance FromJSON ApiKeyPair where parseJSON = lensyParseJSON 4

data ApiReq = ApiReq {
  _ylType :: Maybe String,
  _ylPactId :: Maybe PactId,
  _ylStep :: Maybe Int,
  _ylRollback :: Maybe Bool,
  _ylResume :: Maybe Value,
  _ylData :: Maybe Value,
  _ylDataFile :: Maybe FilePath,
  _ylCode :: Maybe String,
  _ylCodeFile :: Maybe FilePath,
  _ylKeyPairs :: [ApiKeyPair],
  _ylNonce :: Maybe String
  } deriving (Eq,Show,Generic)
instance ToJSON ApiReq where toJSON = lensyToJSON 3
instance FromJSON ApiReq where parseJSON = lensyParseJSON 3

apiReq :: FilePath -> Bool -> IO ()
apiReq fp local = do
  (_,exec) <- mkApiReq fp
  if local then
    BSL.putStrLn $ encode exec
    else
    BSL.putStrLn $ encode $ SubmitBatch [exec]
  return ()

mkApiReq :: FilePath -> IO ((ApiReq,String,Value,PublicMeta),Command Text)
mkApiReq fp = do
  ar@ApiReq {..} <- either (dieAR . show) return =<<
                 liftIO (Y.decodeFileEither fp)
  kps <- mkKeyPairs _ylKeyPairs
  case _ylType of
    Just "exec" -> mkApiReqExec ar kps fp
    Just "cont" -> mkApiReqCont ar kps fp
    Nothing     -> mkApiReqExec ar kps fp -- Default
    _      -> dieAR "Expected a valid message type: either 'exec' or 'cont'"



mkApiReqExec :: ApiReq -> [SomeKeyPair] -> FilePath -> IO ((ApiReq,String,Value,PublicMeta),Command Text)
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
  let pubMeta = def
  ((ar,code,cdata,pubMeta),) <$> mkExec code cdata pubMeta kps _ylNonce

mkExec :: String -> Value -> PublicMeta -> [SomeKeyPair] -> Maybe String -> IO (Command Text)
mkExec code mdata pubMeta kps ridm = do
  rid <- maybe (show <$> getCurrentTime) return ridm
  cmd <- mkCommand
         kps
         pubMeta
         (pack $ show rid)
         (Exec (ExecMsg (pack code) mdata))
  return $ decodeUtf8 <$> cmd


mkApiReqCont :: ApiReq -> [SomeKeyPair] -> FilePath -> IO ((ApiReq,String,Value,PublicMeta),Command Text)
mkApiReqCont ar@ApiReq{..} kps fp = do
  txId <- case _ylPactId of
    Just t  -> return t
    Nothing -> dieAR "Expected a 'txid' entry"

  step <- case _ylStep of
    Just s  -> return s
    Nothing -> dieAR "Expected a 'step' entry"

  rollback <- case _ylRollback of
    Just r  -> return r
    Nothing -> dieAR "Expected a 'rollback' entry"

  cdata <- withCurrentDirectory (takeDirectory fp) $ do
    case (_ylDataFile,_ylData) of
      (Nothing,Just v) -> return v -- either (\e -> dieAR $ "Data decode failed: " ++ show e) return $ eitherDecode (BSL.pack v)
      (Just f,Nothing) -> liftIO (BSL.readFile f) >>=
                          either (\e -> dieAR $ "Data file load failed: " ++ show e) return .
                          eitherDecode
      (Nothing,Nothing) -> return Null
      _ -> dieAR "Expected either a 'data' or 'dataFile' entry, or neither"
  let pubMeta = def
  ((ar,"",cdata,pubMeta),) <$> mkCont txId step rollback cdata pubMeta kps _ylNonce

mkCont :: PactId -> Int -> Bool  -> Value -> PublicMeta -> [SomeKeyPair]
  -> Maybe String -> IO (Command Text)
mkCont txid step rollback mdata pubMeta kps ridm = do
  rid <- maybe (show <$> getCurrentTime) return ridm
  cmd <- mkCommand
         kps
         pubMeta
         (pack $ show rid)
         (Continuation (ContMsg txid step rollback mdata) :: (PactRPC ContMsg))
  return $ decodeUtf8 <$> cmd



mkKeyPairs :: [ApiKeyPair] -> IO [SomeKeyPair]
mkKeyPairs keyPairs = traverse mkPair keyPairs
  where isValidKeyPair ApiKeyPair{..} =
          case _akpScheme of
            Nothing  -> importKeyPair defaultScheme _akpPublic _akpSecret
            Just ppk -> importKeyPair (toScheme ppk) _akpPublic _akpSecret

        mkPair akp = case _akpAddress akp of
          Nothing    -> either dieAR return (isValidKeyPair akp)
          Just addrT -> do
            addrBS <- either dieAR return (parseB16TextOnly addrT)
            kp     <- either dieAR return (isValidKeyPair akp)

            -- Enforces that user provided address matches the address derived from the Public Key
            -- for transparency and a better user experience. User provided address not used except
            -- for this purpose.

            case (addrBS, formatPublicKey kp) of
              (expectAddr, actualAddr)
                | expectAddr == actualAddr  -> return kp
                | otherwise                 -> dieAR $ "Address provided "
                                               ++ show (toB16Text expectAddr)
                                               ++ " does not match actual Address "
                                               ++ show (toB16Text actualAddr)

dieAR :: String -> IO a
dieAR errMsg = throwM . userError $ "Failure reading request yaml. Yaml file keys: \n\
  \  code: Transaction code \n\
  \  codeFile: Transaction code file \n\
  \  data: JSON transaction data \n\
  \  dataFile: JSON transaction data file \n\
  \  keyPairs: list of key pairs for signing (use pact -g to generate): [\n\
  \    public: base 16 public key \n\
  \    secret: base 16 secret key \n\
  \    address: base 16 address   \n\
  \    scheme: cryptographic scheme \n\
  \    ] \n\
  \  nonce: optional request nonce, will use current time if not provided \n\
  \Error message: " ++ errMsg
