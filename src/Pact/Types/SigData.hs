{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      :  Pact.Types.SigData
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--

module Pact.Types.SigData
  ( PublicKeyHex(..)
  , SigData(..)
  , commandToSigData
  , sigDataToCommand
  , sampleSigData
  ) where

import Control.Error
import Control.Monad.State.Strict
import Data.Aeson
import Data.Aeson.Types
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import Data.String
import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.Text.Encoding
import GHC.Generics

import Pact.Parse
import Pact.Types.Command
import Pact.Types.Runtime

newtype PublicKeyHex = PublicKeyHex { unPublicKeyHex :: Text }
  deriving (Eq,Ord,Show,Generic)

instance IsString PublicKeyHex where
  fromString = PublicKeyHex . pack
instance ToJSONKey PublicKeyHex
instance FromJSONKey PublicKeyHex
instance ToJSON PublicKeyHex where
  toJSON (PublicKeyHex hex) = String hex
instance FromJSON PublicKeyHex where
  parseJSON = withText "PublicKeyHex" $ \t -> do
    if T.length t == 64 && isRight (parseB16TextOnly t)
      then return $ PublicKeyHex t
      else fail "Public key must have 64 hex characters"

-- | This type is designed to represent signatures in all possible situations
-- where they might be wanted. It must satisfy at least the following
-- requirements:
--
-- 1. It must support multisig.
-- 2. It must have values representing state both before and after all the
-- signatures have been supplied.
-- 3. It must be usable in a cold wallet setting where transfer may be high
-- latency with low bandwidth. This means having the option to leave out the
-- payload to facilitate easier transmission via QR codes and other low
-- bandwidth channels.
data SigData a = SigData
  { _sigDataHash :: PactHash
  , _sigDataSigs :: [(PublicKeyHex, Maybe UserSig)]
  -- ^ This is not a map because the order must be the same as the signers inside the command.
  , _sigDataCmd :: Maybe a
  } deriving (Eq,Show,Generic)

instance ToJSON a => ToJSON (SigData a) where
  toJSON (SigData h s c) = object $ concat
    [ ["hash" .= h]
    , ["sigs" .= object (map (\(k,ms) -> (unPublicKeyHex k, maybe Null (toJSON . _usSig) ms)) s)]
    , "cmd" .?= c
    ]
    where
      k .?= v = case v of
        Nothing -> mempty
        Just v' -> [k .= v']

instance FromJSON a => FromJSON (SigData a) where
  parseJSON = withObject "SigData" $ \o -> do
    h <- o .: "hash"
    s <- withObject "SigData Pairs" f =<< (o .: "sigs")
    c <- o .:? "cmd"
    pure $ SigData h s c
    where
      f = mapM g . HM.toList
      g (k,Null) = pure (PublicKeyHex k, Nothing)
      g (k,String t) = pure (PublicKeyHex k, Just $ UserSig t)
      g (_,v) = typeMismatch "Signature should be String or Null" v

commandToSigData :: Command Text -> Either String (SigData Text)
commandToSigData c = do
  let ep = traverse parsePact =<< (eitherDecodeStrict' $ encodeUtf8 $ _cmdPayload c)
  case ep :: Either String (Payload Value ParsedCode) of
    Left e -> Left $ "Error decoding payload: " <> e
    Right p -> do
      let sigs = map (\s -> (PublicKeyHex $ _siPubKey s, Nothing)) $ _pSigners p
      Right $ SigData (_cmdHash c) sigs (Just $ _cmdPayload c)

sigDataToCommand :: SigData Text -> Either String (Command Text)
sigDataToCommand (SigData _ _ Nothing) = Left "Can't reconstruct command"
sigDataToCommand (SigData h sigList (Just c)) = do
  payload :: Payload Value ParsedCode <- traverse parsePact =<< eitherDecodeStrict' (encodeUtf8 c)
  let sigMap = M.fromList sigList
  -- It is ok to use a map here because we're iterating over the signers list and only using the map for lookup.
  let sigs = catMaybes $ map (\signer -> join $ M.lookup (PublicKeyHex $ _siPubKey signer) sigMap) $ _pSigners payload
  let sessionPubkey = Nothing
  pure $ Command c sigs sessionPubkey h

sampleSigData :: SigData Text
sampleSigData = SigData (either error id $ fromText' "b57_gSRIwDEo6SAYseppem57tykcEJkmbTFlCHDs0xc")
  [("acbe76b30ccaf57e269a0cd5eeeb7293e7e84c7d68e6244a64c4adf4d2df6ea1", Nothing)] Nothing
