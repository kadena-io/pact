{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      :  Pact.Types.Command
-- Copyright   :  (C) 2016 Stuart Popejoy, Will Martino
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>, Will Martino <will@kadena.io>
--
-- Specifies types for commands in a consensus/DL setting.
--

module Pact.Types.Command
--  (Command(..)
--  ) where
  where

import Control.Applicative
import Control.Lens hiding ((.=))
import Control.Monad.Reader

import Data.Aeson as A
import Data.Maybe
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.Serialize as SZ
import Data.String
import Data.Text hiding (filter, null, all)
import Data.Hashable (Hashable)


import GHC.Generics hiding (from)
import Prelude hiding (log,exp)

import Pact.Types.Util
import Pact.Types.Orphans ()
import Pact.Types.Crypto as Base


data Command a = PublicCommand
  { _cmdPayload :: !a
  , _cmdSigs :: ![UserSig]
  , _cmdHash :: !Hash
  } deriving (Eq,Show,Generic,Functor,Foldable,Traversable)
instance (Serialize a) => Serialize (Command a)
instance (ToJSON a) => ToJSON (Command a) where
    toJSON (PublicCommand payload uSigs hsh) =
        object [ "cmd" .= payload
               , "sigs" .= toJSON uSigs
               , "hash" .= hsh
               ]
instance (FromJSON a) => FromJSON (Command a) where
    parseJSON = withObject "Command" $ \o ->
                PublicCommand <$> (o .: "cmd")
                              <*> (o .: "sigs" >>= parseJSON)
                              <*> (o .: "hash")

mkCommand :: ToJSON a => [(PPKScheme, PrivateKey, Base.PublicKey)] -> Text -> a -> Command ByteString
mkCommand creds nonce a = mkCommand' creds $ BSL.toStrict $ A.encode (Payload a nonce)

mkCommand' :: [(PPKScheme, PrivateKey, Base.PublicKey)] -> ByteString -> Command ByteString
mkCommand' creds env = PublicCommand env (sig <$> creds) hsh
  where
    hsh = hash env
    sig (scheme, sk, pk) = UserSig scheme (toB16Text $ exportPublic pk) (toB16Text $ exportSignature $ sign hsh sk pk)

data ProcessedCommand a =
  ProcSucc !(Command (Payload a)) |
  ProcFail !String
  deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

verifyCommand :: FromJSON a => Command ByteString -> ProcessedCommand a
verifyCommand orig@PublicCommand{..} = case (ppcmdPayload', ppcmdHash', mSigIssue) of
      (Right env', Right _, Nothing) -> ProcSucc $! orig { _cmdPayload = env' }
      (e, h, s) -> ProcFail $! "Invalid command: " ++ toErrStr e ++ toErrStr h ++ fromMaybe "" s
  where
    !ppcmdPayload' = A.eitherDecodeStrict' _cmdPayload
    !(ppcmdSigs' :: [(UserSig,Bool)]) = (\u -> (u,verifyUserSig _cmdHash u)) <$> _cmdSigs
    !ppcmdHash' = verifyHash _cmdHash _cmdPayload
    mSigIssue = if all snd ppcmdSigs' then Nothing
      else Just $ "Invalid sig(s) found: " ++ show ((A.encode . fst) <$> filter (not.snd) ppcmdSigs')
    toErrStr :: Either String a -> String
    toErrStr (Right _) = ""
    toErrStr (Left s) = s ++ "; "
{-# INLINE verifyCommand #-}



data Payload a = Payload
  { _pPayload :: !a
  , _pNonce :: !Text
  } deriving (Show, Eq, Generic, Functor, Foldable, Traversable)
instance ToJSON a => ToJSON (Payload a) where
  toJSON (Payload r rid) = object [ "payload" .= r, "nonce" .= rid]
instance FromJSON a => FromJSON (Payload a) where
  parseJSON = withObject "Payload" $ \o ->
                    Payload <$> o .: "payload" <*> o .: "nonce"


data UserSig = UserSig
  { _usScheme :: !PPKScheme
  , _usPubKey :: !Text
  , _usSig :: !Text }
  deriving (Eq, Ord, Show, Generic)


instance Serialize UserSig
instance ToJSON UserSig where
  toJSON UserSig {..} = object [
    "scheme" .= _usScheme, "pubKey" .= _usPubKey, "sig" .= _usSig ]
instance FromJSON UserSig where
  parseJSON = withObject "UserSig" $ \o ->
    UserSig . fromMaybe ED25519 <$> o .:? "scheme" <*> o .: "pubKey" <*> o .: "sig"




verifyUserSig :: Hash -> UserSig -> Bool
verifyUserSig h UserSig{..} = case _usScheme of
  ED25519 -> case (fromJSON (String _usPubKey),fromJSON (String _usSig)) of
    (Success pk,Success sig) -> valid h pk sig
    _ -> False
{-# INLINE verifyUserSig #-}


data CommandError = CommandError {
      _ceMsg :: String
    , _ceDetail :: Maybe String
}
instance ToJSON CommandError where
    toJSON (CommandError m d) =
        object $ [ "status" .= ("failure" :: String)
                 , "error" .= m ] ++
        maybe [] ((:[]) . ("detail" .=)) d

data CommandSuccess a = CommandSuccess {
      _csData :: a
    }
instance (ToJSON a) => ToJSON (CommandSuccess a) where
    toJSON (CommandSuccess a) =
        object [ "status" .= ("success" :: String)
               , "data" .= a ]


data CommandResult = CommandResult {
  _prReqKey :: RequestKey,
  _prResult :: Value
  } deriving (Eq,Show)


cmdToRequestKey :: Command a -> RequestKey
cmdToRequestKey PublicCommand {..} = RequestKey _cmdHash

type ApplyCmd e = e -> Command ByteString -> IO CommandResult
type ApplyPPCmd e a = e -> Command ByteString -> ProcessedCommand a -> IO CommandResult

data CommandExecInterface e a = CommandExecInterface
  { _ceiApplyCmd :: ApplyCmd e
  , _ceiApplyPPCmd :: ApplyPPCmd e a
  }


requestKeyToB16Text :: RequestKey -> Text
requestKeyToB16Text (RequestKey h) = hashToB16Text h


newtype RequestKey = RequestKey { unRequestKey :: Hash}
  deriving (Eq, Ord, Generic, Serialize, Hashable)
instance ToJSON RequestKey where toJSON = String . requestKeyToB16Text
instance FromJSON RequestKey where parseJSON v = RequestKey <$> parseJSON v

instance Show RequestKey where
  show (RequestKey rk) = show rk

initialRequestKey :: RequestKey
initialRequestKey = RequestKey initialHash



makeLenses ''UserSig
makeLenses ''CommandExecInterface
