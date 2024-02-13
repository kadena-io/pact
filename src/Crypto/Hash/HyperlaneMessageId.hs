{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

-- | Implementation of the `hyperlane-message-id` pact native.
--
--   `hyperlane-message-id` takes as input a Pact object representing a
--   'HyperlaneMessage', and returns a base16-encoded hash of the abi-encoding
--   of the input.
module Crypto.Hash.HyperlaneMessageId (hyperlaneMessageId) where

import Control.Error.Util (hush)
import Control.Lens ((^?), at, _Just, Prism', _1)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Short qualified as BSS
import Data.Decimal (Decimal)
import Data.Map (Map)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.WideWord.Word256 (Word256(..))
import Data.Word (Word8, Word32)
import Ethereum.Misc (keccak256, _getKeccak256Hash, _getBytesN)
import Pact.Types.Runtime (Object(..), ObjectMap(..), FieldKey, Name, Literal, _TLiteral, _TObject, _LDecimal, _LInteger, _LString)
import Pact.Types.Term (Term)

----------------------------------------------
--               Primitive                  --
----------------------------------------------

hyperlaneMessageId :: Object Name -> Text
hyperlaneMessageId o = case decodeHyperlaneMessageObject o of
  Nothing -> error "Couldn't decode HyperlaneMessage"
  Just hm -> getHyperlaneMessageId hm

----------------------------------------------
--        Hyperlane Message Encoding        --
----------------------------------------------

data HyperlaneMessage = HyperlaneMessage
  { hmVersion :: Word8 -- uint8
  , hmNonce :: Word32 -- uint32
  , hmOriginDomain :: Word32 -- uint32
  , hmSender :: ByteString -- 32x uint8
  , hmDestinationDomain :: Word32 -- uint32
  , hmRecipient :: ByteString -- 32x uint8
  , hmTokenMessage :: TokenMessageERC20 -- variable
  }

packHyperlaneMessage :: HyperlaneMessage -> Builder
packHyperlaneMessage (HyperlaneMessage{..}) =
  BB.word8 hmVersion
  <> BB.word32BE hmNonce
  <> BB.word32BE hmOriginDomain
  <> BB.byteString (padLeft hmSender)
  <> BB.word32BE hmDestinationDomain
  <> BB.byteString (padLeft hmRecipient)
  <> packTokenMessageERC20 hmTokenMessage

data TokenMessageERC20 = TokenMessageERC20
  { tmRecipient :: Text -- variable
  , tmAmount :: Word256 -- uint256
  , tmChainId :: Maybe Word256 -- uint256
  }

packTokenMessageERC20 :: TokenMessageERC20 -> Builder
packTokenMessageERC20 t =
  word256BE 64
  <> word256BE (tmAmount t)

  <> word256BE recipientSize
  <> BB.byteString recipient
  where
    (recipient, recipientSize) = padRight (Text.encodeUtf8 (tmRecipient t))

word256BE :: Word256 -> Builder
word256BE (Word256 a b c d) =
  BB.word64BE a <> BB.word64BE b <> BB.word64BE c <> BB.word64BE d

-- | Pad with zeroes on the left to 32 bytes
--
-- > padLeft "hello world"
-- "\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NULhello world"
padLeft :: ByteString -> ByteString
padLeft s = BS.replicate (32 - BS.length s) 0 <> s

-- | Pad with zeroes on the right, such that the resulting size is a multiple of 32.
--
-- > padRight "hello world"
-- ("hello world\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL",11)
padRight :: ByteString -> (ByteString, Word256)
padRight s =
  let
    size = BS.length s
    missingZeroes = restSize size
  in (s <> BS.replicate missingZeroes 0, fromIntegral size)

-- | Returns the modular of 32 bytes.
restSize :: Integral a => a -> a
restSize size = (32 - size) `mod` 32

----------------------------------------------
--        Hyperlane Message Hashing         --
----------------------------------------------

getHyperlaneMessageId :: HyperlaneMessage -> Text
getHyperlaneMessageId =
  encodeHex
  . keccak256Hash
  . BL.toStrict
  . BB.toLazyByteString
  . packHyperlaneMessage

keccak256Hash :: ByteString -> ByteString
keccak256Hash = BSS.fromShort . _getBytesN . _getKeccak256Hash . keccak256

encodeHex :: ByteString -> Text
encodeHex b = "0x" <> Text.decodeUtf8 (Base16.encode b)

decodeHex :: Text -> Maybe ByteString
decodeHex s = do
  h <- Text.stripPrefix "0x" s
  hush (Base16.decode (Text.encodeUtf8 h))

----------------------------------------------
--      Hyperlane Pact Object Decoding      --
----------------------------------------------

decodeHyperlaneMessageObject :: Object Name -> Maybe HyperlaneMessage
decodeHyperlaneMessageObject o = do
  let om = _objectMap (_oObject o)

  hmVersion           <- fromIntegral @Integer @Word8  <$> grabField om "version" _LInteger
  hmNonce             <- fromIntegral @Integer @Word32 <$> grabField om "nonce" _LInteger
  hmOriginDomain      <- fromIntegral @Integer @Word32 <$> grabField om "originDomain" _LInteger
  hmSender            <- Text.encodeUtf8               <$> grabField om "sender" _LString
  hmDestinationDomain <- fromIntegral @Integer @Word32 <$> grabField om "destinationDomain" _LInteger
  hmRecipient         <- decodeHex                     =<< grabField om "recipient" _LString

  let tokenObject = om ^? at "tokenMessage" . _Just . _TObject . _1
  hmTokenMessage <- case decodeTokenMessageERC20 =<< tokenObject of
    Just t -> pure t
    _ -> error "Couldn't encode TokenMessageERC20"

  pure HyperlaneMessage{..}

decodeTokenMessageERC20 :: Object Name -> Maybe TokenMessageERC20
decodeTokenMessageERC20 o = do
  let om = _objectMap (_oObject o)
  tmRecipient <- grabField om "recipient" _LString
  tmAmount <- decimalToWord <$> grabField om "amount" _LDecimal
  let tmChainId = Nothing
  pure $ TokenMessageERC20{..}

decimalToWord :: Decimal -> Word256
decimalToWord d =
  let ethInWei = 1_000_000_000_000_000_000 -- 1e18
  in round $ d * ethInWei

grabField :: Map FieldKey (Term Name) -> FieldKey -> Prism' Literal a -> Maybe a
grabField m key p = m ^? at key . _Just . _TLiteral . _1 . p
