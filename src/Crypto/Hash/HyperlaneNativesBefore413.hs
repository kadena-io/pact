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
module Crypto.Hash.HyperlaneNativesBefore413
  ( hyperlaneMessageId
  , hyperlaneDecodeTokenMessage
  )
  where

import Control.Lens ((^?), at, _Just, Prism', _1)
import Control.Monad (unless)
import Data.Bifunctor (first)
import Data.Binary (get)
import Data.Binary.Get (Get, runGetOrFail, getByteString, isEmpty)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Short qualified as BSS
import Data.Decimal (Decimal)
import Data.Default (def)
import Data.List qualified as List
import Data.Map (Map)
import Data.Ratio ((%))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.WideWord.Word256 (Word256(..))
import Data.Word (Word8, Word32)
import Ethereum.Misc (keccak256, _getKeccak256Hash, _getBytesN)
import Pact.JSON.Decode qualified as J
import Pact.Types.PactValue (PactValue(..), fromPactValue)
import Pact.Types.Pretty (Doc, pretty)
import Pact.Types.Runtime (Object(..), ObjectMap(..), FieldKey, Name, Literal(..), _TLiteral, _TObject, _LDecimal, _LInteger, _LString, ChainId(..), toTObject, Type(..))
import Pact.Types.Term (Term(..), toTerm)
import Pact.Types.Util (decodeBase64UrlUnpadded)

----------------------------------------------
--               Primitive                  --
----------------------------------------------

hyperlaneMessageId :: Object Name -> Either Doc Text
hyperlaneMessageId o = case decodeHyperlaneMessageObject o of
  Left _err -> Left "Couldn't decode HyperlaneMessage"
  Right hm -> Right $ getHyperlaneMessageId hm

hyperlaneDecodeTokenMessage :: Text -> Either Doc (Term Name)
hyperlaneDecodeTokenMessage msg = do
  bytes <- first (const "Failed to base64-decode token message") $ decodeBase64UrlUnpadded (Text.encodeUtf8 msg)
  case runGetOrFail (getTokenMessageERC20 <* eof) (BL.fromStrict bytes) of
    -- In case of Binary decoding failure, emit a terse error message.
    -- If the error message begins with TokenError, we know that we
    -- created it, and it is going to be stable (non-forking).
    -- If it does not start with TokenMessage, it may have come from
    -- the Binary library, and we will suppress it to shield ourselves
    -- from forking behavior if we update our Binary version.
    Left (_,_,e) | "TokenMessage" `List.isPrefixOf` e -> Left $ "Decoding error: " <> pretty e
    Left _ -> Left "Decoding error: binary decoding failed"
    Right (_,_,(amount, chain, recipient)) ->
      case PGuard <$> J.eitherDecode (BS.fromStrict  $ Text.encodeUtf8 recipient) of
        Left _ -> Left "Could not parse recipient into a guard"
        Right g ->
          pure $ toTObject TyAny def
            [("recipient", fromPactValue g)
            ,("amount", TLiteral (LDecimal $ wordToDecimal amount) def)
            ,("chainId", toTerm chain)
            ]
  where
    -- The TokenMessage contains a recipient (text) and an amount (word-256).
    -- A schematic of the message format:
    -- 0000000000000000000000000000000000000000000000000000000000000060 # offset of the recipient string = 96, because first three lines are 32 bytes each
    -- 0000000000000000000000000000000000000000000000008ac7230489e80000 # amount = 10000000000000000000
    -- 0000000000000000000000000000000000000000000000000000000000000000 # chainId = 0
    -- 0000000000000000000000000000000000000000000000000000000000000062 # recipientSize = 98
    -- 7B2270726564223A20226B6579732D616C6C222C20226B657973223A205B2264 # {"pred": "keys-all", "keys": ["da1a339bd82d2c2e9180626a00dc043275deb3ababb27b5738abf6b9dcee8db6"]}
    -- 6131613333396264383264326332653931383036323661303064633034333237
    -- 3564656233616261626232376235373338616266366239646365653864623622
    -- 5D7D
    getTokenMessageERC20 :: Get (Word256, ChainId, Text)
    getTokenMessageERC20 = do

      -- Parse the size of the following amount field.
      firstOffset <- fromIntegral @Word256 @Int <$> getWord256be
      unless (firstOffset == 96)
        (fail $ "TokenMessage firstOffset expected 96, found " ++ show firstOffset)
      tmAmount <- getWord256be
      tmChainId <- getWord256be

      recipientSize <- getWord256be
      tmRecipient <- Text.decodeUtf8 <$> getRecipient recipientSize

      return (tmAmount, ChainId { _chainId = Text.pack (show (toInteger tmChainId))}, tmRecipient)
      where
        getWord256be = get @Word256

        -- | Reads a given number of bytes and the rest because binary data padded up to 32 bytes.
        getRecipient :: Word256 -> Get BS.ByteString
        getRecipient size = do
          recipient <- BS.take (fromIntegral size) <$> getByteString (fromIntegral $ size + restSize size)
          if BS.length recipient < fromIntegral size
            then fail "TokenMessage recipient was smaller than expected"
            else pure recipient

    wordToDecimal :: Word256 -> Decimal
    wordToDecimal w =
      let ethInWei = 1000000000000000000 -- 1e18
      in fromRational (toInteger w % ethInWei)

    eof :: Get ()
    eof = do
      done <- isEmpty
      unless done $ fail "pending bytes in input"

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

decodeHex :: Text -> Either Doc ByteString
decodeHex s = do
  h <- case Text.stripPrefix "0x" s of
    Nothing -> Left "Missing 0x prefix on hex string"
    Just h -> pure h

  case Base16.decode (Text.encodeUtf8 h) of
    Left _ -> Left "Failed to hex-decode string"
    Right b -> Right b

----------------------------------------------
--      Hyperlane Pact Object Decoding      --
----------------------------------------------

decodeHyperlaneMessageObject :: Object Name -> Either Doc HyperlaneMessage
decodeHyperlaneMessageObject o = do
  let om = _objectMap (_oObject o)

  hmVersion           <- fromIntegral @Integer @Word8  <$> grabField om "version" _LInteger
  hmNonce             <- fromIntegral @Integer @Word32 <$> grabField om "nonce" _LInteger
  hmOriginDomain      <- fromIntegral @Integer @Word32 <$> grabField om "originDomain" _LInteger
  hmSender            <- Text.encodeUtf8               <$> grabField om "sender" _LString
  hmDestinationDomain <- fromIntegral @Integer @Word32 <$> grabField om "destinationDomain" _LInteger
  hmRecipient         <- decodeHex                     =<< grabField om "recipient" _LString

  let tokenObject = om ^? at "tokenMessage" . _Just . _TObject . _1
  hmTokenMessage <- case tokenObject of
    Just t -> decodeTokenMessageERC20 t
    Nothing -> Left "tokenMessage object missing"

  pure HyperlaneMessage{..}

decodeTokenMessageERC20 :: Object Name -> Either Doc TokenMessageERC20
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

grabField :: Map FieldKey (Term Name) -> FieldKey -> Prism' Literal a -> Either Doc a
grabField m key p = case m ^? at key . _Just . _TLiteral . _1 . p of
  Nothing -> Left $ "Failed to find key in object: " <> pretty key
  Just a -> Right a
