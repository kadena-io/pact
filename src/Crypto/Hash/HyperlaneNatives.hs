{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Implementation of Hyperlane natives.
module Crypto.Hash.HyperlaneNatives
  ( HyperlaneMessage(..)
  , TokenMessageERC20(..)
  , decodeHyperlaneMessageObject
  , decodeTokenMessageERC20
  , packHyperlaneMessage
  , packTokenMessageERC20
  , unpackTokenMessageERC20
  , tokenMessageToTerm

    -- Implementation of natives
  , hyperlaneMessageId
  , hyperlaneDecodeTokenMessage
  ) where

import Control.Error.Util (hush)
import Control.Lens ((^?), at, _Just, Prism', _1)
import Control.Monad (guard, unless)
import Control.Monad.Except (throwError)
import Data.Bifunctor (first)
import Data.Binary qualified as Bin
import Data.Binary.Get (Get)
import Data.Binary.Get qualified as Bin
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
import Pact.Types.Exp (Literal(..))
import Pact.Types.PactValue (PactValue(PGuard), fromPactValue)
import Pact.Types.Pretty (Doc, pretty)
import Pact.Types.Runtime (Object(..), ObjectMap(..), FieldKey, Name, Type(TyAny), _TLiteral, _TObject, _LDecimal, _LInteger, _LString, toTObject, ChainId(..))
import Pact.Types.Term (Term(..), toTerm)
import Pact.Types.Util (decodeBase64UrlUnpadded)

----------------------------------------------
--               Primitives                 --
----------------------------------------------

hyperlaneMessageId :: Object Name -> Text
hyperlaneMessageId o = case decodeHyperlaneMessageObject o of
  Nothing -> error "Couldn't decode HyperlaneMessage"
  Just hm -> getHyperlaneMessageId hm

-- | Decode a hyperlane 'TokenMessageERC20'
hyperlaneDecodeTokenMessage :: Text -> Either Doc (Term Name)
hyperlaneDecodeTokenMessage i = do
  tm <- first displayHyperlaneDecodeError $ do
    bytes <- first (const HyperlaneDecodeErrorBase64) $ decodeBase64UrlUnpadded (Text.encodeUtf8 i)
    case Bin.runGetOrFail (unpackTokenMessageERC20 <* eof) (BL.fromStrict bytes) of
      Left (_, _, e) | "TokenMessage" `List.isPrefixOf` e -> do
        throwError $ HyperlaneDecodeErrorInternal e
      Left _ -> do
        throwError HyperlaneDecodeErrorBinary
      Right (_, _, tm) -> do
        pure tm
  tokenMessageToTerm tm

----------------------------------------------
--              Error Types                 --
----------------------------------------------

data HyperlaneDecodeError
  = HyperlaneDecodeErrorBase64
    -- ^ We discard the error message in this case to maintain error message
    --   equality with the original implementation - otherwise this would have a
    --   string in it
  | HyperlaneDecodeErrorInternal String
    -- ^ Decoding error that our own code threw, not `binary`
  | HyperlaneDecodeErrorBinary
    -- ^ We encountered an error not thrown by us but by `binary`. We discard
    --   the error message to avoid potentially forking behaviour introduced
    --   by a library update.
  | HyperlaneDecodeErrorParseRecipient
    -- ^ Failed to parse the Recipient into a Guard

displayHyperlaneDecodeError :: HyperlaneDecodeError -> Doc
displayHyperlaneDecodeError = \case
  HyperlaneDecodeErrorBase64 -> "Failed to base64-decode token message"
  HyperlaneDecodeErrorInternal errmsg -> "Decoding error: " <> pretty errmsg
  HyperlaneDecodeErrorBinary -> "Decoding error: binary decoding failed"
  HyperlaneDecodeErrorParseRecipient -> "Could not parse recipient into a guard"


----------------------------------------------
--         Hyperlane Message Types          --
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

data TokenMessageERC20 = TokenMessageERC20
  { tmRecipient :: Text -- variable
  , tmAmount :: Word256 -- uint256
  , tmChainId :: Word256 -- uint256
  }

----------------------------------------------
--    Hyperlane Message Binary Encoding     --
----------------------------------------------

packHyperlaneMessage :: HyperlaneMessage -> Builder
packHyperlaneMessage (HyperlaneMessage{..}) =
  BB.word8 hmVersion
  <> BB.word32BE hmNonce
  <> BB.word32BE hmOriginDomain
  <> BB.byteString (padLeft hmSender)
  <> BB.word32BE hmDestinationDomain
  <> BB.byteString (padLeft hmRecipient)
  <> packTokenMessageERC20 hmTokenMessage

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
packTokenMessageERC20 :: TokenMessageERC20 -> Builder
packTokenMessageERC20 t =
  word256BE 96
  <> word256BE (tmAmount t)
  <> word256BE (tmChainId t)
  <> word256BE recipientSize
  <> BB.byteString recipient
  where
    (recipient, recipientSize) = padRight (Text.encodeUtf8 (tmRecipient t))

unpackTokenMessageERC20 :: Get TokenMessageERC20
unpackTokenMessageERC20 = do
  firstOffset <- Bin.get @Word256
  unless (firstOffset == 96) $ do
    fail $ "TokenMessage firstOffset expected 96, found " ++ show firstOffset

  tmAmount <- Bin.get @Word256
  tmChainId <- Bin.get @Word256

  recipientSize <- Bin.get @Word256
  tmRecipient <- Text.decodeUtf8 <$> do
    let size = fromIntegral @Word256 @Int recipientSize
    recipient <- BS.take size
      <$> Bin.getByteString (fromIntegral @Word256 @Int (recipientSize + restSize recipientSize))
    if BS.length recipient < size
    then fail "TokenMessage recipient was smaller than expected"
    else pure recipient

  pure $ TokenMessageERC20 {..}

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

  hmVersion           <- grabInt @Word8  om "version"
  hmNonce             <- grabInt @Word32 om "nonce"
  hmOriginDomain      <- grabInt @Word32 om "originDomain"
  hmSender            <- Text.encodeUtf8 <$> grabField om "sender" _LString
  hmDestinationDomain <- grabInt @Word32 om "destinationDomain"
  hmRecipient         <- decodeHex =<< grabField om "recipient" _LString

  let tokenObject = om ^? at "tokenMessage" . _Just . _TObject . _1
  hmTokenMessage <- case decodeTokenMessageERC20 =<< tokenObject of
    Just t -> pure t
    _ -> error "Couldn't decode TokenMessageERC20"

  pure HyperlaneMessage{..}

decodeTokenMessageERC20 :: Object Name -> Maybe TokenMessageERC20
decodeTokenMessageERC20 o = do
  let om = _objectMap (_oObject o)
  tmRecipient <- grabField om "recipient" _LString
  tmAmount <- decimalToWord <$> grabField om "amount" _LDecimal
  tmChainId <- grabInt @Word256 om "chainId"
  pure $ TokenMessageERC20{..}

----------------------------------------------
--                Utilities                 --
----------------------------------------------

decimalToWord :: Decimal -> Word256
decimalToWord d = round (d * ethInWei)

wordToDecimal :: Word256 -> Decimal
wordToDecimal w = fromRational (toInteger w % ethInWei)

ethInWei :: Num a => a
ethInWei = 1_000_000_000_000_000_000 -- 1e18
{-# inline ethInWei #-}

grabField :: Map FieldKey (Term Name) -> FieldKey -> Prism' Literal a -> Maybe a
grabField m key p = m ^? at key . _Just . _TLiteral . _1 . p

-- | Grab a bounded integral value out of the pact object, and make sure
--   the integer received is a valid element of that type
grabInt :: forall a. (Integral a, Bounded a) => Map FieldKey (Term Name) -> FieldKey -> Maybe a
grabInt m key = do
  i <- grabField m key _LInteger
  guard (i >= fromIntegral @a @Integer minBound && i <= fromIntegral @a @Integer maxBound)
  pure (fromIntegral @Integer @a i)

eof :: Get ()
eof = do
  done <- Bin.isEmpty
  unless done $ fail "pending bytes in input"

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

tokenMessageToTerm :: TokenMessageERC20 -> Either Doc (Term Name)
tokenMessageToTerm tm = first displayHyperlaneDecodeError $ do
  g <- first (const HyperlaneDecodeErrorParseRecipient) $ fmap PGuard $ J.eitherDecode
    (BL.fromStrict (Text.encodeUtf8 (tmRecipient tm)))
  let chainId = ChainId { _chainId = Text.pack (show (toInteger (tmChainId tm))) }
  pure $ toTObject TyAny def
    [ ("recipient", fromPactValue g)
    , ("amount", TLiteral (LDecimal (wordToDecimal (tmAmount tm))) def)
    , ("chainId", toTerm chainId)
    ]
