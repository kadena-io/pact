{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Crypto.Hash.HyperlaneMessageId (hyperlaneMessageId, benchy) where

import Control.Lens ((^?), at, _Just, _Right, _1, to)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Short qualified as BSS
import Data.Decimal (Decimal)
import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.WideWord.Word256 (Word256(..))
import Data.Word (Word8, Word32)
import Ethereum.Misc (keccak256, _getKeccak256Hash, _getBytesN)
import Pact.Types.Runtime

import Gauge.Main

hyperlaneMessageId :: Object Name -> Text
hyperlaneMessageId o = case decodeHyperlaneMessageObject o of
  Nothing -> error "Couldn't decode HyperlaneMessage"
  Just hm -> getHyperlaneMessageId hm

benchy :: IO ()
benchy = do
  defaultMain
    [ bgroup "hyperlane"
        [ bench "0" $ whnf getHyperlaneMessageId hm0
        , bench "10" $ whnf getHyperlaneMessageId hm10
        , bench "20" $ whnf getHyperlaneMessageId hm20
        , bench "50" $ whnf getHyperlaneMessageId hm50
        , bench "100" $ whnf getHyperlaneMessageId hm100
        , bench "500" $ whnf getHyperlaneMessageId hm500
        , bench "1000" $ whnf getHyperlaneMessageId hm1000
        , bench "10000" $ whnf getHyperlaneMessageId hm10000
        ]
    ]

hm0, hm10, hm20, hm50, hm100, hm500, hm1000, hm10000 :: HyperlaneMessage
hm0 = genHM 0
hm10 = genHM 10
hm20 = genHM 20
hm50 = genHM 50
hm100 = genHM 100
hm500 = genHM 500
hm1000 = genHM 1_000
hm10000 = genHM 10_000

genHM :: Int -> HyperlaneMessage
genHM recipientSize = HyperlaneMessage
  { hmVersion = 0
  , hmNonce = 0
  , hmOriginDomain = 0
  , hmSender = BS.replicate 32 0
  , hmDestinationDomain = 0
  , hmRecipient = BS.replicate 32 0
  , hmTokenMessage = TokenMessageERC20
      { tmRecipient = Text.pack $ List.replicate recipientSize 'A'
      , tmAmount = 0
      , tmChainId = Nothing
      }
  }
{-# noinline genHM #-}

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

decodeHex :: Text -> Either String ByteString
decodeHex s
  | Just h <- Text.stripPrefix "0x" s = Base16.decode (Text.encodeUtf8 h)
  | otherwise = Left "decodeHex: does not start with 0x"

----------------------------------------------
--      Hyperlane Pact Object Decoding      --
----------------------------------------------

decodeHyperlaneMessageObject :: Object Name -> Maybe HyperlaneMessage
decodeHyperlaneMessageObject o = do
  let om = _objectMap (_oObject o)

  hmVersion <- om ^? at "version" . _Just . _TLiteral . _1 . _LInteger . to fromIntegral
  hmNonce <- om ^? at "nonce" . _Just . _TLiteral . _1 . _LInteger . to fromIntegral
  hmOriginDomain <- om ^? at "originDomain" . _Just . _TLiteral . _1 . _LInteger . to fromIntegral
  hmSender <- om ^? at "sender" . _Just . _TLiteral . _1 . _LString . to Text.encodeUtf8
  hmDestinationDomain <- om ^? at "destinationDomain" . _Just . _TLiteral . _1 . _LInteger . to fromIntegral
  hmRecipient <- om ^? at "recipient" . _Just . _TLiteral . _1 . _LString . to decodeHex . _Right

  let tokenObject = om ^? at "tokenMessage" . _Just . _TObject . _1
  hmTokenMessage <- case decodeTokenMessageERC20 =<< tokenObject of
    Just t -> pure t
    _ -> error "Couldn't encode TokenMessageERC20"

  pure HyperlaneMessage{..}

decodeTokenMessageERC20 :: Object Name -> Maybe TokenMessageERC20
decodeTokenMessageERC20 o = do
  let om = _objectMap (_oObject o)
  tmRecipient <- om ^? at "recipient" . _Just . _TLiteral . _1 . _LString
  tmAmount <- om ^? at "amount" . _Just . _TLiteral . _1 . _LDecimal . to decimalToWord
  let tmChainId = Nothing
  pure $ TokenMessageERC20{..}

decimalToWord :: Decimal -> Word256
decimalToWord d =
  let ethInWei = 1000000000000000000 -- 1e18
  in round $ d * ethInWei
