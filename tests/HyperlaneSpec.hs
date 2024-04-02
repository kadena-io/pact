{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module HyperlaneSpec (spec) where

import Data.Either (fromRight)
import Data.Decimal (Decimal)
import Data.WideWord.Word256 (Word256(..))
import Data.Maybe (fromMaybe)
import Control.Lens ((^?), at, _Just, _1)
import Crypto.Hash.HyperlaneNatives (HyperlaneMessage(..), TokenMessageERC20(..), decodeHyperlaneMessageObject, hyperlaneMessageId, hyperlaneDecodeTokenMessage, packTokenMessageERC20, tokenMessageToTerm)
import Data.Default (def)
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Pact.Types.Runtime (FieldKey, Object(..), ObjectMap(..), Term, Literal(..), tLit, tStr, asString, toTObject, Type(..), _TObject)
import Test.Hspec
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Base16 qualified as Base16
import Pact.Types.Util (encodeBase64UrlUnpadded)

referenceObject :: Object n
referenceObject = mkObject
  [ ("message",) $ obj
      [ ("version", tLit $ LInteger 3)
      , ("nonce", tLit $ LInteger 0)
      , ("originDomain", tLit $ LInteger 31_337)
      , ("sender", tStr $ asString ("0x000000000000000000000000c29f578e252f1a97fb3cbe4c3c570af74fa74405" :: Text))
      , ("destinationDomain", tLit $ LInteger 626)
      , ("recipient", tStr $ asString ("0x30472d564f4549754b6b4a723750756b434975464e306d5a4371644f5a695754" :: Text))
      , ("tokenMessage", obj
          [ ("recipient", tStr $ asString ("{\"pred\":\"keys-all\",\"keys\":[\"e5db35973f544642cb8b1539cb8bdf039cfe11e5f7e1127a146bd2a6d13d28c4\"]}" :: Text))
          , ("amount", tLit $ LDecimal 20)
          , ("chainId", tLit $ LInteger 0)
          ]
        )
      ]
  ]

referenceHyperlaneMessageObject :: Object n
referenceHyperlaneMessageObject
  | Just message <- unwrapObject referenceObject ^? at "message" . _Just . _TObject . _1 = message
  | otherwise = error "Extracting HyperlaneMessage Object failed"

referenceHyperlaneMessage :: HyperlaneMessage
referenceHyperlaneMessage = fromMaybe (error "Decoding reference hyperlane message failed") $ do
  decodeHyperlaneMessageObject referenceHyperlaneMessageObject

referenceTokenMessage :: TokenMessageERC20
referenceTokenMessage = hmTokenMessage referenceHyperlaneMessage

referenceTokenMessageText :: Text
referenceTokenMessageText = Text.concat
  [ "0000000000000000000000000000000000000000000000000000000000000060" -- offset (decimal 96)
  , "000000000000000000000000000000000000000000000001158e460913d00000" -- amount
  , "0000000000000000000000000000000000000000000000000000000000000000" -- chainId
  , "000000000000000000000000000000000000000000000000000000000000005f" -- recipient length
  , "7b2270726564223a226b6579732d616c6c222c226b657973223a5b2265356462" -- recipient
  , "3335393733663534343634326362386231353339636238626466303339636665" -- |
  , "31316535663765313132376131343662643261366431336432386334225d7d00" -- V
  ]

spec :: Spec
spec = describe "hyperlane" $ do
  describe "TokenMessage Encoding/Decoding" $ do
    it "encodes to the correct bytes" $ do
      let hexMessage = Text.decodeUtf8 (Base16.encode (BL.toStrict (BB.toLazyByteString (packTokenMessageERC20 referenceTokenMessage))))
      hexMessage `shouldBe` referenceTokenMessageText

  describe "hyperlane-message-id" $ do
    it "computes the correct message id" $ do
      print $ decodeHyperlaneMessageObject referenceHyperlaneMessageObject
      hyperlaneMessageId referenceHyperlaneMessageObject `shouldBe` "0xa5c3b3c117ed9f44f306bb1dfbc3d3d960a12b1394b54f44c2bd4056d0928108"

  describe "hyperlane-decode-token-message" $ do
    it "decodes the correct token message out" $ do
      let input =
              Text.decodeUtf8
            . encodeBase64UrlUnpadded
            . fromRight (error "base16 decoding error")
            . Base16.decode
            . Text.encodeUtf8
            $ referenceTokenMessageText
      hyperlaneDecodeTokenMessage input `shouldBe` tokenMessageToTerm referenceTokenMessage

mkObject :: [(FieldKey, Term n)] -> Object n
mkObject ps = Object (ObjectMap (Map.fromList ps)) TyAny Nothing def

obj :: [(FieldKey, Term n)] -> Term n
obj = toTObject TyAny def

unwrapObject :: Object n -> Map FieldKey (Term n)
unwrapObject o = _objectMap (_oObject o)
