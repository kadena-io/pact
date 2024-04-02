{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module HyperlaneSpec (spec) where

import Data.Maybe (fromMaybe)
import Control.Lens ((^?), at, _Just, _1)
import Crypto.Hash.HyperlaneNatives (HyperlaneMessage(..), TokenMessageERC20(..), decodeHyperlaneMessageObject, hyperlaneMessageId, hyperlaneDecodeTokenMessage, packTokenMessageERC20, tokenMessageToTerm)
import Data.Default (def)
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Pact.Types.Runtime (FieldKey, Object(..), ObjectMap(..), Term, Literal(..), tLit, tStr, asString, toTObject, Type(..), _TObject)
import Test.Hspec
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Lazy qualified as BL
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
          , ("amount", tLit $ LDecimal 20_000_000_000_000_000_000)
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

spec :: Spec
spec = describe "hyperlane" $ do
  describe "hyperlane-message-id" $ do
    it "computes the correct message id" $ do
      hyperlaneMessageId referenceHyperlaneMessageObject `shouldBe` "0xa5c3b3c117ed9f44f306bb1dfbc3d3d960a12b1394b54f44c2bd4056d0928108"

  describe "hyperlane-decode-token-message" $ do
    it "decodes the correct token message out" $ do
      hyperlaneDecodeTokenMessage (tokenMessageToInput referenceTokenMessage) `shouldBe` tokenMessageToTerm referenceTokenMessage

mkObject :: [(FieldKey, Term n)] -> Object n
mkObject ps = Object (ObjectMap (Map.fromList ps)) TyAny Nothing def

obj :: [(FieldKey, Term n)] -> Term n
obj = toTObject TyAny def

unwrapObject :: Object n -> Map FieldKey (Term n)
unwrapObject o = _objectMap (_oObject o)

tokenMessageToInput :: TokenMessageERC20 -> Text
tokenMessageToInput =
  Text.decodeUtf8
  . encodeBase64UrlUnpadded
  . BL.toStrict
  . BB.toLazyByteString
  . packTokenMessageERC20
