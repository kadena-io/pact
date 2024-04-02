{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module HyperlaneSpec (spec) where

import Control.Lens ((^?), at, _Just, _1)
import Crypto.Hash.HyperlaneNatives (HyperlaneMessage(..), TokenMessageERC20(..), hyperlaneMessageId, decodeHyperlaneMessageObject, packTokenMessageERC20, unpackTokenMessageERC20)
import Data.Binary.Get qualified as Bin
import Data.Default (def)
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Lazy qualified as BL
import Data.Either (fromRight)
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Pact.Types.Runtime (FieldKey, Object(..), ObjectMap(..), Term, Literal(..), tLit, tStr, asString, toTObject, Type(..), _TObject, Name)
import Pact.Types.Util (decodeBase64UrlUnpadded, encodeBase64UrlUnpadded)
import Test.Hspec

data Reference = Reference
  { object :: Object Name
  , tokenMessageText :: Text
  , messageId :: Text
  }

testRefs :: [Reference] -> Spec
testRefs refs = describe "hyperlane" $ mapM_ (uncurry testRef) (zip [0..] refs)
  where
    testRef :: Word -> Reference -> Spec
    testRef refId ref = describe ("reference " <> show refId) $ do

      let
        hyperlaneMessageObject :: Object Name
        hyperlaneMessageObject
          | Just message <- unwrapObject ref.object ^? at "message" . _Just . _TObject . _1 = message
          | otherwise = error "Extracting HyperlaneMessage Object failed"

      let
        hyperlaneMessage :: HyperlaneMessage
        hyperlaneMessage = fromRight (error "Decoding reference hyperlane message failed") $ do
          decodeHyperlaneMessageObject hyperlaneMessageObject

      let
        tokenMessage :: TokenMessageERC20
        tokenMessage = Bin.runGet unpackTokenMessageERC20 (BL.fromStrict (hmMessageBody hyperlaneMessage))

      it "Computes the correct message id" $ do
        hyperlaneMessageId hyperlaneMessageObject `shouldBe` Right ref.messageId

      it "TokenMessage encoding matches reference" $ do
        let hexMessage = Text.decodeUtf8 (encodeBase64UrlUnpadded (BL.toStrict (BB.toLazyByteString (packTokenMessageERC20 tokenMessage))))
        hexMessage `shouldBe` ref.tokenMessageText

      -- TODO: This only applies on ETH -> KDA (when TokenMessage recipient is a guard)
      --it "TokenMessage decodes properly into a Pact Term" $ do
      --  hyperlaneDecodeTokenMessage ref.tokenMessageText `shouldBe` tokenMessageToTerm tokenMessage

-- Recipient info
-- ETH -> KDA = Guard
-- KDA -> ETH = ETH Address

spec :: Spec
spec = testRefs
  [ let
      tokenMsgText = Text.decodeUtf8 $ encodeBase64UrlUnpadded $ BL.toStrict $ BB.toLazyByteString $
        packTokenMessageERC20 $ TokenMessageERC20
          { tmAmount = 10
          , tmChainId = 0
          , tmRecipient = Text.encodeUtf8 "{\"pred\":\"keys-all\",\"keys\":[\"e5db35973f544642cb8b1539cb8bdf039cfe11e5f7e1127a146bd2a6d13d28c4\"]}"
          }
   in
   Reference
     { object = mkObject
          [ ("message",) $ obj
              [ ("version", tLit $ LInteger 3)
              , ("nonce", tLit $ LInteger 0)
              , ("originDomain", tLit $ LInteger 31_337)
              , ("sender", tStr $ asString ("AAAAAAAAAAAAAAAAf6k4W-ECrD6sKXSD3WIz1is-FJY" :: Text))
              , ("destinationDomain", tLit $ LInteger 626)
              , ("recipient", tStr $ asString ("AAAAAAAAAADpgrOqkM0BOY-FQnNzkDXuYlsVcf50GRU" :: Text))
              , ("messageBody", tStr tokenMsgText)
              ]
          ]
      , messageId = "E9W6As6Nqv0tW66uxgQjzKekSS23utRfWzwsIN7HEqw"
      , tokenMessageText = tokenMsgText
      }
  , let
      tokenMsgText = Text.decodeUtf8 $ encodeBase64UrlUnpadded $ BL.toStrict $ BB.toLazyByteString $
        packTokenMessageERC20 $ TokenMessageERC20
          { tmAmount = 10
          , tmChainId = 0
          , tmRecipient = fromRight (error "failed to decode TokenMessage recipient") $ decodeBase64UrlUnpadded "cSOeAK6UKzlLOpGrIp5SZK2Db28"
          }
    in
    Reference
      { object = mkObject
          [ ("message",) $ obj
              [ ("version", tLit $ LInteger 3)
              , ("nonce", tLit $ LInteger 0)
              , ("originDomain", tLit $ LInteger 31_337)
              , ("sender", tStr $ asString ("AAAAAAAAAAAAAAAAf6k4W-ECrD6sKXSD3WIz1is-FJY" :: Text))
              , ("destinationDomain", tLit $ LInteger 626)
              , ("recipient", tStr $ asString ("AAAAAAAAAADpgrOqkM0BOY-FQnNzkDXuYlsVcf50GRU" :: Text))
              , ("messageBody", tStr tokenMsgText)
              ]
          ]
      , messageId = "kJGRc6EIp8d5c2jWYE87_X64P-MQGP8zrCLJ0GyjNE4"
      , tokenMessageText = tokenMsgText
      }
  ]

mkObject :: [(FieldKey, Term n)] -> Object n
mkObject ps = Object (ObjectMap (Map.fromList ps)) TyAny Nothing def

obj :: [(FieldKey, Term n)] -> Term n
obj = toTObject TyAny def

unwrapObject :: Object n -> Map FieldKey (Term n)
unwrapObject o = _objectMap (_oObject o)
