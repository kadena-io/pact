{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module HyperlaneSpec (spec) where

import Control.Lens ((^?), at, _Just, _1)
import Crypto.Hash.HyperlaneNatives (HyperlaneMessage(..), TokenMessageERC20(..), decodeHyperlaneMessageObject, hyperlaneMessageId, hyperlaneDecodeTokenMessage, packTokenMessageERC20, tokenMessageToTerm)
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Lazy qualified as BL
import Data.Default (def)
import Data.Either (fromRight)
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Pact.Types.Runtime (FieldKey, Object(..), ObjectMap(..), Term, Literal(..), tLit, tStr, asString, toTObject, Type(..), _TObject, Name)
import Pact.Types.Util (encodeBase64UrlUnpadded)
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
        hyperlaneMessage = fromMaybe (error "Decoding reference hyperlane message failed") $ do
          decodeHyperlaneMessageObject hyperlaneMessageObject

      let
        tokenMessage :: TokenMessageERC20
        tokenMessage = hmTokenMessage hyperlaneMessage

      it "TokenMessage encoding matches reference" $ do
        let hexMessage = Text.decodeUtf8 (Base16.encode (BL.toStrict (BB.toLazyByteString (packTokenMessageERC20 tokenMessage))))
        hexMessage `shouldBe` ref.tokenMessageText

      it "Computes the correct message id" $ do
        hyperlaneMessageId hyperlaneMessageObject `shouldBe` ref.messageId

      it "TokenMessage decodes properly into a Pact Term" $ do
        let input =
                Text.decodeUtf8
              . encodeBase64UrlUnpadded
              . fromRight (error "base16 decoding error")
              . Base16.decode
              . Text.encodeUtf8
              $ ref.tokenMessageText
        hyperlaneDecodeTokenMessage input `shouldBe` tokenMessageToTerm tokenMessage

spec :: Spec
spec = testRefs
  [ Reference
      { object = mkObject
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
      , tokenMessageText = Text.concat
          [ "0000000000000000000000000000000000000000000000000000000000000060" -- offset (decimal 96)
          , "000000000000000000000000000000000000000000000001158e460913d00000" -- amount
          , "0000000000000000000000000000000000000000000000000000000000000000" -- chainId
          , "000000000000000000000000000000000000000000000000000000000000005f" -- recipient length
          , "7b2270726564223a226b6579732d616c6c222c226b657973223a5b2265356462" -- recipient
          , "3335393733663534343634326362386231353339636238626466303339636665" -- |
          , "31316535663765313132376131343662643261366431336432386334225d7d00" -- V
          ]
      , messageId = "0xa5c3b3c117ed9f44f306bb1dfbc3d3d960a12b1394b54f44c2bd4056d0928108"
      }
  , Reference
      { object = mkObject
          [ ("message",) $ obj
              [ ("version", tLit $ LInteger 3)
              , ("nonce", tLit $ LInteger 0)
              , ("originDomain", tLit $ LInteger 31_337)
              , ("sender", tStr $ asString ("0x0000000000000000000000006171479a003d1d89915dd9e71657620313870283" :: Text))
              , ("destinationDomain", tLit $ LInteger 626)
              , ("recipient", tStr $ asString ("0x676a5f45557a44534f6e54497a4d72676c6e725f77584b56494454467a773465" :: Text))
              , ("tokenMessage", obj
                  --[ ("recipient", tStr $ asString ("{\"keys\":[\"94c35ab1bd70243ec670495077f7846373b4dc5e9779d7a6732b5ceb6fde059c\"],\"pred\":\"keys-all\"}" :: Text))
                  [ ("recipient", tStr $ asString ("{\"pred\":\"keys-all\",\"keys\":[\"94c35ab1bd70243ec670495077f7846373b4dc5e9779d7a6732b5ceb6fde059c\"]}" :: Text))
                  , ("amount", tLit $ LDecimal 0.5)
                  , ("chainId", tLit $ LInteger 0)
                  ]
                )
              ]
          ]
      , tokenMessageText = Text.concat
          [ "0000000000000000000000000000000000000000000000000000000000000060" -- offset (decimal 96)
          , "00000000000000000000000000000000000000000000000006f05b59d3b20000" -- amount
          , "0000000000000000000000000000000000000000000000000000000000000000" -- chainId
          , "000000000000000000000000000000000000000000000000000000000000005f" -- recipient length
          , "7b2270726564223a226b6579732d616c6c222c226b657973223a5b2239346333" -- recipient
          , "3561623162643730323433656336373034393530373766373834363337336234" -- |
          , "64633565393737396437613637333262356365623666646530353963225d7d00" -- V
          ]
      , messageId = "0x984831166082c9530bb0cc7293e9f99c9e6eb31729be11f20ca9cb72565e4aff"
      }
  ]

mkObject :: [(FieldKey, Term n)] -> Object n
mkObject ps = Object (ObjectMap (Map.fromList ps)) TyAny Nothing def

obj :: [(FieldKey, Term n)] -> Term n
obj = toTObject TyAny def

unwrapObject :: Object n -> Map FieldKey (Term n)
unwrapObject o = _objectMap (_oObject o)

{-
  "version": 3,
  "nonce": 0,
  "originDomain": 31337,
  "sender": "0x0000000000000000000000006171479a003d1d89915dd9e71657620313870283",
  "destinationDomain": 626,
  "recipient": "0x676a5f45557a44534f6e54497a4d72676c6e725f77584b56494454467a773465",
  "tokenMessage": {
    "amount": 0.5,
    "chainId": 0,
    "recipient": "{\"keys\":[\"94c35ab1bd70243ec670495077f7846373b4dc5e9779d7a6732b5ceb6fde059c\"],\"pred\":\"keys-all\"}"
  }
-}

{-
expected:

0000000000000000000000000000000000000000000000000000000000000060 -- | tag (96)
00000000000000000000000000000000000000000000000006f05b59d3b20000 -- | amount
0000000000000000000000000000000000000000000000000000000000000000 -- | chainId
000000000000000000000000000000000000000000000000000000000000005f -- | recipient length
7b2270726564223a226b6579732d616c6c222c226b657973223a5b2239346333 -- | recipient
3561623162643730323433656336373034393530373766373834363337336234    |
64633565393737396437613637333262356365623666646530353963225d7d00    |

but got:

0000000000000000000000000000000000000000000000000000000000000060 -- | tag (96)
00000000000000000000000000000000000000000000000006f05b59d3b20000 -- | amount
0000000000000000000000000000000000000000000000000000000000000000 -- | chainId
000000000000000000000000000000000000000000000000000000000000005f -- | recipient length
7b226b657973223a5b2239346333356162316264373032343365633637303439 -- | recipient
3530373766373834363337336234646335653937373964376136373332623563    |
65623666646530353963225d2c2270726564223a226b6579732d616c6c227d00    |

-}

