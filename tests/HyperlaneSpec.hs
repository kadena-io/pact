{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module HyperlaneSpec (spec) where

import Control.Lens ((^?), at, _Just, _1)
import Crypto.Hash.HyperlaneNatives (hyperlaneMessageId)
import Data.Default (def)
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Pact.Types.Runtime (FieldKey, Object(..), ObjectMap(..), Term, Literal(..), tLit, tStr, asString, toTObject, Type(..), _TObject, Name)
import Pact.Types.Util (encodeBase64UrlUnpadded)
import Test.Hspec

data Reference = Reference
  { object :: Object Name
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

      it "Computes the correct message id" $ do
        hyperlaneMessageId hyperlaneMessageObject `shouldBe` Right ref.messageId

spec :: Spec
spec = testRefs
  [ let
    tokenMessageText = Text.decodeUtf8 $ encodeBase64UrlUnpadded $ mconcat
          [ "0000000000000000000000000000000000000000000000000000000000000060" -- offset (decimal 96)
          , "000000000000000000000000000000000000000000000001158e460913d00000" -- amount
          , "0000000000000000000000000000000000000000000000000000000000000000" -- chainId
          , "000000000000000000000000000000000000000000000000000000000000005f" -- recipient length
          , "7b2270726564223a226b6579732d616c6c222c226b657973223a5b2265356462" -- recipient
          , "3335393733663534343634326362386231353339636238626466303339636665" -- |
          , "31316535663765313132376131343662643261366431336432386334225d7d00" -- V
          ]
   in Reference
      { object = mkObject
          [ ("message",) $ obj
              [ ("version", tLit $ LInteger 3)
              , ("nonce", tLit $ LInteger 0)
              , ("originDomain", tLit $ LInteger 31_337)
              , ("sender", tStr $ asString ("AAAAAAAAAAAAAAAAf6k4W-ECrD6sKXSD3WIz1is-FJY" :: Text))
              , ("destinationDomain", tLit $ LInteger 626)
              , ("recipient", tStr $ asString ("AAAAAAAAAADpgrOqkM0BOY-FQnNzkDXuYlsVcf50GRU" :: Text))
              , ("messageBody", tStr tokenMessageText)
              ]
          ]
      , messageId = "0x8a9ff9b92e972a0fe9b66806e4564d7c3879a97ac4e3eb4c8db34d3b85d8e4ad"
      }
  , let
    tokenMessageText = Text.decodeUtf8 $ encodeBase64UrlUnpadded $ mconcat
        [ "0000000000000000000000000000000000000000000000000000000000000060" -- offset (decimal 96)
        , "00000000000000000000000000000000000000000000000006f05b59d3b20000" -- amount
        , "0000000000000000000000000000000000000000000000000000000000000000" -- chainId
        , "000000000000000000000000000000000000000000000000000000000000005f" -- recipient length
        , "7b2270726564223a226b6579732d616c6c222c226b657973223a5b2239346333" -- recipient
        , "3561623162643730323433656336373034393530373766373834363337336234" -- |
        , "64633565393737396437613637333262356365623666646530353963225d7d00" -- V
        ]
  in Reference
      { object = mkObject
          [ ("message",) $ obj
              [ ("version", tLit $ LInteger 3)
              , ("nonce", tLit $ LInteger 0)
              , ("originDomain", tLit $ LInteger 31_337)
              , ("sender", tStr $ asString ("AAAAAAAAAAAAAAAAf6k4W-ECrD6sKXSD3WIz1is-FJY" :: Text))
              , ("destinationDomain", tLit $ LInteger 626)
              , ("recipient", tStr $ asString ("AAAAAAAAAADpgrOqkM0BOY-FQnNzkDXuYlsVcf50GRU" :: Text))
              , ("messageBody", tStr tokenMessageText)
              ]
          ]
      , messageId = "0xbf63aed32b96d7bcdbc73a29c4e8d08f9b8bdb1cdaefc600a94b0404b6a5dfa3"
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

