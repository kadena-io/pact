{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module HyperlaneSpec (spec) where

import Control.Lens ((^?), at, _Just, _1)
import Crypto.Hash.HyperlaneMessageId (hyperlaneMessageId)
import Data.Default (def)
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Pact.Types.Runtime (FieldKey, Object(..), ObjectMap(..), Term, Literal(..), tLit, tStr, asString, toTObject, Type(..), _TObject)
import Test.Hspec

spec :: Spec
spec = describe "hyperlane" $ do
  describe "hyperlane-message-id" $ do
    it "computes the correct message id" $ do
      let obj' = mkObject
            [ ("message",) $ obj
                [ ("version", tLit $ LInteger 1)
                , ("nonce", tLit $ LInteger 325)
                , ("originDomain", tLit $ LInteger 626)
                , ("sender", tStr $ asString ("0x6b622d746f6b656e2d726f75746572" :: Text))
                , ("destinationDomain", tLit $ LInteger 1)
                , ("recipient", tStr $ asString ("0x71C7656EC7ab88b098defB751B7401B5f6d8976F" :: Text))
                , ("tokenMessage", obj
                    [ ("recipient", tStr $ asString ("0x71C7656EC7ab88b098defB751B7401B5f6d8976F" :: Text))
                    , ("amount", tLit $ LDecimal 10000000000000000000)
                    ]
                  )
                ]
            ]
      Just message <- pure (unwrapObject obj' ^? at "message" . _Just . _TObject . _1)
      hyperlaneMessageId message `shouldBe` "0x97d98aa7fdb548f43c9be37aaea33fca79680247eb8396148f1df10e6e0adfb7"

mkObject :: [(FieldKey, Term n)] -> Object n
mkObject ps = Object (ObjectMap (Map.fromList ps)) TyAny Nothing def

obj :: [(FieldKey, Term n)] -> Term n
obj = toTObject TyAny def

unwrapObject :: Object n -> Map FieldKey (Term n)
unwrapObject o = _objectMap (_oObject o)
