{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}

module Pact.Types.Verifier
  ( VerifierName(..)
  , Verifier(..)
  , verifierName
  , verifierArgs
  , verifierCaps
  , ParsedVerifierArgs(..)
  ) where

import Control.DeepSeq
import Control.Lens
import Data.Aeson
import Data.Text
import GHC.Generics
import Test.QuickCheck(Arbitrary(..), scale)

import qualified Pact.JSON.Encode as J

import Pact.Types.Orphans()
import Pact.Types.PactValue
import Pact.Types.Capability

newtype VerifierName = VerifierName Text
  deriving newtype (J.Encode, Arbitrary, NFData, Eq, Show, Ord, FromJSON)
  deriving stock Generic

data Verifier args = Verifier
  { _verifierName :: VerifierName
  , _verifierArgs :: args
  , _verifierCaps :: [UserCapability]
  }
  deriving (Eq, Show, Generic, Ord, Functor, Foldable, Traversable)

makeLenses ''Verifier

instance NFData a => NFData (Verifier a)
instance Arbitrary a => Arbitrary (Verifier a) where
    arbitrary =
      Verifier <$>
        (VerifierName . pack <$> arbitrary) <*>
        arbitrary <*>
        scale (min 10) arbitrary
instance J.Encode a => J.Encode (Verifier a) where
  build va = J.object
    [ "name" J..= _verifierName va
    , "args" J..= _verifierArgs va
    , "caps" J..= J.Array (_verifierCaps va)
    ]
instance FromJSON a => FromJSON (Verifier a) where
  parseJSON = withObject "Verifier" $ \o -> do
    name <- o .: "name"
    args <- o .: "args"
    caps <- o .: "caps"
    return $ Verifier name args caps

newtype ParsedVerifierArgs = ParsedVerifierArgs [PactValue]
  deriving newtype (NFData, Eq, Show, Ord, FromJSON)
  deriving stock Generic

instance J.Encode ParsedVerifierArgs where
  build (ParsedVerifierArgs as) = J.build (J.Array as)

instance Arbitrary ParsedVerifierArgs where
  arbitrary = ParsedVerifierArgs <$> scale (min 10) arbitrary
