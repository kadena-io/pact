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
  , verifierProof
  , verifierCaps
  , ParsedVerifierProof(..)
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

data Verifier prf = Verifier
  { _verifierName :: VerifierName
  , _verifierProof :: prf
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
    , "proof" J..= _verifierProof va
    , "clist" J..= J.Array (_verifierCaps va)
    ]
instance FromJSON a => FromJSON (Verifier a) where
  parseJSON = withObject "Verifier" $ \o -> do
    name <- o .: "name"
    proof <- o .: "proof"
    caps <- o .: "clist"
    return $ Verifier name proof caps

newtype ParsedVerifierProof = ParsedVerifierProof PactValue
  deriving newtype (NFData, Eq, Show, Ord, FromJSON)
  deriving stock Generic

instance J.Encode ParsedVerifierProof where
  build (ParsedVerifierProof as) = J.build as

instance Arbitrary ParsedVerifierProof where
  arbitrary = ParsedVerifierProof <$> arbitrary
