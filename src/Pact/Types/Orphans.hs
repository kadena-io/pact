{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      :  Pact.Types.Orphans
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Various orphans.
--
module Pact.Types.Orphans where

import Data.Serialize
import Data.Decimal
import qualified Data.Aeson as A
import Text.Trifecta.Delta
import Data.Text (Text)
import Data.Text.Encoding
import Pact.Time.Internal (NominalDiffTime(..), UTCTime(..))
import Data.Default
import Data.Hashable
import Data.Set (Set)
import qualified Data.Set as S
import Control.DeepSeq
import Bound
import Control.Applicative ((<|>))
import Test.QuickCheck (Arbitrary(..))


instance (Arbitrary i) => Arbitrary (DecimalRaw i) where
  arbitrary = Decimal <$> arbitrary <*> arbitrary

instance (Serialize i) => Serialize (DecimalRaw i) where
    put (Decimal p i) = put p >> put i
    get = Decimal <$> get <*> get
    {-# INLINE put #-}
    {-# INLINE get #-}

instance Serialize A.Value where
    put v = put (A.encode v)
    get = get >>= \g -> either fail return $ A.eitherDecode g
    {-# INLINE put #-}
    {-# INLINE get #-}

instance NFData Delta

instance Default Text where def = ""
instance Serialize Text where
  put = put . encodeUtf8
  get = decodeUtf8 <$> get

------ Bound/Scope/Var instances ------

instance (A.ToJSON a, A.ToJSON b) =>
  A.ToJSON (Var b a) where
  toJSON (B b) = A.object [ "b" A..= b ]
  toJSON (F a) = A.object [ "f" A..= a ]

instance (A.FromJSON a, A.FromJSON b) =>
  A.FromJSON (Var b a) where
  parseJSON = A.withObject "Var" $ \v ->
    ((B <$> v A..: "b") <|> (F <$> v A..: "f"))

instance (A.ToJSON b, Functor f, A.ToJSON (f A.Value), A.ToJSON (f a)) =>
  A.ToJSON (Scope b f a) where
  toJSON (Scope s) = A.object [ "scope" A..= (fmap A.toJSON s) ]

instance (A.FromJSON b, Traversable f, A.FromJSON (f A.Value), A.FromJSON (f a)) =>
  A.FromJSON (Scope b f a) where
  parseJSON = A.withObject "Scope" $ \o -> do
    f <- o A..: "scope"
    Scope <$> traverse A.parseJSON f

-- -------------------------------------------------------------------- --
-- Set's Hashable orphan

instance Hashable a => Hashable (Set a) where
  hashWithSalt n = hashWithSalt n . S.toList

-- -------------------------------------------------------------------------- --
-- Time Orphans

deriving newtype instance Arbitrary NominalDiffTime
deriving newtype instance Arbitrary UTCTime

