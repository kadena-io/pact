{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

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
import Control.DeepSeq
import Bound
import Control.Applicative ((<|>))
import Test.QuickCheck (Arbitrary(..))

import qualified Pact.JSON.Encode as J

instance (Arbitrary i) => Arbitrary (DecimalRaw i) where
  arbitrary = Decimal <$> arbitrary <*> arbitrary

instance (Serialize i) => Serialize (DecimalRaw i) where
    put (Decimal p i) = put p >> put i
    get = Decimal <$> get <*> get
    {-# INLINE put #-}
    {-# INLINE get #-}

instance NFData Delta

instance Default Text where def = ""
instance Serialize Text where
  put = put . encodeUtf8
  get = decodeUtf8 <$> get

------ Bound/Scope/Var instances ------

instance (J.Encode a, J.Encode (J.Aeson b)) => J.Encode (Var b a) where
  build (B b) = J.object ["b" J..= J.Aeson b]
  build (F a) = J.object ["f" J..= a]
  {-# INLINE build #-}

instance (A.FromJSON a, A.FromJSON b) =>
  A.FromJSON (Var b a) where
  parseJSON = A.withObject "Var" $ \v ->
    (B <$> v A..: "b") <|> (F <$> v A..: "f")
  {-# INLINE parseJSON #-}

instance (J.Encode (f (Var b (f a)))) => J.Encode (Scope b f a) where
  build (Scope s) = J.object ["scope" J..= s]
  {-# INLINE build #-}

instance (A.FromJSON b, Traversable f, A.FromJSON (f A.Value), A.FromJSON (f a)) =>
  A.FromJSON (Scope b f a) where
  parseJSON = A.withObject "Scope" $ \o -> do
    f <- o A..: "scope"
    Scope <$> traverse A.parseJSON f
  {-# INLINE parseJSON #-}

-- -------------------------------------------------------------------------- --
-- Time Orphans

deriving newtype instance Arbitrary NominalDiffTime
deriving newtype instance Arbitrary UTCTime

