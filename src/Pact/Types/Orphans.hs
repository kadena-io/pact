{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE LambdaCase #-}
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
import Data.Thyme
import Data.Thyme.Internal.Micro
import Data.Decimal
import qualified Data.Aeson as A
import Text.Trifecta.Combinators (DeltaParsing(..))
import Text.Trifecta.Delta
import qualified Data.Attoparsec.Text as AP
import qualified Data.Attoparsec.Internal.Types as APT
import Data.Text (Text)
import Data.Text.Encoding
import Data.Default
import Control.DeepSeq
import Bound
import Control.Applicative ((<|>))

instance Serialize Micro
instance Serialize NominalDiffTime
instance Serialize UTCTime

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


-- | Atto DeltaParsing instance provides 'position' only (with no support for
-- hidden chars like Trifecta).
instance DeltaParsing AP.Parser where
    line = return mempty
    position = attoPos >>= \(APT.Pos p) -> let p' = fromIntegral p in return $ Columns p' p'  -- p p
    slicedWith f a = (`f` mempty) <$> a
    rend = return mempty
    restOfLine = return mempty

-- | retrieve pos from Attoparsec.
attoPos :: APT.Parser n APT.Pos
attoPos = APT.Parser $ \t pos more _lose win -> win t pos more pos

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
