{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
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
import qualified Data.Attoparsec.Text as AP

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



instance DeltaParsing AP.Parser where
    line = return mempty
    position = return mempty
    slicedWith f a = (`f` mempty) <$> a
    rend = return mempty
    restOfLine = return mempty
