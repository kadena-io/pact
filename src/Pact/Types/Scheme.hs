{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies, GADTs, DataKinds #-}


module Pact.Types.Scheme
  ( PPKScheme(..)
  , defPPKScheme
  , SPPKScheme(..)
  ) where



import GHC.Generics
import Control.DeepSeq
import Data.Serialize
import Data.Aeson

import Pact.Types.Util (ParseText(..))


--------- PPKSCHEME DATA TYPE ---------

data PPKScheme = ED25519 | ETH
  deriving (Show, Eq, Ord, Generic)


instance NFData PPKScheme
instance Serialize PPKScheme
instance ToJSON PPKScheme where
  toJSON ED25519 = "ED25519"
  toJSON ETH = "ETH"
instance FromJSON PPKScheme where
  parseJSON = withText "PPKScheme" parseText
  {-# INLINE parseJSON #-}
instance ParseText PPKScheme where
  parseText s = case s of
    "ED25519" -> return ED25519
    "ETH" -> return ETH
    _ -> fail $ "Unsupported PPKScheme: " ++ show s
  {-# INLINE parseText #-}


defPPKScheme :: PPKScheme
defPPKScheme = ED25519


-- Run-time witness to PPKScheme kind.

data SPPKScheme :: PPKScheme -> * where
  SED25519 :: SPPKScheme 'ED25519
  SETH :: SPPKScheme 'ETH
