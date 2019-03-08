{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      :  Pact.Types.Info
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Code-related metadata.
--

module Pact.Types.Info
 (
   Parsed(..),
   Code(..),
   Info(..),
   renderInfo,
   renderParsed,
   HasInfo(..)
   ) where


import Text.Trifecta.Delta hiding (Columns)
import Data.List
import Prelude
import Data.Text (Text,unpack)
import qualified Data.Text as T
import Data.Aeson
import Data.String
import Data.Default
import GHC.Generics (Generic)
import Control.DeepSeq
#if !defined(ghcjs_HOST_OS)
import Data.SBV (Mergeable (symbolicMerge))
#endif

import Pact.Types.Orphans ()
import Pact.Types.Pretty
import Pact.Types.Util

--import Pact.Types.Crypto (Hash(..))

-- | Code location, length from parsing.
data Parsed = Parsed {
  _pDelta :: Delta,
  _pLength :: Int
  } deriving (Eq,Show,Ord,Generic)

instance NFData Parsed
instance Default Parsed where def = Parsed mempty 0
instance HasBytes Parsed where bytes = bytes . _pDelta
instance Pretty Parsed where pretty = pretty . _pDelta


newtype Code = Code { _unCode :: Text }
  deriving (Eq,Ord,IsString,ToJSON,FromJSON,Semigroup,Monoid,Generic,NFData,AsString)
instance Show Code where show = unpack . _unCode
instance Pretty Code where
  pretty (Code c)
    | T.compareLength c maxLen == GT
    = pretty $ T.take maxLen c <> "..."
    | otherwise = pretty c
    where maxLen = 30

-- | For parsed items, original code and parse info;
-- for runtime items, nothing
newtype Info = Info { _iInfo :: Maybe (Code,Parsed) } deriving (Generic)

instance NFData Info
-- show instance uses Trifecta renderings
instance Show Info where
    show (Info Nothing) = ""
    show (Info (Just (r,_d))) = renderCompactString r
instance Eq Info where
    Info Nothing == Info Nothing = True
    Info (Just (_,d)) == Info (Just (_,e)) = d == e
    _ == _ = False
instance Ord Info where
  Info Nothing <= Info Nothing = True
  Info (Just (_,d)) <= Info (Just (_,e)) = d <= e
  Info Nothing <= _ = True
  _ <= Info Nothing = False

instance Default Info where def = Info Nothing

#if !defined(ghcjs_HOST_OS)
instance Mergeable Info where
  -- Because Info values have no effect on execution we just take the max
  -- (which could possibly have more info)
  symbolicMerge _ _ a b = max a b
#endif


-- renderer for line number output.
renderInfo :: Info -> String
renderInfo (Info Nothing) = ""
renderInfo (Info (Just (_, parsed))) = renderParsed parsed

renderParsed :: Parsed -> String
renderParsed (Parsed d _) = case d of
  (Directed f l c _ _) -> asString' f ++ ":" ++ show (succ l) ++ ":" ++ show c
  (Lines l c _ _) -> "<interactive>:" ++ show (succ l) ++ ":" ++ show c
  _ -> "<interactive>:0:0"


class HasInfo a where
  getInfo :: a -> Info

instance HasInfo Info where getInfo = id
