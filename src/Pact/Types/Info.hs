{-# LANGUAGE RecordWildCards #-}
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
   mkInfo,
   renderInfo,
   renderParsed,
   HasInfo(..)
   ) where


import Text.Trifecta.Delta
import Data.List
import Prelude
import Data.Text (Text,unpack)
import Data.Text.Encoding
import qualified Data.Text as T
import Data.Aeson
import Data.Aeson.Types
import Data.String
import Data.Default
import GHC.Generics (Generic)
import Control.DeepSeq
#if !defined(ghcjs_HOST_OS)
import Data.SBV (Mergeable (symbolicMerge))
#endif
import qualified Data.Vector as V

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
newtype Info = Info { _iInfo :: Maybe (Code,Parsed) } deriving (Eq,Ord,Generic)

instance NFData Info
instance Show Info where
    show (Info Nothing) = ""
    show (Info (Just (r,_d))) = renderCompactString r

instance Default Info where def = Info Nothing

-- make an Info that refers to the indicated text
mkInfo :: Text -> Info
mkInfo t = Info $ Just (Code t,Parsed delt len)
  where len = T.length t
        delt = Directed (encodeUtf8 t) 0 0 (fromIntegral len) (fromIntegral len)

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
  (Columns c _) -> "<interactive>:0:" ++ show c
  (Tab _ c _ ) -> "<interactive>:0:" ++ show c


class HasInfo a where
  getInfo :: a -> Info

instance HasInfo Info where getInfo = id

instance ToJSON Info where
  toJSON (Info Nothing) = Null
  toJSON (Info (Just (code,Parsed{..}))) = object
    [ "c" .= code
    , "d" .= case _pDelta of
        (Directed a b c d e) -> [pl,toJSON (decodeUtf8 a),toJSON b,toJSON c,toJSON d,toJSON e]
        (Lines a b c d) -> [pl,toJSON a,toJSON b,toJSON c,toJSON d]
        (Columns a b) -> [pl,toJSON a,toJSON b]
        (Tab a b c) -> [pl,toJSON a,toJSON b,toJSON c]

    ] where pl = toJSON _pLength

instance FromJSON Info where
  parseJSON Null = pure $ Info Nothing
  parseJSON v = withObject "Info" go v
    where
      go o = Info . Just <$>
        ((,) <$> o .: "c" <*> (o .: "d" >>= withArray "Delta" doParsed))
      doParsed d = parsed $ case V.length d of
        6 -> Directed <$> (encodeUtf8 <$> col 1) <*> col 2 <*> col 3 <*> col 4 <*> col 5
        5 -> Lines <$> col 1 <*> col 2 <*> col 3 <*> col 4
        4 -> Tab <$> col 1 <*> col 2 <*> col 3
        3 -> Columns <$> col 1 <*> col 2
        _ -> fail "Delta: invalid JSON"
        where col :: FromJSON v => Int -> Parser v
              col i = parseJSON (d V.! i)
              parsed p = Parsed <$> p <*> col 0
