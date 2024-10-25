{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

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
   noParsed,
   Code(..),
   Info(..),
   mkInfo,
   renderInfo,
   renderParsed,
   HasInfo(..),
   noInfo
   ) where


import qualified Data.ByteString as B
import Control.Monad
import Text.Trifecta.Delta
import Data.List
import Prelude
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.Text as T
import Data.Aeson
import qualified Data.Aeson.Types as A
import Data.String
import Data.Default
import GHC.Generics (Generic)
import Control.DeepSeq
#if defined(BUILD_TOOL)
import Data.SBV (Mergeable (symbolicMerge))
#endif
import qualified Data.Vector as V
import Test.QuickCheck

import Pact.Types.Orphans ()
import Pact.Types.Pretty
import Pact.Types.SizeOf
import Pact.Types.Util

import qualified Pact.JSON.Encode as J

--import Pact.Types.Crypto (Hash(..))

-- | Code location, length from parsing.
data Parsed = Parsed {
  _pDelta :: !Delta,
  _pLength :: !Int
  } deriving (Eq,Show,Ord,Generic)

instance Arbitrary Parsed where
  -- Renderer and parser ignore length, so we set it to 0
  arbitrary = Parsed <$> genDelta <*> pure 0
    where
      genPositiveInt64 = getPositive <$> arbitrary
      genFilename = encodeUtf8 <$> genBareText
      genDelta = oneof
        -- The parser assumes that the second number of columns is always 0
        [ Columns <$> genPositiveInt64 <*> pure 0

        -- Tab is not supported by parser
        -- The parser always assumes that the last to numbers are zero
        , Lines <$> genPositiveInt64 <*> genPositiveInt64 <*> pure 0 <*> pure 0

        -- The parser always assumes that the last to numbers are zero
        , Directed <$> genFilename <*> genPositiveInt64 <*> genPositiveInt64 <*> pure 0 <*> pure 0 ]
instance NFData Parsed
instance Default Parsed where def = noParsed
instance Pretty Parsed where pretty = pretty . _pDelta

noParsed :: Parsed
noParsed = Parsed mempty 0

newtype Code = Code { _unCode :: Text }
  deriving (Eq,Ord,Generic)
  deriving newtype (IsString,FromJSON,Semigroup,Monoid,NFData,AsString,SizeOf,J.Encode)

instance Show Code where show = T.unpack . _unCode
instance Pretty Code where
  pretty (Code c)
    | T.compareLength c maxLen == GT
    = pretty $ T.take maxLen c <> "..."
    | otherwise = pretty c
    where maxLen = 30

instance Arbitrary Code where
  arbitrary = Code <$> scale (min 1000) genBareText

-- | For parsed items, original code and parse info;
-- for runtime items, nothing
newtype Info = Info { _iInfo :: Maybe (Code,Parsed) }
  deriving (Eq,Ord,Generic,Arbitrary)

instance NFData Info
instance Show Info where
    show (Info Nothing) = ""
    show (Info (Just (r,_d))) = renderCompactString r

instance Default Info where def = noInfo

noInfo :: Info
noInfo = Info Nothing

-- | Charge zero for Info to avoid quadratic blowup (i.e. for modules)
instance SizeOf Info where
  sizeOf _ _ = 0

-- | Make an Info that refers to the indicated text.
--
-- Note that the pact parser returns column offsets in bytes.
--
-- This method is currently only used in testing and benchmarking in chainweb.
--
mkInfo :: Text -> Info
mkInfo t = Info $ Just (Code t,Parsed delt len)
  where len = B.length $ encodeUtf8 t
        delt = Directed (encodeUtf8 t) 0 0 (fromIntegral len) (fromIntegral len)

#if defined(BUILD_TOOL)
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

instance J.Encode Info where
  build (Info Nothing) = J.null
  build (Info (Just (code,Parsed{..}))) = J.object
    [ case _pDelta of
      (Directed a b c d e) -> "d" J..= J.Array (i pl, decodeUtf8 a, i b, i c, i d, i e)
      (Lines a b c d) -> "d" J..= J.Array (i pl, i a, i b, i c, i d)
      (Columns a b) -> "d" J..= J.Array (i pl, i a, i b)
      (Tab a b c) -> "d" J..= J.Array (i pl, i a, i b, i c)
    , "c" J..= code
    ]
   where
    pl = _pLength
    i :: forall n . n -> J.Aeson n
    i = J.Aeson

-- | This instance is not used everywhere. For instance, the FromJSON instance
-- of PactError uses a custom 'Info' parser that uses 'parseRenderedInfo'.
--
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
        where col :: FromJSON v => Int -> A.Parser v
              col i = parseJSON (d V.! i)
              parsed p = Parsed <$> p <*> col 0
