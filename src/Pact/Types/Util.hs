{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}


-- |
-- Module      :  Pact.Types.Util
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Utility types and functions.
--
module Pact.Types.Util where

import Data.Aeson
import Data.Aeson.Types
import GHC.Generics
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.Char
import Data.Text (Text)
import Data.Text.Encoding
import Text.PrettyPrint.ANSI.Leijen hiding ((<>),(<$>))
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import Control.Concurrent
import Control.Lens

lensyOptions :: Int -> Options
lensyOptions n = defaultOptions { fieldLabelModifier = lensyConstructorToNiceJson n }

lensyToJSON :: (Generic a, GToJSON (Rep a)) => Int -> a -> Value
lensyToJSON n = genericToJSON (lensyOptions n)

lensyParseJSON
  :: (Generic a, GFromJSON (Rep a)) => Int -> Value -> Parser a
lensyParseJSON n = genericParseJSON (lensyOptions n)

lensyConstructorToNiceJson :: Int -> String -> String
lensyConstructorToNiceJson n fieldName = firstToLower $ drop n fieldName
  where
    firstToLower (c:cs) = toLower c : cs
    firstToLower _ = error "You've managed to screw up the drop number or the field name"

parseB16JSON :: Value -> Parser ByteString
parseB16JSON = withText "Base16" parseB16Text

parseB16Text :: Text -> Parser ByteString
parseB16Text t = case B16.decode (encodeUtf8 t) of
                 (s,leftovers) | leftovers == B.empty -> return s
                               | otherwise -> fail $ "Base16 decode failed: " ++ show t

toB16JSON :: ByteString -> Value
toB16JSON s = String $ toB16Text s

toB16Text :: ByteString -> Text
toB16Text s = decodeUtf8 $ B16.encode s

failMaybe :: Monad m => String -> Maybe a -> m a
failMaybe err m = maybe (fail err) return m



-- | Utility for unsafe parse of JSON
unsafeFromJSON :: FromJSON a => Value -> a
unsafeFromJSON v = case fromJSON v of Success a -> a; Error e -> error ("JSON parse failed: " ++ show e)

-- | Utility for GHCI output of JSON
outputJSON :: ToJSON a => a -> IO ()
outputJSON a = BSL8.putStrLn $ encode a


data RenderColor = RColor | RPlain

renderString :: Pretty a => (Doc -> SimpleDoc) -> RenderColor -> a -> String
renderString renderf colors p = displayS (renderf ((case colors of RColor -> id; RPlain -> plain) (pretty p))) ""

renderCompactString :: Pretty a => a -> String
renderCompactString = renderString renderCompact RPlain

renderPrettyString :: Pretty a => RenderColor -> a -> String
renderPrettyString c a = renderString (renderPretty 0.4 100) c a


-- | Provide unquoted string representation.
class AsString a where asString :: a -> String
instance AsString String where asString = id



-- | Pure version of 'modifyMVar_'
modifyMVar' :: MVar a -> (a -> a) -> IO ()
modifyMVar' mv f = modifyMVar_ mv (\ps -> return (f ps))
{-# INLINE modifyMVar' #-}

-- | Modify the target of a lens.
modifyingMVar :: MVar s -> Lens' s a -> (a -> IO a) -> IO ()
modifyingMVar mv l f = modifyMVar_ mv $ \ps -> (\b -> set l b ps) <$> f (view l ps)
{-# INLINE modifyingMVar #-}

-- | Lens view into mvar.
useMVar :: MVar s -> Getting a s a -> IO a
useMVar e l = view l <$> readMVar e
{-# INLINE useMVar #-}

-- | Prelude-friendly replacement for <$>
infixr 5 <$$>
(<$$>) :: Doc -> Doc -> Doc
(<$$>) = (PP.<$>)

-- | Pretty show.
pshow :: Show a => a -> Doc
pshow = text . show
