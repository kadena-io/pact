{-# LANGUAGE OverloadedStrings #-}
module Pact.Utils
( decodeB64UrlNoPaddingText
, encodeB64UrlNoPaddingText
)where


import qualified Data.ByteString as B
import qualified Data.ByteString.Base64.URL as B64U
import qualified Data.Text as T
import qualified Data.Text.Encoding as T


-- | Decode a binary value from a textual base64-url without padding
-- representation. A 'Base64DecodeException' is thrown if the input is not a
-- valid base64-url without padding encoding.
--
decodeB64UrlNoPaddingText :: T.Text -> Either String B.ByteString
decodeB64UrlNoPaddingText = B64U.decode . T.encodeUtf8 . pad
  where
    pad t =
      let s = T.length t `mod` 4
      in t <> T.replicate ((4 - s) `mod` 4) "="
{-# INLINE decodeB64UrlNoPaddingText #-}


-- | Encode a binary value to a textual base64-url without padding
-- representation.
--
encodeB64UrlNoPaddingText :: B.ByteString -> T.Text
encodeB64UrlNoPaddingText = T.dropWhileEnd (== '=')
  . T.decodeUtf8
  . B64U.encode
{-# INLINE encodeB64UrlNoPaddingText #-}
