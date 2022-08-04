{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
module Pact.Types.Principal
( Principal(..)
, _W, _K, _R, _U, _M, _P
, principalParser
, guardToPrincipal
, mkPrincipalIdent
, showPrincipalType
) where

import Control.Applicative
import Control.Lens

import Data.Aeson (encode)
import Data.Attoparsec.Text
import Data.ByteString.Lazy (toStrict)
import Data.Foldable
import Data.Functor (void)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Pact.Eval (enforcePactValue')
import Pact.Types.Hash
import Pact.Types.Info
import Pact.Types.Names
import Pact.Types.Runtime

import Text.Parser.Combinators (eof)
import Data.Char (isHexDigit)


data Principal
  = K PublicKey
    -- ^ format: `k:public key`, where hex public key
    -- is the text public key of the underlying keyset
  | W Text Text
    -- ^ format: `w:b64url-encoded hash:pred` where
    -- the hash is a b64url-encoding of the hash of
    -- the list of public keys of the multisig keyset
  | R Text
    -- ^ format: `r:keyset-name` where keyset name is
    -- any definable keyset name
  | U Text Text
    -- ^ format: `u:fqn of user guard function:b64url-encoded
    -- hash of args
  | M ModuleName Text
    -- ^ format: `m:fq module name:fqn of module guard function
  | P PactId Text
    -- ^ format: `p:pactid:fqn of pact function
  deriving Eq
makePrisms ''Principal

instance Show Principal where
  show = T.unpack . showPrincipalType

-- | Given a principal type, construct its textual representation
--
-- Invariant: should roundtrip with parser.
--
mkPrincipalIdent :: Principal -> Text
mkPrincipalIdent = \case
  P pid n -> "p:" <> asString pid <> ":" <> asString n
  K pk -> "k:" <> asString pk
  W ph n -> "w:" <> ph <> ":" <> asString n
  R n -> "r:" <> asString n
  U n ph -> "u:" <> asString n <> ":" <> asString ph
  M mn n -> "m:" <> asString mn <> ":" <> asString n

-- | Show a principal guard type as a textual value
--
showPrincipalType :: Principal -> Text
showPrincipalType = \case
  K{} -> "k:"
  W{} -> "w:"
  R{} -> "r:"
  U{} -> "u:"
  M{} -> "m:"
  P{} -> "p:"

-- | Parser producing a principal value from a textual representation.
--
principalParser :: HasInfo i => i -> Parser Principal
principalParser (getInfo -> i) = kParser
  <|> wParser
  <|> rParser
  <|> uParser
  <|> mParser
  <|> pParser
  where
    base64UrlUnpaddedAlphabet :: String
    base64UrlUnpaddedAlphabet =
      "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"

    base64UrlHashParser = T.pack <$> count 43 (satisfy f) where
      f c = c `elem` base64UrlUnpaddedAlphabet

    hexKeyFormat =
      PublicKey . T.encodeUtf8 . T.pack <$> count 64 (satisfy isHexDigit)

    char' = void . char
    eof' = void eof
    nameParser' i' = asString <$> nameParser i'

    kParser = do
      char' 'k'
      char' ':'
      k <- hexKeyFormat
      eof'
      pure $ K k

    wParser = do
      char' 'w'
      char' ':'
      h <- base64UrlHashParser
      char' ':'
      n <- nameParser' i
      eof'
      pure $ W h n

    pParser = do
      char' 'p'
      char' ':'
      h <- PactId <$> base64UrlHashParser
      char' ':'
      n <- nameParser' i
      eof'
      pure $ P h n

    rParser = do
      char' 'r'
      char' ':'
      n <- nameParser' i
      eof'
      pure $ R n

    mParser = do
      char' 'm'
      char' ':'
      mn <- moduleNameParser
      char' ':'
      n <- nameParser' i
      eof'
      pure $ M mn n

    uParser = do
      char' 'u'
      char' ':'
      n <- nameParser' i
      char' ':'
      h <- base64UrlHashParser
      eof'
      pure $ U n h

-- | Given a pact guard, convert to a principal type
--
guardToPrincipal :: Guard (Term Name) -> Eval e Principal
guardToPrincipal = \case
  GPact (PactGuard pid n) -> pure $ P pid n
  GKeySet (KeySet ks pf) -> case (toList ks,asString pf) of
    ([k],"keys-all") -> pure $ K k
    (l,fun) -> pure $ W (asString $ mkHash $ map _pubKey l) fun
  GKeySetRef (KeySetName n) -> pure $ R n
  GModule (ModuleGuard mn n) -> pure $ M mn n
  GUser (UserGuard f args) -> do
    args' <- enforcePactValue' args
    let a = asString $ mkHash $ map toJSONPactValue args'
        f' = asString  f
    pure $ U f' a
  where
    mkHash bs = pactHash $ mconcat bs
    toJSONPactValue = toStrict . encode
