{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
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
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (toStrict)
import Data.Foldable
import Data.Functor (void)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Pact.Types.Hash
import Pact.Types.Info
import Pact.Types.KeySet (keysetNameParser)
import Pact.Types.Names
import Pact.Types.PactValue (PactValue)
import Pact.Types.Term
import Pact.Types.Util

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
  | R KeySetName
    -- ^ format: `r:keyset-name` where keyset name is
    -- any definable keyset name
  | U Text Text
    -- ^ format: `u:fqn of user guard function:b64url-encoded
    -- hash of args
  | M ModuleName Text
    -- ^ format: `m:fq module name:fqn of module guard function
  | P PactId Text
    -- ^ format: `p:pactid:fqn of pact function
  | C Text
    -- ^ format: `c:hash of cap name + cap params + pactId if any
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
  P pid n -> "p:" <> asString pid <> ":" <> n
  K pk -> "k:" <> asString pk
  W ph n -> "w:" <> ph <> ":" <> n
  R n -> "r:" <> asString n
  U n ph -> "u:" <> n <> ":" <> ph
  M mn n -> "m:" <> asString mn <> ":" <> n
  C c -> "c:" <> c

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
  C{} -> "c:"

-- | Parser producing a principal value from a textual representation.
--
principalParser :: HasInfo i => i -> Parser Principal
principalParser (getInfo -> i) = kParser
  <|> wParser
  <|> rParser
  <|> uParser
  <|> mParser
  <|> pParser
  <|> cParser
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
      n <- keysetNameParser
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

    cParser = do
      char' 'c'
      char' ':'
      h <- base64UrlHashParser
      eof'
      pure $ C h


-- | Given a pact guard, convert to a principal type.
-- Parameterized for use with non-Eval contexts.
guardToPrincipal
    :: Monad m
    => (Int -> m ())
    -- ^ gas charger
    -> Guard PactValue
    -> m Principal
guardToPrincipal chargeGas = \case
  GPact (PactGuard pid n) -> chargeGas 1 >> pure (P pid n)
  -- TODO later: revisit structure of principal k and w accounts in light of namespaces
  GKeySet (KeySet ks pf) -> case (toList ks,asString pf) of
    ([k],"keys-all") -> chargeGas 1 >> pure (K k)
    (l,fun) -> do
      h <- mkHash $ map _pubKey l
      pure $ W (asString h) fun
  GKeySetRef ksn -> chargeGas 1 >> pure (R ksn)
  GModule (ModuleGuard mn n) -> chargeGas 1 >> pure (M mn n)
  GUser (UserGuard f args) -> do
    a <- mkHash $ map toJSONPactValue args
    pure $ U (asString f) (asString a)
  GCapability (CapabilityGuard f args pid) -> do
    let args' = map toJSONPactValue args
        f' = T.encodeUtf8 $ asString f
        pid' = T.encodeUtf8 . asString <$> pid
    h <- mkHash $ (f':args') ++ maybe [] pure pid'
    pure $ C $ asString h
  where
    mkHash bss = do
      let bs = mconcat bss
      chargeGas $ 1 + (BS.length bs `quot` 64) -- charge for 64 bytes of hashing
      return $ pactHash bs
    toJSONPactValue = toStrict . encode
