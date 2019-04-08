{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      :  Pact.Types.Term
-- Copyright   :  (C) 2019 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- 'PactOutput' is a type for marshalling term values in Pact's
-- "front end", to address the issue where 'Term Name' cannot
-- be safely or meaningfully represented outside of the interpreter.
-- 'PactOutput' is needed for reliable reproduction of receipt
-- results and continuation arguments in the SPV process.
--

module Pact.Types.PactOutput
  ( PactOutput(..)
  , toPactOutput
  , toPactOutput'
  , fromPactOutput
  ) where

import qualified Data.Aeson as A
import Data.Aeson hiding (Value(..))
import Data.Decimal
import Data.Default (def)
import Data.Scientific
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V

import Pact.Types.Exp (Literal(..))
import Pact.Types.Pretty (renderCompactText)
import Pact.Types.Term (ObjectMap(..),PactGuard(..),KeySet(..),KeySetName(..),
                        ModuleGuard(..),Term(..),Name(..),Object(..),Guard(..),
                        UserGuard(..))
import Pact.Types.Type (Type(TyAny))

data UserGuard' = UserGuard' (ObjectMap PactOutput) Name
   deriving (Eq,Show,Ord)

instance ToJSON UserGuard' where
  toJSON (UserGuard' o n) = object [ "data" .= o, "pred" .= n ]
instance FromJSON UserGuard' where
  parseJSON = withObject "UserGuard'" $ \o ->
    UserGuard' <$> o .: "data" <*> o .: "pred"




data Guard'
  = GPact' !PactGuard
  | GKeySet' !KeySet
  | GKeySetRef' !KeySetName
  | GModule' !ModuleGuard
  | GUser' !UserGuard'
  deriving (Eq,Ord,Show)

data ObjType
  = OObject
  | ODecimal
  | OTime
  | OGPact
  | OGKeySet
  | OGKeySetRef
  | OGModule
  | OGUser

instance ToJSON ObjType where
  toJSON o = A.String $ case o of
    OObject -> "o"
    ODecimal -> "d"
    OTime -> "t"
    OGPact -> "gpact"
    OGKeySet -> "ks"
    OGKeySetRef -> "ksref"
    OGModule -> "gmodule"
    OGUser -> "guser"
instance FromJSON ObjType where
  parseJSON = withText "ObjType" $ \s -> case s of
     "o" -> pure OObject
     "d" -> pure ODecimal
     "t" -> pure OTime
     "gpact" -> pure OGPact
     "ks" -> pure OGKeySet
     "ksref" -> pure OGKeySetRef
     "gmodule" -> pure OGModule
     "guser" -> pure OGUser
     _ -> fail "unrecognized ObjType"




data PactOutput
  = PLiteral Literal
  | PList (Vector PactOutput)
  | PObject (ObjectMap PactOutput)
  | PGuard Guard'
  deriving (Eq,Ord,Show)


pToJSON :: ToJSON v => ObjType -> v -> A.Value
pToJSON t v = object [ "t" .= t, "v" .= v ]

instance ToJSON PactOutput where
  toJSON (PLiteral l) = case l of
    LInteger i -> A.Number (scientific i 0)
    LDecimal d -> case normalizeDecimal d of
      (Decimal p m) -> pToJSON ODecimal $ object [ "p" .= p, "m" .= m ]
    LString s -> toJSON s
    LBool b -> toJSON b
    LTime t -> pToJSON OTime t
  toJSON (PObject o) = pToJSON OObject o
  toJSON (PList v) = toJSON v
  toJSON (PGuard x) = case x of
    GPact' g -> pToJSON OGPact g
    GKeySet' g -> pToJSON OGKeySet g
    GKeySetRef' g -> pToJSON OGKeySetRef g
    GModule' g -> pToJSON OGModule g
    GUser' g -> pToJSON OGUser g

instance FromJSON PactOutput where
  parseJSON (A.Number s)
    | base10Exponent s /= 0 = fail "PactOutput: Only integer numbers supported"
    | otherwise = pure (PLiteral (LInteger (coefficient s)))
  parseJSON (A.String s) = pure (PLiteral (LString s))
  parseJSON (A.Bool b) = pure (PLiteral (LBool b))
  parseJSON A.Null = fail "PactOutput: illegal NULL"
  parseJSON (A.Array v) = PList <$> V.mapM parseJSON v
  parseJSON (A.Object o) = o .: "t" >>= \t -> case t of
    OObject -> PObject <$> pj o
    ODecimal -> pj o >>= fmap (PLiteral . LDecimal) . withObject "PDecimal"
      (\d -> normalizeDecimal <$> (Decimal <$> d .: "p" <*> d .: "m"))
    OTime -> PLiteral . LTime <$> pj o
    OGPact -> PGuard . GPact' <$> pj o
    OGKeySet -> PGuard . GKeySet' <$> pj o
    OGKeySetRef -> PGuard . GKeySetRef' <$> pj o
    OGModule -> PGuard . GModule' <$> pj o
    OGUser -> PGuard . GUser' <$> pj o
    where pj o' = o' .: "v"


toPactOutput :: Term Name -> Either Text PactOutput
toPactOutput (TLiteral l _) = pure $ PLiteral l
toPactOutput (TObject (Object o _ _ _) _) = PObject <$> traverse toPactOutput o
toPactOutput (TList l _ _) = PList <$> V.mapM toPactOutput l
toPactOutput (TGuard x _) = fmap PGuard $ case x of
  GPact g -> pure $ GPact' g
  GKeySet g -> pure $ GKeySet' g
  GKeySetRef g -> pure $ GKeySetRef' g
  GModule g -> pure $ GModule' g
  GUser (UserGuard (Object o _ _ _) n) -> GUser' <$> (UserGuard' <$> traverse toPactOutput o <*> pure n)
toPactOutput t = Left $ "Unable to convert Term: " <> renderCompactText t

fromPactOutput :: PactOutput -> Term Name
fromPactOutput (PLiteral l) = TLiteral l def
fromPactOutput (PObject o) = TObject (Object (fmap fromPactOutput o) TyAny def def) def
fromPactOutput (PList l) = TList (fmap fromPactOutput l) TyAny def
fromPactOutput (PGuard x) = (`TGuard` def) $ case x of
  GPact' g -> GPact g
  GKeySet' g -> GKeySet g
  GKeySetRef' g -> GKeySetRef g
  GModule' g -> GModule g
  GUser' (UserGuard' d p) -> GUser $ UserGuard (Object (fmap fromPactOutput d) TyAny def def) p


toPactOutput' :: Term Name -> PactOutput
toPactOutput' t = case toPactOutput t of
  Right r -> r
  Left _ -> PLiteral $ LString $ renderCompactText t
