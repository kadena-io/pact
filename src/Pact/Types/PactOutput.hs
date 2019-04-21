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

import Control.Applicative ((<|>))
import qualified Data.Aeson as A
import Data.Aeson hiding (Value(..))
import Data.Default (def)
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V

import Pact.Types.Exp (Literal(..))
import Pact.Types.Pretty (renderCompactText)
import Pact.Types.Term (ObjectMap(..),Term(..),Name(..),Object(..),Guard(..))
import Pact.Types.Type (Type(TyAny))


data PactOutput
  = PLiteral Literal
  | PList (Vector PactOutput)
  | PObject (ObjectMap PactOutput)
  | PGuard Guard
  deriving (Eq,Show)


instance ToJSON PactOutput where
  toJSON (PLiteral l) = toJSON l
  toJSON (PObject o) = toJSON o
  toJSON (PList v) = toJSON v
  toJSON (PGuard x) = toJSON x


instance FromJSON PactOutput where
  parseJSON v =
    (PLiteral <$> parseJSON v) <|>
    (PObject <$> parseJSON v) <|>
    (PList <$> parseJSON v) <|>
    (PGuard <$> parseJSON v)


toPactOutput :: Term Name -> Either Text PactOutput
toPactOutput (TLiteral l _) = pure $ PLiteral l
toPactOutput (TObject (Object o _ _ _) _) = PObject <$> traverse toPactOutput o
toPactOutput (TList l _ _) = PList <$> V.mapM toPactOutput l
toPactOutput (TGuard x _) = pure (PGuard x)
toPactOutput t = Left $ "Unable to convert Term: " <> renderCompactText t

fromPactOutput :: PactOutput -> Term Name
fromPactOutput (PLiteral l) = TLiteral l def
fromPactOutput (PObject o) = TObject (Object (fmap fromPactOutput o) TyAny def def) def
fromPactOutput (PList l) = TList (fmap fromPactOutput l) TyAny def
fromPactOutput (PGuard x) = TGuard x def


toPactOutput' :: Term Name -> PactOutput
toPactOutput' t = case toPactOutput t of
  Right r -> r
  Left _ -> PLiteral $ LString $ renderCompactText t
