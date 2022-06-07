{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Pact.Core.Literal where

import Data.Text(Text)
import Pact.Time(UTCTime)
import Data.Decimal

import Pact.Core.Pretty

data Literal
  = LString !Text
  | LInteger !Integer
  | LDecimal !Decimal
  | LUnit
  | LBool !Bool
  | LTime !UTCTime
  deriving (Show, Eq)

instance Pretty Literal where
  pretty = \case
    LString t -> dquotes (pretty t)
    LInteger i -> pretty i
    -- TODO: SUS
    LDecimal d -> pretty (show d)
    LUnit -> "()"
    LBool b -> if b then "true" else "false"
    LTime _t -> "<TODO:TIME>"
