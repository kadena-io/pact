{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Pact.Core.Literal
 ( _LString
 , _LInteger
 , _LDecimal
 , _LUnit
 , _LBool
 , _LTime
 , Literal(..)) where

import Control.Lens(makePrisms)
import Data.Text(Text)
import Data.Decimal

import Pact.Time(UTCTime)
import Pact.Core.Pretty

data Literal
  = LString !Text
  | LInteger !Integer
  | LDecimal !Decimal
  | LUnit
  | LBool !Bool
  | LTime !UTCTime
  deriving (Show, Eq)

makePrisms ''Literal

instance Pretty Literal where
  pretty = \case
    LString t -> dquotes (pretty t)
    LInteger i -> pretty i
    LDecimal d -> pretty (show d)
    LUnit -> "()"
    LBool b -> if b then "true" else "false"
    LTime _t -> "<TODO:TIME>"
