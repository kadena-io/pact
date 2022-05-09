module Pact.Core.Literal where

import Data.Text(Text)
import Pact.Time(UTCTime)
import Data.Decimal

data Literal
  = LString !Text
  | LInteger !Integer
  | LDecimal !Decimal
  | LUnit
  | LBool !Bool
  | LTime !UTCTime
  deriving Show
