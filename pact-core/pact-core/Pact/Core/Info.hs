module Pact.Core.Info
 ( LineInfo(..)
 , combineSpan
 ) where

data LineInfo
  = LineInfo
  { _liLine :: !Int
  , _liColumn :: !Int
  , _liSpan :: !Int
  } deriving (Eq, Show)

-- | Combine two Line infos
-- and spit out how far down the expression spans.
combineSpan :: LineInfo -> LineInfo -> LineInfo
combineSpan (LineInfo l1 c1 _) (LineInfo l2 _ _) =
  LineInfo l1 c1 (l2 - l1 + 1)
