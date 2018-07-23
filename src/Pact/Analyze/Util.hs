{-# language FlexibleContexts #-}

module Pact.Analyze.Util where

import qualified Data.Default              as Default
import           Pact.Types.Lang           (Info(_iInfo), Parsed)
import           Pact.Types.Typecheck      (AST(_aNode), Node(_aId), _tiInfo)

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap

(<&&>) :: (Functor f, Functor g) => f (g a) -> (a -> b) -> f (g b)
(<&&>) = flip (<$$>)

-- | Function composition that consumes two args instead of one
(...) :: (a -> b) -> (x -> y -> a) -> x -> y -> b
(...) = (<$$>)

for2
  :: (Traversable s, Traversable t, Applicative f)
  => s (t a) -> (a -> f b) -> f (s (t b))
for2 = flip (traverse . traverse)

astToParsed :: AST Node -> Parsed
astToParsed = nodeToParsed . _aNode

astToInfo :: AST Node -> Info
astToInfo = nodeToInfo . _aNode

nodeToParsed :: Node -> Parsed
nodeToParsed = infoToParsed . nodeToInfo

nodeToInfo :: Node -> Info
nodeToInfo =  _tiInfo . _aId

infoToParsed :: Info -> Parsed
infoToParsed parsed = case _iInfo parsed of
  Nothing               -> dummyParsed
  Just (_code, parsed') -> parsed'

-- | A 'Parsed' for when there is no location info available.
dummyParsed :: Parsed
dummyParsed = Default.def

-- | An 'Info' for when there is no location info available.
dummyInfo :: Info
dummyInfo = Default.def
