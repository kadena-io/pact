{-# language FlexibleContexts #-}

module Pact.Analyze.Util where

import qualified Data.Default              as Default
import           Pact.Types.Lang           (Info(_iInfo), Parsed)
import           Pact.Types.Typecheck      (AST(_aNode), Node(_aId), _tiInfo)

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap

(<$$$>) :: (Functor f, Functor g, Functor h) => (a -> b) -> f (g (h a)) -> f (g (h b))
(<$$$>) = fmap . fmap . fmap

(<&&>) :: (Functor f, Functor g) => f (g a) -> (a -> b) -> f (g b)
(<&&>) = flip (<$$>)

-- | Function composition that consumes two args instead of one
(...) :: (a -> b) -> (x -> y -> a) -> x -> y -> b
(...) = (<$$>)

for2
  :: (Traversable s, Traversable t, Applicative f)
  => s (t a) -> (a -> f b) -> f (s (t b))
for2 = flip (traverse . traverse)

astParsed :: AST Node -> Parsed
astParsed = nodeParsed . _aNode

nodeParsed :: Node -> Parsed
nodeParsed = getInfoParsed . _tiInfo . _aId

getInfoParsed :: Info -> Parsed
getInfoParsed parsed = case _iInfo parsed of
  Nothing               -> dummyParsed
  Just (_code, parsed') -> parsed'

dummyParsed :: Parsed
dummyParsed = Default.def
