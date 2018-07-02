{-# language FlexibleContexts #-}

module Pact.Analyze.Util where

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap

(<$$$>) :: (Functor f, Functor g, Functor h) => (a -> b) -> f (g (h a)) -> f (g (h b))
(<$$$>) = fmap . fmap . fmap

(<&&>) :: (Functor f, Functor g) => f (g a) -> (a -> b) -> f (g b)
(<&&>) = flip (<$$>)

for2
  :: (Traversable s, Traversable t, Applicative f)
  => s (t a) -> (a -> f b) -> f (s (t b))
for2 = flip (traverse . traverse)
