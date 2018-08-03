{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE ViewPatterns          #-}

module Pact.Analyze.Util where

import           Control.Lens         (Snoc (_Snoc), makeLenses, prism)
import qualified Data.Default         as Default
import           Pact.Types.Lang      (Info (_iInfo), Parsed)
import           Pact.Types.Typecheck (AST (_aNode), Node (_aId), _tiInfo)

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap

(<&&>) :: (Functor f, Functor g) => f (g a) -> (a -> b) -> f (g b)
(<&&>) = flip (<$$>)

-- | Function composition that consumes two args instead of one
(...) :: (a -> b) -> (x -> y -> a) -> x -> y -> b
(...) = (<$$>)

-- | Function composition that consumes three args instead of one
(....) :: (a -> b) -> (x -> y -> z -> a) -> x -> y -> z -> b
(....) = fmap . fmap . fmap

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

vacuousMatch :: String -> a
vacuousMatch msg = error $ "vacuous match: " ++ msg

-- * ReversedList

newtype ReversedList a
  = ReversedList { _reversedList :: [a] }
  deriving (Eq, Ord, Show)

instance Monoid (ReversedList a) where
  mempty = ReversedList []
  ReversedList xs `mappend` ReversedList ys = ReversedList $ ys ++ xs

pattern UnreversedList :: [a] -> ReversedList a
pattern UnreversedList xs <- ReversedList (reverse -> xs)

makeLenses ''ReversedList

instance Snoc (ReversedList a) (ReversedList b) a b where
  _Snoc = prism
    (\(ReversedList as,a) -> ReversedList (a:as))
    (\(ReversedList aas) ->
      case aas of
        (a:as) -> Right (ReversedList as, a)
        []     -> Left  (ReversedList []))
