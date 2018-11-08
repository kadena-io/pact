{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE ViewPatterns          #-}

module Pact.Analyze.Util where

import           Control.Lens         (Iso, Snoc (_Snoc), iso, makeLenses,
                                       prism)
import qualified Data.Default         as Default
import qualified Data.Foldable        as Foldable
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

-- * SnocList
--
-- For when DList is not a great option because you occasionally need to
-- inspect the last-added item.

newtype SnocList a
  = SnocList { _reversed :: [a] }
  deriving (Eq, Ord, Show)

instance Semigroup (SnocList a) where
  SnocList xs <> SnocList ys = SnocList $ ys ++ xs

instance Monoid (SnocList a) where
  mempty = SnocList []

pattern ConsList :: [a] -> SnocList a
pattern ConsList xs <- SnocList (reverse -> xs)
  where ConsList xs = SnocList $ reverse xs

instance Functor SnocList where
  fmap f (SnocList revXs) = SnocList $ fmap f revXs

instance Foldable SnocList where
  foldMap f (SnocList (reverse -> xs)) = foldMap f xs

instance Traversable SnocList where
  -- Not efficient, but we want to sequence the effects in-order:
  traverse f (SnocList (reverse -> xs)) = SnocList . reverse <$> traverse f xs

snocList :: [a] -> SnocList a
snocList = SnocList . reverse

makeLenses ''SnocList

instance Snoc (SnocList a) (SnocList b) a b where
  _Snoc = prism
    (\(SnocList as,a) -> SnocList (a:as))
    (\(SnocList aas) ->
      case aas of
        (a:as) -> Right (SnocList as, a)
        []     -> Left  (SnocList []))

snocConsList :: Iso (SnocList a) (SnocList b) [a] [b]
snocConsList = iso Foldable.toList snocList
