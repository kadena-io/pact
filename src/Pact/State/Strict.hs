{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Pact.State.Strict
-- Copyright: Copyright © 2022 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Pact.State.Strict
( modify

-- * Strict State Setter Lenses
, (.=)
, (%=)
, modifying
, (?=)
, (+=)
, (-=)
, (*=)
, (//=)
, (^=)
, (^^=)
, (**=)
, (&&=)
, (||=)
, (<>=)

-- * Strict Reader Monad Lenses
, locally

-- * Strict Monoidal Concatenation Lenses
, (<>~)

-- * Strict Maybe
, Maybe'(..)
, maybe'
, isJust'
, isNothing'
, fromMaybe'
, toMaybe'
, toMaybe
, mmm
, _Just'
, _Nothing'

-- * Strict Tuple
, T2(..)
) where

import Control.Applicative
import Control.DeepSeq
import qualified Control.Lens as L
import Control.Monad
import qualified Control.Monad.Reader as R
import qualified Control.Monad.State.Strict as S

import qualified Data.Aeson as A
import Data.Default
import Data.Functor.Classes
import Data.Hashable

import GHC.Generics

import Test.QuickCheck

-- internal modules

import Pact.Types.SizeOf

-- -------------------------------------------------------------------------- --
--  Operators

infix 4 .=, %=, ?=, +=, -=, *=, //=, ^=, ^^=, **=, ||=, &&=, <>=, <>~

-- -------------------------------------------------------------------------- --
-- State

modify :: S.MonadState s m => (s -> s) -> m ()
modify = S.modify'

-- -------------------------------------------------------------------------- --
-- Strict Lens Setters

(.=) :: S.MonadState s m => L.ASetter s s a b -> b -> m ()
l .= (!b) = S.modify' (l L..~ b)
{-# INLINE (.=) #-}

(%=) :: S.MonadState s m => L.ASetter s s a b -> (a -> b) -> m ()
l %= f = S.modify' (l L.%~ f) -- TODO
{-# INLINE (%=) #-}

modifying :: S.MonadState s m => L.ASetter s s a b -> (a -> b) -> m ()
modifying l f = S.modify' (L.over l f) -- TODO
{-# INLINE modifying #-}

(?=) :: S.MonadState s m => L.ASetter s s a (Maybe b) -> b -> m ()
l ?= (!b) = S.modify' (l L.?~ b)
{-# INLINE (?=) #-}

(+=) :: (S.MonadState s m, Num a) => L.ASetter' s a -> a -> m ()
l += (!b) = S.modify' (l L.+~ b)
{-# INLINE (+=) #-}

(-=) :: (S.MonadState s m, Num a) => L.ASetter' s a -> a -> m ()
l -= (!b) = S.modify' (l L.-~ b)
{-# INLINE (-=) #-}

(*=) :: (S.MonadState s m, Num a) => L.ASetter' s a -> a -> m ()
l *= (!b) = S.modify' (l L.*~ b)
{-# INLINE (*=) #-}

(//=) :: (S.MonadState s m, Fractional a) => L.ASetter' s a -> a -> m ()
l //= (!a) = S.modify' (l L.//~ a)
{-# INLINE (//=) #-}

(^=) :: (S.MonadState s m, Num a, Integral e) => L.ASetter' s a -> e -> m ()
l ^= (!e) = S.modify' (l L.^~ e)
{-# INLINE (^=) #-}

(^^=) :: (S.MonadState s m, Fractional a, Integral e) => L.ASetter' s a -> e -> m ()
l ^^= (!e) = S.modify' (l L.^^~ e)
{-# INLINE (^^=) #-}

(**=) :: (S.MonadState s m, Floating a) => L.ASetter' s a -> a -> m ()
l **= (!a) = S.modify' (l L.**~ a)
{-# INLINE (**=) #-}

(&&=) :: S.MonadState s m => L.ASetter' s Bool -> Bool -> m ()
l &&= (!b) = S.modify' (l L.&&~ b)
{-# INLINE (&&=) #-}

(||=) :: S.MonadState s m => L.ASetter' s Bool -> Bool -> m ()
l ||= (!b) = S.modify' (l L.||~ b)
{-# INLINE (||=) #-}

(<>=) :: (S.MonadState s m, Semigroup a) => L.ASetter' s a -> a -> m ()
l <>= (!a) = S.modify' (l <>~ a)
{-# INLINE (<>=) #-}

-- -------------------------------------------------------------------------- --
-- Strict Reader Monad Lenses

locally :: R.MonadReader s m => L.ASetter s s a b -> (a -> b) -> m r -> m r
locally l f = R.local $! L.over l f
{-# INLINE locally #-}

-- -------------------------------------------------------------------------- --
-- Strict Monoidal Concatenation Lenses

(<>~) :: Semigroup a => L.ASetter s t a a -> a -> s -> t
l <>~ (!n) = L.over l (<> n)
{-# INLINE (<>~) #-}

-- -------------------------------------------------------------------------- --
-- Strict Maybe

data Maybe' a = Nothing' | Just' a
    deriving (Show, Read, Eq, Ord, Generic)
    deriving (Functor, Traversable, Foldable)
    deriving (Hashable, NFData)

instance Show1 Maybe' where
    liftShowsPrec a b c d = liftShowsPrec a b c (toMaybe d)
    {-# INLINE liftShowsPrec #-}

instance Eq1 Maybe' where
    liftEq f a b = liftEq f (toMaybe a) (toMaybe b)
    {-# INLINE liftEq #-}

instance Semigroup a => Semigroup (Maybe' a) where
    Just' a <> Just' b = Just' (a <> b)
    a <> Nothing' = a
    Nothing' <> b = b
    {-# INLINE (<>) #-}

instance Semigroup a => Monoid (Maybe' a) where
    mempty = Nothing'
    {-# INLINE mempty #-}

instance Applicative Maybe' where
    pure = Just'
    Just' a <*> b = a <$> b
    Nothing' <*> _ = Nothing'
    {-# INLINE pure #-}
    {-# INLINE (<*>) #-}

instance Alternative Maybe' where
    empty = Nothing'
    Just' a <|> _ = Just' a
    Nothing' <|> b = b
    {-# INLINE empty #-}
    {-# INLINE (<|>) #-}

instance Monad Maybe' where
    Just' a >>= b = b a
    Nothing' >>= _ = Nothing'
    {-# INLINE (>>=) #-}

instance MonadPlus Maybe'

maybe' :: b -> (a -> b) -> Maybe' a -> b
maybe' _ f (Just' a) = f a
maybe' b _ Nothing' = b
{-# INLINE maybe' #-}

isJust' :: Maybe' a -> Bool
isJust' Just'{} = True
isJust' Nothing' = False
{-# INLINE isJust' #-}

isNothing' :: Maybe' a -> Bool
isNothing' Just'{} = False
isNothing' Nothing' = True
{-# INLINE isNothing' #-}

fromMaybe' :: a -> Maybe' a -> a
fromMaybe' _ (Just' a) = a
fromMaybe' a _ = a
{-# INLINE fromMaybe' #-}

toMaybe' :: Maybe a -> Maybe' a
toMaybe' (Just a) = Just' a
toMaybe' Nothing = Nothing'
{-# INLINE toMaybe' #-}

toMaybe :: Maybe' a -> Maybe a
toMaybe (Just' a) = Just a
toMaybe Nothing' = Nothing
{-# INLINE toMaybe #-}

instance A.ToJSON a => A.ToJSON (Maybe' a) where
    toJSON = A.toJSON . toMaybe
    toEncoding = A.toEncoding . toMaybe
    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

instance A.FromJSON a => A.FromJSON (Maybe' a) where
    parseJSON = fmap toMaybe' . A.parseJSON
    {-# INLINE parseJSON #-}

instance Default (Maybe' a) where
    def = toMaybe' def
    {-# INLINE def #-}

instance Arbitrary a => Arbitrary (Maybe' a) where
    arbitrary = toMaybe' <$> arbitrary
    {-# INLINE arbitrary #-}

instance SizeOf a => SizeOf (Maybe' a) where
    sizeOf = sizeOf . toMaybe

mmm :: L.Iso' (Maybe' a) (Maybe a)
mmm = L.iso toMaybe toMaybe'
{-# INLINE mmm #-}

_Just' :: L.Prism (Maybe' a) (Maybe' b) a b
_Just' = L.prism Just' $ maybe' (Left Nothing') Right
{-# INLINE _Just' #-}

_Nothing' :: L.Prism' (Maybe' a) ()
_Nothing' = L.prism' (const Nothing') $ maybe' (Just ()) (const Nothing)
{-# INLINE _Nothing' #-}

-- -------------------------------------------------------------------------- --
-- Strict Tuple

data T2 a b = T2 !a !b
    deriving (Show, Eq, Ord, Generic, NFData)

instance (Arbitrary a, Arbitrary b) => Arbitrary (T2 a b) where arbitrary = arbitrary >>= \(a,b) -> return (T2 a b)

