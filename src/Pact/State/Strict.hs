{-# LANGUAGE BangPatterns #-}
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
) where

import qualified Control.Lens as L
import qualified Control.Monad.Reader as R
import qualified Control.Monad.State.Strict as S

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

