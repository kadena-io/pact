{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Pact.Analyze.Alloc
  ( MonadAlloc (forAll, exists, free)
  , Alloc
  , runAlloc
  ) where

import           Control.Monad.Except        (ExceptT)
import           Control.Monad.Reader        (ReaderT)
import qualified Control.Monad.State.Lazy    as LS
import           Control.Monad.State.Strict  (StateT)
import           Control.Monad.Trans         (MonadTrans (lift))
import           Control.Monad.Trans.Maybe   (MaybeT)
import           Control.Monad.Writer.Strict (WriterT)
import qualified Control.Monad.Writer.Lazy   as LW
import           Data.SBV                    (Symbolic, SymWord)
import qualified Data.SBV                    as SBV

import           Pact.Analyze.Types          (S, sansProv)

-- | A restricted symbolic context in which only quantified variable allocation
-- is permitted.
class Monad m => MonadAlloc m where
  forAll :: SymWord a => m (S a) -- ^ universally quantified
  exists :: SymWord a => m (S a) -- ^ existentially quantified
  free   :: SymWord a => m (S a) -- ^ quantified per the context of sat vs prove

  default forAll :: (MonadTrans t, MonadAlloc m', m ~ t m', SymWord a) => m (S a)
  forAll = lift forAll

  default exists :: (MonadTrans t, MonadAlloc m', m ~ t m', SymWord a) => m (S a)
  exists = lift exists

  default free :: (MonadTrans t, MonadAlloc m', m ~ t m', SymWord a) => m (S a)
  free = lift free

instance MonadAlloc m             => MonadAlloc (ExceptT e m)
instance MonadAlloc m             => MonadAlloc (MaybeT m)
instance MonadAlloc m             => MonadAlloc (ReaderT r m)
instance MonadAlloc m             => MonadAlloc (StateT s m)
instance MonadAlloc m             => MonadAlloc (LS.StateT s m)
instance (MonadAlloc m, Monoid w) => MonadAlloc (WriterT w m)
instance (MonadAlloc m, Monoid w) => MonadAlloc (LW.WriterT w m)

-- * Standard 'MonadAlloc' implementation; 'Symbolic' restricted to use only
-- use quantified variable allocation.

-- We can't implement @AllocT@ yet because sbv doesn't have a @SymbolicT@.

newtype Alloc a = Alloc { runAlloc :: Symbolic a }
  deriving (Functor, Applicative, Monad)

instance MonadAlloc Alloc where
  forAll = Alloc $ sansProv <$> SBV.forall_
  exists = Alloc $ sansProv <$> SBV.exists_
  free   = Alloc $ sansProv <$> SBV.free_
