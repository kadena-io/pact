{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Pact.Analyze.Alloc
  ( MonadAlloc (forAll, exists, free, singForAll, singExists, singFree)
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
import           Data.SBV                    (Symbolic)
import qualified Data.SBV                    as SBV

import           Pact.Analyze.Types          (S, SingI(sing), SingTy, Concrete,
                                              sansProv, withSymWord)

-- | A restricted symbolic context in which only quantified variable allocation
-- is permitted.
class Monad m => MonadAlloc m where
  forAll :: SingI a => m (S (Concrete a)) -- ^ universally quantified
  exists :: SingI a => m (S (Concrete a)) -- ^ existentially quantified
  free   :: SingI a => m (S (Concrete a)) -- ^ quantified per the context of sat vs prove

  singForAll :: SingTy a -> m (S (Concrete a))
  singExists :: SingTy a -> m (S (Concrete a))
  singFree   :: SingTy a -> m (S (Concrete a))

  forAll = singForAll sing
  exists = singExists sing
  free   = singFree sing

  default singForAll
    :: (MonadTrans t, MonadAlloc m', m ~ t m')
    => SingTy a -> m (S (Concrete a))
  singForAll ty = lift (singForAll ty)

  default singExists
    :: (MonadTrans t, MonadAlloc m', m ~ t m')
    => SingTy a -> m (S (Concrete a))
  singExists ty = lift (singExists ty)

  default singFree
    :: (MonadTrans t, MonadAlloc m', m ~ t m')
    => SingTy a -> m (S (Concrete a))
  singFree   = lift . singFree

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
  singForAll ty = Alloc $ withSymWord ty $ sansProv <$> SBV.forall_
  singExists ty = Alloc $ withSymWord ty $ sansProv <$> SBV.exists_
  singFree   ty = Alloc $ withSymWord ty $ sansProv <$> SBV.free_
