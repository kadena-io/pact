{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}

-- | Monadic contexts, more restricted than 'Symbolic', that only allow
-- allocation of quantified symbolic variables.
module Pact.Analyze.Alloc
  ( MonadAlloc (singForAll, singExists, singFree)
  , forAll, exists, free
  , Alloc
  , runAlloc
  ) where

import           Control.Monad.Except        (ExceptT)
import           Control.Monad.Reader        (ReaderT)
import qualified Control.Monad.State.Lazy    as LS
import           Control.Monad.State.Strict  (StateT)
import           Control.Monad.Trans         (MonadTrans (lift))
import           Control.Monad.Trans.Maybe   (MaybeT)
import qualified Control.Monad.Writer.Lazy   as LW
import           Control.Monad.Writer.Strict (WriterT)
import           Data.SBV                    (Symbolic)
import qualified Data.SBV                    as SBV

import           Pact.Analyze.Types          (Concrete, S, SingI (sing), SingTy,
                                              sansProv, withSymVal)

-- | A restricted symbolic context in which only quantified variable allocation
-- is permitted.
class Monad m => MonadAlloc m where

  singForAll :: SingTy a -> m (S (Concrete a)) -- ^ universally quantified
  singExists :: SingTy a -> m (S (Concrete a)) -- ^ existentially quantified
  singFree   :: SingTy a -> m (S (Concrete a)) -- ^ quantified per the context of sat vs prove

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

forAll :: forall a m. (MonadAlloc m, SingI a) => m (S (Concrete a))
forAll = singForAll (sing @a)

exists :: forall a m. (MonadAlloc m, SingI a) => m (S (Concrete a))
exists = singExists (sing @a)

free :: forall a m. (MonadAlloc m, SingI a) => m (S (Concrete a))
free = singFree (sing @a)

instance MonadAlloc m             => MonadAlloc (ExceptT e m)
instance MonadAlloc m             => MonadAlloc (MaybeT m)
instance MonadAlloc m             => MonadAlloc (ReaderT r m)
instance MonadAlloc m             => MonadAlloc (StateT s m)
instance MonadAlloc m             => MonadAlloc (LS.StateT s m)
instance (MonadAlloc m, Monoid w) => MonadAlloc (WriterT w m)
instance (MonadAlloc m, Monoid w) => MonadAlloc (LW.WriterT w m)

-- * Standard 'MonadAlloc' implementation; 'Symbolic' restricted to use only
-- use quantified variable allocation.

-- TODO: implement @AllocT@ now that sbv has @SymbolicT@.

newtype Alloc a = Alloc { runAlloc :: Symbolic a }
  deriving (Functor, Applicative, Monad)

instance MonadAlloc Alloc where
  singForAll ty = Alloc $ withSymVal ty $ sansProv <$> SBV.forall_
  singExists ty = Alloc $ withSymVal ty $ sansProv <$> SBV.exists_
  singFree   ty = Alloc $ withSymVal ty $ sansProv <$> SBV.free_
