{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :  Pact.Runtime.Capabilities
-- Copyright   :  (C) 2019 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Runtime capability handling.
--

module Pact.Runtime.Capabilities
    (acquireCapability
    ,popCapStack
    ,capabilityGranted
    ,revokeAllCapabilities
    ,grantedCaps
    ) where

import Control.Lens hiding (DefName)
import Data.Default
import Data.Foldable
import qualified Data.Set as S

import Pact.Types.Capability
import Pact.Types.Runtime



grantedCaps :: Eval e (S.Set Capability)
grantedCaps = S.fromList . toList <$> use evalCapabilities

capabilityGranted :: Capability -> Eval e Bool
capabilityGranted cap = S.member cap <$> grantedCaps

popCapStack :: (CapSlot Capability -> Eval e a) -> Eval e a
popCapStack act = do
  s <- use $ evalCapabilities . capStack
  case s of
    [] -> evalError def "acquireCapability: unexpected error: empty stack"
    (c:cs) -> do
      evalCapabilities . capStack .= cs
      act c

-- | Test if capability is already installed, if not
-- evaluate `test` which is expected to fail by some
-- guard throwing a failure. Upon successful return of
-- `test` install capability.
acquireCapability :: CapScope -> Capability -> Eval e () -> Eval e CapAcquireResult
acquireCapability scope cap test = do
  granted <- capabilityGranted cap
  if granted then return AlreadyAcquired else do
    -- push onto stack, which doubles as a "pending" queue for collecting composed caps.
    evalCapabilities . capStack %= (CapSlot scope cap []:)
    -- run test
    test
    case scope of
      CapCallStack -> return ()
      CapManaged _ -> popCapStack $ \c ->
        -- install managed
        evalCapabilities . capManaged %= (c:)
      CapComposed -> popCapStack $ \c ->
        -- install composed into slot at head of stack
        evalCapabilities . capStack . _head . csComposed %= (_csCap c:)

    return NewlyAcquired


revokeAllCapabilities :: Eval e ()
revokeAllCapabilities = evalCapabilities .= def
