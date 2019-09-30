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
    ,capabilityGranted
    ,popCapStack
    ,revokeAllCapabilities
    ,grantedCaps
    ,ApplyMgrFun,noopApplyMgrFun
    ) where

import Control.Lens hiding (DefName)
import Data.Bool
import Data.Default
import Data.Foldable
import qualified Data.Set as S

import Pact.Types.Capability
import Pact.Types.PactValue
import Pact.Types.Runtime

-- | Tie the knot with Pact.Eval by having caller supply `apply` and inflate/deflate
-- params to proper object arguments.
type ApplyMgrFun e = Def Ref -> [PactValue] -> [PactValue] -> Eval e (Maybe [PactValue])

noopApplyMgrFun :: ApplyMgrFun e
noopApplyMgrFun _ _ _ = return Nothing


grantedCaps :: Eval e (S.Set Capability)
grantedCaps = S.fromList . toList <$> use evalCapabilities

-- | Matches Managed -> managed list, callstack and composed to stack list
-- Composed should possibly check both ...
capabilityGranted :: CapScope -> Capability -> Eval e Bool
capabilityGranted scope cap = memSet <$> scopeCaps
  where
    memSet = S.member cap . S.fromList . concatMap toList
    scopeCaps = case scope of
      CapManaged _ -> use $ evalCapabilities . capManaged
      _ -> use $ evalCapabilities . capStack

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
acquireCapability :: ApplyMgrFun e -> CapScope -> Capability -> Eval e () -> Eval e CapAcquireResult
acquireCapability af scope cap test = granted >>= bool evalCap alreadyGranted
  where

    granted = capabilityGranted scope cap
    alreadyGranted = return AlreadyAcquired

    evalCap = do
      -- liftIO $ print ("acquireCapability",scope,cap)
      case scope of
        CapManaged _ -> evalManaged
        CapCallStack -> evalStack
        CapComposed -> evalComposed
      return NewlyAcquired

    -- Managed: push, test, pop and install
    evalManaged =
      push >> test >> popCapStack installManaged

    installManaged c = evalCapabilities . capManaged %= (c:)

    -- Callstack: check if managed, in which case push, otherwise
    -- push and test.
    evalStack = checkManaged af cap >>= \r -> case r of
      Nothing -> push >> test
      Just {} -> push

    -- Composed: check if managed, in which case install onto head,
    -- otherwise push, test, pop and install onto head
    evalComposed = checkManaged af cap >>= \r -> case r of
      Nothing -> push >> test >> popCapStack installComposed
      Just {} -> installComposed (CapSlot scope cap [])

    installComposed c = evalCapabilities . capStack . _head . csComposed %= (_csCap c:)

    push = evalCapabilities . capStack %= (CapSlot scope cap []:)


checkManaged :: ApplyMgrFun e -> Capability -> Eval e (Maybe ())
checkManaged applyF cap = use (evalCapabilities . capManaged) >>= check []
  where
    check _ [] = return Nothing
    check rms (mgd:ms) = runManaged mgd >>= handleResult rms ms (check (mgd:rms) ms)

    runManaged mcs = case _csScope mcs of -- validate scope
      CapManaged (Just mf) -> case _csCap mcs of -- validate user mg cap and has mgr fun
        UserCapability mqn mas -> case cap of -- validate user test cap
          UserCapability cqn cas
            | cqn == mqn -> -- check type match and apply mgr fun
              applyMgrFun mcs mqn mf mas cas
            | otherwise -> return Nothing
          _ -> return Nothing
        _ -> return Nothing
      _ -> return Nothing

    handleResult _ _ next Nothing = next
    handleResult rms ms _ (Just mgd') = do
      -- install modified mgd cap
      evalCapabilities . capManaged .= (rms ++ (mgd':ms))
      -- success
      return $ Just ()

    applyMgrFun mcs mqn mf mas cas = applyF mf mas cas >>= \r -> case r of
      Nothing -> return Nothing
      Just mas' -> return $ Just $ set csCap (UserCapability mqn mas') mcs


revokeAllCapabilities :: Eval e ()
revokeAllCapabilities = evalCapabilities .= def
