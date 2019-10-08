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
    ,checkSigCaps
    ) where

import Control.Monad
import Control.Lens hiding (DefName)
import Data.Default
import Data.Foldable
import Data.List
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Pact.Types.Capability
import Pact.Types.PactValue
import Pact.Types.Pretty
import Pact.Types.Runtime

-- | Tie the knot with Pact.Eval by having caller supply `apply` and inflate/deflate
-- params to proper object arguments.
type ApplyMgrFun e = Def Ref -> [PactValue] -> [PactValue] -> Eval e (Either PactError [PactValue])

-- | Noop fun always matches/returns same
noopApplyMgrFun :: ApplyMgrFun e
noopApplyMgrFun _ mgd _ = return $ Right mgd

-- | Get any cap that is currently granted, of any scope.
grantedCaps :: Eval e (S.Set Capability)
grantedCaps = toSet <$> getAllStackCaps <*> getAllManaged
  where
    toSet a b = S.fromList $ a ++ b
    getAllManaged = concatMap toList . S.toList <$> use (evalCapabilities . capManaged)

-- | Matches Managed -> managed list, callstack and composed to stack list
-- Composed should possibly check both ...
capabilityGranted :: CapScope m -> Capability -> Eval e Bool
capabilityGranted scope cap = elem cap <$> scopeCaps
  where
    scopeCaps :: Eval e [Capability]
    scopeCaps = case scope of
      -- Managed: only check top-level, not composed
      CapManaged _ -> map _csCap . S.toList <$> use (evalCapabilities . capManaged)
      -- Other: check acquired, both top and composed.
      _ -> getAllStackCaps

getAllStackCaps :: Eval e [Capability]
getAllStackCaps = concatMap toList <$> use (evalCapabilities . capStack)

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
acquireCapability
  :: HasInfo i
  => i
  -> ApplyMgrFun e
  -> CapScope (Maybe (Def Ref))
  -> Capability
  -> Eval e ()
  -> Eval e CapAcquireResult
acquireCapability i af scope cap test = go
  where

    alreadyGranted = return AlreadyAcquired

    go = do
      granted <- capabilityGranted scope cap
      if granted
        then alreadyGranted
        else do
          seen <- seenCaps
          if cap `S.member` seen
            then evalError' i $ "Duplicate install of capability " <> pretty cap <> ", scope " <> pretty scope
            else evalCap

    seenCaps = case scope of
      CapManaged _ -> use $ evalCapabilities . capManagedSeen
      _ -> return mempty

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

    installManaged cs = do
      evalCapabilities . capManagedSeen %= S.insert (_csCap cs)
      evalCapabilities . capManaged %= S.insert cs

    -- Callstack: check if managed, in which case push, otherwise
    -- push and test.
    evalStack = checkManaged af cap >>= \r -> case r of
      Nothing -> push >> test
      Just (Left e) -> evalError def (prettyString e)
      Just (Right mgd) -> pushSlot (CapSlot scope cap (_csComposed mgd))

    -- Composed: check if managed, in which case install onto head,
    -- otherwise push, test, pop and install onto head
    evalComposed = checkManaged af cap >>= \r -> case r of
      Nothing -> push >> test >> popCapStack installComposed
      Just (Left e) -> evalError def (prettyString e)
      Just (Right mgd) -> installComposed (CapSlot scope cap (_csComposed mgd))

    installComposed c = evalCapabilities . capStack . _head . csComposed %= (_csCap c:)

    push = pushSlot (CapSlot scope cap [])

    pushSlot s = evalCapabilities . capStack %= (s:)


checkManaged :: ApplyMgrFun e -> Capability -> Eval e (Maybe (Either String (CapSlot Capability)))
checkManaged applyF cap = use (evalCapabilities . capManaged) >>= go
  where
    noMatch = return $ Nothing

    go mgdCaps = do
      (success,failures,processedMgdCaps) <- foldM check (Nothing,[],mempty) mgdCaps
      case success of
        Just newMgdCap -> do
          evalCapabilities . capManaged .= processedMgdCaps
          return $ Just $ Right newMgdCap
        Nothing -> case failures of
          [] -> return Nothing
          es -> return $ Just $ Left $
            "Acquire of managed capability failed: " ++
            (intercalate "," (map (renderCompactString' . peDoc) es))

    check (successR,failedRs,ms) m = case successR of
      Just {} -> return (successR,[],S.insert m ms) -- short circuit on success
      Nothing -> do
        r <- runManaged m
        case r of
          Nothing -> return (Nothing,failedRs,S.insert m ms) -- skip
          Just (Left e) -> return (Nothing,e:failedRs,S.insert m ms) -- record failure and continue
          Just (Right newMgdCap) ->
            return (Just newMgdCap,[],S.insert newMgdCap ms)

    runManaged mcs = case _csScope mcs of -- validate scope
      CapManaged (Just mf) -> case _csCap mcs of -- validate user mg cap and has mgr fun
        UserCapability mqn mas -> case cap of -- validate user test cap
          UserCapability cqn cas
            | cqn == mqn -> -- check type match and apply mgr fun
                Just <$> applyMgrFun mcs mqn mf mas cas
            | otherwise -> noMatch
          _ -> noMatch
        _ -> noMatch
      _ -> noMatch

    applyMgrFun mcs mqn mf mas cas = fmap updateManagedSlot <$> applyF mf mas cas
      where
        -- on success stick updated managed params into slot for reinstall
        updateManagedSlot mas' = set csCap (UserCapability mqn mas') mcs

revokeAllCapabilities :: Eval e ()
revokeAllCapabilities = evalCapabilities .= def

-- | Check signature caps against current granted set.
checkSigCaps
  :: M.Map PublicKey (S.Set Capability)
     -> Eval e (M.Map PublicKey (S.Set Capability))
checkSigCaps sigs = go
  where
    go = do
      granted <- grantedCaps
      return $ M.filter (match granted) sigs

    match granted sigCaps =
      S.null sigCaps ||
      not (S.null (S.intersection granted sigCaps))
