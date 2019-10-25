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
    (evalUserCapability
    ,acquireModuleAdminCapability
    ,popCapStack
    ,revokeAllCapabilities
    ,capabilityAcquired
    ,ApplyMgrFun
    ,InstallMgd
    ,checkSigCaps
    ) where

import Control.Monad
import Control.Lens hiding (DefName)
import Data.Default
import Data.Foldable
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Pact.Types.Capability
import Pact.Types.PactValue
import Pact.Types.Pretty
import Pact.Types.Runtime

-- | Tie the knot with Pact.Eval by having caller supply `apply` etc
type ApplyMgrFun e = Def Ref -> PactValue -> PactValue -> Eval e PactValue
-- | More knot tying to on-demand install a managed cap
type InstallMgd e = UserCapability -> Def Ref -> Eval e (ManagedCapability UserCapability)


-- | Check for acquired/stack (or composed therein) capability.
capabilityAcquired :: UserCapability -> Eval e Bool
capabilityAcquired cap = elem cap <$> getAllStackCaps

-- | Check for managed cap installed.
capabilityInstalled :: UserCapability -> Eval e Bool
capabilityInstalled cap = any (`matchManaged` cap) <$> use (evalCapabilities . capManaged)

getAllStackCaps :: Eval e (S.Set UserCapability)
getAllStackCaps = S.fromList . concatMap toList <$> use (evalCapabilities . capStack)

popCapStack :: (CapSlot UserCapability -> Eval e a) -> Eval e a
popCapStack act = do
  s <- use $ evalCapabilities . capStack
  case s of
    [] -> evalError def "acquireCapability: unexpected error: empty stack"
    (c:cs) -> do
      evalCapabilities . capStack .= cs
      act c

acquireModuleAdminCapability
  :: ModuleName -> Eval e () -> Eval e CapEvalResult
acquireModuleAdminCapability mc test = do
  prev <- preuse $ evalCapabilities . capModuleAdmin . ix mc
  case prev of
    Just {} -> return AlreadyAcquired
    Nothing -> do
      test
      evalCapabilities . capModuleAdmin %= S.insert mc
      return NewlyAcquired

-- | Test if capability is already installed, if not
-- evaluate `test` which is expected to fail by some
-- guard throwing a failure. Upon successful return of
-- `test` install capability.
evalUserCapability
  :: HasInfo i
  => i
  -> (ApplyMgrFun e,InstallMgd e)
  -- ^ knot-tying continuations
  -> CapScope
  -- ^ acquiring/installing scope
  -> UserCapability
  -- ^ acquiring/installing cap
  -> Def Ref
  -- ^ cap definition
  -> Eval e ()
  -- ^ test to validate install
  -> Eval e CapEvalResult
evalUserCapability i af scope cap cdef test = go scope
  where

    go CapManaged = do
      ci <- capabilityInstalled cap
      when ci $ evalError' i $ "Duplicate install of managed capability " <> pretty cap
      push >> test >> popCapStack installManaged
    go CapCallStack = ifNotAcquired evalStack
    go CapComposed = ifNotAcquired evalComposed


    ifNotAcquired act = do
      ca <- capabilityAcquired cap
      if ca
        then return AlreadyAcquired
        else act >> return NewlyAcquired


    installManaged cs = mkMC >>= install
      where
        install mc = do
          evalCapabilities . capManaged %= S.insert mc
          return (NewlyInstalled mc)
        mkMC = case _dDefMeta cdef of
          Nothing -> evalError' i $ "Installing managed capability without @managed metadata"
          Just (DMDefcap dcm@(DefcapMeta mgrFunRef argName)) -> do
            (idx,static,v) <- defCapMetaParts i cap dcm cdef
            case mgrFunRef of
              (TVar (Ref (TDef d di)) _) -> case _dDefType d of
                Defun -> return $! ManagedCapability cs static v idx argName d
                _ -> evalError' di $ "Capability manager ref must be defun"
              t -> evalError' t $ "Capability manager ref must be a function"

    -- Callstack: check if managed, in which case push, otherwise
    -- push and test.
    evalStack = checkManaged i af cap cdef >>= \r -> case r of
      Nothing -> push >> test
      Just composed -> pushSlot (CapSlot scope cap composed)

    -- Composed: check if managed, in which case install onto head,
    -- otherwise push, test, pop and install onto head
    evalComposed = checkManaged i af cap cdef >>= \r -> case r of
      Nothing -> push >> test >> popCapStack installComposed
      Just composed -> installComposed (CapSlot scope cap composed)

    installComposed c = evalCapabilities . capStack . _head . csComposed %= (_csCap c:)

    push = pushSlot (CapSlot scope cap [])

    pushSlot s = evalCapabilities . capStack %= (s:)

defCapMetaParts :: HasInfo i => i -> UserCapability -> DefcapMeta a -> Def Ref -> Eval e (Int, SigCapability, PactValue)
defCapMetaParts i cap (DefcapMeta _ argName) cdef = case findArg argName of
  Nothing ->
    evalError' cdef $ "Invalid managed argument name: " <> pretty argName
  Just idx -> case decomposeManaged' idx cap of
    Nothing -> evalError' i $ "Missing argument index from capability: " <> pretty idx
    Just (static,v) -> return (idx,static,v)
  where
    findArg an = foldl' (matchArg an) Nothing (zip [0..] $ _ftArgs $ _dFunType cdef)
    matchArg _ (Just idx) _ = Just idx
    matchArg an Nothing (idx,Arg{..}) | _aName == an = Just idx
                                      | otherwise = Nothing

checkManaged
  :: HasInfo i
  => i
  -> (ApplyMgrFun e,InstallMgd e)
  -> UserCapability
  -> Def Ref
  -> Eval e (Maybe [UserCapability])
checkManaged i (applyF,installF) cap@SigCapability{..} cdef = case _dDefMeta cdef of
  Nothing -> return Nothing
  Just (DMDefcap dcm) -> use (evalCapabilities . capManaged) >>= go dcm . S.toList
  where
    go dcm [] = do
      checkSigs dcm >>= \r -> case r of
        Nothing -> die
        Just mc -> testMC mc die
    go dcm (mc:mcs) = testMC mc (go dcm mcs)

    die = evalError' i $ "Managed capability not installed: " <> pretty cap

    testMC (mc@ManagedCapability{..}) cont = case decomposeManaged' _mcManageParamIndex cap of
      Nothing -> cont
      Just (cap',rv)
        | cap' /= _mcStatic -> cont
        | otherwise -> check mc rv

    check mc@ManagedCapability{..} rv = do
      newMgdValue <- applyF _mcMgrFun _mcManaged rv
      evalCapabilities . capManaged %= S.insert (set mcManaged newMgdValue mc)
      return $ Just $ _csComposed _mcInstalled

    getStatic dcm c = defCapMetaParts i c dcm cdef >>= \r -> case r of
      (_,s,_) -> return s

    checkSigs dcm = do
      capStatic <- getStatic dcm cap
      sigCaps <- foldl1 (<>) <$> view eeMsgSigs
      foldrM (matchSig dcm capStatic) Nothing sigCaps

    matchSig _ _ _ r@Just{} = return r
    matchSig dcm capStatic sigCap Nothing = do
      sigStatic <- getStatic dcm sigCap
      if sigStatic == capStatic
        then Just <$> doMgdInstall sigCap
        else return Nothing

    doMgdInstall sigCap = installF sigCap cdef


revokeAllCapabilities :: Eval e ()
revokeAllCapabilities = evalCapabilities .= def

-- | Check signature caps against current granted set.
checkSigCaps
  :: M.Map PublicKey (S.Set UserCapability)
     -> Eval e (M.Map PublicKey (S.Set UserCapability))
checkSigCaps sigs = go
  where
    go = do
      granted <- getAllStackCaps
      return $ M.filter (match granted) sigs

    match granted sigCaps =
      S.null sigCaps ||
      not (S.null (S.intersection granted sigCaps))
