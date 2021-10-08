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
    ,emitCapability
    ) where

import Control.Monad
import Control.Lens hiding (DefName)
import Data.Default
import Data.Foldable
import Data.List
import Data.Text (Text)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Pact.Types.Capability
import Pact.Types.PactValue
import Pact.Types.Pretty
import Pact.Types.Runtime
import Pact.Runtime.Utils

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
      when ci $ evalError' i $
        "Duplicate install of managed capability " <> pretty cap
      push >> test >> popCapStack installManaged
    go CapCallStack = ifNotAcquired evalStack
    go CapComposed = ifNotAcquired evalComposed


    ifNotAcquired act = do
      ca <- capabilityAcquired cap
      if ca
        then return AlreadyAcquired
        else act >> return NewlyAcquired

    -- managed: assemble managed cap for install.
    -- TODO: given this is install code, why the indirection
    -- 'InstallMgd e'?
    installManaged cs = mkMC >>= install
      where
        install mc = do
          evalCapabilities . capManaged %= S.insert mc
          return (NewlyInstalled mc)
        mkMC = case _dDefMeta cdef of
          Just (DMDefcap (DefcapManaged dcm)) -> case dcm of
            Nothing -> return $!
              ManagedCapability cs (_csCap cs) (Left (AutoManagedCap True))
            Just (argName,mgrFunRef) -> case defCapMetaParts cap argName cdef of
              Left e -> evalError' cdef e
              Right (idx,static,v) -> case mgrFunRef of
                (TVar (Ref (TDef d di)) _) -> case _dDefType d of
                  Defun -> return $!
                    ManagedCapability cs static $
                      Right $ UserManagedCap v idx argName d
                  _ -> evalError' di $ "Capability manager ref must be defun"
                t -> evalError' t $ "Capability manager ref must be a function"
          _ -> evalError' i $
            "Installing managed capability without @managed metadata"

    -- Callstack: check if managed, in which case push/emit,
    -- otherwise push and test.
    evalStack = checkManaged i af cap cdef >>= \r -> case r of
      Nothing -> push >> test >> emitMaybe
      Just composed -> emitCap >> pushSlot (CapSlot scope cap composed)

    -- Composed: check if managed, in which case install onto head/emit,
    -- otherwise push, test, pop and install onto head
    evalComposed = checkManaged i af cap cdef >>= \r -> case r of
      Nothing -> push >> test >> emitMaybe >> popCapStack installComposed
      Just composed -> emitCap >> installComposed (CapSlot scope cap composed)

    installComposed c =
      evalCapabilities . capStack . _head . csComposed <>= (_csCap c:_csComposed c)

    push = pushSlot (CapSlot scope cap [])

    emitCap = emitCapability i cap

    emitMaybe =
      when (_dDefMeta cdef == Just (DMDefcap DefcapEvent)) emitCap

    pushSlot s = evalCapabilities . capStack %= (s:)

emitCapability :: HasInfo i => i -> UserCapability -> Eval e ()
emitCapability i cap = emitEvent i (_scName cap) (_scArgs cap)

defCapMetaParts :: UserCapability -> Text -> Def Ref
                -> Either Doc (Int, SigCapability, PactValue)
defCapMetaParts cap argName cdef = case findArg argName of
  Nothing -> Left $ "Invalid managed argument name: " <> pretty argName
  Just idx -> case decomposeManaged' idx cap of
    Nothing -> Left $ "Missing argument index from capability: " <> pretty idx
    Just (static,v) -> return (idx,static,v)
  where
    findArg an = findIndex ((==) an . _aName) $ _ftArgs (_dFunType cdef)

-- Check managed state, if any, to approve acquisition.
-- Handles lazy installation of sig + auto caps, as a fallback
-- case if no matching installed managed caps are found.
-- Once found/matched, compute installed logic to approve acquisition.
-- Upon success return composed caps that were assembled during install
-- to copy into acquired slot.
checkManaged
  :: HasInfo i
  => i
  -> (ApplyMgrFun e,InstallMgd e)
  -> UserCapability
  -> Def Ref
  -> Eval e (Maybe [UserCapability])
checkManaged i (applyF,installF) cap@SigCapability{} cdef = case _dDefMeta cdef of
  -- managed: go
  Just (DMDefcap dcm@DefcapManaged {}) ->
    use (evalCapabilities . capManaged) >>= go dcm . S.toList
  -- otherwise noop
  _ -> return Nothing

  where
    -- go: main loop over installed managed caps set
    -- empty case: attempt lazy install and test
    go dcm [] = do
      checkSigs dcm >>= \r -> case r of
        Nothing -> die
        Just mc -> testMC mc die
    -- test installed from set
    go dcm (mc:mcs) = testMC mc (go dcm mcs)

    die = evalError' i $ "Managed capability not installed: " <> pretty cap

    -- test an already-installed mgd cap by executing mgmt functionality
    testMC (mc@ManagedCapability{..}) cont = case _mcManaged of
      Left oneShot | cap == _mcStatic -> doOneShot mc oneShot
                   | otherwise -> cont
      Right umc@UserManagedCap{..} ->
        case decomposeManaged' _umcManageParamIndex cap of
          Nothing -> cont
          Just (cap',rv)
            | cap' /= _mcStatic -> cont
            | otherwise -> check mc umc rv

    doOneShot mc (AutoManagedCap True) = do
      evalCapabilities . capManaged %=
        S.insert (set (mcManaged . _Left . amcActive) False mc)
      return $ Just $ _csComposed (_mcInstalled mc)
    doOneShot _mc (AutoManagedCap False) = evalError' i $ "Capability already fired"

    -- execute manager function and compute/store result
    check mc@ManagedCapability{..} umc@UserManagedCap{..} rv = do
      newMgdValue <- applyF _umcMgrFun _umcManagedValue rv
      let newUmc = set umcManagedValue newMgdValue umc
      evalCapabilities . capManaged %= S.insert (set mcManaged (Right newUmc) mc)
      return $ Just $ _csComposed _mcInstalled

    getStatic (DefcapManaged dcm) c = case dcm of
      Nothing -> return c
      Just (argName,_) -> view _2 <$> defCapMetaParts c argName cdef
    getStatic DefcapEvent c = return c

    -- check sig and autonomous caps for match
    -- to install.
    checkSigs dcm = case getStatic dcm cap of
      Left e -> evalError' cdef e
      Right capStatic -> do
        autos <- use $ evalCapabilities . capAutonomous
        sigCaps <- (S.union autos . S.unions) <$> view eeMsgSigs
        foldM (matchSig dcm capStatic) Nothing sigCaps

    matchSig _ _ r@Just{} _ = return r
    matchSig dcm capStatic Nothing sigCap = case getStatic dcm sigCap of
      Left _ -> return Nothing
      Right sigStatic | sigStatic == capStatic -> Just <$> doMgdInstall sigCap
                      | otherwise -> return Nothing

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
      autos <- use $ evalCapabilities . capAutonomous
      return $ M.filter (match (S.null autos) granted) sigs

    match allowEmpty granted sigCaps =
      (S.null sigCaps && allowEmpty) ||
      not (S.null (S.intersection granted sigCaps))
