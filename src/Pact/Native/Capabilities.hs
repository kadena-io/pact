{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Pact.Native.Capabilities
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Builtins for working with capabilities.
--

module Pact.Native.Capabilities (capDefs) where

import Control.Monad
import Pact.Eval
import Pact.Native.Internal
import Pact.Types.Runtime


capDefs :: NativeModule
capDefs =
  ("Capabilities",
   [ withCapability
   , enforceGuardDef "enforce-guard"
   , requireCapability
   ])

tvA :: Type n
tvA = mkTyVar "a" []


withCapability :: NativeDef
withCapability =
  defNative "with-capability" withCapability'
  (funType tvA [("capability",TyFun $ funType' tTyBool []),("body",TyList TyAny)])
  "Specifies and requests grant of CAPABILITY which is an application of a 'defcap' \
  \production. Given the unique token specified by this application, ensure \
  \that the token is granted in the environment during execution of BODY. \
  \'with-capability' can only be called in the same module that declares the \
  \corresponding 'defcap', otherwise module-admin rights are required. \
  \If token is not present, the CAPABILITY is applied, with successful completion \
  \resulting in the installation/granting of the token, which will then be revoked \
  \upon completion of BODY. Nested 'with-capability' calls for the same token \
  \will detect the presence of the token, and will not re-apply CAPABILITY, \
  \but simply execute BODY. \
   \`$(with capability (update-users id) (update users id { salary: new-salary }))`"
  where
    withCapability' :: NativeFun e
    withCapability' i [c@TApp{},body@TList{}] = gasUnreduced i [] $ do
      grantedCap <- evalCap (_tApp c)
      r <- reduceBody body
      mapM_ revokeCapability grantedCap
      return r
    withCapability' i as = argsError' i as

-- | Given cap app, enforce in-module call, eval args to form capability,
-- and attempt to acquire. Return capability if newly-granted.
evalCap :: App (Term Ref) -> Eval e (Maybe Capability)
evalCap a@App{..} = requireDefcap a >>= \d@Def{..} -> do
      guardForModuleCall _appInfo _dModule $ return ()
      prep@(args,_) <- prepareUserAppArgs d _appArgs
      let cap = UserCapability _dDefName args
      acquired <- acquireCapability cap $ do
        g <- computeUserAppGas d _appInfo
        void $ evalUserAppBody d prep _appInfo g reduceBody
      return $ case acquired of
        NewlyAcquired -> Just cap
        AlreadyAcquired -> Nothing

requireDefcap :: App (Term Ref) -> Eval e (Def Term Ref)
requireDefcap App{..} = case _appFun of
  (TVar (Ref (TDef d@Def{..} _)) _) -> case _dDefType of
    Defcap -> return d
    _ -> evalError _appInfo $ "Can only apply defcap here, found: " ++ show _dDefType
  t -> evalError (_tInfo t) $ "Attempting to apply non-def: " ++ show _appFun

requireCapability :: NativeDef
requireCapability =
  defNative "require-capability" requireCapability'
  (funType tTyBool [("capability",TyFun $ funType' tTyBool [])])
  "Specifies and tests for existing grant of CAPABILITY, failing if not found in environment. \
  \`$(require-capability (TRANSFER src dest))`"
  where
    requireCapability' :: NativeFun e
    requireCapability' i [TApp a@App{..} _] = gasUnreduced i [] $ requireDefcap a >>= \d@Def{..} -> do
      (args,_) <- prepareUserAppArgs d _appArgs
      let cap = UserCapability _dDefName args
      granted <- capabilityGranted cap
      unless granted $ evalError' i $ "require-capability: not granted: " ++ show cap
      return $ toTerm True
    requireCapability' i as = argsError' i as
