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

import Control.Monad (void,unless)

import Pact.Eval
import Pact.Native.Internal
import Pact.Types.Runtime
import Data.Default


capDefs :: NativeModule
capDefs =
  ("Capabilities",
   [ withCapability
   , enforceGuard
   ])

tvA :: Type n
tvA = mkTyVar "a" []


withCapability :: NativeDef
withCapability =
  defNative "with-capability" withCapability'
  (funType tvA [("capability",TyFun $ funType' tTyBool []),("body",TyList TyAny)])
  "Specifies and requests grant of CAPABILITY which is an application of a 'defcap' \
   \production; given the unique token specified by this application, ensure \
   \that the token is granted in the environment during execution of BODY. If token is not \
   \present, the CAPABILITY is applied, with \
   \successful completion resulting in the installation/granting of the token, which \
   \will then be revoked upon completion of BODY. \
   \Nested 'with-capability' calls for the same token will detect the presence of \
   \the token, and will not re-apply CAPABILITY, but simply execute BODY. \
   \`$(with capability (update-users id) (update users id { salary: new-salary }))`"
  where
    withCapability' :: NativeFun e
    withCapability' i [c@TApp{},body@TList{}] = gasUnreduced i [] $ do
      grantedCap <- evalCap (_tApp c)
      r <- reduceBody body
      mapM_ revokeCapability grantedCap
      return r
    withCapability' i as = argsError' i as

evalCap :: App (Term Ref) -> Eval e (Maybe Capability)
evalCap App{..} = case _appFun of
  (TVar (Ref (TDef d@Def{..} _)) _) -> case _dDefType of
    Defcap -> do
      prep@(args,_) <- prepareUserAppArgs d _appArgs
      let cap = UserCapability _dDefName args
      acquired <- acquireCapability cap $ do
        g <- computeUserAppGas d _appInfo
        void $ evalUserAppBody d prep _appInfo g reduceBody
      return $ case acquired of
        NewlyAcquired -> Just cap
        AlreadyAcquired -> Nothing
    _ -> evalError _appInfo $ "Can only apply defcap here, found: " ++ show _dDefType
  t -> evalError (_tInfo t) $ "Attempting to apply non-def: " ++ show _appFun


enforceGuard :: NativeDef
enforceGuard =
  defRNative "enforce-guard" enforceGuard'
  (funType tTyBool [("guard",tTyGuard Nothing)])
  "Execute GUARD to enforce whatever predicate is modeled. Failure will \
  \fail the transaction."
  where
    enforceGuard' :: RNativeFun e
    enforceGuard' i [TGuard g _] = do
      case g of
        GKeySet k -> runPure $ enforceKeySet (_faInfo i) Nothing k
        GKeySetRef n -> enforceKeySetName (_faInfo i) n
        GPact PactGuard{..} -> do
          pid <- getPactId i
          unless (pid == _pgPactId) $
            evalError' i $
              "Pact guard failed, intended: " ++ show _pgPactId ++ ", active: " ++ show pid
        GModule mg@ModuleGuard{..} -> do
          m <- getModule (_faInfo i) _mgModuleName
          case m of
            Module{..} -> enforceKeySetName (_faInfo i) _mKeySet
            Interface{} -> evalError' i $ "ModuleGuard not allowed on interface: " ++ show mg
        GUser UserGuard{..} -> do
          void $ enscopeApply $ App (TVar _ugPredFun def) [_ugData] (_faInfo i)
      return $ toTerm True
    enforceGuard' i as = argsError i as
