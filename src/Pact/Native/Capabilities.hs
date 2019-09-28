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

module Pact.Native.Capabilities
  ( capDefs
  , evalCap
  ) where

import Control.Lens
import Control.Monad
import Data.Maybe (isJust)

import Pact.Eval
import Pact.Native.Internal
import Pact.RuntimeTypecheck
import Pact.Types.Capability
import Pact.Types.Pretty
import Pact.Types.Runtime


capDefs :: NativeModule
capDefs =
  ("Capabilities",
   [ withCapability
   , installCapability
   , enforceGuardDef "enforce-guard"
   , requireCapability
   , composeCapability
   , createUserGuard
   , createPactGuard
   , createModuleGuard
   , keysetRefGuard
   ])

tvA :: Type n
tvA = mkTyVar "a" []

withCapability :: NativeDef
withCapability =
  defNative (specialForm WithCapability) withCapability'
  (funType tvA [("capability",TyFun $ funType' tTyBool []),("body",TyList TyAny)])
  [LitExample "(with-capability (UPDATE-USERS id) (update users id { salary: new-salary }))"]
  "Specifies and requests grant of _scoped_ CAPABILITY which is an application of a 'defcap' \
  \production. Given the unique token specified by this application, ensure \
  \that the token is granted in the environment during execution of BODY. \
  \'with-capability' can only be called in the same module that declares the \
  \corresponding 'defcap', otherwise module-admin rights are required. \
  \If token is not present, the CAPABILITY is evaluated, with successful completion \
  \resulting in the installation/granting of the token, which will then be revoked \
  \upon completion of BODY. Nested 'with-capability' calls for the same token \
  \will detect the presence of the token, and will not re-apply CAPABILITY, \
  \but simply execute BODY. 'with-capability' cannot be called from within an evaluating defcap."
  where
    withCapability' i [c@TApp{},body@TList{}] = gasUnreduced i [] $ do

      enforceNotWithinDefcap i "with-capability"

      -- evaluate in-module cap
      acquireResult <- evalCap CapCallStack True (_tApp c)

      -- execute scoped code
      r <- reduceBody body

      -- pop if newly acquired
      when (acquireResult == NewlyAcquired) $ popCapStack (const (return ()))

      return r

    withCapability' i as = argsError' i as

installCapability :: NativeDef
installCapability =
  defNative "install-capability" installCapability'
  (funType tTyString
    [("capability",TyFun $ funType' tTyBool [])
    ,("mgr-fun",TyFun mgrFunTy)
    ])
  [LitExample "(install-capability (PAY \"alice\" \"bob\" 10.0) (manage-PAY))"]
  "Specifies, and validates install of, a _managed_ CAPABILITY whose scope is controlled \
  \by MGR-FUN. The type of the objects in the MGR_FUN parameters, C-TYPE, is the name of the \
  \specified 'defcap' of CAPABILITY. The C-TYPE defcap is evaluated to validate the \
  \install of CAPABILITY into the 'managed' runtime c-list. Upon request of a scoped \
  \capability of type C-TYPE using 'with-capability', MGR-FUN is invoked for each capability \
  \in the 'managed' c-list of type C-TYPE: for each, MGR-FUN is called with MANAGED having the \
  \parameters the managed capability, and with REQUESTED having the parameters of the requested \
  \scoped capability. MGR-FUN enforces that the requested capability matches the managed one, \
  \and produces a new managed capability parameter object to replace the previous managed one, \
  \if the desired logic allows it, otherwise it should fail. Upon success, the managed capability \
  \is swapped with the new parameters returned by MGR-FUN, and the requested capability \
  \successfully enters into callstack scope. \
  \Note that signatures that are scoped to a managed capability are only validated upon the \
  \first install of the capability, after which they can no longer be used in the context of that \
  \capability. This ensures that the managed capability can only be installed once (if controlled \
  \by the associated signature[s])."
  where
    ctype = tTyObject (mkSchemaVar "c-type")
    mgrFunTy = funType' ctype [("installed", ctype),("requested", ctype)]

    installCapability' i as = case as of
      [TApp cap _,TApp mgrFun _] -> gasUnreduced i [] $ do

        enforceNotWithinDefcap i "install-capability"

        mfDef <- requireDefApp Defun mgrFun
        defTy <- traverse reduce $ _dFunType mfDef
        typecheckDef mfDef defTy mgrFunTy

        void $ evalCap CapManaged True cap

        return $ tStr $ "Installed capability"

      _ -> argsError' i as


-- | Given cap app, enforce in-module call, eval args to form capability,
-- and attempt to acquire. Return capability if newly-granted. When
-- 'inModule' is 'True', natives can only be run within module scope.
evalCap :: CapScope -> Bool -> App (Term Ref) -> Eval e CapAcquireResult
evalCap scope inModule a@App{..} = do
      (cap,d,prep) <- appToCap a
      when inModule $ guardForModuleCall _appInfo (_dModule d) $ return ()
      acquireCapability scope cap $ do
        g <- computeUserAppGas d _appInfo
        void $ evalUserAppBody d prep _appInfo g reduceBody


enforceNotWithinDefcap :: HasInfo i => i -> Doc -> Eval e ()
enforceNotWithinDefcap i msg = defcapInStack >>= \p -> when p $
  evalError' i $ msg <> " not allowed within defcap execution"

requireCapability :: NativeDef
requireCapability =
  defNative "require-capability" requireCapability'
  (funType tTyBool [("capability",TyFun $ funType' tTyBool [])])
  [LitExample "(require-capability (TRANSFER src dest))"]
  "Specifies and tests for existing grant of CAPABILITY, failing if not found in environment."
  where
    requireCapability' :: NativeFun e
    requireCapability' i [TApp a@App{..} _] = gasUnreduced i [] $ do
      (cap,_,_) <- appToCap a
      enforceNotWithinDefcap i "require-capability"
      granted <- capabilityGranted cap
      unless granted $ evalError' i $ "require-capability: not granted: " <> pretty cap
      return $ toTerm True
    requireCapability' i as = argsError' i as

composeCapability :: NativeDef
composeCapability =
  defNative "compose-capability" composeCapability'
  (funType tTyBool [("capability",TyFun $ funType' tTyBool [])])
  [LitExample "(compose-capability (TRANSFER src dest))"]
  "Specifies and requests grant of CAPABILITY which is an application of a 'defcap' \
  \production, only valid within a (distinct) 'defcap' body, as a way to compose \
  \CAPABILITY with the outer capability such that the scope of the containing \
  \'with-capability' call will \"import\" this capability. Thus, a call to \
  \'(with-capability (OUTER-CAP) OUTER-BODY)', where the OUTER-CAP defcap calls \
  \'(compose-capability (INNER-CAP))', will result in INNER-CAP being granted \
  \in the scope of OUTER-BODY."
  where
    composeCapability' :: NativeFun e
    composeCapability' i [TApp app _] = gasUnreduced i [] $ do
      -- enforce in defcap
      defcapInStack >>= \p -> unless p $ evalError' i "compose-capability valid only within defcap body"
      -- evalCap as composed, which will install onto head of pending cap
      void $ evalCap CapComposed True app
      return $ toTerm True
    composeCapability' i as = argsError' i as

-- | Traverse up the call stack returning 'True' if a containing
-- defcap application is found.
defcapInStack :: Eval e Bool
defcapInStack = isJust <$> preuse (evalCallStack . traverse . sfApp . _Just . _1 . faDefType . _Defcap)



createPactGuard :: NativeDef
createPactGuard =
  defRNative "create-pact-guard" createPactGuard'
  (funType (tTyGuard (Just GTyPact)) [("name",tTyString)])
  []
  "Defines a guard predicate by NAME that captures the results of 'pact-id'. \
  \At enforcement time, the success condition is that at that time 'pact-id' must \
  \return the same value. In effect this ensures that the guard will only succeed \
  \within the multi-transaction identified by the pact id."
  where
    createPactGuard' :: RNativeFun e
    createPactGuard' i [TLitString name] = do
      pid <- getPactId i
      return $ (`TGuard` (_faInfo i)) $ GPact $ PactGuard pid name
    createPactGuard' i as = argsError i as


createModuleGuard :: NativeDef
createModuleGuard =
  defRNative "create-module-guard" createModuleGuard'
  (funType (tTyGuard (Just GTyModule)) [("name",tTyString)])
  []
  "Defines a guard by NAME that enforces the current module admin predicate."
  where
    createModuleGuard' :: RNativeFun e
    createModuleGuard' i [TLitString name] = findCallingModule >>= \m -> case m of
      Just mn ->
        return $ (`TGuard` (_faInfo i)) $ GModule $ ModuleGuard mn name
      Nothing -> evalError' i "create-module-guard: must call within module"
    createModuleGuard' i as = argsError i as

keysetRefGuard :: NativeDef
keysetRefGuard =
  defRNative "keyset-ref-guard" keysetRefGuard'
  (funType (tTyGuard (Just GTyKeySetName)) [("keyset-ref",tTyString)])
  []
  "Creates a guard for the keyset registered as KEYSET-REF with 'define-keyset'. \
  \Concrete keysets are themselves guard types; this function is specifically to \
  \store references alongside other guards in the database, etc."
  where
    keysetRefGuard' :: RNativeFun e
    keysetRefGuard' i [TLitString ks] =
      return $ (`TGuard` (_faInfo i)) $ GKeySetRef (KeySetName ks)
    keysetRefGuard' i as = argsError i as


createUserGuard :: NativeDef
createUserGuard =
  defNative "create-user-guard" createUserGuard'
  (funType (tTyGuard (Just GTyUser)) [("closure",TyFun $ funType' tTyBool [])])
  []
  "Defines a custom guard CLOSURE whose arguments are strictly evaluated at definition time, \
  \to be supplied to indicated function at enforcement time."
  where
    createUserGuard' :: NativeFun e
    createUserGuard' i [TApp App {..} _] = gasUnreduced i [] $ do
      args <- mapM reduce _appArgs
      fun <- case _appFun of
        (TVar (Ref (TDef Def{..} _)) _) -> case _dDefType of
          Defun -> return (QName $ QualifiedName _dModule (asString _dDefName) _dInfo)
          _ -> evalError _appInfo $ "User guard closure must be defun, found: " <> pretty _dDefType
        t -> evalError (_tInfo t) $ "User guard closure function must be def: " <> pretty _appFun
      return $ (`TGuard` (_faInfo i)) $ GUser (UserGuard fun args)
    createUserGuard' i as = argsError' i as
