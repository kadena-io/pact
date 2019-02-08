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

-- TODO This is to hide a warning involving `enforceKeySet`, which has a typeclass
-- constraint unused in the function itself, but is critical for preventing misuse
-- by a caller. There is probably a better way to enforce this restriction,
-- allowing us to remove this warning suppression.
-- See: https://github.com/kadena-io/pact/pull/206/files#r215468087
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

-- |
-- Module      :  Pact.Eval
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Pact interpreter.
--

module Pact.Eval
    (eval
    ,evalBeginTx,evalRollbackTx,evalCommitTx
    ,reduce,reduceBody
    ,resolveFreeVars,resolveArg,resolveRef
    ,enforceKeySet,enforceKeySetName
    ,checkUserType
    ,deref
    ,installModule
    ,runPure,runReadOnly,Purity
    ,liftTerm,apply
    ,preGas
    ,acquireCapability,acquireModuleAdmin
    ,capabilityGranted
    ,revokeCapability,revokeAllCapabilities
    ,computeUserAppGas,prepareUserAppArgs,evalUserAppBody
    ,enscopeApply
    ) where

import Control.Lens
import Control.Monad.IO.Class
import Control.Applicative
import Control.Monad.Catch (throwM)
import Data.List
import Control.Monad
import Prelude
import Bound
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import Safe
import Data.Default
import Control.Arrow hiding (app)
import Data.Maybe
import Data.Foldable
import Data.Graph
import qualified Data.Set as S
import Control.Monad.State.Strict
import Control.Monad.Reader
import Unsafe.Coerce
import Data.Aeson (Value)
import Text.PrettyPrint.ANSI.Leijen (Pretty(pretty), Doc, dquotes, line, text, parens)

import Pact.Types.Runtime
import Pact.Gas


evalBeginTx :: Info -> Eval e ()
evalBeginTx i = revokeAllCapabilities >> view eeTxId >>= beginTx i
{-# INLINE evalBeginTx #-}

evalRollbackTx :: Info -> Eval e ()
evalRollbackTx i = revokeAllCapabilities >> void (rollbackTx i)
{-# INLINE evalRollbackTx #-}

evalCommitTx :: Info -> Eval e [TxLog Value]
evalCommitTx i = do
  revokeAllCapabilities
  tid <- view eeTxId
  case tid of
    Nothing -> evalRollbackTx i >> return []
    Just {} -> commitTx i
{-# INLINE evalCommitTx #-}

enforceKeySetName :: Info -> KeySetName -> Eval e ()
enforceKeySetName mi mksn = do
  ks <- maybe (evalError mi $ "No such keyset: " <> pretty mksn) return =<< readRow mi KeySets mksn
  runPure $ enforceKeySet mi (Just mksn) ks
{-# INLINE enforceKeySetName #-}

-- | Enforce keyset against environment
enforceKeySet :: PureNoDb e => Info ->
             Maybe KeySetName -> KeySet -> Eval e ()
enforceKeySet i ksn KeySet{..} = do
  sigs <- view eeMsgSigs
  let count = length _ksKeys
      matched = S.size $ S.intersection (S.fromList _ksKeys) sigs
      failed = failTx i $ "Keyset failure " <> parens (pretty _ksPredFun) <>
        maybe "" (\ksn' -> ": " <> pretty ksn') ksn
      runBuiltIn p | p count matched = return ()
                   | otherwise = failed
      atLeast t m = m >= t
  case M.lookup _ksPredFun keyPredBuiltins of
    Just KeysAll -> runBuiltIn (\c m -> atLeast c m)
    Just KeysAny -> runBuiltIn (\_ m -> atLeast 1 m)
    Just Keys2 -> runBuiltIn (\_ m -> atLeast 2 m)
    Nothing -> do
      r <- enscopeApply $
        App (TVar _ksPredFun def) [toTerm count,toTerm matched] i
      case r of
        (TLiteral (LBool b) _) | b -> return ()
                               | otherwise -> failTx i $ "Keyset failure: " <>
                                   maybe "[dynamic]" pretty ksn
        _ -> evalError i $ "Invalid response from keyset predicate: " <> pretty r
{-# INLINE enforceKeySet #-}


-- Hoist Name back to ref
liftTerm :: Term Name -> Term Ref
liftTerm a = TVar (Direct a) def

enscopeApply :: App (Term Name) -> Eval e (Term Name)
enscopeApply a = do
  a' <- enscope $ TApp a (_appInfo a)
  reduce a'

-- | Application with additional args.
apply :: App (Term Ref) -> [Term Name] -> Eval e (Term Name)
apply app as = reduceApp $ over appArgs (++ map liftTerm as) app

-- | Precompute gas on unreduced args returning reduced values.
preGas :: FunApp -> [Term Ref] -> Eval e (Gas,[Term Name])
preGas i as =
  computeGas (Right i) (GUnreduced as) >>= \g -> (g,) <$> mapM reduce as

topLevelCall
  :: Info -> Text -> GasArgs -> (Gas -> Eval e (Gas, a)) -> Eval e a
topLevelCall i name gasArgs action = call (StackFrame name i Nothing) $
  computeGas (Left (i,name)) gasArgs >>= action

capabilityGranted :: Capability -> Eval e Bool
capabilityGranted cap = (cap `elem`) <$> use (evalCapabilities . capGranted)

-- | Test if capability is already installed, if not
-- evaluate `test` which is expected to fail by some
-- guard throwing a failure. Upon successful return of
-- `test` install capability.
acquireCapability :: Capability -> Eval e () -> Eval e CapAcquireResult
acquireCapability cap test = do
  granted <- capabilityGranted cap
  if granted then return AlreadyAcquired else do
    test
    evalCapabilities . capGranted %= (cap:)
    return NewlyAcquired

acquireModuleAdmin :: Info -> ModuleName -> KeySetName -> Eval e CapAcquireResult
acquireModuleAdmin i modName modKeySetName =
  acquireCapability moduleAdminCapability $ enforceKeySetName i modKeySetName
  where moduleAdminCapability = ModuleAdminCapability modName


revokeAllCapabilities :: Eval e ()
revokeAllCapabilities = evalCapabilities . capGranted .= []

revokeCapability :: Capability -> Eval e ()
revokeCapability c = evalCapabilities . capGranted %= filter (/= c)

-- | Evaluate current namespace and prepend namespace to the
-- module name. This should be done before any lookups, as
-- 'namespace.modulename' is the name we will associate
-- with a module unless the namespace policy is defined
-- otherwise
evalNamespace :: Info -> Module -> Eval e Module
evalNamespace info m = do
  mNs <- use $ evalRefs . rsNamespace
  case mNs of
    Nothing -> do
      policy <- view (eeNamespacePolicy . nsPolicy)
      unless (policy mNs) $ evalError info "Definitions in default namespace are not authorized"
      return m
    Just (Namespace n _) ->
      return $ over (case m of Module {} -> mName; _ -> interfaceName) (mangleModuleName n) m
  where
    mangleModuleName :: NamespaceName -> ModuleName -> ModuleName
    mangleModuleName n mn@(ModuleName nn ns) =
      case ns of
        Nothing -> ModuleName nn (Just n)
        Just {} -> mn

-- | Evaluate top-level term.
eval ::  Term Name ->  Eval e (Term Name)
eval (TUse u@Use{..} i) = topLevelCall i "use" (GUse _uModuleName _uModuleHash) $ \g ->
  evalUse u >> return (g,tStr $ pack $ "Using " ++ renderCompactString _uModuleName)
eval (TModule m@Module{} bod i) =
  topLevelCall i "module" (GModuleDecl m) $ \g0 -> do
    -- prepend namespace def to module name
    mangledM <- evalNamespace i m
    -- enforce old module keysets
    oldM <- readRow i Modules $ _mName mangledM
    case oldM of
      Nothing -> return ()
      Just om ->
        case om of
          Module{} -> void $ acquireModuleAdmin i (_mName om) (_mKeySet om)
          Interface{..} -> evalError i $
            "Name overlap: module " <> pretty (_mName om) <> " overlaps with interface  " <> pretty _interfaceName
    -- enforce new module keyset
    enforceKeySetName i (_mKeySet mangledM)
    -- build/install module from defs
    g <- loadModule mangledM bod i g0
    writeRow i Write Modules (_mName mangledM) mangledM
    return (g, msg $ "Loaded module " <> pretty (_mName mangledM) <> ", hash " <> pretty (_mHash mangledM))
eval (TModule m@Interface{} bod i) =
  topLevelCall i "interface" (GInterfaceDecl m) $ \gas -> do
     -- prepend namespace def to module name
    mangledI <- evalNamespace i m
    -- enforce old module keysets
    oldI <- readRow i Modules $ _interfaceName mangledI
    case oldI of
      Nothing -> return ()
      Just oi ->
        case oi of
          Module{..} -> evalError i $
            "Name overlap: interface " <> pretty (_interfaceName m) <>
            " overlaps with module " <> pretty _mName
          Interface{..} -> return ()
    g <- loadModule mangledI bod i gas
    writeRow i Write Modules (_interfaceName mangledI) mangledI
    return (g, msg $ "Loaded interface " <> pretty (_interfaceName mangledI))
eval t = enscope t >>= reduce

evalUse :: Use -> Eval e ()
evalUse (Use mn h i) = do
  mm <- resolveName mn
  case mm of
    Nothing -> evalError i $ "Module " <> pretty mn <> " not found"
    Just md -> do
      case view mdModule md of
        Module{..} ->
          case h of
            Nothing -> return ()
            Just mh | mh == _mHash -> return ()
                    | otherwise -> evalError i $ "Module " <>
                        pretty mn <> " does not match specified hash: " <>
                        pretty mh <> ", " <> pretty _mHash
        Interface{..} ->
          case h of
            Nothing -> return ()
            Just _ -> evalError i $
              "Interfaces should not have associated hashes: " <>
              pretty _interfaceName

      installModule md

mangleDefs :: ModuleName -> Term Name -> Term Name
mangleDefs mn term = modifyMn term
  where
    modifyMn = case term of
      TDef{}    -> set (tDef . dModule) mn
      TConst{}  -> set tModule mn
      TSchema{} -> set tModule mn
      TTable{}  -> set tModule mn
      _         -> id

-- | Make table of module definitions for storage in namespace/RefStore.
loadModule :: Module -> Scope n Term Name -> Info -> Gas -> Eval e Gas
loadModule m@Module{..} bod1 mi g0 = do
  (g1,mdefs) <-
    case instantiate' bod1 of
      (TList bd _ _bi) -> do
        let doDef (g,rs) t = do
              dnm <- case t of
                TDef {..} -> return $ Just $ asString (_dDefName _tDef)
                TConst {..} -> return $ Just $ _aName _tConstArg
                TSchema {..} -> return $ Just $ asString _tSchemaName
                TTable {..} -> return $ Just $ asString _tTableName
                TUse (Use {..}) _ -> return Nothing
                _ -> evalError (_tInfo t) "Invalid module member"
              case dnm of
                Nothing -> return (g, rs)
                Just dn -> do
                  g' <- computeGas (Left (_tInfo t,dn)) (GModuleMember m)
                  return (g + g',(dn,t):rs)
        second HM.fromList <$> foldM doDef (g0,[]) bd
      t -> evalError (_tInfo t) "Malformed module"
  mapM_ evalUse _mImports
  evaluatedDefs <- evaluateDefs mi (fmap (mangleDefs _mName) mdefs)
  (m', solvedDefs) <- evaluateConstraints mi m evaluatedDefs
  let md = ModuleData m' solvedDefs
  installModule md
  (evalRefs . rsNewModules) %= HM.insert _mName md
  return g1
loadModule i@Interface{..} body info gas0 = do
  (gas1,idefs) <- case instantiate' body of
    (TList bd _ _bi) -> do
      let doDef (g,rs) t = do
            dnm <- case t of
              TDef {..} -> return $ Just $ asString (_dDefName _tDef)
              TConst {..} -> return $ Just $ _aName _tConstArg
              TSchema {..} -> return $ Just $ asString _tSchemaName
              TUse (Use {..}) _ -> return Nothing
              _ -> evalError (_tInfo t) "Invalid interface member"
            case dnm of
              Nothing -> return (g, rs)
              Just dn -> do
                g' <- computeGas (Left (_tInfo t,dn)) (GModuleMember i)
                return (g + g',(dn,t):rs)
      second HM.fromList <$> foldM doDef (gas0,[]) bd
    t -> evalError (_tInfo t) "Malformed interface"
  mapM_ evalUse _interfaceImports
  evaluatedDefs <- evaluateDefs info (fmap (mangleDefs _interfaceName) idefs)
  let md = ModuleData i evaluatedDefs
  installModule md
  (evalRefs . rsNewModules) %= HM.insert _interfaceName md
  return gas1

-- | Definitions are transformed such that all free variables are resolved either to
-- an existing ref in the refstore/namespace ('Right Ref'), or a symbol that must
-- resolve to a definition in the module ('Left String'). A graph is formed from
-- all 'Left String' entries and enforced as acyclic, proving the definitions
-- to be non-recursive. The graph is walked to unify the Either to
-- the 'Ref's it already found or a fresh 'Ref' that will have already been added to
-- the table itself: the topological sort of the graph ensures the reference will be there.
evaluateDefs :: Info -> HM.HashMap Text (Term Name) -> Eval e (HM.HashMap Text Ref)
evaluateDefs info defs = do
  cs <- traverseGraph defs
  sortedDefs <- forM cs $ \c ->
      case c of
        AcyclicSCC v -> return v
        CyclicSCC vs -> evalError (if null vs then info else _tInfo $ view _1 $ head vs) $
          "Recursion detected: " <> pretty (vs & traverse . _1 %~ fmap (either pretty pretty))
  let dresolve ds (d,dn,_) = HM.insert dn (Ref $ unify ds <$> d) ds
      unifiedDefs = foldl dresolve HM.empty sortedDefs
  traverse (runPure . evalConsts) unifiedDefs

traverseGraph :: HM.HashMap Text (Term Name) -> Eval e [SCC (Term (Either Text Ref), Text, [Text])]
traverseGraph defs = fmap stronglyConnCompR $ forM (HM.toList defs) $ \(dn,d) -> do
  d' <- forM d $ \(f :: Name) -> do
    dm <- resolveRef f
    case (dm, f) of
      (Just t, _) -> return (Right t)
      (Nothing, Name fn _) ->
        case HM.lookup fn defs of
          Just _ -> return (Left fn)
          Nothing -> evalError (_nInfo f) $ "Cannot resolve " <> dquotes (pretty f)
      (Nothing, _) -> evalError (_nInfo f) $ "Cannot resolve " <> dquotes (pretty f)
  return (d', dn, mapMaybe (either Just (const Nothing)) $ toList d')

-- | Evaluate interface constraints in module.
evaluateConstraints
  :: Info
  -> Module
  -> HM.HashMap Text Ref
  -> Eval e (Module, HM.HashMap Text Ref)
evaluateConstraints info Interface{} _ =
  evalError info "Unexpected: interface found in module position while solving constraints"
evaluateConstraints info m evalMap =
  foldM evaluateConstraint (m, evalMap) $ _mInterfaces m
  where
    evaluateConstraint (m', refMap) ifn = do
      refData <- resolveName ifn
      case refData of
        Nothing -> evalError info $ "Interface not defined: " <> pretty ifn
        Just (ModuleData Interface{..} irefs) -> do
          em' <- HM.foldrWithKey (solveConstraint info) (pure refMap) irefs
          let um = over mMeta (<> _interfaceMeta) m'
          pure (um, em')
        Just _ -> evalError info "Unexpected: module found in interface position while solving constraints"

-- | Compare implemented member signatures with their definitions.
-- At this stage, we have not merged consts, so we still check for overlap
solveConstraint
  :: Info
  -> Text
  -> Ref
  -> Eval e (HM.HashMap Text Ref)
  -> Eval e (HM.HashMap Text Ref)
solveConstraint info refName (Direct t) _ =
  evalError info $ "found native reference " <> pretty t <> " while resolving module contraints: " <> pretty refName
solveConstraint info refName (Ref t) evalMap = do
  em <- evalMap
  case HM.lookup refName em of
    Nothing ->
      case t of
        TConst{..} -> evalMap
        _ -> evalError info $ "found unimplemented member while resolving model constraints: " <> pretty refName
    Just (Direct s) ->
      evalError info $ "found native reference " <> pretty s <> " while resolving module contraints: " <> pretty t
    Just (Ref s) ->
      case (t, s) of
        (TDef (Def _n _mn dt (FunType args rty) _ m _) _,
          TDef (Def _n' _mn' dt' (FunType args' rty') _ _ _) _) -> do
          when (dt /= dt') $ evalError info $ "deftypes mismatching: " <> pretty dt <> line <> pretty dt'
          when (rty /= rty') $ evalError info $ "return types mismatching: " <> pretty rty <> line <> pretty rty'
          when (length args /= length args') $ evalError info $ "mismatching argument lists: " <> pretty args <> line <> pretty args'
          forM_ (args `zip` args') $ \((Arg n ty _), (Arg n' ty' _)) -> do
            when (n /= n') $ evalError info $ "mismatching argument names: " <> pretty n <> " and " <> pretty n'
            when (ty /= ty') $ evalError info $ "mismatching types: " <> pretty ty <> " and " <> pretty ty'
          -- the model concatenation step: we reinsert the ref back into the map with new models
          pure $ HM.insert refName (Ref $ over (tDef . dMeta) (<> m) s) em
        _ -> evalError info $ "found overlapping const refs - please resolve: " <> pretty t

resolveName :: ModuleName -> Eval e (Maybe ModuleData)
resolveName mn = do
  md <- preview $ eeRefStore . rsModules . ix mn
  case md of
    Just _ -> return md
    Nothing -> do
      case (_mnNamespace mn) of
        Just {} -> pure Nothing -- explicit namespace not found
        Nothing -> do
          mNs <- use $ evalRefs . rsNamespace
          case mNs of
            Just ns -> preview $ eeRefStore . rsModules . ix (set mnNamespace (Just . _nsName $ ns) mn)
            Nothing -> pure Nothing

resolveRef :: Name -> Eval e (Maybe Ref)
resolveRef (QName q n _) = do
  let lookupQn q' n' = preview $ eeRefStore . rsModules . ix q' . mdRefMap . ix n'
  dsm <- lookupQn q n
  case dsm of
    d@Just {} -> return d
    Nothing -> do
      case (_mnNamespace q) of
        Just {} -> pure Nothing -- explicit namespace not found
        Nothing -> do
          mNs <- use $ evalRefs . rsNamespace
          case mNs of
            Just ns -> lookupQn (set mnNamespace (Just $ _nsName ns) q) n
            Nothing -> pure Nothing -- no explicit namespace or decalared namespace
resolveRef nn@(Name _ _) = do
  nm <- preview $ eeRefStore . rsNatives . ix nn
  case nm of
    d@Just {} -> return d
    Nothing -> preview (evalRefs . rsLoaded . ix nn) <$> get

-- | This should be impure. See 'evaluateDefs'. Refs are
-- expected to exist, and if they don't, it is a serious bug
unify :: HM.HashMap Text Ref -> Either Text Ref -> Ref
unify _ (Right r) = r
unify m (Left t) = m HM.! t

evalConsts :: PureNoDb e => Ref -> Eval e Ref
evalConsts (Ref r) = case r of
  c@TConst {..} -> case _tConstVal of
    CVRaw raw -> do
      v <- reduce =<< traverse evalConsts raw
      traverse reduce _tConstArg >>= \a -> typecheck [(a,v)]
      return $ Ref (TConst _tConstArg _tModule (CVEval raw $ liftTerm v) _tMeta _tInfo)
    _ -> return $ Ref c
  _ -> Ref <$> traverse evalConsts r
evalConsts r = return r


deref :: Ref -> Eval e (Term Name)
deref (Direct n) = return n
deref (Ref r) = reduce r

-- | Only can be used by "static" terms with no refs/variables in them
unsafeReduce :: Term Ref -> Eval e (Term Name)
unsafeReduce t = return (t >>= const (tStr "Error: unsafeReduce on non-static term"))


-- | Main function for reduction/evaluation.
reduce :: Term Ref ->  Eval e (Term Name)
reduce (TApp a _) = reduceApp a
reduce (TVar t _) = deref t
reduce t@TLiteral {} = unsafeReduce t
reduce t@TGuard {} = unsafeReduce t
reduce t@TValue {} = unsafeReduce t
reduce TList {..} = TList <$> mapM reduce _tList <*> traverse reduce _tListType <*> pure _tInfo
reduce t@TDef {} = return $ toTerm $ pack $ show t
reduce t@TNative {} = return $ toTerm $ pack $ show t
reduce TConst {..} = case _tConstVal of
  CVEval _ t -> reduce t
  CVRaw a -> evalError _tInfo $ "internal error: reduce: unevaluated const: " <> pretty a
reduce (TObject ps t i) =
  TObject <$> forM ps (\(k,v) -> (,) <$> reduce k <*> reduce v) <*> traverse reduce t <*> pure i
reduce (TBinding ps bod c i) = case c of
  BindLet -> reduceLet ps bod i
  BindSchema _ -> evalError i "Unexpected schema binding"
reduce t@TModule{} = evalError (_tInfo t) "Modules and Interfaces only allowed at top level"
reduce t@TUse {} = evalError (_tInfo t) "Use only allowed at top level"
reduce t@TStep {} = evalError (_tInfo t) "Step at invalid location"
reduce TSchema {..} = TSchema _tSchemaName _tModule _tMeta <$> traverse (traverse reduce) _tFields <*> pure _tInfo
reduce TTable {..} = TTable _tTableName _tModule _tHash <$> mapM reduce _tTableType <*> pure _tMeta <*> pure _tInfo

mkDirect :: Term Name -> Term Ref
mkDirect = (`TVar` def) . Direct

reduceBody :: Term Ref -> Eval e (Term Name)
reduceBody (TList bs _ _) = last <$> mapM reduce bs
reduceBody t = evalError (_tInfo t) "Expected body forms"

reduceLet :: [(Arg (Term Ref),Term Ref)] -> Scope Int Term Ref -> Info -> Eval e (Term Name)
reduceLet ps bod i = do
  ps' <- mapM (\(a,t) -> (,) <$> traverse reduce a <*> reduce t) ps
  typecheck ps'
  reduceBody (instantiate (resolveArg i (map (mkDirect . snd) ps')) bod)


{-# INLINE resolveArg #-}
resolveArg :: Info -> [Term n] -> Int -> Term n
resolveArg ai as i = fromMaybe (appError ai $ "Missing argument value at index " <> pretty i) $
                     as `atMay` i

appCall :: Show t => FunApp -> Info -> [Term t] -> Eval e (Gas,a) -> Eval e a
appCall fa ai as = call (StackFrame (_faName fa) ai (Just (fa,map (pack.abbrev) as)))

reduceApp :: App (Term Ref) -> Eval e (Term Name)
reduceApp (App (TVar (Direct t) _) as ai) = reduceDirect t as ai
reduceApp (App (TVar (Ref r) _) as ai) = reduceApp (App r as ai)
reduceApp (App (TDef d@Def{..} _) as ai) = do
  g <- computeUserAppGas d ai
  af <- prepareUserAppArgs d as
  evalUserAppBody d af ai g $ \bod' ->
    case _dDefType of
      Defun -> reduceBody bod'
      Defpact -> applyPact bod'
      Defcap -> evalError ai "Cannot directly evaluate defcap"
reduceApp (App (TLitString errMsg) _ i) = evalError i $ text $ unpack errMsg
reduceApp (App r _ ai) = evalError ai $ "Expected def: " <> pretty r

-- | precompute "UserApp" cost
computeUserAppGas :: Def Ref -> Info -> Eval e Gas
computeUserAppGas Def{..} ai = computeGas (Left (ai, asString _dDefName)) GUserApp

-- | prepare reduced args and funtype, and typecheck
prepareUserAppArgs :: Def Ref -> [Term Ref] -> Eval e ([Term Name], FunType (Term Name))
prepareUserAppArgs Def{..} as = do
  as' <- mapM reduce as
  ft' <- traverse reduce _dFunType
  typecheck (zip (_ftArgs ft') as')
  return (as',ft')

-- | Instantiate args in body and evaluate using supplied action.
evalUserAppBody :: Def Ref -> ([Term Name], FunType (Term Name)) -> Info -> Gas
                -> (Term Ref -> Eval e a) -> Eval e a
evalUserAppBody Def{..} (as',ft') ai g run =
  let bod' = instantiate (resolveArg ai (map mkDirect as')) _dDefBody
      fa = FunApp _dInfo (asString _dDefName) (Just _dModule) _dDefType (funTypes ft') (_mDocs _dMeta)
  in appCall fa ai as' $ fmap (g,) $ run bod'



reduceDirect :: Term Name -> [Term Ref] -> Info ->  Eval e (Term Name)
reduceDirect TNative {..} as ai =
  let fa = FunApp ai (asString _tNativeName) Nothing Defun _tFunTypes (Just _tNativeDocs)
      -- toplevel: only empty callstack or non-module-having callstack allowed
      enforceTopLevel = traverse_ $ \c ->
        case preview (sfApp . _Just . _1 . faModule . _Just) c of
          Nothing -> return ()
          Just m -> evalError ai $ "Top-level call used in module " <> pretty m <>
            ": " <> pretty _tNativeName
  in do
    when _tNativeTopLevelOnly $ use evalCallStack >>= enforceTopLevel
    appCall fa ai as $ _nativeFun _tNativeFun fa as

reduceDirect (TLitString errMsg) _ i = evalError i $ text $ unpack errMsg
reduceDirect r _ ai = evalError ai $ "Unexpected non-native direct ref: " <> pretty r

-- | Apply a pactdef, which will execute a step based on env 'PactStep'
-- defaulting to the first step.
applyPact ::  Term Ref ->  Eval e (Term Name)
applyPact (TList steps _ i) = do
  -- only one pact allowed in a transaction
  use evalPactExec >>= \bad -> unless (isNothing bad) $ evalError i "Nested pact execution, aborting"
  -- get step from environment or create a new one
  PactStep{..} <- view eePactStep >>= \ps -> case ps of
    Nothing -> view eeTxId >>= \tid ->
      return $ PactStep 0 False (PactId $ maybe "[localPactId]" (pack . show) tid) Nothing
    Just v -> return v
  -- retrieve indicated step from code
  s <- maybe (evalError i $ "applyPact: step not found: " <> pretty _psStep) return $ steps `atMay` _psStep
  case s of
    step@TStep {} -> do
      stepEntity <- traverse reduce (_tStepEntity step)
      let
        initExec executing = evalPactExec .= Just (PactExec (length steps) Nothing executing _psStep _psPactId)
        execStep = do
          initExec True
          case (_psRollback,_tStepRollback step) of
            (False,_) -> reduce $ _tStepExec step
            (True,Just rexp) -> reduce rexp
            (True,Nothing) -> return $ tStr $ renderCompactText $
              "No rollback on step " <> pretty _psStep
      case stepEntity of
        Just (TLitString se) -> view eeEntity >>= \envEnt -> case envEnt of
          Just (EntityName en) | se == en -> execStep -- matched for "private" step exec
                               | otherwise -> initExec False >> return (tStr "Skip step")
          Nothing -> evalError (_tInfo step) "Private step executed against non-private environment"
        Just t -> evalError (_tInfo t) "step entity must be String value"
        Nothing -> execStep -- "public" step exec
    t -> evalError (_tInfo t) "expected step"
applyPact t = evalError (_tInfo t) "applyPact: expected list of steps"


-- | Create special error form handled in 'reduceApp'
appError :: Info -> Doc -> Term n
appError i errDoc = TApp (App (msg errDoc) [] i) i

resolveFreeVars ::  Info -> Scope d Term Name ->  Eval e (Scope d Term Ref)
resolveFreeVars i b = traverse r b where
    r fv = resolveRef fv >>= \m -> case m of
             Nothing -> evalError i $ "Cannot resolve " <> pretty fv
             Just d -> return d

installModule :: ModuleData ->  Eval e ()
installModule ModuleData{..} = do
  (evalRefs . rsLoaded) %= HM.union (HM.fromList . map (first (`Name` def)) . HM.toList $ _mdRefMap)
  let n = case _mdModule of
        Module{..} -> _mName
        Interface{..} -> _interfaceName
  (evalRefs . rsLoadedModules) %= HM.insert n _mdModule

msg :: Doc -> Term n
msg = toTerm . renderCompactText

enscope ::  Term Name ->  Eval e (Term Ref)
enscope t = instantiate' <$> (resolveFreeVars (_tInfo t) . abstract (const Nothing) $ t)

instantiate' :: Scope n Term a -> Term a
instantiate' = instantiate1 (toTerm ("No bindings" :: Text))

-- | Runtime input typecheck, enforced on let bindings, consts, user defun app args.
-- Output checking -- app return values -- left to static TC.
-- Native funs not checked here, as they use pattern-matching etc.
typecheck :: [(Arg (Term Name),Term Name)] -> Eval e ()
typecheck ps = foldM_ tvarCheck M.empty ps where
  tvarCheck m (Arg {..},t) = do
    r <- typecheckTerm _aInfo _aType t
    case r of
      Nothing -> return m
      Just (v,ty) -> case M.lookup v m of
        Nothing -> return $ M.insert v ty m
        Just prevTy | prevTy == ty -> return m
                    | otherwise ->
                        evalError (_tInfo t) $ "Type error: values for variable " <> pretty _aType <>
                        " do not match: " <> pretty (prevTy,ty)

-- | 'typecheckTerm i spec t' checks a Term 't' against a specified type 'spec'.
-- Returns `Nothing` on successful check against concrete/untyped,
-- or `Just` a pair for successful check against a type variable, where
-- the pair is the type variable itself and the term type.
typecheckTerm :: forall e . Info -> Type (Term Name) -> Term Name
       -> Eval e (Maybe (TypeVar (Term Name),Type (Term Name)))
typecheckTerm i spec t = do

  ty <- case typeof t of
    Left s -> evalError i $ "Invalid type in value location: " <> pretty s
    Right r -> return r

  let

    tcFail :: Pretty a => a -> Eval e b
    tcFail found = evalError i $
      "Type error: expected " <> pretty spec <> ", found " <> pretty found

    tcOK = return Nothing

    -- | check container parameterized type.
    -- 'paramCheck pspec pty check' check specified param ty 'pspec' with
    -- value param ty 'pty'. If not trivially equal, use 'check'
    -- to determine actual container value type, and compare for equality
    -- with specified.
    paramCheck :: Type (Term Name)
               -> Type (Term Name)
               -> (Type (Term Name) -> Eval e (Type (Term Name)))
               -> Eval e (Maybe (TypeVar (Term Name),Type (Term Name)))
    paramCheck TyAny _ _ = tcOK -- no spec
    paramCheck pspec pty check
      | pspec == pty = tcOK -- equality OK
      | otherwise = do
          -- run check function to get actual content type
          checked <- check pspec
          -- final check expects full match with toplevel 'spec'
          if checked == spec then tcOK else tcFail checked

    -- | infer list value type
    checkList es lty = return $ TyList $
                    case nub (map typeof es) of
                      [Right a] -> a -- uniform value type: return it
                      [] -> lty -- empty: return specified
                      _ -> TyAny -- otherwise untyped

  case (spec,ty,t) of
    (_,_,_) | spec == ty -> tcOK -- identical types always OK
    (TyAny,_,_) -> tcOK -- var args are untyped
    (TyVar {..},_,_) ->
      if spec `canUnifyWith` ty
      then return $ Just (_tyVar,ty) -- collect found types under vars
      else tcFail ty -- constraint failed
    -- check list
    (TyList lspec,TyList lty,TList {..}) ->
      paramCheck lspec lty (checkList _tList)
    -- check object
    (TySchema TyObject ospec specPartial,TySchema TyObject oty _,TObject {..}) ->
      paramCheck ospec oty (checkUserType specPartial i _tObject)
    (TyPrim (TyGuard a),TyPrim (TyGuard b),_) -> case (a,b) of
      (Nothing,Just _) -> tcOK
      (Just _,Nothing) -> tcOK
      (c,d) -> if c == d then tcOK else tcFail ty
    _ -> tcFail ty

-- | check object args. Used in 'typecheckTerm' above and also in DB writes.
-- Total flag allows for partial row types if False.
checkUserType :: SchemaPartial -> Info  -> [(Term Name,Term Name)] -> Type (Term Name) -> Eval e (Type (Term Name))
checkUserType partial i ps (TyUser tu@TSchema {..}) = do
  let fields = M.fromList . map (_aName &&& id) $ _tFields
  aps <- forM ps $ \(k,v) -> case k of
    TLitString ks -> case M.lookup ks fields of
      Nothing -> evalError i $ "Invalid field for {" <> pretty _tSchemaName <> "}: " <> pretty ks
      Just a -> return (a,v)
    t -> evalError i $ "Invalid object, non-String key found: " <> pretty t
  let findMissing fs = do
        let missing = M.difference fs (M.fromList (map (first _aName) aps))
        unless (M.null missing) $ evalError i $
          "Missing fields for {" ++ pretty _tSchemaName ++ "}: " ++ pretty (M.elems missing)
  case partial of
    FullSchema -> findMissing fields
    PartialSchema fs -> findMissing (M.restrictKeys fields fs)
    AnySubschema -> return ()
  typecheck aps
  return $ TySchema TyObject (TyUser tu) partial
checkUserType _ i _ t = evalError i $ "Invalid reference in user type: " <> pretty t

runPure :: Eval (EnvNoDb e) a -> Eval e a
runPure action = ask >>= \env -> case _eePurity env of
  PNoDb -> unsafeCoerce action -- yuck. would love safer coercion here
  _ -> mkNoDbEnv env >>= runPure' action

runReadOnly :: Info -> Eval (EnvReadOnly e) a -> Eval e a
runReadOnly i action = ask >>= \env -> case _eePurity env of
  PNoDb -> evalError i "internal error: attempting sysread in pure context"
  PReadOnly -> unsafeCoerce action -- yuck. would love safer coercion here
  _ -> mkReadOnlyEnv env >>= runPure' action

runPure' :: Eval f b -> EvalEnv f -> Eval e b
runPure' action pureEnv = do
  s <- get
  (o,_s) <- liftIO $ runEval' s pureEnv action
  either throwM return o
