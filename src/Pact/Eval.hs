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

-- Suppress unused constraint on enforce-keyset.
-- TODO unused constraint is a dodgy warning, probably should not do it.
-- See: https://github.com/kadena-io/pact/pull/206/files#r215468087
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

-- |
-- Module      :  Pact.Eval
-- Copyright   :  (C) 2016,2019 Stuart Popejoy, Emily Pillmore, Kadena LLC
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>,
--                Emily Pillmore <emily@kadena.io>
--
-- Pact interpreter.
--

module Pact.Eval
    (eval
    ,evalBeginTx,evalRollbackTx,evalCommitTx
    ,reduce,reduceBody
    ,resolveFreeVars,resolveArg,resolveRef,lookupModule
    ,enforceKeySet,enforceKeySetName
    ,deref
    ,liftTerm,apply
    ,acquireModuleAdmin
    ,computeUserAppGas,prepareUserAppArgs,evalUserAppBody
    ,evalByName
    ,resumePact
    ,enforcePactValue,enforcePactValue'
    ,toPersistDirect
    ,searchCallStackApps
    ) where

import Bound
import Control.Lens hiding (DefName)
import Control.Monad
import Control.Monad.Reader
import Data.Aeson (Value)
import Data.Default
import Data.Foldable
import Data.Functor.Classes
import Data.Graph
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Vector as V
import Data.Text (Text, pack)
import qualified Data.Text as T
import Safe

import Pact.Gas
import Pact.Runtime.Capabilities
import Pact.Runtime.Typecheck
import Pact.Types.Capability
import Pact.Types.PactValue
import Pact.Types.Pretty
import Pact.Types.Purity
import Pact.Types.Runtime


evalBeginTx :: Info -> Eval e (Maybe TxId)
evalBeginTx i = view eeMode >>= beginTx i
{-# INLINE evalBeginTx #-}

evalRollbackTx :: Info -> Eval e ()
evalRollbackTx i = revokeAllCapabilities >> void (rollbackTx i)
{-# INLINE evalRollbackTx #-}

evalCommitTx :: Info -> Eval e [TxLog Value]
evalCommitTx i = do
  revokeAllCapabilities
  -- backend now handles local exec
  commitTx i
{-# INLINE evalCommitTx #-}

enforceKeySetName :: Info -> KeySetName -> Eval e ()
enforceKeySetName mi mksn = do
  ks <- maybe (evalError mi $ "No such keyset: " <> pretty mksn) return =<< readRow mi KeySets mksn
  _ <- computeGas (Left (mi,"enforce keyset name")) (GPostRead (ReadKeySet mksn ks))
  runSysOnly $ enforceKeySet mi (Just mksn) ks
{-# INLINE enforceKeySetName #-}

-- | Enforce keyset against environment.
enforceKeySet :: PureSysOnly e => Info -> Maybe KeySetName -> KeySet -> Eval e ()
enforceKeySet i ksn KeySet{..} = go
  where
    go = do
      sigs <- M.filterWithKey matchKey <$> view eeMsgSigs
      sigs' <- checkSigCaps sigs
      runPred (M.size sigs')
    matchKey k _ = k `elem` _ksKeys
    failed = failTx i $ "Keyset failure " <> parens (pretty _ksPredFun) <> ": " <>
      maybe (pretty $ map (elide . asString) $ toList _ksKeys) pretty ksn
    atLeast t m = m >= t
    elide pk | T.length pk < 8 = pk
             | otherwise = T.take 8 pk <> "..."
    count = length _ksKeys
    runPred matched =
      case M.lookup _ksPredFun keyPredBuiltins of
        Just KeysAll -> runBuiltIn (\c m -> atLeast c m)
        Just KeysAny -> runBuiltIn (\_ m -> atLeast 1 m)
        Just Keys2 -> runBuiltIn (\_ m -> atLeast 2 m)
        Nothing -> do
          r <- evalByName _ksPredFun [toTerm count,toTerm matched] i
          case r of
            (TLiteral (LBool b) _) | b -> return ()
                                   | otherwise -> failed
            _ -> evalError i $ "Invalid response from keyset predicate: " <> pretty r
      where
        runBuiltIn p | p count matched = return ()
                     | otherwise = failed
{-# INLINE enforceKeySet #-}


-- | Hoist Name back to ref
liftTerm :: Term Name -> Term Ref
liftTerm a = TVar (Direct a) def

-- | Search up through call stack apps to find the first `Just a`
searchCallStackApps :: (FunApp -> Maybe a) -> Eval e (Maybe a)
searchCallStackApps f = uses evalCallStack $
  preview (traverse . sfApp . _Just . _1 . to f . _Just)

-- | Eval a function by name with supplied args, and guard against recursive execution.
evalByName :: Name -> [Term Name] -> Info -> Eval e (Term Name)
evalByName n as i = do

  -- Build and resolve TApp

  app <- enscope (TApp (App (TVar n def) as i) i)

  -- lens into user function if any to test for loop

  case preview (tApp . appFun . tVar . _Ref . tDef) app of
    Nothing -> return ()
    Just Def{..} -> do

      -- lens into call stack to find first app with matching name/module, blowup if found

      let sameName :: DefName -> ModuleName -> FunApp -> Maybe ()
          sameName dn mn FunApp{..}
            | (DefName _faName) == dn && Just mn == _faModule = Just ()
            | otherwise = Nothing

      found <- searchCallStackApps $ sameName _dDefName _dModule

      case found of
        Just () -> evalError i $ "evalByName: loop detected: " <> pretty n
        _ -> return ()

  -- success: evaluate

  reduce app


-- | Application with additional args.
apply :: App (Term Ref) -> [Term Name] -> Eval e (Term Name)
apply app as = reduceApp $ over appArgs (++ map liftTerm as) app

topLevelCall
  :: Info -> Text -> GasArgs -> (Gas -> Eval e (Gas, a)) -> Eval e a
topLevelCall i name gasArgs action = call (StackFrame name i Nothing) $
  computeGas (Left (i,name)) gasArgs >>= action

-- | Acquire module admin with enforce.
acquireModuleAdmin :: Info -> ModuleName -> Governance (Def Ref) -> Eval e CapEvalResult
acquireModuleAdmin i modName modGov =
  acquireModuleAdminCapability modName $ enforceModuleAdmin i modGov


enforceModuleAdmin :: Info -> Governance (Def Ref) -> Eval e ()
enforceModuleAdmin i modGov =
    case _gGovernance modGov of
      Left ks -> enforceKeySetName i ks
      Right d@Def{..} -> case _dDefType of
        Defcap -> do
          af <- prepareUserAppArgs d [] _dInfo
          g <- computeUserAppGas d _dInfo
          void $ evalUserAppBody d af _dInfo g reduceBody
        _ -> evalError i "enforceModuleAdmin: module governance must be defcap"



-- | Evaluate current namespace and prepend namespace to the
-- module name. This should be done before any lookups, as
-- 'namespace.modulename' is the name we will associate
-- with a module unless the namespace policy is defined
-- otherwise
evalNamespace :: Info -> (Setter' s ModuleName) -> s -> Eval e s
evalNamespace info setter m = do
  mNs <- use $ evalRefs . rsNamespace
  case mNs of
    Nothing -> do
      policy <- view eeNamespacePolicy
      unless (allowRoot policy) $
        evalError info "Definitions in default namespace are not authorized"
      return m
    Just (Namespace n _ _) -> return $ over setter (mangleModuleName n) m
  where
    mangleModuleName :: NamespaceName -> ModuleName -> ModuleName
    mangleModuleName n mn@(ModuleName nn ns) =
      case ns of
        Nothing -> ModuleName nn (Just n)
        Just {} -> mn
    allowRoot (SimpleNamespacePolicy f) = f Nothing
    allowRoot (SmartNamespacePolicy ar _) = ar

-- | Lookup module in state or database with exact match on 'ModuleName'.
lookupModule :: HasInfo i => i -> ModuleName -> Eval e (Maybe (ModuleData Ref))
lookupModule i mn = do
  loaded <- preuse $ evalRefs . rsLoadedModules . ix mn
  case loaded of
    Just (m,_) -> return $ Just m
    Nothing -> do
      stored <- readRow (getInfo i) Modules mn
      case stored of
        Just mdStored -> do
          _ <- computeGas (Left ((getInfo i), "lookup module")) $ case (_mdModule mdStored) of
            MDModule m -> GPostRead (ReadModule (_mName m) (_mCode m))
            MDInterface int -> GPostRead (ReadInterface (_interfaceName int) (_interfaceCode int))
          natives <- view $ eeRefStore . rsNatives
          let natLookup (NativeDefName n) = case HM.lookup (Name (BareName n def)) natives of
                Just (Direct t) -> Just t
                _ -> Nothing
          case traverse (traverse (fromPersistDirect natLookup)) mdStored of
            Right md -> do
              evalRefs . rsLoadedModules %= HM.insert mn (md,False)
              return $ Just md
            Left e -> evalError' i $ "Internal error: module restore failed: " <> pretty e
        Nothing -> return Nothing


-- | Evaluate top-level term.
eval ::  Term Name ->  Eval e (Term Name)
eval (TUse u@Use{..} i) = topLevelCall i "use" (GUse _uModuleName _uModuleHash) $ \g ->
  evalUse u >> return (g,tStr $ renderCompactText' $ "Using " <> pretty _uModuleName)
eval (TModule (MDModule m) bod i) =
  topLevelCall i "module" (GModuleDecl (_mName m) (_mCode m)) $ \g0 -> do
    checkAllowModule i
    -- prepend namespace def to module name
    mangledM <- evalNamespace i mName m
    -- enforce old module governance
    preserveModuleNameBug <- isExecutionFlagSet FlagPreserveModuleNameBug
    oldM <- lookupModule i $ _mName $ if preserveModuleNameBug then m else mangledM
    case oldM of
      Nothing -> return ()
      Just (ModuleData omd _) ->
        case omd of
          MDModule om -> void $ acquireModuleAdmin i (_mName om) (_mGovernance om)
          MDInterface Interface{..} -> evalError i $
            "Name overlap: module " <> pretty (_mName m) <>
            " overlaps with interface  " <> pretty _interfaceName
    case _gGovernance $ _mGovernance mangledM of
      -- enforce new module keyset on install
      Left ks -> enforceKeySetName i ks
      -- governance is granted on install without testing the cap.
      -- rationale is governance might be some vote or something
      -- that doesn't exist yet. Of course, if governance is
      -- busted somehow, this means we won't find out, and
      -- can't fix it later.
      _ -> void $ acquireModuleAdminCapability (_mName m) $ return ()
    -- build/install module from defs
    (g,govM) <- loadModule mangledM bod i g0
    _ <- computeGas (Left (i,"module")) (GPreWrite (WriteModule (_mName m) (_mCode m)))
    writeRow i Write Modules (_mName mangledM) =<< traverse (traverse toPersistDirect') govM
    return (g, msg $ "Loaded module " <> pretty (_mName mangledM) <> ", hash " <> pretty (_mHash mangledM))

eval (TModule (MDInterface m) bod i) =
  topLevelCall i "interface" (GInterfaceDecl (_interfaceName m) (_interfaceCode m)) $ \gas -> do
    checkAllowModule i
     -- prepend namespace def to module name
    mangledI <- evalNamespace i interfaceName m
    -- enforce no upgrades
    void $ lookupModule i (_interfaceName mangledI) >>= traverse
      (const $ evalError i $ "Existing interface found (interfaces cannot be upgraded)")
    (g,govI) <- loadInterface mangledI bod i gas
    _ <- computeGas (Left (i, "interface")) (GPreWrite (WriteInterface (_interfaceName m) (_interfaceCode m)))
    writeRow i Write Modules (_interfaceName mangledI) =<< traverse (traverse toPersistDirect') govI
    return (g, msg $ "Loaded interface " <> pretty (_interfaceName mangledI))
eval t = enscope t >>= reduce

checkAllowModule :: Info -> Eval e ()
checkAllowModule i = do
  disabled <- isExecutionFlagSet FlagDisableModuleInstall
  when disabled $ evalError i $ "Module/interface install not supported"


toPersistDirect' :: Term Name -> Eval e PersistDirect
toPersistDirect' t = case toPersistDirect t of
  Right v -> return v
  Left e -> evalError (getInfo t) $ "Attempting to serialize non pact-value in module def: " <> pretty e


evalUse :: Use -> Eval e ()
evalUse (Use mn h mis i) = do
  mm <- resolveModule i mn
  case mm of
    Nothing -> evalError i $ "Module " <> pretty mn <> " not found"
    Just md -> do
      case _mdModule md of
        MDModule Module{..} ->
          case h of
            Nothing -> return ()
            Just mh | mh == _mHash -> return ()
                    | otherwise -> evalError i $ "Module " <>
                        pretty mn <> " does not match specified hash: " <>
                        pretty mh <> ", " <> pretty _mHash
        MDInterface i' ->
          case h of
            Nothing -> return ()
            Just _ -> evalError i
              $ "Interfaces should not have associated hashes: "
              <> pretty (_interfaceName i')

      validateImports i (_mdRefMap md) mis
      installModule False md mis

validateImports :: Info -> HM.HashMap Text Ref -> Maybe (V.Vector Text) -> Eval e ()
validateImports _ _ Nothing = return ()
validateImports i rs (Just is) = traverse_ go is
  where
    go imp = case HM.lookup imp rs of
      Nothing -> evalError i $ "imported name not found: " <> pretty imp
      Just (Ref r) -> case r of
        TDef d _ -> case _dDefType d of
          Defcap -> evalError i $ "cannot import capabilities: " <> pretty imp
          _ -> return ()
        TConst{} -> return ()
        TSchema{} -> return ()
        _ -> evalError i
          $ "invalid import - only function, schema, and constant symbols allowed: "
          <> pretty imp
      Just _ -> return ()

mangleDefs :: ModuleName -> Term Name -> Term Name
mangleDefs mn term = modifyMn term
  where
    modifyMn = case term of
      TDef{}    -> set (tDef . dModule) mn
      TConst{}  -> set tModule $ Just mn
      TSchema{} -> set tModule $ Just mn
      TTable{}  -> set tModuleName mn
      _         -> id

-- | Make table of module definitions for storage in namespace/RefStore.
loadModule
  :: Module (Term Name)
  -> Scope n Term Name
  -> Info
  -> Gas
  -> Eval e (Gas,ModuleData Ref)
loadModule m bod1 mi g0 = do
  mapM_ evalUse $ _mImports m
  (g1,mdefs) <- collectNames g0 (GModuleMember $ MDModule m) bod1 $ \t -> case t of
    TDef d _ -> return $ Just $ asString (_dDefName d)
    TConst a _ _ _ _ -> return $ Just $ _aName a
    TSchema n _ _ _ _ -> return $ Just $ asString n
    tt@TTable{} -> return $ Just $ asString (_tTableName tt)
    TUse _ _ -> return Nothing
    _ -> evalError' t "Invalid module member"
  evaluatedDefs <- evaluateDefs mi $ mangleDefs (_mName m) <$> mdefs
  (m', solvedDefs) <- evaluateConstraints mi m evaluatedDefs
  mGov <- resolveGovernance solvedDefs m'
  let md = ModuleData mGov solvedDefs
  installModule True md Nothing
  return (g1,md)

loadInterface
  :: Interface
  -> Scope n Term Name
  -> Info
  -> Gas
  -> Eval e (Gas,ModuleData Ref)
loadInterface i body info gas0 = do
  mapM_ evalUse $ _interfaceImports i
  (gas1,idefs) <- collectNames gas0 (GModuleMember $ MDInterface i) body $ \t -> case t of
    TDef d _ -> return $ Just $ asString (_dDefName d)
    TConst a _ _ _ _ -> return $ Just $ _aName a
    TSchema n _ _ _ _ -> return $ Just $ asString n
    TUse _ _ -> return Nothing
    _ -> evalError' t "Invalid interface member"
  evaluatedDefs <- evaluateDefs info $ mangleDefs (_interfaceName i) <$> idefs
  let md = ModuleData (MDInterface i) evaluatedDefs
  installModule True md Nothing
  return (gas1,md)

-- | Retrieve map of definition names to their corresponding terms
-- and compute their gas value
--
collectNames
  :: Gas
    -- ^ initial gas value
  -> GasArgs
    -- ^ gas args (should be GModuleMember)
  -> Scope n Term Name
    -- ^ module body
  -> (Term Name -> Eval e (Maybe Text))
    -- ^ function extracting definition names
  -> Eval e (Gas, HM.HashMap Text (Term Name))
collectNames g0 args body k = case instantiate' body of
    TList bd _ _ -> do
      ns <- view $ eeRefStore . rsNatives
      foldM (go ns) (g0, mempty) bd
    t -> evalError' t $ "malformed declaration"
  where
    go ns (g,ds) t = k t >>= \dnm -> case dnm of
      Nothing -> return (g, ds)
      Just dn -> do
        -- disallow native overlap
        when (isJust $ HM.lookup (Name (BareName dn def)) ns) $
          evalError' t $ "definitions cannot overlap with native names: " <> pretty dn
        -- disallow conflicting members
        when (isJust $ HM.lookup dn ds) $
          evalError' t $ "definition name conflict: " <> pretty dn

        g' <- computeGas (Left (_tInfo t,dn)) args
        return (g + g',HM.insert dn t ds)


resolveGovernance
  :: HM.HashMap Text Ref
  -> Module (Term Name)
  -> Eval e (ModuleDef (Def Ref))
resolveGovernance solvedDefs m' = fmap MDModule $ forM m' $ \g -> case g of
    TVar (Name (BareName n _)) _ -> case HM.lookup n solvedDefs of
      Just r -> case r of
        Ref (TDef govDef _) -> case _dDefType govDef of
          Defcap -> return govDef
          _ -> evalError (_tInfo g) "Invalid module governance, must be defcap"
        _ -> evalError (_tInfo g) "Invalid module governance, should be def ref"
      Nothing -> evalError (_tInfo g) "Unknown module governance reference"
    _ -> evalError (_tInfo g) "Invalid module governance, should be var"


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
  sortedDefs <- forM cs $ \c -> case c of
    AcyclicSCC v -> return v
    CyclicSCC vs -> do
      let i = if null vs then info else _tInfo $ view _1 $ head vs
          pl = over (traverse . _3) (SomeDoc . prettyList)
            $ over (traverse . _1) (fmap mkSomeDoc)
            $ vs

      evalError i $ "Recursion detected: " <> prettyList pl

  -- the order of evaluation matters for 'dresolve' - this *must* be a left fold
  let dresolve ds (d,dn,_) = HM.insert dn (Ref $ unify ds <$> d) ds
      unifiedDefs = foldl' dresolve HM.empty sortedDefs

  traverse (runSysOnly . evalConsts) unifiedDefs
  where
    mkSomeDoc = either (SomeDoc . pretty) (SomeDoc . pretty)

    traverseGraph ds = fmap stronglyConnCompR $ forM (HM.toList ds) $ \(dn,d) -> do
      d' <- forM d $ \(f :: Name) -> do
        dm <- resolveRef f f
        case (dm, f) of
          (Just t, _) -> return (Right t)
          (Nothing, Name (BareName fn _)) ->
            case HM.lookup fn ds of
              Just _ -> return (Left fn)
              Nothing -> evalError' f $ "Cannot resolve " <> dquotes (pretty f)
          (Nothing, _) -> evalError' f $ "Cannot resolve " <> dquotes (pretty f)

      return (d', dn, mapMaybe (either Just (const Nothing)) $ toList d')

-- | Evaluate interface constraints in module.
evaluateConstraints
  :: Info
  -> (Module n)
  -> HM.HashMap Text Ref
  -> Eval e (Module n, HM.HashMap Text Ref)
evaluateConstraints info m evalMap =
  foldM evaluateConstraint (m, evalMap) $ _mInterfaces m
  where
    evaluateConstraint (m', refMap) ifn = do
      refData <- resolveModule info ifn
      case refData of
        Nothing -> evalError info $
          "Interface not defined: " <> pretty ifn
        Just (ModuleData (MDInterface Interface{..}) irefs) -> do
          em' <- HM.foldrWithKey (solveConstraint ifn info) (pure refMap) irefs
          let um = over mMeta (<> _interfaceMeta) m'
          pure (um, em')
        Just _ -> evalError info "Unexpected: module found in interface position while solving constraints"

-- | Compare implemented member signatures with their definitions.
-- At this stage, we have not merged consts, so we still check for overlap
solveConstraint
  :: ModuleName
  -> Info
  -> Text
  -> Ref
  -> Eval e (HM.HashMap Text Ref)
  -> Eval e (HM.HashMap Text Ref)
solveConstraint _ifn info refName (Direct t) _ =
  evalError info $ "found native reference " <> pretty t
  <> " while resolving module contraints: " <> pretty refName
solveConstraint ifn info refName (Ref t) evalMap = do
  em <- evalMap
  case HM.lookup refName em of
    Nothing ->
      case t of
        TConst{} -> evalMap
        TSchema{} -> evalMap
        _ -> evalError info $
          "found unimplemented member while resolving model constraints: " <> pretty refName
    Just (Direct s) ->
      evalError info $ "found native reference " <> pretty s <>
      " while resolving module contraints: " <> pretty t
    Just (Ref s) ->
      case (t, s) of
        (TDef (Def _n _mn dt (FunType args rty) _ m dmeta _) _,
          TDef (Def _n' _mn' dt' (FunType args' rty') _ _ dmeta' _) _) -> do
          match s "Def type mismatch" dt dt'
          matchWith termEq1 s "Return type mismatch" rty rty'
          match s "Arity mismatch" (length args) (length args')
          matchWith (liftEq defMetaEq) s "Defmeta mismatch" dmeta dmeta'
          forM_ (args `zip` args') $ \((Arg n ty _), a@(Arg n' ty' _)) -> do
            -- FV requires exact argument names as opposed to positional info
            match a "Argument name mismatch" n n'
            matchWith termEq1 a ("Argument type mismatch for " <> n) ty ty'
          -- the model concatenation step: we reinsert the ref back into the map with new models
          pure $ HM.insert refName (Ref $ over (tDef . dMeta) (<> m) s) em
        _ -> evalError' s $ "found overlapping refs - please resolve: " <> pretty t

  where
    match :: (HasInfo i, Eq v, Pretty v) => i -> Text -> v -> v -> Eval e ()
    match = matchWith (==)
    matchWith :: (HasInfo i, Pretty v) => (v -> v -> Bool) -> i -> Text -> v -> v -> Eval e ()
    matchWith test i desc expected actual = unless (expected `test` actual) $
      evalError' i $ pretty desc <> " with " <> pretty ifn <> ": found " <>
        pretty actual <> ", expected " <> pretty expected
    termEq1 :: Eq1 f => f (Term Ref) -> f (Term Ref) -> Bool
    termEq1 = liftEq termEq
    -- | For DefcapMeta, we only enforce if iface specifies user mgd fun, in which case
    -- we just want the mgr fun names to match
    defMetaEq :: DefMeta (Term Ref) -> DefMeta (Term Ref) -> Bool
    defMetaEq (DMDefcap (DefcapManaged ifaceDM)) (DMDefcap (DefcapManaged implDM)) = case (ifaceDM,implDM) of
      -- interface auto or unspecified: OK
      (Nothing,_) -> True
      -- interface explicit: match name+arg
      (Just a,Just b) -> getDefName a == getDefName b
      (Just _,Nothing) -> False
    getDefName (an,TVar (Ref (TDef Def {..} _)) _) = Just (_dDefName,an)
    getDefName _ = Nothing


-- | Lookup module in state or db, resolving against current namespace if unqualified.
resolveModule :: HasInfo i => i -> ModuleName -> Eval e (Maybe (ModuleData Ref))
resolveModule = moduleResolver lookupModule

-- | Perform some lookup involving a 'ModuleName' which if unqualified
-- will re-perform the lookup with the current namespace, if any
moduleResolver :: HasInfo i => (i -> ModuleName -> Eval e (Maybe a)) ->
                  i -> ModuleName -> Eval e (Maybe a)
moduleResolver lkp i mn = do
  md <- lkp i mn
  case md of
    Just _ -> return md
    Nothing -> do
      case _mnNamespace mn of
        Just {} -> pure Nothing -- explicit namespace not found
        Nothing -> do
          mNs <- use $ evalRefs . rsNamespace
          case mNs of
            Just ns -> lkp i $ set mnNamespace (Just . _nsName $ ns) mn
            Nothing -> pure Nothing


resolveRef :: HasInfo i => i -> Name -> Eval e (Maybe Ref)
resolveRef i (QName (QualifiedName q n _)) = moduleResolver (lookupQn n) i q
  where
    lookupQn n' i' q' = do
      m <- lookupModule i' q'
      return $ join $ HM.lookup n' . _mdRefMap <$> m
resolveRef _i nn@Name {} = do
  nm <- preview $ eeRefStore . rsNatives . ix nn
  case nm of
    d@Just {} -> return d
    Nothing -> preuse $ evalRefs . rsLoaded . ix nn

-- | This should be impure. See 'evaluateDefs'. Refs are
-- expected to exist, and if they don't, it is a serious bug
unify :: HM.HashMap Text Ref -> Either Text Ref -> Ref
unify _ (Right r) = r
unify m (Left t) = m HM.! t

evalConsts :: PureSysOnly e => Ref -> Eval e Ref
evalConsts rr@(Ref r) = case r of
  TConst {..} -> case _tConstVal of
    CVRaw raw -> do
      v <- reduce =<< traverse evalConsts raw
      traverse reduce _tConstArg >>= \a -> typecheck [(a,v)]
      return $ Ref (TConst _tConstArg _tModule (CVEval raw $ liftTerm v) _tMeta _tInfo)
    _ -> return rr
  _ -> Ref <$> traverse evalConsts r
evalConsts r = return r


deref :: Ref -> Eval e (Term Name)
deref (Direct t@TConst{}) = case _tConstVal t of
  CVEval _ v -> return v
  CVRaw _ -> evalError' t $ "internal error: deref: unevaluated const: " <> pretty t
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
reduce TList {..} = TList <$> mapM reduce _tList <*> traverse reduce _tListType <*> pure _tInfo
reduce t@TDef {} = return $ toTerm $ pack $ show t
reduce t@TNative {} = return $ toTerm $ pack $ show t
reduce TConst {..} = case _tConstVal of
  CVEval _ t -> reduce t
  CVRaw a -> evalError _tInfo $ "internal error: reduce: unevaluated const: " <> pretty a
reduce (TObject (Object ps t ko oi) i) =
  TObject <$> (Object <$> traverse reduce ps <*> traverse reduce t <*> pure ko <*> pure oi) <*> pure i
reduce (TBinding ps bod c i) = case c of
  BindLet -> reduceLet ps bod i
  BindSchema _ -> evalError i "Unexpected schema binding"
reduce t@TModule{} = evalError (_tInfo t) "Modules and Interfaces only allowed at top level"
reduce t@TUse {} = evalError (_tInfo t) "Use only allowed at top level"
reduce t@TStep {} = evalError (_tInfo t) "Step at invalid location"
reduce TSchema {..} = TSchema _tSchemaName _tModule _tMeta <$> traverse (traverse reduce) _tFields <*> pure _tInfo
reduce TTable {..} = TTable _tTableName _tModuleName _tHash <$> mapM reduce _tTableType <*> pure _tMeta <*> pure _tInfo

mkDirect :: Term Name -> Term Ref
mkDirect = (`TVar` def) . Direct

reduceBody :: Term Ref -> Eval e (Term Name)
reduceBody (TList bs _ _) =
  -- unsafe but only called in validated body contexts
  V.last <$> V.mapM reduce bs
reduceBody t = evalError (_tInfo t) "Expected body forms"

reduceLet :: [BindPair (Term Ref)] -> Scope Int Term Ref -> Info -> Eval e (Term Name)
reduceLet ps bod i = do
  ps' <- mapM (\(BindPair a t) -> (,) <$> traverse reduce a <*> reduce t) ps
  typecheck ps'
  reduceBody (instantiate (resolveArg i (map (mkDirect . snd) ps')) bod)


{-# INLINE resolveArg #-}
resolveArg :: Info -> [Term n] -> Int -> Term n
resolveArg ai as i = fromMaybe (appError ai $ "Missing argument value at index " <> pretty i) $
                     as `atMay` i

appCall :: Pretty t => FunApp -> Info -> [Term t] -> Eval e (Gas,a) -> Eval e a
appCall fa ai as = call (StackFrame (_faName fa) ai (Just (fa,map abbrev as)))


enforcePactValue :: (Term Name) -> Eval e PactValue
enforcePactValue t = case toPactValue t of
  Left s -> evalError' t $ "Only value-level terms permitted: " <> pretty s
  Right v -> return v

enforcePactValue' :: Traversable f => f (Term Name) -> Eval e (f PactValue)
enforcePactValue' = traverse enforcePactValue

reduceApp :: App (Term Ref) -> Eval e (Term Name)
reduceApp (App (TVar (Direct t) _) as ai) = reduceDirect t as ai
reduceApp (App (TVar (Ref r) _) as ai) = reduceApp (App r as ai)
reduceApp (App (TDef d@Def{..} _) as ai) = {- eperf (asString _dDefName) $ -} do
  g <- computeUserAppGas d ai
  af <- prepareUserAppArgs d as ai
  evalUserAppBody d af ai g $ \bod' ->
    case _dDefType of
      Defun ->
        {- eperf (asString _dDefName <> ":body") $ -} reduceBody bod'
      Defpact -> do
        continuation <- PactContinuation (QName (QualifiedName _dModule (asString _dDefName) def))
          <$> enforcePactValue' (fst af)
        initPact ai continuation bod'
      Defcap ->
        evalError ai "Cannot directly evaluate defcap"
reduceApp (App (TLitString errMsg) _ i) = evalError i $ pretty errMsg
reduceApp (App r _ ai) = evalError ai $ "Expected def: " <> pretty r

-- | precompute "UserApp" cost
computeUserAppGas :: Def Ref -> Info -> Eval e Gas
computeUserAppGas Def{..} ai = computeGas (Left (ai, asString _dDefName)) (GUserApp _dDefType)

-- | prepare reduced args and funtype, and typecheck
prepareUserAppArgs :: Def Ref -> [Term Ref] -> Info -> Eval e ([Term Name], FunType (Term Name))
prepareUserAppArgs Def{..} args i = do
  as' <- mapM reduce args
  ft' <- traverse reduce _dFunType
  typecheckArgs i _dDefName ft' as'
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
  in {- eperf (asString _tNativeName) $ -} do
    when _tNativeTopLevelOnly $ use evalCallStack >>= enforceTopLevel
    appCall fa ai as $ _nativeFun _tNativeFun fa as

reduceDirect (TLitString errMsg) _ i = evalError i $ pretty errMsg
reduceDirect r _ ai = evalError ai $ "Unexpected non-native direct ref: " <> pretty r

initPact :: Info -> PactContinuation -> Term Ref -> Eval e (Term Name)
initPact i app bod = view eePactStep >>= \es -> case es of
  Just v -> evalError i $ "initPact: internal error: step already in environment: " <> pretty v
  Nothing -> view eeHash >>= \hsh ->
    applyPact i app bod $ PactStep 0 False (toPactId hsh) Nothing


-- | Apply or resume a pactdef step.
applyPact :: Info -> PactContinuation -> Term Ref -> PactStep -> Eval e (Term Name)
applyPact i app (TList steps _ _) PactStep {..} = do

  -- only one pact state allowed in a transaction
  use evalPactExec >>= \bad -> unless (isNothing bad) $
    evalError i "Multiple or nested pact exec found"

  -- retrieve indicated step from code
  st <- maybe (evalError i $ "applyPact: step not found: " <> pretty _psStep) return $ steps V.!? _psStep
  step <- case st of
    TStep step _meta _i -> return step
    t -> evalError (_tInfo t) "expected step"

  -- determine if step is skipped (for private execution)
  executePrivate <- traverse reduce (_sEntity step) >>= traverse (\stepEntity -> case stepEntity of
    (TLitString se) -> view eeEntity >>= \envEnt -> case envEnt of
      Just (EntityName en) -> return $ (se == en) -- execute if req entity matches context entity
      Nothing -> evalError' step "applyPact: private step executed against non-private environment"
    t -> evalError' t "applyPact: step entity must be String value")

  let stepCount = length steps
      rollback = isJust $ _sRollback step

  -- init pact state
  evalPactExec .=
      Just (PactExec stepCount Nothing executePrivate _psStep _psPactId app rollback)

  -- evaluate
  result <- case executePrivate of
    Just False -> return $ tStr "skip step"
    _ -> case (_psRollback,_sRollback step) of
      (False,_) -> reduce $ _sExec step
      (True,Just rexp) -> reduce rexp
      (True,Nothing) -> evalError' step $ "Rollback requested but none in step"

  resultState <- use evalPactExec >>= (`maybe` pure)
    (evalError i "Internal error, pact exec state not found after execution")

  -- update database, determine if done
  let isLastStep = _psStep == pred stepCount
      private = isJust executePrivate
      done =
        (not _psRollback && isLastStep) -- done if normal exec of last step
        || (not private && _psRollback) -- done if public rollback
        || (private && _psRollback && _psStep == 0) -- done if private and rolled back to step 0

  writeRow i Write Pacts _psPactId $ if done then Nothing else Just resultState

  return result

applyPact _ _ t _ = evalError' t "applyPact: invalid defpact body, expected list of steps"



-- | Resume a pact, either as specified or as found in database.
-- Expects a 'PactStep' to be populated in the environment.
resumePact :: Info -> Maybe PactExec -> Eval e (Term Name)
resumePact i crossChainContinuation = do

  ps@PactStep{..} <- view eePactStep >>= (`maybe` pure)
    (evalError i "resumePact: no step in environment")

  -- query for previous exec
  dbState <- readRow i Pacts _psPactId

  -- validate db state
  let proceed = resumePactExec i ps
      matchCC :: (Eq b, Pretty b) => (a -> b) -> Text -> a -> a -> Eval e ()
      matchCC acc fname cc db
        | acc cc == acc db = return ()
        | otherwise = evalError i $ "resumePact: cross-chain " <> pretty fname <> " " <>
             pretty (acc cc) <>
             " does not match db " <> pretty fname <> " " <>
             pretty (acc db)
  case (dbState,crossChainContinuation) of

    -- Terminated pact in db: always fail
    (Just Nothing,_) ->
      evalError i $ "resumePact: pact completed: " <> pretty _psPactId

    -- Nothing in db, Nothing cross-chain continuation: fail
    (Nothing,Nothing) ->
      evalError i $ "resumePact: no previous execution found for: " <> pretty _psPactId

    -- Nothing in db, Just cross-chain continuation: proceed with cross-chain
    (Nothing,Just ccExec) -> proceed ccExec

    -- Active db record, Nothing cross-chain continuation: proceed with db
    (Just (Just dbExec),Nothing) -> proceed dbExec

    -- Active db record, cross-chain continuation:
    -- A valid possibility iff this is a flip-flop from another chain, e.g.
    --   0. This chain: start pact
    --   1. Other chain: continue pact
    --   2. This chain: continue pact
    -- Thus check at least one step skipped.
    (Just (Just dbExec),Just ccExec) -> do

      unless (_peStep ccExec > _peStep dbExec + 1) $
        evalError i $ "resumePact: db step " <> pretty (_peStep dbExec) <>
        " must be at least 2 steps before cross-chain continuation step " <>
        pretty (_peStep ccExec)

      -- validate continuation and step count against db
      -- peExecuted and peStepHasRollback is ignored in 'resumePactExec'
      matchCC _peContinuation "continuation" ccExec dbExec
      matchCC _peStepCount "step count" ccExec dbExec

      proceed ccExec


-- | Resume a pact with supplied PactExec context.
resumePactExec :: Info -> PactStep -> PactExec -> Eval e (Term Name)
resumePactExec i req ctx = do

  when (_psPactId req /= _pePactId ctx) $ evalError i $
    "resumePactExec: request and context pact IDs do not match: " <>
    pretty (_psPactId req,_pePactId ctx)

  when (_psStep req < 0 || _psStep req >= _peStepCount ctx) $ evalError i $
    "resumePactExec: invalid step in request: " <> pretty (_psStep req)

  if _psRollback req
    then when (_psStep req /= _peStep ctx) $ evalError i $
         "resumePactExec: rollback step mismatch with context: " <> pretty (_psStep req,_peStep ctx)
    else when (_psStep req /= succ (_peStep ctx)) $ evalError i $
         "resumePactExec: exec step mismatch with context: " <> pretty (_psStep req,_peStep ctx)

  target <- resolveRef i (_pcDef (_peContinuation ctx)) >>= (`maybe` pure)
    (evalError i $ "resumePactExec: could not resolve continuation ref: " <>
     pretty (_pcDef $ _peContinuation ctx))

  def' <- case target of
    (Ref (TDef d _)) -> do
      when (_dDefType d /= Defpact) $
         evalError' d $ "resumePactExec: defpact required"
      return d
    t -> evalError' t $ "resumePactExec: defpact ref required"

  let args = map (liftTerm . fromPactValue) (_pcArgs (_peContinuation ctx))

  g <- computeUserAppGas def' i
  af <- prepareUserAppArgs def' args i

  -- if resume is in step, use that, otherwise get from exec state
  let resume = case _psResume req of
        r@Just {} -> r
        Nothing -> _peYield ctx

  -- run local environment with yield from pact exec
  local (set eePactStep (Just $ set psResume resume req)) $
    evalUserAppBody def' af i g $ \bod ->
      applyPact i (_peContinuation ctx) bod req


-- | Create special error form handled in 'reduceApp'
appError :: Info -> Doc -> Term n
appError i errDoc = TApp (App (msg errDoc) [] i) i

resolveFreeVars ::  Info -> Scope d Term Name ->  Eval e (Scope d Term Ref)
resolveFreeVars i b = traverse r b where
    r fv = resolveRef i fv >>= \m -> case m of
             Nothing -> evalError i $ "Cannot resolve " <> pretty fv
             Just d -> return d

-- | Install module into local namespace. If supplied a vector of qualified imports,
-- only load those references. If supplied an 'True' (updated/new module), update
-- loaded modules.
--
installModule :: Bool -> ModuleData Ref -> Maybe (V.Vector Text) -> Eval e ()
installModule updated md = go . maybe allDefs filteredDefs
  where
    go f = do
      evalRefs . rsLoaded %= HM.union (HM.foldlWithKey' f mempty $ _mdRefMap md)
      when updated $
        evalRefs . rsLoadedModules %= HM.insert (moduleDefName $ _mdModule md) (md,updated)

    filteredDefs is m k v =
      if V.elem k is
      then HM.insert (Name $ BareName k def) v m
      else m

    allDefs m k v = HM.insert (Name $ BareName k def) v m

msg :: Doc -> Term n
msg = toTerm . renderCompactText'

enscope :: Term Name -> Eval e (Term Ref)
enscope t = instantiate' <$> (resolveFreeVars (_tInfo t) . abstract (const Nothing) $ t)

instantiate' :: Scope n Term a -> Term a
instantiate' = instantiate1 (toTerm ("No bindings" :: Text))
