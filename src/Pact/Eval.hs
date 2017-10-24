{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
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
    ,resolveFreeVars,resolveArg
    ,enforceKeySet,enforceKeySetName
    ,checkUserType
    ,deref
    ,installModule
    ,runPure,runPureSys,Purity
    ,liftTerm,apply,apply'
    ) where

import Control.Lens hiding (op)
import Control.Applicative
import Control.Monad.Catch (throwM)
import Control.Concurrent.MVar
import Data.List
import Control.Monad
import Prelude hiding (exp,mod)
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

import Pact.Types.Runtime
import Pact.Types.Crypto (Hash)


evalBeginTx :: Info -> Eval e ()
evalBeginTx i = beginTx i =<< view eeTxId
{-# INLINE evalBeginTx #-}

evalRollbackTx :: Info -> Eval e ()
evalRollbackTx = void . rollbackTx
{-# INLINE evalRollbackTx #-}

evalCommitTx :: Info -> Eval e [TxLog]
evalCommitTx i = do
  tid <- view eeTxId
  case tid of
    Nothing -> evalRollbackTx i >> return []
    Just {} -> commitTx i
{-# INLINE evalCommitTx #-}


enforceKeySetName ::  Info -> KeySetName ->  Eval e ()
enforceKeySetName mi mksn = do
  ks <- maybe (evalError mi $ "No such keyset: " ++ show mksn) return =<< readRow mi KeySets mksn
  runPure $ enforceKeySet mi (Just mksn) ks
{-# INLINE enforceKeySetName #-}

-- | Enforce keyset against environment
enforceKeySet :: Info ->
             Maybe KeySetName -> KeySet -> Eval (Purity e) ()
enforceKeySet i ksn KeySet{..} = do
  sigs <- view eeMsgSigs
  let count = length _ksKeys
      matched = S.size $ S.intersection (S.fromList _ksKeys) sigs
      failed = failTx i $ "Keyset failure (" ++ show _ksPredFun ++ ")" ++ maybe "" (": " ++) (fmap show ksn)
      runBuiltIn p | p count matched = return ()
                   | otherwise = failed
      atLeast t m = m >= t
  case M.lookup _ksPredFun keyPredBuiltins of
    Just KeysAll -> runBuiltIn (\c m -> atLeast c m)
    Just KeysAny -> runBuiltIn (\_ m -> atLeast 1 m)
    Just Keys2 -> runBuiltIn (\_ m -> atLeast 2 m)
    Nothing -> do
      let app = TApp (TVar (Name _ksPredFun def) def)
                [toTerm count,toTerm matched] i
      app' <- instantiate' <$> resolveFreeVars i (abstract (const Nothing) app)
      r <- reduce app'
      case r of
        (TLiteral (LBool b) _) | b -> return ()
                               | otherwise -> failTx i $ "Keyset failure: " ++ maybe "[dynamic]" show ksn
        _ -> evalError i $ "Invalid response from keyset predicate: " ++ show r
{-# INLINE enforceKeySet #-}

-- Hoist Name back to ref
liftTerm :: Term Name -> Term Ref
liftTerm a = TVar (Direct a) def

-- | Re-application of 'f as' with additional args.
apply :: Term Ref -> [Term Ref] -> Info -> [Term Name] ->  Eval e (Term Name)
apply f as i as' = reduce (TApp f (as ++ map liftTerm as') i)

-- | Unsafe version of 'apply' where first arg is assumed to be a 'TApp',
-- to which additional args are applied.
apply' :: Term Ref -> [Term Name] -> Eval e (Term Name)
apply' app as' = apply (_tAppFun app) (_tAppArgs app) (_tInfo app) as'


-- | Evaluate top-level term.
eval ::  Term Name ->  Eval e (Term Name)
eval (TUse mn h i) = evalUse mn h i >> return (tStr $ pack $ "Using " ++ show mn)


eval t@(TModule m bod i) = do
  -- enforce old module keysets
  oldM <- readRow i Modules (_mName m)
  case oldM of
    Nothing -> return ()
    Just om -> enforceKeySetName i (_mKeySet om)
  -- enforce new module keyset
  enforceKeySetName i (_mKeySet m)
  -- build/install module from defs
  _defs <- call (StackFrame (pack $ abbrev t) i Nothing) $ loadModule m bod i
  writeRow i Write Modules (_mName m) m
  return $ msg $ pack $ "Loaded module " ++ show (_mName m) ++ ", hash " ++ show (_mHash m)

eval t = enscope t >>= reduce


evalUse :: ModuleName -> Maybe Hash -> Info -> Eval e ()
evalUse mn h i = do
  mm <- HM.lookup mn <$> view (eeRefStore.rsModules)
  case mm of
    Nothing -> evalError i $ "Module " ++ show mn ++ " not found"
    Just m@(Module{..},_) -> do
      case h of
        Nothing -> return ()
        Just mh | mh == _mHash -> return ()
                | otherwise -> evalError i $ "Module " ++ show mn ++ " does not match specified hash: " ++
                               show mh ++ ", " ++ show _mHash
      installModule m


-- | Make table of module definitions for storage in namespace/RefStore.
--
-- Definitions are transformed such that all free variables are resolved either to
-- an existing ref in the refstore/namespace ('Right Ref'), or a symbol that must
-- resolve to a definition in the module ('Left String'). A graph is formed from
-- all 'Left String' entries and enforced as acyclic, proving the definitions
-- to be non-recursive. The graph is walked to unify the Either to
-- the 'Ref's it already found or a fresh 'Ref' that will have already been added to
-- the table itself: the topological sort of the graph ensures the reference will be there.
loadModule :: Module -> Scope n Term Name -> Info ->
              Eval e (HM.HashMap Text (Term Name))
loadModule m bod1 mi = do
  modDefs1 <-
    case instantiate' bod1 of
      (TList bd _ _bi) ->
        fmap (HM.fromList . concat) $ forM bd $ \t -> do
          dnm <- case t of
            TDef {..} -> return $ Just _tDefName
            TNative {..} -> return $ Just $ asString _tNativeName
            TConst {..} -> return $ Just $ _aName _tConstArg
            TSchema {..} -> return $ Just $ asString _tSchemaName
            TTable {..} -> return $ Just $ asString _tTableName
            TUse {..} -> evalUse _tModuleName _tModuleHash _tInfo >> return Nothing
            _ -> evalError (_tInfo t) "Invalid module member"
          return $ maybe [] (\dn -> [(dn,t)]) dnm
      t -> evalError (_tInfo t) "Malformed module"
  cs :: [SCC (Term (Either Text Ref), Text, [Text])] <-
    fmap stronglyConnCompR $ forM (HM.toList modDefs1) $ \(dn,d) ->
      call (StackFrame (pack $ abbrev d) (_tInfo d) Nothing) $
      do
        d' <- forM d $ \(f :: Name) -> do
                dm <- resolveRef f
                case (dm,f) of
                  (Just t,_) -> return (Right t)
                  (Nothing,Name fn _) ->
                      case HM.lookup fn modDefs1 of
                        Just _ -> return (Left fn)
                        Nothing -> evalError (_nInfo f) ("Cannot resolve \"" ++ show f ++ "\"")
                  (Nothing,_) -> evalError (_nInfo f) ("Cannot resolve \"" ++ show f ++ "\"")
        return (d',dn,mapMaybe (either Just (const Nothing)) $ toList d')
  sorted <- forM cs $ \c -> case c of
              AcyclicSCC v -> return v
              CyclicSCC vs -> evalError mi $ "Recursion detected: " ++ show vs
  let defs :: HM.HashMap Text Ref
      defs = foldl dresolve HM.empty sorted
      -- insert a fresh Ref into the map, fmapping the Either to a Ref via 'unify'
      dresolve ds (d,dn,_) = HM.insert dn (Ref (fmap (unify ds) d)) ds
  installModule (m,defs)
  (evalRefs.rsNew) %= ((_mName m,(m,defs)):)
  return modDefs1



resolveRef :: Name -> Eval e (Maybe Ref)
resolveRef qn@(QName q n _) = do
          dsm <- firstOf (eeRefStore.rsModules.ix q._2.ix n) <$> ask
          case dsm of
            d@Just {} -> return d
            Nothing -> firstOf (evalRefs.rsLoaded.ix qn) <$> get
resolveRef nn@(Name _ _) = do
          nm <- firstOf (eeRefStore.rsNatives.ix nn) <$> ask
          case nm of
            d@Just {} -> return d
            Nothing -> firstOf (evalRefs.rsLoaded.ix nn) <$> get


unify :: HM.HashMap Text Ref -> Either Text Ref -> Ref
unify _ (Right d) = d
unify m (Left f) = m HM.! f

deref :: Ref -> Eval e (Term Name)
deref (Direct n) = return n
deref (Ref r) = reduce r

-- | Only can be used by "static" terms with no refs/variables in them
unsafeReduce :: Term Ref -> Eval e (Term Name)
unsafeReduce t = return (t >>= const (tStr "Error: unsafeReduce on non-static term"))


-- | Main function for reduction/evaluation.
reduce :: Term Ref ->  Eval e (Term Name)
reduce (TApp f as ai) = reduceApp f as ai
reduce (TVar t _) = deref t
reduce t@TLiteral {} = unsafeReduce t
reduce t@TKeySet {} = unsafeReduce t
reduce t@TValue {} = unsafeReduce t
reduce TList {..} = TList <$> mapM reduce _tList <*> traverse reduce _tListType <*> pure _tInfo
reduce t@TDef {} = return $ toTerm $ pack $ show t
reduce t@TNative {} = return $ toTerm $ pack $ show t
reduce (TConst _ _ t _ _) = reduce t
reduce (TObject ps t i) =
  TObject <$> forM ps (\(k,v) -> (,) <$> reduce k <*> reduce v) <*> traverse reduce t <*> pure i
reduce (TBinding ps bod c i) = case c of
  BindLet -> reduceLet ps bod i
  BindSchema _ -> evalError i "Unexpected schema binding"
reduce t@TModule {} = evalError (_tInfo t) "Module only allowed at top level"
reduce t@TUse {} = evalError (_tInfo t) "Use only allowed at top level"
reduce t@TStep {} = evalError (_tInfo t) "Step at invalid location"
reduce TSchema {..} = TSchema _tSchemaName _tModule _tDocs <$> traverse (traverse reduce) _tFields <*> pure _tInfo
reduce TTable {..} = TTable _tTableName _tModule <$> mapM reduce _tTableType <*> pure _tDocs <*> pure _tInfo

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
resolveArg ai as i = fromMaybe (appError ai $ pack $ "Missing argument value at index " ++ show i) $
                     as `atMay` i

reduceApp :: Term Ref -> [Term Ref] -> Info ->  Eval e (Term Name)
reduceApp (TVar (Direct t) _) as ai = reduceDirect t as ai
reduceApp (TVar (Ref r) _) as ai = reduceApp r as ai
reduceApp TDef {..} as ai = do
  as' <- mapM reduce as
  ft' <- traverse reduce _tFunType
  typecheck (zip (_ftArgs ft') as')
  let bod' = instantiate (resolveArg ai (map mkDirect as')) _tDefBody
      fa = FunApp _tInfo _tDefName (Just _tModule) _tDefType (funTypes ft') _tDocs
  call (StackFrame _tDefName ai (Just (fa,map (pack.abbrev) as))) $
    case _tDefType of
      Defun -> reduceBody bod'
      Defpact -> applyPact bod'
reduceApp (TLitString errMsg) _ i = evalError i $ unpack errMsg
reduceApp r _ ai = evalError ai $ "Expected def: " ++ show r

reduceDirect :: Term Name -> [Term Ref] -> Info ->  Eval e (Term Name)
reduceDirect TNative {..} as ai =
  _nativeFun _tNativeFun
  (FunApp ai (asString _tNativeName) Nothing Defun _tFunTypes (Just _tNativeDocs)) as
reduceDirect (TLitString errMsg) _ i = evalError i $ unpack errMsg
reduceDirect r _ ai = evalError ai $ "Unexpected non-native direct ref: " ++ show r

-- | Apply a pactdef, which will execute a step based on env 'PactStep'
-- defaulting to the first step.
applyPact ::  Term Ref ->  Eval e (Term Name)
applyPact (TList ss _ i) = do
  (stepIdx,doRollback) <- maybe (0,False) (_psStep &&& _psRollback) <$> view eePactStep
  s <- maybe (evalError i $ "applyPact: step not found: " ++ show stepIdx) return $ ss `atMay` stepIdx
  use evalYield >>= \badY -> unless (isNothing badY) $ evalError i "Nested pact, aborting"
  case s of
    ts@TStep {} -> do
      se <- traverse reduce (_tStepEntity ts)
      let execStep = do
              evalYield .= Just (PactYield (length ss) Nothing True)
              if doRollback
                then case _tStepRollback ts of
                       Nothing -> return $ tStr $ pack $ "No rollback on step " ++ show stepIdx
                       Just rexp -> reduce rexp
                else reduce $ _tStepExec ts
      case se of
        Just (TLitString stepEnt) -> do
          (EntityName en) <- view eeEntity
          if stepEnt == en
            then execStep
            else do
              evalYield .= Just (PactYield (length ss) Nothing False)
              return $ tStr "Skip step"
        Just t -> evalError (_tInfo t) "step entity must be String value"
        Nothing -> execStep
    t -> evalError (_tInfo t) "expected step"
applyPact t = evalError (_tInfo t) "applyPact: expected list of steps"


-- | Create special error form handled in 'reduceApp'
appError :: Info -> Text -> Term n
appError i errMsg = TApp (toTerm errMsg) [] i

resolveFreeVars ::  Info -> Scope d Term Name ->  Eval e (Scope d Term Ref)
resolveFreeVars i b = traverse r b where
    r fv = resolveRef fv >>= \m -> case m of
             Nothing -> evalError i $ "Cannot resolve " ++ show fv
             Just d -> return d

installModule :: ModuleData ->  Eval e ()
installModule (m,defs) = do
  (evalRefs.rsLoaded) %= HM.union (HM.fromList . map (first (`Name` def)) . HM.toList $ defs)
  (evalRefs.rsLoadedModules) %= HM.insert (_mName m) m

msg :: Text -> Term n
msg = toTerm

enscope ::  Term Name ->  Eval e (Term Ref)
enscope t = instantiate' <$> (resolveFreeVars (_tInfo t) . abstract (const Nothing) $ t)

instantiate' :: Scope n Term a -> Term a
instantiate' = instantiate1 (toTerm ("No bindings" :: Text))

-- | Runtime input typecheck -- let bindings and defuns.
-- Output checking -- defconsts, function return values -- left to static TC.
-- Native funs TC via pattern-matching etc.
typecheck :: [(Arg (Term Name),Term Name)] -> Eval e ()
typecheck ps = void $ foldM tvarCheck M.empty ps where
  tvarCheck m (Arg {..},t) = do
    r <- check1 _aInfo _aType t
    case r of
      Nothing -> return m
      Just (v,ty) -> case M.lookup v m of
        Nothing -> return $ M.insert v ty m
        Just prevTy | prevTy == ty -> return m
                    | otherwise ->
                        evalError (_tInfo t) $ "Type error: values for variable " ++ show _aType ++
                        " do not match: " ++ show (prevTy,ty)

check1 :: forall e . Info -> Type (Term Name) -> Term Name -> Eval e (Maybe (TypeVar (Term Name),Type (Term Name)))
check1 i spec t = do
  ty <- case typeof t of
    Left s -> evalError i $ "Invalid type in value location: " ++ unpack s
    Right r -> return r
  let
    tcFail :: Show a => a -> Eval e b
    tcFail found = evalError i $ "Type error: expected " ++ show spec ++ ", found " ++ show found
    tcOK = return Nothing
    paramCheck :: Eq t => Type t -> Type t -> (Type t -> Eval e (Type (Term Name))) ->
                  Eval e (Maybe (TypeVar (Term Name),Type (Term Name)))
    paramCheck TyAny _ _ = tcOK
    paramCheck TyVar {} _ _ = tcOK
    paramCheck pspec pty check | pspec == pty = tcOK -- dupe check as below, for totality
                               | not (isUnconstrainedTy pty) = tcFail ty -- unequal constrained fails
                               | otherwise = do
                                   checked <- check pspec
                                   if checked == spec then tcOK else tcFail checked
    checkList [] lty = return lty
    checkList es lty = return $ TyList $
                    case nub (map typeof es) of
                      [] -> lty
                      [Right a] -> a
                      _ -> TyAny
  case (spec,ty,t) of
    (_,_,_) | spec == ty -> tcOK -- identical types always OK
    (TyAny,_,_) -> tcOK -- var args are untyped
    (TyVar {..},_,_) ->
      if spec `canUnifyWith` ty
      then return $ Just (_tyVar,ty) -- collect found types under vars
      else tcFail ty -- constraint failed
    (TyList lspec,TyList lty,TList {..}) -> paramCheck lspec lty (checkList _tList)
    (TySchema TyObject ospec,TySchema TyObject oty,TObject {..}) -> paramCheck ospec oty (checkUserType True i _tObject)
    _ -> tcFail ty


checkUserType :: Bool -> Info  -> [(Term Name,Term Name)] -> Type (Term Name) -> Eval e (Type (Term Name))
checkUserType total i ps (TyUser tu@TSchema {..}) = do
  let uty = M.fromList . map (_aName &&& id) $ _tFields
  aps <- forM ps $ \(k,v) -> case k of
    TLitString ks -> case M.lookup ks uty of
      Nothing -> evalError i $ "Invalid field for {" ++ unpack (asString _tSchemaName) ++ "}: " ++ show ks
      Just a -> return (a,v)
    t -> evalError i $ "Invalid object, non-String key found: " ++ show t
  when total $ do
    let missing = M.difference uty (M.fromList (map (first _aName) aps))
    unless (M.null missing) $ evalError i $ "Missing fields for {" ++ unpack (asString _tSchemaName) ++ "}: " ++ show (M.elems missing)
  typecheck aps
  return $ TySchema TyObject (TyUser tu)
checkUserType _ i _ t = evalError i $ "Invalid reference in user type: " ++ show t

-- | Drop into pure execution, preventing database access, but potentially allowing system table reads
runPureSys :: Bool -> Eval (Purity e) a -> Eval e a
runPureSys allowSys a = do
  e <- purify allowSys =<< ask
  s <- get
  (o,_s) <- liftIO $ runEval' s e a
  either throwM return o

-- | Drop into pure execution, preventing database access.
runPure :: Eval (Purity e) a -> Eval e a
runPure = runPureSys False

newtype Purity e = Purity (EvalEnv e)

purity :: Bool -> PactDb (Purity e)
purity allowSys = PactDb {
      _readRow = if allowSys then \d k e -> case d of
                   KeySets -> withMVar e $ \(Purity EvalEnv {..}) -> _readRow _eePactDb d k _eePactDbVar
                   Modules -> withMVar e $ \(Purity EvalEnv {..}) -> _readRow _eePactDb d k _eePactDbVar
                   _ -> diePure e
                 else \_ _ -> diePure
    , _writeRow = \_ _ _ _ -> diePure
    , _keys = const diePure
    , _txids = \_ _ -> diePure
    , _createUserTable = \_ _ _ -> diePure
    , _getUserTableInfo = const diePure
    , _beginTx = const diePure
    , _commitTx = diePure
    , _rollbackTx = diePure
    , _getTxLog = \_ _ -> diePure
    }
       where diePure _ = evalError def "Illegal database access in pure context"

purify :: Bool -> EvalEnv e -> Eval e (EvalEnv (Purity e))
purify allowSys e@EvalEnv{..} = do
  v <- liftIO $ newMVar (Purity e)
  return $ EvalEnv
    _eeRefStore
    _eeMsgSigs
    _eeMsgBody
    _eeTxId
    _eeEntity
    _eePactStep
    v
    (purity allowSys)
