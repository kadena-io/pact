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
    ,reduce,resolveFreeVars,resolveArg
    ,enforceKeySet,enforceKeySetName
    ,checkUserType
    ,deref
    ) where

import Control.Lens hiding (op)
import Control.Applicative
import Data.List
import Control.Monad
import Prelude hiding (exp,mod)
import Bound
import Pact.Types
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import Safe
import Data.Default
import Control.Arrow hiding (app)
import Data.Maybe
import Pact.Compile
import Text.Trifecta as TF
import Data.Foldable
import Data.Graph
import qualified Data.Set as S
import Control.Monad.State.Strict
import Control.Monad.Reader

evalBeginTx :: Eval e ()
evalBeginTx = beginTx
{-# INLINE evalBeginTx #-}

evalRollbackTx ::   Eval e ()
evalRollbackTx = void rollbackTx
{-# INLINE evalRollbackTx #-}

evalCommitTx ::   Eval e ()
evalCommitTx = commitTx =<< view eeTxId
{-# INLINE evalCommitTx #-}


enforceKeySetName ::  Info -> KeySetName ->  Eval e ()
enforceKeySetName mi mksn = do
  ks <- maybe (evalError mi $ "No such keyset: " ++ show mksn) return =<< readRow KeySets mksn
  enforceKeySet mi (Just mksn) ks
{-# INLINE enforceKeySetName #-}


-- | Enforce keyset against environment
enforceKeySet :: Info ->
             Maybe KeySetName -> PactKeySet -> Eval e ()
enforceKeySet i ksn ks = do
  sigs <- view eeMsgSigs
  let keys' = _pksKeys ks
      matched = S.size $ S.intersection (S.fromList keys') sigs
      app = TApp (TVar (Name $ _pksPredFun ks) def)
            [toTerm (length keys'),toTerm matched] i
  app' <- instantiate' <$> resolveFreeVars i (abstract (const Nothing) app)
  r <- reduce app'
  case r of
    (TLiteral (LBool b) _) | b -> return ()
                           | otherwise -> failTx $ "Keyset failure: " ++ maybe "[dynamic]" show ksn
    _ -> evalError i $ "Invalid response from keyset predicate: " ++ show r
{-# INLINE enforceKeySet #-}



-- | Evaluate top-level term.
eval ::  Term Name ->  Eval e (Term Name)
eval (TUse mn i) = do
  mm <- HM.lookup mn <$> view (eeRefStore.rsModules)
  case mm of
    Nothing -> evalError i $ "Module " ++ show mn ++ " not found"
    Just m -> installModule m >> return (tStr $ "Using " ++ show mn)


eval t@(TModule m bod i) = do
  -- enforce old module keysets
  oldM <- readRow Modules (_mName m)
  case oldM of
    Nothing -> return ()
    Just om -> enforceKeySetName i (_mKeySet om)
  -- enforce new module keyset
  enforceKeySetName i (_mKeySet m)
  -- build/install module from defs
  _defs <- call (StackFrame (abbrev t) i Nothing) $ loadModule m bod i
  writeRow Write Modules (_mName m) m
  return $ msg $ "Loaded module " ++ show (_mName m)

eval t = enscope t >>= reduce

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
              Eval e (HM.HashMap String (Term Name))
loadModule m bod1 mi = do
  modDefs1 <-
    case instantiate' bod1 of
      (TList bd _ _bi) ->
        HM.fromList <$> forM bd
          (\t -> do
              dn <- case t of
                TDef {..} -> return _tDefName
                TNative {..} -> return $ asString _tNativeName
                TConst {..} -> return $ _aName _tConstArg
                TUserType {..} -> return $ asString _tUserTypeName
                TTable {..} -> return $ asString _tTableName
                _ -> evalError (_tInfo t) "Invalid module member"
              return (dn,t))
      t -> evalError (_tInfo t) "Malformed module"
  cs :: [SCC (Term (Either String Ref), String, [String])] <-
    fmap stronglyConnCompR $ forM (HM.toList modDefs1) $ \(dn,d) ->
      call (StackFrame (abbrev d) (_tInfo d) Nothing) $
      do
        d' <- forM d $ \(f :: Name) -> do
                dm <- resolveRef f
                case (dm,f) of
                  (Just t,_) -> return (Right t)
                  (Nothing,Name fn) ->
                      case HM.lookup fn modDefs1 of
                        Just _ -> return (Left fn)
                        Nothing -> evalError (_tInfo d) ("Cannot resolve \"" ++ show f ++ "\"")
                  (Nothing,_) -> evalError (_tInfo d) ("Cannot resolve \"" ++ show f ++ "\"")
        return (d',dn,mapMaybe (either Just (const Nothing)) $ toList d')
  sorted <- forM cs $ \c -> case c of
              AcyclicSCC v -> return v
              CyclicSCC vs -> evalError mi $ "Recursion detected: " ++ show vs
  let defs :: HM.HashMap String Ref
      defs = foldl dresolve HM.empty sorted
      -- insert a fresh Ref into the map, fmapping the Either to a Ref via 'unify'
      dresolve ds (d,dn,_) = HM.insert dn (Ref (fmap (unify ds) d)) ds
  installModule (m,defs)
  (evalRefs.rsNew) %= ((_mName m,(m,defs)):)
  return modDefs1



resolveRef :: Name -> Eval e (Maybe Ref)
resolveRef qn@(QName q n) = do
          dsm <- firstOf (eeRefStore.rsModules.ix q._2.ix n) <$> ask
          case dsm of
            d@Just {} -> return d
            Nothing -> firstOf (evalRefs.rsLoaded.ix qn) <$> get
resolveRef nn@(Name n) = do
          nm <- firstOf (eeRefStore.rsNatives.ix n) <$> ask
          case nm of
            d@Just {} -> return d
            Nothing -> firstOf (evalRefs.rsLoaded.ix nn) <$> get


unify :: HM.HashMap String Ref -> Either String Ref -> Ref
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
reduce (TList bs _ _) = last <$> mapM reduce bs -- note "body" usage here, bug?
reduce t@TDef {} = return $ toTerm $ show t
reduce t@TNative {} = return $ toTerm $ show t
reduce (TConst _ _ t _ _) = reduce t
reduce (TObject ps t i) =
  TObject <$> forM ps (\(k,v) -> (,) <$> reduce k <*> reduce v) <*> traverse reduce t <*> pure i
reduce (TBinding ps bod c i) = case c of
  BindLet -> reduceLet ps bod i
  BindKV -> evalError i "Unexpected key-value binding"
reduce t@TModule {} = evalError (_tInfo t) "Module only allowed at top level"
reduce t@TUse {} = evalError (_tInfo t) "Use only allowed at top level"
reduce t@TStep {} = evalError (_tInfo t) "Step at invalid location"
reduce TUserType {..} = TUserType _tUserTypeName _tModule _tDocs <$> traverse (traverse reduce) _tFields <*> pure _tInfo
reduce TTable {..} = TTable _tTableName _tModule <$> mapM reduce _tTableType <*> pure _tDocs <*> pure _tInfo

mkDirect :: Term Name -> Term Ref
mkDirect = (`TVar` def) . Direct

reduceLet :: [(Arg (Term Ref),Term Ref)] -> Scope Int Term Ref -> Info -> Eval e (Term Name)
reduceLet ps bod i = do
  ps' <- mapM (\(a,t) -> (,) <$> traverse reduce a <*> reduce t) ps
  typecheck ps'
  reduce (instantiate (resolveArg i (map (mkDirect . snd) ps')) bod)


{-# INLINE resolveArg #-}
resolveArg :: Info -> [Term n] -> Int -> Term n
resolveArg ai as i = fromMaybe (appError ai $ "Missing argument value at index " ++ show i) $
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
  call (StackFrame _tDefName ai (Just (fa,map abbrev as))) $
    case _tDefType of
      Defun -> reduce bod'
      Defpact -> applyPact bod'
reduceApp (TLitString errMsg) _ i = evalError i errMsg
reduceApp r _ ai = evalError ai $ "Expected def: " ++ show r

reduceDirect :: Term Name -> [Term Ref] -> Info ->  Eval e (Term Name)
reduceDirect TNative {..} as ai =
  _nativeFun _tNativeFun
  (FunApp ai (asString _tNativeName) Nothing Defun _tFunTypes (Just _tNativeDocs)) as
reduceDirect (TLitString errMsg) _ i = evalError i errMsg
reduceDirect r _ ai = evalError ai $ "Unexpected non-native direct ref: " ++ show r

-- | Apply a pactdef, which will execute a step based on env 'PactStep'
-- defaulting to the first step.
applyPact ::  Term Ref ->  Eval e (Term Name)
applyPact (TList ss _ i) = do
  steps <- forM ss $ \s ->
           case s of
             (TStep ent e r si) -> return (ent,e,r,si)
             t -> evalError (_tInfo t) "applyPact: expected step"
  (idx,doRollback) <- maybe (0,False) (_psStep &&& _psRollback) <$> view eePactStep
  (ent,ex,rb,si) <- maybe (evalError i $ "applyPact: step not found: " ++ show idx) return $
                    steps `atMay` idx
  entr <- reduce ent
  us <- view eeEntity
  case entr of
    (TLitString target)
        | target /= us -> return (tStr $ "Skip step " ++ show idx)
        | otherwise ->
            case (doRollback,rb) of
              (False,_) -> do
                 let next | succ idx < length steps = Just (succ idx)
                          | otherwise = Nothing
                     rbi = fmap (const idx) rb
                 evalStep ex (PactYield next rbi)
              (True,Just r) -> evalStep r def
              (True,Nothing) -> evalError si "applyPact: expected rollback"
    t -> evalError (_tInfo t) "step entity must be String value"
applyPact t = evalError (_tInfo t) "applyPact: expected list of steps"

-- | Evaluate pact steps to apply one if found for this identity.
evalStep ::  Term Ref -> PactYield ->  Eval e (Term Name)
evalStep exp yield = do
  oldy <- use evalYield
  case oldy of
    Just _ -> evalError (_tInfo exp) "Yield set more than once, aborting pact"
    Nothing -> do
              evalYield .= Just yield
              reduce exp

-- | Create special error form handled in 'reduceApp'
appError :: Info -> String -> Term n
appError i errMsg = TApp (toTerm errMsg) [] i

resolveFreeVars ::  Info -> Scope d Term Name ->  Eval e (Scope d Term Ref)
resolveFreeVars i b = traverse r b where
    r fv = resolveRef fv >>= \m -> case m of
             Nothing -> evalError i $ "Cannot resolve " ++ show fv
             Just d -> return d

installModule :: ModuleData ->  Eval e ()
installModule (m,defs) = do
  (evalRefs.rsLoaded) %= HM.union (HM.fromList . map (first Name) . HM.toList $ defs)
  (evalRefs.rsLoadedModules) %= HM.insert (_mName m) m

msg :: String -> Term n
msg = toTerm

enscope ::  Term Name ->  Eval e (Term Ref)
enscope t = instantiate' <$> (resolveFreeVars (_tInfo t) . abstract (const Nothing) $ t)

instantiate' :: Scope n Term a -> Term a
instantiate' = instantiate1 (toTerm ("No bindings" :: String))

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
    Left s -> evalError i $ "Invalid type in value location: " ++ s
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
                               | (not (isUnconstrainedTy pty)) = tcFail ty -- unequal constrained fails
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
checkUserType total i ps (TyUser tu@TUserType {..}) = do
  let uty = M.fromList . map (_aName &&& id) $ _tFields
  aps <- forM ps $ \(k,v) -> case k of
    TLitString ks -> case M.lookup ks uty of
      Nothing -> evalError i $ "Invalid field for {" ++ asString _tUserTypeName ++ "}: " ++ ks
      Just a -> return (a,v)
    t -> evalError i $ "Invalid object, non-String key found: " ++ show t
  when total $ do
    let missing = M.difference uty (M.fromList (map (first _aName) aps))
    unless (M.null missing) $ evalError i $ "Missing fields for {" ++ asString _tUserTypeName ++ "}: " ++ show (M.elems missing)
  typecheck aps
  return $ TySchema TyObject (TyUser tu)
checkUserType _ i _ t = evalError i $ "Invalid reference in user type: " ++ show t


_compile :: String -> Term Name
_compile s = let (TF.Success f) = TF.parseString expr mempty s
                 (Right t) = compile f
             in t
