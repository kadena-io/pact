{-# LANGUAGE TemplateHaskell #-}
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
    ) where

import Control.Lens hiding (op)
import Control.Applicative
import Data.List
import Control.Monad
import Prelude hiding (exp,mod)
import Bound
import Pact.Types
import qualified Data.HashMap.Strict as M
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
      app = TApp (TVar (Name $ _pksPredFun ks) def) [toTerm (length keys'),toTerm matched] i
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
  mm <- M.lookup mn <$> view (eeRefStore.rsModules)
  case mm of
    Nothing -> evalError i $ "Module " ++ show mn ++ " not found"
    Just m -> installModule m >> return (tStr $ "Using " ++ show mn)


eval t@(TModule mn mksn _md bod mc mi) = do
  -- enforce old module keysets
  oldM <- readRow Modules mn
  case oldM of
    Nothing -> return ()
    Just (Module _ omksn _) -> enforceKeySetName mi omksn
  -- enforce new module keyset
  enforceKeySetName mi mksn
  -- build/install module from defs
  _defs <- call (StackFrame Nothing mi (abbrev t) []) $ loadModule mn bod mi
  writeRow Write Modules mn (Module mn mksn (show mc))
  return $ msg $ "Loaded module " ++ show mn

eval t = enscope t >>= reduce

loadModule :: ModuleName -> Scope n Term Name -> Info ->
              Eval e (M.HashMap String (Term Name))
loadModule mn bod1 mi = do
  modDefs1 <-
    case instantiate' bod1 of
      (TList bd _bi) ->
        M.fromList <$> forM bd (\t ->
            case t of
              TDef dd _ _ _ -> return (_dName dd,set (tDefData.dModule) (Just mn) t)
              TNative dd _ _ -> return (_dName dd,set (tDefData.dModule) (Just mn) t)
              TConst dd _ _ -> return (_dName dd,set (tDefData.dModule) (Just mn) t)
              _ -> evalError (_tInfo t) "Non-def in module body")
      t -> evalError (_tInfo t) "Malformed module"
  cs <-
    fmap stronglyConnCompR $ forM (M.toList modDefs1) $ \(dn,d) ->
      call (StackFrame Nothing (_tInfo d) (abbrev d) []) $
      do
        d' <- forM d $ \f -> do
                dm <- resolveRef f
                case (dm,f) of
                  (Just t,_) -> return (Right t)
                  (Nothing,Name fn) ->
                      case M.lookup fn modDefs1 of
                        Just _ -> return (Left fn)
                        Nothing -> evalError (_tInfo d) ("Cannot resolve \"" ++ show f ++ "\"")
                  (Nothing,_) -> evalError (_tInfo d) ("Cannot resolve \"" ++ show f ++ "\"")
        return (d',dn,mapMaybe (either Just (const Nothing)) $ toList d')
  sorted <- forM cs $ \c -> case c of
              AcyclicSCC v -> return v
              CyclicSCC vs -> evalError mi $ "Recursion detected: " ++ show vs
  let defs :: M.HashMap String Ref
      defs = foldl dresolve M.empty sorted
      dresolve m (d,dn,_) = M.insert dn (Ref (fmap (unify m) d)) m
  installModule defs
  (evalRefs.rsNew) %= ((mn,defs):)
  return modDefs1



resolveRef :: Name -> Eval e (Maybe Ref)
resolveRef qn@(QName q n) = do
          dsm <- firstOf (eeRefStore.rsModules.ix q.ix n) <$> ask
          case dsm of
            d@Just {} -> return d
            Nothing -> firstOf (evalRefs.rsLoaded.ix qn) <$> get
resolveRef nn@(Name n) = do
          nm <- firstOf (eeRefStore.rsNatives.ix n) <$> ask
          case nm of
            d@Just {} -> return d
            Nothing -> firstOf (evalRefs.rsLoaded.ix nn) <$> get


unify :: M.HashMap String Ref -> Either String Ref -> Ref
unify _ (Right d) = d
unify m (Left f) = m M.! f


-- | Recursive reduction.
reduce ::  Term Ref ->  Eval e (Term Name)
reduce (TApp f as ai) = reduceApp f as ai
reduce (TVar t _) = case t of Direct n -> return n; Ref r -> reduce r
reduce (TLiteral l i) = return $ TLiteral l i
reduce (TKeySet k i) = return $ TKeySet k i
reduce (TList bs _) = last <$> mapM reduce bs
reduce t@TDef {} = return $ toTerm $ show t
reduce t@TNative {} = return $ toTerm $ show t
reduce (TConst _ t _) = reduce t
reduce (TObject ps i) = forM ps (\(k,v) -> (,) <$> reduce k <*> reduce v) >>= \ps' -> return $ TObject ps' i
reduce (TBinding ps bod i) = reduceLet ps bod i
reduce t@TModule {} = evalError (_tInfo t) "Module only allowed at top level"
reduce t@TUse {} = evalError (_tInfo t) "Use only allowed at top level"
reduce t@TStep {} = evalError (_tInfo t) "Step at invalid location"
reduce (TValue v i) = return $ TValue v i

reduceLet :: [(String,Term Ref)] -> Scope Int Term Ref -> Info -> Eval e (Term Name)
reduceLet ps bod i = reduce (instantiate (resolveArg i (map snd ps)) bod)

{-# INLINE resolveArg #-}
resolveArg :: Info -> [Term n] -> Int -> Term n
resolveArg ai as i = fromMaybe (appError ai $ "Missing argument value at index " ++ show i) $
                     as `atMay` i

reduceApp :: Term Ref -> [Term Ref] -> Info ->  Eval e (Term Name)
reduceApp (TVar (Direct t) _) as ai = reduceDef t as ai
reduceApp (TVar (Ref r) _) as ai = reduceApp r as ai
reduceApp (TDef dd@(DefData _ dt dm dargs _) bod _exp _di) as ai = do
      let bod' = instantiate (resolveArg ai as) bod
      call (StackFrame dm ai (defName dd) (zip dargs (map abbrev as))) $
                     case dt of
                       Defun -> reduce bod'
                       Defpact -> applyPact bod'
                       Defconst -> evalError ai "Defconst in apply"
reduceApp (TLitString errMsg) _ i = evalError i errMsg
reduceApp r _ ai = evalError ai $ "Can only apply defs: " ++ show r

reduceDef ::  Term Name -> [Term Ref] -> Info ->  Eval e (Term Name)
reduceDef (TNative dd (NativeDFun _ ndd) _di) as ai = ndd (FunApp ai dd) as
reduceDef (TLitString errMsg) _ i = evalError i errMsg
reduceDef r _ ai = evalError ai $ "Can only apply defs: " ++ show r

-- | Apply a pactdef, which will execute a step based on env 'PactStep'
-- defaulting to the first step.
applyPact ::  Term Ref ->  Eval e (Term Name)
applyPact (TList ss i) = do
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



installModule ::  M.HashMap String Ref ->  Eval e ()
installModule defs = (evalRefs.rsLoaded) %= M.union (M.fromList . map (first Name) . M.toList $ defs)

msg :: String -> Term n
msg = toTerm

enscope ::  Term Name ->  Eval e (Term Ref)
enscope t = instantiate' <$> (resolveFreeVars (_tInfo t) . abstract (const Nothing) $ t)

instantiate' :: Scope n Term a -> Term a
instantiate' = instantiate1 (toTerm ("No bindings" :: String))

_compile :: String -> Term Name
_compile s = let (TF.Success f) = TF.parseString expr mempty s
                 (Right t) = compile f
             in t
