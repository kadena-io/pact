{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}


-- |
-- Module      :  Pact.Core.IR.Typecheck
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- HM type inference for core IR.
-- Todo: concrete types.
--
module Pact.Core.IR.Typecheck where

import Control.Lens
import Data.Set(Set)
import Data.Foldable(fold)
import Data.Map(Map)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Control.Monad.Except
import Control.Monad.Reader
-- import Control.Monad.State.Strict
import Pact.Core.Type
import Pact.Core.Names
import Pact.Core.IR.Term

data TyScheme n = TyScheme [n] [n] (Type n)
  deriving (Eq, Show)

data TCEnv name tyname builtin
  = TCEnv
  { _tcSupply :: Supply
  -- ^ Fresh name supply
  , _tcEnv :: Map.Map name (TyScheme tyname)
  -- ^ Typescheme environment
  , _tcNonGen :: Set.Set tyname
  -- ^ Monomorphic set to not generalize
  , _tcIface :: Map tyname (Map name (TyScheme tyname))
  -- ^ Map of iface name -> iface fields -> field types
  -- Necessary since the best we can do for dynamic module
  -- references is typecheck them nominally.
  , _tcLiftName :: Unique -> tyname
  -- ^ uniques babyyy
  , _tcBuiltin :: Map.Map builtin (TyScheme tyname)
  -- ^ builtin type schemes.
  }

makeLenses ''TCEnv

freshVar :: (MonadIO m, MonadReader (TCEnv name tyname builtin) m) => m tyname
freshVar = do
  f <- view tcLiftName
  s <- view tcSupply
  f <$> liftIO (newUnique s)

lookupTyEnv :: (Ord name, MonadReader (TCEnv name tyname builtin) m) => name -> m (TyScheme tyname)
lookupTyEnv n = do
  m <- view tcEnv
  pure $ m Map.! n

inTcEnv :: (MonadReader (TCEnv name tyname builtin) m, Ord name) => name -> TyScheme tyname -> m a -> m a
inTcEnv name typ = locally tcEnv (Map.insert name typ)

inTcEnvNonGen :: (MonadReader (TCEnv name tyname builtin) m, Ord tyname, Ord name) => name -> tyname -> TyScheme tyname -> m a -> m a
inTcEnvNonGen name tv typ =
  local $ \env -> env & tcNonGen %~ Set.insert tv & tcEnv %~ Map.insert name typ

data Constraint n
  = EqConst (Type n) (Type n)
  deriving (Eq, Show)


data Subst n
  = Subst
  { _stypes :: Map n (Type n)
  , _srows :: Map n (Row n)
  } deriving (Eq, Show)

data FreeTyVars n
  = FreeTyVars
  { _ftvTy :: Set n
  , _ftvRow :: Set n
  } deriving (Eq, Show)

makeLenses ''Subst
makeLenses ''FreeTyVars

ftvIntersection :: Ord n => FreeTyVars n -> FreeTyVars n -> FreeTyVars n
ftvIntersection (FreeTyVars l r) (FreeTyVars l' r') =
  FreeTyVars (Set.intersection l l') (Set.intersection r r')

instance Ord n => Semigroup (Subst n) where
  (Subst l r) <> (Subst l' r') = Subst (l <> l') (r <> r')

instance Ord n => Monoid (Subst n) where
  mempty = Subst mempty mempty

instance Ord n => Semigroup (FreeTyVars n) where
  (FreeTyVars l r) <> (FreeTyVars l' r') = FreeTyVars (l <> l') (r <> r')

instance Ord n => Monoid (FreeTyVars n) where
  mempty = FreeTyVars mempty mempty

class Substitutable p n | p -> n where
  subst :: Subst n -> p -> p

instance Ord n => Substitutable (Row n) n where
  subst s@(Subst _ rows) = \case
    RowVar r -> fromMaybe (RowVar r) $ rows ^. at r
    EmptyRow -> EmptyRow
    RowTy fields mv ->
      case mv of
        Nothing -> RowTy (subst s <$> fields) Nothing
        Just rv -> case rows ^. at rv of
          Just (RowVar rv') -> subst s (RowTy fields (Just rv'))
          Just (RowTy fields' rv') -> subst s (RowTy (Map.union fields fields') rv')
          Just EmptyRow -> RowTy (subst s <$> fields) Nothing
          _ -> RowTy (subst s <$> fields) mv

instance Ord n => Substitutable (Type n) n where
  subst s@(Subst tys rows) = \case
    TyVar n -> fromMaybe (TyVar n) $ tys ^. at n
    TyPrim p -> TyPrim p
    TyList ty -> TyList (subst s ty)
    TyRow r -> TyRow $ subst s r
    TyFun l r -> TyFun (subst s l) (subst s r)
    TyInterface n -> TyInterface n
    TyModule n -> TyModule n
    TyForall ns rs ty ->
      let tys' = Map.fromList [(n', TyVar n') | n' <- ns] `Map.union` tys
          rows' = Map.fromList [(r', RowVar r') | r' <- rs] `Map.union` rows
      in TyForall ns rs (subst (Subst tys' rows') ty)

instance Ord n => Substitutable (TyScheme n) n where
  subst m (TyScheme ns rs ty) =
    let m' = m  <> Subst (Map.fromList [(n', TyVar n') | n' <- ns]) (Map.fromList [(r', RowVar r') | r' <- rs])
    in TyScheme ns rs (subst m' ty)

instance Ord n => Substitutable (Constraint n) n where
   subst s (EqConst t1 t2) = EqConst (subst s t1) (subst s t2)

instance (Substitutable (t n) n) => Substitutable (Map k (t n)) n where
  subst s = fmap (subst s)

class FTV p n | p -> n where
  ftv :: p -> FreeTyVars n

instance Ord n => FTV (Map.Map k (Type n)) n where
  ftv = foldMap ftv

instance Ord tyname => FTV (Type tyname) tyname where
  ftv = \case
    TyVar n -> FreeTyVars (Set.singleton n) mempty
    TyPrim _ -> mempty
    TyFun l r -> ftv l <> ftv r
    TyRow rows -> ftv rows
    TyList t -> ftv t
    TyInterface _ -> mempty
    TyModule _ -> mempty
    TyForall ns rs typ ->
      let (FreeTyVars fts frs) = ftv typ
      in FreeTyVars (fts `Set.difference` Set.fromList ns) (frs `Set.difference` Set.fromList rs)

instance Ord tyname => FTV (Row tyname) tyname where
  ftv = \case
    RowTy m n -> ftv m <> maybe mempty (FreeTyVars mempty . Set.singleton) n
    RowVar n -> FreeTyVars mempty (Set.singleton n)
    EmptyRow -> mempty

instance Ord tyname => FTV (TyScheme tyname) tyname where
  ftv (TyScheme ns rs typ) =
    let (FreeTyVars tys rows) = ftv typ
    in FreeTyVars (tys `Set.difference` Set.fromList ns) (rows `Set.difference` Set.fromList rs)


----------------------------
-- Core Inference.
----------------------------

compose :: Ord tyname => Subst tyname -> Subst tyname -> Subst tyname
compose m1 m2 =
  let m2' = m2 & stypes . mapped %~ subst m1 & srows . mapped %~ subst m1
  in m2' <> m1

-- Occurs checking
bind :: (Ord tyname, MonadError [Char] f) => tyname -> Type tyname -> f (Subst tyname)
bind n t | t == TyVar n = pure mempty
         | occursCheck n t = throwError ""
         | otherwise = pure $ Subst (Map.singleton n t) mempty

-- todo: occurs check for rows.
bindRow :: (Ord tyname, MonadError String f) => tyname -> Row tyname -> f (Subst tyname)
bindRow n t | t == RowVar n = pure mempty
            | otherwise = pure $ Subst mempty (Map.singleton n t)

occursCheck :: (Ord n, FTV f n) => n -> f -> Bool
occursCheck n t = Set.member n $ _ftvTy (ftv t)

unifyRows
  ::Ord tyname
  => MonadError String m
  => MonadIO m
  => MonadReader (TCEnv name tyname builtin) m
  => Row tyname
  -> Row tyname
  -> m (Subst tyname)
unifyRows (RowVar n) t =  n `bindRow` t
unifyRows t (RowVar n) =  n `bindRow` t
-- Unify labels present in both m and m'
-- Labels not present: unify with row variable.
unifyRows (RowTy m mrv) (RowTy m' mrv') =
  case (mrv, mrv') of
    -- Two open rows
    (Just rv, Just rv') -> do
      unif <- fold <$> traverse (uncurry unifies) (Map.intersectionWith (,) m m')
      leftVar <- freshVar
      rightVar <- freshVar
      let notInR = Map.difference m m'
          notInRSubst = (rv,) $ if Map.null notInR then RowVar leftVar else RowTy notInR (Just leftVar)
          notInL = Map.difference m' m
          notInLSubst = (rv',) $ if Map.null notInL then RowVar rightVar else RowTy notInR (Just rightVar)
      pure $ unif <> Subst mempty (Map.fromList [notInRSubst, notInLSubst])
    -- Right closed
    -- Means Keys(l) <= Keys(r)
    (Just rv, Nothing) -> do
      if all (`Map.member` m') (Map.keys m) then do
        unif <- fold <$> traverse (uncurry unifies) (Map.intersectionWith (,) m m')
        let diff = Map.difference m' m
            s = if Map.null diff then EmptyRow else RowTy diff Nothing
        pure $ unif <> Subst mempty (Map.singleton rv s)
      else throwError "Row does not unify"
    -- Left closed
    (Nothing, Just rv) -> do
      if all (`Map.member` m) (Map.keys m') then do
        unif <- fold <$> traverse (uncurry unifies) (Map.intersectionWith (,) m m')
        let diff = Map.difference m m'
            s = if Map.null diff then EmptyRow else RowTy diff Nothing
        pure $ unif <> Subst mempty (Map.singleton rv s)
      else throwError "Row does not unify"
    (Nothing, Nothing) ->
      if Map.keys m == Map.keys m' then
        fold <$> traverse (uncurry unifies) (Map.intersectionWith (,) m m')
      else throwError "Row does not unify"
unifyRows EmptyRow EmptyRow = pure mempty
unifyRows _ _ = throwError "row unif mismatch"

--
unifyIfaceModule
  :: Ord tyname
  => MonadError String m
  => InterfaceType tyname -> ModuleType tyname -> m (Subst tyname)
unifyIfaceModule (InterfaceType n) (ModuleType _ ns) =
  if elem n ns then pure mempty
  else throwError "cannot unify interface/module"

-- note: For IR we currently don't unify against
-- `TyForall` unless strucurally equal.
-- We don't allow rankN despite it showing up in
-- our type language.
unifies ::
  ( Ord tyname
  , MonadError String m
  , MonadIO m
  , MonadReader (TCEnv name tyname builtin) m)
  => Type tyname
  -> Type tyname
  -> m (Subst tyname)
unifies t1 t2 | t1 == t2 = pure mempty
unifies (TyVar n) t2 = n `bind` t2
unifies t1 (TyVar n) = n `bind` t1
unifies (TyFun l r) (TyFun l' r') = do
  s1 <- unifies l l'
  s2 <- unifies (subst s1 r) (subst s1 r')
  pure (s2 `compose` s1)
unifies (TyList l) (TyList r) = unifies l r
unifies (TyRow l) (TyRow r) = unifyRows l r
unifies (TyInterface n) (TyModule m) = unifyIfaceModule n m
unifies (TyModule m) (TyInterface n) = unifyIfaceModule n m
unifies (TyInterface n) (TyInterface n') | n == n' = pure mempty
unifies (TyModule m) (TyModule m') | m == m' = pure mempty
unifies _ _ = error "reee"

generalize :: Ord tyname => FreeTyVars tyname -> Type tyname -> TyScheme tyname
generalize (FreeTyVars freetys freerows) t  = TyScheme as rs t
  where
  (FreeTyVars ftys frows) = ftv t
  as = Set.toList $ ftys`Set.difference` freetys
  rs = Set.toList $ frows `Set.difference` freerows

instantiate :: (MonadIO m, MonadReader (TCEnv name tyname builtin) m, Ord tyname) => TyScheme tyname -> m (Type tyname)
instantiate (TyScheme as rs t) = do
    as' <- traverse (const freshVar) as
    rs' <- traverse (const freshVar) rs
    let tyS = Map.fromList $ zip as (TyVar <$> as')
        rowS = Map.fromList $ zip rs (RowVar <$> rs')
    return $ subst (Subst tyS rowS) t

-- todo: propagate infos for better errors.
-- todo: quadratic appends. Optimize with `Seq`.
-- | Infer the type of a term, and annotate the node with the generated type
--   and all names with their respective type. In the case of let-binds
--   universally quantified types get paired with the name, as well as
--   for modref dynamic members.
inferTerm
  :: Ord tyname
  => Ord builtin
  => Ord name
  => MonadReader (TCEnv name tyname builtin) m
  => MonadIO m
  => MonadError String m
  => Term name tyname builtin info
  -> m ( Term (name, Type tyname) tyname builtin (info, Type tyname)
       , [Constraint tyname])
inferTerm = \case
  Constant l i -> do
    let ty = typeOfLit l
    pure (Constant l (i, ty), [])
  Var n i -> do
    ty <- instantiate =<< lookupTyEnv n
    pure (Var (n, ty) (i, ty), [])
  Lam n mty body i -> do
    rawTv <- freshVar
    let tv = TyVar rawTv
    -- If we see a type ann,
    -- unify with the type variable and emit a constraint.
    -- todo: It does mean we unify the constraint twice,
    -- probably unnecessary
    (csig, s) <- case mty of
      Nothing -> pure (mempty, mempty)
      Just ty -> ([EqConst ty tv],) <$> unifies ty tv
    let nty = TyScheme [] [] (subst s tv)
    -- if mty is present then probably `rawTv` is unnecessary.
    (body', cs) <- inTcEnvNonGen n rawTv nty $ inferTerm body
    let t = termTy body'
        lamArgTy = subst s tv
        outTy = TyFun lamArgTy t
    pure
      ( Lam (n, lamArgTy) mty body' (i, outTy)
      , csig++cs )
  Let n mty e1 e2 i -> do
    (e1', c1) <- inferTerm e1
    let t1 = termTy e1'
    csig <- case mty of
      Nothing -> pure mempty
      Just ty -> pure [EqConst ty t1]
    s <- solve (c1 ++ csig)
    ftvs <- ftvSubstNonGen s <$> view tcNonGen
    -- todo: we're CBV, so we need value restriction.
    let ntyp = generalize ftvs (subst s t1)
    (e2', c2) <- locally tcEnv (subst s) $ inTcEnv n ntyp $ inferTerm e2
    let t2 = termTy e2'
    pure
      ( Let (n, tsToTyForall ntyp) mty e1' e2' (i, t2)
      , c1 ++ csig ++ c2 )

  App e1 e2 i -> do
    tv <- TyVar <$> freshVar
    (e1', cs1) <- inferTerm e1
    (e2', cs2) <- inferTerm e2
    pure
      ( App e1' e2' (i, tv)
      , cs1 ++ cs2 ++ [EqConst (termTy e1') (TyFun (termTy e2') tv)] )
  Sequence e1 e2 i -> do
    -- Will we require that the lhs of sequenced statements be unit?
    -- likely yes, it doesn't make sense to otherwise discard value results without binding them in a clause.
    -- for now, no.
    -- We might emit a constraint `t1 ~ ()` here  eventually tho.
    (e1', cs1) <- inferTerm e1
    (e2', cs2) <- inferTerm e2
    pure
      ( Sequence e1' e2' (i, termTy e2')
      , cs1 ++ cs2 )
  -- Errors are just `forall a. a`.
  Error e i -> do
    tv <- TyVar <$> freshVar
    pure (Error e (i, tv), [])
  -- m::f
  DynAccess modref mem i -> do
    ty <- lookupTyEnv modref
    case ty of
      TyScheme [] [] (TyInterface (InterfaceType iface)) -> do
        ifaces <- view tcIface
        case ifaces ^? at iface . _Just . at mem . _Just of
          Just fn -> do
            typ <- instantiate fn
            let ifTyp = TyInterface (InterfaceType iface)
                fTyp = tsToTyForall fn
            pure (DynAccess (modref, ifTyp) (mem, fTyp) (i, typ), [])
          Nothing -> throwError "no such function in interface"
      _ -> throwError "not en interface"
  Builtin b i -> do
    benv <- view tcBuiltin
    bty <- instantiate $ benv Map.! b
    pure (Builtin b (i, bty), [])
  where
  ftvSubstNonGen unif nonGen = foldMap ftv $ subst unif . TyVar <$> Set.toList nonGen

tsToTyForall :: TyScheme tyname -> Type tyname
tsToTyForall (TyScheme as rs ty) = TyForall as rs ty

termTy :: Term name tyname builtin (info, Type tyname) -> Type tyname
termTy = view (termInfo._2)

----------------------------
-- Constraint solving
----------------------------

solve :: (Ord tyname, MonadError String m, MonadIO m, MonadReader (TCEnv name tyname builtin) m) => [Constraint tyname] -> m (Subst tyname)
solve = solve' mempty

solve' ::
  ( Ord tyname
  , MonadError String m
  , MonadIO m
  , MonadReader (TCEnv name tyname builtin) m)
  => Subst tyname
  -> [Constraint tyname]
  -> m (Subst tyname)
solve' s0 = \case
  [] -> return s0
  (EqConst t1 t2) : xs -> do
    s1 <- unifies t1 t2
    let s2 = s1 `compose` s2
    solve' s2 (fmap (subst s2) xs)

----------------------------
-- Running the typechecker
----------------------------

inferAndSolve
  :: MonadIO m
  => MonadError String m
  => MonadReader (TCEnv name tyname builtin) m
  => Ord tyname
  => Ord name
  => Ord builtin
  => Term name tyname builtin info
  -> m (Term (name, Type tyname) tyname builtin (info, Type tyname), Subst tyname)
inferAndSolve term = do
  (term', cs) <- inferTerm term
  s <- solve cs
  pure (over _2 (subst s) <$> term', s)


inferDefun
  :: MonadIO m
  => MonadError String m
  => MonadReader (TCEnv name tyname builtin) m
  => Ord tyname
  => Ord name
  => Ord builtin
  => Defun name tyname builtin i
  -> m (Defun (name, Type tyname) tyname builtin (i, Type tyname))
inferDefun (Defun dn term typ) = do
  (term', _) <- inferTerm term
  let rty = generalize mempty $ termTy term'
  case typ of
    Just ty -> do
      fty <- instantiate $ generalize mempty ty
      _ <- unifies fty (termTy term')
      pure (Defun (dn, tsToTyForall rty) term' typ)
    Nothing -> pure (Defun (dn, tsToTyForall rty) term' typ)

