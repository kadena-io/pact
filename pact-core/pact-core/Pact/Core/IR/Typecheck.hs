{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}


-- |
-- Module      :  Pact.Core.IR.Typecheck
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- HM type inference for core IR.
-- Todo: concrete types.
-- Todo: zonking
--
module Pact.Core.IR.Typecheck where

-- import Control.Lens
-- import Control.Monad.Except
-- import Control.Monad.Reader
-- import Data.List.NonEmpty(NonEmpty(..))
-- import Data.Foldable(fold, foldlM)
-- import Data.Map(Map)
-- import Data.Maybe (fromMaybe)
-- import Data.Set(Set)
-- import Data.Text(Text)

-- import qualified Data.Set as Set
-- import qualified Data.Map.Strict as Map
-- import qualified Data.List.NonEmpty as NE
-- import qualified Data.Vector as V
-- import qualified Data.Text as T

-- import Pact.Core.Builtin
-- import Pact.Core.Type
-- import Pact.Core.Names
-- import qualified Pact.Core.IR.Term as IR
-- import qualified Pact.Core.Typed.Term as Typed

-- data TypecheckError tn
--   = InfiniteType tn (Type tn)
--   | InfiniteRow tn (Row tn)
--   | UnificationError (Type tn) (Type tn)
--   | RowUnificationError (Row tn) (Row tn)
--   | InterfaceUnificationError Text
--   | DynamicAccessError Text
--   deriving Show

-- data TyScheme tn
--   = TyScheme [tn] [tn] (Type tn)
--   deriving Show

-- data Constraint
--   = EqConst (Type TypeVar) (Type TypeVar)
--   deriving Show

-- data Subst
--   = Subst
--   { _stypes :: Map TypeVar (Type TypeVar)
--   , _srows :: Map TypeVar (Row TypeVar)
--   } deriving Show

-- data FreeTyVars
--   = FreeTyVars
--   { _ftvTy :: Set TypeVar
--   , _ftvRow :: Set TypeVar
--   } deriving Show

-- makeLenses ''Subst
-- makeLenses ''FreeTyVars


-- data TCEnv builtin
--   = TCEnv
--   { _tcSupply :: Supply
--   -- ^ Fresh name supply
--   , _tcEnv :: Map.Map IRName (TyScheme TypeVar)
--   -- ^ Typescheme environment
--   , _tcNonGen :: Set.Set TypeVar
--   -- ^ Monomorphic set to not generalize
--   , _tcIface :: Map TypeVar (Map IRName (TyScheme TypeVar))
--   -- ^ Map of iface name -> iface fields -> field types
--   -- Necessary since the best we can do for dynamic module
--   -- references is typecheck them nominally.
--   , _tcBuiltin :: Map.Map builtin (TyScheme TypeVar)
--   -- ^ builtin type schemes.
--   }
-- makeLenses ''TCEnv

-- newtype InferT builtin a =
--   InferT { _runInferT :: ReaderT (TCEnv builtin) (ExceptT (TypecheckError TypeVar) IO) a }
--   deriving ( Functor, Applicative, Monad, MonadReader (TCEnv builtin)
--            , MonadError (TypecheckError TypeVar), MonadIO )
--   via (ReaderT (TCEnv builtin) (ExceptT (TypecheckError TypeVar) IO))

-- freshVar :: InferT b TypeVar
-- freshVar = do
--   s <- view tcSupply
--   u <- liftIO (newUnique s)
--   let tv = T.pack ("_unif" <> show (_unique u))
--   pure $ UnificationVar tv u

-- lookupTyEnv :: IRName -> InferT b (TyScheme TypeVar)
-- lookupTyEnv n = do
--   m <- view tcEnv
--   pure $ m Map.! n

-- inTcEnv :: IRName -> (TyScheme TypeVar) -> InferT b a -> InferT b a
-- inTcEnv name typ =
--   locally tcEnv (Map.insert name typ)

-- inTcEnvNonGen ::IRName -> TypeVar -> (TyScheme TypeVar) -> InferT b a -> InferT b a
-- inTcEnvNonGen name tv typ =
--   local $ \env -> env & tcNonGen %~ Set.insert tv & tcEnv %~ Map.insert name typ


-- ftvIntersection :: FreeTyVars -> FreeTyVars -> FreeTyVars
-- ftvIntersection (FreeTyVars l r) (FreeTyVars l' r') =
--   FreeTyVars (Set.intersection l l') (Set.intersection r r')

-- instance Semigroup Subst where
--   (Subst l r) <> (Subst l' r') = Subst (l <> l') (r <> r')

-- instance Monoid Subst where
--   mempty = Subst mempty mempty

-- instance Semigroup FreeTyVars where
--   (FreeTyVars l r) <> (FreeTyVars l' r') = FreeTyVars (l <> l') (r <> r')

-- instance Monoid FreeTyVars where
--   mempty = FreeTyVars mempty mempty

-- class Substitutable p where
--   subst :: Subst -> p -> p

-- instance Substitutable (Row TypeVar) where
--   subst s@(Subst _ rows) = \case
--     RowVar r -> fromMaybe (RowVar r) $ rows ^. at r
--     EmptyRow -> EmptyRow
--     RowTy fields mv ->
--       case mv of
--         Nothing -> RowTy (subst s <$> fields) Nothing
--         Just rv -> case rows ^. at rv of
--           Just (RowVar rv') -> subst s (RowTy fields (Just rv'))
--           Just (RowTy fields' rv') -> subst s (RowTy (Map.union fields fields') rv')
--           Just EmptyRow -> RowTy (subst s <$> fields) Nothing
--           _ -> RowTy (subst s <$> fields) mv

-- instance Substitutable (Type TypeVar) where
--   subst s@(Subst tys rows) = \case
--     TyVar n -> fromMaybe (TyVar n) $ tys ^. at n
--     TyPrim p -> TyPrim p
--     TyList ty -> TyList (subst s ty)
--     TyRow r -> TyRow $ subst s r
--     TyFun l r -> TyFun (subst s l) (subst s r)
--     -- TyInterface n -> TyInterface n
--     -- TyModule n -> TyModule n
--     TyTable row ->
--       TyTable (subst s row)
--     TyCap ->
--       TyCap
--     TyForall ns rs ty ->
--       let tys' = Map.fromList [(n', TyVar n') | n' <- ns] `Map.union` tys
--           rows' = Map.fromList [(r', RowVar r') | r' <- rs] `Map.union` rows
--       in TyForall ns rs (subst (Subst tys' rows') ty)

-- instance Substitutable (TyScheme TypeVar) where
--   subst m (TyScheme ns rs ty) =
--     let m' = m  <> Subst (Map.fromList [(n', TyVar n') | n' <- ns]) (Map.fromList [(r', RowVar r') | r' <- rs])
--     in TyScheme ns rs (subst m' ty)

-- instance Substitutable Constraint where
--    subst s (EqConst t1 t2) = EqConst (subst s t1) (subst s t2)

-- instance Substitutable p => Substitutable (Map k p) where
--   subst s = fmap (subst s)

-- class FTV p where
--   ftv :: p -> FreeTyVars

-- instance FTV (Map.Map k (Type TypeVar)) where
--   ftv = foldMap ftv

-- instance FTV (Type TypeVar) where
--   ftv = \case
--     TyVar n -> FreeTyVars (Set.singleton n) mempty
--     TyPrim _ -> mempty
--     TyFun l r -> ftv l <> ftv r
--     TyRow rows -> ftv rows
--     TyList t -> ftv t
--     -- TyInterface _ -> mempty
--     -- TyModule _ -> mempty
--     TyTable row -> ftv row
--     TyCap -> mempty
--     TyForall ns rs typ ->
--       let (FreeTyVars fts frs) = ftv typ
--       in FreeTyVars (fts `Set.difference` Set.fromList ns) (frs `Set.difference` Set.fromList rs)

-- instance FTV (Row TypeVar) where
--   ftv = \case
--     RowTy m n -> ftv m <> maybe mempty (FreeTyVars mempty . Set.singleton) n
--     RowVar n -> FreeTyVars mempty (Set.singleton n)
--     EmptyRow -> mempty

-- instance FTV (TyScheme TypeVar) where
--   ftv (TyScheme ns rs typ) =
--     let (FreeTyVars tys rows) = ftv typ
--     in FreeTyVars (tys `Set.difference` Set.fromList ns) (rows `Set.difference` Set.fromList rs)

-- ----------------------------
-- -- Core Inference.
-- ----------------------------

-- compose :: Subst -> Subst -> Subst
-- compose m1 m2 =
--   let m2' = m2 & stypes . mapped %~ subst m1 & srows . mapped %~ subst m1
--   in m2' <> m1

-- -- Occurs checking
-- bind :: TypeVar -> Type TypeVar -> InferT b Subst
-- bind n t | isSameTyVar t = pure mempty
--          | occursCheck n t = throwError (InfiniteType n t)
--          | otherwise = pure $ Subst (Map.singleton n t) mempty
--   where
--   isSameTyVar = \case
--     TyVar n' -> n == n'
--     _ -> False

-- -- todo: occurs check for rows.
-- bindRow :: TypeVar -> Row TypeVar -> InferT b Subst
-- bindRow n t | isSameRowVar t = pure mempty
--             | otherwise = pure $ Subst mempty (Map.singleton n t)
--   where
--   isSameRowVar = \case
--     RowVar n' -> n == n'
--     _ -> False

-- occursCheck :: FTV p => TypeVar -> p -> Bool
-- occursCheck n t = Set.member n $ _ftvTy (ftv t)

-- unifyRows
--   :: Row TypeVar
--   -> Row TypeVar
--   -> InferT b Subst
-- unifyRows (RowVar n) t =  n `bindRow` t
-- unifyRows t (RowVar n) =  n `bindRow` t
-- -- Unify labels present in both m and m'
-- -- Labels not present: unify with row variable.
-- unifyRows r1@(RowTy m mrv) r2@(RowTy m' mrv') =
--   case (mrv, mrv') of
--     -- Two open rows
--     (Just rv, Just rv') -> do
--       unif <- fold <$> traverse (uncurry unifies) (Map.intersectionWith (,) m m')
--       leftVar <- freshVar
--       rightVar <- freshVar
--       let notInR = Map.difference m m'
--           notInRSubst = (rv,) $ if Map.null notInR then RowVar leftVar else RowTy notInR (Just leftVar)
--           notInL = Map.difference m' m
--           notInLSubst = (rv',) $ if Map.null notInL then RowVar rightVar else RowTy notInR (Just rightVar)
--       pure $ unif <> Subst mempty (Map.fromList [notInRSubst, notInLSubst])
--     -- Right closed
--     -- Means Keys(l) <= Keys(r)
--     (Just rv, Nothing) -> do
--       if all (`Map.member` m') (Map.keys m) then do
--         unif <- fold <$> traverse (uncurry unifies) (Map.intersectionWith (,) m m')
--         let diff = Map.difference m' m
--             s = if Map.null diff then EmptyRow else RowTy diff Nothing
--         pure $ unif <> Subst mempty (Map.singleton rv s)
--       else throwError (RowUnificationError r1 r2)
--     -- Left closed
--     (Nothing, Just rv) -> do
--       if all (`Map.member` m) (Map.keys m') then do
--         unif <- fold <$> traverse (uncurry unifies) (Map.intersectionWith (,) m m')
--         let diff = Map.difference m m'
--             s = if Map.null diff then EmptyRow else RowTy diff Nothing
--         pure $ unif <> Subst mempty (Map.singleton rv s)
--       else throwError (RowUnificationError r1 r2)
--     (Nothing, Nothing) ->
--       if Map.keys m == Map.keys m' then
--         fold <$> traverse (uncurry unifies) (Map.intersectionWith (,) m m')
--       else throwError (RowUnificationError r1 r2)
-- unifyRows EmptyRow EmptyRow = pure mempty
-- unifyRows r1 r2 = throwError (RowUnificationError r1 r2)

-- --
-- -- unifyIfaceModule
-- --   :: InterfaceType TypeVar -> ModuleType TypeVar -> InferT b Subst
-- -- unifyIfaceModule (InterfaceType n) (ModuleType _ ns) =
-- --   if elem n ns then pure mempty
-- --   else throwError (InterfaceUnificationError "cannot unify interface/module")

-- -- note: For IR we currently don't unify against
-- -- `TyForall` unless strucurally equal.
-- -- We don't allow rankN despite it showing up in
-- -- our type language.
-- unifies
--   :: Type TypeVar
--   -> Type TypeVar
--   -> InferT b Subst
-- unifies (TyPrim t1) (TyPrim t2) | t1 == t2 = pure mempty
-- unifies (TyVar n) t2 = n `bind` t2
-- unifies t1 (TyVar n) = n `bind` t1
-- unifies (TyFun l r) (TyFun l' r') = do
--   s1 <- unifies l l'
--   s2 <- unifies (subst s1 r) (subst s1 r')
--   pure (s2 `compose` s1)
-- unifies (TyList l) (TyList r) = unifies l r
-- unifies (TyTable l) (TyTable r) = unifyRows l r
-- unifies TyCap TyCap = pure mempty
-- unifies (TyRow l) (TyRow r) = unifyRows l r
-- -- unifies (TyInterface n) (TyModule m) = unifyIfaceModule n m
-- -- unifies (TyModule m) (TyInterface n) = unifyIfaceModule n m
-- -- unifies (TyInterface n) (TyInterface n') | n == n' = pure mempty
-- -- unifies (TyModule m) (TyModule m') | m == m' = pure mempty
-- unifies _ _ = error "reee"

-- generalize :: FreeTyVars -> Type TypeVar -> (TyScheme TypeVar)
-- generalize (FreeTyVars freetys freerows) t  = TyScheme as rs t
--   where
--   toTypeVarName n = TypeVar (_tyVarName n) (_tyVarUnique n)
--   (FreeTyVars ftys frows) = ftv t
--   as = fmap toTypeVarName $ Set.toList $ ftys `Set.difference` freetys
--   rs = fmap toTypeVarName $ Set.toList $ frows `Set.difference` freerows

-- generalizeWithTerm :: FreeTyVars -> Type TypeVar -> Typed.Term IRName TypeVar b info -> (TyScheme TypeVar, Typed.Term IRName TypeVar b info)
-- generalizeWithTerm (FreeTyVars freetys freerows) t e  =
--   case ((,Typed.TyVarType) <$> as) ++ ((,Typed.RowVarType) <$> rs) of
--     [] -> (TyScheme [] [] t, e)
--     x:xs -> (TyScheme as rs t, Typed.TyAbs (x :| xs) e (e ^. Typed.termInfo))
--   where
--   toTypeVarName n = TypeVar (_tyVarName n) (_tyVarUnique n)
--   (FreeTyVars ftys frows) = ftv t
--   as = fmap toTypeVarName $ Set.toList $ ftys `Set.difference` freetys
--   rs = fmap toTypeVarName $ Set.toList $ frows `Set.difference` freerows

-- instantiate :: TyScheme TypeVar -> InferT b (Type TypeVar)
-- instantiate (TyScheme as rs t) = do
--     as' <- traverse (const freshVar) as
--     rs' <- traverse (const freshVar) rs
--     let tyS = Map.fromList $ zip as (TyVar <$> as')
--         rowS = Map.fromList $ zip rs (RowVar <$> rs')
--     return $ subst (Subst tyS rowS) t

-- instantiateWithTerm
--   :: TyScheme TypeVar
--   -> Typed.Term IRName TypeVar b info
--   -> InferT b (Typed.Term IRName TypeVar b info, Type TypeVar)
-- instantiateWithTerm (TyScheme as rs t) wrap = do
--     as' <- traverse (const freshVar) as
--     rs' <- traverse (const freshVar) rs
--     let tyS = Map.fromList $ zip as (TyVar <$> as')
--         rowS = Map.fromList $ zip rs (RowVar <$> rs')
--         ty' = subst (Subst tyS rowS) t
--         tyApps = ((, Typed.TyVarType) . TyVar <$> as')
--         rsApps = ((, Typed.RowVarType) . TyVar <$> rs')
--     case tyApps ++ rsApps of
--       x:xs ->
--         return (Typed.TyApp wrap (x:|xs) (wrap ^. Typed.termInfo), ty')
--       [] ->
--         return (wrap, ty')

-- neunzip3 :: NonEmpty (a, b, c) -> (NonEmpty a, NonEmpty b, NonEmpty c)
-- neunzip3 ((a, b, c) :| rest) =
--   let (as, bs, cs) = unzip3 rest
--   in (a :| as, b :| bs, c :| cs)

-- -- todo: propagate infos for better errors.
-- -- todo: quadratic appends. Optimize with `Seq`.
-- -- | Infer the type of a term, and annotate the node with the generated type
-- --   and all names with their respective type. In the case of let-binds
-- --   universally quantified types get paired with the name, as well as
-- --   for modref dynamic members.
-- inferTerm
--   :: Ord b
--   => IR.Term IRName TypeVar b info
--   -> InferT b (Typed.Term IRName TypeVar b info, Type TypeVar, [Constraint])
-- inferTerm = \case
--   IR.Constant l i -> do
--     let ty = typeOfLit l
--     pure (Typed.Constant l i, ty, [])
--   IR.Var n i -> do
--     ts <- lookupTyEnv n
--     let out = Typed.Var n i
--     (out', ty) <- instantiateWithTerm ts out
--     pure (out', ty, [])
--   IR.Lam _name nts body i -> do
--     let (names, mtys) = NE.unzip nts
--     rawTvs <- traverse (const freshVar) nts
--     (csigs, substs) <- NE.unzip <$> traverse (uncurry withAnnotation) (NE.zip rawTvs mtys)
--     let csig = concat csigs
--         s0 = fold substs
--     (body', retTy, cs) <- local (inEnv rawTvs s0) $ inferTerm body
--     let tvSubsts = subst s0 . TyVar <$> rawTvs
--         ty = foldr (\arg ret -> TyFun (subst s0 arg) ret) retTy tvSubsts
--         nts' = NE.zip names tvSubsts
--     pure (Typed.Lam _name nts' body' i, ty, csig++cs)
--     where
--     withAnnotation rawTv (Just annTy) =
--       ([EqConst annTy (TyVar rawTv)],) <$> unifies annTy (TyVar rawTv)
--     withAnnotation _ _ = pure ([], mempty)
--     inEnv rawTvs s0 =
--       let entries = Map.fromList $ NE.toList $ NE.zip (fst <$> nts) (TyScheme [] [] . subst s0 . TyVar <$> rawTvs)
--           nonGen = Set.fromList (NE.toList rawTvs)
--       in over tcNonGen (Set.union nonGen) . over tcEnv (Map.union entries)
--   IR.Let n mty e1 e2 i -> do
--     (e1', t1, c1) <- inferTerm e1
--     csig <- case mty of
--       Nothing -> pure mempty
--       Just ty -> pure [EqConst ty t1]
--     s <- solve (c1 ++ csig)
--     ftvs <- ftvSubstNonGen s <$> view tcNonGen
--     -- todo: we're CBV, so we need value restriction.
--     let (ntyp, e1'') = generalizeWithTerm ftvs (subst s t1) (over Typed.traverseTermType (subst s) e1')
--     (e2', ty, c2) <- locally tcEnv (subst s) $ inTcEnv n ntyp $ inferTerm e2
--     let argTy = tsToTyForall ntyp
--     let out = Typed.App (Typed.Lam n (pure (n, argTy)) e2' i) (pure e1'') i
--     pure (out, ty, c1 ++ csig ++ c2 )

--   IR.App e1 (e2 :| es) i -> do
--     tv <- TyVar <$> freshVar
--     (e1', e1ty, cs1) <- inferTerm e1
--     (e2', e2ty, cs2) <- inferTerm e2
--     let cs = cs1 ++ cs2 ++ [EqConst e1ty (TyFun e2ty tv)]
--     (ret, es', cs') <- foldlM inferApps (tv, [], []) es
--     pure
--       ( Typed.App e1' (e2':| reverse es') i, ret, cs ++ cs')
--     where
--     -- Todo: redo as fold
--     inferApps (ty, xs, ccs) x = do
--       tv <- TyVar <$> freshVar
--       (x', tx, cs) <- inferTerm x
--       let cs' = cs ++ [EqConst ty (TyFun tx tv)]
--       pure (tv, x':xs, ccs ++ cs')
--   IR.Block terms i -> do
--     -- Will we require that the lhs of sequenced statements be unit?
--     -- likely yes, it doesn't make sense to otherwise discard value results without binding them in a clause.
--     -- for now, no.
--     -- We might emit a constraint `t1 ~ ()` here  eventually though
--     (terms', tys, csli) <- neunzip3 <$> traverse inferTerm terms
--     let retTy = NE.last tys
--         constraints = concat csli
--     pure (Typed.Block terms' i, retTy, constraints )
--   IR.ListLit terms i -> do
--     tv <- TyVar <$> freshVar
--     joined <- traverse (listInfer tv) terms
--     let (terms', cs) = V.unzip joined
--     pure (Typed.ListLit (TyList tv) terms' i, TyList tv, fold cs)
--     where
--     listInfer tv t = do
--       (t', ty', cs) <- inferTerm t
--       pure (t', cs ++ [EqConst ty' tv])
--   IR.ObjectLit objMap i -> do
--     rv <- freshVar
--     objMapCs <- traverse inferTerm objMap
--     let cs = foldMap (view _3) objMapCs
--         objMap' = view _1 <$> objMapCs
--         outTy = TyRow (RowTy (view _2 <$> objMapCs) (Just rv))
--     pure (Typed.ObjectLit objMap' i, outTy, cs)
--   -- Errors are just `forall a. a`.
--   IR.Error e i -> do
--     tv <- TyVar <$> freshVar
--     pure (Typed.Error e tv i, tv, [])
--   -- m::f
--   -- moduleref in core.
--   IR.DynAccess _modref _mem _i -> undefined
--     -- ty <- lookupTyEnv modref
--     -- case ty of
--     --   TyScheme [] [] (TyInterface (InterfaceType iface)) -> do
--     --     ifaces <- view tcIface
--     --     case ifaces ^? at iface . _Just . at mem . _Just of
--     --       Just fn -> do
--     --         typ <- instantiate fn
--     --         let ifTyp = TyInterface (InterfaceType iface)
--     --             fTyp = tsToTyForall fn
--     --         pure (DynAccess (modref, ifTyp) (mem, fTyp) (i, typ), [])
--     --       Nothing -> throwError (DynamicAccessError "Function not in interface")
--     --   _ -> throwError (DynamicAccessError "Not An Interface")
--   IR.Builtin b i -> do
--     benv <- view tcBuiltin
--     (out, bty) <- instantiateWithTerm (benv Map.! b) (Typed.Builtin b i)
--     pure (out, bty, [])
--   where
--   ftvSubstNonGen unif nonGen = foldMap ftv $ subst unif . TyVar <$> Set.toList nonGen

-- tsToTyForall :: (TyScheme TypeVar) -> Type TypeVar
-- tsToTyForall (TyScheme [] [] ty) = ty
-- tsToTyForall (TyScheme as rs ty) = TyForall as rs ty

-- termTy :: Typed.Term name tyname builtin (info, Type tyname) -> Type tyname
-- termTy = view (Typed.termInfo._2)

-- ----------------------------
-- -- Constraint solving
-- ----------------------------

-- solve :: [Constraint] -> InferT b Subst
-- solve = solve' mempty

-- solve'
--   :: Subst
--   -> [Constraint]
--   -> InferT b Subst
-- solve' s0 = \case
--   [] -> return s0
--   (EqConst t1 t2) : xs -> do
--     s1 <- unifies t1 t2
--     let s2 = s1 `compose` s2
--     solve' s2 (fmap (subst s2) xs)

-- ----------------------------
-- -- Running the typechecker
-- ----------------------------

-- inferAndSolve
--   :: Ord b
--   => IR.Term IRName TypeVar b info
--   -> InferT b (Typed.Term IRName TypeVar b info, Type TypeVar, Subst)
-- inferAndSolve term = do
--   (term', ty, cs) <- inferTerm term
--   s <- solve cs
--   pure (over Typed.traverseTermType (subst s) term', subst s ty, s)


-- -- inferDefun
-- --   :: Ord b
-- --   => Defun IRName TypeVar b i
-- --   -> InferT b (Defun (IRName, Type TypeVar) TypeVar b (i, Type TypeVar))
-- -- inferDefun (Defun dn term typ) = do
-- --   (term', _) <- inferTerm term
-- --   let rty = tsToTyForall $ generalize mempty $ termTy term'
-- --   case typ of
-- --     Just ty -> do
-- --       fty <- instantiate $ generalize mempty ty
-- --       _ <- unifies fty (termTy term')
-- --       pure (Defun (dn, rty) term' typ)
-- --     Nothing ->
-- --       pure (Defun (dn, rty) term' (Just rty))

-- -- inferDefConst
-- --   :: Ord b
-- --   => DefConst IRName TypeVar b i
-- --   -> InferT b (DefConst (IRName, Type TypeVar) TypeVar b (i, Type TypeVar))
-- -- inferDefConst (DefConst name term typ) = do
-- --   (term', _) <- inferTerm term
-- --   case typ of
-- --     Just ty -> do
-- --       _ <- unifies ty (termTy term')
-- --       pure (DefConst (name, ty) term' (Just ty))
-- --     Nothing ->
-- --       pure (DefConst (name, termTy term') term' (Just (termTy term')))


-- renameTypeScheme :: MonadIO m => Supply -> TyScheme Text -> m (TyScheme TypeVar)
-- renameTypeScheme sup (TyScheme ts rs ty) = do
--   ts' <- traverse mkTypeName ts
--   rs' <- traverse mkTypeName rs
--   ty' <- flip runReaderT (Map.fromList (zip ts ts' ++ zip rs rs')) $ renameType ty
--   pure (TyScheme ts' rs' ty')
--   where
--   mkTypeName :: MonadIO f => Text -> f TypeVar
--   mkTypeName n = do
--     u <- liftIO (newUnique sup)
--     pure (TypeVar n u)
--   renameType = \case
--     TyVar n ->
--       asks (Map.lookup n) >>= \case
--         Just n' -> pure $ TyVar n'
--         Nothing -> error $ "Unbound free type variable found in type renaming: " ++ show n
--     TyPrim p -> pure (TyPrim p)
--     TyFun l r ->
--       TyFun <$> renameType l <*> renameType r
--     TyRow r -> TyRow <$> renameRow r
--     TyList l -> TyList <$> renameType l
--     TyTable t -> TyTable <$> renameRow t
--     TyCap -> pure TyCap
--     TyForall tts rrs tty -> do
--       tts' <- traverse mkTypeName tts
--       rrs' <- traverse mkTypeName rrs
--       let env' = Map.fromList (zip tts tts' ++ zip rrs rrs')
--       tty' <- local (Map.union env') $ renameType tty
--       pure $ TyForall tts' rrs' tty'
--     -- _ -> error "todo: modules/interfaces"
--   lookupRowVar n =
--     asks (Map.lookup n) >>= \case
--       Just n' -> pure n'
--       Nothing -> error $ "Unbound free row variable found in type renaming: " ++ show n
--   renameRow EmptyRow = pure EmptyRow
--   renameRow (RowVar n) =
--     RowVar <$> lookupRowVar n
--   renameRow (RowTy obj n) = do
--     obj' <- traverse renameType obj
--     n' <- traverse lookupRowVar n
--     pure (RowTy obj' n')

-- rawBuiltinType :: RawBuiltin -> TyScheme Text
-- rawBuiltinType = \case
--   RawAdd -> sameArgBinop
--   RawSub -> sameArgBinop
--   RawMultiply -> sameArgBinop
--   RawDivide -> sameArgBinop
--   RawNegate -> sameArgBinop
--   -- Boolean Ops
--   RawAnd -> binaryBoolOp
--   RawOr -> binaryBoolOp
--   RawNot -> TyScheme [] [] (TyBool :~> TyBool)
--   -- Equality and Comparisons
--   RawEq -> comparisonBinop
--   RawNeq -> comparisonBinop
--   RawGT -> comparisonBinop
--   RawGEQ -> comparisonBinop
--   RawLT -> comparisonBinop
--   RawLEQ -> comparisonBinop
--   -- Bitwise Ops
--   RawBitwiseAnd -> binaryIntOp
--   RawBitwiseOr -> binaryIntOp
--   RawBitwiseXor -> binaryIntOp
--   RawBitwiseFlip -> unaryIntOp
--   RawBitShift -> binaryIntOp
--     -- Other Numerics
--   RawAbs -> unaryNumeric
--   RawRound ->
--     TyScheme [] [] (TyDecimal :~> TyInt)
--   RawCeiling ->
--     TyScheme [] [] (TyDecimal :~> TyInt)
--   RawExp ->
--     TyScheme ["a"] [] (TyVar "a" :~> TyDecimal)
--   RawFloor ->
--     TyScheme [] [] (TyDecimal :~> TyInt)
--   RawLn ->
--     TyScheme ["a"] [] (TyVar "a" :~> TyDecimal)
--   RawLogBase -> sameArgBinop
--   RawMod -> binaryIntOp
--   -- General
--   RawMap ->
--     TyScheme ["a", "b"] [] ((TyVar "a" :~> TyVar "b") :~> TyList (TyVar "a") :~> TyList (TyVar "b"))
--   RawFilter ->
--     TyScheme ["a"] [] ((TyVar "a" :~> TyBool) :~> TyList (TyVar "a") :~> TyList (TyVar "a"))
--   RawIf ->
--     let tv = TyVar "a"
--     in TyScheme ["a"] [] (TyBool :~> tv :~> tv :~> TyVar "a")
--   RawIntToStr ->
--     TyScheme [] [] (TyInt :~> TyInt :~> TyString)
--   RawConcat ->
--     TyScheme [] [] (TyList TyString :~> TyString)
--   RawStrToInt ->
--     TyScheme [] [] (TyString :~> TyInt)
--   RawTake ->
--     TyScheme ["a"] [] (TyInt :~> TyVar "a" :~> TyVar "a")
--   RawDrop ->
--     TyScheme ["a"] [] (TyInt :~> TyVar "a" :~> TyVar "a")
--   RawLength ->
--     TyScheme ["a"] [] (TyVar "a" :~> TyInt)
--   RawDistinct ->
--     TyScheme ["a"] [] (TyList (TyVar "a") :~> TyList (TyVar "a"))
--   RawEnforce ->
--     TyScheme [] [] (TyBool :~> TyString :~> TyBool)
--   RawEnforceOne ->
--     TyScheme [] [] (TyList TyBool :~> TyString :~> TyBool)
--   RawEnumerate ->
--     TyScheme [] [] (TyInt :~> TyInt :~> TyList TyInt)
--   RawEnumerateStepN ->
--     TyScheme [] [] (TyInt :~> TyInt :~> TyInt :~> TyList TyInt)
--   where
--   unaryIntOp =
--     TyScheme [] [] (TyInt :~> TyInt)
--   unaryNumeric =
--     TyScheme ["a"] [] (TyVar "a" :~> TyVar "a")
--   sameArgBinop =
--     TyScheme ["a"] [] (TyVar "a" :~> TyVar "a" :~> TyVar "a")
--   comparisonBinop =
--     TyScheme ["a"] [] (TyVar "a" :~> TyVar "a" :~> TyBool)
--   binaryBoolOp =
--     TyScheme [] [] (TyBool :~> TyBool :~> TyBool)
--   binaryIntOp =
--     TyScheme [] [] (TyInt :~> TyInt :~> TyInt)

-- rawBuiltinTypes :: MonadIO m => Supply -> m (Map RawBuiltin (TyScheme TypeVar))
-- rawBuiltinTypes sup = do
--   let mkts b = (b,) <$> renameTypeScheme sup (rawBuiltinType b)
--   Map.fromList <$> traverse mkts [minBound .. maxBound]
