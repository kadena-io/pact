{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE StandaloneDeriving #-}


-- |
-- Module      :  Pact.Core.IR.Typecheck
-- Copyright   :  (C) 2022 Kadena
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Jose Cardona <jose@kadena.io>
--
-- HM type inference for core IR.
--
module Pact.Core.IR.Typecheck
 ( runInferProgram
 , runInferTerm
 , runInferModule
 , runInferTopLevel
 , runInferReplProgram
 , rawBuiltinType
 ) where

import Control.Lens hiding (Level)
import Control.Monad.Reader
import Control.Monad.ST
import Control.Monad.ST.Unsafe(unsafeIOToST, unsafeSTToIO)
import Control.Monad.Except
import Control.Exception(throwIO, catch)
import Data.Void
import Data.Dynamic (Typeable)
import Data.RAList(RAList)
import Data.Foldable(traverse_, foldlM)
import Data.STRef
import Data.Map(Map)
import Data.Text(Text)

import qualified Data.Map.Strict as Map
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.RAList as RAList

import Pact.Core.Builtin
import Pact.Core.Type
import Pact.Core.Names
import Pact.Core.Errors
import Pact.Core.Persistence
import qualified Pact.Core.IR.Term as IR
import qualified Pact.Core.Typed.Term as Typed
import qualified Pact.Core.Untyped.Term as Untyped

-- inference based on https://okmij.org/ftp/ML/generalization.html
-- Note: Type inference levels in the types
-- a-la sound-lazy might be worth implementing later on.
-- The eager implementation is simpler to maintain and extend to typeclasses.

-- note, we will debruijnize, so this is purely for
-- Display purposes
type UniqueSupply s = STRef s Unique
type Level = Int

data Tv s
  = Unbound !Text !Unique !Level
  | Bound !Text !Unique
  | Link !(Type (TvRef s))
  deriving Eq

-- Note: TyVar equality
-- is reference equality
newtype TvRef s =
  TvRef (STRef s (Tv s))
  deriving Eq

data TCState s b
  = TCState
  { _tcSupply :: UniqueSupply s
  -- ^ Supply for fresh variables.
  , _tcVarEnv :: RAList (Type (TvRef s))
  -- Variable environment for locally bound and top level names
  , _tcBuiltins :: b -> TypeScheme NamedDeBruijn
  -- ^ Builtins map, that uses the enum instance
  , _tcFree :: Map ModuleName (Map Text (Type Void))
  -- ^ Free variables
  , _tcLevel :: STRef s Level
  -- Type Variable "Region"
  }

makeLenses ''TCState

type TCType s = Type (TvRef s)
type TCPred s = Pred (TvRef s)

-- | Term emitted by desugar
type IRTerm b i = IR.Term Name b i
type IRModule b i = IR.Module Name b i

-- | Term emitted by the typechecker prior to final generalization/unification.
type TCTerm s b i = Typed.Term Name (TvRef s) (b, [TCType s], [TCPred s]) i

-- Term/defun outputs post typechecking
-- with ST monad existential removed
type TypedTerm b i = Typed.OverloadedTerm b i

type TypedDefun b i = Typed.OverloadedDefun b i

type TypedDefConst b i = Typed.OverloadedDefConst b i

type TypedDef b i = Typed.OverloadedDef b i

type TypedTopLevel b i = Typed.OverloadedTopLevel b i

type TypedReplTopLevel b i = Typed.OverloadedReplTopLevel b i

type TypedModule b i = Typed.OverloadedModule b i

newtype InferM s b i a =
  InferT (ReaderT (TCState s b) (ST s) a)
  deriving
    ( Functor, Applicative, Monad
    , MonadReader (TCState s b)
    , MonadFail)
  via (ReaderT (TCState s b) (ST s))

-- ABSOLUTELY UNHOLY USE OF ST
instance (Show i, Typeable i) => MonadError (PactError i) (InferM s b i) where
  throwError e =
    InferT (ReaderT (const (unsafeIOToST (throwIO e))))
  catchError act handler =
    InferT (ReaderT (\s -> unsafeIOToST $ catch (unsafeMkIO s act) (handle' s)))
    where
    unsafeMkIO :: TCState s b -> InferM s b i a -> IO a
    unsafeMkIO s (InferT act') = unsafeSTToIO (runReaderT act' s)
    handle' s e = unsafeMkIO s (handler e)


liftST :: ST s a -> InferM s b i a
liftST action = InferT (ReaderT (const action))

_dbgTypedTerm
  :: TCTerm s b i
  -> InferM s b i (Typed.Term Text String (b, [Type String], [Pred String]) i)
_dbgTypedTerm = \case
  Typed.Var n i -> pure (Typed.Var (_nName n) i)
  Typed.Lam nel body i -> do
    nel' <- (traversed._2) _dbgType nel
    body' <- _dbgTypedTerm body
    pure (Typed.Lam (over (mapped._1) _nName nel') body' i)
  Typed.App fn body i ->
    Typed.App <$> _dbgTypedTerm fn <*> traverse _dbgTypedTerm body <*> pure i
  Typed.Let n e1 e2 i ->
    Typed.Let (_nName n) <$> _dbgTypedTerm e1 <*> _dbgTypedTerm e2 <*> pure i
  Typed.Builtin (b, tys, preds) i -> do
    tys' <- traverse _dbgType tys
    preds' <- traverse _dbgPred preds
    pure (Typed.Builtin (b, tys', preds') i)
  Typed.Constant l i -> pure (Typed.Constant l i)
  Typed.TyApp t nelty i ->
    Typed.TyApp <$> _dbgTypedTerm t <*> traverse _dbgType nelty <*> pure i
  Typed.Sequence e1 e2 i ->
    Typed.Sequence <$> _dbgTypedTerm e1 <*> _dbgTypedTerm e2 <*> pure i
  Typed.Try e1 e2 i ->
    Typed.Try <$> _dbgTypedTerm e1 <*> _dbgTypedTerm e2 <*> pure i
  Typed.Error t e i ->
    Typed.Error <$> _dbgType t <*> pure e <*> pure i
  Typed.ListLit ty li i ->
    Typed.ListLit <$> _dbgType ty <*> traverse _dbgTypedTerm li <*> pure i
  -- Typed.ObjectLit obj i ->
  --   Typed.ObjectLit <$> traverse _dbgTypedTerm obj <*> pure i
  -- Typed.ObjectOp oop i ->
  --   Typed.ObjectOp <$> traverse _dbgTypedTerm oop <*> pure i

_dbgTypeScheme :: TypeScheme (TvRef s) -> InferM s b i (TypeScheme String)
_dbgTypeScheme (TypeScheme tvs preds ty) = do
  tvs' <- traverse rv tvs
  preds' <- traverse _dbgPred preds
  ty' <- _dbgType ty
  pure (TypeScheme tvs' preds' ty')
  where
  rv n = readTvRef n >>= \case
    Unbound u l _ -> pure ("unbound" <> show (u, l))
    Bound u l -> pure ("bound" <> show (u, l))
    Link _ -> pure "linktv"

_dbgTvRef :: TvRef s -> InferM s b i String
_dbgTvRef tv = readTvRef tv >>= \case
    Unbound u l _ -> pure ("unbound" <> show (u, l))
    Bound u l -> pure ("bound" <> show (u, l))
    Link ty -> do
      ty' <- _dbgType ty
      pure $ "linked type: " <> show ty'

_dbgPred :: TCPred s -> InferM s b i (Pred String)
_dbgPred (Pred i t) = Pred i <$> _dbgType t

_dbgType :: TCType s -> InferM s b i (Type String)
_dbgType = \case
  TyVar tv -> readTvRef tv >>= \case
    Unbound u l _ -> pure (TyVar ("unbound" <> show (u, l)))
    Bound u l -> pure (TyVar ("bound" <> show (u, l)))
    Link ty -> _dbgType ty
  TyFun l r -> TyFun <$> _dbgType l <*> _dbgType r
  TyList t -> TyList <$> _dbgType t
  TyPrim p -> pure (TyPrim p)
  TyGuard -> pure TyGuard
  TyForall {} -> fail "impredicative"


enterLevel :: InferM s b i ()
enterLevel = do
  lref <- asks _tcLevel
  liftST (modifySTRef' lref succ)

leaveLevel :: InferM s b i ()
leaveLevel = do
  lref <- asks _tcLevel
  liftST (modifySTRef' lref pred)

currentLevel :: InferM s b i Level
currentLevel =
  asks _tcLevel >>= liftST . readSTRef

readTvRef :: TvRef s -> InferM s b i (Tv s)
readTvRef (TvRef tv) = liftST (readSTRef tv)

writeTvRef :: TvRef s -> Tv s -> InferM s b i ()
writeTvRef (TvRef tv) t = liftST (writeSTRef tv t)

newTvRef :: InferM s b i (TvRef s)
newTvRef = do
  uref <- asks _tcSupply
  u <- liftST (readSTRef uref)
  let tvName = "'a_" <> T.pack (show u)
  l <- currentLevel
  liftST (modifySTRef' uref (+ 1))
  TvRef <$> liftST (newSTRef (Unbound tvName u l))

newSupplyIx :: InferM s b i Unique
newSupplyIx = do
  uref <- asks _tcSupply
  u <- liftST (readSTRef uref)
  liftST (modifySTRef' uref (+ 1))
  pure u
---------------------------------------------------------------
-- Type class instances,
-- entailment, context reduction.
-- ---------------------------------------------------------------

-- | For some typeclass C,
-- is there an instance of C t?
-- If so, return the qualifiers of the instance.
-- that is, for (C a_1, .., C a_n) => C t
-- byInst (C t) returns Just [C a_1, .., C a_n].
-- Note: if these were user defined, if we decide to extend to this
-- byInst would have to match the type of C (K t) to an instantiated version
-- of the qualified type (C a_1, .., C a_n) => C (K t_1) for type constructors
byInst :: Pred (TvRef s) -> InferM s b i (Maybe [Pred (TvRef s)])
byInst (Pred p ty) = case p of
  Eq -> eqInst ty
  Add -> addInst ty
  Num -> numInst ty
  Ord -> ordInst ty
  Show -> showInst ty
  ListLike -> listLikeInst ty
  Fractional -> fractionalInst ty

-- | Instances of Eq:
--
--  instance Eq integer
--  instance Eq decimal
--  instance Eq string
--  instance Eq unit
--  instance Eq bool
--  instance Eq time <- todo
--
--  instance (Eq 'a) => Eq (list 'a)
--  For rows:
--  instance (Eq {l1:t1, .., ln:tn}) where t1..tn are monotypes without type variables.
--
eqInst :: TCType s -> InferM s b i (Maybe [Pred (TvRef s)])
eqInst = \case
  TyVar tv -> readTvRef tv >>= \case
    Link ty -> eqInst ty
    _ -> pure Nothing
  -- All prims have an EQ instance
  TyPrim _ -> pure (Just [])
  TyList t -> pure (Just [Pred Eq t])
  _ -> pure Nothing

-- | Instances of Ord:
--
--  instance Ord integer
--  instance Ord decimal
--  instance Ord string
--  instance Ord unit
--  instance Ord time <- todo
--
--  instance (Ord 'a) => Ord (list 'a)
--  For rows:
--
ordInst :: TCType s -> InferM s b i (Maybe [Pred (TvRef s)])
ordInst = \case
  TyVar tv -> readTvRef tv >>= \case
    Link ty -> ordInst ty
    _ -> pure Nothing
  -- All prims have an Ord instance
  TyPrim p -> case p of
    PrimInt -> pure (Just [])
    PrimDecimal -> pure (Just [])
    PrimString -> pure (Just [])
    PrimTime -> pure (Just [])
    PrimUnit -> pure (Just [])
    _ -> pure Nothing
  TyList t -> pure (Just [Pred Ord t])
  _ -> pure Nothing


-- | Instances of Add:
--
--  instance Add integer
--  instance Add decimal
--  instance Add string
--  instance Add (list 'a)
--
--
addInst :: TCType s -> InferM s b i (Maybe [Pred (TvRef s)])
addInst = \case
  TyVar tv -> readTvRef tv >>= \case
    Link ty -> addInst ty
    _ -> pure Nothing
  -- All prims have an EQ instance
  TyPrim p -> pure $ case p of
    PrimString -> Just []
    PrimInt -> Just []
    PrimDecimal -> Just []
    _ -> Nothing
  TyList _ -> pure (Just [])
  _ -> pure Nothing

-- | Instances of num:
-- instance Num integer
-- instance Num decimal
numInst :: TCType s -> InferM s b i (Maybe [Pred (TvRef s)])
numInst = \case
  TyVar tv -> readTvRef tv >>= \case
    Link ty -> numInst ty
    _ -> pure Nothing
  -- All prims have an EQ instance
  TyPrim p -> pure $ case p of
    PrimInt -> Just []
    PrimDecimal -> Just []
    _ -> Nothing
  _ -> pure Nothing

-- | Instances of fractional:
-- instance Fractional integer
-- instance Fractional decimal
fractionalInst :: TCType s -> InferM s b i (Maybe [Pred (TvRef s)])
fractionalInst = \case
  TyVar tv -> readTvRef tv >>= \case
    Link ty -> fractionalInst ty
    _ -> pure Nothing
  TyPrim p -> pure $ case p of
    PrimInt -> Just []
    PrimDecimal -> Just []
    _ -> Nothing
  _ -> pure Nothing

listLikeInst :: TCType s -> InferM s b i (Maybe [Pred (TvRef s)])
listLikeInst = \case
  TyVar tv -> readTvRef tv >>= \case
    Link ty -> listLikeInst ty
    _ -> pure Nothing
  TyPrim p -> pure $ case p of
    PrimString -> Just []
    _ -> Nothing
  TyList _ -> pure $ Just []
  _ -> pure Nothing

-- | Instances of Show:
--
--  instance Show integer
--  instance Show decimal
--  instance Show string
--  instance Show unit
--  instance Show bool
--  instance Show time <- todo
--
--  instance (Show 'a) => Show (list 'a)
--  instance (Show )
--  For rows:
--  instance (Eq {l1:t1, .., ln:tn}) where t1..tn are monotypes without type variables.
--
showInst :: TCType s -> InferM s b i (Maybe [Pred (TvRef s)])
showInst = \case
  TyVar tv -> readTvRef tv >>= \case
    Link ty -> showInst ty
    _ -> pure Nothing
  -- All prims have an EQ instance
  TyPrim _p -> pure (Just [])
  TyList t -> pure (Just [Pred Show t])
  _ -> pure Nothing

entail :: [Pred (TvRef s)] -> Pred (TvRef s) -> InferM s b i Bool
entail ps p = byInst p >>= \case
  Nothing -> pure False
  Just qs -> and <$> traverse (entail ps) qs

isHnf :: Pred (TvRef s) -> InferM s b i Bool
isHnf (Pred c t) = case t of
  TyVar tv -> readTvRef tv >>= \case
    Link ty -> isHnf (Pred c ty)
    _ -> pure True
  _ -> pure False

toHnf :: Pred (TvRef s) -> InferM s b i [Pred (TvRef s)]
toHnf p = isHnf p >>= \case
  True -> pure [p]
  False -> byInst p >>= \case
    Nothing -> fail "context reduction failure"
    Just ps -> toHnfs ps

toHnfs :: [Pred (TvRef s)] -> InferM s b i [Pred (TvRef s)]
toHnfs ps = do
  pss <- traverse toHnf ps
  pure (concat pss)

simplify :: [Pred (TvRef s)] -> InferM s b i [Pred (TvRef s)]

simplify = loop []
  where
  loop rs [] = pure rs
  loop rs (p:ps) = entail (rs ++ rs) p >>= \cond ->
    if cond then loop rs ps else loop (p:rs) ps

reduce :: [Pred (TvRef s)]-> InferM s b i [Pred (TvRef s)]
reduce ps = toHnfs ps >>= simplify

split :: [TCPred s] -> InferM s b i ([TCPred s], [TCPred s])
split ps = do
  ps' <- reduce ps
  partition' ([], []) ps'
  where
  partition' (ds, rs) (p@(Pred _ ty) : xs) = do
    cond <- hasUnbound ty
    if cond then partition' (p:ds, rs) xs
    else partition' (ds, p:rs) xs
  partition' (ds, rs) [] =
    pure (reverse ds, reverse rs)
  varUnbound ref = readTvRef ref >>= \case
    Unbound{} -> pure True
    Link ty -> hasUnbound ty
    Bound {} -> pure False
  hasUnbound = \case
    TyVar n -> varUnbound n
    TyPrim _ -> pure False
    TyList t -> hasUnbound t
    TyFun l r -> do
      l' <- hasUnbound l
      if l' then pure l' else hasUnbound r
    _ -> pure False

----------------------------------------------------------------------
---- Instantiations
---------------------------------------------------------------------

-- | Instantiate a typescheme with bound variables with fresh bound variables
-- Corresponds to the following inference rule
--
-- That is (∀E):
--     P | Γ ⊢ E : ∀a_1..a_n. ρ
--     b_1, ..,  b_n fresh
--     ---------------------------------------
--     P | Γ ⊢ E ~> E_f[b_1,..,b c_n] : ρ
-- instantiateWithTerm
--   :: TypeScheme (TvRef s)
--   -> TCTerm s b i
--   -> InferM s b i (TCType s, TCTerm s b i, [TCPred s])
-- instantiateWithTerm (TypeScheme ts preds ty) term = do
--   nts <- fmap TyVar <$> traverse (const newTvRef) ts
--   let m = zip ts nts
--   preds' <- traverse (instPred m) preds
--   ty' <- instBound m ty
--   case nts of
--     x:xs -> do
--       let tyapps = Typed.TyApp term (x:|xs) info
--       dvars <- traverse toDVar preds'
--       case dvars of
--         p:ps -> pure (ty', Typed.App tyapps (p:|ps) info, preds')
--         [] -> pure (ty', tyapps, [])
--     [] -> pure (ty', term, [])
--   where
--   info = term ^. Typed.termInfo
  -- toDVar p = do
  --   i <- newSupplyIx
  --   let n = OverloadedName ("_dict" <> T.pack (show i)) (OBuiltinDict p)
  --   pure $ Typed.Var n info
  -- instPred m (Pred n tt) =
  --   Pred n <$> instBound m tt
  -- instBound m = \case
  --   t@(TyVar tv) -> readTvRef tv >>= \case
  --     Bound{} -> case lookup tv m of
  --       Just t' -> pure t'
  --       Nothing -> pure t
  --     Link lt -> instBound m lt
  --     _ -> pure t
  --   TyPrim p -> pure (TyPrim p)
  --   TyFun l r ->
  --     TyFun <$> instBound m l <*> instBound m r
  --   TyList t -> TyList <$> instBound m t
  --   t -> pure t

instantiateImported :: TypeScheme NamedDeBruijn -> InferM s b i (TCType s, [TvRef s], [TCPred s])
instantiateImported  = \case
  TypeScheme tvs preds ty -> do
    ntvs <- traverse (const newTvRef) tvs
    let rl = RAList.fromList (reverse ntvs)
    ty' <- inst rl ty
    preds' <- traverse (instPred rl) preds
    pure (ty', ntvs, preds')
  where
  instPred rl (Pred tc ty) =
    Pred tc <$> inst rl ty
  inst rl = \case
    TyVar (NamedDeBruijn i _) -> pure (TyVar (rl RAList.!! i))
    TyPrim p -> pure (TyPrim p)
    TyFun l r -> TyFun <$> inst rl l <*> inst rl r
    TyList t -> TyList <$> inst rl t
    TyGuard -> pure TyGuard
    -- Impredicative type might work
    -- If we change unification.
    TyForall _ _ -> fail "unsupported impredicative polymorphism"

occurs :: TvRef s -> TCType s -> InferM s b i ()
occurs tv = \case
  TyVar tv' | tv == tv' -> fail "occurs check failed"
  TyVar tv' -> bindRef tv'
  TyFun l r -> occurs tv l *> occurs tv r
  TyList l -> occurs tv l
  _ -> pure ()
  where
  bindRef tv' = readTvRef tv' >>= \case
    Unbound n u l' -> do
      ml <- minLevel
      writeTvRef tv' (Unbound n u ml)
      where
      minLevel = readTvRef tv >>= \case
        Unbound _ _ l -> pure (min l l')
        _ -> pure l'
    Link ty -> occurs tv ty
    _ -> pure ()

unifyTyVar :: TvRef s -> TCType s -> InferM s b i ()
unifyTyVar tv t1 = readTvRef tv >>= \case
  Unbound{} -> do
    occurs tv t1
    writeTvRef tv (Link t1)
  Link t2 -> unify t2 t1
  _ -> pure ()

unify :: TCType s -> TCType s -> InferM s b i ()
unify t1 t2 | t1 == t2 = pure ()
unify (TyVar tv) t = unifyTyVar tv t
unify t (TyVar tv) = unifyTyVar tv t
unify (TyFun l r) (TyFun l' r') = unify l l' *> unify r r'
unify (TyList t) (TyList t') = unify t t'
unify (TyPrim p) (TyPrim p') | p == p' = pure ()
unify _ _ = fail "types do not unify"

-- | We essentially only
-- generalize on lambdas atm.
-- generalizeWithTerm
--   :: TCType s
--   -> [TCPred s]
--   -> TCTerm s b i
--   -> InferM s b i (TypeScheme (TvRef s), TCTerm s b i, [TCPred s])
-- generalizeWithTerm ty pp term = case term of
--   Typed.Lam{} -> generalizeWithTerm' ty pp term
--   _ -> pure (TypeScheme [] [] ty, term, pp)

-- Generalization in general
-- generalizeWithTerm'
--   :: TCType s
--   -> [TCPred s]
--   -> TCTerm s b i
--   -> InferM s b i (TypeScheme (TvRef s), TCTerm s b i, [TCPred s])
-- generalizeWithTerm' ty pp term = do
--   preds <- nubPreds pp
--   sts <- liftST (newSTRef Set.empty)
--   (ftvs, ty') <- gen' sts ty
--   (deferred, retained) <- split preds
--   retained' <- traverse (genPred sts) retained
--   when (retained /= []) $ fail ""
--   case ftvs of
--     [] -> do
--       when (retained /= []) $ fail "Retained predicates despite no quantified variables"
--       pure (TypeScheme [] [] ty' , term, deferred)
--     (x:xs) -> do
--       when (retained /= []) $ fail "Dictionary variables not currently supported"
--       pure (TypeScheme ftvs retained ty', Typed.TyAbs (x:|xs) term info, deferred)
--   where
--   nubPreds li = nubPreds' li []
--   -- we expect
--   nubPreds' (p@(Pred tc x) : xs) elems = case x of
--     TyVar rv -> readTvRef rv >>= \case
--       Link tl -> nubPreds' (Pred tc tl :xs) elems
--       _ ->
--         if elem p elems
--         then nubPreds' xs elems
--         else nubPreds' xs (Pred tc x:elems)
--     _ -> nubPreds' xs elems
--   nubPreds' [] elems = pure (reverse elems)

--   info = term ^. Typed.termInfo
--   toTyArg p@(Pred t tytv) =
--     let n  = OverloadedName "#dictVar" (OBuiltinDict p)
--         nty = tcToRowType t tytv
--     in (n, nty)
--   genPred sts (Pred t pty) = do
--     (o, pty')  <- gen' sts pty
--     when (o /= []) $ fail "BOOM1"
--     pure (Pred t pty')
--   gen' sts (TyVar tv) = readTvRef tv >>= \case
--     Unbound n u l -> do
--       cl <- currentLevel
--       if l > cl then do
--         s <- liftST (readSTRef sts)
--         writeTvRef tv (Bound n u)
--         if Set.member u s then pure ([], TyVar tv)
--         else liftST (writeSTRef sts (Set.insert u s)) $> ([tv], TyVar tv)
--       else pure ([], TyVar tv)
--     Link t' -> gen' sts t'
--     Bound _ _ -> pure ([], TyVar tv)
--   gen' sts (TyFun l r) = do
--     (ftvl, l') <- gen' sts l
--     (ftvr, r') <- gen' sts r
--     pure (ftvl ++ ftvr,TyFun l' r')
--   gen' _ t@TyPrim{} = pure ([], t)
--   gen' sts (TyList t) = over _2 TyList <$> gen' sts t
--   gen' _sts TyGuard = pure ([], TyGuard)
--   gen' _sts t@TyForall{} = pure ([], t)

liftType :: Type Void -> Type a
liftType = fmap absurd

-- toOName :: Name -> OverloadedName b
-- toOName (Name n nk u) =
--   OverloadedName n $ case nk of
--     IRBound -> OBound u
--     IRTopLevel m mh -> OTopLevel m mh

-- Todo: bidirectionality
inferTerm :: IRTerm b i -> InferM s b i (TCType s, TCTerm s b i, [TCPred s])
inferTerm = \case
  IR.Var irn@(Name n nk) i -> case nk of
    NBound u -> do
      views tcVarEnv (`RAList.lookup` u) >>= \case
        Just ty -> do
          let v' = Typed.Var irn i
          pure (ty, v', [])
        Nothing -> fail ("unbound variable in term infer" <> show n)
    NTopLevel mn _mh ->
      views tcFree (preview (ix mn . ix n)) >>= \case
        Just ty -> do
          let newVar = Typed.Var irn i
          pure (liftType ty, newVar, [])
          -- Todo: Remove this commented block when
          -- case tvs of
          --   t:ts -> let
          --     tyVars = TyVar <$> t:|ts
          --     tyApp = Typed.TyApp newVar tyVars i
          --     in case preds of
          --       p:ps -> do
          --         let predVars = OverloadedName "#dictVar" . OBuiltinDict <$> (p :| ps)
          --             predTerms = (`Typed.Var` i) <$> predVars
          --         pure (ty, Typed.App tyApp predTerms i, preds)
          --       [] -> pure (ty, tyApp, preds)
          --   [] -> do
          --     when (preds /= []) $ fail "invariant failure: propagating non parameterized dicts"
          --     pure (ty, newVar, [])
        Nothing -> fail ("unbound free variable in term infer " <> show irn)
  IR.Lam nts e i -> do
    let names = fst <$> nts
    ntys <- traverse withTypeInfo nts
    -- Todo: bidirectionality
    -- let m = IntMap.fromList $ NE.toList $ NE.zipWith (\n t ->  (_irUnique n, t)) names ntys
    let m = RAList.fromList (reverse (NE.toList ntys))
    (ty, e', preds) <- locally tcVarEnv (m RAList.++) $ inferTerm e
    let nts' = NE.zip names ntys
        rty = foldr TyFun ty ntys
    pure (rty, Typed.Lam nts' e' i, preds)
    where
    withTypeInfo p = case snd p of
      Just ty -> pure (liftType ty)
      Nothing -> TyVar <$> newTvRef
  IR.App e args i -> do
    tv1 <- TyVar <$> newTvRef
    (te, e', pte) <- inferTerm e
    as <- traverse inferTerm args
    let tys = view _1 <$> as
        args' = view _2 <$> as
        preds' = concat (pte : NE.toList (view _3 <$> as))
    unify te (foldr TyFun tv1 tys)
    pure (tv1, Typed.App e' args' i, preds')
  IR.Let n mty e1 e2 i -> do
    enterLevel
    (te1, e1', pe1) <- inferTerm e1
    leaveLevel
    _ <- _Just (unify te1 . liftType) mty
    -- (ts, e1Qual, deferred) <- generalizeWithTerm te1 pe1 e1Unqual
    (te2, e2', pe2) <- locally tcVarEnv (RAList.cons te1) $ inferTerm e2
    pure (te2, Typed.Let n e1' e2' i, pe1 ++ pe2)
  IR.Sequence e1 e2 i -> do
    (_, e1', pe1) <- inferTerm e1
    (te2, e2', pe2) <- inferTerm e2
    pure (te2, Typed.Sequence e1' e2' i, pe1 ++ pe2)
  -- Todo: Here, convert to dictionary
  IR.Builtin b i -> do
    tyImported <- views tcBuiltins ($ b)
    (ty, tvs, preds) <- instantiateImported tyImported
    let tvs' = TyVar <$> tvs
    let term' = Typed.Builtin (b, tvs', preds) i
    pure (ty, term', preds)
  -- TODO: note,
  -- for this to work, we have to have proper bidirectionality working, including scoped type variables working fine
  IR.Constant l i ->
    pure (typeOfLit l, Typed.Constant l i,[])
  -- -- note: object literals are closed rows.
  -- IR.ObjectLit _obj _i -> undefined
  -- -- Todo: comment this case out better.
  -- IR.ObjectOp _oop _i -> undefined
  IR.ListLit li i -> do
    tv <- TyVar <$> newTvRef
    liTup <- traverse inferTerm li
    let preds = concat (view _3 <$> liTup)
    traverse_ (unify tv . view _1) liTup
    pure (TyList tv, Typed.ListLit tv (view _2 <$> liTup) i, preds)
  IR.Try e1 e2 i -> do
    (te1, e1', p1) <- inferTerm e1
    (te2, e2', p2)<- inferTerm e2
    unify te1 te2
    pure (te1, Typed.Try e1' e2' i, p1 ++ p2)
  IR.Error e i -> do
    ty <- TyVar <$> newTvRef
    pure (ty, Typed.Error ty e i, [])

-- Todo: generic types?
-- We can't generalize yet since
-- we're not allowing type schemes just yet.
inferDefun
  :: IR.Defun Name b i
  -> InferM s b i (TypedDefun b i)
inferDefun (IR.Defun name dfTy term info) = do
  enterLevel
  (termTy, term', preds) <- inferTerm term
  leaveLevel
  (deferred, retained) <- split preds
  unless (null deferred && null retained) $ fail "typeclass constraints not supported in defun"
  unify (liftType dfTy) termTy
  fterm <- noTyVarsinTerm term'
  pure (Typed.Defun name (liftType dfTy) fterm info)

inferDefConst
  :: IR.DefConst Name b i
  -> InferM s b i (TypedDefConst b i)
inferDefConst (IR.DefConst name dcTy term info) = do
  enterLevel
  (termTy, term', preds) <- inferTerm term
  leaveLevel
  fterm <- noTyVarsinTerm term'
  let dcTy' = liftType <$> dcTy
  _ <- maybe (pure ()) (`unify` termTy) dcTy'
  unless (null preds) $ fail "typeclass constraints not supported in defun"
  rty' <- noTypeVariables (maybe termTy id dcTy')
  pure (Typed.DefConst name rty' fterm info)

inferDef
  :: IR.Def Name b i
  -> InferM s b i (TypedDef b i)
inferDef = \case
  IR.Dfun d -> Typed.Dfun <$> inferDefun d
  IR.DConst d -> Typed.DConst <$> inferDefConst d
  -- IR.DCap d -> Typed.DCap <$> inferDefCap d

inferModule
  :: IR.Module Name b i
  -> InferM s b i (TypedModule b i)
inferModule (IR.Module mname defs blessed imports impl mh) = do
  -- gov' <- traverse (dbjName [] 0 . toOName ) gov
  fv <- Map.insert mname mempty <$> view tcFree
  (defs', _) <- foldlM infer' ([], fv) defs
  pure (Typed.Module mname (reverse defs') blessed imports impl mh)
  where
  infer' (xs, m) d = do
    def' <- local (set tcFree m) (inferDef d)
    let name' = Typed.defName def'
        ty = liftType (Typed.defType def')
        m' = Map.adjust (Map.insert name' ty) mname  m
    pure (def':xs, m')

-- | Note: debruijnizeType will
-- ensure that terms that are generic will fail
inferTermNonGen :: IRTerm b i -> InferM s b i (TypedTerm b i)
inferTermNonGen t = do
  (ty, t',preds) <- inferTerm t
  (deferred, retained) <- split preds
  unless (null deferred && null retained) $ fail "term inferred with generic signature"
  _ <- noTypeVariables ty
  noTyVarsinTerm t'

inferTopLevel
  :: IR.TopLevel Name b i
  -> InferM s b i (TypedTopLevel b i)
inferTopLevel = \case
  IR.TLModule m -> Typed.TLModule <$> inferModule m
  IR.TLTerm m -> Typed.TLTerm <$> inferTermNonGen m
  IR.TLInterface _ -> error "todo: implement interface inference"

inferReplTopLevel
  :: IR.ReplTopLevel Name b i
  -> InferM s b i (TypedReplTopLevel b i)
inferReplTopLevel = \case
  IR.RTLModule m -> Typed.RTLModule <$> inferModule m
  IR.RTLTerm m -> Typed.RTLTerm <$> inferTermNonGen m
  IR.RTLDefun dfn -> Typed.RTLDefun <$> inferDefun dfn
  IR.RTLDefConst dconst -> Typed.RTLDefConst <$> inferDefConst dconst
  IR.RTLInterface _ -> error "todo: implement interface inference"

inferProgram
  :: [IR.TopLevel Name b i]
  -> InferM s b i [TypedTopLevel b i]
inferProgram = traverse inferTopLevel

inferReplProgram
  :: [IR.ReplTopLevel Name b i]
  -> InferM s b i [TypedReplTopLevel b i]
inferReplProgram = traverse inferReplTopLevel


-- Todo: check interfaces
-- inferModule (IR.Module mname gov defs )

-- -- | Transform types into their debruijn-indexed version
-- -- Essentially: Start at depth 0:
-- --  rename : (Term, Γ, Int) -> IxTerm
-- --  rename (ΛX.e, tyEnv, DEPTH) = Λ. (rename (e, tyEnv[depth/X], DEPTH+1))
-- --  .. other cases are simply renaming recursively and calling `renameType`
-- --  on occurences of Type
-- --
-- --  NOTE: the passed in DEPTH is 1 higher than the highest binder.
-- --
-- --  renameType : (Type, Γ, Int) -> IxType
-- --  renameType (a, env, DEPTH) = DEPTH - env(a) - 1
-- --  .. other recursive cases are straightforward
-- --
-- --  Quip: when we debruijnize types, we expect no impredicative polymorphism atm,
-- --  thus we will fail on universially quantified types found in application and
-- --  var binding sites.
-- --  The typechecker does not spit out impredicative polymorphism, but while
-- --  it would be trivial to support their renaming here, I'd rather fail
-- --  for now as the typechecker does not support it and it functions as a sanity check
-- debruijnizeTermTypes :: TCTerm s b i -> InferM s b i (TypedTerm b i)
-- debruijnizeTermTypes = dbj [] 0
  -- where
  -- dbj :: [(TvRef s, NamedDeBruijn)] -> DeBruijn -> TCTerm s b i -> InferM s b i (TypedTerm b i)
  -- dbj env depth = \case
  --   Typed.Var n i ->
  --     pure (Typed.Var n i)
  --   Typed.Lam nts e i -> do
  --     nts' <- _2 (dbjTyp env depth) nts
  --     e' <- dbj env depth e
  --     pure (Typed.Lam nts' e' i)
      -- where
      -- dbjBoth (nn, tty) = do
      --   nn' <- dbjName env depth nn
      --   tty' <- dbjTyp env depth tty
      --   pure (nn', tty')
    -- Typed.App l r i ->
    --   Typed.App <$> dbj env depth l <*> traverse (dbj env depth) r <*> pure i
    -- Typed.Let n e1 e2 i -> do
    --   e1' <- dbj env depth e1
    --   e2' <- dbj env depth e2
    --   pure (Typed.Let n e1' e2' i)
    -- Typed.TyApp e args i -> do
    --   e' <- dbj env depth e
    --   args' <- traverse (dbjTyp env depth) args
    --   pure (Typed.TyApp e' args' i)
    -- Typed.Block nel i ->
    --   Typed.Block <$> traverse (dbj env depth) nel <*> pure i
    -- Typed.ObjectLit obj i ->
    --   Typed.ObjectLit <$> traverse (dbj env depth) obj <*> pure i
    -- Typed.ObjectOp oop i -> fmap (`Typed.ObjectOp` i) $ case oop of
    --   ObjectAccess f o -> do
    --     o' <- dbj env depth o
    --     pure (ObjectAccess f o')
    --   ObjectRemove f o -> do
    --     o' <- dbj env depth o
    --     pure (ObjectRemove f o')
    --   ObjectExtend f v o -> do
    --     v' <- dbj env depth v
    --     o' <- dbj env depth o
    --     pure (ObjectExtend f v' o')
    -- Typed.ListLit ty v i ->
    --   Typed.ListLit <$> dbjTyp env depth ty <*> traverse (dbj env depth) v <*> pure i
    -- Typed.Builtin (b, tys, preds) i -> do
    --   tys' <- traverse (dbjTyp env depth) tys
    --   preds' <- traverse (dbjPred env depth) preds
    --   pure (Typed.Builtin (b, tys', preds') i)
    -- Typed.Constant l i -> pure (Typed.Constant l i)



nameTvs :: DeBruijn -> (TvRef s, DeBruijn) -> InferM s b i NamedDeBruijn
nameTvs depth (nt, i) = readTvRef nt >>= \case
  Bound n _ -> pure (NamedDeBruijn (depth - i - 1) n)
  _ -> fail "found unbound variable"

noTypeVariables :: TCType s -> InferM s b i (Type Void)
noTypeVariables = \case
  TyVar n -> readTvRef n >>= \case
    Link ty -> noTypeVariables ty
    _ -> fail "Found bound or unbound type variable"
  TyPrim p -> pure (TyPrim p)
  TyFun l r -> TyFun <$> noTypeVariables l <*> noTypeVariables r
  TyList l -> TyList <$> noTypeVariables l
  TyGuard -> pure TyGuard
  TyForall _ _ -> fail "Encountered universal quantification emitted by the typechecker. Impossible"

noTyVarsinPred :: TCPred s -> InferM s b i (Pred Void)
noTyVarsinPred (Pred tc ty) = Pred tc <$> noTypeVariables ty

noTyVarsinTerm :: TCTerm s b i -> InferM s b i (TypedTerm b i)
noTyVarsinTerm = \case
  Typed.Var n i ->
    pure (Typed.Var n i)
  Typed.Lam nts e i ->
    Typed.Lam <$> (traversed._2) noTypeVariables nts <*> noTyVarsinTerm e <*> pure i
  Typed.App e args i ->
    Typed.App <$> noTyVarsinTerm e <*> traverse noTyVarsinTerm args <*> pure i
  Typed.Let n e1 e2 i ->
    Typed.Let n <$> noTyVarsinTerm e1 <*> noTyVarsinTerm e2 <*> pure i
  Typed.Builtin (b, ty, p) i -> do
    ty' <- traverse noTypeVariables ty
    p' <- traverse noTyVarsinPred p
    pure $ Typed.Builtin (b, ty', p') i
  Typed.Constant l i -> pure (Typed.Constant l i)
  Typed.TyApp l tys i ->
    Typed.TyApp <$> noTyVarsinTerm l <*> traverse noTypeVariables tys <*> pure i
  Typed.Sequence e1 e2 i ->
    Typed.Sequence <$> noTyVarsinTerm e1 <*> noTyVarsinTerm e2 <*> pure i
  Typed.ListLit ty li i ->
    Typed.ListLit <$> noTypeVariables ty <*> traverse noTyVarsinTerm li <*> pure i
  Typed.Try e1 e2 i ->
    Typed.Try <$> noTyVarsinTerm e1 <*> noTyVarsinTerm e2 <*> pure i
  Typed.Error t e i ->
    Typed.Error <$> noTypeVariables t <*> pure e <*> pure i

-- dbjName
--   :: [(TvRef s, NamedDeBruijn)]
--   -> DeBruijn
--   -> OverloadedName (TCPred s)
--   -> InferM s b i (OverloadedName (Pred NamedDeBruijn))
-- dbjName env depth (OverloadedName n kind) = fmap (OverloadedName n) $ case kind of
--   OBound b -> pure (OBound b)
--   OTopLevel m h -> pure (OTopLevel m h)
--   OBuiltinDict b -> OBuiltinDict <$> dbjPred env depth b

-- debruijnizeTypeScheme :: TypeScheme (TvRef s) -> InferM s b i (TypeScheme NamedDeBruijn)
-- debruijnizeTypeScheme (TypeScheme tvs preds t) = do
--     let len = fromIntegral (length tvs)
--     let ixs = [0.. len - 1]
--     names <- traverse (nameTvs len) (zip tvs ixs)
--     let env = zip tvs names
--     t' <- dbjTyp env len t
--     preds' <- traverse (dbjPred env len) preds
--     pure (TypeScheme names preds' t')

-- debruijnizeType :: TCType s -> InferM s b i (Type NamedDeBruijn)
-- debruijnizeType = dbjTyp [] 0

-- dbjPred
--   :: [(TvRef s, NamedDeBruijn)]
--   -> DeBruijn
--   -> TCPred s
--   -> InferM s b i (Pred NamedDeBruijn)
-- dbjPred env depth (Pred tc ty) =
--   Pred tc <$> dbjTyp env depth ty

-- dbjTyp
--   :: [(TvRef s, NamedDeBruijn)]
--   -> DeBruijn
--   -> TCType s
--   -> InferM s b i (Type NamedDeBruijn)
-- dbjTyp env depth = \case
--   TyVar n -> case lookup n env of
--     Just v -> pure (TyVar v)
--     Nothing -> readTvRef n >>= \case
--       Unbound {} -> fail "unbound type"
--       Bound{} -> fail "invariant failure: bound variable otuside "
--       Link ty -> dbjTyp env depth ty
--   TyPrim p -> pure (TyPrim p)
--   TyFun l r -> TyFun <$> dbjTyp env depth l <*> dbjTyp env depth r
--   TyList l -> TyList <$> dbjTyp env depth l
--   _ -> fail "impredicative"

-- -----------------------------------------
-- --- Built-in type wiring
-- ------------------------------------------

-- -- todo: debruijnize automatically
rawBuiltinType :: RawBuiltin -> TypeScheme NamedDeBruijn
rawBuiltinType = \case
  -- Add
  RawAdd ->
    addBinopType
  -- Num
  RawSub ->
    numBinopType
  RawMultiply ->
    numBinopType
  RawDivide ->
    numBinopType
  RawNegate ->
    unaryNumType
  RawAbs ->
    unaryNumType
  -- Bool ops
  RawAnd ->
    binaryBool
  RawOr ->
    binaryBool
  RawNot ->
    TypeScheme [] [] (TyBool :~> TyBool)
  -- Eq ops
  RawEq ->
    eqTyp
  RawNeq ->
    eqTyp
  -- Ord ops
  RawGT ->
    ordTyp
  RawGEQ ->
    ordTyp
  RawLT ->
    ordTyp
  RawLEQ ->
    ordTyp
  -- Integer ops
  RawBitwiseAnd ->
    binaryInt
  RawBitwiseOr ->
    binaryInt
  RawBitwiseXor ->
    binaryInt
  RawBitwiseFlip ->
    unaryInt
  RawMod ->
    binaryInt
  RawBitShift ->
    unaryInt
  -- Rounding ops
  RawRound ->
    roundingFn
  RawCeiling ->
    roundingFn
  RawFloor ->
    roundingFn
  -- Fractional
  RawExp ->
    unaryFractional
  RawLn ->
    unaryFractional
  RawSqrt ->
    unaryFractional
  RawLogBase ->
    let aVar = nd "a" 0
        a = TyVar aVar
    in TypeScheme [aVar] [Pred Fractional a] (a :~> a :~> a)
  -- ListLike
  RawConcat ->
    let aVar = nd "a" 0
        a = TyVar aVar
    in TypeScheme [aVar] [Pred ListLike a] (TyList a :~> a)
  RawTake ->
    takeDropTy
  RawDrop ->
    takeDropTy
  RawLength ->
    let aVar = nd "a" 0
        a = TyVar aVar
    in TypeScheme [aVar] [Pred ListLike a] (a :~> TyInt)
  RawReverse ->
    let aVar = nd "a" 0
        a = TyVar aVar
    in TypeScheme [aVar] [Pred ListLike a] (a :~> a)
  -- General
  RawMap ->
    let aVar = nd "a" 1
        bVar = nd "b" 0
        a = TyVar aVar
        b = TyVar bVar
    in TypeScheme [aVar, bVar] [] ((a :~> b) :~> TyList a :~> TyList b)
  RawFold ->
    let aVar = nd "a" 1
        bVar = nd "b" 0
        a = TyVar aVar
        b = TyVar bVar
    in TypeScheme [aVar, bVar] [] ((a :~> b :~> a) :~> a :~> TyList b :~> a)
  RawFilter ->
    let aVar = nd "a" 0
        a = TyVar aVar
    in TypeScheme [aVar] [] ((a :~> TyBool) :~> TyList a :~> TyList a)
  RawZip ->
    let aVar = nd "a" 2
        a = TyVar aVar
        bVar = nd "b" 1
        b = TyVar bVar
        cVar = nd "c" 0
        c = TyVar cVar
    in TypeScheme [aVar, bVar, cVar] [] ((a :~> b :~> c) :~> TyList a :~> TyList b :~> TyList c)
  RawIf ->
    let aVar = nd "a" 0
        a = TyVar aVar
    in TypeScheme [aVar] [] (TyBool :~> (TyUnit :~> a) :~> (TyUnit :~> a) :~> a)
  RawIntToStr ->
    TypeScheme [] [] (TyInt :~> TyString)
  RawStrToInt ->
    TypeScheme  [] [] (TyString :~> TyInt)
  RawDistinct ->
    let aVar  = nd "a" 0
        a = TyVar aVar
    in TypeScheme [aVar] [Pred Eq a] (TyList a :~> TyList a)
  RawEnforce ->
    TypeScheme [] [] (TyBool :~> TyString :~> TyUnit)
  RawEnforceOne -> error "todo"
  RawEnumerate ->
    TypeScheme [] [] (TyInt :~> TyInt :~> TyList TyInt)
  RawEnumerateStepN ->
    TypeScheme [] [] (TyInt :~> TyInt :~> TyInt :~> TyList TyInt)
  RawShow ->
    let aVar = nd "a" 0
        a = TyVar aVar
    in TypeScheme [aVar] [Pred Show a] (a :~> TyString)
  RawReadInteger ->
    TypeScheme [] [] (TyString :~> TyInt)
  RawReadDecimal ->
    TypeScheme [] [] (TyString :~> TyDecimal)
  RawReadString ->
    TypeScheme [] [] (TyString :~> TyString)
  -- RawReadKeyset ->
  --   TypeScheme [] [] (TyString :~> TyGuard)
  -- RawEnforceGuard ->
  --   TypeScheme [] [] (TyGuard :~> TyUnit)
  -- RawKeysetRefGuard ->
  --   TypeScheme [] [] (TyString :~> TyGuard)
  -- RawCreateUserGuard -> let
  --   a = nd "a" 0
  --   in TypeScheme [a] [] ((TyUnit :~> TyVar a) :~> TyGuard)
  RawListAccess -> let
    a = nd "a" 0
    in TypeScheme [a] [] (TyInt :~> TyList (TyVar a) :~> TyVar a)
  RawB64Encode ->
    TypeScheme [] [] (TyString :~> TyString)
  RawB64Decode ->
    TypeScheme [] [] (TyString :~> TyString)
  where
  nd b a = NamedDeBruijn a b
  unaryNumType =
    let aVar = nd "a" 0
        a = TyVar aVar
    in TypeScheme [aVar] [Pred Num a] (a :~> a)
  unaryFractional =
    let aVar = nd "a" 0
        a = TyVar aVar
    in TypeScheme [aVar] [Pred Fractional a] (a :~> TyDecimal)
  addBinopType =
    let aVar = nd "a" 0
        a = TyVar aVar
    in TypeScheme [aVar] [Pred Add a] (a :~> a :~> a)
  numBinopType =
    let aVar = nd "a" 0
        a = TyVar aVar
    in TypeScheme [aVar] [Pred Num a] (a :~> a :~> a)
  eqTyp =
    let aVar = nd "a" 0
        a = TyVar aVar
    in TypeScheme [aVar] [Pred Eq a] (a :~> a :~> TyBool)
  ordTyp =
    let aVar = nd "a" 0
        a = TyVar aVar
    in TypeScheme [aVar] [Pred Ord a] (a :~> a :~> TyBool)
  unaryInt = TypeScheme [] [] (TyInt :~> TyInt)
  -- integer -> integer -> integer
  binaryInt = TypeScheme [] [] (TyInt :~> TyInt :~> TyInt)
  -- decimal -> integer
  roundingFn = TypeScheme [] [] (TyDecimal :~> TyInt)
  -- bool -> bool -> bool
  binaryBool = TypeScheme [] [] (TyBool :~> TyBool :~> TyBool)
  -- forall a. ListLike a => int -> a -> a
  takeDropTy =
    let aVar = nd "a" 0
        a = TyVar aVar
    in TypeScheme [aVar] [Pred ListLike a] (TyInt :~> a :~> a)

mkFree :: Loaded builtin info -> Map ModuleName (Map Text (Type Void))
mkFree loaded = let
  tl = _loModules loaded
  toTy d = (Untyped.defName d, Untyped.defType d)
  mdefs =  Untyped._mDefs . _mdModule <$> tl
  in Map.fromList . fmap toTy <$> mdefs

runInfer
  :: Loaded b' i'
  -> (b -> TypeScheme NamedDeBruijn)
  -> InferM s b i a
  -> ST s a
runInfer loaded bfn (InferT act) = do
  uref <- newSTRef 0
  lref <- newSTRef 1
  let tcs = TCState uref mempty bfn (mkFree loaded) lref
  runReaderT act tcs

runInferTerm
  :: Loaded b' i'
  -> (b -> TypeScheme NamedDeBruijn)
  -> IRTerm b i
  -> IO (Type Void, TypedTerm b i)
runInferTerm loaded bfn term0 = stToIO $
  runInfer loaded bfn $ do
    enterLevel
    (ty, term1, preds) <- inferTerm term0
    leaveLevel
    (deferred, retained) <- split preds
    unless (null deferred && null retained) $ fail "term inferred with generic signature"
    ty' <- noTypeVariables ty
    tt <- noTyVarsinTerm term1
    pure (ty', tt)

runInferModule
  :: Loaded b' i'
  -> (b -> TypeScheme NamedDeBruijn)
  -> IRModule b i
  -> IO (TypedModule b i)
runInferModule loaded bfn term0 =
  stToIO $ runInfer loaded bfn (inferModule term0)

runInferTopLevel
  :: Loaded b' i'
  -> (b -> TypeScheme NamedDeBruijn)
  -> IR.TopLevel Name b i
  -> IO (TypedTopLevel b i)
runInferTopLevel l bfn tl =
  stToIO $ runInfer l bfn (inferTopLevel tl)

runInferProgram
  :: Loaded b' i'
  -> (b -> TypeScheme NamedDeBruijn)
  -> [IR.TopLevel Name b info]
  -> IO [TypedTopLevel b info]
runInferProgram l bfn prog =
  stToIO $ runInfer l bfn $ inferProgram prog

runInferReplProgram
  :: Loaded b' i'
  -> (b -> TypeScheme NamedDeBruijn)
  -> [IR.ReplTopLevel Name b info]
  -> IO [TypedReplTopLevel b info]
runInferReplProgram l bfn prog =
  stToIO $ runInfer l bfn $ inferReplProgram prog
