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
 , replBuiltinType
 ) where

import Control.Lens hiding (Level)
import Control.Monad.Reader
import Control.Monad.ST
-- import Control.Monad.ST.Unsafe(unsafeIOToST, unsafeSTToIO)
import Control.Monad.State.Strict
import Control.Monad.Except
-- import Control.Exception(throwIO, catch)
import Data.Void
-- import Data.Dynamic (Typeable)
import Data.RAList(RAList)
import Data.Foldable(traverse_, foldlM)
import Data.Functor(($>))
import Data.STRef
import Data.Map(Map)
import Data.Text(Text)
import Data.List.NonEmpty(NonEmpty(..))

import qualified Data.Map.Strict as Map
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.RAList as RAList
import qualified Data.Set as Set

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
  -- ^ Type Variable "Region"
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
type TypedTerm b i = Typed.OverloadedTerm NamedDeBruijn b i

type TypedGenTerm b i = Typed.OverloadedTerm NamedDeBruijn b i

type TypedDefun b i = Typed.OverloadedDefun NamedDeBruijn b i

type TypedDefConst b i = Typed.OverloadedDefConst NamedDeBruijn b i

type TypedDef b i = Typed.OverloadedDef NamedDeBruijn b i

type TypedTopLevel b i = Typed.OverloadedTopLevel NamedDeBruijn b i

type TypedReplTopLevel b i = Typed.OverloadedReplTopLevel NamedDeBruijn b i

type TypedModule b i = Typed.OverloadedModule NamedDeBruijn b i

newtype InferM s b i a =
  InferT (ExceptT (PactError i) (ReaderT (TCState s b) (ST s)) a)
  deriving
    ( Functor, Applicative, Monad
    , MonadReader (TCState s b)
    , MonadError (PactError i))
  via (ExceptT (PactError i) (ReaderT (TCState s b) (ST s)))

liftST :: ST s a -> InferM s b i a
liftST action = InferT (ExceptT (Right <$> ReaderT (const action)))

throwTypecheckError :: TypecheckError -> i -> InferM s b i a
throwTypecheckError te = throwError . PETypecheckError te

_dbgTypedTerm
  :: TCTerm s b i
  -> InferM s b i (Typed.Term Text Text (b, [Type Text], [Pred Text]) i)
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
  Typed.TyAbs tys term i -> do
    tys' <- traverse _dbgTvRef tys
    term' <- _dbgTypedTerm term
    pure (Typed.TyAbs tys' term' i)
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

_dbgTypeScheme :: TypeScheme (TvRef s) -> InferM s b i (TypeScheme Text)
_dbgTypeScheme (TypeScheme tvs preds ty) = do
  tvs' <- traverse rv tvs
  preds' <- traverse _dbgPred preds
  ty' <- _dbgType ty
  pure (TypeScheme tvs' preds' ty')
  where
  rv n = readTvRef n >>= \case
    Unbound u l _ -> pure ("unbound" <> T.pack (show (u, l)))
    Bound u l -> pure ("bound" <> T.pack (show (u, l)))
    Link _ -> pure "linktv"

_dbgTvRef :: TvRef s -> InferM s b i Text
_dbgTvRef tv = readTvRef tv >>= \case
    Unbound u l _ -> pure ("unbound" <> T.pack (show (u, l)))
    Bound u l -> pure ("bound" <> T.pack (show (u, l)))
    Link ty -> do
      ty' <- _dbgType ty
      pure $ "linked type<" <> T.pack (show ty') <> ">"

_dbgPred :: TCPred s -> InferM s b i (Pred Text)
_dbgPred (Pred i t) = Pred i <$> _dbgType t

_dbgType :: TCType s -> InferM s b i (Type Text)
_dbgType = \case
  TyVar tv -> readTvRef tv >>= \case
    Unbound u l _ -> pure (TyVar ("unbound" <> T.pack (show (u, l))))
    Bound u l -> pure (TyVar ("bound" <> T.pack (show (u, l))))
    Link ty -> _dbgType ty
  TyFun l r -> TyFun <$> _dbgType l <*> _dbgType r
  TyList t -> TyList <$> _dbgType t
  TyPrim p -> pure (TyPrim p)
  TyGuard -> pure TyGuard
  TyForall {} -> error "impredicative"


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

toHnf :: Pred (TvRef s) -> i -> InferM s b i [Pred (TvRef s)]
toHnf p i = isHnf p >>= \case
  True -> pure [p]
  False -> byInst p >>= \case
    Nothing -> do
      p' <- _dbgPred p
      throwTypecheckError (ContextReductionError p') i
    Just ps -> toHnfs ps i

toHnfs :: [Pred (TvRef s)] -> i -> InferM s b i [Pred (TvRef s)]
toHnfs ps i = do
  pss <- traverse (`toHnf` i) ps
  pure (concat pss)

simplify :: [Pred (TvRef s)] -> InferM s b i [Pred (TvRef s)]

simplify = loop []
  where
  loop rs [] = pure rs
  loop rs (p:ps) = entail (rs ++ rs) p >>= \cond ->
    if cond then loop rs ps else loop (p:rs) ps

reduce :: [Pred (TvRef s)]-> i -> InferM s b i [Pred (TvRef s)]
reduce ps i = toHnfs ps i >>= simplify

split
  :: [TCPred s]
  -> i
  -> InferM s b i ([TCPred s], [TCPred s])
split ps i = do
  ps' <- reduce ps i
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

checkReducible :: [Pred (TvRef s)] -> i -> InferM s b i ()
checkReducible ps i =
  reduce ps i >>= \case
    [] -> pure ()
    xs -> do
      xs' <- traverse _dbgPred xs
      throwTypecheckError (UnsupportedTypeclassGeneralization xs') i

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

instantiateImported
  :: TypeScheme NamedDeBruijn
  -> i
  -> InferM s b i (TCType s, [TvRef s], [TCPred s])
instantiateImported (TypeScheme tvs preds ty) i = do
    ntvs <- traverse (const newTvRef) tvs
    let rl = RAList.fromList (reverse ntvs)
    ty' <- inst rl ty
    preds' <- traverse (instPred rl) preds
    pure (ty', ntvs, preds')
  where
  instPred rl (Pred tc pty) =
    Pred tc <$> inst rl pty
  inst rl = \case
    TyVar (NamedDeBruijn i' _) -> pure (TyVar (rl RAList.!! i'))
    TyPrim p -> pure (TyPrim p)
    TyFun l r -> TyFun <$> inst rl l <*> inst rl r
    TyList t -> TyList <$> inst rl t
    TyGuard -> pure TyGuard
    -- Impredicative type might work
    -- If we change unification.
    TyForall _ _ ->
      throwTypecheckError UnsupportedImpredicativity i

occurs
  :: TvRef s
  -> TCType s
  -> i
  -> InferM s b i ()
occurs tv tct i = case tct of
  TyVar tv' | tv == tv' -> do
    tv'' <- _dbgType tct
    throwTypecheckError (OccursCheckFailure tv'') i
  TyVar tv' -> bindRef tv'
  TyFun l r -> occurs tv l i *> occurs tv r i
  TyList l -> occurs tv l i
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
    Link ty -> occurs tv ty i
    _ -> pure ()

unifyTyVar
  :: TvRef s
  -> TCType s
  -> i
  -> InferM s b i ()
unifyTyVar tv t1 i = readTvRef tv >>= \case
  Unbound{} -> do
    occurs tv t1 i
    writeTvRef tv (Link t1)
  Link t2 -> unify t2 t1 i
  _ -> pure ()

unify
  :: TCType s
  -> TCType s
  -> i
  -> InferM s b i ()
unify t1 t2 _ | t1 == t2 = pure ()
unify (TyVar tv) t i = unifyTyVar tv t i
unify t (TyVar tv) i = unifyTyVar tv t i
unify (TyFun l r) (TyFun l' r') i = unify l l' i *> unify r r' i
unify (TyList t) (TyList t') i = unify t t' i
unify (TyPrim p) (TyPrim p') _ | p == p' = pure ()
unify t1 t2 i = do
  t1' <- _dbgType t1
  t2' <- _dbgType t2
  throwTypecheckError (UnificationError t1' t2') i

-- | We essentially only
-- generalize on lambdas atm.
generalizeWithTerm
  :: TCType s
  -> [TCPred s]
  -> TCTerm s b i
  -> InferM s b i (TypeScheme (TvRef s), TCTerm s b i, [TCPred s])
generalizeWithTerm ty pp term
  | isValue term = generalizeWithTerm' ty pp term
  | otherwise = do
    pp' <- reduce pp (view Typed.termInfo term)
    pure (TypeScheme [] [] ty, term, pp')
  where
  isValue = \case
    Typed.Var{} -> True
    Typed.Constant{} -> True
    Typed.Lam{} -> True
    Typed.Error{} -> True
    Typed.Builtin{} -> True
    _ -> False

-- Generalization that emits a typed term
-- Note: Deferred predicates are purely for the sake of
-- callsite dictionary overloaded variables.
-- These are currently disabled.
generalizeWithTerm'
  :: TCType s
  -> [TCPred s]
  -> TCTerm s b i
  -> InferM s b i (TypeScheme (TvRef s), TCTerm s b i, [TCPred s])
generalizeWithTerm' ty pp term = do
  preds <- nubPreds pp
  ((ftvs, ty'), s) <- runStateT (gen' ty) Set.empty
  (deferred, retained) <- split preds (view Typed.termInfo term)
  retained' <- evalStateT (traverse genPred retained) s
  when (retained' /= []) $ do
    retained'' <- traverse _dbgPred retained
    throwTypecheckError (UnsupportedTypeclassGeneralization retained'') info
  case ftvs of
    [] -> do
      pure (TypeScheme [] [] ty' , term, deferred)
    (x:xs) -> do
      pure (TypeScheme ftvs [] ty', Typed.TyAbs (x:|xs) term info, deferred)
  where
  nubPreds li = nubPreds' li []
  -- we expect
  nubPreds' (p@(Pred tc x) : xs) elems = case x of
    TyVar rv -> readTvRef rv >>= \case
      Link tl -> nubPreds' (Pred tc tl :xs) elems
      _ ->
        if elem p elems
        then nubPreds' xs elems
        else nubPreds' xs (Pred tc x:elems)
    _ -> nubPreds' xs elems
  nubPreds' [] elems = pure (reverse elems)
  info = term ^. Typed.termInfo
  genPred (Pred t pty) = do
    (o, pty')  <- gen' pty
    when (o /= []) $
      lift (throwTypecheckError (TCInvariantFailure "Generalizing predicates") info)
    pure (Pred t pty')
  gen' (TyVar tv) = lift (readTvRef tv) >>= \case
    Unbound n u l -> do
      cl <- lift currentLevel
      if l > cl then do
        lift (writeTvRef tv (Bound n u))
        gets (Set.member u) >>= \case
          True -> pure ([], TyVar tv)
          False -> modify' (Set.insert u) $> ([tv], TyVar tv)
      else pure ([], TyVar tv)
    Link t' -> gen' t'
    Bound _ _ -> pure ([], TyVar tv)
  gen' (TyFun l r) = do
    (ftvl, l') <- gen' l
    (ftvr, r') <- gen' r
    pure (ftvl ++ ftvr,TyFun l' r')
  gen' t@TyPrim{} = pure ([], t)
  gen' (TyList t) = over _2 TyList <$> gen' t
  gen' TyGuard = pure ([], TyGuard)
  gen' t@TyForall{} = pure ([], t)

liftType :: Type Void -> Type a
liftType = fmap absurd

-- Todo: bidirectionality
inferTerm :: IRTerm b i -> InferM s b i (TCType s, TCTerm s b i, [TCPred s])
inferTerm = \case
  IR.Var irn@(Name n nk) i -> case nk of
    NBound u -> do
      views tcVarEnv (`RAList.lookup` u) >>= \case
        Just ty -> do
          let v' = Typed.Var irn i
          pure (ty, v', [])
        Nothing ->
          throwTypecheckError (TCUnboundTermVariable n) i
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
        Nothing ->
          throwTypecheckError (TCUnboundFreeVariable mn n) i
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
    unify te (foldr TyFun tv1 tys) i
    pure (tv1, Typed.App e' args' i, preds')
  IR.Let n mty e1 e2 i -> do
    enterLevel
    (te1, e1', pe1) <- inferTerm e1
    leaveLevel
    _ <- _Just (\te2 -> unify te1 (liftType te2) i) mty
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
    (ty, tvs, preds) <- instantiateImported tyImported i
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
    traverse_ (\(t,_, _) -> unify tv t i) liTup
    pure (TyList tv, Typed.ListLit tv (view _2 <$> liTup) i, preds)
  IR.Try e1 e2 i -> do
    (te1, e1', p1) <- inferTerm e1
    (te2, e2', p2)<- inferTerm e2
    unify te1 te2 i
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
  checkReducible preds (view IR.termInfo term)
  -- fail "typeclass constraints not supported in defun"
  unify (liftType dfTy) termTy info
  fterm <- noTyVarsinTerm info term'
  pure (Typed.Defun name (liftType dfTy) fterm info)

inferDefConst
  :: IR.DefConst Name b i
  -> InferM s b i (TypedDefConst b i)
inferDefConst (IR.DefConst name dcTy term info) = do
  enterLevel
  (termTy, term', preds) <- inferTerm term
  leaveLevel
  checkReducible preds info
  fterm <- noTyVarsinTerm info term'
  let dcTy' = liftType <$> dcTy
  _ <- maybe (pure ()) (\dct -> unify dct termTy info) dcTy'
  rty' <- noTypeVariables info (maybe termTy id dcTy')
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
  (ty, t', preds) <- inferTerm t
  checkReducible preds (view IR.termInfo t)
  _ <- noTypeVariables (view IR.termInfo t) ty
  noTyVarsinTerm (view IR.termInfo t) t'

inferTermGen
  :: IRTerm b i
  -> InferM s b i (TypeScheme NamedDeBruijn, TypedGenTerm b i)
inferTermGen term = do
  let info = view IR.termInfo term
  enterLevel
  (ty, term0 , preds) <- inferTerm term
  leaveLevel
  (tys', typedTerm, deferred) <- generalizeWithTerm ty preds term0
  unless (null deferred) $ do
      deferred' <- traverse _dbgPred deferred
      throwTypecheckError (UnsupportedTypeclassGeneralization deferred') (view IR.termInfo term)
  dbjTyScheme <- debruijnizeTypeScheme info tys'
  dbjTerm <- debruijnizeTermTypes info typedTerm
  pure (dbjTyScheme, dbjTerm)

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


-- | Transform types into their debruijn-indexed version
-- Essentially: Start at depth 0:
--  rename : (Term, Γ, Int) -> IxTerm
--  rename (ΛX.e, tyEnv, DEPTH) = Λ. (rename (e, tyEnv[depth/X], DEPTH+1))
--  .. other cases are simply renaming recursively and calling `renameType`
--  on occurences of Type
--
--  NOTE: the passed in DEPTH is 1 higher than the highest binder.
--
--  renameType : (Type, Γ, Int) -> IxType
--  renameType (a, env, DEPTH) = DEPTH - env(a) - 1
--  .. other recursive cases are straightforward
--
--  Quip: when we debruijnize types, we expect no impredicative polymorphism atm,
--  thus we will fail on universially quantified types found in application and
--  var binding sites.
--  The typechecker does not spit out impredicative polymorphism, but while
--  it would be trivial to support their renaming here, I'd rather fail
--  for now as the typechecker does not support it and it functions as a sanity check
debruijnizeTermTypes
  :: forall s b i. i
  -> TCTerm s b i
  -> InferM s b i (Typed.Term Name NamedDeBruijn (b, [Type NamedDeBruijn], [Pred NamedDeBruijn]) i)
debruijnizeTermTypes info = dbj [] 0
  where
  dbj
    :: [(TvRef s, NamedDeBruijn)]
    -> DeBruijn
    -> TCTerm s b i
    -> InferM s b i (Typed.Term Name NamedDeBruijn (b, [Type NamedDeBruijn], [Pred NamedDeBruijn]) i)
  dbj env depth = \case
    Typed.Var n i ->
      pure (Typed.Var n i)
    Typed.Lam nts e i -> do
      nts' <- (traversed._2) (dbjTyp info env depth) nts
      e' <- dbj env depth e
      pure (Typed.Lam nts' e' i)
    Typed.App l r i ->
      Typed.App <$> dbj env depth l <*> traverse (dbj env depth) r <*> pure i
    Typed.Let n e1 e2 i -> do
      e1' <- dbj env depth e1
      e2' <- dbj env depth e2
      pure (Typed.Let n e1' e2' i)
    Typed.TyAbs ntys e i -> do
      let len = fromIntegral (NE.length ntys)
          ixs = NE.fromList [depth .. depth + len - 1]
      names <- traverse (nameTvs info (depth + len)) (NE.zip ntys ixs)
      let env' = NE.toList $ NE.zip ntys names
      Typed.TyAbs names <$> dbj (env' ++ env) (depth + len) e <*> pure i
    Typed.TyApp e args i -> do
      e' <- dbj env depth e
      args' <- traverse (dbjTyp info env depth) args
      pure (Typed.TyApp e' args' i)
    Typed.Sequence e1 e2 i ->
      Typed.Sequence <$> dbj env depth e1 <*> dbj env depth e2 <*> pure i
    Typed.Try e1 e2 i ->
      Typed.Try <$> dbj env depth e1 <*> dbj env depth e2 <*> pure i
    Typed.Error t e i -> do
      ty <- dbjTyp info env depth t
      pure (Typed.Error ty e i)
    Typed.ListLit ty v i ->
      Typed.ListLit <$> dbjTyp info env depth ty <*> traverse (dbj env depth) v <*> pure i
    Typed.Builtin (b, tys, preds) i -> do
      tys' <- traverse (dbjTyp info env depth) tys
      preds' <- traverse (dbjPred info env depth) preds
      pure (Typed.Builtin (b, tys', preds') i)
    Typed.Constant l i -> pure (Typed.Constant l i)


nameTvs
  :: i
  -> DeBruijn
  -> (TvRef s, DeBruijn)
  -> InferM s b i NamedDeBruijn
nameTvs info depth (nt, i) = readTvRef nt >>= \case
  Bound n _ ->
    pure (NamedDeBruijn (depth - i - 1) n)
  _ ->
    throwTypecheckError (TCInvariantFailure "Found unbound variable during generalization") info

noTypeVariables
  :: i
  -> TCType s
  -> InferM s b i (Type a)
noTypeVariables i = \case
  TyVar n -> readTvRef n >>= \case
    Link ty -> noTypeVariables i ty
    _ ->
      throwTypecheckError (DisabledGeneralization "Inferred generic signature") i
  TyPrim p -> pure (TyPrim p)
  TyFun l r -> TyFun <$> noTypeVariables i l <*> noTypeVariables i r
  TyList l -> TyList <$> noTypeVariables i l
  TyGuard -> pure TyGuard
  TyForall _ _ ->
    throwTypecheckError (TCInvariantFailure "Encountered universal quantification emitted by the typechecker. Impossible") i

noTyVarsinPred
  :: i
  -> TCPred s
  -> InferM s b i (Pred NamedDeBruijn)
noTyVarsinPred i (Pred tc ty) = Pred tc <$> noTypeVariables i ty

noTyVarsinTerm
  :: i
  -> TCTerm s b i
  -> InferM s b i (TypedTerm b i)
noTyVarsinTerm info = \case
  Typed.Var n i ->
    pure (Typed.Var n i)
  Typed.Lam nts e i ->
    Typed.Lam <$> (traversed._2) (noTypeVariables info) nts <*> noTyVarsinTerm info e <*> pure i
  Typed.App e args i ->
    Typed.App <$> noTyVarsinTerm info e <*> traverse (noTyVarsinTerm info) args <*> pure i
  Typed.Let n e1 e2 i ->
    Typed.Let n <$> noTyVarsinTerm info e1 <*> noTyVarsinTerm info e2 <*> pure i
  Typed.Builtin (b, ty, p) i -> do
    ty' <- traverse (noTypeVariables info) ty
    p' <- traverse (noTyVarsinPred info) p
    pure $ Typed.Builtin (b, ty', p') i
  Typed.TyAbs _ns _e _i ->
    throwTypecheckError (DisabledGeneralization "Generic terms are disabled") info
  Typed.Constant l i ->
    pure (Typed.Constant l i)
  Typed.TyApp l tys i ->
    Typed.TyApp
      <$> noTyVarsinTerm info l
      <*> traverse (noTypeVariables info) tys
      <*> pure i
  Typed.Sequence e1 e2 i ->
    Typed.Sequence <$> noTyVarsinTerm info e1 <*> noTyVarsinTerm info e2 <*> pure i
  Typed.ListLit ty li i ->
    Typed.ListLit <$> noTypeVariables info ty <*> traverse (noTyVarsinTerm info) li <*> pure i
  Typed.Try e1 e2 i ->
    Typed.Try <$> noTyVarsinTerm info e1 <*> noTyVarsinTerm info e2 <*> pure i
  Typed.Error t e i ->
    Typed.Error <$> noTypeVariables info t <*> pure e <*> pure i

-- dbjName
--   :: [(TvRef s, NamedDeBruijn)]
--   -> DeBruijn
--   -> OverloadedName (TCPred s)
--   -> InferM s b i (OverloadedName (Pred NamedDeBruijn))
-- dbjName env depth (OverloadedName n kind) = fmap (OverloadedName n) $ case kind of
--   OBound b -> pure (OBound b)
--   OTopLevel m h -> pure (OTopLevel m h)
--   OBuiltinDict b -> OBuiltinDict <$> dbjPred env depth b

debruijnizeTypeScheme
  :: i
  -> TypeScheme (TvRef s)
  -> InferM s b i (TypeScheme NamedDeBruijn)
debruijnizeTypeScheme i (TypeScheme tvs preds t) = do
    let len = fromIntegral (length tvs)
    let ixs = [0.. len - 1]
    names <- traverse (nameTvs i len) (zip tvs ixs)
    let env = zip tvs names
    t' <- dbjTyp i env len t
    preds' <- traverse (dbjPred i env len) preds
    pure (TypeScheme names preds' t')

debruijnizeType
  :: i
  -> TCType s
  -> InferM s b i (Type NamedDeBruijn)
debruijnizeType i = dbjTyp i [] 0

dbjPred
  :: i
  -> [(TvRef s, NamedDeBruijn)]
  -> DeBruijn
  -> TCPred s
  -> InferM s b i (Pred NamedDeBruijn)
dbjPred i env depth (Pred tc ty) =
  Pred tc <$> dbjTyp i env depth ty

dbjTyp
  :: i
  -> [(TvRef s, NamedDeBruijn)]
  -> DeBruijn
  -> TCType s
  -> InferM s b i (Type NamedDeBruijn)
dbjTyp i env depth = \case
  TyVar n -> case lookup n env of
    Just v -> pure (TyVar v)
    Nothing -> readTvRef n >>= \case
      Unbound {} ->
        throwTypecheckError (TCInvariantFailure "Found unbound type variable after type checking") i
      Bound{} ->
        throwTypecheckError (TCInvariantFailure "Found bound variable outside of the calculated set") i
      Link ty -> dbjTyp i env depth ty
  TyPrim p -> pure (TyPrim p)
  TyFun l r -> TyFun <$> dbjTyp i env depth l <*> dbjTyp i env depth r
  TyList l -> TyList <$> dbjTyp i env depth l
  TyGuard -> pure TyGuard
  TyForall{} ->
    throwTypecheckError (TCInvariantFailure "Found impredicative Type") i

-- -----------------------------------------
-- --- Built-in type wiring
-- ------------------------------------------
replBuiltinType :: (b -> TypeScheme NamedDeBruijn) -> ReplBuiltin b -> TypeScheme NamedDeBruijn
replBuiltinType f = \case
  RBuiltinWrap b -> f b
  RExpect -> let
    aVar = nd "a" 0
    aTv = TyVar aVar
    in TypeScheme [aVar] [Pred Eq aTv, Pred Show aTv] (TyString :~> aTv :~> (TyUnit :~> aTv) :~> TyString)
  RExpectFailure -> let
    aVar = nd "a" 0
    aTv = TyVar aVar
    in TypeScheme [aVar] [] (TyString :~> (TyUnit :~> aTv) :~> TyString)
  RExpectThat -> let
    aVar = nd "a" 0
    aTv = TyVar aVar
    in TypeScheme [aVar] [] (TyString :~> (aTv :~> TyBool) :~> aTv :~> TyString)
  RPrint -> let
    aVar = nd "a" 0
    aTv = TyVar aVar
    in TypeScheme [aVar] [Pred Show aTv] (aTv :~> TyUnit)
  where
  nd b a = NamedDeBruijn a b

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
  -> ST s (Either (PactError i) a)
runInfer loaded bfn (InferT act) = do
  uref <- newSTRef 0
  lref <- newSTRef 1
  let tcs = TCState uref mempty bfn (mkFree loaded) lref
  runReaderT (runExceptT act) tcs

runInferTerm
  :: Loaded b' i'
  -> (b -> TypeScheme NamedDeBruijn)
  -> IRTerm b i
  -> Either (PactError i) (TypeScheme NamedDeBruijn, TypedGenTerm b i)
runInferTerm loaded bfn term0 = runST $
  runInfer loaded bfn $ inferTermGen term0

runInferTermNonGen
  :: Loaded b' i'
  -> (b -> TypeScheme NamedDeBruijn)
  -> IRTerm b i
  -> Either (PactError i) (TypedTerm b i)
runInferTermNonGen loaded bfn term0 = runST $
  runInfer loaded bfn $ inferTermNonGen term0

runInferModule
  :: Loaded b' i'
  -> (b -> TypeScheme NamedDeBruijn)
  -> IRModule b i
  -> Either (PactError i) (TypedModule b i)
runInferModule loaded bfn term0 =
  runST $ runInfer loaded bfn (inferModule term0)

runInferTopLevel
  :: Loaded b' i'
  -> (b -> TypeScheme NamedDeBruijn)
  -> IR.TopLevel Name b i
  -> Either (PactError i) (TypedTopLevel b i)
runInferTopLevel l bfn tl =
  runST $ runInfer l bfn (inferTopLevel tl)

runInferProgram
  :: Loaded b' i'
  -> (b -> TypeScheme NamedDeBruijn)
  -> [IR.TopLevel Name b info]
  -> Either (PactError info) [TypedTopLevel b info]
runInferProgram l bfn prog =
  runST $ runInfer l bfn $ inferProgram prog

runInferReplProgram
  :: Loaded b' i'
  -> (b -> TypeScheme NamedDeBruijn)
  -> [IR.ReplTopLevel Name b info]
  -> Either (PactError info) [TypedReplTopLevel b info]
runInferReplProgram l bfn prog =
  runST $ runInfer l bfn $ inferReplProgram prog
