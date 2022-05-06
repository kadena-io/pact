{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}

module Pact.Core.Typed.Term
( Defun(..)
, DefConst(..)
, DefCap(..)
, DefPact(..)
, Def(..)
, Term(..)
, Module(..)
, Interface(..)
, IfDefun(..)
, IfDef(..)
, Literal(..)
, DefType(..)
, TyVarType(..)
, termInfo
)
 where

import Control.Lens
import Data.Map.Strict(Map)
import Data.List.NonEmpty(NonEmpty)
import Data.Vector (Vector)
import qualified Data.Set as Set


import Pact.Core.Names
import Pact.Core.Type
import Pact.Core.Imports
import Pact.Types.Hash (Hash)
import Pact.Types.Term (ModuleHash(..), Governance(..))
import Pact.Types.Exp (Literal(..))

data DefType
  = DTDefCap
  | DTDefun
  | DTDefPact
  | DTDefConst
  deriving Show

data Defun name tyname builtin info
  = Defun
  { _dfunName :: name
  , _dfunTerm :: Term name tyname builtin info
  , _dfunTermType :: Maybe (Type tyname)
  } deriving Show

data DefConst name tyname builtin info
  = DefConst
  { _dcName :: name
  , _dcTerm :: Term name tyname builtin info
  , _dcTermType :: Maybe (Type tyname)
  } deriving Show

data DefCap name tyname builtin info
  = DefCap
  { _dcapName :: name
  , _dcapTerm :: Term name tyname builtin info
  , _dcapTermType :: Maybe (Type tyname)
  } deriving Show

-- Todo :: probably need a special form
-- either structurally, or these typecheck different.
data DefPact name tyname builtin info
  = DefPact
  { _dpName :: name
  , _dpTerm :: Term name tyname builtin info
  , _dpTermType :: Maybe (Type tyname)
  } deriving Show

data DefSchema name tyname info
  = DefSchema
  { _dsName :: name
  , _dsType :: RowObject tyname
  } deriving Show

data DefTable name tyname info
  = DefTable
  { _dtName :: name
  , _dtType :: Type tyname
  } deriving Show

data Def name tyname builtin info
  = Dfun (Defun name tyname builtin info)
  | DConst (DefConst name tyname builtin info)
  | DPact (DefPact name tyname builtin info)
  | DSchema (DefSchema name tyname info)
  | DTable (DefTable name tyname info)
  deriving Show

data Module name tyname builtin info
  = Module
  { _mName :: ModuleName
  , _mGovernance :: Governance (DefCap name tyname builtin info)
  , _mDefs :: [Def name tyname builtin info]
  , _mBlessed :: !(Set.Set ModuleHash)
  , _mImports :: [Import]
  , _mHash :: ModuleHash
  } deriving Show

data Interface name tyname builtin info
  = Interface
  { _ifName :: name
  , _ifDefns :: [IfDef name tyname builtin info]
  , _ifHash :: Hash
  } deriving Show

data IfDefun name tyname info
  = IfDefun
  { _ifdName :: name
  , _ifdType :: Type tyname
  , _ifdInfo :: info
  } deriving Show

data IfDef name tyname builtin info
  = IfDfun (IfDefun name tyname info)
  | IFDConst (DefConst name tyname builtin info)
  deriving Show

data TopLevel name tyname builtin info
  = TLModule (Module name tyname builtin info)
  | TLInterface (Interface name tyname builtin info)
  | TLTerm (Term name tyname builtin info)
  deriving Show

data TyVarType
  = TyVarType
  | RowVarType
  deriving Show

-- | Typed pact core terms
data Term name tyname builtin info
  = Var name info
  -- ^ single variables, e.g the term `x`
  | Lam name (NonEmpty name) (NonEmpty (Type tyname)) (Term name tyname builtin info) info
  -- ^ f = \a b c -> e
  -- All lambdas, even anonymous ones, are named, for the sake of them adding a stack frame
  | App (Term name tyname builtin info) (NonEmpty (Term name tyname builtin info)) info
  -- ^ (e_1 e_2 .. e_n)
  | TyApp (Term name tyname builtin info) (NonEmpty (Type tyname, TyVarType)) info
  -- ^ (e_1 @t)
  | TyAbs (NonEmpty (tyname, TyVarType)) (Term name tyname builtin info) info
  -- ^ ΛX.e
  | Block (NonEmpty (Term name tyname builtin info)) info
  -- ^ ΛX.e
  | ObjectLit (Row tyname) (Map Field (Term name tyname builtin info)) info
  -- ^ {f_1:e_1, .., f_n:e_n}
  | ListLit (Type tyname) (Vector (Term name tyname builtin info)) info
  -- ^ [e_1, e_2, .., e_n]
  | Error String (Type tyname) info
  -- ^ error terms + their inferred type
  | Builtin builtin info
  -- ^ Built-in functions (or natives)
  | Constant Literal info
  -- ^ Constant/Literal values
  deriving (Show)

-- fromIR
--   :: Ord tyname
--   => IO name
--   -> IR.Term (name, Type tyname) tyname builtin (i, Type tyname)
--   -> IO (Term name tyname builtin i)
-- fromIR mkName = \case
--   IR.Var (n, _) (i, _) -> pure (Var n i)
--   -- Todo: for generalized nodes, tyAbs inserted here.
--   IR.Lam (n, ty) ns _ body (i, _) -> do
--     t <- Lam n (fst <$> ns) (snd <$> ns) <$> fromIR mkName body <*> pure i
--     pure (mkAbsLam i ty t)
--   -- Todo: type apps here.
--   IR.App l r (i, _) ->
--     collectApps l (pure r) i
--   IR.Let (n, typ) _ e1 e2 (i, _) ->
--     collectLets e2 (pure (n, typ)) (pure e1) i
--   IR.Constant l (i, _) -> pure (Constant l i)
--   IR.Block terms (i, _) ->
--     Block <$> traverse (fromIR mkName) terms <*> pure i
--   IR.Error errstr (i, ty) ->
--     pure (Error errstr ty i)
--   IR.ObjectLit objs (i, rty) -> do
--     objs' <- traverse (fromIR mkName) objs
--     case rty of
--       TyRow row ->
--         pure (ObjectLit row objs' i)
--       _ -> error "internal, fatal error: type inferred for object not a row type"
--   IR.ListLit objs (i, ty) ->
--     ListLit ty <$> traverse (fromIR mkName) objs <*> pure i
--   IR.Builtin b (i, _) -> pure (Builtin b i)
--   IR.DynAccess _mod _fn _i -> error "unsupported atm"
--   where
--   mkAbsLam _ (TyForall [] [] _) lam = lam
--   -- It is guaranteed that the concat of both is not empty
--   mkAbsLam i (TyForall xs ys _) lam =
--     let tyAbs = (,TyVarType) <$> xs
--         rowAbs = (,RowVarType) <$> ys
--         allAbs = NE.fromList (tyAbs ++ rowAbs)
--     in TyAbs allAbs lam i
--   mkAbsLam _ _ lam = lam
--   collectLets (IR.Let (n, typ) _ e1 e2 _) ns apps i =
--     collectLets e2 (NE.cons (n, typ) ns) (NE.cons e1 apps) i
--   collectLets t ans appArgs i = do
--     name <- mkName
--     let (ns, tys) = NE.unzip ans
--     lamT <- Lam name ns tys <$> fromIR mkName t <*> pure i
--     appArgs' <- traverse (fromIR mkName) appArgs
--     let lamAppArgs = uncurry (mkAbsLam i) <$> NE.zip tys appArgs'
--     pure (App lamT lamAppArgs i)
--   collectApps (IR.App l r _) rs i =
--     collectApps l (NE.cons r rs) i
--   collectApps t collected i =
--     mkTyApp t collected i
--     -- App <$> fromIR mkName t <*> traverse (formIR mkName) collected <*> pure i
--   mkTyApp l@(IR.Lam (_, ty) _ _ _ _) appArgs i = case ty of
--     TyForall [] [] _ ->
--       App <$> fromIR mkName l <*> traverse (fromIR mkName) appArgs <*> pure i
--     TyForall tvs rvs typ -> case tyFunToArgList typ of
--       Just (NE.fromList -> funArgs, _) -> do
--         let argTys = view (IR.termInfo._2) <$> appArgs
--             substs = foldMap (uncurry tyAppUnify) $ NE.zip funArgs argTys
--             tvApps = (\n ->  fromMaybe (TyVar n, TyVarType) (Map.lookup n substs)) <$> tvs
--             rvApps = (\n -> fromMaybe (TyRow (RowVar n), RowVarType) (Map.lookup n substs)) <$> rvs
--         l' <- fromIR mkName l
--         appArgs' <- traverse (fromIR mkName) appArgs
--         let tyApp = (TyApp l' (NE.fromList (tvApps ++ rvApps)) i)
--         pure (App tyApp appArgs' i)
--       Nothing -> error "oop???"
--     _ ->
--       App <$> fromIR mkName l <*> traverse (fromIR mkName) appArgs <*> pure i
--   mkTyApp l appArgs i =
--     App <$> fromIR mkName l <*> traverse (fromIR mkName) appArgs <*> pure i



-- -- Left biased unification.
-- -- It is only to generate type/row applications
-- tyAppUnify :: (Ord n) => Type n -> Type n -> Map n (Type n, TyVarType)
-- tyAppUnify (TyVar n) ty = Map.singleton n (ty, TyVarType)
-- tyAppUnify (TyFun l r) (TyFun l' r') =
--   tyAppUnify l l' <> tyAppUnify r r'
-- tyAppUnify (TyList l) (TyList r) =
--   tyAppUnify l r
-- tyAppUnify (TyRow lrow) (TyRow rrow) = rowUnifies lrow rrow
--   where
--   rowUnifies (RowVar r) r' = Map.singleton r (TyRow r', RowVarType)
--   rowUnifies (RowTy obj (Just r)) (RowTy obj' r') =
--     let notInL = Map.difference obj' obj
--     in Map.singleton r (TyRow (RowTy notInL r'), RowVarType)
--   rowUnifies (RowTy _ (Just r)) EmptyRow =
--     Map.singleton r (TyRow EmptyRow, RowVarType)
--   rowUnifies (RowTy _ (Just r)) rv =
--     Map.singleton r (TyRow rv, RowVarType)
--   rowUnifies _ _ = Map.empty
-- tyAppUnify _ _ = Map.empty

termInfo :: Lens' (Term name tyname builtin info) info
termInfo f = \case
  Var n i -> Var n <$> f i
  Lam n ns ty term i -> Lam n ns ty term <$> f i
  App t1 t2 i -> App t1 t2 <$> f i
  TyApp term ty i -> TyApp term ty <$> f i
  TyAbs ty term i -> TyAbs ty term <$> f i
  Block terms i -> Block terms <$> f i
  ObjectLit row obj i -> ObjectLit row obj <$> f i
  ListLit ty v i -> ListLit ty v <$> f i
  Error s ty i -> Error s ty <$> f i
  Builtin b i -> Builtin b <$> f i
  Constant l i -> Constant l <$> f i

instance Plated (Term name tyname builtin info) where
  plate f = \case
    Var n i -> pure (Var n i)
    Lam n ns ty term i -> Lam n ns ty <$> f term <*> pure i
    App t1 t2 i -> App <$> f t1 <*> traverse f t2 <*> pure i
    TyApp term ty i -> TyApp <$> f term <*> pure ty <*> pure i
    TyAbs ty term i -> TyAbs ty <$> f term <*> pure i
    ObjectLit ty tm i ->
      ObjectLit ty <$> traverse f tm <*> pure i
    ListLit ty ts i ->
      ListLit ty <$> traverse f ts <*> pure i
    Block terms i -> Block <$> traverse f terms <*> pure i
    Error s ty i -> pure (Error s ty i)
    Builtin b i -> pure (Builtin b i)
    Constant l i -> pure (Constant l i)

