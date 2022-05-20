{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}

module Pact.Core.IR.Desugar where


import Control.Monad.Reader
import Control.Lens hiding (List)
import Data.Text(Text)
import Data.Map.Strict(Map)
import Data.List.NonEmpty(NonEmpty(..))
import qualified Data.Map.Strict as Map
import qualified Data.List.NonEmpty as NE
import qualified Data.Vector as V

import Pact.Core.Builtin
import Pact.Core.Names
import Pact.Core.Type
import Pact.Core.Literal

import Pact.Core.IR.Term as Term
import qualified Pact.Core.IR.ParseTree as PT

type DesugarT = ReaderT DesugarState IO

data DesugarState
  = DesugarState
  { _dsBinds :: Map Text (Unique, IRNameKind)
  , _dsModuleBinds :: Map QualifiedName Unique
  , _dsTyBinds :: Map Text Unique
  , _dsSupply :: Supply
  }

makeLenses ''DesugarState

-- todo: this qualifier use could work for aliasing!
toIRName :: Unique -> IRNameKind -> ParsedName -> IRName
toIRName u irnk = \case
  BN (BareName bn) -> IRName bn irnk u
  QN (QualifiedName bn qual) -> IRName bn (IRTopLevelName qual) u

newUnique' :: DesugarT Unique
newUnique' = view dsSupply >>= liftIO . newUnique

resolveUnique :: ParsedName -> DesugarT (Maybe (Unique, IRNameKind))
resolveUnique = \case
  BN (BareName bn) ->
    views dsBinds (Map.lookup bn)
  QN qn ->
    fmap (,IRTopLevelName (_qnModName qn)) <$> views dsModuleBinds (Map.lookup qn)

nameInEnv :: IRName -> DesugarState -> DesugarState
nameInEnv irn = case _irNameKind irn of
  IRLocallyBoundName ->
    over dsBinds $ Map.insert (_irName irn) (_irUnique irn, IRLocallyBoundName)
  IRTopLevelName qn ->
    over dsModuleBinds $ Map.insert (QualifiedName (_irName irn) qn) (_irUnique irn)
  _ -> error "todo: unsupported name kind"

localName :: IRName -> DesugarT a -> DesugarT a
localName irn = local (nameInEnv irn)

desugarTerm :: PT.Expr ParsedName i -> DesugarT (Term IRName TypeVar RawBuiltin i)
desugarTerm = \case
  PT.Var (BN n) i | isReservedNative (_bnName n) ->
    pure (Builtin (rawBuiltinMap Map.! _bnName n) i)
  PT.Var n i ->
    resolveUnique n >>= \case
      Just (u, irnk) -> pure $ Var (toIRName u irnk n) i
      Nothing -> error "unbound variable"
  PT.Block (h :| hs) _ ->
    unLetBlock h hs
  -- Names will always come out are barenames from the parser
  PT.Let name mt expr i -> do
    u <- newUnique'
    let name' = toIRName u IRLocallyBoundName name
    expr' <- desugarTerm expr
    mt' <- traverse desugarType mt
    pure (Let name' mt' expr' (Constant LUnit i) i)
  PT.Lam name nsts body i -> do
    let (ns, ts) = NE.unzip nsts
    name' <- resolveLamName name
    ns' <- traverse resolveLamArgName ns
    term' <- lamsArgsInEnv ns' $ desugarTerm body
    ts' <- (traverse.traverse) desugarType ts
    pure $ Lam name' ns' ts' term' i
  PT.If cond e1 e2 i -> do
    cond' <- desugarTerm cond
    e1' <- desugarTerm e1
    e2' <- desugarTerm e2
    pure $ App (Builtin RawIf i) (cond' :| [e1', e2']) i
  PT.App e [] i ->
    App <$> desugarTerm e <*> pure (Constant LUnit i :| []) <*> pure i
  PT.App e (h:hs) i -> do
    e' <- desugarTerm e
    h' <- desugarTerm h
    hs' <- traverse desugarTerm hs
    pure $ App e' (h' :| hs') i
  PT.BinaryOp bop e1 e2 i -> do
    e1' <- desugarTerm e1
    e2' <- desugarTerm e2
    pure $ App (Builtin (desugarBinary bop) i) (e1' :| [e2']) i
  PT.UnaryOp uop e1 i -> do
    e1' <- desugarTerm e1
    pure $ App (Builtin (desugarUnary uop) i) (e1' :| []) i
  PT.List e1 i ->
    ListLit <$> fmap V.fromList (traverse desugarTerm e1) <*> pure i
  PT.Constant l i ->
    pure $ Constant l i
  PT.Object objs i ->
    ObjectLit <$> traverse desugarTerm objs <*> pure i
  PT.Error text i ->
    pure $ Error text i
  where
  isReservedNative n =
    elem n rawBuiltinNames
  resolveLamName n = resolveUnique n >>= \case
    Just (u, irnk) -> pure $ toIRName u irnk n
    Nothing -> do
      u <- newUnique'
      pure $ toIRName u IRLocallyBoundName n
  lamsArgsInEnv irns = do
    locally dsBinds $ Map.union $ Map.fromList $ NE.toList $ fmap (\(IRName n k u) -> (n, (u, k))) irns
  resolveLamArgName = \case
    b@(BN _) -> do
      u <- newUnique'
      pure $ toIRName u IRLocallyBoundName b
    _ -> error "lambda argument is qualified: impossible"
  unLetBlock (PT.Let name mt expr i) (h:hs) = do
    u <- newUnique'
    let name' = toIRName u IRLocallyBoundName name
    expr' <- desugarTerm expr
    mt' <- traverse desugarType mt
    Let name' mt' expr' <$> localName name' (unLetBlock h hs) <*> pure i
  unLetBlock other l = case l of
    h:hs -> do
      other' <- desugarTerm other
      unLetBlock h hs >>= \case
        Block nel' i' ->
          pure $ Block (NE.cons other' nel') i'
        t -> pure $ Block (other' :| [t]) (other ^. PT.termInfo)
    [] -> desugarTerm other

desugarType :: PT.Type -> DesugarT (Type TypeVar)
desugarType = \case
  PT.TyVar v ->
    TyVar <$> lookupTyVar v
  PT.TyPrim p -> pure $ TyPrim p
  PT.TyFun l r ->
    TyFun <$> desugarType l <*> desugarType r
  PT.TyObject o mt -> do
    o' <- traverse desugarType o
    mt' <- traverse lookupTyVar mt
    pure (TyRow (RowTy o' mt'))
  PT.TyList t ->
    TyList <$> desugarType t
  PT.TyCap -> pure $ TyCap
  where
  lookupTyVar v = views dsTyBinds (Map.lookup v) >>= \case
    Just u ->
      pure $ TypeVar v u
    Nothing -> do
      u <- newUnique'
      pure $ TypeVar v u

desugarUnary :: PT.UnaryOp -> RawBuiltin
desugarUnary = \case
  PT.NegateOp -> RawNegate
  PT.FlipBitsOp -> RawBitwiseFlip

desugarBinary :: PT.BinaryOp -> RawBuiltin
desugarBinary = \case
  PT.AddOp -> RawAdd
  PT.SubOp -> RawSub
  PT.MultOp -> RawMultiply
  PT.DivOp -> RawDivide
  PT.GTOp -> RawGT
  PT.GEQOp -> RawGEQ
  PT.LTOp -> RawLT
  PT.LEQOp -> RawLEQ
  PT.EQOp -> RawEq
  PT.NEQOp -> RawNeq
  PT.BitAndOp -> RawBitwiseAnd
  PT.BitOrOp -> RawBitwiseOr

