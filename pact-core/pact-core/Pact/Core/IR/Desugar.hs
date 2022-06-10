{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}

module Pact.Core.IR.Desugar where


import Control.Monad.Reader
import Control.Lens hiding (List,ix)
import Data.Text(Text)
import Data.Map.Strict(Map)
import Data.List.NonEmpty(NonEmpty(..))
import Data.IORef
import qualified Data.Map.Strict as Map
import qualified Data.List.NonEmpty as NE
import qualified Data.Vector as V

import Pact.Core.Builtin
import Pact.Core.Names
import Pact.Core.Type
import Pact.Core.Literal

import Pact.Core.IR.Term as Term
import qualified Pact.Core.Syntax.New.ParseTree as PT

type DesugarT = ReaderT DesugarState IO

data DesugarState
  = DesugarState
  { _dsBinds :: Map Text (IRNameKind, Unique)
  , _dsModuleBinds :: Map QualifiedName IRNameKind
  , _dsTyBinds :: Map Text Unique
  , _dsSupply :: IORef Supply
  }

makeLenses ''DesugarState

newUnique' :: DesugarT Unique
newUnique' = do
  sup <- view dsSupply
  u <- lift (readIORef sup)
  lift (modifyIORef' sup (+ 1))
  pure u

resolve :: ParsedName -> DesugarT (Maybe (IRNameKind, Unique))
resolve = \case
  BN (BareName bn) ->
    views dsBinds (Map.lookup bn)
  QN qn ->
    fmap (, -1111) <$> views dsModuleBinds (Map.lookup qn)

desugarTerm :: PT.Expr ParsedName i -> DesugarT (Term IRName TypeVar RawBuiltin i)
desugarTerm = \case
  PT.Var (BN n) i | isReservedNative (_bnName n) ->
    pure (Builtin (rawBuiltinMap Map.! _bnName n) i)
  PT.Var n i -> do
    resolve n >>= \case
      Just (nk, u) -> do
        let n' = IRName (rawParsedName n) nk u
        pure (Var n' i)
      Nothing -> error ("unbound variable " <> show n)
  PT.Block (h :| hs) _ ->
    unLetBlock h hs
  -- Names will always come out are barenames from the parser
  PT.Let name mt expr i -> do
    nu <- newUnique'
    let name' = IRName name IRBound nu
    expr' <- desugarTerm expr
    mt' <- traverse desugarType mt
    pure (Let name' mt' expr' (Constant LUnit i) i)
  PT.LetIn name mt e1 e2 i -> do
    nu <- newUnique'
    let name' = IRName name IRBound nu
    e1' <- desugarTerm e1
    mt' <- traverse desugarType mt
    e2' <- locally dsBinds (Map.insert name (IRBound, nu)) $ desugarTerm e2
    pure (Let name' mt' e1' e2' i)
  PT.Lam name nsts body i -> do
    name' <- resolveLamName name
    let (ns, ts) = NE.unzip nsts
    nUniques <- traverse (const newUnique') ns
    let m = Map.fromList $ NE.toList $ NE.zip ns ((IRBound,) <$> nUniques)
        ns' = NE.zipWith (\n u -> IRName n IRBound u) ns nUniques
    term' <- lamArgsInEnv m $ desugarTerm body
    ts' <- (traverse.traverse) desugarType ts
    pure $ Lam name' (NE.zip ns' ts') term' i
  PT.If cond e1 e2 i -> do
    cond' <- desugarTerm cond
    e1' <- suspend i e1
    e2' <- suspend i e2
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
  PT.ObjectOp o i ->
    ObjectOp <$> traverse desugarTerm o <*> pure i
  PT.Error text i ->
    pure $ Error text i
  where
  suspend i e = do
    nu <- newUnique'
    let name = IRName "#ifArg" IRBound nu
    e' <- desugarTerm e
    pure (Lam name ((name, Nothing) :| []) e' i)
  isReservedNative n =
    elem n rawBuiltinNames
  resolveLamName n = resolve n >>= \case
    Just (nk, u) -> pure $ IRName (rawParsedName n) nk u
    Nothing -> do
      u <- newUnique'
      pure (IRName (rawParsedName n) IRBound u)
  lamArgsInEnv m =
    locally dsBinds (Map.union m)
  unLetBlock (PT.Let name mt expr i) (h:hs) = do
    u <- newUnique'
    let name' = IRName name IRBound u
    expr' <- desugarTerm expr
    mt' <- traverse desugarType mt
    let inEnv = over dsBinds (Map.insert name (IRBound, u))
    e2 <- local inEnv $ unLetBlock h hs
    pure (Let name' mt' expr' e2 i)
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
  PT.AndOp -> RawAnd
  PT.OrOp -> RawOr

runDesugarTerm :: PT.Expr ParsedName i -> IO (Term IRName TypeVar RawBuiltin i, Supply)
runDesugarTerm e = do
  ref <- newIORef 0
  let dstate = DesugarState mempty mempty mempty ref
  lastSupply <- readIORef ref
  (,lastSupply) <$> runReaderT (desugarTerm e) dstate
