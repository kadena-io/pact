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
import qualified Pact.Core.IR.ParseTree as PT

type DesugarT = ReaderT DesugarState IO

data DesugarState
  = DesugarState
  { _dsBinds :: Map Text NameKind
  , _dsModuleBinds :: Map QualifiedName NameKind
  , _dsTyBinds :: Map Text Unique
  , _dsSupply :: IORef Supply
  , _dsDepth :: DeBruijn
  }

makeLenses ''DesugarState

-- todo: this qualifier use could work for aliasing!
toIRName :: Unique -> IRNameKind -> ParsedName -> IRName
toIRName u irnk = \case
  BN (BareName bn) -> IRName bn irnk u
  QN (QualifiedName bn qual) -> IRName bn (IRTopLevelName qual) u

newUnique' :: DesugarT Unique
newUnique' = do
  sup <- view dsSupply
  u <- lift (readIORef sup)
  lift (modifyIORef' sup (+ 1))
  pure u

resolve :: ParsedName -> DesugarT (Maybe NameKind)
resolve = \case
  BN (BareName bn) ->
    views dsBinds (Map.lookup bn)
  QN qn ->
    views dsModuleBinds (Map.lookup qn)

desugarTerm :: PT.Expr ParsedName i -> DesugarT (Term Name TypeVar RawBuiltin i)
desugarTerm = \case
  PT.Var (BN n) i | isReservedNative (_bnName n) ->
    pure (Builtin (rawBuiltinMap Map.! _bnName n) i)
  PT.Var n i -> do
    depth <- view dsDepth
    resolve n >>= \case
      Just nk -> case nk of
        LocallyBoundName d -> pure $ Var (Name (rawParsedName n) (LocallyBoundName (depth - d - 1))) i
        _ -> pure $ Var (Name (rawParsedName n) nk) i
      Nothing -> error ("unbound variable " <> show n)
  PT.Block (h :| hs) _ ->
    unLetBlock h hs
  -- Names will always come out are barenames from the parser
  PT.Let name mt expr i -> do
    let name' = Name name (LocallyBoundName 0)
    expr' <- desugarTerm expr
    mt' <- traverse desugarType mt
    pure (Let name' mt' expr' (Constant LUnit i) i)
  PT.Lam name nsts body i -> do
    depth <- view dsDepth
    let (ns, ts) = NE.unzip nsts
        len = fromIntegral (NE.length nsts)
        newDepth = depth + len
        ixs = NE.fromList [depth .. newDepth - 1]
        ns' = NE.zipWith (\n ix -> Name n (LocallyBoundName (newDepth - ix - 1))) ns ixs
        m = Map.fromList $ NE.toList $ NE.zip ns (LocallyBoundName <$> ixs)
    name' <- resolveLamName name
    term' <- lamArgsInEnv m newDepth $ desugarTerm body
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
    let name = Name "#ifArg" (LocallyBoundName 0)
    e' <- locally dsDepth succ $ desugarTerm e
    pure (Lam name ((name, Nothing) :| []) e' i)
  isReservedNative n =
    elem n rawBuiltinNames
  resolveLamName n = resolve n >>= \case
    Just nk -> pure $ Name (rawParsedName n) nk
    Nothing -> pure $ Name (rawParsedName n) (LocallyBoundName 0)
  lamArgsInEnv m newDepth =
    local (over dsBinds (Map.union m) . set dsDepth newDepth)
  unLetBlock (PT.Let name mt expr i) (h:hs) = do
    depth <- view dsDepth
    let name' = Name name (LocallyBoundName 0)
    expr' <- desugarTerm expr
    mt' <- traverse desugarType mt
    let inEnv = over dsBinds (Map.insert name (LocallyBoundName depth)) . over dsDepth (+ 1)
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

runDesugarTerm :: PT.Expr ParsedName i -> IO (Term Name TypeVar RawBuiltin i, Supply)
runDesugarTerm e = do
  ref <- newIORef 0
  let depth = 0
      dstate = DesugarState mempty mempty mempty ref depth
  lastSupply <- readIORef ref
  (,lastSupply) <$> runReaderT (desugarTerm e) dstate
