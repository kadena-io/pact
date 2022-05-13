{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Pact.Core.IR.Desugar where


import Control.Monad.Reader

import Control.Lens hiding (List)
import Data.Text(Text)
import Data.List.NonEmpty(NonEmpty(..))
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
  } deriving Show

makeLenses ''RenamerState

toIRName :: Unique -> ParsedName -> IRName
toIRName u = \case
  BN (BareName bn) -> IRName bn IRLocallyBoundName u
  QN (QualifiedName bn qual) -> IRName bn (IRTopLevelName qual) u

newUnique' :: DesugarT Unique
newUnique' = view dsSupply >>= liftIO . newUnique

resolveUnique :: ParsedName -> DesugarT (Maybe (Unique, IRNameKind))
resolveUnique = \case
  BN (BareName bn) ->
    views dsBinds (Map.lookup bn)
  QN qn ->
    views dsModuleBinds $ Map.lookup qn

desugarTerm :: PT.Expr ParsedName i -> DesugarT (Term IRName Text RawBuiltin i)
desugarTerm = \case
  PT.Var n i ->
    Var n i
  PT.Block (h :| hs) _ ->
    unLetBlock h hs
  -- Names will always come out are barenames from the parser
  PT.Let name mt expr i -> do
    u <- newUnique'
    let name' = toIRName u name
    expr' <- desugarTerm expr
    mt' <- traverse desugarTypeA mt
    pure (Let name' mt' expr' (Constant LUnit i) i)
  PT.Lam name nsts body i -> do
    let (ns, ts) = NE.unzip nsts'
    ru <- resolveUnique
    in Lam name ns (fmap desugarType <$> ts) (desugarTerm body) i
  PT.If cond e1 e2 i ->
    App (Builtin RawIf i) (desugarTerm cond :| [suspend e1, suspend e2]) i
  PT.App e [] i ->
    App (desugarTerm e) (Constant LUnit i :| []) i
  PT.App e (h:hs) i ->
    App (desugarTerm e) (desugarTerm h :| fmap desugarTerm hs) i
  PT.BinaryOp bop e1 e2 i ->
    App (Builtin (desugarBinary bop) i) (desugarTerm e1 :| [desugarTerm e2]) i
  PT.UnaryOp uop e1 i ->
    App (Builtin (desugarUnary uop) i) (desugarTerm e1 :| []) i
  PT.List e1 i ->
    ListLit (V.fromList (desugarTerm <$> e1)) i
  PT.Constant l i ->
    Constant l i
  PT.Object objs i ->
    ObjectLit (desugarTerm <$> objs) i
  PT.Error text i ->
    Error text i
  where
  suspend t =
    Lam
      (BN (BareName "#ifBuiltin"))
      (BN (BareName "#unitArg") :| [])
      (Just (TyPrim PrimUnit) :| [])
      (desugarTerm t)
      (t ^. PT.termInfo)
  unLetBlock (PT.Let name mt expr i) (h:hs) = do
    Let name (desugarType <$> mt) (desugarTerm expr) (unLetBlock h hs) i
  unLetBlock other l = case l of
    h:hs ->
      let other' = desugarTerm other
      in
        case unLetBlock h hs of
          Block nel' i' ->
            Block (NE.cons other' nel') i'
          t -> Block (other' :| [t]) (other ^. PT.termInfo)
    [] -> desugarTerm other

desugarType :: PT.Type -> DesugarT (Type Text)
desugarType = \case
  _ -> undefined
  -- PT.TyVar v -> TyVar v
  -- PT.TyPrim p -> TyPrim p
  -- PT.TyFun l r ->
  --   TyFun (desugarType l) (desugarType r)
  -- PT.TyObject o mt ->
  --   TyRow (RowTy (desugarType <$> o) mt)
  -- PT.TyList t ->
  --   TyList (desugarType t)
  -- PT.TyCap -> TyCap

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

