{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
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

type DesugarT = ReaderT Supply IO

newUnique' :: DesugarT Unique
newUnique' = ask >>= liftIO . newUnique

desugarTerm :: PT.Expr ParsedName i -> Term ParsedName Text RawBuiltin i
desugarTerm = \case
  PT.Var n i ->
    Var n i
  PT.Block (h :| hs) _ ->
    unLetBlock h hs
  PT.Let name mt expr i ->
    Let name (desugarType <$> mt) (desugarTerm expr) (Constant LUnit i) i
  PT.Lam name nsts body i ->
    let (ns, ts) = NE.unzip nsts
    in Lam name ns (fmap desugarType <$> ts) (desugarTerm body) i
  PT.If cond e1 e2 i ->
    App (Builtin RawIf i) (desugarTerm cond :| [suspend e1, suspend e2]) i
  -- f() <=> (f ())
  PT.App e [] i ->
    App (desugarTerm e) (Constant LUnit i :| []) i
  PT.App e (h:hs) i ->
    -- Todo:: Core IR as multi-arg
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
desugarType :: PT.Type -> Type Text
desugarType = \case
  _ -> undefined

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



