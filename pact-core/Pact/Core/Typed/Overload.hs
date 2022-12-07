{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}


module Pact.Core.Typed.Overload
 ( runOverloadTerm
 , runOverloadModule
 , runOverloadTopLevel
 , runOverloadReplTopLevel
 , runOverloadProgram
 , runOverloadReplProgram
 ) where

import Control.Lens
import Control.Monad.Except
import Data.Text(Text)
import Data.List.NonEmpty(NonEmpty(..))

import qualified Data.Text as T

import Pact.Core.Type
import Pact.Core.Names
import Pact.Core.Builtin
import Pact.Core.Typed.Term
import Pact.Core.Errors

newtype OverloadM info a =
  OverloadM (Either (PactError info) a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadError (PactError info))
  via (Either (PactError info))

throwOverloadError :: String -> i -> OverloadM i a
throwOverloadError e = throwError . PEOverloadError (OverloadError (T.pack e))

class SolveOverload raw resolved | raw -> resolved where
  solveOverload
    :: info
    -> raw
    -> [Type tyname]
    -> [Pred tyname]
    -> OverloadM info (Term Name tyname resolved info)
  liftRaw :: RawBuiltin -> raw

resolveTerm
  :: forall tyname raw reso info.
    (SolveOverload raw reso)
  => OverloadedTerm tyname raw info
  -> OverloadM info (Term Name tyname reso info)
resolveTerm = \case
  Var n i -> pure (Var n i)
  Lam nts e i ->
    Lam nts <$> resolveTerm e <*> pure i
  Let n e1 e2 i ->
    Let n <$> resolveTerm e1 <*> resolveTerm e2 <*> pure i
  App f args i ->
    App <$> resolveTerm f <*> traverse resolveTerm args <*> pure i
  TyApp l rs i ->
    TyApp <$> resolveTerm l <*> pure rs <*> pure i
  Sequence e1 e2 i ->
    Sequence <$> resolveTerm e1 <*> resolveTerm e2 <*> pure i
  ListLit tn ts i ->
    ListLit tn <$> traverse resolveTerm ts <*> pure i
  Constant lit i ->
    pure (Constant lit i)
  Builtin (bs, tys, preds) i ->
    solveOverload i bs tys preds
  Try e1 e2 i ->
    Try <$> resolveTerm e1 <*> resolveTerm e2 <*> pure i
  TyAbs ns term i ->
    TyAbs ns <$> resolveTerm term <*> pure i
  Error t e i ->
    pure (Error t e i)

withTyApps
  :: Term name tyname builtin info
  -> [Type tyname]
  -> Term name tyname builtin info
withTyApps t = \case
  x:xs -> TyApp t (x :| xs) (view termInfo t)
  [] -> t

instance SolveOverload RawBuiltin CoreBuiltin where
  solveOverload = solveCoreOverload
  liftRaw = id

instance (SolveOverload raw resolved) => SolveOverload (ReplBuiltin raw) (ReplBuiltin resolved) where
  solveOverload i b tys preds = case b of
    RBuiltinWrap raw -> over termBuiltin RBuiltinWrap <$> solveOverload i raw tys preds
    RExpect ->
      case preds of
        [Pred Eq t1, Pred Show t2] -> do
          pEq <- solveOverload i (liftRaw RawEq :: ReplBuiltin raw) tys [Pred Eq t1]
          pShow <- solveOverload i (liftRaw RawShow :: ReplBuiltin raw) tys [Pred Show t2]
          let bApp = withTyApps (Builtin RExpect i) tys
          pure (App bApp (pEq :| [pShow]) i)
        _ -> throwOverloadError "Expect" i
    RExpectFailure -> pure $ withTyApps (Builtin RExpectFailure i) tys
    RExpectThat -> pure $ withTyApps (Builtin RExpectThat i) tys
    RPrint -> case preds of
      [Pred Show t1] -> do
        eqT <- solveOverload i (liftRaw RawShow :: ReplBuiltin raw) tys [Pred Show t1]
        let bApp = withTyApps (Builtin RPrint i) tys
        pure (App bApp (pure eqT) i)
      _ -> throwOverloadError "Print" i
  liftRaw r = RBuiltinWrap (liftRaw r)


listEqualityInstance
  :: info
  -> RawBuiltin
  -> CoreBuiltin
  -> Pred tyname
  -> OverloadM info (Term Name tyname CoreBuiltin info)
listEqualityInstance i raw inst (Pred tc t) = do
  b <- solveCoreOverload i raw [t] [Pred tc t]
  let tyApp = TyApp (Builtin inst i) (t :| []) i
  pure $ App tyApp (b :| []) i

specializeAdd
  :: info
  -> Pred tyname
  -> OverloadM info (CoreEvalTerm tyname info)
specializeAdd i (Pred _ t) = case t of
  TyInt -> pure (Builtin AddInt i)
  TyDecimal -> pure (Builtin AddDec i)
  TyString -> pure (Builtin AddStr i)
  TyList _ -> pure (Builtin AddList i)
  _ -> throwOverloadError "unable to resolve overload for Add Operation" i

specializeNumOp
  :: info
  -> NumResolution
  -> Pred tyname
  -> OverloadM info (CoreEvalTerm tyname info)
specializeNumOp i reso (Pred _ t) = case t of
  TyInt -> pure (Builtin (_nrIntInstance reso) i)
  TyDecimal -> pure (Builtin (_nrDecInstance reso) i)
  _ -> throwOverloadError "unable to resolve overload for Num Operation" i

specializeEq
  :: info
  -> RawBuiltin
  -> EqResolution
  -> Pred tyname
  -> OverloadM info (CoreEvalTerm tyname info)
specializeEq i raw reso (Pred _ t) = case t of
  TyInt -> pure (Builtin (_erIntInstance reso) i)
  TyDecimal -> pure (Builtin (_erDecInstance reso) i)
  TyString -> pure (Builtin (_erStrInstance reso) i)
  TyUnit -> pure (Builtin (_erUnitInstance reso) i)
  TyBool -> pure (Builtin (_erBoolInstance reso) i)
  TyList t' ->
    listEqualityInstance i raw (_erListInstance reso) (Pred Eq t')
  _ -> throwOverloadError "unable to resolve overload for Eq Operation" i

specializeOrd
  :: info
  -> RawBuiltin
  -> OrdResolution
  -> Pred tyname
  -> OverloadM info (CoreEvalTerm tyname info)
specializeOrd i raw reso (Pred _ t) = case t of
  TyInt -> pure (Builtin (_orIntInstance reso) i)
  TyDecimal -> pure (Builtin (_orDecInstance reso) i)
  TyString -> pure (Builtin (_orStrInstance reso) i)
  TyUnit -> pure (Builtin (_orUnitInstance reso) i)
  TyList t' ->
    listEqualityInstance i raw (_orListInstance reso) (Pred Ord t')
  _ -> throwOverloadError "unable to resolve overload for Ord Operation" i

specializeFracOp
  :: info
  -> FracResolution
  -> Pred tyname
  -> OverloadM info (CoreEvalTerm tyname info)
specializeFracOp i reso (Pred _ t) = case t of
  TyInt -> pure (Builtin (_frIntInstance reso) i)
  TyDecimal -> pure (Builtin (_frDecInstance reso) i)
  _ -> throwOverloadError "unable to resolve overload for Fractional Operation" i

specializeShow
  :: i
  -> Pred tyname
  -> OverloadM i (Term Name tyname CoreBuiltin i)
specializeShow i (Pred _ t) = case t of
  TyInt -> pure (Builtin ShowInt i)
  TyDecimal -> pure (Builtin ShowDec i)
  TyString -> pure (Builtin ShowStr i)
  TyUnit -> pure (Builtin ShowUnit i)
  TyBool -> pure (Builtin ShowBool i)
  TyList t' -> do
    b <- solveCoreOverload i RawShow [t'] [Pred Show t']
    let tyApp = TyApp (Builtin ShowList i) (t' :| []) i
    pure $ App tyApp (b :| []) i
  _ -> throwOverloadError "Show" i

specializeListLikeOp
  :: info
  -> ListLikeResolution
  -> Pred tyname
  -> OverloadM info (CoreEvalTerm tyname info)
specializeListLikeOp i reso (Pred _ t) = case t of
  TyString -> pure (Builtin (_llrStrInstance reso) i)
  TyList _ -> pure (Builtin (_llrListInstance reso) i)
  _ -> throwOverloadError "unable to resolve overload for ListLike Operation" i

  -- We specialize here on the common case
  -- and solve overloaded variables.
  -- see: [Typeclasses and Instances] in
  -- Builtin.hs
  -- NOTE: We rely on the invariant that
  -- the typechecker produces the right dictionaries passed
  -- to the builtins
  -- Todo: refactor to get an exhaustivity check
  -- on the raw builtins
solveCoreOverload
  :: info
  -> RawBuiltin
  -> [Type tyname]
  -> [Pred tyname]
  -> OverloadM info (CoreEvalTerm tyname info)
solveCoreOverload i b tys preds = case b of
  RawAdd ->
    singlePred preds i (specializeAdd i) "Add"
  RawSub ->
    singlePred preds i (specializeNumOp i subResolve) "Subtract"
  RawMultiply ->
    singlePred preds i (specializeNumOp i mulResolve) "Multiply"
  RawDivide ->
    singlePred preds i (specializeNumOp i divResolve) "Divide"
  RawNegate ->
    singlePred preds i (specializeNumOp i negateResolve) "Negate"
  RawAbs ->
    singlePred preds i (specializeNumOp i absResolve) "Abs"
  RawAnd ->
    pure (Builtin AndBool i)
  RawOr ->
    pure (Builtin OrBool i)
  RawNot ->
    pure (Builtin NotBool i)
  RawEq ->
    singlePred preds i (specializeEq i RawEq eqResolve) "Eq"
  RawNeq ->
    singlePred preds i (specializeEq i RawNeq neqResolve) "Neq"
  RawGT ->
    singlePred preds i (specializeOrd i RawGT gtResolve) "GT"
  RawGEQ ->
    singlePred preds i (specializeOrd i RawGEQ geqResolve) "GEQ"
  RawLT ->
    singlePred preds i (specializeOrd i RawLT ltResolve) "LT"
  RawLEQ ->
    singlePred preds i (specializeOrd i RawLEQ leqResolve) "LEQ"
  RawBitwiseAnd ->
    pure (Builtin BitAndInt i)
  RawBitwiseOr ->
    pure (Builtin BitOrInt i)
  RawBitwiseXor ->
    pure (Builtin BitXorInt i)
  RawBitwiseFlip ->
    pure (Builtin BitComplementInt i)
  RawBitShift ->
    pure (Builtin BitShiftInt i)
  RawRound ->
    pure (Builtin RoundDec i)
  RawCeiling ->
    pure (Builtin CeilingDec i)
  RawFloor ->
    pure (Builtin FloorDec i)
  RawExp ->
    singlePred preds i (specializeFracOp i expResolve) "Exp"
  RawLn ->
    singlePred preds i (specializeFracOp i lnResolve) "Ln"
  RawSqrt ->
    singlePred preds i (specializeFracOp i sqrtResolve) "Sqrt"
  RawLogBase ->
    singlePred preds i (specializeFracOp i logBaseResolve) "Log"
  RawLength ->
    singlePred preds i (specializeListLikeOp i lengthResolve) "Length"
  RawTake ->
    singlePred preds i (specializeListLikeOp i takeResolve) "Take"
  RawDrop ->
    singlePred preds i (specializeListLikeOp i dropResolve) "Drop"
  RawConcat ->
    singlePred preds i (specializeListLikeOp i concatResolve) "Concat"
  RawReverse ->
    singlePred preds i (specializeListLikeOp i reverseResolve) "Reverse"
  RawMod ->
    pure (Builtin ModInt i)
  RawMap -> case tys of
    [t1, t2] ->
      let bt = Builtin MapList i
      in pure (TyApp bt (t1:|[t2]) i)
    _ -> throwOverloadError "Invalid map type variables" i
  RawFilter -> case tys of
    [t1] -> do
      let bt = Builtin FilterList i
      pure (TyApp bt (t1:|[]) i)
    _ -> throwOverloadError "Filter" i
  RawZip -> case tys of
    [t1, t2, t3] -> do
      let bt = Builtin ZipList i
      pure (TyApp bt (t1:|[t2, t3]) i)
    _ -> throwOverloadError "Zip" i
  RawIf -> case tys of
    [t1] -> do
      let bt = Builtin IfElse i
      pure (TyApp bt (t1:|[]) i)
    _ -> throwOverloadError "If" i
  RawIntToStr -> error "Todo: implement"
  RawStrToInt -> error "Todo: implement"
  RawFold -> case tys of
    [l, r] ->
      let bt = Builtin FoldList i
      in pure (TyApp bt (l :| [r]) i)
    _ -> throwOverloadError "Fold" i
  RawDistinct -> error "Distinct"
  RawEnforce ->
    pure (Builtin Enforce i)
  RawEnforceOne ->
    pure (Builtin EnforceOne i)
  RawEnumerate ->
    pure (Builtin Enumerate i)
  RawEnumerateStepN ->
    pure (Builtin EnumerateStepN i)
  RawShow ->
    singlePred preds i (specializeShow i) "Show"
  RawReadInteger ->
    pure (Builtin ReadInteger i)
  RawReadDecimal ->
    pure (Builtin ReadDecimal i)
  RawReadString ->
    pure (Builtin ReadString i)
  RawListAccess ->
    pure (Builtin ListAccess i)
  RawB64Encode ->
    pure (Builtin B64Encode i)
  RawB64Decode ->
    pure (Builtin B64Decode i)
    -- Addition
    -- Note, we can also sanity check this here.
    -- (+) Add instances for base types + dynamic access
    -- (RawAdd, [_], [p]) ->
    --   specializeAdd i p

    -- -- (-) Num instances
    -- (RawSub, [_], [p]) ->
    --   specializeNumOp i subResolve p

    -- -- (*) Instances + Dynamic access
    -- (RawMultiply, [_], [p]) ->
    --   specializeNumOp i mulResolve p

    -- -- (/) instances + dynamic access
    -- (RawDivide, [_], [p]) ->
    --   specializeNumOp i divResolve p
    -- -- (negate) instances + dynamic access
    -- (RawNegate, [_], [p]) ->
    --   specializeNumOp i negateResolve p

    -- (RawAbs, [_], [p]) ->
    --   specializeNumOp i absResolve p
    -- -- bool ops
    -- (RawAnd, [] , []) ->
    --   pure (Builtin AndBool i)
    -- (RawOr, [], []) ->
    --   pure (Builtin OrBool i)
    -- (RawNot, [], []) ->
    --   pure (Builtin NotBool i)
    -- -- (==) instance + dyn access
    -- -- TODO: TIME
    -- (RawEq, [_], [p]) ->
    --   specializeEq i RawEq eqResolve p
    -- -- (/=) instance + dyn access
    -- (RawNeq, [_], [p]) ->
    --   specializeEq i RawNeq neqResolve p
    -- -- Ord : GT (>) instances
    -- -- todo: time
    -- (RawGT, [_], [p]) ->
    --   specializeOrd i RawGT gtResolve p
    -- -- Ord : GEQ
    -- (RawGEQ, [_], [p]) ->
    --   specializeOrd i RawGEQ geqResolve p
    -- -- Ord: LT
    -- (RawLT, [_], [p]) ->
    --   specializeOrd i RawLT ltResolve p
    -- -- Ord : LEQ
    -- (RawLEQ, [_], [p]) ->
    --   specializeOrd i RawLEQ leqResolve p

    -- (RawBitwiseAnd, _, _) ->
    --   pure (Builtin BitAndInt i)

    -- (RawBitwiseOr, _, _) ->
    --   pure (Builtin BitOrInt i)

    -- (RawBitwiseXor, _, _) ->
    --   pure (Builtin BitXorInt i)

    -- (RawBitwiseFlip, _, _) ->
    --   pure (Builtin BitComplementInt i)

    -- (RawBitShift, _,  _) ->
    --   pure (Builtin BitShiftInt i)
    -- (RawRound, [], []) ->
    --   pure (Builtin RoundDec i)
    -- (RawCeiling, [], []) ->
    --   pure (Builtin CeilingDec i)
    -- (RawFloor, [], []) ->
    --   pure (Builtin FloorDec i)
    -- -- Fractional instnaces
    -- (RawExp, [_], [p]) ->
    --   specializeFracOp i expResolve p
    -- (RawLn, [_], [p]) ->
    --   specializeFracOp i lnResolve p
    -- (RawSqrt, [_], [p]) ->
    --   specializeFracOp i sqrtResolve p
    -- (RawLogBase, [_], [p]) ->
    --   specializeFracOp i logBaseResolve p
    -- -- ListLike instances
    -- (RawLength, [_], [p]) ->
    --   specializeListLikeOp i lengthResolve p
    -- (RawTake, [_], [p]) ->
    --   specializeListLikeOp i takeResolve p
    -- (RawDrop, [_], [p]) ->
    --   specializeListLikeOp i dropResolve p
    -- (RawConcat, [_], [p]) ->
    --   specializeListLikeOp i concatResolve p
    -- (RawReverse, [_], [p]) ->
    --   specializeListLikeOp i reverseResolve p
    -- Todo: overload logbase
    -- (RawMod, [], []) ->
    --   pure (Builtin ModInt i)
    -- -- General
    -- (RawMap, [t1, t2], []) -> do
    --   let b = Builtin MapList i
    --   pure (TyApp b (t1:|[t2]) i)
    -- (RawFilter, [t1], []) ->  do
    --   let b = Builtin FilterList i
    --   pure (TyApp b (t1:|[]) i)
    -- (RawZip, [t1, t2, t3], []) ->  do
    --   let b = Builtin ZipList i
    --   pure (TyApp b (t1:|[t2, t3]) i)
    -- (RawIf, [t1], []) -> do
    --   let b = Builtin IfElse i
    --   pure (TyApp b (t1:|[]) i)
    -- (RawShow, [TyInt], _) ->
    --   pure (Builtin ShowInt i)
    -- (RawShow, [TyDecimal], _) ->
    --   pure (Builtin ShowDec i)
    -- (RawShow, [TyString], _) ->
    --   pure (Builtin ShowStr i)
    -- (RawShow, [TyUnit], _) ->
    --   pure (Builtin ShowUnit i)
    -- (RawShow, [TyBool], _) ->
    --   pure (Builtin ShowBool i)
    -- (RawShow, [TyList t], [_]) -> do
    --   b <- solveOverload i (RawShow, [t], [Pred Show t])
    --   let a1Var = Name "" (NBound 0)
    --       a1 = (a1Var, t)
    --       app = App (Builtin ShowList i) (b :| [Var a1Var i]) i
    --   pure (Lam (a1 :| []) app i)
    -- (RawEnumerate, [], []) ->
    --   pure (Builtin Enumerate i)
    -- (RawEnumerateStepN, [], []) ->
    --   pure (Builtin EnumerateStepN i)
    -- (RawFold, [l, r], []) ->
    --   let b = Builtin FoldList i
    --   in pure (TyApp b (l :| [r]) i)
    -- (RawReadInteger, _, _) ->
    --   pure (Builtin ReadInteger i)
    -- (RawReadDecimal, _, _) ->
    --   pure (Builtin ReadDecimal i)
    -- (RawReadString, _, _) ->
    --   pure (Builtin ReadString i)
    -- (RawReadKeyset, _, _) ->
    --   pure (Builtin ReadKeyset i)
    -- (RawEnforceGuard, _, _) ->
    --   pure (Builtin EnforceGuard  i)
    -- (RawKeysetRefGuard, _, _) ->
    --   pure (Builtin KeysetRefGuard i)
    -- (RawCreateUserGuard, _, _) ->
    --   pure (Builtin CreateUserGuard i)
    -- (RawListAccess, _, _) ->
    --   pure (Builtin ListAccess i)
    -- (RawB64Encode, _, _) ->
    --   pure (Builtin B64Encode i)
    -- (RawB64Decode, _, _) ->
    --   pure (Builtin B64Decode i)
    -- _ -> throwOverloadError "could not resolve overload" i

singlePred :: [t] -> i -> (t -> OverloadM i a) -> String -> OverloadM i a
singlePred preds i f msg = case preds of
  [p] -> f p
  _ -> throwOverloadError msg i

resolveDefun
  :: SolveOverload raw reso
  => OverloadedDefun tyname raw info
  -> OverloadM info (Defun Name tyname reso info)
resolveDefun (Defun dname ty term info) = do
  term' <- resolveTerm term
  pure (Defun dname ty term' info)

resolveDefConst
  :: SolveOverload raw reso
  => OverloadedDefConst tyname raw info
  -> OverloadM info (DefConst Name tyname reso info)
resolveDefConst (DefConst dname ty term info) = do
  term' <- resolveTerm term
  pure (DefConst dname ty term' info)

-- resolveDefCap
--   :: OverloadedDefCap RawBuiltin info
--   -> OverloadM (DefCap Name Void CoreBuiltin info)
-- resolveDefCap (DefCap dname dargs term captype termtype info) = do
--   term' <- resolveTerm term
--   let captype' = unsafeToTLName <$> captype
--   pure (DefCap dname dargs term' captype' termtype info)

resolveDef
  :: SolveOverload raw reso
  => OverloadedDef tyname raw info
  -> OverloadM info (Def Name tyname reso info)
resolveDef = \case
  Dfun d -> Dfun <$> resolveDefun d
  DConst d -> DConst <$> resolveDefConst d
  -- DCap d -> DCap <$> resolveDefCap d

resolveModule
  :: SolveOverload raw reso
  => OverloadedModule tyname raw info
  -> OverloadM info (Module Name tyname reso info)
resolveModule m = do
  defs' <- traverse resolveDef (_mDefs m)
  -- let gov' = unsafeToTLName <$> _mGovernance m
  pure m{_mDefs=defs'}

resolveTopLevel
  :: SolveOverload raw reso
  => OverloadedTopLevel tyname raw info
  -> OverloadM info (TopLevel Name tyname reso info)
resolveTopLevel = \case
  TLModule m -> TLModule <$> resolveModule m
  TLTerm t -> TLTerm <$> resolveTerm t
  _ -> error "unimplemented"

resolveProgram
  :: SolveOverload raw reso
  => [OverloadedTopLevel tyname raw info]
  -> OverloadM info [TopLevel Name tyname reso info]
resolveProgram  = traverse resolveTopLevel

resolveReplTopLevel
  :: SolveOverload raw reso
  => OverloadedReplTopLevel tyname raw info
  -> OverloadM info (ReplTopLevel Name tyname reso info)
resolveReplTopLevel = \case
  RTLModule m -> RTLModule <$> resolveModule m
  RTLTerm t -> RTLTerm <$> resolveTerm t
  RTLDefun d -> RTLDefun <$> resolveDefun d
  RTLDefConst d -> RTLDefConst <$> resolveDefConst d

resolveReplProgram
  :: SolveOverload raw reso
  => [OverloadedReplTopLevel tyname raw info]
  -> OverloadM info [ReplTopLevel Name tyname reso info]
resolveReplProgram = traverse resolveReplTopLevel

-------------------------------------------------
-- Auxiliary data types to group
-- builtin resolution
-- todo: time instances
-------------------------------------------------
data NumResolution
  = NumResolution
  { _nrRawName :: Text
  , _nrIntInstance :: CoreBuiltin
  , _nrDecInstance :: CoreBuiltin
  } deriving Show

data EqResolution
  = EqResolution
  { _erRawName :: Text
  , _erIntInstance :: CoreBuiltin
  , _erDecInstance :: CoreBuiltin
  , _erStrInstance :: CoreBuiltin
  , _erUnitInstance :: CoreBuiltin
  , _erBoolInstance :: CoreBuiltin
  , _erListInstance :: CoreBuiltin
  } deriving Show

data OrdResolution
  = OrdResolution
  { _orRawName :: Text
  , _orIntInstance :: CoreBuiltin
  , _orDecInstance :: CoreBuiltin
  , _orStrInstance :: CoreBuiltin
  , _orUnitInstance :: CoreBuiltin
  , _orListInstance :: CoreBuiltin }
  deriving Show

data FracResolution
  = FracResolution
  { _frRawName :: Text
  , _frIntInstance :: CoreBuiltin
  , _frDecInstance :: CoreBuiltin
  } deriving Show


data ListLikeResolution
  = ListLikeResolution
  { _llrRawName :: Text
  , _llrStrInstance :: CoreBuiltin
  , _llrListInstance :: CoreBuiltin
  } deriving Show

-- Num instances
subResolve :: NumResolution
subResolve =
  NumResolution
  { _nrRawName = "-"
  , _nrIntInstance = SubInt
  , _nrDecInstance = SubDec }

mulResolve :: NumResolution
mulResolve =
  NumResolution
  { _nrRawName = "*"
  , _nrIntInstance = MulInt
  , _nrDecInstance = MulDec }

divResolve :: NumResolution
divResolve =
  NumResolution
  { _nrRawName = "/"
  , _nrIntInstance = DivInt
  , _nrDecInstance = DivDec }

absResolve :: NumResolution
absResolve =
  NumResolution
  { _nrRawName = "abs"
  , _nrIntInstance = AbsInt
  , _nrDecInstance = AbsDec }

negateResolve :: NumResolution
negateResolve =
  NumResolution
  { _nrRawName = "negate"
  , _nrIntInstance = NegateInt
  , _nrDecInstance = NegateDec }

-- Eq instances
eqResolve :: EqResolution
eqResolve =
  EqResolution
  { _erRawName = "=="
  , _erIntInstance = EqInt
  , _erDecInstance = EqDec
  , _erStrInstance = EqStr
  , _erUnitInstance = EqUnit
  , _erBoolInstance = EqBool
  , _erListInstance = EqList
  }

neqResolve :: EqResolution
neqResolve =
  EqResolution
  { _erRawName = "/="
  , _erIntInstance = NeqInt
  , _erDecInstance = NeqDec
  , _erStrInstance = NeqStr
  , _erUnitInstance = NeqUnit
  , _erBoolInstance = NeqBool
  , _erListInstance = NeqList
  }

-- Ord instances
gtResolve :: OrdResolution
gtResolve =
  OrdResolution
  { _orRawName = ">"
  , _orIntInstance = GTInt
  , _orDecInstance = GTDec
  , _orStrInstance = GTStr
  , _orUnitInstance = NeqUnit
  , _orListInstance = GTList
  }

geqResolve :: OrdResolution
geqResolve =
  OrdResolution
  { _orRawName = ">="
  , _orIntInstance = GEQInt
  , _orDecInstance = GEQDec
  , _orStrInstance = GEQStr
  , _orUnitInstance = EqUnit
  , _orListInstance = GEQList
  }

ltResolve :: OrdResolution
ltResolve =
  OrdResolution
  { _orRawName = "<"
  , _orIntInstance = LTInt
  , _orDecInstance = LTDec
  , _orStrInstance = LTStr
  , _orUnitInstance = NeqUnit
  , _orListInstance = LTList
  }

leqResolve :: OrdResolution
leqResolve =
  OrdResolution
  { _orRawName = "<="
  , _orIntInstance = LEQInt
  , _orDecInstance = LEQDec
  , _orStrInstance = LEQStr
  , _orUnitInstance = EqUnit
  , _orListInstance = LEQList
  }

-- Fractional instances
lnResolve :: FracResolution
lnResolve =
  FracResolution
  { _frRawName = "ln"
  , _frIntInstance = LnInt
  , _frDecInstance = LnDec
  }

expResolve :: FracResolution
expResolve =
  FracResolution
  { _frRawName = "exp"
  , _frIntInstance = ExpInt
  , _frDecInstance = ExpDec
  }

sqrtResolve :: FracResolution
sqrtResolve =
  FracResolution
  { _frRawName = "sqrt"
  , _frIntInstance = SqrtInt
  , _frDecInstance = SqrtDec
  }

logBaseResolve :: FracResolution
logBaseResolve =
  FracResolution
  { _frRawName = "logBase"
  , _frIntInstance = LogBaseInt
  , _frDecInstance = LogBaseDec
  }

-- ListLike instances
takeResolve :: ListLikeResolution
takeResolve =
  ListLikeResolution
  { _llrRawName = "take"
  , _llrStrInstance = TakeStr
  , _llrListInstance = TakeList
  }

dropResolve :: ListLikeResolution
dropResolve =
  ListLikeResolution
  { _llrRawName = "drop"
  , _llrStrInstance = DropStr
  , _llrListInstance = DropList
  }

concatResolve :: ListLikeResolution
concatResolve =
  ListLikeResolution
  { _llrRawName = "concat"
  , _llrStrInstance = ConcatStr
  , _llrListInstance = ConcatList
  }

reverseResolve :: ListLikeResolution
reverseResolve =
  ListLikeResolution
  { _llrRawName = "reverse"
  , _llrStrInstance = ReverseStr
  , _llrListInstance = ReverseList
  }


lengthResolve :: ListLikeResolution
lengthResolve =
  ListLikeResolution
  { _llrRawName = "length"
  , _llrStrInstance = LengthStr
  , _llrListInstance = LengthList
  }

-- varOverloaded :: i -> Pred Void -> OverloadM (CoreEvalTerm i)
-- varOverloaded i (Pred tc ty) = case tc of
--   Add -> addOverloaded i ty
--   Eq -> eqOverloaded i ty
--   Num -> numOverloaded i ty
--   Ord -> ordOverloaded i ty
--   Show -> showOverloaded i ty
--   Fractional -> fractionalOverloaded i ty
--   ListLike -> listLikeOverloaded i ty

-- lookupDictVar :: i -> Pred Void -> OverloadM (CoreEvalTerm i)
-- lookupDictVar i p = do
--   depth <- view roVarDepth
--   ols <- view roOverloads
--   case lookup p ols of
--     Just d -> do
--       let n = Name "#dictInst" $ NBound (depth - d - 1)
--       pure (Var n i)
--     Nothing -> throwError "invariant failure: unbound dictionary variable"

-- addOverloaded
--   :: info
--   -> Type Void
--   -> OverloadM (CoreEvalTerm info)
-- addOverloaded i = \case
--   TyInt -> pure (addInt i)
--   TyDecimal -> pure (addDec i)
--   TyString -> pure (addStr i)
--   TyList _ -> pure (addList i)
--   t@TyVar{} -> bindDictVar i Add t
--   _ -> throwError "invariant failure: no instance for add"


-- eqOverloaded
--   :: i
--   -> Type Void
--   -> OverloadM (CoreEvalTerm i)
-- eqOverloaded i = \case
--   TyPrim p -> case p of
--     PrimInt -> pure (eqInt i)
--     PrimDecimal -> pure (eqDecimal i)
--     PrimString -> pure (eqString i)
--     PrimBool -> pure (eqBool i)
--     PrimUnit -> pure (eqUnit i)
--     _ -> throwError "invariant failure: no instance for eq"
--   t@TyVar{} -> bindDictVar i Eq t
--   _ -> throwError "invariant failure: no instance for eq"

-- ordOverloaded
--   :: info
--   -> Type Void
--   -> OverloadM (CoreEvalTerm info)
-- ordOverloaded i = \case
--   TyPrim p -> case p of
--     PrimInt -> pure (ordInt i)
--     PrimDecimal -> pure (ordDec i)
--     PrimString -> pure (ordStr i)
--     PrimUnit -> pure (ordUnit i)
--     _ -> throwError "invariant failure, no such ord instance"
--   t@TyVar{} -> bindDictVar i Ord t
--   _ -> throwError "invariant failure, no such ord instance"

-- bindDictVar
--   :: info
--   -> BuiltinTC
--   -> Type Void
--   -> OverloadM (CoreEvalTerm info)
-- bindDictVar i tc t = do
--   depth <- view roVarDepth
--   ols <- view roOverloads
--   case lookup (Pred tc t) ols of
--     Just d -> do
--       let n = Name "#dictInst" $ NBound (depth - d - 1)
--       pure (Var n i)
--     Nothing -> throwError "invariant failure: unbound dictionary variable"

-- showOverloaded
--   :: info
--   -> Type Void
--   -> OverloadM (CoreEvalTerm info)
-- showOverloaded i = \case
--   TyPrim p -> case p of
--     PrimInt -> pure (showInt i)
--     PrimDecimal -> pure (showDec i)
--     PrimString -> pure (showStr i)
--     PrimBool -> pure (showBool i)
--     PrimUnit -> pure (showUnit i)
--     _ -> throwError "invariant failure: no instance for show"
--   t@TyVar{} -> bindDictVar i Show t
--   _ -> throwError "invariant failure: no instance for show"

-- numOverloaded
--   :: info
--   -> Type Void
--   -> OverloadM (CoreEvalTerm info)
-- numOverloaded i = \case
--   TyInt -> pure (numInt i)
--   TyDecimal -> pure (numDec i)
--   t@TyVar{} -> bindDictVar i Num t
--   _ -> throwError "invariant failure: no instance for num"

-- fractionalOverloaded
--   :: info
--   -> Type Void
--   -> OverloadM (CoreEvalTerm info)
-- fractionalOverloaded i = \case
--   TyInt -> pure (fracInt i)
--   TyDecimal -> pure (fracDec i)
--   t@TyVar{} -> bindDictVar i Fractional t
--   _ -> throwError "invariant failure: no instance for frac"

-- listLikeOverloaded
--   :: info
--   -> Type Void
--   -> OverloadM (CoreEvalTerm info)
-- listLikeOverloaded i = \case
--   TyString -> pure (listLikeStr i)
--   TyList _ -> pure (listLikeList i)
--   t@TyVar{} -> bindDictVar i ListLike t
--   _ -> throwError "invariant failure: no instance for listLike"

-- eqDict :: b -> b -> i -> Term n tn b i
-- eqDict eq neq i =
--   ObjectLit o i
--   where
--   o = Map.fromList
--     [(Field "==", Builtin eq i)
--     ,(Field "/=", Builtin neq i)]

-- showDict :: b -> i -> Term n tn b i
-- showDict s i =
--   ObjectLit o i
--   where
--   o = Map.singleton (Field "show") (Builtin s i)

-- ordDict :: b -> b -> b -> b -> i -> Term n tn b i
-- ordDict gt geq lt leq i =
--   ObjectLit o i
--   where
--   o = Map.fromList
--     [ (Field ">", Builtin gt i)
--     , (Field ">=", Builtin geq i)
--     , (Field "<", Builtin lt i)
--     , (Field "<=", Builtin leq i)]

-- addDict :: builtin -> info -> Term name tyname builtin info
-- addDict add i =
--   ObjectLit o i
--   where
--   o = Map.singleton (Field "+") (Builtin add i)

-- numDict :: builtin
--   -> builtin
--   -> builtin
--   -> builtin
--   -> builtin
--   -> info
--   -> Term name tyname builtin info
-- numDict sub mul divv neg abs' i =
--   ObjectLit o i
--   where
--   o = Map.fromList
--     [ (Field "-", Builtin sub i)
--     , (Field "*", Builtin mul i)
--     , (Field "/", Builtin divv i)
--     , (Field "negate", Builtin neg i)
--     , (Field "abs", Builtin abs' i)]

-- fracDict :: builtin
--   -> builtin
--   -> builtin
--   -> builtin
--   -> info
--   -> Term name tyname builtin info
-- fracDict ln_ exp_ sqrt_ logBase_ i =
--   ObjectLit o i
--   where
--   o = Map.fromList
--     [ (Field "ln", Builtin ln_ i)
--     , (Field "exp", Builtin exp_ i)
--     , (Field "sqrt", Builtin sqrt_ i)
--     , (Field "log-base", Builtin logBase_ i)]

-- listLikeDict :: builtin
--   -> builtin
--   -> builtin
--   -> builtin
--   -> builtin
--   -> info
--   -> Term name tyname builtin info
-- listLikeDict take_ drop_ concat_ rev len i =
--   ObjectLit o i
--   where
--   o = Map.fromList
--     [ (Field "take", Builtin take_ i)
--     , (Field "drop", Builtin drop_ i)
--     , (Field "concat", Builtin concat_ i)
--     , (Field "reverse", Builtin rev i)
-- --     , (Field "length", Builtin len i)]

-- eqInt :: info -> CoreEvalTerm info
-- eqInt = eqDict EqInt NeqInt

-- eqDecimal :: info -> CoreEvalTerm info
-- eqDecimal = eqDict EqDec NeqDec

-- eqString :: info -> CoreEvalTerm info
-- eqString = eqDict EqStr NeqStr

-- eqUnit :: info -> CoreEvalTerm info
-- eqUnit = eqDict EqUnit NeqUnit

-- eqBool :: info -> CoreEvalTerm info
-- eqBool = eqDict EqBool NeqBool

-- numInt, numDec :: info -> CoreEvalTerm info
-- numInt = numDict SubInt MulInt DivInt NegateInt AbsInt
-- numDec = numDict AddDec MulDec DivDec NegateDec AbsDec

-- addInt, addDec, addStr, addList :: info -> CoreEvalTerm info
-- addInt = addDict AddInt
-- addDec = addDict AddDec
-- addStr = addDict AddStr
-- addList = addDict AddList

-- ordInt, ordDec, ordStr, ordUnit :: info -> CoreEvalTerm info
-- ordInt = ordDict GTInt GEQInt LTInt LEQInt
-- ordDec = ordDict GTDec GEQDec LTDec LEQDec
-- ordStr = ordDict GTStr GEQStr LTStr LEQStr
-- ordUnit = ordDict NeqUnit EqUnit NeqUnit EqUnit

-- showInt, showDec, showStr, showUnit, showBool :: info -> CoreEvalTerm info
-- showInt = showDict ShowInt
-- showDec = showDict ShowDec
-- showStr = showDict ShowStr
-- showUnit = showDict ShowUnit
-- showBool = showDict ShowBool

-- fracInt, fracDec :: info -> CoreEvalTerm info
-- fracInt = fracDict LnInt ExpInt SqrtInt LogBaseInt
-- fracDec = fracDict LnDec ExpDec SqrtDec LogBaseDec

-- listLikeStr, listLikeList :: info -> CoreEvalTerm info
-- listLikeStr = listLikeDict TakeStr DropStr ConcatStr ReverseStr LengthStr
-- listLikeList = listLikeDict TakeList DropList ConcatList ReverseList LengthList

-- Todo: Better errors during overloading
runOverload
  :: OverloadM info a
  -> Either (PactError info) a
runOverload (OverloadM e) = e

runOverloadTerm
  :: SolveOverload raw reso
  => OverloadedTerm tyname raw i
  -> Either (PactError i) (Term Name tyname reso i)
runOverloadTerm t = runOverload (resolveTerm t)

runOverloadModule
  :: SolveOverload raw reso
  => OverloadedModule tyname raw i
  -> Either (PactError i) (Module Name tyname reso i)
runOverloadModule m = runOverload (resolveModule m)

runOverloadTopLevel
  :: SolveOverload raw reso
  => OverloadedTopLevel tyname raw info
  -> Either (PactError info) (TopLevel Name tyname reso info)
runOverloadTopLevel tl = runOverload (resolveTopLevel tl)

runOverloadReplTopLevel
  :: SolveOverload raw reso
  => OverloadedReplTopLevel tyname raw info
  -> Either (PactError info) (ReplTopLevel Name tyname reso info)
runOverloadReplTopLevel tl = runOverload (resolveReplTopLevel tl)

runOverloadProgram
  :: SolveOverload raw reso
  => [OverloadedTopLevel tyname raw info]
  -> Either (PactError info) [TopLevel Name tyname reso info]
runOverloadProgram prog = runOverload (resolveProgram prog)

runOverloadReplProgram
  :: SolveOverload raw reso
  => [OverloadedReplTopLevel tyname raw info]
  -> Either (PactError info) [ReplTopLevel Name tyname reso info]
runOverloadReplProgram prog = runOverload (resolveReplProgram prog)
