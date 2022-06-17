{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}



module Pact.Core.Typed.Overload
 ( runOverloadTerm
 , runOverloadModule
 , runOverloadTopLevel
 , runOverloadReplTopLevel
 , runOverloadProgram
 , runOverloadReplProgram
 ) where

import Control.Lens hiding (ix, op)
import Control.Monad.Reader
import Data.Text(Text)
import Data.List.NonEmpty(NonEmpty(..))
import Data.IntMap.Strict(IntMap)
import Data.Foldable(foldl')
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IntMap

import Pact.Core.Type
import Pact.Core.Names
import Pact.Core.Builtin
import Pact.Core.Typed.Term

data ROState
  = ROState
  { _roBoundVars :: IntMap DeBruijn
  , _roVarDepth :: DeBruijn
  , _roOverloads :: [(Pred NamedDeBruijn, DeBruijn)]
  }
makeLenses ''ROState

type OverloadT = ReaderT ROState IO

resolveTerm
  :: OverloadedTerm RawBuiltin info
  -> OverloadT (CoreEvalTerm info)
resolveTerm = \case
  Var (OverloadedName n nk) i -> case nk of
    OBound u -> do
      depth <- view roVarDepth
      views roBoundVars (IntMap.lookup u) >>= \case
        Just d -> do
          let n' = Name n (NBound (depth - d - 1))
          pure (Var n' i)
        Nothing -> fail "invariant failure: unbound name"
    OTopLevel m h -> do
      let n' = Name n (NTopLevel m h)
      pure (Var n' i)
    OBuiltinDict p ->
      varOverloaded i p
  Lam nts e info -> do
    depth <- view roVarDepth
    let (ns, ts) = NE.unzip nts
        len = fromIntegral (NE.length nts)
        newDepth = depth + len
        ixs = NE.fromList [depth..newDepth-1]
        nsix = NE.zip ns ixs
        (m, de) = foldl' mkEnv (mempty, []) nsix
        ns' = toON <$> nsix
    e' <- local (inEnv m de newDepth) (resolveTerm e)
    pure (Lam (NE.zip ns' ts) e' info)
    where
    inEnv m de depth =
      over roBoundVars (IntMap.union m)
      . set roVarDepth depth
      . over roOverloads (de ++)
    toON (n, ix) = Name (_olName n) (NBound ix)
    mkEnv (m, de) (OverloadedName _ nk, ix)  = case nk of
      OBound u -> (IntMap.insert u ix m, de)
      OBuiltinDict p -> (m, (p, ix):de)
      _ -> (m, de)
  Let n e1 e2 i -> do
    u <- case _olNameKind n of
      OBound u -> pure u
      _ -> fail "invariant error: let bound as incorrect variable"
    depth <- view roVarDepth
    let n' = Name (_olName n) (NBound depth)
    e1' <- resolveTerm e1
    e2' <- local (inEnv u depth) $ resolveTerm e2
    pure (Let n' e1' e2' i)
    where
    inEnv u depth =
      over roVarDepth (+ 1) . over roBoundVars (IntMap.insert u depth)
  App f args i ->
    App <$> resolveTerm f <*> traverse resolveTerm args <*> pure i
  TyApp l rs i ->
    TyApp <$> resolveTerm l <*> pure rs <*> pure i
  TyAbs nel t i ->
    TyAbs nel <$> resolveTerm t <*> pure i
  Block nel i ->
    Block <$> traverse resolveTerm nel <*> pure i
  ObjectLit ms i ->
    ObjectLit <$> traverse resolveTerm ms <*> pure i
  ListLit tn ts i ->
    ListLit tn <$> traverse resolveTerm ts <*> pure i
  Constant lit i ->
    pure (Constant lit i)
  Builtin b i -> solveOverload i b
  ObjectOp o i ->
    ObjectOp <$> traverse resolveTerm o <*> pure i
  Error t1 t2 i ->
    pure (Error t1 t2 i)
  where
  listEqualityInstance i raw inst (Pred tc t) = do
    b <- solveOverload i (raw, [t], [Pred tc t])
    let a1Var = Name "" (NBound 1)
        a1 = (a1Var, TyList t)
        a2Var = Name "" (NBound 0)
        a2 = (a2Var, TyList t)
        app = App (Builtin inst i) (b :| [Var a1Var i, Var a2Var i]) i
    pure (Lam (a1 :| [a2]) app i)

  specializeAdd
    :: info
    -> Pred NamedDeBruijn
    -> OverloadT (CoreEvalTerm info)
  specializeAdd i p@(Pred _ t) = case t of
    TyInt -> pure (Builtin AddInt i)
    TyDecimal -> pure (Builtin AddDec i)
    TyString -> pure (Builtin AddStr i)
    TyList _ -> pure (Builtin AddList i)
    _ -> accessDict i "+" <$> lookupDictVar i p

  specializeNumOp
    :: info
    -> NumResolution
    -> Pred NamedDeBruijn
    -> OverloadT (CoreEvalTerm info)
  specializeNumOp i reso p@(Pred _ t) = case t of
    TyInt -> pure (Builtin (_nrIntInstance reso) i)
    TyDecimal -> pure (Builtin (_nrDecInstance reso) i)
    _ -> accessDict i (_nrRawName reso) <$> lookupDictVar i p

  specializeEq
    :: info
    -> RawBuiltin
    -> EqResolution
    -> Pred NamedDeBruijn
    -> OverloadT (CoreEvalTerm info)
  specializeEq i raw reso p@(Pred _ t) = case t of
    TyInt -> pure (Builtin (_erIntInstance reso) i)
    TyDecimal -> pure (Builtin (_erDecInstance reso) i)
    TyString -> pure (Builtin (_erStrInstance reso) i)
    TyUnit -> pure (Builtin (_erUnitInstance reso) i)
    TyBool -> pure (Builtin (_erBoolInstance reso) i)
    TyList t' ->
      listEqualityInstance i raw (_erListInstance reso) (Pred Eq t')
    _ -> accessDict i (_erRawName reso) <$> lookupDictVar i p

  specializeOrd
    :: info
    -> RawBuiltin
    -> OrdResolution
    -> Pred NamedDeBruijn
    -> OverloadT (CoreEvalTerm info)
  specializeOrd i raw reso p@(Pred _ t) = case t of
    TyInt -> pure (Builtin (_orIntInstance reso) i)
    TyDecimal -> pure (Builtin (_orDecInstance reso) i)
    TyString -> pure (Builtin (_orStrInstance reso) i)
    TyUnit -> pure (Builtin (_orUnitInstance reso) i)
    TyList t' ->
      listEqualityInstance i raw (_orListInstance reso) (Pred Ord t')
    _ -> accessDict i (_orRawName reso) <$> lookupDictVar i p

  specializeFracOp
    :: info
    -> FracResolution
    -> Pred NamedDeBruijn
    -> OverloadT (CoreEvalTerm info)
  specializeFracOp i reso p@(Pred _ t) = case t of
    TyInt -> pure (Builtin (_frIntInstance reso) i)
    TyDecimal -> pure (Builtin (_frDecInstance reso) i)
    _ -> accessDict i (_frRawName reso) <$> lookupDictVar i p

  specializeListLikeOp
    :: info
    -> ListLikeResolution
    -> Pred NamedDeBruijn
    -> OverloadT (CoreEvalTerm info)
  specializeListLikeOp i reso p@(Pred _ t) = case t of
    TyString -> pure (Builtin (_llrStrInstance reso) i)
    TyList _ -> pure (Builtin (_llrListInstance reso) i)
    _ -> accessDict i (_llrRawName reso) <$> lookupDictVar i p
  accessDict i f v =
    let op = ObjectAccess (Field f) v
    in ObjectOp op i
  -- We specialize here on the common case
  -- and solve overloaded variables.
  -- see: [Typeclasses and Instances] in
  -- Builtin.hs
  -- NOTE: We rely on the invariant that
  -- the typechecker produces the right dictionaries passed
  -- to the builtins
  -- Todo: refactor to get an exhaustivity check
  -- on the raw builtins
  solveOverload
    :: info
    -> (RawBuiltin, [Type NamedDeBruijn], [Pred NamedDeBruijn])
    -> OverloadT (CoreEvalTerm info)
  solveOverload i = \case
    -- Addition
    -- Note, we can also sanity check this here.
    -- (+) Add instances for base types + dynamic access
    (RawAdd, [_], [p]) ->
      specializeAdd i p

    -- (-) Num instances
    (RawSub, [_], [p]) ->
      specializeNumOp i subResolve p

    -- (*) Instances + Dynamic access
    (RawMultiply, [_], [p]) ->
      specializeNumOp i mulResolve p

    -- (/) instances + dynamic access
    (RawDivide, [_], [p]) ->
      specializeNumOp i divResolve p
    -- (negate) instances + dynamic access
    (RawNegate, [_], [p]) ->
      specializeNumOp i negateResolve p

    (RawAbs, [_], [p]) ->
      specializeNumOp i absResolve p
    -- bool ops
    (RawAnd, [] , []) ->
      pure (Builtin AndBool i)
    (RawOr, [], []) ->
      pure (Builtin OrBool i)
    (RawNot, [], []) ->
      pure (Builtin NotBool i)
    -- (==) instance + dyn access
    -- TODO: TIME
    (RawEq, [TyRow _], _) ->
      pure (Builtin EqObj i)
    (RawEq, [_], [p]) ->
      specializeEq i RawEq eqResolve p
    -- (/=) instance + dyn access
    (RawNeq, [TyRow _], _) ->
      pure (Builtin NeqObj i)
    (RawNeq, [_], [p]) ->
      specializeEq i RawNeq neqResolve p
    -- Ord : GT (>) instances
    -- todo: time
    (RawGT, [_], [p]) ->
      specializeOrd i RawGT gtResolve p
    -- Ord : GEQ
    (RawGEQ, [_], [p]) ->
      specializeOrd i RawGEQ geqResolve p
    -- Ord: LT
    (RawLT, [_], [p]) ->
      specializeOrd i RawLT ltResolve p
    -- Ord : LEQ
    (RawLEQ, [_], [p]) ->
      specializeOrd i RawLEQ leqResolve p

    (RawBitwiseAnd, _, _) ->
      pure (Builtin BitAndInt i)

    (RawBitwiseOr, _, _) ->
      pure (Builtin BitOrInt i)

    (RawBitwiseXor, _, _) ->
      pure (Builtin BitXorInt i)

    (RawBitwiseFlip, _, _) ->
      pure (Builtin BitComplementInt i)

    (RawBitShift, _,  _) ->
      pure (Builtin BitShiftInt i)
    (RawRound, [], []) ->
      pure (Builtin RoundDec i)
    (RawCeiling, [], []) ->
      pure (Builtin CeilingDec i)
    (RawFloor, [], []) ->
      pure (Builtin FloorDec i)
    -- Fractional instnaces
    (RawExp, [_], [p]) ->
      specializeFracOp i expResolve p
    (RawLn, [_], [p]) ->
      specializeFracOp i lnResolve p
    (RawSqrt, [_], [p]) ->
      specializeFracOp i sqrtResolve p
    (RawLogBase, [_], [p]) ->
      specializeFracOp i logBaseResolve p
    -- ListLike instances
    (RawLength, [_], [p]) ->
      specializeListLikeOp i lengthResolve p
    (RawTake, [_], [p]) ->
      specializeListLikeOp i takeResolve p
    (RawDrop, [_], [p]) ->
      specializeListLikeOp i dropResolve p
    (RawConcat, [_], [p]) ->
      specializeListLikeOp i concatResolve p
    (RawReverse, [_], [p]) ->
      specializeListLikeOp i reverseResolve p
    -- Todo: overload logbase
    (RawMod, [], []) ->
      pure (Builtin ModInt i)
    -- General
    (RawMap, [t1, t2], []) -> do
      let b = Builtin MapList i
      pure (TyApp b (t1:|[t2]) i)
    (RawFilter, [t1], []) ->  do
      let b = Builtin FilterList i
      pure (TyApp b (t1:|[]) i)
    (RawZip, [t1, t2, t3], []) ->  do
      let b = Builtin ZipList i
      pure (TyApp b (t1:|[t2, t3]) i)
    (RawIf, [t1], []) -> do
      let b = Builtin IfElse i
      pure (TyApp b (t1:|[]) i)
    (RawShow, [TyInt], _) ->
      pure (Builtin ShowInt i)
    (RawShow, [TyDecimal], _) ->
      pure (Builtin ShowDec i)
    (RawShow, [TyString], _) ->
      pure (Builtin ShowStr i)
    (RawShow, [TyUnit], _) ->
      pure (Builtin ShowUnit i)
    (RawShow, [TyBool], _) ->
      pure (Builtin ShowBool i)
    (RawShow, [TyList t], [_]) -> do
      b <- solveOverload i (RawShow, [t], [Pred Show t])
      let a1Var = Name "" (NBound 0)
          a1 = (a1Var, t)
          app = App (Builtin ShowList i) (b :| [Var a1Var i]) i
      pure (Lam (a1 :| []) app i)
    (RawShow, [_], [p]) ->
      accessDict i "show" <$> lookupDictVar i p
    (RawEnumerate, [], []) ->
      pure (Builtin Enumerate i)
    (RawEnumerateStepN, [], []) ->
      pure (Builtin EnumerateStepN i)
    (RawFold, [l, r], []) ->
      let b = Builtin FoldList i
      in pure (TyApp b (l :| [r]) i)
    _ -> error "could not resolve overload"

unsafeToTLName :: OverloadedName o -> Name
unsafeToTLName (OverloadedName n nk) = case nk of
  OTopLevel m mh -> Name n (NTopLevel m mh)
  _ -> error "invalid"

resolveDefun
  :: OverloadedDefun RawBuiltin info
  -> OverloadT (CoreEvalDefun info)
resolveDefun (Defun dname ty term info) = do
  let dname' = unsafeToTLName dname
  term' <- resolveTerm term
  pure (Defun dname' ty term' info)

resolveDefConst
  :: OverloadedDefConst RawBuiltin info
  -> OverloadT (CoreEvalDefConst info)
resolveDefConst (DefConst dname ty term info) = do
  let dname' = unsafeToTLName dname
  term' <- resolveTerm term
  pure (DefConst dname' ty term' info)

resolveDef
  :: OverloadedDef RawBuiltin info
  -> OverloadT (CoreEvalDef info)
resolveDef = \case
  Dfun d -> Dfun <$> resolveDefun d
  DConst d -> DConst <$> resolveDefConst d

resolveModule
  :: OverloadedModule RawBuiltin info
  -> OverloadT (CoreEvalModule info)
resolveModule m = do
  defs' <- traverse resolveDef (_mDefs m)
  let gov' = unsafeToTLName <$> _mGovernance m
  pure m{_mDefs=defs', _mGovernance=gov'}

resolveTopLevel
  :: OverloadedTopLevel RawBuiltin info
  -> OverloadT (CoreEvalTopLevel info)
resolveTopLevel = \case
  TLModule m -> TLModule <$> resolveModule m
  TLTerm t -> TLTerm <$> resolveTerm t
  _ -> error "unimplemented"

resolveProgram
  :: [OverloadedTopLevel RawBuiltin info]
  -> OverloadT [CoreEvalTopLevel info]
resolveProgram  = traverse resolveTopLevel

resolveReplTopLevel
  :: OverloadedReplTopLevel RawBuiltin info
  -> OverloadT (CoreEvalReplTopLevel info)
resolveReplTopLevel = \case
  RTLModule m -> RTLModule <$> resolveModule m
  RTLTerm t -> RTLTerm <$> resolveTerm t
  RTLDefun d -> RTLDefun <$> resolveDefun d
  RTLDefConst d -> RTLDefConst <$> resolveDefConst d

resolveReplProgram
  :: [OverloadedReplTopLevel RawBuiltin info]
  -> OverloadT [CoreEvalReplTopLevel info]
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

varOverloaded :: i -> Pred NamedDeBruijn -> OverloadT (CoreEvalTerm i)
varOverloaded i (Pred tc ty) = case tc of
  WithoutField _ -> fail "invariant failure"
  Add -> addOverloaded i ty
  Eq -> eqOverloaded i ty
  Num -> numOverloaded i ty
  Ord -> ordOverloaded i ty
  Show -> showOverloaded i ty
  Fractional -> fractionalOverloaded i ty
  ListLike -> listLikeOverloaded i ty

lookupDictVar :: i -> Pred NamedDeBruijn -> OverloadT (CoreEvalTerm i)
lookupDictVar i p = do
  depth <- view roVarDepth
  ols <- view roOverloads
  case lookup p ols of
    Just d -> do
      let n = Name "#dictInst" $ NBound (depth - d - 1)
      pure (Var n i)
    Nothing -> fail "invariant failure: unbound dictionary variable"

addOverloaded
  :: info
  -> Type NamedDeBruijn
  -> OverloadT (CoreEvalTerm info)
addOverloaded i = \case
  TyInt -> pure (addInt i)
  TyDecimal -> pure (addDec i)
  TyString -> pure (addStr i)
  TyList _ -> pure (addList i)
  t@TyVar{} -> bindDictVar i Add t
  _ -> fail "invariant failure: no instance for add"


eqOverloaded
  :: i
  -> Type NamedDeBruijn
  -> OverloadT (CoreEvalTerm i)
eqOverloaded i = \case
  TyPrim p -> case p of
    PrimInt -> pure (eqInt i)
    PrimDecimal -> pure (eqDecimal i)
    PrimString -> pure (eqString i)
    PrimBool -> pure (eqBool i)
    PrimUnit -> pure (eqUnit i)
    _ -> fail "invariant failure: no instance for eq"
  t@TyVar{} -> bindDictVar i Eq t
  TyRow _ -> pure (eqObj i)
  _ -> fail "invariant failure: no instance for eq"

ordOverloaded
  :: info
  -> Type NamedDeBruijn
  -> OverloadT (CoreEvalTerm info)
ordOverloaded i = \case
  TyPrim p -> case p of
    PrimInt -> pure (ordInt i)
    PrimDecimal -> pure (ordDec i)
    PrimString -> pure (ordStr i)
    PrimUnit -> pure (ordUnit i)
    _ -> fail "invariant failure, no such ord instance"
  t@TyVar{} -> bindDictVar i Ord t
  _ -> fail "invariant failure, no such ord instance"

bindDictVar
  :: info
  -> BuiltinTC
  -> Type NamedDeBruijn
  -> OverloadT (CoreEvalTerm info)
bindDictVar i tc t = do
  depth <- view roVarDepth
  ols <- view roOverloads
  case lookup (Pred tc t) ols of
    Just d -> do
      let n = Name "#dictInst" $ NBound (depth - d - 1)
      pure (Var n i)
    Nothing -> fail "invariant failure: unbound dictionary variable"

showOverloaded
  :: info
  -> Type NamedDeBruijn
  -> OverloadT (CoreEvalTerm info)
showOverloaded i = \case
  TyPrim p -> case p of
    PrimInt -> pure (showInt i)
    PrimDecimal -> pure (showDec i)
    PrimString -> pure (showStr i)
    PrimBool -> pure (showBool i)
    PrimUnit -> pure (showUnit i)
    _ -> fail "invariant failure: no instance for show"
  t@TyVar{} -> bindDictVar i Show t
  TyRow _ -> pure (eqObj i)
  _ -> fail "invariant failure: no instance for show"

numOverloaded
  :: info
  -> Type NamedDeBruijn
  -> OverloadT (CoreEvalTerm info)
numOverloaded i = \case
  TyInt -> pure (numInt i)
  TyDecimal -> pure (numDec i)
  t@TyVar{} -> bindDictVar i Num t
  _ -> fail "invariant failure: no instance for num"

fractionalOverloaded
  :: info
  -> Type NamedDeBruijn
  -> OverloadT (CoreEvalTerm info)
fractionalOverloaded i = \case
  TyInt -> pure (fracInt i)
  TyDecimal -> pure (fracDec i)
  t@TyVar{} -> bindDictVar i Fractional t
  _ -> fail "invariant failure: no instance for frac"

listLikeOverloaded
  :: info
  -> Type NamedDeBruijn
  -> OverloadT (CoreEvalTerm info)
listLikeOverloaded i = \case
  TyString -> pure (listLikeStr i)
  TyList _ -> pure (listLikeList i)
  t@TyVar{} -> bindDictVar i ListLike t
  _ -> fail "invariant failure: no instance for listLike"

eqDict :: b -> b -> i -> Term n tn b i
eqDict eq neq i =
  ObjectLit o i
  where
  o = Map.fromList
    [(Field "==", Builtin eq i)
    ,(Field "/=", Builtin neq i)]

showDict :: b -> i -> Term n tn b i
showDict s i =
  ObjectLit o i
  where
  o = Map.singleton (Field "show") (Builtin s i)

ordDict :: b -> b -> b -> b -> i -> Term n tn b i
ordDict gt geq lt leq i =
  ObjectLit o i
  where
  o = Map.fromList
    [ (Field ">", Builtin gt i)
    , (Field ">=", Builtin geq i)
    , (Field "<", Builtin lt i)
    , (Field "<=", Builtin leq i)]

addDict :: builtin -> info -> Term name tyname builtin info
addDict add i =
  ObjectLit o i
  where
  o = Map.singleton (Field "+") (Builtin add i)

numDict :: builtin
  -> builtin
  -> builtin
  -> builtin
  -> builtin
  -> info
  -> Term name tyname builtin info
numDict sub mul divv neg abs' i =
  ObjectLit o i
  where
  o = Map.fromList
    [ (Field "-", Builtin sub i)
    , (Field "*", Builtin mul i)
    , (Field "/", Builtin divv i)
    , (Field "negate", Builtin neg i)
    , (Field "abs", Builtin abs' i)]

fracDict :: builtin
  -> builtin
  -> builtin
  -> builtin
  -> info
  -> Term name tyname builtin info
fracDict ln_ exp_ sqrt_ logBase_ i =
  ObjectLit o i
  where
  o = Map.fromList
    [ (Field "ln", Builtin ln_ i)
    , (Field "exp", Builtin exp_ i)
    , (Field "sqrt", Builtin sqrt_ i)
    , (Field "log-base", Builtin logBase_ i)]

listLikeDict :: builtin
  -> builtin
  -> builtin
  -> builtin
  -> builtin
  -> info
  -> Term name tyname builtin info
listLikeDict take_ drop_ concat_ rev len i =
  ObjectLit o i
  where
  o = Map.fromList
    [ (Field "take", Builtin take_ i)
    , (Field "drop", Builtin drop_ i)
    , (Field "concat", Builtin concat_ i)
    , (Field "reverse", Builtin rev i)
    , (Field "length", Builtin len i)]

eqInt :: info -> CoreEvalTerm info
eqInt = eqDict EqInt NeqInt

eqDecimal :: info -> CoreEvalTerm info
eqDecimal = eqDict EqDec NeqDec

eqString :: info -> CoreEvalTerm info
eqString = eqDict EqStr NeqStr

eqUnit :: info -> CoreEvalTerm info
eqUnit = eqDict EqUnit NeqUnit

eqBool :: info -> CoreEvalTerm info
eqBool = eqDict EqBool NeqBool

eqObj :: info -> CoreEvalTerm info
eqObj = eqDict EqObj NeqObj

numInt, numDec :: info -> CoreEvalTerm info
numInt = numDict SubInt MulInt DivInt NegateInt AbsInt
numDec = numDict AddDec MulDec DivDec NegateDec AbsDec

addInt, addDec, addStr, addList :: info -> CoreEvalTerm info
addInt = addDict AddInt
addDec = addDict AddDec
addStr = addDict AddStr
addList = addDict AddList

ordInt, ordDec, ordStr, ordUnit :: info -> CoreEvalTerm info
ordInt = ordDict GTInt GEQInt LTInt LEQInt
ordDec = ordDict GTDec GEQDec LTDec LEQDec
ordStr = ordDict GTStr GEQStr LTStr LEQStr
ordUnit = ordDict NeqUnit EqUnit NeqUnit EqUnit

showInt, showDec, showStr, showUnit, showBool :: info -> CoreEvalTerm info
showInt = showDict ShowInt
showDec = showDict ShowDec
showStr = showDict ShowStr
showUnit = showDict ShowUnit
showBool = showDict ShowBool

fracInt, fracDec :: info -> CoreEvalTerm info
fracInt = fracDict LnInt ExpInt SqrtInt LogBaseInt
fracDec = fracDict LnDec ExpDec SqrtDec LogBaseDec

listLikeStr, listLikeList :: info -> CoreEvalTerm info
listLikeStr = listLikeDict TakeStr DropStr ConcatStr ReverseStr LengthStr
listLikeList = listLikeDict TakeList DropList ConcatList ReverseList LengthList

runOverload :: OverloadT a -> IO a
runOverload act = do
  let st = ROState mempty 0 []
  runReaderT act st

runOverloadTerm :: OverloadedTerm RawBuiltin i -> IO (CoreEvalTerm i)
runOverloadTerm t = runOverload (resolveTerm t)

runOverloadModule :: OverloadedModule RawBuiltin i -> IO (CoreEvalModule i)
runOverloadModule m = runOverload (resolveModule m)

runOverloadTopLevel :: OverloadedTopLevel RawBuiltin info -> IO (CoreEvalTopLevel info)
runOverloadTopLevel tl = runOverload (resolveTopLevel tl)

runOverloadReplTopLevel
  :: OverloadedReplTopLevel RawBuiltin info
  -> IO (CoreEvalReplTopLevel info)
runOverloadReplTopLevel tl = runOverload (resolveReplTopLevel tl)

runOverloadProgram
  :: [OverloadedTopLevel RawBuiltin info]
  -> IO [CoreEvalTopLevel info]
runOverloadProgram prog = runOverload (resolveProgram prog)

runOverloadReplProgram
  :: [OverloadedReplTopLevel RawBuiltin info]
  -> IO [CoreEvalReplTopLevel info]
runOverloadReplProgram prog = runOverload (resolveReplProgram prog)
