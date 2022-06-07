{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}



module Pact.Core.Typed.Overload where

import Control.Lens hiding (ix, op)
import Control.Monad.Reader
-- import Data.Map.Strict(Map)
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


resolveOverload
  :: OverloadedTerm RawBuiltin info
  -> OverloadT (EvalTerm info)
resolveOverload = \case
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
  Lam lamn nts e info -> do
    depth <- view roVarDepth
    let lamn' = Name (_olName lamn) (NBound 0)
        (ns, ts) = NE.unzip nts
        len = fromIntegral (NE.length nts)
        newDepth = depth + len
        ixs = NE.fromList [depth..newDepth-1]
        nsix = NE.zip ns ixs
        (m, de) = foldl' mkEnv (mempty, []) nsix
        ns' = toON <$> nsix
    e' <- local (inEnv m de newDepth) (resolveOverload e)
    pure (Lam lamn' (NE.zip ns' ts) e' info)
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
    u <- case (_olNameKind n) of
      OBound u -> pure u
      _ -> fail "invariant error: let bound as incorrect variable"
    depth <- view roVarDepth
    let n' = Name (_olName n) (NBound depth)
    e1' <- resolveOverload e1
    e2' <- local (inEnv u depth) $ resolveOverload e2
    pure (Let n' e1' e2' i)
    where
    inEnv u depth =
      over roVarDepth (+ 1) . over roBoundVars (IntMap.insert u depth)
  App f args i ->
    App <$> resolveOverload f <*> traverse resolveOverload args <*> pure i
  TyApp l rs i ->
    TyApp <$> resolveOverload l <*> pure rs <*> pure i
  TyAbs nel t i ->
    TyAbs nel <$> resolveOverload t <*> pure i
  Block nel i ->
    Block <$> traverse resolveOverload nel <*> pure i
  ObjectLit ms i ->
    ObjectLit <$> traverse resolveOverload ms <*> pure i
  ListLit tn ts i ->
    ListLit tn <$> traverse resolveOverload ts <*> pure i
  Constant lit i ->
    pure (Constant lit i)
  Builtin b i -> solveOverload i b
  ObjectOp o i ->
    ObjectOp <$> traverse resolveOverload o <*> pure i
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
    pure (Lam a1Var (a1 :| [a2]) app i)

  specializeAdd
    :: info
    -> Pred NamedDeBruijn
    -> OverloadT (EvalTerm info)
  specializeAdd i p@(Pred _ t) = case t of
    TyInt -> pure (Builtin AddInt i)
    TyDecimal -> pure (Builtin AddDec i)
    TyString -> pure (Builtin AddStr i)
    TyList _ -> pure (Builtin AddList i)
    _ -> accessDict i "+" <$> lookupDictVar i p

  specializeNumOp i reso p@(Pred _ t) = case t of
    TyInt -> pure (Builtin (_nrIntInstance reso) i)
    TyDecimal -> pure (Builtin (_nrDecInstance reso) i)
    _ -> accessDict i (_nrRawName reso) <$> lookupDictVar i p

  specializeEq i raw reso p@(Pred _ t) = case t of
    TyInt -> pure (Builtin (_erIntInstance reso))
    TyDecimal -> pure (Builtin (_erDecInstance reso) i)
    TyString -> pure (Builtin (_erStrInstance reso) i)
    TyUnit -> pure (Builtin (_erUnitInstance reso) i)
    TyBool -> pure (Builtin (_erBoolInstance reso) i)
    TyList t' ->
      listEqualityInstance raw (_erListInstance reso) (Pred Eq t')
    _ -> accessDict i (_orRawName reso) <$> lookupDictVar i p

  specializeOrd i raw reso p@(Pred _ t) = case t of
    TyInt -> pure (Builtin (_orIntInstance reso) i)
    TyDecimal -> pure (Builtin (_orDecInstance reso) i)
    TyString -> pure (Builtin (_orStrInstance reso) i)
    TyUnit -> pure (Builtin (_orUnitInstance reso) i)
    TyList t' ->
      listEqualityInstance raw (_orListInstance reso) (Pred Ord t')
    _ -> accessDict i (_orRawName reso) <$> lookupDictVar i p

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
  solveOverload
    :: info
    -> (RawBuiltin, [Type NamedDeBruijn], [Pred NamedDeBruijn])
    -> OverloadT (EvalTerm info)
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
    (RawEq, [TyInt], _) ->
      pure (Builtin EqInt i)
    (RawEq, [TyDecimal], _) ->
      pure (Builtin EqDec i)
    (RawEq, [TyString], _) ->
      pure (Builtin EqStr i)
    (RawEq, [TyUnit], _) ->
      pure (Builtin EqUnit i)
    (RawEq, [TyBool], _) ->
      pure (Builtin EqBool i)
    -- Eq for lists essentially builds the closure
    -- for some type T enclosed by list T
    -- (\a b -> eqList #eqFnT a b)
    (RawEq, [TyList t], [p]) -> do
      listEqualityInstance i RawEq EqList (Pred )
    (RawEq, [TyRow _], _) ->
      pure (Builtin EqObj i)
    (RawEq, _, [p]) ->
      accessDict i "==" <$> lookupDictVar i p
    -- (/=) instance + dyn access
    (RawNeq, [TyInt], _) ->
      pure (Builtin NeqInt i)

    (RawNeq, [TyDecimal], _) ->
      pure (Builtin NeqDec i)

    (RawNeq, [TyString], _) ->
      pure (Builtin NeqStr i)

    (RawNeq, [TyUnit], _) ->
      pure (Builtin NeqUnit i)

    (RawNeq, [TyBool], _) ->
      pure (Builtin NeqBool i)

    -- See: comment on list eq instance
    (RawNeq, [TyList t], [_]) -> do
      b <- solveOverload i (RawNeq, [t], [Pred Eq t])
      let a1Var = Name "" (NBound 1)
          a1 = (a1Var, TyList t)
          a2Var = Name "" (NBound 0)
          a2 = (a2Var, TyList t)
          app = App (Builtin EqList i) (b :| [Var a1Var i, Var a2Var i]) i
      pure (Lam a1Var (a1 :| [a2]) app i)
    (RawNeq, [TyRow _], _) ->
      pure (Builtin NeqObj i)
    (RawNeq, _, [p]) ->
      accessDict i "/=" <$> lookupDictVar i p
    -- Ord : GT (>) instances
    -- todo: time
    (RawGT, [TyInt], _) ->
      pure (Builtin GTInt i)

    (RawGT, [TyDecimal], _) ->
      pure (Builtin GTDec i)

    (RawGT, [TyString], _) ->
      pure (Builtin GTStr i)

    (RawGT, [TyUnit], _) ->
      pure (Builtin NeqUnit i)

    (RawGT, [_], [p]) ->
      accessDict i ">" <$> lookupDictVar i p
    -- Ord : GEQ
    (RawGEQ, [TyInt], _) ->
      pure (Builtin GEQInt i)

    (RawGEQ, [TyDecimal], _) ->
      pure (Builtin GEQDec i)

    (RawGEQ, [TyString], _) ->
      pure (Builtin GEQStr i)

    (RawGEQ, [TyUnit], _) ->
      pure (Builtin EqUnit i)

    (RawGEQ, [_], [p]) ->
      accessDict i ">=" <$> lookupDictVar i p
    -- Ord: LT
    (RawLT, [TyInt], _) ->
      pure (Builtin LTInt i)

    (RawLT, [TyDecimal], _) ->
      pure (Builtin LTDec i)

    (RawLT, [TyString], _) ->
      pure (Builtin LTStr i)

    (RawLT, [TyUnit], _) ->
      pure (Builtin NeqUnit i)

    (RawLT, [_], [p]) ->
      accessDict i "<" <$> lookupDictVar i p
    -- Ord : LEQ
    (RawLEQ, [TyInt], _) ->
      pure (Builtin LEQInt i)

    (RawLEQ, [TyDecimal], _) ->
      pure (Builtin LEQDec i)

    (RawLEQ, [TyString], _) ->
      pure (Builtin LEQStr i)

    (RawLEQ, [TyUnit], _) ->
      pure (Builtin EqUnit i)

    (RawLEQ, [_], [p]) ->
      accessDict i "<=" <$> lookupDictVar i p

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
    -- Todo: OVERLOADS HERE
    (RawAbs, _, _) ->
      pure (Builtin AbsInt i)
    (RawRound, [], []) ->
      pure (Builtin RoundDec i)
    (RawCeiling, [], []) ->
      pure (Builtin CeilingDec i)
    (RawFloor, [], []) ->
      pure (Builtin FloorDec i)
    (RawExp, [], []) ->
      pure (Builtin ExpDec i)
    (RawLn, [], []) ->
      pure (Builtin LnDec i)
    -- Todo: overload logbase
    (RawLogBase, [], []) ->
      pure (Builtin LogBaseDec i)
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
      error "todo: show bool"
    (RawShow, [TyList t], [_]) -> do
      b <- solveOverload i (RawShow, [t], [Pred Show t])
      let a1Var = Name "" (NBound 0)
          a1 = (a1Var, t)
          app = App (Builtin ShowList i) (b :| [Var a1Var i]) i
      pure (Lam a1Var (a1 :| []) app i)
    (RawShow, [_], [p]) ->
      accessDict i "show" <$> lookupDictVar i p
    _ -> error "unimplemented"
    -- RawAbs -> AbsInt
    -- RawRound -> RoundDec
    -- RawCeiling -> CeilingDec
    -- RawExp -> ExpDec
    -- RawFloor -> FloorDec
    -- RawLn -> LnDec
    -- RawLogBase -> LogBaseDec
    -- RawMod -> ModInt
    -- RawMap -> MapList
    -- RawFilter -> FilterList
    -- RawIf -> IfElse
    -- RawIntToStr -> undefined
    -- RawConcat -> ConcatStr
    -- RawStrToInt -> undefined
    -- RawTake -> TakeList
    -- RawDrop -> DropList
    -- RawLength -> LengthList
    -- RawFold -> FoldList
    -- RawDistinct -> DistinctList
    -- RawEnforce -> Enforce
    -- RawEnforceOne -> EnforceOne
    -- RawEnumerate -> Enumerate
    -- RawEnumerateStepN -> EnumerateStepN
    -- RawDummy -> Dummy
    -- _ -> undefined
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
  , _llStrInstance :: CoreBuiltin
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
  , _llStrInstance = TakeStr
  , _llrListInstance = TakeList
  }

dropResolve :: ListLikeResolution
dropResolve =
  ListLikeResolution
  { _llrRawName = "drop"
  , _llStrInstance = DropStr
  , _llrListInstance = DropList
  }

concatResolve :: ListLikeResolution
concatResolve =
  ListLikeResolution
  { _llrRawName = "concat"
  , _llStrInstance = ConcatStr
  , _llrListInstance = ConcatList
  }

reverseResolve :: ListLikeResolution
reverseResolve =
  ListLikeResolution
  { _llrRawName = "reverse"
  , _llStrInstance = ReverseStr
  , _llrListInstance = ReverseList
  }


lengthResolve :: ListLikeResolution
lengthResolve =
  ListLikeResolution
  { _llrRawName = "length"
  , _llStrInstance = LengthStr
  , _llrListInstance = LengthList
  }

varOverloaded :: i -> Pred NamedDeBruijn -> ReaderT ROState IO (EvalTerm i)
varOverloaded i (Pred tc ty) = case tc of
  WithoutField _ -> fail "invariant failure"
  Eq -> eqOverloaded i ty
  _ -> error "unimplemented"

lookupDictVar :: i -> Pred NamedDeBruijn -> ReaderT ROState IO (EvalTerm i)
lookupDictVar i p = do
  depth <- view roVarDepth
  ols <- view roOverloads
  case lookup p ols of
    Just d -> do
      let n = Name "#dictInst" $ NBound (depth - d - 1)
      pure (Var n i)
    Nothing -> fail "invariant failure: unbound dictionary variable"

eqOverloaded :: i -> Type NamedDeBruijn -> OverloadT (EvalTerm i)
eqOverloaded i = \case
  TyPrim p -> case p of
    PrimInt -> pure (eqInt i)
    PrimDecimal -> pure (eqDecimal i)
    PrimString -> pure (eqString i)
    PrimBool -> pure (eqBool i)
    PrimTime -> pure (eqTime i)
    PrimUnit -> pure (eqUnit i)
  t@TyVar{} -> do
    depth <- view roVarDepth
    ols <- view roOverloads
    case lookup (Pred Eq t) ols of
      Just d -> do
        let n = Name "#dictInst" $ NBound (depth - d - 1)
        pure (Var n i)
      Nothing -> fail "invariant failure: unbound dictionary variable"
  TyList t -> eqOverloaded i t
  TyRow _ -> pure (eqObj i)
    -- term' <- eqOverloaded i t
    -- let b = Builtin EqList i
    -- pure (App b (term' :| []) i)
  _ -> fail "invariant failure: no instance for eq"

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

addDict :: b -> i -> Term n tn b i
addDict add i =
  ObjectLit o i
  where
  o = Map.singleton (Field "+") (Builtin add i)

numDict :: b -> b -> b -> b -> i -> Term n tn b i
numDict sub mul divv neg i =
  ObjectLit o i
  where
  o = Map.fromList
    [ (Field "-", Builtin sub i)
    , (Field "*", Builtin mul i)
    , (Field "/", Builtin divv i)
    , (Field "negate", Builtin neg i)]

eqInt :: info -> EvalTerm info
eqInt = eqDict EqInt NeqInt

eqDecimal :: info -> EvalTerm info
eqDecimal = eqDict EqDec NeqDec

eqString :: info -> EvalTerm info
eqString = eqDict EqStr NeqStr

eqUnit :: info -> EvalTerm info
eqUnit = eqDict EqUnit NeqUnit

eqBool :: info -> EvalTerm info
eqBool = eqDict EqBool NeqBool

eqObj :: info -> EvalTerm info
eqObj = eqDict EqObj NeqObj

eqTime :: info -> EvalTerm info
eqTime = undefined

numInt, numDec :: info -> EvalTerm info
numInt = numDict SubInt MulInt DivInt NegateInt
numDec = numDict AddDec MulDec DivDec NegateDec

addInt, addDec, addStr, addList :: info -> EvalTerm info
addInt = addDict AddInt
addDec = addDict AddDec
addStr = addDict AddStr
addList = addDict AddList

ordInt, ordDec, ordStr, ordUnit :: info -> EvalTerm info
ordInt = ordDict GTInt GEQInt LTInt LEQInt
ordDec = ordDict GTDec GEQDec LTDec LEQDec
ordStr = ordDict GTStr GEQStr LTStr LEQStr
ordUnit = ordDict NeqUnit EqUnit NeqUnit EqUnit

runOverload :: OverloadedTerm RawBuiltin i -> IO (EvalTerm i)
runOverload t = do
  let st = ROState mempty 0 []
  runReaderT (resolveOverload t) st
