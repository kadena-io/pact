{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Analyze.Translate where

import           Control.Lens              hiding (op, (...))
import           Control.Monad.Morph       (generalize, hoist)
import           Control.Monad.Reader      (ReaderT (runReaderT))
import           Control.Monad.Trans.Class (MonadTrans (lift))
import           Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT),
                                            exceptToMaybeT)

import           Pact.Analyze.Translate    (translateNodeNoGraph)
import           Pact.Analyze.Types        hiding (Object, Term)
import           Pact.Analyze.Util         (dummyInfo)

import           Pact.Eval                 (liftTerm)
import           Pact.Native               (dropDef, enforceDef, enforceOneDef,
                                            formatDef, hashDef, ifDef,
                                            lengthDef, makeListDef,
                                            pactVersionDef, readDecimalDef,
                                            reverseDef, sortDef, strToIntDef,
                                            takeDef
  -- TODO: mapDef foldDef filterDef whereDef composeDef atDef
                                            )
import           Pact.Native.Keysets
import           Pact.Native.Ops
import           Pact.Native.Time
import           Pact.Typechecker          (typecheckTopLevel)
import           Pact.Types.Exp            (Literal (..))
import           Pact.Types.Native         (NativeDef)
import           Pact.Types.Term           (Meta (Meta),
                                            Term (TApp, TConst, TLiteral))
import qualified Pact.Types.Term           as Pact
import qualified Pact.Types.Type           as Pact
import qualified Pact.Types.Typecheck      as Pact

import           Analyze.Gen


toPactTm :: ETerm -> ReaderT (GenEnv, GenState) Maybe (Pact.Term Pact.Ref)
toPactTm = \case
  -- core terms:

  Existential SDecimal (Inj (DecArithOp op x y)) ->
    mkApp (arithOpToDef op) [Existential SDecimal x, Existential SDecimal y]

  Existential SDecimal (Inj (DecUnaryArithOp op x)) ->
    mkApp (unaryArithOpToDef op) [Existential SDecimal x]

  Existential SInteger (Inj (IntArithOp op x y)) ->
    mkApp (arithOpToDef op) [Existential SInteger x, Existential SInteger y]

  Existential SInteger (Inj (IntUnaryArithOp op x)) ->
    mkApp (unaryArithOpToDef op) [Existential SInteger x]

  Existential SInteger (Inj (RoundingLikeOp1 op x)) ->
    mkApp (roundingLikeOpToDef op) [Existential SDecimal x]

  Existential SDecimal (Inj (RoundingLikeOp2 op x y)) ->
    mkApp (roundingLikeOpToDef op) [Existential SDecimal x, Existential SInteger y]

  Existential SDecimal (Inj (DecIntArithOp op x y)) ->
    mkApp (arithOpToDef op) [Existential SDecimal x, Existential SInteger y]

  Existential SDecimal (Inj (IntDecArithOp op x y)) ->
    mkApp (arithOpToDef op) [Existential SInteger x, Existential SDecimal y]

  Existential SInteger (Inj (ModOp x y)) ->
    mkApp modDef [Existential SInteger x, Existential SInteger y]

  Existential SInteger (Inj (StrLength x)) ->
    mkApp lengthDef [Existential SStr x]

  Existential SStr (Inj (StrConcat x y)) ->
    mkApp addDef [Existential SStr x, Existential SStr y]

  Existential SInteger (Inj (StrToInt s)) ->
    mkApp strToIntDef [Existential SStr s]

  Existential SInteger (Inj (StrToIntBase b s)) ->
    mkApp strToIntDef [Existential SInteger b, Existential SStr s]

  Existential SBool (Inj (IntegerComparison op x y)) ->
    mkApp (comparisonOpToDef op) [Existential SInteger x, Existential SInteger y]

  Existential SBool (Inj (DecimalComparison op x y)) ->
    mkApp (comparisonOpToDef op) [Existential SDecimal x, Existential SDecimal y]

  Existential SBool (Inj (StrComparison op x y)) ->
    mkApp (comparisonOpToDef op) [Existential SStr x, Existential SStr y]

  Existential SBool (Inj (BoolComparison op x y)) ->
    mkApp (comparisonOpToDef op) [Existential SBool x, Existential SBool y]

  Existential SBool (Inj (TimeComparison op x y)) ->
    mkApp (comparisonOpToDef op) [Existential STime x, Existential STime y]

  Existential STime (Inj (IntAddTime x y)) ->
    mkApp defAddTime [Existential STime x, Existential SInteger y]

  Existential STime (Inj (DecAddTime x y)) ->
    mkApp defAddTime [Existential STime x, Existential SDecimal y]

  Existential SBool (Inj (Logical op args)) ->
    mkApp (logicalOpToDef op) (Existential SBool <$> args)

  Existential SInteger     (CoreTerm (Lit x))
    -> pure $ TLiteral (LInteger x) dummyInfo
  Existential SDecimal (CoreTerm (Lit x))
    -> pure $ TLiteral (LDecimal (toPact decimalIso x)) dummyInfo
  Existential SStr     (TextLit x)
    -> pure $ TLiteral (LString x) dummyInfo
  Existential SBool    (CoreTerm (Lit x))
    -> pure $ TLiteral (LBool x) dummyInfo
  Existential STime    (CoreTerm (Lit x))
    -> pure $ TLiteral (LTime (toPact timeIso x)) dummyInfo

  Existential SKeySet  (CoreTerm (Lit (KeySet x))) -> do
    keysets <- view (_1 . envKeysets)
    case keysets ^? ix (fromIntegral x) of
      Just (ks, _) -> pure $ Pact.TKeySet ks dummyInfo
      Nothing      -> error $ "no keysets found at index " ++ show x

  -- term-specific terms:
  Existential SBool (Enforce _ x)
    -> mkApp enforceDef [Existential SBool x, Existential SStr $ CoreTerm $ Lit "(enforce)"]
  Existential SBool (EnforceOne Left{})
    -> mkApp' enforceOneDef (Existential SStr $ CoreTerm $ Lit "(enforce-one)") []

  Existential SBool (EnforceOne (Right xs)) -> mkApp' enforceOneDef
    (Existential SStr $ CoreTerm $ Lit "(enforce-one)")
    (Existential SBool . snd <$> xs)

  -- Existential SBool (KsAuthorized x)
  -- Existential SBool (NameAuthorized x)

  Existential SStr PactVersion -> mkApp pactVersionDef []
  Existential SStr (Format template vals)
    -> mkApp' formatDef (Existential SStr template) vals
  Existential SStr (FormatTime x y)
    -> mkApp defFormatTime [Existential SStr x, Existential STime y]
  Existential SStr (Hash x) -> mkApp hashDef [x]

  Existential SKeySet (ReadKeySet x) -> mkApp readKeysetDef [Existential SStr x]

  Existential SDecimal (ReadDecimal x) -> mkApp readDecimalDef [Existential SStr x]

  Existential STime (ParseTime Nothing x) ->
    mkApp timeDef [Existential SStr x]

  Existential STime (ParseTime (Just x) y) ->
    mkApp parseTimeDef [Existential SStr x, Existential SStr y]

  -- Existential ty (Sequence etm tm) -> do
  --   t1 <- toPactTm etm
  --   t2 <- toPactTm (Existential ty tm)
  --   pure $ TList [t1, t2] (Pact.TyList Pact.TyAny) dummyInfo

--   Existential ty (Let name _vid etm bodyTm) -> do
--     t1 <- toPactTm etm
--     t2 <- toPactTm (Existential ty bodyTm)
--     pure $ TBinding [(Pact.Arg name undefined dummyInfo, t1)]
--       t2 undefined dummyInfo

  Existential ty (IfThenElse _ t1 (_, t2) (_, t3)) ->
    mkApp ifDef [Existential SBool t1, Existential ty t2, Existential ty t3]

  Existential SBool (CoreTerm (ListEqNeq ty op l1 l2)) ->
    mkApp (eqNeqOpToDef op) [ Existential (SList ty) l1, Existential (SList ty) l2 ]

  Existential ty (CoreTerm (ListReverse _ lst)) ->
    mkApp reverseDef [ Existential ty lst ]

  Existential ty (CoreTerm (ListSort _ lst)) ->
    mkApp sortDef [ Existential ty lst ]

  Existential ty (CoreTerm (ListConcat _ l1 l2)) ->
    mkApp addDef [ Existential ty l1, Existential ty l2 ]

  Existential ty (CoreTerm (ListDrop _ i l2)) ->
    mkApp dropDef [ Existential SInteger i, Existential ty l2 ]

  Existential ty (CoreTerm (ListTake _ i l2)) ->
    mkApp takeDef [ Existential SInteger i, Existential ty l2 ]

  Existential (SList ty) (CoreTerm (MakeList _ i a)) ->
    mkApp makeListDef [ Existential SInteger i, Existential ty a ]

  tm -> error $ "TODO: toPactTm " ++ show tm

  where
    mkApp :: NativeDef -> [ETerm]
      -> ReaderT (GenEnv, GenState) Maybe (Pact.Term Pact.Ref)
    mkApp (_, defTm) args = do
      args' <- traverse toPactTm args
      pure $ TApp (liftTerm defTm) args' dummyInfo

    -- Like mkApp but for functions that take two arguments, the second of
    -- which is a list. This pattern is used in `enforce-one` and `format`
    mkApp'
      :: NativeDef
      -> ETerm
      -> [ETerm]
      -> ReaderT (GenEnv, GenState) Maybe (Pact.Term Pact.Ref)
    mkApp' (_, defTm) arg argList = do
      arg'     <- toPactTm arg
      argList' <- traverse toPactTm argList
      pure $ TApp (liftTerm defTm)
        [arg', Pact.TList argList' (Pact.TyList Pact.TyAny) dummyInfo]
        dummyInfo

    arithOpToDef :: ArithOp -> NativeDef
    arithOpToDef = \case
      Add -> addDef
      Sub -> subDef
      Mul -> mulDef
      Div -> divDef
      Pow -> powDef
      Log -> logDef

    unaryArithOpToDef :: UnaryArithOp -> NativeDef
    unaryArithOpToDef = \case
      Negate -> subDef
      Sqrt   -> sqrtDef
      Ln     -> lnDef
      Exp    -> expDef
      Abs    -> absDef
      Signum -> error "not yet implemented: we don't generate this operator"

    roundingLikeOpToDef :: RoundingLikeOp -> NativeDef
    roundingLikeOpToDef = \case
      Round   -> roundDef
      Ceiling -> ceilDef
      Floor   -> floorDef

    comparisonOpToDef :: ComparisonOp -> NativeDef
    comparisonOpToDef = \case
      Gt  -> gtDef
      Lt  -> ltDef
      Gte -> gteDef
      Lte -> lteDef
      Eq  -> eqDef
      Neq -> neqDef

    eqNeqOpToDef :: EqNeq -> NativeDef
    eqNeqOpToDef = \case
        Eq'  -> eqDef
        Neq' -> neqDef

    logicalOpToDef :: LogicalOp -> NativeDef
    logicalOpToDef = \case
      AndOp -> andDef
      OrOp  -> orDef
      NotOp -> notDef

toPactTm'
  -- :: Applicative m
  :: Monad m
  => (GenEnv, GenState) -> ETerm -> MaybeT m (Pact.Term Pact.Ref)
toPactTm' envState etm = MaybeT $ do
  pure $ runReaderT (toPactTm etm) envState

toAnalyze :: Pact.Type (Pact.Term Pact.Ref) -> Pact.Term Pact.Ref -> MaybeT IO ETerm
toAnalyze ty tm = do
  let cnst = TConst
        (Pact.Arg "tm" ty dummyInfo)
        "module"
        (Pact.CVRaw tm)
        (Meta Nothing [])
        dummyInfo
      ref = Pact.Ref cnst
  maybeConst <- lift $ Pact.runTC 0 False $ typecheckTopLevel ref
  (_cTy, ast) <- case maybeConst of
    (Pact.TopConst _info _name constTy constAst _meta, _tcState)
      -> pure (constTy, constAst)
    _ -> MaybeT $ pure Nothing

  hoist generalize $
    exceptToMaybeT $
      translateNodeNoGraph ast

-- This is limited to simple types for now
reverseTranslateType :: SingTy a -> Pact.Type b
reverseTranslateType = \case
  SBool     -> Pact.TyPrim Pact.TyBool
  SDecimal  -> Pact.TyPrim Pact.TyDecimal
  SInteger  -> Pact.TyPrim Pact.TyInteger
  SStr      -> Pact.TyPrim Pact.TyString
  STime     -> Pact.TyPrim Pact.TyTime
  SKeySet   -> Pact.TyPrim Pact.TyKeySet
  SAny      -> Pact.TyAny
  SList a   -> Pact.TyList $ reverseTranslateType a
  -- TODO(joel): this is no longer valid
  -- SObject needs to hold a type name or something
  SObject _ -> error "this reverse-translation is currently impossible"

fromPactVal :: EType -> Pact.Term Pact.Ref -> IO (Maybe ETerm)
fromPactVal (EType ty)  = runMaybeT . toAnalyze (reverseTranslateType ty)
