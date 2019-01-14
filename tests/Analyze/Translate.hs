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

  Some SDecimal (Inj (DecArithOp op x y)) ->
    mkApp (arithOpToDef op) [Some SDecimal x, Some SDecimal y]

  Some SDecimal (Inj (DecUnaryArithOp op x)) ->
    mkApp (unaryArithOpToDef op) [Some SDecimal x]

  Some SInteger (Inj (IntArithOp op x y)) ->
    mkApp (arithOpToDef op) [Some SInteger x, Some SInteger y]

  Some SInteger (Inj (IntUnaryArithOp op x)) ->
    mkApp (unaryArithOpToDef op) [Some SInteger x]

  Some SInteger (Inj (RoundingLikeOp1 op x)) ->
    mkApp (roundingLikeOpToDef op) [Some SDecimal x]

  Some SDecimal (Inj (RoundingLikeOp2 op x y)) ->
    mkApp (roundingLikeOpToDef op) [Some SDecimal x, Some SInteger y]

  Some SDecimal (Inj (DecIntArithOp op x y)) ->
    mkApp (arithOpToDef op) [Some SDecimal x, Some SInteger y]

  Some SDecimal (Inj (IntDecArithOp op x y)) ->
    mkApp (arithOpToDef op) [Some SInteger x, Some SDecimal y]

  Some SInteger (Inj (ModOp x y)) ->
    mkApp modDef [Some SInteger x, Some SInteger y]

  Some SInteger (Inj (StrLength x)) ->
    mkApp lengthDef [Some SStr x]

  Some SStr (Inj (StrConcat x y)) ->
    mkApp addDef [Some SStr x, Some SStr y]

  Some SInteger (Inj (StrToInt s)) ->
    mkApp strToIntDef [Some SStr s]

  Some SInteger (Inj (StrToIntBase b s)) ->
    mkApp strToIntDef [Some SInteger b, Some SStr s]

  Some SBool (Inj (IntegerComparison op x y)) ->
    mkApp (comparisonOpToDef op) [Some SInteger x, Some SInteger y]

  Some SBool (Inj (DecimalComparison op x y)) ->
    mkApp (comparisonOpToDef op) [Some SDecimal x, Some SDecimal y]

  Some SBool (Inj (StrComparison op x y)) ->
    mkApp (comparisonOpToDef op) [Some SStr x, Some SStr y]

  Some SBool (Inj (BoolComparison op x y)) ->
    mkApp (comparisonOpToDef op) [Some SBool x, Some SBool y]

  Some SBool (Inj (TimeComparison op x y)) ->
    mkApp (comparisonOpToDef op) [Some STime x, Some STime y]

  Some STime (Inj (IntAddTime x y)) ->
    mkApp defAddTime [Some STime x, Some SInteger y]

  Some STime (Inj (DecAddTime x y)) ->
    mkApp defAddTime [Some STime x, Some SDecimal y]

  Some SBool (Inj (Logical op args)) ->
    mkApp (logicalOpToDef op) (Some SBool <$> args)

  Some SInteger     (CoreTerm (Lit x))
    -> pure $ TLiteral (LInteger x) dummyInfo
  Some SDecimal (CoreTerm (Lit x))
    -> pure $ TLiteral (LDecimal (toPact decimalIso x)) dummyInfo
  Some SStr     (TextLit x)
    -> pure $ TLiteral (LString x) dummyInfo
  Some SBool    (CoreTerm (Lit x))
    -> pure $ TLiteral (LBool x) dummyInfo
  Some STime    (CoreTerm (Lit x))
    -> pure $ TLiteral (LTime (toPact timeIso x)) dummyInfo

  Some SKeySet  (CoreTerm (Lit (KeySet x))) -> do
    keysets <- view (_1 . envKeysets)
    case keysets ^? ix (fromIntegral x) of
      Just (ks, _) -> pure $ Pact.TKeySet ks dummyInfo
      Nothing      -> error $ "no keysets found at index " ++ show x

  -- term-specific terms:
  Some SBool (Enforce _ x)
    -> mkApp enforceDef [Some SBool x, Some SStr $ CoreTerm $ Lit "(enforce)"]
  Some SBool (EnforceOne Left{})
    -> mkApp' enforceOneDef (Some SStr $ CoreTerm $ Lit "(enforce-one)") []

  Some SBool (EnforceOne (Right xs)) -> mkApp' enforceOneDef
    (Some SStr $ CoreTerm $ Lit "(enforce-one)")
    (Some SBool . snd <$> xs)

  -- Some SBool (KsAuthorized x)
  -- Some SBool (NameAuthorized x)

  Some SStr PactVersion -> mkApp pactVersionDef []
  Some SStr (Format template vals)
    -> mkApp' formatDef (Some SStr template) vals
  Some SStr (FormatTime x y)
    -> mkApp defFormatTime [Some SStr x, Some STime y]
  Some SStr (Hash x) -> mkApp hashDef [x]

  Some SKeySet (ReadKeySet x) -> mkApp readKeysetDef [Some SStr x]

  Some SDecimal (ReadDecimal x) -> mkApp readDecimalDef [Some SStr x]

  Some STime (ParseTime Nothing x) ->
    mkApp timeDef [Some SStr x]

  Some STime (ParseTime (Just x) y) ->
    mkApp parseTimeDef [Some SStr x, Some SStr y]

  -- Some ty (Sequence etm tm) -> do
  --   t1 <- toPactTm etm
  --   t2 <- toPactTm (Some ty tm)
  --   pure $ TList [t1, t2] (Pact.TyList Pact.TyAny) dummyInfo

--   Some ty (Let name _vid etm bodyTm) -> do
--     t1 <- toPactTm etm
--     t2 <- toPactTm (Some ty bodyTm)
--     pure $ TBinding [(Pact.Arg name undefined dummyInfo, t1)]
--       t2 undefined dummyInfo

  Some ty (IfThenElse _ t1 (_, t2) (_, t3)) ->
    mkApp ifDef [Some SBool t1, Some ty t2, Some ty t3]

  Some SBool (CoreTerm (ListEqNeq ty op l1 l2)) ->
    mkApp (eqNeqOpToDef op) [ Some (SList ty) l1, Some (SList ty) l2 ]

  Some ty (CoreTerm (ListReverse _ lst)) ->
    mkApp reverseDef [ Some ty lst ]

  Some ty (CoreTerm (ListSort _ lst)) ->
    mkApp sortDef [ Some ty lst ]

  Some ty (CoreTerm (ListConcat _ l1 l2)) ->
    mkApp addDef [ Some ty l1, Some ty l2 ]

  Some ty (CoreTerm (ListDrop _ i l2)) ->
    mkApp dropDef [ Some SInteger i, Some ty l2 ]

  Some ty (CoreTerm (ListTake _ i l2)) ->
    mkApp takeDef [ Some SInteger i, Some ty l2 ]

  Some (SList ty) (CoreTerm (MakeList _ i a)) ->
    mkApp makeListDef [ Some SInteger i, Some ty a ]

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
