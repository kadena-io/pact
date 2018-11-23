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
import           Pact.Native               (enforceDef, enforceOneDef,
                                            formatDef, hashDef, ifDef,
                                            lengthDef, pactVersionDef,
                                            readDecimalDef, strToIntDef)
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

  ESimple SDecimal (Inj (DecArithOp op x y)) ->
    mkApp (arithOpToDef op) [ESimple SDecimal x, ESimple SDecimal y]

  ESimple SDecimal (Inj (DecUnaryArithOp op x)) ->
    mkApp (unaryArithOpToDef op) [ESimple SDecimal x]

  ESimple SInteger (Inj (IntArithOp op x y)) ->
    mkApp (arithOpToDef op) [ESimple SInteger x, ESimple SInteger y]

  ESimple SInteger (Inj (IntUnaryArithOp op x)) ->
    mkApp (unaryArithOpToDef op) [ESimple SInteger x]

  ESimple SInteger (Inj (RoundingLikeOp1 op x)) ->
    mkApp (roundingLikeOpToDef op) [ESimple SDecimal x]

  ESimple SDecimal (Inj (RoundingLikeOp2 op x y)) ->
    mkApp (roundingLikeOpToDef op) [ESimple SDecimal x, ESimple SInteger y]

  ESimple SDecimal (Inj (DecIntArithOp op x y)) ->
    mkApp (arithOpToDef op) [ESimple SDecimal x, ESimple SInteger y]

  ESimple SDecimal (Inj (IntDecArithOp op x y)) ->
    mkApp (arithOpToDef op) [ESimple SInteger x, ESimple SDecimal y]

  ESimple SInteger (Inj (ModOp x y)) ->
    mkApp modDef [ESimple SInteger x, ESimple SInteger y]

  ESimple SInteger (Inj (StrLength x)) ->
    mkApp lengthDef [ESimple SStr x]

  ESimple SStr (Inj (StrConcat x y)) ->
    mkApp addDef [ESimple SStr x, ESimple SStr y]

  ESimple SInteger (Inj (StrToInt s)) ->
    mkApp strToIntDef [ESimple SStr s]

  ESimple SInteger (Inj (StrToIntBase b s)) ->
    mkApp strToIntDef [ESimple SInteger b, ESimple SStr s]

  ESimple SBool (Inj (IntegerComparison op x y)) ->
    mkApp (comparisonOpToDef op) [ESimple SInteger x, ESimple SInteger y]

  ESimple SBool (Inj (DecimalComparison op x y)) ->
    mkApp (comparisonOpToDef op) [ESimple SDecimal x, ESimple SDecimal y]

  ESimple SBool (Inj (StringComparison op x y)) ->
    mkApp (comparisonOpToDef op) [ESimple SStr x, ESimple SStr y]

  ESimple SBool (Inj (BoolComparison op x y)) ->
    mkApp (comparisonOpToDef op) [ESimple SBool x, ESimple SBool y]

  ESimple SBool (Inj (TimeComparison op x y)) ->
    mkApp (comparisonOpToDef op) [ESimple STime x, ESimple STime y]

  ESimple STime (Inj (IntAddTime x y)) ->
    mkApp defAddTime [ESimple STime x, ESimple SInteger y]

  ESimple STime (Inj (DecAddTime x y)) ->
    mkApp defAddTime [ESimple STime x, ESimple SDecimal y]

  ESimple SBool (Inj (Logical op args)) ->
    mkApp (logicalOpToDef op) (ESimple SBool <$> args)

  ESimple SInteger     (CoreTerm (Lit x))
    -> pure $ TLiteral (LInteger x) dummyInfo
  ESimple SDecimal (CoreTerm (Lit x))
    -> pure $ TLiteral (LDecimal (toPact decimalIso x)) dummyInfo
  ESimple SStr     (TextLit x)
    -> pure $ TLiteral (LString x) dummyInfo
  ESimple SBool    (CoreTerm (Lit x))
    -> pure $ TLiteral (LBool x) dummyInfo
  ESimple STime    (CoreTerm (Lit x))
    -> pure $ TLiteral (LTime (toPact timeIso x)) dummyInfo

  ESimple SKeySet  (CoreTerm (Lit (KeySet x))) -> do
    keysets <- view (_1 . envKeysets)
    case keysets ^? ix (fromIntegral x) of
      Just (ks, _) -> pure $ Pact.TKeySet ks dummyInfo
      Nothing      -> error $ "no keysets found at index " ++ show x

  -- term-specific terms:
  ESimple SBool (Enforce _ x)
    -> mkApp enforceDef [ESimple SBool x, ESimple SStr $ CoreTerm $ Lit "(enforce)"]
  ESimple SBool (EnforceOne Left{})
    -> mkApp' enforceOneDef (ESimple SStr $ CoreTerm $ Lit "(enforce-one)") []

  ESimple SBool (EnforceOne (Right xs)) -> mkApp' enforceOneDef
    (ESimple SStr $ CoreTerm $ Lit "(enforce-one)")
    (ESimple SBool . snd <$> xs)

  -- ESimple SBool (KsAuthorized x)
  -- ESimple SBool (NameAuthorized x)

  ESimple SStr PactVersion -> mkApp pactVersionDef []
  ESimple SStr (Format template vals)
    -> mkApp' formatDef (ESimple SStr template) vals
  ESimple SStr (FormatTime x y)
    -> mkApp defFormatTime [ESimple SStr x, ESimple STime y]
  ESimple SStr (Hash x) -> mkApp hashDef [x]

  ESimple SKeySet (ReadKeySet x) -> mkApp readKeysetDef [ESimple SStr x]

  ESimple SDecimal (ReadDecimal x) -> mkApp readDecimalDef [ESimple SStr x]

  ESimple STime (ParseTime Nothing x) ->
    mkApp timeDef [ESimple SStr x]

  ESimple STime (ParseTime (Just x) y) ->
    mkApp parseTimeDef [ESimple SStr x, ESimple SStr y]

  -- ESimple ty (Sequence etm tm) -> do
  --   t1 <- toPactTm etm
  --   t2 <- toPactTm (ESimple ty tm)
  --   pure $ TList [t1, t2] (Pact.TyList Pact.TyAny) dummyInfo

--   ESimple ty (Let name _vid etm bodyTm) -> do
--     t1 <- toPactTm etm
--     t2 <- toPactTm (ESimple ty bodyTm)
--     pure $ TBinding [(Pact.Arg name undefined dummyInfo, t1)]
--       t2 undefined dummyInfo

  ESimple ty (IfThenElse t1 (_, t2) (_, t3)) ->
    mkApp ifDef [ESimple SBool t1, ESimple ty t2, ESimple ty t3]

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
reverseTranslateType :: SingTy k a -> Pact.Type b
reverseTranslateType = \case
  SBool    -> Pact.TyPrim Pact.TyBool
  SDecimal -> Pact.TyPrim Pact.TyDecimal
  SInteger -> Pact.TyPrim Pact.TyInteger
  SStr     -> Pact.TyPrim Pact.TyString
  STime    -> Pact.TyPrim Pact.TyTime
  SKeySet  -> Pact.TyPrim Pact.TyKeySet
  SAny     -> Pact.TyAny
  SList a  -> Pact.TyList $ reverseTranslateType a
  SObject  -> error "TODO"

fromPactVal :: EType -> Pact.Term Pact.Ref -> IO (Maybe ETerm)
fromPactVal (EType ty)  = runMaybeT . toAnalyze (reverseTranslateType ty)
fromPactVal EObjectTy{} = const (pure Nothing) -- TODO
