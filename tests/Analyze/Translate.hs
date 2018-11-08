{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Analyze.Translate where

import           Control.Lens               hiding (op, (...))
import           Control.Monad.Morph        (generalize, hoist)
import           Control.Monad.Reader       (ReaderT (runReaderT))
import           Control.Monad.Trans.Class  (MonadTrans (lift))
import           Control.Monad.Trans.Maybe  (MaybeT (MaybeT, runMaybeT),
                                             exceptToMaybeT)
import qualified Data.Text                  as T

import           Pact.Analyze.Translate     (translateNodeNoGraph)
import           Pact.Analyze.Types         hiding (Object, Term)
import           Pact.Analyze.Util          (dummyInfo)

import           Pact.Eval                  (liftTerm)
import           Pact.Native                (enforceDef, enforceOneDef,
                                             formatDef, hashDef, ifDef,
                                             lengthDef, pactVersionDef,
                                             readDecimalDef)
import           Pact.Native.Keysets
import           Pact.Native.Ops
import           Pact.Native.Time
import           Pact.Typechecker           (typecheckTopLevel)
import           Pact.Types.Exp             (Literal (..))
import           Pact.Types.Native          (NativeDef)
import           Pact.Types.Term            (Meta (Meta),
                                             Term (TApp, TConst, TLiteral))
import qualified Pact.Types.Term            as Pact
import qualified Pact.Types.Type            as Pact
import qualified Pact.Types.Typecheck       as Pact

import           Analyze.Gen


toPactTm :: ETerm -> ReaderT (GenEnv, GenState) Maybe (Pact.Term Pact.Ref)
toPactTm = \case
  -- core terms:

  ESimple TDecimal (Inj (DecArithOp op x y)) ->
    mkApp (arithOpToDef op) [ESimple TDecimal x, ESimple TDecimal y]

  ESimple TDecimal (Inj (DecUnaryArithOp op x)) ->
    mkApp (unaryArithOpToDef op) [ESimple TDecimal x]

  ESimple TInt (Inj (IntArithOp op x y)) ->
    mkApp (arithOpToDef op) [ESimple TInt x, ESimple TInt y]

  ESimple TInt (Inj (IntUnaryArithOp op x)) ->
    mkApp (unaryArithOpToDef op) [ESimple TInt x]

  ESimple TInt (Inj (RoundingLikeOp1 op x)) ->
    mkApp (roundingLikeOpToDef op) [ESimple TDecimal x]

  ESimple TDecimal (Inj (RoundingLikeOp2 op x y)) ->
    mkApp (roundingLikeOpToDef op) [ESimple TDecimal x, ESimple TInt y]

  ESimple TDecimal (Inj (DecIntArithOp op x y)) ->
    mkApp (arithOpToDef op) [ESimple TDecimal x, ESimple TInt y]

  ESimple TDecimal (Inj (IntDecArithOp op x y)) ->
    mkApp (arithOpToDef op) [ESimple TInt x, ESimple TDecimal y]

  ESimple TInt (Inj (ModOp x y)) ->
    mkApp modDef [ESimple TInt x, ESimple TInt y]

  ESimple TInt (Inj (StrLength x)) ->
    mkApp lengthDef [ESimple TStr x]

  ESimple TStr (Inj (StrConcat x y)) ->
    mkApp addDef [ESimple TStr x, ESimple TStr y]

  ESimple TBool (Inj (IntegerComparison op x y)) ->
    mkApp (comparisonOpToDef op) [ESimple TInt x, ESimple TInt y]

  ESimple TBool (Inj (DecimalComparison op x y)) ->
    mkApp (comparisonOpToDef op) [ESimple TDecimal x, ESimple TDecimal y]

  ESimple TBool (Inj (StringComparison op x y)) ->
    mkApp (comparisonOpToDef op) [ESimple TStr x, ESimple TStr y]

  ESimple TBool (Inj (BoolComparison op x y)) ->
    mkApp (comparisonOpToDef op) [ESimple TBool x, ESimple TBool y]

  ESimple TBool (Inj (TimeComparison op x y)) ->
    mkApp (comparisonOpToDef op) [ESimple TTime x, ESimple TTime y]

  ESimple TTime (Inj (IntAddTime x y)) ->
    mkApp defAddTime [ESimple TTime x, ESimple TInt y]

  ESimple TTime (Inj (DecAddTime x y)) ->
    mkApp defAddTime [ESimple TTime x, ESimple TDecimal y]

  ESimple TBool (Inj (Logical op args)) ->
    mkApp (logicalOpToDef op) (ESimple TBool <$> args)

  ESimple TInt     (CoreTerm (Lit x))
    -> pure $ TLiteral (LInteger x) dummyInfo
  ESimple TDecimal (CoreTerm (Lit x))
    -> pure $ TLiteral (LDecimal (toPact decimalIso x)) dummyInfo
  ESimple TStr     (CoreTerm (Lit x))
    -> pure $ TLiteral (LString (T.pack x)) dummyInfo
  ESimple TBool    (CoreTerm (Lit x))
    -> pure $ TLiteral (LBool x) dummyInfo
  ESimple TTime    (CoreTerm (Lit x))
    -> pure $ TLiteral (LTime (toPact timeIso x)) dummyInfo

  ESimple TKeySet  (CoreTerm (Lit (KeySet x))) -> do
    keysets <- view (_1 . envKeysets)
    case keysets ^? ix (fromIntegral x) of
      Just (ks, _) -> pure $ Pact.TKeySet ks dummyInfo
      Nothing      -> error $ "no keysets found at index " ++ show x

  -- term-specific terms:
  ESimple TBool (Enforce _ x)
    -> mkApp enforceDef [ESimple TBool x, ESimple TStr $ CoreTerm $ Lit "(enforce)"]
  ESimple TBool (EnforceOne Left{})
    -> mkApp' enforceOneDef (ESimple TStr $ CoreTerm $ Lit "(enforce-one)") []

  ESimple TBool (EnforceOne (Right xs)) -> mkApp' enforceOneDef
    (ESimple TStr $ CoreTerm $ Lit "(enforce-one)")
    (ESimple TBool . snd <$> xs)

  -- ESimple TBool (KsAuthorized x)
  -- ESimple TBool (NameAuthorized x)

  ESimple TStr PactVersion -> mkApp pactVersionDef []
  ESimple TStr (Format template vals)
    -> mkApp' formatDef (ESimple TStr template) vals
  ESimple TStr (FormatTime x y)
    -> mkApp defFormatTime [ESimple TStr x, ESimple TTime y]
  ESimple TStr (Hash x) -> mkApp hashDef [x]

  ESimple TKeySet (ReadKeySet x) -> mkApp readKeysetDef [ESimple TStr x]

  ESimple TDecimal (ReadDecimal x) -> mkApp readDecimalDef [ESimple TStr x]

  ESimple TTime (ParseTime Nothing x) ->
    mkApp timeDef [ESimple TStr x]

  ESimple TTime (ParseTime (Just x) y) ->
    mkApp parseTimeDef [ESimple TStr x, ESimple TStr y]

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
    mkApp ifDef [ESimple TBool t1, ESimple ty t2, ESimple ty t3]

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
reverseTranslateType :: Type a -> Pact.Type b
reverseTranslateType = \case
  TBool    -> Pact.TyPrim Pact.TyBool
  TDecimal -> Pact.TyPrim Pact.TyDecimal
  TInt     -> Pact.TyPrim Pact.TyInteger
  TStr     -> Pact.TyPrim Pact.TyString
  TTime    -> Pact.TyPrim Pact.TyTime
  TKeySet  -> Pact.TyPrim Pact.TyKeySet
  TAny     -> Pact.TyAny

fromPactVal :: EType -> Pact.Term Pact.Ref -> IO (Maybe ETerm)
fromPactVal (EType ty)  = runMaybeT . toAnalyze (reverseTranslateType ty)
fromPactVal EObjectTy{} = const (pure Nothing) -- TODO
