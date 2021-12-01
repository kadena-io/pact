{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

#if __GLASGOW_HASKELL__ >= 810
{-# options_ghc -fmax-pmcheck-models=100000000 #-}
#else
{-# options_ghc -fmax-pmcheck-iterations=100000000 #-}
#endif

module Analyze.Translate where

import           Control.Lens              hiding (op, (...))
import           Control.Monad.Morph       (generalize, hoist)
import           Control.Monad.Reader      (ReaderT (runReaderT))
import           Control.Monad.Trans.Class (MonadTrans (lift))
import           Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT),
                                            exceptToMaybeT)
import qualified Data.Vector               as V

import           Pact.Analyze.Translate    (translateNodeNoGraph)
import           Pact.Analyze.Types        hiding (Object, Term)
import           Pact.Analyze.Util         (dummyInfo)

import           Pact.Eval                 (liftTerm)
import           Pact.Native               (dropDef, enforceDef, enforceOneDef,
                                            formatDef, hashDef, ifDef,
                                            lengthDef, makeListDef,
                                            pactVersionDef, readDecimalDef,
                                            readIntegerDef, readStringDef,
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
import           Pact.Types.Term           (Meta (Meta), App(..),
                                            Term (TApp, TConst, TLiteral))
import qualified Pact.Types.Term           as Pact
import qualified Pact.Types.Type           as Pact
import qualified Pact.Types.Typecheck      as Pact

import           Analyze.Gen


toPactTm :: ETerm -> ReaderT (GenEnv, GenState) Maybe (Pact.Term Pact.Ref)
toPactTm = \case
  -- core terms:

  Some ty (CoreTerm tm) -> case tm of

    Numerical numerical -> case numerical of
      DecArithOp op x y ->
        mkApp (arithOpToDef op) [Some SDecimal x, Some SDecimal y]

      DecUnaryArithOp op x ->
        mkApp (unaryArithOpToDef op) [Some SDecimal x]

      IntArithOp op x y ->
        mkApp (arithOpToDef op) [Some SInteger x, Some SInteger y]

      IntUnaryArithOp op x ->
        mkApp (unaryArithOpToDef op) [Some SInteger x]

      RoundingLikeOp1 op x ->
        mkApp (roundingLikeOpToDef op) [Some SDecimal x]

      RoundingLikeOp2 op x y ->
        mkApp (roundingLikeOpToDef op) [Some SDecimal x, Some SInteger y]

      DecIntArithOp op x y ->
        mkApp (arithOpToDef op) [Some SDecimal x, Some SInteger y]

      IntDecArithOp op x y ->
        mkApp (arithOpToDef op) [Some SInteger x, Some SDecimal y]

      ModOp x y ->
        mkApp modDef [Some SInteger x, Some SInteger y]

      BitwiseOp op args ->
        mkApp (bitwiseOpToDef op) (Some SInteger <$> args)

    StrLength x ->
      mkApp lengthDef [Some SStr x]

    StrConcat x y ->
      mkApp addDef [Some SStr x, Some SStr y]

    StrToInt s ->
      mkApp strToIntDef [Some SStr s]

    StrToIntBase b s ->
      mkApp strToIntDef [Some SInteger b, Some SStr s]

    StrTake i l2 ->
      mkApp takeDef [ Some SInteger i, Some SStr l2 ]

    StrDrop i l2 ->
      mkApp dropDef [ Some SInteger i, Some SStr l2 ]

    Comparison ty' op x y ->
      mkApp (comparisonOpToDef op) [Some ty' x, Some ty' y]

    IntAddTime x y ->
      mkApp defAddTime [Some STime x, Some SInteger y]

    DecAddTime x y ->
      mkApp defAddTime [Some STime x, Some SDecimal y]

    Logical op args ->
      mkApp (logicalOpToDef op) (Some SBool <$> args)

    Lit x -> case ty of
      SInteger -> pure $ TLiteral (LInteger x) dummyInfo
      SDecimal -> pure $ TLiteral (LDecimal (toPact decimalIso x)) dummyInfo
      SStr     -> pure $ TLiteral (LString (strToText x)) dummyInfo
      SBool    -> pure $ TLiteral (LBool x) dummyInfo
      STime    -> pure $ TLiteral (LTime (toPact timeIso x)) dummyInfo
      SGuard   -> case x of
        Guard x' -> do
          keysets <- view (_1 . envKeysets)
          case keysets ^? ix (fromIntegral x') of
            Just (ks, _) -> pure $ Pact.TGuard (Pact.GKeySet ks) dummyInfo
            Nothing      -> error $ "no keysets found at index " ++ show x'
      _ -> error "unexpected lit type"

    ListEqNeq ty' op l1 l2 ->
      mkApp (eqNeqOpToDef op) [ Some (SList ty') l1, Some (SList ty') l2 ]

    ListReverse _ lst ->
      mkApp reverseDef [ Some ty lst ]

    ListSort _ lst ->
      mkApp sortDef [ Some ty lst ]

    ListConcat _ l1 l2 ->
      mkApp addDef [ Some ty l1, Some ty l2 ]

    ListDrop _ i l2 ->
      mkApp dropDef [ Some SInteger i, Some ty l2 ]

    ListTake _ i l2 ->
      mkApp takeDef [ Some SInteger i, Some ty l2 ]

    MakeList _ i a -> case ty of
      SList ty' -> mkApp makeListDef [ Some SInteger i, Some ty' a ]

    _ -> withSing ty $ error $ "TODO: handle core term: " ++ show (CoreTerm tm)

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

  Some SGuard (ReadKeySet x) -> mkApp readKeysetDef [Some SStr x]

  Some SDecimal (ReadDecimal x) -> mkApp readDecimalDef [Some SStr x]

  Some SInteger (ReadInteger x) -> mkApp readIntegerDef [Some SStr x]

  Some SStr (ReadString x) -> mkApp readStringDef [Some SStr x]

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

  tm -> error $ "TODO: toPactTm " ++ show tm

  where
    mkApp :: NativeDef -> [ETerm]
      -> ReaderT (GenEnv, GenState) Maybe (Pact.Term Pact.Ref)
    mkApp (_, defTm) args = do
      args' <- traverse toPactTm args
      pure $ TApp (App (liftTerm defTm) args' dummyInfo) dummyInfo

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
      pure $ (`TApp` dummyInfo) $ App (liftTerm defTm)
        [arg', Pact.TList (V.fromList argList') (Pact.TyList Pact.TyAny) dummyInfo]
        dummyInfo

    arithOpToDef :: ArithOp -> NativeDef
    arithOpToDef = \case
      Add -> addDef
      Sub -> subDef
      Mul -> mulDef
      Div -> divDef
      Pow -> powDef
      Log -> logDef

    bitwiseOpToDef :: BitwiseOp -> NativeDef
    bitwiseOpToDef = \case
      BitwiseAnd -> bitAndDef
      BitwiseOr  -> bitOrDef
      Xor        -> xorDef
      Shift      -> shiftDef
      Complement -> complementDef

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

toAnalyze
  :: Pact.Type (Pact.Term Pact.Ref) -> Pact.Term Pact.Ref -> MaybeT IO ETerm
toAnalyze ty tm = do
  let cnst = TConst
        (Pact.Arg "tm" ty dummyInfo)
        (Just "module")
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
  SGuard    -> Pact.TyPrim $ Pact.TyGuard Nothing
  SAny      -> Pact.TyAny
  SList a   -> Pact.TyList $ reverseTranslateType a
  -- SObject needs to hold a type name or something
  SObjectUnsafe _ -> error "this reverse-translation is currently impossible"

fromPactVal :: EType -> Pact.Term Pact.Ref -> IO (Maybe ETerm)
fromPactVal (EType ty)  = runMaybeT . toAnalyze (reverseTranslateType ty)
