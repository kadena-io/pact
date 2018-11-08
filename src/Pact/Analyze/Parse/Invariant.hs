{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE ViewPatterns      #-}
module Pact.Analyze.Parse.Invariant (expToInvariant) where

import           Control.Applicative      ((<|>))
import           Control.Lens             ((^.))
import           Control.Monad.Reader     (ask)
import           Data.Foldable            (asum, find)
import qualified Data.Text                as T
import           Prelude                  hiding (exp)

import           Pact.Types.Lang          hiding (KeySet, KeySetName, SchemaVar,
                                           TKeySet, TableName, Type)
import qualified Pact.Types.Lang          as Pact
import           Pact.Types.Util          (tShow)

import           Pact.Analyze.Feature     hiding (Type, Var, ks, obj, str)
import           Pact.Analyze.Parse.Types
import           Pact.Analyze.Types
import           Pact.Analyze.Util

expToInvariant :: Type a -> Exp Info -> InvariantParse (Invariant a)
expToInvariant ty exp = case (ty, exp) of
  (_, EAtom' varName) -> do
    schemaTys <- ask
    case find (\(arg, _vid) -> arg ^. aName == varName) schemaTys of
      Just (Pact.Arg _name (TyPrim primTy) _info, vid) -> case (ty, primTy) of
        (TInt,     TyInteger) -> pure (CoreInvariant (Var vid varName))
        (TDecimal, TyDecimal) -> pure (CoreInvariant (Var vid varName))
        (TTime,    TyTime)    -> pure (CoreInvariant (Var vid varName))
        (TStr,     TyString)  -> pure (CoreInvariant (Var vid varName))
        (TBool,    TyBool)    -> pure (CoreInvariant (Var vid varName))
        (TKeySet,  TyKeySet)  -> pure (CoreInvariant (Var vid varName))
        (_,        TyValue)   -> throwErrorIn exp
          "Invariants can't constrain opaque values"
        (_,        _)         -> throwErrorIn exp $
          "found variable " <> varName <> " of type " <> tShow primTy <>
          " where " <> userShow ty <> " was expected"
      _ -> throwErrorT $ "couldn't find column named " <> varName

  (TDecimal, ELiteral' (LDecimal d)) -> pure (ILiteral (fromPact decimalIso d))
  (TInt, ELiteral' (LInteger i))     -> pure (ILiteral i)
  (TStr, ELiteral' (LString s))      -> pure (ILiteral (T.unpack s))
  (TTime, ELiteral' (LTime t))       -> pure (ILiteral (fromPact timeIso t))
  (TBool, ELiteral' (LBool b))       -> pure (ILiteral b)
  (_, ELiteral _)                    ->
    throwErrorIn exp "literal of unexpected type"

  (TInt, ParenList [EAtom' SStringLength, str])
    -> CoreInvariant . StrLength <$> expToInvariant TStr str
  (TStr, ParenList [EAtom' SStringConcatenation, a, b]) -> CoreInvariant ... StrConcat
    <$> expToInvariant TStr a <*> expToInvariant TStr b

  (TDecimal, ParenList [EAtom' (toOp arithOpP -> Just op), a, b]) -> asum
    [ Inj ... DecArithOp    op <$> expToInvariant TDecimal a <*> expToInvariant TDecimal b
    , Inj ... DecIntArithOp op <$> expToInvariant TDecimal a <*> expToInvariant TInt b
    , Inj ... IntDecArithOp op <$> expToInvariant TInt a     <*> expToInvariant TDecimal b
    ] <|> throwErrorIn exp "unexpected argument types"
  (TInt, ParenList [EAtom' (toOp arithOpP -> Just op), a, b])
    -> Inj ... IntArithOp op <$> expToInvariant TInt a <*> expToInvariant TInt b
  (TDecimal, ParenList [EAtom' (toOp unaryArithOpP -> Just op), a])
    -> Inj . DecUnaryArithOp op <$> expToInvariant TDecimal a
  (TInt, ParenList [EAtom' (toOp unaryArithOpP -> Just op), a])
    -> Inj . IntUnaryArithOp op <$> expToInvariant TInt a

  (TBool, ParenList [EAtom' op'@(toOp comparisonOpP -> Just op), a, b]) -> asum
    [ CoreInvariant ... IntegerComparison op
      <$> expToInvariant TInt a     <*> expToInvariant TInt b
    , CoreInvariant ... DecimalComparison op
      <$> expToInvariant TDecimal a <*> expToInvariant TDecimal b
    , CoreInvariant ... TimeComparison op
      <$> expToInvariant TTime a    <*> expToInvariant TTime b
    , CoreInvariant ... BoolComparison op
      <$> expToInvariant TBool a    <*> expToInvariant TBool b
    , CoreInvariant ... StringComparison op
      <$> expToInvariant TStr a     <*> expToInvariant TStr b
    , case toOp eqNeqP op' of
      Just eqNeq -> CoreInvariant ... KeySetEqNeq eqNeq
        <$> expToInvariant TKeySet a
        <*> expToInvariant TKeySet b
      Nothing -> throwErrorIn exp $
        op' <> " is an invalid operation for keysets (only " <> SEquality <>
        " or " <> SInequality <> " allowed)"
    ] <|> throwErrorIn exp "unexpected argument types"

  (TBool, ParenList (EAtom' op:args))
    | Just op' <- toOp logicalOpP op -> do
    operands' <- traverse (expToInvariant TBool) args
    case (op', operands') of
      (AndOp, [a, b]) -> pure (ILogicalOp AndOp [a, b])
      (OrOp, [a, b])  -> pure (ILogicalOp OrOp [a, b])
      (NotOp, [a])    -> pure (ILogicalOp NotOp [a])
      _ -> throwErrorIn exp $ "logical op with wrong number of args: " <> op

  (_, EAtom {})        -> throwErrorIn exp "illegal invariant form"
  (_, EList {})        -> throwErrorIn exp "illegal invariant form"
  (_, BraceList {})    -> throwErrorIn exp "illegal invariant form"
  (_, ESeparator {})   -> throwErrorIn exp "illegal invariant form"
