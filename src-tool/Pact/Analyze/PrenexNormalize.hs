{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}

-- | Prenex normalization pass, to float all quantifiers to the "outside" of a
-- 'Prop' expression. This is necessary because SBV assumes that all inputs are
-- already in prenex normal form:
-- https://hackage.haskell.org/package/sbv-8.0/docs/Data-SBV.html#g:34.
module Pact.Analyze.PrenexNormalize
  ( prenexConvert
  ) where

import           Data.Bifunctor     (bimap)

import           Pact.Analyze.Types
import           Pact.Analyze.Util

-- Note: pattern match shortcomings
--
-- Unfortunately, the version of GHC we're using doesn't support the COMPLETE
-- pragma to mark a set of pattern synonyms as complete. Because of this, we
-- forgo pattern synonyms on the left-hand-side, resulting in some unfortunate
-- patterns, like `CoreProp (Numerical (IntArithOp op a b))` (as compared to
-- `PIntArithOp op a b`).
--
-- Also, in several places we need to mark a `Numerical` pattern as vacuous,
-- for reasons that elude me.

singFloat :: SingTy a -> Prop a -> ([Quantifier], Prop a)
singFloat ty p = case p of
  CoreProp Var{}                  -> ([], p)
  CoreProp Lit{}                  -> ([], p)
  CoreProp Sym{}                  -> ([], p)

  -- prop specific
  PropSpecific Result             -> ([], p)
  PropSpecific Abort              -> ([], p)
  PropSpecific Success            -> ([], p)
  PropSpecific GovPasses          -> ([], p)
  PropSpecific TableWrite{}       -> ([], p)
  PropSpecific TableRead{}        -> ([], p)
  PropSpecific ColumnWritten{}    -> ([], p)
  PropSpecific ColumnRead{}       -> ([], p)
  PropSpecific GuardPassed{}      -> ([], p)
  PropSpecific RowEnforced{}      -> ([], p)
  PropSpecific IntColumnDelta{}   -> ([], p)
  PropSpecific DecColumnDelta{}   -> ([], p)
  PropSpecific (PropRead ty' ba tn pRk)
    -> PropSpecific . PropRead ty' ba tn <$> float pRk

  -- functions
  CoreProp (Identity tya a) -> CoreProp . Identity tya <$> singFloat tya a;
  CoreProp (Constantly tyb a b) -> CoreProp <$>
    (Constantly tyb <$> singFloat ty a <*> singFloat tyb b)
  CoreProp (Compose tya tyb tyc a b c) -> CoreProp <$> (Compose tya tyb tyc
    <$> singFloat     tya a
    <*> singFloatOpen tyb b
    <*> singFloatOpen tyc c)

  -- int
  CoreProp (Numerical (IntArithOp op a b))
    -> PNumerical ... IntArithOp      op <$> float a <*> float b
  CoreProp (Numerical (IntUnaryArithOp op a))
    -> PNumerical .   IntUnaryArithOp op <$> float a
  CoreProp (Numerical (ModOp a b))
    -> PNumerical ... ModOp              <$> float a <*> float b
  CoreProp (Numerical (RoundingLikeOp1 op a))
    -> PNumerical . RoundingLikeOp1 op   <$> float a
  CoreProp (StrLength pStr)
    -> PStrLength <$> float pStr
  CoreProp (StrToInt s)
    -> CoreProp . StrToInt <$> float s
  CoreProp (StrToIntBase b s)
    -> CoreProp ... StrToIntBase <$> float b <*> float s
  PropSpecific (IntCellDelta tn cn a)
    -> PropSpecific . IntCellDelta tn cn <$> float a
  PropSpecific (RowWriteCount tn pRk)
    -> PropSpecific . RowWriteCount tn   <$> float pRk
  PropSpecific (RowReadCount tn pRk)
    -> PropSpecific . RowReadCount tn    <$> float pRk
  CoreProp (Numerical (BitwiseOp op args))
    -> PNumerical . BitwiseOp op         <$> traverse (singFloat ty) args

  -- decimal
  CoreProp (Numerical (DecArithOp op a b))
    -> PNumerical ... DecArithOp      op <$> float a <*> float b
  CoreProp (Numerical (DecUnaryArithOp op a))
    -> PNumerical .   DecUnaryArithOp op <$> float a
  CoreProp (Numerical (DecIntArithOp op a b))
    -> PNumerical ... DecIntArithOp   op <$> float a <*> float b
  CoreProp (Numerical (IntDecArithOp op a b))
    -> PNumerical ... IntDecArithOp   op <$> float a <*> float b
  CoreProp (Numerical (RoundingLikeOp2 op a b))
    -> PNumerical ... RoundingLikeOp2 op <$> float a <*> float b
  PropSpecific (DecCellDelta tn cn a)
    -> PropSpecific . DecCellDelta tn cn  <$> float a

  -- str
  CoreProp (StrTake s1 s2) -> PStrTake <$> float s1 <*> float s2
  CoreProp (StrDrop s1 s2) -> PStrDrop <$> float s1 <*> float s2
  CoreProp (StrConcat s1 s2) -> PStrConcat <$> float s1 <*> float s2
  CoreProp (Typeof tya a) -> CoreProp . Typeof tya <$> singFloat tya a

  -- time
  CoreProp (IntAddTime time int) -> PIntAddTime <$> float time <*> float int
  CoreProp (DecAddTime time dec) -> PDecAddTime <$> float time <*> float dec

  -- bool
  -- - quantification
  PropSpecific (Forall uid name ty' prop) ->
    let (qs, prop') = float prop
    in (Forall' uid name ty':qs, prop')
  PropSpecific (Exists uid name ty' prop) ->
    let (qs, prop') = float prop
    in (Exists' uid name ty':qs, prop')

  -- - comparison
  CoreProp (Comparison ty' op a b)
    -> CoreProp ... Comparison ty' op <$> singFloat ty' a <*> singFloat ty' b
  CoreProp (ObjectEqNeq ty1 ty2 op a b)
    -> CoreProp <$> (ObjectEqNeq ty1 ty2 op <$> singFloat ty1 a <*> singFloat ty2 b)
  CoreProp (GuardEqNeq op a b)
    -> CoreProp <$> (GuardEqNeq op <$> float a <*> float b)
  CoreProp (ListEqNeq ty' op a b) ->
    CoreProp <$> (ListEqNeq ty' op <$> singFloat (SList ty') a <*> singFloat (SList ty') b)
  CoreProp (StrContains needle haystack) -> CoreProp <$>
    (StrContains <$> float needle <*> float haystack)
  CoreProp (ListContains ty' needle haystack) ->
    CoreProp <$> (ListContains ty' <$> singFloat ty' needle <*> singFloat (SList ty') haystack)

  -- - logical
  CoreProp (Logical AndOp [a, b]) -> PAnd <$> float a <*> float b
  CoreProp (Logical OrOp  [a, b]) -> POr  <$> float a <*> float b
  CoreProp (Logical NotOp [a]) -> bimap (fmap flipQuantifier) PNot (float a)
  CoreProp (Logical _ _) -> error ("ill-defined logical op: " ++ showTm p)
  CoreProp (AndQ ty' f g a) -> CoreProp <$>
    (AndQ ty' <$> floatOpen f <*> floatOpen g <*> singFloat ty' a)
  CoreProp (OrQ ty' f g a) -> CoreProp <$>
    (OrQ ty' <$> floatOpen f <*> floatOpen g <*> singFloat ty' a)

  PropSpecific (RowRead  tn pRk)  -> PropSpecific . RowRead   tn <$> float pRk
  PropSpecific (RowWrite tn pRk)  -> PropSpecific . RowWrite  tn <$> float pRk
  PropSpecific (RowExists tn pRk beforeAfter)
    -> PropSpecific ... RowExists tn <$> float pRk <*> pure beforeAfter

  -- lists
  CoreProp (ListLength ty' pLst)
    -> CoreProp . ListLength ty' <$> singFloat (SList ty') pLst
  CoreProp (ListAt ty' a b) -> CoreProp <$>
    (ListAt ty' <$> float a <*> singFloat (SList ty') b)
  CoreProp (ListReverse ty' lst)
    -> CoreProp . ListReverse ty' <$> singFloat (SList ty') lst
  CoreProp (ListSort ty' lst)
    -> CoreProp . ListSort ty' <$> singFloat (SList ty') lst
  CoreProp (ListDrop ty' a b)
    -> CoreProp <$> (ListDrop ty' <$> float a <*> singFloat (SList ty') b)
  CoreProp (ListTake ty' a b)
    -> CoreProp <$> (ListTake ty' <$> float a <*> singFloat (SList ty') b)
  CoreProp (ListConcat ty' l1 l2) -> CoreProp <$>
    (ListConcat ty' <$> singFloat (SList ty') l1 <*> singFloat (SList ty') l2)
  CoreProp (MakeList ty' a b)
    -> CoreProp <$> (MakeList ty' <$> float a <*> singFloat ty' b)
  CoreProp (LiteralList ty' as)
    -> CoreProp <$> (LiteralList ty' <$> traverse (singFloat ty') as)
  CoreProp (ListMap tya tyb b as) -> CoreProp <$>
    (ListMap tya tyb <$> singFloatOpen tyb b <*> singFloat (SList tya) as)
  CoreProp (ListFilter ty' a b)
    -> CoreProp <$> (ListFilter ty' <$> floatOpen a <*> singFloat (SList ty') b)
  CoreProp (ListFold tya tyb (Open v1 nm1 a) b c) -> do
    a' <- singFloatOpen tya a
    b' <- singFloat tya b
    c' <- singFloat (SList tyb) c
    pure $ CoreProp $ ListFold tya tyb (Open v1 nm1 a') b' c'

  -- objects
  CoreProp (ObjAt ty' str obj) -> PObjAt ty' <$> float str <*> singFloat ty' obj
  CoreProp (ObjContains ty' a b) -> CoreProp <$>
    (ObjContains ty' <$> float a <*> singFloat ty' b)
  CoreProp (ObjLength ty' obj) -> CoreProp <$>
    (ObjLength ty' <$> singFloat ty' obj)
  CoreProp (ObjDrop ty' keys obj) -> CoreProp <$>
    (ObjDrop ty' <$> float keys <*> singFloat ty' obj)
  CoreProp (ObjTake ty' keys obj) -> CoreProp <$>
    (ObjTake ty' <$> float keys <*> singFloat ty' obj)
  CoreProp LiteralObject{} -> ([], p)
  CoreProp ObjMerge{}      -> ([], p)
  CoreProp (Where objty tya a b c) -> CoreProp <$>
    (Where objty tya <$> float a <*> floatOpen b <*> singFloat objty c)

  where

    floatOpen :: SingI b => Open a Prop b -> ([Quantifier], Open a Prop b)
    floatOpen = singFloatOpen sing

    singFloatOpen :: SingTy b -> Open a Prop b -> ([Quantifier], Open a Prop b)
    singFloatOpen ty' (Open v nm a) = Open v nm <$> singFloat ty' a

    float :: SingI a => Prop a -> ([Quantifier], Prop a)
    float = singFloat sing

    flipQuantifier :: Quantifier -> Quantifier
    flipQuantifier = \case
      Forall' uid name ty' -> Exists' uid name ty'
      Exists' uid name ty' -> Forall' uid name ty'

reassembleFloated :: [Quantifier] -> Prop 'TyBool -> Prop 'TyBool
reassembleFloated qs prop =
  let mkQuantifiedProp q acc = case q of
        Forall' uid name ty -> PropSpecific (Forall uid name ty acc)
        Exists' uid name ty -> PropSpecific (Exists uid name ty acc)
  in foldr mkQuantifiedProp prop qs

-- We first use @singFloat SBool@ to remove all quantifiers from the @Prop@
-- (modifying them as necessary, then put them back in place on the outside of
-- the syntax tree.
--
-- The only interesting cases are those for @Forall@, @Exists@, and @PNot@. In
-- the first two cases, we capture the quantifier to singFloat it up. In the
-- @PNot@ case, we flip all the quantifiers found inside the @PNot@ as we lift
-- them over it.
prenexConvert :: Prop 'TyBool -> Prop 'TyBool
prenexConvert = uncurry reassembleFloated . singFloat SBool
