{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE Rank2Types            #-}
{-# options_ghc -fno-warn-orphans #-}

module Pact.Analyze.PrenexNormalize (prenexConvert) where

import           Data.Bifunctor     (bimap)
import           Prelude            hiding (Float)

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

float :: SingTy a -> Prop a -> ([Quantifier], Prop a)
float ty p = case p of
  CoreProp Var{}                  -> ([], p)
  CoreProp Lit{}                  -> ([], p)
  CoreProp Sym{}                  -> ([], p)

  -- prop specific
  PropSpecific Result             -> ([], p)
  PropSpecific Abort              -> ([], p)
  PropSpecific Success            -> ([], p)
  PropSpecific TableWrite{}       -> ([], p)
  PropSpecific TableRead{}        -> ([], p)
  PropSpecific ColumnWritten{}    -> ([], p)
  PropSpecific ColumnRead{}       -> ([], p)
  PropSpecific KsNameAuthorized{} -> ([], p)
  PropSpecific RowEnforced{}      -> ([], p)
  PropSpecific IntColumnDelta{}   -> ([], p)
  PropSpecific DecColumnDelta{}   -> ([], p)
  PropSpecific (PropRead ty' schema ba tn pRk)
    -> PropSpecific . PropRead ty' schema ba tn <$> singFloat pRk

  -- functions
  CoreProp (Identity tya a) -> CoreProp . Identity tya <$> float tya a;
  CoreProp (Constantly tyb a b) -> CoreProp <$>
    (Constantly tyb <$> float ty a <*> float tyb b)
  CoreProp (Compose tya tyb tyc a b c) -> CoreProp <$> (Compose tya tyb tyc
    <$> float     tya a
    <*> floatOpen tyb b
    <*> floatOpen tyc c)

  -- int
  CoreProp (Numerical (IntArithOp op a b))
    -> PNumerical ... IntArithOp      op <$> singFloat a <*> singFloat b
  CoreProp (Numerical (IntUnaryArithOp op a))
    -> PNumerical .   IntUnaryArithOp op <$> singFloat a
  CoreProp (Numerical (ModOp a b))
    -> PNumerical ... ModOp              <$> singFloat a <*> singFloat b
  CoreProp (Numerical (RoundingLikeOp1 op a))
    -> PNumerical . RoundingLikeOp1 op   <$> singFloat a
  CoreProp (StrLength pStr)
    -> PStrLength <$> singFloat pStr
  CoreProp (StrToInt s)
    -> CoreProp . StrToInt <$> singFloat s
  CoreProp (StrToIntBase b s)
    -> CoreProp ... StrToIntBase <$> singFloat b <*> singFloat s
  PropSpecific (IntCellDelta tn cn a)
    -> PropSpecific . IntCellDelta tn cn <$> singFloat a
  PropSpecific (RowWriteCount tn pRk)
    -> PropSpecific . RowWriteCount tn   <$> singFloat pRk
  PropSpecific (RowReadCount tn pRk)
    -> PropSpecific . RowReadCount tn    <$> singFloat pRk

  -- decimal
  CoreProp (Numerical (DecArithOp op a b))
    -> PNumerical ... DecArithOp      op <$> singFloat a <*> singFloat b
  CoreProp (Numerical (DecUnaryArithOp op a))
    -> PNumerical .   DecUnaryArithOp op <$> singFloat a
  CoreProp (Numerical (DecIntArithOp op a b))
    -> PNumerical ... DecIntArithOp   op <$> singFloat a <*> singFloat b
  CoreProp (Numerical (IntDecArithOp op a b))
    -> PNumerical ... IntDecArithOp   op <$> singFloat a <*> singFloat b
  CoreProp (Numerical (RoundingLikeOp2 op a b))
    -> PNumerical ... RoundingLikeOp2 op <$> singFloat a <*> singFloat b
  PropSpecific (DecCellDelta tn cn a)
    -> PropSpecific . DecCellDelta tn cn  <$> singFloat a

  -- str
  CoreProp (StrConcat s1 s2) -> PStrConcat <$> singFloat s1 <*> singFloat s2
  CoreProp (Typeof tya a) -> CoreProp . Typeof tya <$> float tya a

  -- time
  CoreProp (IntAddTime time int) -> PIntAddTime <$> singFloat time <*> singFloat int
  CoreProp (DecAddTime time dec) -> PDecAddTime <$> singFloat time <*> singFloat dec

  -- bool
  -- - quantification
  PropSpecific (Forall uid name ty' prop) ->
    let (qs, prop') = singFloat prop
    in (Forall' uid name ty':qs, prop')
  PropSpecific (Exists uid name ty' prop) ->
    let (qs, prop') = singFloat prop
    in (Exists' uid name ty':qs, prop')

  -- - comparison
  CoreProp (Comparison ty' op a b)
    -> CoreProp ... Comparison ty' op <$> float ty' a <*> float ty' b
  CoreProp (ObjectEqNeq ty' op a b) -> PObjectEqNeq ty' op <$> float ty' a <*> float ty' b
  CoreProp (ListEqNeq ty' op a b) ->
    CoreProp <$> (ListEqNeq ty' op <$> float (SList ty') a <*> float (SList ty') b)
  CoreProp (StrContains needle haystack) -> CoreProp <$>
    (StrContains <$> singFloat needle <*> singFloat haystack)
  CoreProp (ListContains ty' needle haystack) ->
    CoreProp <$> (ListContains ty' <$> float ty' needle <*> float (SList ty') haystack)

  -- - logical
  CoreProp (Logical AndOp [a, b]) -> PAnd <$> singFloat a <*> singFloat b
  CoreProp (Logical OrOp [a, b]) -> POr  <$> singFloat a <*> singFloat b
  CoreProp (Logical NotOp [a]) -> bimap (fmap flipQuantifier) PNot (singFloat a)
  CoreProp (Logical _ _) -> error ("ill-defined logical op: " ++ showTm p)
  CoreProp (AndQ ty' f g a) -> CoreProp <$>
    (AndQ ty' <$> singFloatOpen f <*> singFloatOpen g <*> float ty' a)
  CoreProp (OrQ ty' f g a) -> CoreProp <$>
    (OrQ ty' <$> singFloatOpen f <*> singFloatOpen g <*> float ty' a)

  PropSpecific (RowRead  tn pRk)  -> PropSpecific . RowRead   tn <$> singFloat pRk
  PropSpecific (RowWrite tn pRk)  -> PropSpecific . RowWrite  tn <$> singFloat pRk
  PropSpecific (RowExists tn pRk beforeAfter)
    -> PropSpecific ... RowExists tn <$> singFloat pRk <*> pure beforeAfter

  -- lists
  CoreProp (ListLength ty' pLst)
    -> CoreProp . ListLength ty' <$> float (SList ty') pLst
  CoreProp (ListAt ty' a b) -> CoreProp <$>
    (ListAt ty' <$> singFloat a <*> float (SList ty') b)
  CoreProp (ListReverse ty' lst)  -> CoreProp . ListReverse ty' <$> float (SList ty') lst
  CoreProp (ListSort ty' lst)     -> CoreProp . ListSort ty' <$> float (SList ty') lst
  CoreProp (ListDrop ty' a b)
    -> CoreProp <$> (ListDrop ty' <$> singFloat a <*> float (SList ty') b)
  CoreProp (ListTake ty' a b)
    -> CoreProp <$> (ListTake ty' <$> singFloat a <*> float (SList ty') b)
  CoreProp (ListConcat ty' l1 l2)
    -> CoreProp <$> (ListConcat ty' <$> float (SList ty') l1 <*> float (SList ty') l2)
  CoreProp (MakeList ty' a b)
    -> CoreProp <$> (MakeList ty' <$> singFloat a <*> float ty' b)
  CoreProp (LiteralList ty' as)
    -> CoreProp <$> (LiteralList ty' <$> traverse (float ty') as)
  CoreProp (ListMap tya tyb b as) ->
       CoreProp <$> (ListMap tya tyb <$> floatOpen tyb b <*> float (SList tya) as)
  CoreProp (ListFilter ty' a b)
    -> CoreProp <$> (ListFilter ty' <$> singFloatOpen a <*> float (SList ty') b)
  CoreProp (ListFold tya tyb (Open v1 nm1 a) b c) -> do
    a' <- floatOpen tya a
    b' <- float tya b
    c' <- float (SList tyb) c
    pure $ CoreProp $ ListFold tya tyb (Open v1 nm1 a') b' c'

  -- objects
  CoreProp (ObjAt ty' str obj) -> PObjAt ty' <$> singFloat str <*> float ty' obj
  CoreProp (ObjContains ty' a b) -> CoreProp <$>
    (ObjContains ty' <$> singFloat a <*> float ty' b)
  CoreProp (ObjDrop ty' keys obj) -> CoreProp <$>
    (ObjDrop ty' <$> singFloat keys <*> float ty' obj)
  CoreProp (ObjTake ty' keys obj) -> CoreProp <$>
    (ObjTake ty' <$> singFloat keys <*> float ty' obj)
  CoreProp LiteralObject{} -> ([], p)
  CoreProp ObjMerge{}      -> ([], p)
  CoreProp (Where objty tya a b c) -> CoreProp <$>
    (Where objty tya <$> singFloat a <*> singFloatOpen b <*> float objty c)

  where

    singFloatOpen :: SingI b => Open a Prop b -> ([Quantifier], Open a Prop b)
    singFloatOpen = floatOpen sing

    floatOpen :: SingTy b -> Open a Prop b -> ([Quantifier], Open a Prop b)
    floatOpen ty' (Open v nm a) = Open v nm <$> float ty' a

    singFloat :: SingI a => Prop a -> ([Quantifier], Prop a)
    singFloat = float sing

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

-- We first use @floatBoolQuantifiers@ to remove all quantifiers from the
-- @Prop@ (modifying them as necessary, then put them back in place on the
-- outside of the syntax tree.
--
-- The only interesting cases are those for @Forall@, @Exists@, and @PNot@. In
-- the first two cases, we capture the quantifier to float it up. In the @PNot@
-- case, we flip all the quantifiers found inside the @PNot@ as we lift them
-- over it.
prenexConvert :: Prop 'TyBool -> Prop 'TyBool
prenexConvert = uncurry reassembleFloated . float SBool -- floatBoolQuantifiers
