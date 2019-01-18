{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE Rank2Types          #-}
{-# options_ghc -fno-warn-redundant-constraints #-}
module Pact.Analyze.Eval.Core where

import           Control.Lens                (over)
import           Data.Foldable               (asum)
import qualified Data.Map.Strict             as Map
import           Data.Maybe                  (fromMaybe)
import           Data.Monoid                 ((<>))
import           Data.SBV                    (EqSymbolic ((./=), (.==)),
                                              OrdSymbolic ((.<), (.<=), (.>), (.>=)),
                                              SymVal,
                                              SBV, ite, unliteral, literal, uninterpret)
import           Data.SBV.List               ((.:))
import qualified Data.SBV.List               as SBVL
import           Data.SBV.Tools.BoundedList  (band, bfoldr, breverse, bsort,
                                              bzipWith, bmapM, bfoldrM)
import qualified Data.SBV.String             as SBVS
import           Data.SBV.Tuple              (_1, _2, tuple)
-- import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Type.Equality          ((:~:)(Refl))
import           Data.Typeable               (Typeable)
import           GHC.Stack
import           GHC.TypeLits                (symbolVal)

import           Pact.Analyze.Errors
import           Pact.Analyze.Eval.Numerical
import           Pact.Analyze.Types
import           Pact.Analyze.Types.Eval
import           Pact.Analyze.Util           (Boolean(..))
import qualified Pact.Native                 as Pact

-- | Bound on the size of lists we check. This may be user-configurable in the
-- future.
listBound :: Int
listBound = 10

-- Note [Time Representation]
--
-- Pact uses the Thyme library (UTCTime) to represent times. Thyme internally
-- uses a 64-bit count of microseconds since the MJD epoch. So, our symbolic
-- representation is naturally a 64-bit integer.
--
-- The effect from a Pact-user's point of view is that we store 6 digits to
-- the right of the decimal point in times (even though we don't print
-- sub-second precision by default...).
--
-- pact> (add-time (time "2016-07-23T13:30:45Z") 0.001002)
-- "2016-07-23T13:30:45Z"
-- pact> (= (add-time (time "2016-07-23T13:30:45Z") 0.001002)
--          (add-time (time "2016-07-23T13:30:45Z") 0.0010021))
-- true
-- pact> (= (add-time (time "2016-07-23T13:30:45Z") 0.001002)
--          (add-time (time "2016-07-23T13:30:45Z") 0.001003))
-- false

evalIntAddTime
  :: Analyzer m
  => TermOf m 'TyTime
  -> TermOf m 'TyInteger
  -> m (S Time)
evalIntAddTime timeT secsT = do
  time <- eval timeT
  secs <- eval secsT
  -- Convert seconds to milliseconds /before/ conversion to Integer (see note
  -- [Time Representation]).
  pure $ time + fromIntegralS (secs * 1000000)

evalDecAddTime
  :: Analyzer m
  => TermOf m 'TyTime
  -> TermOf m 'TyDecimal
  -> m (S Time)
evalDecAddTime timeT secsT = do
  time <- eval timeT
  secs <- eval secsT
  if isConcreteS secs
  -- Convert seconds to milliseconds /before/ conversion to Integer (see note
  -- [Time Representation]).
  then pure $ time + fromIntegralS (banker'sMethodS (secs * fromInteger' 1000000))
  else throwErrorNoLoc $ PossibleRoundoff
    "A time being added is not concrete, so we can't guarantee that roundoff won't happen when it's converted to an integer."

evalComparisonOp
  :: forall m a.
     Analyzer m
  => SingTy a
  -> ComparisonOp
  -> TermOf m a
  -> TermOf m a
  -> m (S Bool)
evalComparisonOp ty op xT yT = do
  x <- withSing ty $ eval xT
  y <- withSing ty $ eval yT
  let f :: SymVal (Concrete a) => SBV Bool
      f = case op of
        Gt  -> x .> y
        Lt  -> x .< y
        Gte -> x .>= y
        Lte -> x .<= y
        Eq  -> x .== y
        Neq -> x ./= y
  pure $ sansProv $ withSymVal ty f

singIte
  :: forall m a a'. (Analyzer m, a' ~ Concrete a, SingI a)
  => SingTy a -> SBV Bool -> m (S a') -> m (S a') -> m (S a')
singIte ty a b c = withSymVal ty $ withMergeableAnalyzer @m ty $ ite a b c

evalLogicalOp
  :: Analyzer m
  => LogicalOp
  -> [TermOf m 'TyBool]
  -> m (S Bool)
evalLogicalOp AndOp [a, b] = do
  a' <- eval a
  singIte SBool (_sSbv a') (eval b) (pure sFalse)
evalLogicalOp OrOp [a, b] = do
  a' <- eval a
  singIte SBool (_sSbv a') (pure sTrue) (eval b)
evalLogicalOp NotOp [a] = sNot <$> eval a
evalLogicalOp op terms = throwErrorNoLoc $ MalformedLogicalOpExec op $ length terms

-- | Throw an analyze failure when Nothing
(??) :: Analyzer m => Maybe a -> AnalyzeFailureNoLoc -> m a
(Just a) ?? _   = pure a
Nothing  ?? err = throwErrorNoLoc err
infix 0 ??

evalCore :: forall m a.
  (Analyzer m, SingI a) => Core (TermOf m) a -> m (S (Concrete a))
evalCore (Lit a)
  = withSymVal (sing :: SingTy a) $ pure $ literalS a
evalCore (Sym s) = pure s
evalCore (Var vid name) = do
  mVal <- getVar vid
  case mVal of
    Nothing                -> throwErrorNoLoc $ VarNotInScope name vid
    Just (AVal mProv sval) -> pure $ mkS mProv sval
    Just OpaqueVal         -> throwErrorNoLoc OpaqueValEncountered
evalCore (Identity _ a)     = eval a
evalCore (Constantly _ a _) = eval a
evalCore (Compose tya tyb _ a (Open vida _nma tmb) (Open vidb _nmb tmc)) = do
  a' <- withSing tya $ eval a
  b' <- withVar vida (mkAVal a') $ withSing tyb $ eval tmb
  withVar vidb (mkAVal b') $ eval tmc
evalCore (StrConcat p1 p2)                 = (.++) <$> eval p1 <*> eval p2
evalCore (StrLength p)
  = over s2Sbv SBVS.length . coerceS @Str @String <$> eval p
evalCore (StrToInt s)                      = evalStrToInt s
evalCore (StrToIntBase b s)                = evalStrToIntBase b s
evalCore (Numerical a)                     = evalNumerical a
evalCore (IntAddTime time secs)            = evalIntAddTime time secs
evalCore (DecAddTime time secs)            = evalDecAddTime time secs
evalCore (Comparison ty op x y)            = evalComparisonOp ty op x y
evalCore (Logical op props)                = evalLogicalOp op props
evalCore (ObjAt schema colNameT objT)
  = evalObjAt schema colNameT objT (sing :: SingTy a)
evalCore (LiteralObject (SObjectUnsafe (SingList SNil)) (Object SNil))
  = pure $ literalS ()
evalCore
  (LiteralObject
    (SObjectUnsafe (SingList (SCons _ ty tys)))
    (Object (SCons _ (Column _ val) vals)))
  = do
    let objTy = SObjectUnsafe $ SingList tys
    withSing objTy $ withSymVal ty $ withSymVal objTy $ do
      S _ val'  <- eval val
      S _ vals' <- evalCore $ LiteralObject objTy $ Object vals
      pure $ sansProv $ tuple (val', vals')
evalCore LiteralObject{} = error "impossible match"
evalCore (ObjMerge
  ty1@(SObjectUnsafe schema1)
  ty2@(SObjectUnsafe schema2)
  obj1 obj2) = withSing ty1 $ withSing ty2 $ do
    S _ obj1' <- eval obj1
    S _ obj2' <- eval obj2
    case sing @a of
      SObjectUnsafe schema -> pure $ sansProv $
        evalObjMerge (obj1' :< schema1) (obj2' :< schema2) schema
      _ -> error "impossible match"
evalCore ObjMerge{} = error "impossible match"
evalCore (ObjContains (SObjectUnsafe schema) key _obj)
  = hasKey schema <$> eval key
evalCore (StrContains needle haystack) = do
  needle'   <- eval needle
  haystack' <- eval haystack
  pure $ sansProv $
    _sSbv (coerceS @Str @String needle')
    `SBVS.isInfixOf`
    _sSbv (coerceS @Str @String haystack')
evalCore (ListContains ty needle haystack) = withSymVal ty $ do
  S _ needle'   <- withSing ty $ eval needle
  S _ haystack' <- withSing ty $ eval haystack
  pure $ sansProv $
    bfoldr listBound (\cell rest -> cell .== needle' .|| rest) sFalse haystack'
evalCore (ListEqNeq ty op a b) = withSymVal ty $ do
  S _ a' <- withSing ty $ eval a
  S _ b' <- withSing ty $ eval b

  let wrongLength = case op of
        Eq'  -> sFalse
        Neq' -> sTrue
      zipF = case op of
        Eq'  -> (.==)
        Neq' -> (./=)

  pure $ ite (SBVL.length a' .== SBVL.length b')
    (sansProv $ band listBound $ bzipWith listBound zipF a' b')
    wrongLength
evalCore (ListAt ty i l) = withSymVal ty $ do
  S _ i' <- eval i
  S _ l' <- eval l

  -- valid range [0..length l - 1]
  markFailure $ i' .< 0 .|| i' .>= SBVL.length l'

  -- statically build a list of index comparisons
  pure $ sansProv $ SBVL.elemAt l' i'
evalCore (ListLength ty l) = withSymVal ty $ withSing ty $ do
  S prov l' <- eval l
  pure $ S prov $ SBVL.length l'

evalCore (LiteralList ty xs) = withSymVal ty $ withSing ty $ do
  vals <- traverse (fmap _sSbv . eval) xs
  pure $ sansProv $ SBVL.implode vals

evalCore (ListDrop ty n list) = withSymVal ty $ withSing ty $ do
  S _ n'    <- eval n
  S _ list' <- eval list

  -- if the index is positive, count from the start of the list, otherwise
  -- count from the end.
  pure $ sansProv $ ite (n' .>= 0)
    (SBVL.drop n' list')
    (SBVL.take (SBVL.length list' + n') list')

evalCore (ListTake ty n list) = withSymVal ty $ withSing ty $ do
  S _ n'    <- eval n
  S _ list' <- eval list

  -- if the index is positive, count from the start of the list, otherwise
  -- count from the end.
  pure $ sansProv $ ite (n' .>= 0)
    (SBVL.take n' list')
    (SBVL.drop (SBVL.length list' + n') list')

evalCore (ListConcat ty p1 p2) = withSymVal ty $ withSing ty $ do
  S _ p1' <- eval p1
  S _ p2' <- eval p2
  pure $ sansProv $ SBVL.concat p1' p2'
evalCore (ListReverse ty l) = withSymVal ty $ withSing ty $ do
  S prov l' <- eval l
  pure $ S prov $ breverse listBound l'
evalCore (ListSort ty l) = withSymVal ty $ withSing ty $ do
  S prov l' <- eval l
  pure $ S prov $ bsort listBound l'
evalCore (MakeList ty i a) = withSymVal ty $ withSing ty $ do
  S _ i' <- eval i
  S _ a' <- eval a
  case unliteral i' of
    Just i'' -> pure $ sansProv $ SBVL.implode $ replicate (fromInteger i'') a'
    Nothing  -> throwErrorNoLoc $ UnhandledTerm
      "make-list currently requires a statically determined length"

evalCore (ListMap tya tyb (Open vid _ expr) as)
  = withSymVal tya $ withSymVal tyb $ withSing tya $ withSing tyb $
    withMergeableAnalyzer @m (SList tyb) $ do
  S _ as' <- eval as
  bs <- bmapM listBound
    (\val -> _sSbv <$> withVar vid (mkAVal' val) (eval expr))
    as'
  pure $ sansProv bs

evalCore (ListFilter tya (Open vid _ f) as)
  = withSymVal tya $ withMergeableAnalyzer @m (SList tya) $ do
  S _ as' <- eval as
  let bfilterM = bfoldrM listBound
        (\sbva svblst -> do
          S _ x' <- withVar vid (mkAVal' sbva) (eval f)
          pure $ ite x' (sbva .: svblst) svblst)
        (literal [])
  sansProv <$> bfilterM as'

evalCore (ListFold tya tyb (Open vid1 _ (Open vid2 _ f)) a bs)
  = withSymVal tya $ withSymVal tyb $ withSing tyb $
    withMergeableAnalyzer @m tya $ do
  S _ a'  <- eval a
  S _ bs' <- eval bs
  result <- bfoldrM listBound
    (\sbvb sbva -> fmap _sSbv $
      withVar vid1 (mkAVal' sbvb) $
        withVar vid2 (mkAVal' sbva) $
          eval f)
    a' bs'
  pure $ sansProv result

evalCore (AndQ tya (Open vid1 _ f) (Open vid2 _ g) a) = do
  S _ a' <- withSing tya $ eval a
  fv     <- withVar vid1 (mkAVal' a') $ eval f
  gv     <- withVar vid2 (mkAVal' a') $ eval g
  pure $ fv .&& gv

evalCore (OrQ tya (Open vid1 _ f) (Open vid2 _ g) a) = do
  S _ a' <- withSing tya $ eval a
  fv     <- withVar vid1 (mkAVal' a') $ eval f
  gv     <- withVar vid2 (mkAVal' a') $ eval g
  pure $ fv .|| gv

evalCore (Where schema tya key (Open vid _ f) obj) = withSymVal tya $ do
  S _ v <- evalObjAt schema key obj tya
  withVar vid (mkAVal' v) $ eval f

evalCore (Typeof tya _a) = pure $ literalS $ Str $ T.unpack $ userShow tya

evalCore (ObjectEqNeq
  (SObjectUnsafe (SingList SNil))
  (SObjectUnsafe (SingList SNil))
  eqNeq _ _) = pure $ case eqNeq of { Eq' -> sTrue; Neq' -> sFalse }
evalCore (ObjectEqNeq
  (SObjectUnsafe (SingList SNil))
  (SObjectUnsafe (SingList SCons{}))
  eqNeq _ _) = pure $ case eqNeq of { Eq' -> sFalse; Neq' -> sTrue }
evalCore (ObjectEqNeq
  (SObjectUnsafe (SingList SCons{}))
  (SObjectUnsafe (SingList SNil))
  eqNeq _ _) = pure $ case eqNeq of { Eq' -> sFalse; Neq' -> sTrue }

evalCore (ObjectEqNeq
  objTy1@(SObjectUnsafe schema@(SingList SCons{}))
  objTy2@(SObjectUnsafe        (SingList SCons{}))
  eqNeq obj1 obj2) = case singEq objTy1 objTy2 of
    Nothing   -> pure sFalse
    Just Refl -> withSing objTy1 $ do
      S _ obj1' <- eval obj1
      S _ obj2' <- eval obj2

      let go
            :: SingList tys
            -> SBV (Concrete ('TyObject tys))
            -> SBV (Concrete ('TyObject tys))
            -> SBV Bool
          go (SingList SNil) _ _ = sTrue
          go (SingList (SCons _ colTy' schema')) a b
            = withSymVal colTy'
            $ withSymVal (SObjectUnsafe (SingList schema'))
            $ _1 a .== _1 b .&& go (SingList schema') (_2 a) (_2 b)

      let objsEq = go schema obj1' obj2'
      pure $ sansProv $ case eqNeq of { Eq' -> objsEq; Neq' -> sNot objsEq }

evalCore (ObjDrop ty@(SObjectUnsafe schema') _keys obj) = withSing ty $ do
  S prov obj' <- eval obj
  case sing @a of
    SObjectUnsafe schema -> pure $ S prov $ evalDropTake (obj' :< schema') schema
evalCore (ObjTake ty@(SObjectUnsafe schema') _keys obj) = withSing ty $ do
  S prov obj' <- eval obj
  case sing @a of
    SObjectUnsafe schema -> pure $ S prov $ evalDropTake (obj' :< schema') schema

-- | Implementation for both drop and take.
--
-- We @schema@ -- the type of the object we're starting with, and @schema'@ --
-- the type we need to produce. @schema'@ must be a sub-list of @schema@.
-- Therefore, we work through the elements of the object one at a time, keeping
-- the ones that go in the resulting object.
evalDropTake
  :: SBV (ConcreteObj schema) :< SingList schema
  -> SingList schema'
  -> SBV (ConcreteObj schema')
evalDropTake _ (SingList SNil) = literal ()
evalDropTake
  (tm :<   SingList (SCons k  ty  ks ))
  schema'@(SingList (SCons k' ty' ks'))
  = withHasKind ty $ withSymVal (SObjectUnsafe (SingList ks))

  -- Test equality of both the key names and types. If the key matches then the
  -- type should as well, but we need to test both to convince ghc
  $ fromMaybe (evalDropTake (_2 tm :< SingList ks) schema') $ do
    Refl <- eqSym  k  k'
    Refl <- singEq ty ty'
    withSymVal ty $ withSymVal (SObjectUnsafe (SingList ks')) $ pure $
      tuple (_1 tm, evalDropTake (_2 tm :< SingList ks) (SingList ks'))

evalDropTake _ _ = error "evalDropTake invariant violation"

data SomeObj where
  SomeObj :: SingList schema -> SBV (ConcreteObj schema) -> SomeObj

-- | Shrink the given object to the largest subobject smaller than the search
-- schema. Meaning its first key is > than the search schema's first key.
subObject
  :: SBV (ConcreteObj schema) :< SingList schema
  -> SingList searchSchema
  -> SomeObj
subObject (_ :< SingList SNil) _ = SomeObj (SingList SNil) (literal ())
subObject _ (SingList SNil)      = SomeObj (SingList SNil) (literal ())
subObject
  (obj :< schema@(SingList (SCons k  v   kvs)))
    searchSchema@(SingList (SCons sk _sv _skvs))
  = withHasKind v $ withSymVal (SObjectUnsafe (SingList kvs)) $
    case cmpSym k sk of
      GT -> SomeObj schema obj
      _  -> subObject (_2 obj :< SingList kvs) searchSchema


-- | Merge two objects, returning one with the requested schema.
--
-- We simultaneously shrink each input object, via @subObject@ so that we're
-- always examining the smallest objects that could possibly match our
-- searched-for schema.
--
-- Example:
--
--   obj1:   { x: 1, y: 2                   }
--   obj2:   {       y: 4,       z: 5       }
--   schema: {       y: integer, z: integer }
--
-- Note that this example illustrates why we must shrink objects before the
-- first call. If we didn't shrink here then obj2 would match first even though
-- obj1 should.
--
-- Step 1: shrink
--
--   obj1:   { y: 2                   }
--   obj2:   { y: 4,       z: 5       }
--   schema: { y: integer, z: integer }
--
-- 2: match
--
--   y = 2
--
-- 3: shrink
--
--   obj1:   {            }
--   obj2:   { z: 5       }
--   schema: { z: integer }
--
-- 3.5. switch to @evalObjMerge1@
--
--   obj2:   { z: 5       }
--   schema: { z: integer }
--
-- 4: match
--
--   z = 5
--
-- 5: shrink
--
--   obj2:   {}
--   schema: {}
--
-- 6: terminate
--
-- result: { y: 2, z: 5 }
--
-- XXX must shrink before first call!
evalObjMerge
  :: (SBV (ConcreteObj schema1) :< SingList schema1)
  -> (SBV (ConcreteObj schema2) :< SingList schema2)
  -> SingList schema
  -> SBV (ConcreteObj schema)
evalObjMerge _ _ (SingList SNil) = literal ()
evalObjMerge
  (obj1 :< schema1@(SingList (SCons k1 ty1 subSchema1)))
  (obj2 :< schema2@(SingList (SCons k2 ty2 subSchema2)))
            schema@(SingList (SCons k  ty  subSchema ))
  = withSymVal (SObjectUnsafe (SingList subSchema1)) $ withHasKind ty1 $
    withSymVal (SObjectUnsafe (SingList subSchema2)) $ withHasKind ty2 $
    withSymVal ty $ withSymVal (SObjectUnsafe (SingList subSchema)) $

    fromMaybe (error "evalObjMerge invariant violation") $
      case subObject (obj1 :< schema1) schema of
        SomeObj schema1' obj1' -> case subObject (obj2 :< schema2) schema of
          SomeObj schema2' obj2' -> asum
            -- object 1 matches (left-biased)
            [ do Refl <- eqSym  k1  k
                 Refl <- singEq ty1 ty
                 pure $ tuple
                   ( _1 obj1
                   , evalObjMerge (obj1' :< schema1') (obj2' :< schema2')
                     (SingList subSchema)
                   )

            -- object 2 matches
            , do Refl <- eqSym  k2  k
                 Refl <- singEq ty2 ty
                 pure $ tuple
                   (_1 obj2
                   , evalObjMerge (obj1' :< schema1') (obj2' :< schema2')
                     (SingList subSchema)
                   )

            -- neither object matches
            , pure $ evalObjMerge (obj1' :< schema1') (obj2' :< schema2') schema
            ]

evalObjMerge
  (obj1 :< schema1@(SingList SCons{}))
  (_    :<          SingList SNil)
            schema@(SingList SCons{})
  = evalDropTake (obj1 :< schema1) schema

evalObjMerge
  (_    :<          SingList SNil)
  (obj2 :< schema2@(SingList SCons{}))
            schema@(SingList SCons{})
  = evalDropTake (obj2 :< schema2) schema

evalObjMerge (_ :< SingList SNil) (_ :< SingList SNil) (SingList SCons{})
  = error "evalObjMerge invariant violation: both input object exhausted"

hasKey :: SingList schema -> S Str -> S Bool
hasKey (SingList SNil) _ = sFalse
hasKey (SingList (SCons k _ schema')) key
  = ite (literalS (Str (symbolVal k)) .== key)
        sTrue
        (hasKey (SingList schema') key)

evalStrToInt :: Analyzer m => TermOf m 'TyStr -> m (S Integer)
evalStrToInt sT = do
  s' <- _sSbv <$> eval sT
  let s = coerceSBV @Str @String s'
  markFailure $ SBVS.null s
  markFailure $ SBVS.length s .> 128
  let nat = SBVS.strToNat s
  markFailure $ nat .< 0 -- will happen if empty or contains a non-digit
  pure $ sansProv nat

evalStrToIntBase
  :: (Analyzer m) => TermOf m 'TyInteger -> TermOf m 'TyStr -> m (S Integer)
evalStrToIntBase bT sT = do
  b  <- eval bT
  s' <- eval sT
  let s = coerceS @Str @String s'

  markFailure $ SBVS.null $ _sSbv s
  markFailure $ SBVS.length (_sSbv s) .> 128

  case (unliteralS b, unliteralS s) of
    -- Symbolic base and string: give up; too many possible solutions.
    (Nothing, Nothing) ->
      throwErrorNoLoc "Unable to convert string to integer for symbolic base and string"

    -- Symbolic base and concrete string: pre-compute all 17 possible outcomes
    (Nothing, Just conStr) ->
      let conText = T.pack conStr
      in foldr
           (\conBase rest ->
             singIte SInteger (b .== literalS conBase)
               (precompute conBase conText)
               rest)
           symbolicFailure
           [2..16]

    -- Concrete base and symbolic string: only support base 10
    (Just conBase, Nothing)
      | conBase == 10 -> evalStrToInt $ inject' $ coerceS @String @Str s
      | otherwise     -> throwErrorNoLoc $ FailureMessage $ T.pack $ "Unable to statically determine the string for conversion to integer from base " ++ show conBase

    -- Concrete base and string: use pact native impl
    (Just conBase, Just conStr) ->
      case Pact.baseStrToInt conBase (T.pack conStr) of
        Left err -> throwErrorNoLoc $
          FailureMessage $ "Failed to convert string to integer: " <> err
        Right res -> pure $ literalS res

  where
    symbolicFailure = do
      markFailure sTrue
      pure (literalS 0)

    precompute base conText =
      case Pact.baseStrToInt base conText of
        Left _err -> symbolicFailure
        Right res -> pure (literalS res)

relevantFields :: (Typeable a, SingI a) => SingTy a -> Object tm obj -> EObject tm
relevantFields _ obj@(Object SNil) = EObject SNil' obj
relevantFields targetTy (Object (SCons key (Column vTy v) vals))
  = case relevantFields targetTy (Object vals) of
      EObject ty obj'@(Object vals') -> case singEq targetTy vTy of
        Nothing   -> EObject ty obj'
        Just Refl -> EObject (SCons' key sing ty) $
          Object (SCons key (Column vTy v) vals')

evalObjAt
  :: forall a m schema.
     (Analyzer m, HasCallStack)
  => SingTy ('TyObject schema)
  -> TermOf m 'TyStr
  -> TermOf m ('TyObject schema)
  -> SingTy a
  -> m (S (Concrete a))
evalObjAt objTy@(SObjectUnsafe schema) colNameT obj retType
  = withSymVal retType $ withSing objTy $ do
    needColName <- eval colNameT
    S mObjProv objVal <- eval obj

    let go :: HasCallStack
           => SingList tys -> SBV (Concrete ('TyObject tys)) -> m (SBV (Concrete a))
        go (SingList SNil) _ = do
          markFailure sTrue
          pure $ uninterpret "notfound"
        go (SingList (SCons sym colTy schema')) obj'
          = withSymVal (SObjectUnsafe (SingList schema')) $ withSymVal colTy $
            withMergeableAnalyzer @m retType $ ite
              (needColName .== literalS (Str (symbolVal sym)))
              (case singEq colTy retType of
                 Nothing   -> throwErrorNoLoc "TODO (evalObjAt mismatched field types)"
                 Just Refl -> pure $ _1 obj'
              )
              (go (SingList schema') (_2 obj'))

        mProv :: Maybe Provenance
        mProv = do
          FromRow ocMap <- mObjProv
          -- NOTE: We can only propagate the provenance from the row- to the
          -- cell-level if we know the column name statically:
          Str cnStr <- unliteralS needColName
          FromCell <$> Map.lookup (ColumnName cnStr) ocMap

    S mProv <$> go schema objVal

evalExistential :: Analyzer m => Existential (TermOf m) -> m (EType, AVal)
evalExistential (Some ty prop) = do
  prop' <- withSing ty $ eval prop
  pure (EType ty, mkAVal prop')
