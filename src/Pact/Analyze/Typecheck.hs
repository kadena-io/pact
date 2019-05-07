{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language PatternSynonyms #-}
{-# language TupleSections #-}
module Pact.Analyze.Typecheck where

import Control.Lens ((%=), (%~), (<<+=), (&), _1, _2, (<&>))
import Control.Comonad.Cofree
import Control.Monad.Except
import Control.Monad.State
import Data.Foldable (asum, for_)
import Data.Traversable (for)
import           Data.Text                    (Text, unpack)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Type.Equality ((:~:)(Refl))

import Pact.Analyze.Feature hiding (Type(..), Var(..), str)
import Pact.Analyze.Parse.Types
import Pact.Analyze.Types.Languages
import Pact.Analyze.Types.Numerical
import Pact.Analyze.Types.Types hiding (Ty(..), (:<))
import Pact.Analyze.Types.Shared

data Ty
  = TyInteger
  | TyBool
  | TyStr
  | TyTime
  | TyDecimal
  | TyGuard
  | TyAny
  | TyList !Ty
  | TyObject ![(Text, Ty)] -- ^ Invariant: this list is always sorted
  | TyFun ![Ty] !Ty
  | TyVar !Int
  deriving Eq

-- | Convert a 'Ty' to a singleton 'SingTy' via 'EType'
cvtTy :: Ty -> EType
cvtTy = \case
  TyInteger -> EType SInteger
  TyBool    -> EType SBool
  TyStr     -> EType SStr
  TyTime    -> EType STime
  TyDecimal -> EType SDecimal
  TyGuard   -> EType SGuard
  TyAny     -> EType SAny
  TyList ty -> case cvtTy ty of
    EType sty -> EType $ SList sty
  TyObject tys -> case mkESchema (tys & traverse . _2 %~ cvtTy) of
    ESchema schema -> EType $ SObjectUnsafe schema
  TyFun{} -> error "Function types not allowed in cvtTy"
  TyVar{} -> error "Variable not allowed in cvtTy"

type Check a = StateT (Int, [Options]) (Except String) a

genVar :: Check Int
genVar = _1 <<+= 1

-- locations:            (conjunction)
--   options:            (disjunction)
--     constraints:      (conjunction)

newtype Options = Options ([[(Ty, Ty)]])

constrain :: Options -> Check ()
constrain options = _2 %= (options:)

oneOption :: [(Ty, Ty)] -> Options
oneOption = Options . (:[])

checkPreProp :: Fix PreProp -> Check (Cofree PreProp Ty)
checkPreProp (Fix prop) = case prop of
  PreIntegerLit i -> pure $ TyInteger :< PreIntegerLit i
  PreStringLit  s -> pure $ TyStr     :< PreStringLit  s
  PreDecimalLit d -> pure $ TyDecimal :< PreDecimalLit d
  PreTimeLit    t -> pure $ TyTime    :< PreTimeLit    t
  PreBoolLit    b -> pure $ TyBool    :< PreBoolLit    b

  PreListLit xs -> do
    v   <- genVar
    xs' <- traverse checkPreProp xs
    for_ xs' $ \(ty :< _) -> constrain $ oneOption [(TyVar v, ty)]
    pure $ TyVar v :< PreListLit xs'

  PreAbort   -> pure $ TyBool :< PreAbort
  PreSuccess -> pure $ TyBool :< PreSuccess

  PreResult -> do
    v <- genVar
    pure $ TyVar v :< PreResult

  PreVar       {} -> error "TODO (1)"
  PreGlobalVar {} -> error "TODO (2)"

  PreForall {} -> error "TODO (3)"
  PreExists {} -> error "TODO (4)"

  PreApp name args -> case Map.lookup name funTypes of
    Nothing -> throwError $ "couldn't find function type: " ++ unpack name
    Just candidates -> do
      let candidates' = filter
            (\case
              TyFun args' _ -> length args' == length args
              _ -> error "TODO (5)")
            candidates
      v       <- genVar
      args'   <- traverse checkPreProp args
      options <- for candidates' $ \case
        TyFun candidateArgs result -> do
          let argTys = zipWith (\(ty1 :< _) ty2 -> (ty1, ty2)) args' candidateArgs
          pure $ (TyVar v, result) : argTys
        _ -> error "TODO (6)"

      constrain $ Options options

      pure $ TyVar v :< PreApp name args'

  PreAt            {} -> error "TODO (7)"
  PrePropRead      {} -> error "TODO (8)"
  PreLiteralObject {} -> error "TODO (9)"

-- * traverse over every location, generating type variables & constraints
--   (checkPreProp)
-- * solve constraints (unify)
-- * elaborate
typecheck :: Fix PreProp -> Either String EProp
typecheck tm = do
  (cofreeTm, (_i, constraints))
    <- runExcept $ runStateT (checkPreProp tm) (0, [])
  tyAssignments <- unifyChoice constraints
  elaborate tyAssignments cofreeTm

pair :: [a] -> [b] -> Maybe [(a, b)]
pair []     []     = Just []
pair (a:as) (b:bs) = ((a, b):) <$> pair as bs
pair _      _      = Nothing

unifyChoice :: [Options] -> Either String (Map Int Ty)
unifyChoice options = unifyChoice' options []

unifyChoice' :: [Options] -> [(Ty, Ty)] -> Either String (Map Int Ty)
unifyChoice' [] matches = unify matches
unifyChoice' (Options options : moreOptions) matches = asum $ options <&>
  \option -> unifyChoice' moreOptions (matches <> option)

unify :: [(Ty, Ty)] -> Either String (Map Int Ty)
unify [] = pure Map.empty
unify ((a, b):cs)
  | a == b = unify cs
  | TyVar v <- a = unifyVariable v cs b
  | TyVar v <- b = unifyVariable v cs a
  | TyFun dom1 codom1 <- a
  , TyFun dom2 codom2 <- b
  , Just cs' <- pair dom1 dom2
  = unify $ (codom1, codom2) : cs' <> cs
  | otherwise = Left "a and b don't unify"

unifyVariable :: Int -> [(Ty, Ty)] -> Ty -> Either String (Map Int Ty)
unifyVariable v cs b =
  if occurs v b then Left "failed occurs check"
  else do
    let vIsB = Map.singleton v b
        cs' = substitute vIsB cs
    unifier <- unify cs'
    pure $ unifier <> vIsB

substitute :: Map Int Ty -> [(Ty, Ty)] -> [(Ty, Ty)]
substitute subs = fmap $ \(a, b) -> (replace subs a, replace subs b)

occurs :: Int -> Ty -> Bool
occurs v = \case
  TyList ty    -> occurs v ty
  TyObject tys -> any (occurs v) (fmap snd tys)
  TyFun dom codom -> any (occurs v) dom || occurs v codom
  TyVar v'     -> v == v'
  _            -> False

replace :: Map Int Ty -> Ty -> Ty
replace env ty = Map.foldrWithKey go ty env where
  go v ty' = \case
    TyVar v'
      | v == v'     -> ty'
      | otherwise   -> TyVar v'
    TyList ty''     -> TyList (go v ty' ty'')
    TyObject tys    -> TyObject $ tys & traverse . _2 %~ go v ty'
    TyFun dom codom -> TyFun (dom & traverse %~ go v ty') (go v ty' codom)
    ty''            -> ty''

-- TODO: make reader
elaborate :: Map Int Ty -> Cofree PreProp Ty -> Either String EProp
elaborate env (ty :< tm) = do
  EType ty' <- case ty of
    TyVar v -> case Map.lookup v env of
      Nothing  -> Left "couldn't find type of variable"
      Just ty' -> Right $ cvtTy ty'
    _ -> Right $ cvtTy ty
  case tm of
    PreIntegerLit i -> pure $ Some SInteger (Lit' i)
    PreStringLit  s -> pure $ Some SStr     (TextLit s)
    PreDecimalLit d -> pure $ Some SDecimal (Lit' d)
    PreTimeLit    t -> pure $ Some STime    (Lit' t)
    PreBoolLit    b -> pure $ Some SBool    (Lit' b)

    PreListLit xs -> buildList <$> traverse (elaborate env) xs

    PreAbort   -> pure $ Some SBool (PropSpecific Abort)
    PreSuccess -> pure $ Some SBool (PropSpecific Success)

    PreResult -> pure $ Some ty' $ PropSpecific Result

    PreVar vid name -> pure $ Some ty' $ CoreProp $ Var vid name

    PreGlobalVar{} -> error "TODO (10)"

    PreExists vid name ty'' p -> do
      Some SBool p' <- elaborate env p
      pure $ Some SBool $ PropSpecific $ Exists vid name ty'' p'
    PreForall vid name ty'' p -> do
      Some SBool p' <- elaborate env p
      pure $ Some SBool $ PropSpecific $ Forall vid name ty'' p'

    PreAt{}            -> error "TODO (11)"
    PrePropRead{}      -> error "TODO (12)"
    PreLiteralObject{} -> error "TODO (13)"

    PreApp s args -> case (s, args) of
      (SStringLength, [str]) -> do
        Some SStr str' <- elaborate env str
        pure $ Some SInteger $ CoreProp $ StrLength str'
      (SModulus, [x, y]) -> do
        Some SInteger x' <- elaborate env x
        Some SInteger y' <- elaborate env y
        pure $ Some SInteger $ CoreProp $ Numerical $ ModOp x' y'
      (SBankersRound, [x]) -> do
        Some SDecimal x' <- elaborate env x
        pure $ Some SInteger $ CoreProp $ Numerical $ RoundingLikeOp1 Round x'
      (SBankersRound, [x, y]) -> do
        Some SDecimal x' <- elaborate env x
        Some SInteger y' <- elaborate env y
        pure $ Some SDecimal $ CoreProp $ Numerical $ RoundingLikeOp2 Round x' y'
      (SAddition, [x, y]) -> do
        case ty' of
          SStr -> do
            Some SStr x' <- elaborate env x
            Some SStr y' <- elaborate env y
            pure $ Some SStr $ PStrConcat x' y'
          SInteger -> do
            Some SInteger x' <- elaborate env x
            Some SInteger y' <- elaborate env y
            pure $ Some SInteger $ CoreProp $ Numerical $ IntArithOp Add x' y'
          SDecimal -> do
            Some tyX x' <- elaborate env x
            Some tyY y' <- elaborate env y
            case (tyX, tyY) of
              (SDecimal, SDecimal) -> pure $ Some SDecimal $ CoreProp $ Numerical $ DecArithOp    Add x' y'
              (SDecimal, SInteger) -> pure $ Some SDecimal $ CoreProp $ Numerical $ DecIntArithOp Add x' y'
              (SInteger, SDecimal) -> pure $ Some SDecimal $ CoreProp $ Numerical $ IntDecArithOp Add x' y'
              _                    -> Left "TODO (13.5)"
          _ -> Left "TODO (13.75)"
      _ -> Left "TODO (14)"
      -- etc

buildList :: [EProp] -> EProp
buildList [] = Some (SList SAny) $ CoreProp $ LiteralList SAny []
buildList (Some ty1 prop : props) = case buildList props of
  Some (SList ty2) (CoreProp (LiteralList _ props')) -> case singEq ty1 ty2 of
    Nothing   -> error "TODO (15)"
    Just Refl -> Some (SList ty1) $ CoreProp $ LiteralList ty1 $ prop : props'
  _ -> error "TODO (16)"

pattern (:->) :: [Ty] -> Ty -> Ty
pattern a :-> b = TyFun a b

funTypes :: Map Text [Ty]
funTypes = Map.fromList
  [ (SStringLength,
    [ [TyStr] :-> TyInteger
    ])
  , (SModulus,
    [ [TyInteger, TyInteger] :-> TyInteger
    ])
  , (SBankersRound,
    [ [TyDecimal]            :-> TyInteger
    , [TyDecimal, TyInteger] :-> TyInteger
    ])
  , (SCeilingRound,
    [ [TyDecimal]            :-> TyInteger
    , [TyDecimal, TyInteger] :-> TyInteger
    ])
  , (SFloorRound,
    [ [TyDecimal]            :-> TyInteger
    , [TyDecimal, TyInteger] :-> TyInteger
    ])
  , (STemporalAddition,
    [ [TyTime, TyInteger] :-> TyTime
    , [TyTime, TyDecimal] :-> TyTime
    ])
  , (SAddition,
    [ [TyInteger, TyInteger] :-> TyInteger
    , [TyInteger, TyDecimal] :-> TyDecimal
    , [TyDecimal, TyInteger] :-> TyDecimal
    , [TyDecimal, TyDecimal] :-> TyDecimal
    , [TyStr    , TyStr    ] :-> TyStr
    ])
  ]
