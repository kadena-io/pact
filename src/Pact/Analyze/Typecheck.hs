{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language PatternSynonyms #-}
{-# language TupleSections #-}
module Pact.Analyze.Typecheck where

import Control.Lens ((%=), (%~), (<<+=), (&), _1, _2, (<&>))
import Control.Comonad.Cofree
import Control.Monad.Except
import Control.Monad.State
import Data.Foldable (for_)
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

type Check a = StateT (Int, [[(Ty, Ty)]]) (Except String) a

genVar :: Check Int
genVar = _1 <<+= 1

constrain :: Int -> [Ty] -> Check ()
constrain v tys =
  -- v is equal to any of these types
  let tys' = (TyVar v,) <$> tys
  in _2 %= (tys':)

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
    for_ xs' $ \(ty :< _) -> -- constrain v [ty]
      _2 %= ([(TyVar v, ty)]:)
    pure $ TyVar v :< PreListLit xs'

  PreAbort   -> pure $ TyBool :< PreAbort
  PreSuccess -> pure $ TyBool :< PreSuccess

  PreResult -> do
    v <- genVar
    pure $ TyVar v :< PreResult

  PreVar       {} -> error "TODO"
  PreGlobalVar {} -> error "TODO"

  PreForall {} -> error "TODO"
  PreExists {} -> error "TODO"

  PreApp name args -> case Map.lookup name funTypes of
    Nothing -> throwError $ "couldn't find function type: " ++ unpack name
    Just candidates -> do
      let candidates' = filter (\(args' :-> _) -> length args' == length args)
            candidates
      v       <- genVar
      args'   <- traverse checkPreProp args
      options <- for candidates' $ \(candidateArgs :-> result) ->

      pure $ TyVar v :< PreApp name args'

  PreAt            {} -> error "TODO"
  PrePropRead      {} -> error "TODO"
  PreLiteralObject {} -> error "TODO"

-- * traverse over every location, generating type variables & constraints
--   (checkPreProp)
-- * solve constraints (unify)
-- * elaborate
typecheck :: Fix PreProp -> Either String EProp
typecheck tm = do
  (cofreeTm, (_i, constraints))
    <- runExcept $ runStateT (checkPreProp tm) (0, [])
  elaborate (unify constraints) cofreeTm

pair :: [a] -> [b] -> Maybe [(a, b)]
pair []     []     = Just []
pair (a:as) (b:bs) = ((a, b):) <$> pair as bs
pair _      _      = Nothing

unify :: [[(Ty, Ty)]] -> Map Int Ty
unify [] = Map.empty
unify ((a, b):cs)
  | a == b = unify cs
  | TyVar v <- a = unifyVariable v cs b
  | TyVar v <- b = unifyVariable v cs a
  | TyFun dom1 codom1 <- a
  , TyFun dom2 codom2 <- b
  , Just cs' <- pair dom1 dom2
  = unify $ (codom1, codom2) : cs' <> cs
  | otherwise = error "a and b don't unify"

unifyVariable :: Int -> [(Ty, Ty)] -> Ty -> Map Int Ty
unifyVariable v cs b =
  if occurs v b then error "failed occurs check"
  else let subbed = Map.singleton v b
       in unify (substitute subbed cs) <> subbed

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

    PreGlobalVar{} -> error "TODO"

    PreExists vid name ty'' p -> do
      Some SBool p' <- elaborate env p
      pure $ Some SBool $ PropSpecific $ Exists vid name ty'' p'
    PreForall vid name ty'' p -> do
      Some SBool p' <- elaborate env p
      pure $ Some SBool $ PropSpecific $ Forall vid name ty'' p'

    PreAt{} -> error "TODO"
    PrePropRead{} -> error "TODO"
    PreLiteralObject{} -> error "TODO"

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
      _ -> Left "TODO"
      -- etc

buildList :: [EProp] -> EProp
buildList [] = Some (SList SAny) $ CoreProp $ LiteralList SAny []
buildList (Some ty1 prop : props) = case buildList props of
  Some (SList ty2) (CoreProp (LiteralList _ props')) -> case singEq ty1 ty2 of
    Nothing   -> error "TODO"
    Just Refl -> Some (SList ty1) $ CoreProp $ LiteralList ty1 $ prop : props'
  _ -> error "TODO"

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
  ]
