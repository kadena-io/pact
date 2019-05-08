{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language PatternSynonyms #-}
{-# language TupleSections #-}
module Pact.Analyze.Typecheck where

import Control.Lens ((%=), (%~), (<<+=), (&), _2, (<&>), use, view, at, (.~))
import Control.Comonad.Cofree
import Control.Monad.Except
import Control.Monad.Reader
import Data.Foldable (asum, for_)
import Data.Traversable (for)
import           Data.Text                    (Text, unpack)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.String (fromString)
import           Data.Type.Equality ((:~:)(Refl))

import Pact.Analyze.Feature hiding (Type(..), Var(..), str, Constraint)
import Pact.Analyze.Parse.Types
import Pact.Analyze.Types.Languages
import Pact.Analyze.Types.Numerical
import Pact.Analyze.Types.Types hiding (Ty(..), (:<))
import Pact.Analyze.Types.Shared
import Pact.Types.Pretty (pretty, renderCompactString')

parseTableName :: Fix PreProp -> PropCheck (Prop TyTableName)
parseTableName (Fix (PreGlobalVar var)) = pure (fromString (unpack var))
parseTableName (Fix (PreVar vid name)) = do
  varTy <- view (varTys . at vid)
  case varTy of
    Just QTable -> pure $ CoreProp $ Var vid name
    _           -> throwError $ unpack $ "invalid table name: " <> name
parseTableName bad = throwError "invalid table name: TODO"
-- parseTableName bad = throwError $ renderCompactString' $
--   "invalid table name: " <> pretty bad

parseColumnName :: Fix PreProp -> PropCheck (Prop TyColumnName)
parseColumnName (Fix (PreStringLit str)) = pure (fromString (unpack str))
parseColumnName (Fix (PreVar vid name)) = do
  varTy <- view (varTys . at vid)
  case varTy of
    Just QColumnOf{} -> pure $ CoreProp $ Var vid name
    _                -> throwError $ unpack $
      "invalid column name: " <> name
parseColumnName bad = throwError "invalid column name: TODO"
-- parseColumnName bad = throwError $ renderCompactString' $
--   "invalid column name: " <> pretty bad

parseBeforeAfter :: Fix PreProp -> PropCheck BeforeOrAfter
parseBeforeAfter (Fix (PreStringLit str))
  | str == "before" = pure Before
  | str == "after"  = pure After
parseBeforeAfter other = throwError "expected 'before / 'after TODO"
-- parseBeforeAfter other = throwErrorIn other "expected 'before / 'after"

genVar :: PropCheck VarId
genVar = checkVarGen <<+= 1

-- locations:            (conjunction)
--   options:            (disjunction)
--     constraints:      (conjunction)

constrain :: Options -> PropCheck ()
constrain options = checkConstraints %= (options:)

oneOption :: [Constraint] -> Options
oneOption = Options . (:[])

checkPreProp :: Fix PreProp -> PropCheck (Cofree PreProp Ty)
checkPreProp (Fix prop) = case prop of
  PreIntegerLit i -> pure $ TyInteger :< PreIntegerLit i
  PreStringLit  s -> pure $ TyStr     :< PreStringLit  s
  PreDecimalLit d -> pure $ TyDecimal :< PreDecimalLit d
  PreTimeLit    t -> pure $ TyTime    :< PreTimeLit    t
  PreBoolLit    b -> pure $ TyBool    :< PreBoolLit    b

  PreListLit xs -> do
    v   <- genVar
    xs' <- traverse checkPreProp xs
    for_ xs' $ \(ty :< _) -> constrain $ oneOption [EqConstraint (TyVar v) ty]
    pure $ TyVar v :< PreListLit xs'

  PreAbort   -> pure $ TyBool :< PreAbort
  PreSuccess -> pure $ TyBool :< PreSuccess

  PreResult -> do
    v <- genVar
    pure $ TyVar v :< PreResult

  PreVar vid name -> pure $ TyVar vid :< PreVar vid name
  PreGlobalVar name -> do
    defn        <- view $ localVars    . at name
    definedProp <- view $ definedProps . at name
    case defn of
      Nothing    -> case definedProp of
        -- clear this definition so it can't call itself
        Just (DefinedProperty [] definedProp') ->
          local (definedProps . at name .~ Nothing) $ do
            ty :< _ <- checkPreProp definedProp'
            pure $ ty :< PreGlobalVar name
        Just _ -> throwError $
          unpack name <> " expects arguments but wasn't provided any"
        Nothing -> throwError $ "couldn't find property variable " <> unpack name
      Just defn' -> undefined -- pure defn'

  PreForall vid varName qty body -> do
    body'@(bodyTy :< _) <- checkPreProp body
    constrain $ oneOption [EqConstraint bodyTy TyBool]
    pure $ TyBool :< PreForall vid varName qty body'
  PreExists vid varName qty body -> do
    body'@(bodyTy :< _) <- checkPreProp body
    constrain $ oneOption [EqConstraint bodyTy TyBool]
    pure $ TyBool :< PreForall vid varName qty body'

  PreApp name args -> case Map.lookup name funTypes of
    Nothing -> throwError $ "couldn't find function type: " ++ unpack name
    Just candidates -> do
      let candidates' = filter
            (\case
              TyFun args' _ -> length args' == length args
              _ -> error "invariant violation: candidate is not a function")
            candidates
      v       <- genVar
      args'   <- traverse checkPreProp args
      options <- for candidates' $ \case
        TyFun candidateArgs result -> do
          let argTys = zipWith (\(ty1 :< _) ty2 -> EqConstraint ty1 ty2)
                args' candidateArgs
          pure $ EqConstraint (TyVar v) result : argTys
        _ -> error "invariant violation: candidate is not a function"

      constrain $ Options options

      pure $ TyVar v :< PreApp name args'

  -- we handle two cases:
  -- 1. ix is a concrete string (a *symbol*), container is an object.
  -- 2. ix is an int, container is a list
  PreAt ix container -> case ix of
    Fix (PreStringLit name) -> do
      objTy :< obj <- checkPreProp container
      v            <- genVar
      constrain $ oneOption [HasRowConstraint name (TyVar v) objTy]
      pure $ TyVar v :< PreAt
        (TyStr :< PreStringLit name)
        (objTy :< obj)
    _ -> error "case 2"

  PrePropRead      {} -> error "TODO (8)"
  PreLiteralObject obj -> do
    let fields = Map.toList obj
    obj'' <- for fields $ \(fieldName, tm) -> do
      ty :< tm' <- checkPreProp tm
      pure (fieldName, ty, tm')
    let resultTy  = obj'' <&> \(fieldName, ty, _tm) -> (fieldName, ty      )
        resultObj = obj'' <&> \(fieldName, ty,  tm) -> (fieldName, ty :< tm)
    pure $ TyObject resultTy :< PreLiteralObject (Map.fromList resultObj)

-- * traverse over every location, generating type variables & constraints
--   (checkPreProp)
-- * solve constraints (unify)
-- * elaborate
typecheck :: Fix PreProp -> PropCheck EProp
typecheck tm = do
  cofreeTm      <- checkPreProp tm
  constraints   <- use checkConstraints
  tyAssignments <- lift $ lift $ EitherFail $ unifyChoice constraints
  lift $ lift $ EitherFail $ runReaderT (elaborate cofreeTm) tyAssignments

pair :: [Ty] -> [Ty] -> Maybe [Constraint]
pair []     []     = Just []
pair (a:as) (b:bs) = (EqConstraint a b:) <$> pair as bs
pair _      _      = Nothing

unifyChoice :: [Options] -> Either String (Map VarId Ty)
unifyChoice options = unifyChoice' options []

unifyChoice' :: [Options] -> [Constraint] -> Either String (Map VarId Ty)
unifyChoice' [] matches = unify matches
unifyChoice' (Options options : moreOptions) matches = asum $ options <&>
  \option -> unifyChoice' moreOptions (matches <> option)

unify :: [Constraint] -> Either String (Map VarId Ty)
unify [] = pure Map.empty
unify (EqConstraint a b:cs)
  | a == b = unify cs
  | TyVar v <- a = unifyVariable v cs b
  | TyVar v <- b = unifyVariable v cs a
  | TyFun dom1 codom1 <- a
  , TyFun dom2 codom2 <- b
  , Just cs' <- pair dom1 dom2
  = unify $ EqConstraint codom1 codom2 : cs' <> cs
  | otherwise = Left "a and b don't unify"

unifyVariable :: VarId -> [Constraint] -> Ty -> Either String (Map VarId Ty)
unifyVariable v cs b =
  if occurs v b then Left "failed occurs check"
  else do
    let vIsB = Map.singleton v b
        cs' = substitute vIsB cs
    unifier <- unify cs'
    pure $ unifier <> vIsB

substitute :: Map VarId Ty -> [Constraint] -> [Constraint]
substitute subs = fmap $ \(EqConstraint a b)
  -> EqConstraint (replace subs a) (replace subs b)

occurs :: VarId -> Ty -> Bool
occurs v = \case
  TyList ty    -> occurs v ty
  TyObject tys -> any (occurs v) (fmap snd tys)
  TyFun dom codom -> any (occurs v) dom || occurs v codom
  TyVar v'     -> v == v'
  _            -> False

replace :: Map VarId Ty -> Ty -> Ty
replace env ty = Map.foldrWithKey go ty env where
  go v ty' = \case
    TyVar v'
      | v == v'     -> ty'
      | otherwise   -> TyVar v'
    TyList ty''     -> TyList (go v ty' ty'')
    TyObject tys    -> TyObject $ tys & traverse . _2 %~ go v ty'
    TyFun dom codom -> TyFun (dom & traverse %~ go v ty') (go v ty' codom)
    ty''            -> ty''

elaborate :: Cofree PreProp Ty -> ReaderT (Map VarId Ty) (Either String) EProp
elaborate (ty :< tm) = do
  env <- ask
  EType ty' <- case ty of
    TyVar v -> case Map.lookup v env of
      Nothing  -> throwError "couldn't find type of variable"
      Just ty' -> pure $ cvtTy ty'
    _ -> pure $ cvtTy ty
  case tm of
    PreIntegerLit i -> pure $ Some SInteger (Lit' i)
    PreStringLit  s -> pure $ Some SStr     (TextLit s)
    PreDecimalLit d -> pure $ Some SDecimal (Lit' d)
    PreTimeLit    t -> pure $ Some STime    (Lit' t)
    PreBoolLit    b -> pure $ Some SBool    (Lit' b)

    PreListLit xs -> buildList <$> traverse elaborate xs

    PreAbort   -> pure $ Some SBool (PropSpecific Abort)
    PreSuccess -> pure $ Some SBool (PropSpecific Success)

    PreResult -> pure $ Some ty' $ PropSpecific Result

    PreVar vid name -> pure $ Some ty' $ CoreProp $ Var vid name

    PreGlobalVar{} -> error "TODO (10)"

    PreExists vid name ty'' p -> do
      Some SBool p' <- elaborate p
      pure $ Some SBool $ PropSpecific $ Exists vid name ty'' p'
    PreForall vid name ty'' p -> do
      Some SBool p' <- elaborate p
      pure $ Some SBool $ PropSpecific $ Forall vid name ty'' p'

    PreAt{}            -> error "TODO (11)"
    PrePropRead{}      -> error "TODO (12)"
    PreLiteralObject{} -> error "TODO (13)"

    PreApp s args -> case (s, args) of
      (SStringLength, [str]) -> do
        Some SStr str' <- elaborate str
        pure $ Some SInteger $ CoreProp $ StrLength str'
      (SModulus, [x, y]) -> do
        Some SInteger x' <- elaborate x
        Some SInteger y' <- elaborate y
        pure $ Some SInteger $ CoreProp $ Numerical $ ModOp x' y'
      (SBankersRound, [x]) -> do
        Some SDecimal x' <- elaborate x
        pure $ Some SInteger $ CoreProp $ Numerical $ RoundingLikeOp1 Round x'
      (SBankersRound, [x, y]) -> do
        Some SDecimal x' <- elaborate x
        Some SInteger y' <- elaborate y
        pure $ Some SDecimal $ CoreProp $ Numerical $ RoundingLikeOp2 Round x' y'
      (SAddition, [x, y]) -> do
        case ty' of
          SStr -> do
            Some SStr x' <- elaborate x
            Some SStr y' <- elaborate y
            pure $ Some SStr $ PStrConcat x' y'
          SInteger -> do
            Some SInteger x' <- elaborate x
            Some SInteger y' <- elaborate y
            pure $ Some SInteger $ CoreProp $ Numerical $ IntArithOp Add x' y'
          SDecimal -> do
            Some tyX x' <- elaborate x
            Some tyY y' <- elaborate y
            case (tyX, tyY) of
              (SDecimal, SDecimal) -> pure $ Some SDecimal $ CoreProp $ Numerical $ DecArithOp    Add x' y'
              (SDecimal, SInteger) -> pure $ Some SDecimal $ CoreProp $ Numerical $ DecIntArithOp Add x' y'
              (SInteger, SDecimal) -> pure $ Some SDecimal $ CoreProp $ Numerical $ IntDecArithOp Add x' y'
              _                    -> throwError "TODO (13.5)"
          _ -> throwError "TODO (13.75)"
      _ -> throwError "TODO (14)"
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
