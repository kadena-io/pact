{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE TemplateHaskell       #-}

-- | Types related to parsing from 'Exp' to 'Prop' and 'Invariant'.
module Pact.Analyze.Parse.Types where

import           Control.Applicative        (Alternative)
import           Control.Lens               (makeLenses, (<&>), (&), _2, (%~))
import           Control.Monad.Except       (MonadError (throwError))
import           Control.Monad.Fail
import           Control.Monad.Reader       (ReaderT)
import           Control.Monad.State.Strict (StateT)
import qualified Data.HashMap.Strict        as HM
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Set                   (Set)
import           Data.String                (fromString, IsString)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Prelude                    hiding (exp)

import           Pact.Types.Lang            (AtomExp (..),
                                             Exp (EAtom, ELiteral, ESeparator),
                                             ListDelimiter (..), ListExp (..),
                                             Literal (LString), LiteralExp (..),
                                             Separator (..), SeparatorExp (..))
import qualified Pact.Types.Lang            as Pact
import           Pact.Types.Typecheck       (UserType)
import           Pact.Types.Pretty

import           Pact.Analyze.Feature       hiding (Doc, Type, Var, ks, obj,
                                             str, TyFun, TyVar)
import           Pact.Analyze.Types         hiding (Ty(..))

-- @PreProp@ stands between @Exp@ and @Prop@.
--
-- The conversion from @Exp@ is light, handled in @expToPreProp@.
data PreProp a
  -- literals
  = PreIntegerLit Integer
  | PreStringLit  Text
  | PreDecimalLit Decimal
  | PreTimeLit    Time
  | PreBoolLit    Bool
  | PreListLit    [a]

  -- identifiers
  | PreAbort
  | PreSuccess
  | PreResult

  -- In conversion from @Exp@ to @PreProp@ we maintain a distinction between
  -- bound and unbound variables. Bound (@PreVar@) variables are bound inside
  -- quantifiers. Unbound (@PreGlobalVar@) variables either refer to a property
  -- definition or a table.
  | PreVar       VarId Text
  | PreGlobalVar       Text

  -- quantifiers
  | PreForall VarId Text QType a
  | PreExists VarId Text QType a

  -- applications
  | PreApp Text [a]

  | PreAt a a
  | PrePropRead a a a
  | PreLiteralObject (Map Text a)
  deriving (Eq, Show)

instance Pretty a => Pretty (PreProp a) where
  pretty = \case
    PreIntegerLit i   -> pretty i
    PreStringLit t    -> dquotes $ pretty t
    PreDecimalLit d   -> pretty d
    PreTimeLit t      -> pretty (Pact.LTime (toPact timeIso t))
    PreBoolLit True   -> "true"
    PreBoolLit False  -> "false"
    PreListLit lst    -> commaBrackets $ fmap pretty lst
    PreAbort          -> pretty STransactionAborts
    PreSuccess        -> pretty STransactionSucceeds
    PreResult         -> pretty SFunctionResult
    PreVar _id name   -> pretty name
    PreGlobalVar name -> pretty name

    PreForall _vid name qty prop ->
      "(" <> pretty SUniversalQuantification <> " (" <> pretty name <> ":" <>
        pretty qty <> ") " <> pretty prop <> ")"
    PreExists _vid name qty prop ->
      "(" <> pretty SExistentialQuantification <> " (" <> pretty name <> ":" <>
        pretty qty <> ") " <> pretty prop <> ")"
    PreApp name applicands ->
      "(" <> pretty name <> " " <> hsep (map pretty applicands) <> ")"
    PreAt objIx obj ->
      "(" <> pretty SObjectProjection <> " " <> pretty objIx <> " " <>
        pretty obj <> ")"
    PrePropRead tn rk ba ->
      "(" <> pretty SPropRead <> " '" <> pretty tn <> " " <> pretty rk <> " " <>
        pretty ba <> ")"
    PreLiteralObject obj -> commaBraces $ Map.toList obj <&> \(k, v) ->
      pretty k <> " := " <> pretty v


throwErrorT :: MonadError String m => Text -> m a
throwErrorT = throwError . T.unpack

throwErrorD :: MonadError String m => Doc -> m a
throwErrorD = throwError . renderCompactString'

-- TODO(joel): add location info
throwErrorIn :: (MonadError String m, Pretty a) => a -> Doc -> m b
throwErrorIn exp msg = throwError $ renderCompactString' $
  "in " <> pretty exp <> ", " <> msg

textToQuantifier
  :: Text -> Maybe (VarId -> Text -> QType -> Fix PreProp -> Fix PreProp)
textToQuantifier = \case
  SUniversalQuantification   -> Just $ \a b c d -> Fix $ PreForall a b c d
  SExistentialQuantification -> Just $ \a b c d -> Fix $ PreExists a b c d
  _                          -> Nothing

type TableEnv = TableMap (ColumnMap EType)

newtype Fix f = Fix (f (Fix f))

data PropCheckEnv = PropCheckEnv
  { _tableEnv          :: TableEnv
  , _quantifiedTables  :: Set TableName
  , _quantifiedColumns :: Set ColumnName

  , _varTys            :: Map VarId QType

  -- | User-defined properties
  , _definedProps      :: HM.HashMap Text (DefinedProperty (Fix PreProp))

  -- | Constants and vars bound within a user-defined property
  , _localVars         :: HM.HashMap Text EProp
  }

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
  | TyVar !VarId
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

newtype Options = Options ([[(Ty, Ty)]])

data CheckState = CheckState
  { _checkVarGen      :: !VarId
  , _checkConstraints :: ![Options]
  }
makeLenses ''CheckState

newtype EitherFail e a = EitherFail { _getEither :: Either e a }
    deriving (Show, Eq, Ord, Functor, Applicative, Alternative, Monad, MonadError e)

instance IsString str => MonadFail (EitherFail str) where
    fail = EitherFail . Left . fromString

type ParseEnv = Map Text VarId

type PropParse      = ReaderT ParseEnv (StateT VarId (Either String))
type PropCheck      = ReaderT PropCheckEnv (StateT CheckState (EitherFail String))
type InvariantParse = ReaderT [(Pact.Arg UserType, VarId)] (Either String)

makeLenses ''PropCheckEnv

pattern ParenList :: [Exp t] -> Exp t
pattern ParenList elems <- Pact.EList (ListExp elems Parens _i)

pattern BraceList :: [Exp t] -> Exp t
pattern BraceList elems <- Pact.EList (ListExp elems Braces _i)

pattern SquareList :: [Exp t] -> Exp t
pattern SquareList elems <- Pact.EList (ListExp elems Brackets _i)

pattern EAtom' :: Text -> Exp t
pattern EAtom' name <- EAtom (AtomExp name [] _i)

pattern ELiteral' :: Literal -> Exp t
pattern ELiteral' lit <- ELiteral (LiteralExp lit _i)

pattern EStrLiteral' :: Text -> Exp t
pattern EStrLiteral' lit <- ELiteral (LiteralExp (LString lit) _i)

pattern Colon' :: Exp t
pattern Colon' <- ESeparator (SeparatorExp Colon _i)
