{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE TemplateHaskell       #-}
module Pact.Analyze.Parse.Types where

import           Control.Lens               (makeLenses)
import           Control.Monad.Except       (MonadError (throwError))
import           Control.Monad.Reader       (ReaderT)
import           Control.Monad.State.Strict (StateT)
import qualified Data.HashMap.Strict        as HM
import           Data.Map                   (Map)
import           Data.Set                   (Set)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Prelude                    hiding (exp)

import           Pact.Types.Lang            (AtomExp (..), Exp (..),
                                             ListDelimiter (..), ListExp (..),
                                             Literal (LString), LiteralExp (..),
                                             Separator (..), SeparatorExp (..))
import qualified Pact.Types.Lang            as Pact
import           Pact.Types.Typecheck       (UserType)
import           Pact.Types.Util            (tShow)

import           Pact.Analyze.Feature       hiding (Type, Var, ks, obj, str)
import           Pact.Analyze.Types

-- @PreProp@ stands between @Exp@ and @Prop@.
--
-- The conversion from @Exp@ is light, handled in @expToPreProp@.
data PreProp
  -- literals
  = PreIntegerLit Integer
  | PreStringLit  Text
  | PreDecimalLit Decimal
  | PreTimeLit    Time
  | PreBoolLit    Bool

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
  | PreForall VarId Text QType PreProp
  | PreExists VarId Text QType PreProp

  -- applications
  | PreApp Text [PreProp]

  | PreAt Text PreProp
  | PrePropRead PreProp PreProp PreProp
  | PreLiteralObject (Map Text PreProp)
  deriving (Eq, Show)

instance UserShow PreProp where
  userShowsPrec prec = \case
    PreIntegerLit i   -> tShow i
    PreStringLit t    -> tShow t
    PreDecimalLit d   -> userShow d
    PreTimeLit t      -> tShow (Pact.LTime (toPact timeIso t))
    PreBoolLit b      -> tShow (Pact.LBool b)

    PreAbort          -> STransactionAborts
    PreSuccess        -> STransactionSucceeds
    PreResult         -> SFunctionResult
    PreVar _id name   -> name
    PreGlobalVar name -> name

    PreForall _vid name qty prop ->
      "(" <> SUniversalQuantification <> " (" <> name <> ":" <> userShow qty <>
        ") " <> userShow prop <> ")"
    PreExists _vid name qty prop ->
      "(" <> SExistentialQuantification <> " (" <> name <> ":" <>
        userShow qty <> ") " <> userShow prop <> ")"
    PreApp name applicands ->
      "(" <> name <> " " <> T.unwords (map userShow applicands) <> ")"
    PreAt objIx obj ->
      "(" <> SObjectProjection <> " '" <> objIx <> " " <> userShow obj <> ")"
    PrePropRead tn rk ba ->
      "(" <> SPropRead <> " '" <> userShow tn <> " " <> userShow rk <> " " <>
        userShow ba <> ")"
    PreLiteralObject obj ->
      userShowsPrec prec obj


throwErrorT :: MonadError String m => Text -> m a
throwErrorT = throwError . T.unpack

-- TODO(joel): add location info
throwErrorIn :: (MonadError String m, UserShow a) => a -> Text -> m b
throwErrorIn exp text = throwError $ T.unpack $
  "in " <> userShow exp <> ", " <> text

textToQuantifier
  :: Text -> Maybe (VarId -> Text -> QType -> PreProp -> PreProp)
textToQuantifier = \case
  SUniversalQuantification   -> Just PreForall
  SExistentialQuantification -> Just PreExists
  _                          -> Nothing

type TableEnv = TableMap (ColumnMap EType)

data PropCheckEnv = PropCheckEnv
  { _varTys            :: Map VarId QType
  , _tableEnv          :: TableEnv
  , _quantifiedTables  :: Set TableName
  , _quantifiedColumns :: Set ColumnName

  -- User-defined properties
  , _definedProps      :: HM.HashMap Text (DefinedProperty PreProp)

  -- Vars bound within a user-defined property
  , _localVars         :: HM.HashMap Text EProp
  }

type ParseEnv = Map Text VarId

type PropParse      = ReaderT ParseEnv (StateT VarId (Either String))
type PropCheck      = ReaderT PropCheckEnv (Either String)
type InvariantParse = ReaderT [(Pact.Arg UserType, VarId)] (Either String)

makeLenses ''PropCheckEnv

pattern ParenList :: [Exp t] -> Exp t
pattern ParenList elems <- EList (ListExp elems Parens _i)

pattern BraceList :: [Exp t] -> Exp t
pattern BraceList elems <- EList (ListExp elems Braces _i)

pattern SquareList :: [Exp t] -> Exp t
pattern SquareList elems <- EList (ListExp elems Brackets _i)

pattern EAtom' :: Text -> Exp t
pattern EAtom' name <- EAtom (AtomExp name [] _i)

pattern ELiteral' :: Literal -> Exp t
pattern ELiteral' lit <- ELiteral (LiteralExp lit _i)

pattern EStrLiteral' :: Text -> Exp t
pattern EStrLiteral' lit <- ELiteral (LiteralExp (LString lit) _i)

pattern Colon' :: Exp t
pattern Colon' <- ESeparator (SeparatorExp Colon _i)
