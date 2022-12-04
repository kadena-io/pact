{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveTraversable #-}

module Pact.Core.Type
 ( PrimType(..)
 , Type(..)
 , TypeScheme(..)
 , pattern TyInt
 , pattern TyDecimal
 , pattern TyTime
 , pattern TyBool
 , pattern TyString
 , pattern TyUnit
 , pattern (:~>)
 , tyFunToArgList
 , typeOfLit
 , BuiltinTC(..)
 , Pred(..)
 , renderType
 , renderPred
 ) where

import Data.Text(Text)
import Data.List.NonEmpty(NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T

import Pact.Core.Literal
import Pact.Core.Pretty(Pretty(..), (<+>))

import qualified Pact.Core.Pretty as Pretty

data PrimType =
  PrimInt |
  PrimDecimal |
  PrimTime |
  PrimBool |
  PrimString |
  PrimUnit
  deriving (Eq,Ord,Show)

instance Pretty PrimType where
  pretty = \case
    PrimInt -> "integer"
    PrimDecimal -> "decimal"
    PrimTime -> "time"
    PrimBool -> "bool"
    PrimString -> "string"
    PrimUnit -> "unit"

-- Todo: caps are a bit strange here
-- same with defpacts. Not entirely sure how to type those yet.
-- | Our internal core type language
--   Tables, rows and and interfaces are quite similar,
--    t ::= B
--      |   v
--      |   t -> t
--      |   row
--      |   list<t>
--      |   interface name row
--
--    row  ::= {name:t, row*}
--    row* ::= name:t | ϵ
data Type n
  = TyVar n
  -- ^ Type variables.
  | TyPrim PrimType
  -- ^ Built-in types
  | TyFun (Type n) (Type n)
  -- ^ Row objects
  | TyList (Type n)
  -- ^ List aka [a]
  | TyGuard
  -- ^ Type of Guards.
  | TyForall (NonEmpty n) (Type n)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

pattern TyInt :: Type n
pattern TyInt = TyPrim PrimInt

pattern TyDecimal :: Type n
pattern TyDecimal = TyPrim PrimDecimal

pattern TyTime :: Type n
pattern TyTime = TyPrim PrimTime

pattern TyBool :: Type n
pattern TyBool = TyPrim PrimBool

pattern TyString :: Type n
pattern TyString = TyPrim PrimString

pattern TyUnit :: Type n
pattern TyUnit = TyPrim PrimUnit

pattern (:~>) :: Type n -> Type n -> Type n
pattern l :~> r  = TyFun l r

infixr 5 :~>

-- Built in typeclasses
data BuiltinTC
  = Eq
  | Ord
  | Show
  | Add
  | Num
  | ListLike
  | Fractional
  deriving (Show, Eq, Ord)

instance Pretty BuiltinTC where
  pretty = \case
    Eq -> "Eq"
    Ord -> "Ord"
    Show -> "Show"
    Add -> "Add"
    Num -> "Num"
    ListLike -> "ListLike"
    Fractional -> "Fractional"

-- Note, no superclasses, for now
data Pred tv
  = Pred BuiltinTC (Type tv)
  deriving (Show, Eq, Functor, Foldable, Traversable)

data TypeScheme tv =
  TypeScheme [tv] [Pred tv]  (Type tv)
  deriving Show

tyFunToArgList :: Type n -> Maybe ([Type n], Type n)
tyFunToArgList (TyFun l r) =
  unFun [l] r
  where
  unFun args (TyFun l' r') = unFun (l':args) r'
  unFun args ret = Just (reverse args, ret)
tyFunToArgList _ = Nothing

typeOfLit :: Literal -> Type n
typeOfLit = TyPrim . \case
  LString{} -> PrimString
  LInteger{} -> PrimInt
  LDecimal{} -> PrimDecimal
  LBool{} -> PrimBool
  LTime{} -> PrimTime
  LUnit -> PrimUnit

renderType :: (Pretty n) => Type n -> Text
renderType = T.pack . show . pretty

renderPred :: (Pretty n) => Pred n -> Text
renderPred = T.pack . show . pretty

instance Pretty n => Pretty (Pred n) where
  pretty (Pred tc ty) = pretty tc <>  Pretty.angles (pretty ty)

instance Pretty n => Pretty (Type n) where
  pretty = \case
    TyVar n -> pretty n
    TyPrim p -> pretty p
    TyGuard -> "guard"
    TyFun l r -> fnParens l <+> "->" <+> pretty r
      where
        fnParens t@TyFun{} = Pretty.parens (pretty t)
        fnParens t = pretty t
    TyList l -> "list" <+> liParens l
      where
      liParens t@TyVar{} = pretty t
      liParens t@TyPrim{} = pretty t
      liParens t = Pretty.parens (pretty t)
    TyForall as ty ->
      "∀" <> render (NE.toList as) "*" <> "." <> pretty ty
      where
      render xs suffix =
        Pretty.hsep $ fmap (\n -> Pretty.parens (pretty n <> ":" <+> suffix)) xs

instance Pretty tv => Pretty (TypeScheme tv) where
  pretty (TypeScheme tvs preds ty) =
    quant tvs <> qual preds <> pretty ty
    where
    renderTvs xs suffix =
      Pretty.hsep $ fmap (\n -> Pretty.parens (pretty n <> ":" <+> suffix)) xs
    quant [] = mempty
    quant as =
      "∀" <> renderTvs as "*" <> ". "
    qual [] = mempty
    qual as =
      Pretty.parens (Pretty.hsep $ Pretty.punctuate "," (pretty <$> as)) <+> "=> "
