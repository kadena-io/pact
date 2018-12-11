{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE Rank2Types                 #-}

module Pact.Analyze.Types.Types where

import           Data.Kind                   (Type)
import           Data.Semigroup              ((<>))
import           Data.Type.Equality          ((:~:) (Refl), apply)
import           GHC.TypeLits (Symbol)

import           Pact.Analyze.Types.Map
import           Pact.Analyze.Types.UserShow


data Ty
  = TyInteger
  | TyBool
  | TyStr
  | TyTime
  | TyDecimal
  | TyKeySet
  | TyAny
  | TyList Ty
  | TyObject [ Mapping Symbol Ty ]

type family ListElem (a :: Ty) where
  ListElem ('TyList a) = a

type TyTableName  = 'TyStr
type TyColumnName = 'TyStr
type TyRowKey     = 'TyStr
type TyKeySetName = 'TyStr

data family Sing :: k -> Type

data instance Sing (a :: Ty) where
  SInteger ::           Sing 'TyInteger
  SBool    ::           Sing 'TyBool
  SStr     ::           Sing 'TyStr
  STime    ::           Sing 'TyTime
  SDecimal ::           Sing 'TyDecimal
  SKeySet  ::           Sing 'TyKeySet
  SAny     ::           Sing 'TyAny
  SList    :: Sing a -> Sing ('TyList a)
  SObject  :: Map m  -> Sing ('TyObject m)

type SingTy (a :: Ty) = Sing a

instance Show (SingTy ty) where
  showsPrec p = \case
    SInteger  -> showString "SInteger"
    SBool     -> showString "SBool"
    SStr      -> showString "SStr"
    STime     -> showString "STime"
    SDecimal  -> showString "SDecimal"
    SKeySet   -> showString "SKeySet"
    SAny      -> showString "SAny"
    SList a   -> showParen (p > 10) $ showString "SList "   . showsPrec 11 a
    -- showParen (p > 10) $ showString "SObject " . showsPrec 11 m
    SObject _ -> showString "SObject"

instance UserShow (SingTy ty) where
  userShowPrec _ = \case
    SInteger  -> "integer"
    SBool     -> "bool"
    SStr      -> "string"
    STime     -> "time"
    SDecimal  -> "decimal"
    SKeySet   -> "keyset"
    SAny      -> "*"
    SList a   -> "[" <> userShow a <> "]"
    SObject _ -> "object" -- TODO: show fields

singEq :: forall (a :: Ty) (b :: Ty). Sing a -> Sing b -> Maybe (a :~: b)
singEq SInteger    SInteger    = Just Refl
singEq SBool       SBool       = Just Refl
singEq SStr        SStr        = Just Refl
singEq STime       STime       = Just Refl
singEq SDecimal    SDecimal    = Just Refl
singEq SKeySet     SKeySet     = Just Refl
singEq SAny        SAny        = Just Refl
singEq (SList   a) (SList   b) = apply Refl <$> singEq a b
singEq (SObject a) (SObject b) = apply Refl <$> mapEq  a b
singEq _           _           = Nothing

singSimple :: Sing (a :: Ty) -> Maybe (Sing a)
singSimple ty = case ty of
  SInteger  -> Just ty
  SBool     -> Just ty
  SStr      -> Just ty
  STime     -> Just ty
  SDecimal  -> Just ty
  SKeySet   -> Just ty
  SAny      -> Just ty
  SList{}   -> Nothing
  SObject{} -> Nothing

class SingI a where
  sing :: Sing a

instance SingI 'TyInteger where
  sing = SInteger

instance SingI 'TyBool where
  sing = SBool

instance SingI 'TyStr where
  sing = SStr

instance SingI 'TyTime where
  sing = STime

instance SingI 'TyDecimal where
  sing = SDecimal

instance SingI 'TyKeySet where
  sing = SKeySet

instance SingI 'TyAny where
  sing = SAny

instance SingI a => SingI ('TyList a) where
  sing = SList sing

instance SingI ('TyObject '[]) where
  sing = SObject singMap

class SingMap m where
  singMap :: Map m

instance SingMap '[] where
  singMap = Empty

type family IsSimple (ty :: Ty) :: Bool where
  IsSimple ('TyList _)   = 'False
  IsSimple ('TyObject _) = 'False
  IsSimple _             = 'True

type family IsList (ty :: Ty) :: Bool where
  IsList ('TyList _) = 'True
  IsList _           = 'False

type family IsObject (ty :: Ty) :: Bool where
  IsObject ('TyObject _) = 'True
  IsObject _             = 'False
