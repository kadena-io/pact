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
import           Data.Text                   (intercalate, pack, Text)
import           Data.Type.Equality          ((:~:) (Refl), apply)
import           Data.Typeable               (Typeable)

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
  | TyObject [ Ty ]

data family Sing :: k -> Type

data instance Sing (n :: [Ty]) where
  SNil  ::                     Sing ('[]    :: [Ty])
  SCons :: (Typeable v, SingI v)
        => Sing v -> Sing n -> Sing (v ': n :: [Ty])

type SingList (a :: [Ty]) = Sing a

-- type family Map (f :: Ty -> k) (xs :: [Ty]) where
--    Map f '[]       = '[]
--    Map f (x ': xs) = f x ': Map f xs

data HListOf (f :: Ty -> *) (tys :: [Ty]) where
  NilOf  ::                          HListOf f '[]
  ConsOf :: f ty -> HListOf f tys -> HListOf f (ty ': tys)

data instance Sing (a :: Ty) where
  SInteger ::           Sing 'TyInteger
  SBool    ::           Sing 'TyBool
  SStr     ::           Sing 'TyStr
  STime    ::           Sing 'TyTime
  SDecimal ::           Sing 'TyDecimal
  SKeySet  ::           Sing 'TyKeySet
  SAny     ::           Sing 'TyAny
  SList    :: Sing a -> Sing ('TyList a)
  SObject  :: Sing a -> Sing ('TyObject a)

instance Eq (SingTy a) where
  _ == _ = True
instance Ord (SingTy a) where
  compare _ _ = EQ

type SingTy (a :: Ty) = Sing a

type TyTableName  = 'TyStr
type TyColumnName = 'TyStr
type TyRowKey     = 'TyStr
type TyKeySetName = 'TyStr

singEq :: forall (a :: Ty) (b :: Ty). Sing a -> Sing b -> Maybe (a :~: b)
singEq SInteger    SInteger    = Just Refl
singEq SBool       SBool       = Just Refl
singEq SStr        SStr        = Just Refl
singEq STime       STime       = Just Refl
singEq SDecimal    SDecimal    = Just Refl
singEq SKeySet     SKeySet     = Just Refl
singEq SAny        SAny        = Just Refl
singEq (SList   a) (SList   b) = apply Refl <$> singEq a b
singEq (SObject a) (SObject b) = apply Refl <$> singListEq a b
singEq _           _           = Nothing

singListEq
  :: forall (a :: [Ty]) (b :: [Ty]).
     Sing a -> Sing b -> Maybe (a :~: b)
singListEq SNil SNil = Just Refl
singListEq (SCons v1 n1) (SCons v2 n2) = do
  Refl <- singEq v1 v2
  Refl <- singListEq n1 n2
  pure Refl
singListEq _ _ = Nothing

type family ListElem (a :: Ty) where
  ListElem ('TyList a) = a

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
    SObject m -> showParen (p > 10) $ showString "SObject " . showsM m
    where
      showsM :: SingList a -> ShowS
      showsM SNil = showString "SNil"
      showsM (SCons v n) = showParen True $
        showString "SCons " . shows v . showString " " . showsM n

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
    SObject m -> "{ " <> intercalate ", " (userShowM m) <> " }"
    where
      userShowM :: SingList a -> [Text]
      userShowM SNil        = []
      userShowM (SCons v n) = pack (show v) : userShowM n

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
  sing = SObject SNil

instance (Typeable v, SingI v, SingI m) => SingI ('TyObject (v ': m)) where
  sing = SObject (SCons sing sing)

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
