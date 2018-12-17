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
import           Data.Type.Equality          ((:~:) (Refl), apply, TestEquality(testEquality))
import           GHC.TypeLits                (Symbol, KnownSymbol, symbolVal)
import           Type.Reflection (typeOf)

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

data family Sing :: k -> Type

data instance Sing (n :: [Mapping Symbol Ty]) where
  SNilMapping :: Sing ('[] :: [Mapping Symbol Ty])
  SConsMapping
    :: KnownSymbol k
    => Sing k -> Sing v -> Sing n
    -> Sing ((k ':-> v) ': n :: [Mapping Symbol Ty])

type SingMapping (a :: [Mapping Symbol Ty]) = Sing a

mappingKeys :: SingMapping a -> [String]
mappingKeys SNilMapping = []
mappingKeys (SConsMapping k _ m) = symbolVal k : mappingKeys m

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
singEq (SObject a) (SObject b) = apply Refl <$> singMappingEq a b
singEq _           _           = Nothing

singMappingEq
  :: forall (a :: [Mapping Symbol Ty]) (b :: [Mapping Symbol Ty]).
     Sing a -> Sing b -> Maybe (a :~: b)
singMappingEq SNilMapping SNilMapping = Just Refl
singMappingEq (SConsMapping k1 v1 n1) (SConsMapping k2 v2 n2) = do
  Refl <- testEquality (typeOf k1) (typeOf k2)
  Refl <- singEq v1 v2
  Refl <- singMappingEq n1 n2
  pure Refl
singMappingEq _ _ = Nothing

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
      showsM :: SingMapping a -> ShowS
      showsM SNilMapping = showString "SNilMapping"
      showsM (SConsMapping k v n) = showParen True $
          showString "SConsMapping "
        . showString (symbolVal k)
        . showString " "
        . shows v
        . showString " "
        . showsM n

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
      userShowM :: SingMapping a -> [Text]
      userShowM SNilMapping = []
      userShowM (SConsMapping k v n)
        = pack (symbolVal k) <> " := " <> pack (show v)
        : userShowM n

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
  sing = SObject SNilMapping

instance (KnownSymbol k, SingI v, SingI m)
  => SingI ('TyObject ((k ':-> v) ': m)) where
  sing = SObject (SConsMapping sing sing sing)

data instance Sing (n :: Symbol) = KnownSymbol n => SSym

instance KnownSymbol n => SingI n where
  sing = SSym

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
