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

module Pact.Analyze.Types.Types where

import           Data.Constraint             (Dict (Dict))
import           Data.Constraint.Extras
import           Data.Semigroup              ((<>))
import           Data.Type.Equality          ((:~:) (Refl), apply)

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
  | TyObject

type family ListElem (a :: Ty) where
  ListElem ('TyList a) = a

listElem :: SingTy 'ListK a -> SingTy 'SimpleK (ListElem a)
listElem (SList ty) = ty

type TyTableName  = 'TyStr
type TyColumnName = 'TyStr
type TyRowKey     = 'TyStr
type TyKeySetName = 'TyStr

data Kind = SimpleK | ListK | ObjectK

data SingTy :: Kind -> Ty -> * where
  SInteger ::                      SingTy 'SimpleK 'TyInteger
  SBool    ::                      SingTy 'SimpleK 'TyBool
  SStr     ::                      SingTy 'SimpleK 'TyStr
  STime    ::                      SingTy 'SimpleK 'TyTime
  SDecimal ::                      SingTy 'SimpleK 'TyDecimal
  SKeySet  ::                      SingTy 'SimpleK 'TyKeySet
  SAny     ::                      SingTy 'SimpleK 'TyAny
  SList    :: SingTy 'SimpleK a -> SingTy 'ListK   ('TyList a)
  SObject  ::                      SingTy 'ObjectK 'TyObject

instance Show (SingTy k ty) where
  showsPrec p = \case
    SInteger -> showString "SInteger"
    SBool    -> showString "SBool"
    SStr     -> showString "SStr"
    STime    -> showString "STime"
    SDecimal -> showString "SDecimal"
    SKeySet  -> showString "SKeySet"
    SAny     -> showString "SAny"
    SList a  -> showParen (p > 10) $ showString "SList " . showsPrec 11 a
    SObject  -> showString "SObject"

instance UserShow (SingTy k ty) where
  userShowPrec _ = \case
    SInteger -> "integer"
    SBool    -> "bool"
    SStr     -> "string"
    STime    -> "time"
    SDecimal -> "decimal"
    SKeySet  -> "keyset"
    SAny     -> "*"
    SList a  -> "[" <> userShow a <> "]"
    SObject  -> "object"

instance ArgDict (SingTy k) where
  type ConstraintsFor (SingTy k) c =
    ( c 'TyInteger
    , c 'TyBool
    , c 'TyStr
    , c 'TyTime
    , c 'TyDecimal
    , c 'TyKeySet
    , c 'TyAny
    , c ('TyList 'TyInteger)
    , c ('TyList 'TyBool)
    , c ('TyList 'TyStr)
    , c ('TyList 'TyTime)
    , c ('TyList 'TyDecimal)
    , c ('TyList 'TyKeySet)
    , c ('TyList 'TyAny)
    , c 'TyObject
    )

  type ConstraintsFor' (SingTy k) c g =
    ( c (g 'TyInteger)
    , c (g 'TyBool)
    , c (g 'TyStr)
    , c (g 'TyTime)
    , c (g 'TyDecimal)
    , c (g 'TyKeySet)
    , c (g 'TyAny)
    , c (g ('TyList 'TyInteger))
    , c (g ('TyList 'TyBool))
    , c (g ('TyList 'TyStr))
    , c (g ('TyList 'TyTime))
    , c (g ('TyList 'TyDecimal))
    , c (g ('TyList 'TyKeySet))
    , c (g ('TyList 'TyAny))
    , c (g 'TyObject)
    )

  argDict :: ConstraintsFor (SingTy k) c => SingTy k a -> Dict (c a)
  argDict = \case
    SInteger       -> Dict
    SBool          -> Dict
    SStr           -> Dict
    STime          -> Dict
    SDecimal       -> Dict
    SKeySet        -> Dict
    SAny           -> Dict
    SList SInteger -> Dict
    SList SBool    -> Dict
    SList SStr     -> Dict
    SList STime    -> Dict
    SList SDecimal -> Dict
    SList SKeySet  -> Dict
    SList SAny     -> Dict
    SObject        -> Dict

  -- argDict' :: ConstraintsFor' (SingTy k) c g => (SingTy k) a -> Dict (c (g a))
  argDict' = \case
    SInteger       -> Dict
    SBool          -> Dict
    SStr           -> Dict
    STime          -> Dict
    SDecimal       -> Dict
    SKeySet        -> Dict
    SAny           -> Dict
    SList SInteger -> Dict
    SList SBool    -> Dict
    SList SStr     -> Dict
    SList STime    -> Dict
    SList SDecimal -> Dict
    SList SKeySet  -> Dict
    SList SAny     -> Dict
    SObject        -> Dict

singEq :: SingTy k1 a -> SingTy k2 b -> Maybe (a :~: b)
singEq SInteger  SInteger  = Just Refl
singEq SBool     SBool     = Just Refl
singEq SStr      SStr      = Just Refl
singEq STime     STime     = Just Refl
singEq SDecimal  SDecimal  = Just Refl
singEq SKeySet   SKeySet   = Just Refl
singEq SAny      SAny      = Just Refl
singEq (SList a) (SList b) = apply Refl <$> singEq a b
singEq SObject   SObject   = Just Refl
singEq _         _         = Nothing

singEqB :: SingTy k1 a -> SingTy k2 b -> Bool
singEqB a b = case singEq a b of
  Just Refl -> True
  Nothing   -> False

singCase
  :: SingTy k a
  -> (k :~: 'SimpleK -> b)
  -> (k :~: 'ListK   -> b)
  -> (k :~: 'ObjectK -> b)
  -> b
singCase sing kSimple kList kObject = case sing of
  SInteger -> kSimple Refl
  SBool    -> kSimple Refl
  SStr     -> kSimple Refl
  STime    -> kSimple Refl
  SDecimal -> kSimple Refl
  SKeySet  -> kSimple Refl
  SAny     -> kSimple Refl
  SList _  -> kList   Refl
  SObject  -> kObject Refl

refineSimple :: SingTy k a -> Maybe (SingTy 'SimpleK a)
refineSimple ty = case ty of
  SInteger -> Just ty
  SBool    -> Just ty
  SStr     -> Just ty
  STime    -> Just ty
  SDecimal -> Just ty
  SKeySet  -> Just ty
  SAny     -> Just ty
  _        -> Nothing
