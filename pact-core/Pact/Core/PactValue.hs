{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Pact.Core.PactValue
 ( PactValue(..)
 , _PLiteral
 , _PList
 , _PGuard
 , checkPvType
 , EnvData(..)
 , envMap ) where

import Control.Lens
import Data.Vector(Vector)
import Data.Maybe(isJust)
import Data.Map.Strict(Map)

import Pact.Core.Type
import Pact.Core.Names
import Pact.Core.Guards
import Pact.Core.Literal

data PactValue
  = PLiteral Literal
  | PList (Vector PactValue)
  | PGuard (Guard FullyQualifiedName PactValue)
  deriving (Eq, Show)

makePrisms ''PactValue

checkPvType :: Eq n => Type n -> PactValue -> Maybe (Type n)
checkPvType ty = \case
  PLiteral l -> let
    t = typeOfLit l
    in if t == ty then Just t else Nothing
  PGuard{} -> Just TyGuard
  PList l -> case ty of
    TyList t' | all (isJust . checkPvType t') l -> Just (TyList t')
    _ -> Nothing


newtype EnvData term
  = EnvData { _envMap :: Map Field term }
  deriving (Eq, Show)

envMap
  :: Lens (EnvData term)
          (EnvData term')
          (Map Field term)
          (Map Field term')
envMap f (EnvData m) = fmap EnvData (f m)
