{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TemplateHaskell       #-}

module Pact.Analyze.Types
  ( module Pact.Analyze.Types.Languages
  , module Pact.Analyze.Types.Model
  , module Pact.Analyze.Types.Numerical
  , module Pact.Analyze.Types.Shared
  , module Pact.Analyze.Types.UserShow

  , Check(..)
  , Float(float)
  , HasVarId(varId)
  , Quantifier(..)
  , Table(..)

  , checkGoal
  , genId
  , genVarId
  , tableInvariants
  , tableName
  , tableType

  , pattern ColumnNameLit
  , pattern TableNameLit
  ) where

import           Control.Lens                 (Lens', makeLenses, use, (+=))
import           Control.Monad.State.Strict   (MonadState)
import           Data.Text                    (Text)
import           Prelude                      hiding (Float)

import qualified Pact.Types.Typecheck         as TC

import           Pact.Analyze.Types.Languages
import           Pact.Analyze.Types.Model
import           Pact.Analyze.Types.Numerical
import           Pact.Analyze.Types.Shared
import           Pact.Analyze.Types.UserShow

data Quantifier
  = Forall' VarId Text QType
  | Exists' VarId Text QType

class Float a where
  float :: Prop a -> ([Quantifier], Prop a)

genId :: (MonadState s m, Num i) => Lens' s i -> m i
genId l = do
  i <- use l
  l += 1
  pure i

class HasVarId s where
  varId :: Lens' s VarId

instance HasVarId VarId where
  varId = id

genVarId :: (MonadState s m, HasVarId s) => m VarId
genVarId = genId varId

data Check
  = PropertyHolds !(Prop Bool) -- valid, assuming success
  | Satisfiable   !(Prop Bool) -- sat,   not assuming success
  | Valid         !(Prop Bool) -- valid, not assuming success
  deriving Show

checkGoal :: Check -> Goal
checkGoal (PropertyHolds _) = Validation
checkGoal (Satisfiable _)   = Satisfaction
checkGoal (Valid _)         = Validation

data Table = Table
  { _tableName       :: Text
  , _tableType       :: TC.UserType
  , _tableInvariants :: [Located (Invariant Bool)]
  } deriving (Show)

pattern TableNameLit :: String -> Prop TableName
pattern TableNameLit str = PLit (TableName str)

pattern ColumnNameLit :: String -> Prop ColumnName
pattern ColumnNameLit str = PLit (ColumnName str)

makeLenses ''Table
