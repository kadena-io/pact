{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TemplateHaskell       #-}

-- | Toplevel module for types related to symbolic analysis of Pact programs.
module Pact.Analyze.Types
  ( module Pact.Analyze.Types.Capability
  , module Pact.Analyze.Types.Languages
  , module Pact.Analyze.Types.Model
  , module Pact.Analyze.Types.Numerical
  , module Pact.Analyze.Types.ObjUtil
  , module Pact.Analyze.Types.Shared
  , module Pact.Analyze.Types.Types

  , Check(..)
  , HasVarId(varId)
  , Quantifier(..)
  , Table(..)
  , CheckableType(..)
  , TypecheckableRefs(..)
  , defuns
  , defpacts
  , defconsts

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
import qualified Data.HashMap.Strict          as HM
import           Prelude                      hiding (Float)

import qualified Pact.Types.Typecheck         as TC
import           Pact.Types.Runtime           (Ref)

import           Pact.Analyze.Types.Capability
import           Pact.Analyze.Types.Languages
import           Pact.Analyze.Types.Model
import           Pact.Analyze.Types.Numerical
import           Pact.Analyze.Types.ObjUtil
import           Pact.Analyze.Types.Shared
import           Pact.Analyze.Types.Types

data Quantifier
  = Forall' VarId Text QType
  | Exists' VarId Text QType

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
  = PropertyHolds (Prop 'TyBool) -- valid, assuming success
  | SucceedsWhen  (Prop 'TyBool) -- property implies transaction success (validation)
  | FailsWhen     (Prop 'TyBool) -- property implies transaction failure (validation)
  | Satisfiable   (Prop 'TyBool) -- sat,   not assuming success
  | Valid         (Prop 'TyBool) -- valid, not assuming success

instance Show Check where
  showsPrec p c = showParen (p > 10) $ case c of
    PropertyHolds prop -> showString "PropertyHolds " . showsTm 11 prop
    SucceedsWhen prop  -> showString "SucceedsWhen "  . showsTm 11 prop
    FailsWhen prop     -> showString "FailsWhen "     . showsTm 11 prop
    Satisfiable prop   -> showString "Satisfiable "   . showsTm 11 prop
    Valid prop         -> showString "Valid "         . showsTm 11 prop

checkGoal :: Check -> Goal
checkGoal (PropertyHolds _) = Validation
checkGoal (SucceedsWhen _)  = Validation
checkGoal (FailsWhen _)     = Validation
checkGoal (Satisfiable _)   = Satisfaction
checkGoal (Valid _)         = Validation

data Table = Table
  { _tableName       :: Text
  , _tableType       :: TC.UserType
  , _tableInvariants :: [Located (Invariant 'TyBool)]
  } deriving Show

pattern TableNameLit :: String -> Prop TyTableName
pattern TableNameLit str = StrLit str

pattern ColumnNameLit :: String -> Prop TyColumnName
pattern ColumnNameLit str = StrLit str

-- | The three things we can check properties of
data CheckableType
  = CheckDefun
  | CheckDefpact
  | CheckDefconst
  | CheckPactStep

makeLenses ''Table

data TypecheckableRefs = TypecheckableRefs
  { _defuns    :: HM.HashMap Text Ref
  , _defpacts  :: HM.HashMap Text Ref
  , _defconsts :: HM.HashMap Text Ref
  }

makeLenses ''TypecheckableRefs
