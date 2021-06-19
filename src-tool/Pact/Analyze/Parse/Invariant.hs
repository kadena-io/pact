-- | Conversion from 'Exp' to the 'Invariant' language via the 'Prop' language.
module Pact.Analyze.Parse.Invariant (expToInvariant) where

import           Data.HashMap.Strict      (HashMap)
import qualified Data.HashMap.Strict      as HM
import           Data.Map                 (Map)
import           Data.Text                (Text)
import           Prelude                  hiding (exp)

import           Pact.Types.Lang          hiding (KeySet, KeySetName, SchemaVar,
                                           TableName, Type)

import           Pact.Analyze.Parse.Prop  (expToProp)
import           Pact.Analyze.Types

expToInvariant
  :: VarId
  -- ^ ID to start issuing from
  -> Map Text VarId
  -- ^ Environment mapping names to var IDs
  -> Map VarId EType
  -- ^ Environment mapping var IDs to their types
  -> HashMap Text EProp
  -- ^ Environment mapping names to constants
  -> SingTy a
  -- ^ The expected type of the invariant
  -> Exp Info
  -- ^ Exp to convert
  -> Either String (Invariant a)
expToInvariant genStart nameEnv tyEnv consts ty body =
  propToInvariant =<<
    expToProp (TableMap mempty) genStart nameEnv tyEnv consts HM.empty ty body