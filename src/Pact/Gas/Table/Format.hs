{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}

module Pact.Gas.Table.Format(estimateFormatValues, estimateFormatText) where

import qualified Data.ByteString.Short as SBS
import Data.Decimal
import Data.Foldable
import qualified Data.Map as Map
import Data.Semigroup
import Data.Text (Text)
import qualified Data.Text as T
import Numeric.Natural
import GHC.Integer.Logarithms
import GHC.Int(Int(..))

import Pact.Types.Exp
import Pact.Types.Hash
import Pact.Types.PactValue
import Pact.Types.KeySet
import Pact.Types.Term

class ToPactStringGas ty where
  estimate :: ty -> Sum Natural

estimateList :: (ToPactStringGas ty, Foldable t) => t ty -> Sum Natural
estimateList = foldMap ((+ 1) . estimate)

instance ToPactStringGas ty => ToPactStringGas (Maybe ty) where
  estimate = maybe 0 estimate

instance ToPactStringGas Text where
  estimate = fromIntegral . T.length

instance ToPactStringGas Integer where
  estimate n = fromIntegral $ I# (integerLogBase# 10 n)

instance ToPactStringGas NamespaceName where
  estimate = estimate . _namespaceName

instance ToPactStringGas ModuleName where
  estimate (ModuleName name mns) = estimate name <> estimate mns

instance ToPactStringGas QualifiedName where
  estimate (QualifiedName mn t _info) = estimate mn <> estimate t

instance ToPactStringGas Name where
  estimate (QName qn) = estimate qn
  estimate (Name (BareName t _info)) = estimate t
  estimate (DName (DynamicName member refArg _set _info)) = estimate member <> estimate refArg
  estimate (FQName (FullyQualifiedName name mn mh)) = estimate name <> estimate mn <> fromIntegral (SBS.length $ unHash mh)

instance ToPactStringGas PactId where
  estimate (PactId t) = estimate t

instance ToPactStringGas (Guard PactValue) where
  estimate (GPact (PactGuard (PactId t1) t2)) = estimate t1 <> estimate t2
  estimate (GKeySet (KeySet ks predFun)) = estimateList (_pubKey <$> toList ks) <> estimate predFun
  estimate (GKeySetRef (KeySetName txt mn)) = estimate txt <> estimate mn
  estimate (GModule (ModuleGuard mn t)) = estimate mn <> estimate t
  estimate (GUser (UserGuard name args)) = estimate name <> estimateList args
  estimate (GCapability (CapabilityGuard qn args mid)) = estimate qn <> estimateList args <> estimate mid

instance ToPactStringGas PactValue where
  estimate (PLiteral lit) =
    case lit of
         LString txt -> estimate txt
         LInteger n -> estimate n
         LDecimal (Decimal _ n) -> estimate n
         LBool _ -> 5 -- max length of bool
         LTime _ -> 20 * 5 -- expected length of formatted time, times cost of manipulating [Char]
  estimate (PList vec) = estimateList vec
  estimate (PObject (ObjectMap m)) = Map.foldMapWithKey estimateKV m <> fromIntegral (length m)
    where
      estimateKV (FieldKey k) v = estimate k <> estimate v
  estimate (PGuard g) = estimate g
  estimate (PModRef (ModRef mn _sm _n)) = estimate mn

estimateFormatText :: Text -> Natural
estimateFormatText = getSum . estimate

estimateFormatValues :: Foldable t => t PactValue -> Natural
estimateFormatValues = getSum . estimateList
