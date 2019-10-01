{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MagicHash #-}

-- |
-- Module      :  Pact.Types.Term
-- Copyright   :  (C) 2019 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- 'PactValue' is a type for marshalling term values in Pact's
-- "front end", to address the issue where 'Term Name' cannot
-- be safely or meaningfully represented outside of the interpreter.
-- 'PactValue' is needed for reliable reproduction of receipt
-- results and continuation arguments in the SPV process.
--

module Pact.Types.PactValue
  ( PactValue(..)
  , toPactValue
  , toPactValueLenient
  , fromPactValue
  ) where

import Control.Applicative ((<|>))
import Control.DeepSeq (NFData)
import qualified Data.Aeson as A
import Data.Aeson hiding (Value(..))
import Data.Default (def)
import Data.Text (Text)
import Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Generics
import qualified Data.Map.Strict as M
import Data.Decimal
import Data.Thyme hiding (Vector)
import qualified Data.List as L
import qualified Data.ByteString.UTF8 as BS
import Text.Trifecta.Delta (Delta(..))
import GHC.Integer.GMP.Internals (Integer(..),BigNat(..))
import qualified GHC.Prim as BA
import GHC.Exts (Int(..))

import Pact.Types.Exp (Literal(..))
import Pact.Types.Pretty (Pretty(..),pretty,renderCompactText)
import Pact.Types.Term
import Pact.Types.Type (Type(TyAny))
import Pact.Types.Info (Info(..),Code(..),Parsed(..))


data PactValue
  = PLiteral Literal
  | PList (Vector PactValue)
  | PObject (ObjectMap PactValue)
  | PGuard (Guard PactValue)
  deriving (Eq,Show,Generic,Ord)

instance NFData PactValue

instance ToJSON PactValue where
  toJSON (PLiteral l) = toJSON l
  toJSON (PObject o) = toJSON o
  toJSON (PList v) = toJSON v
  toJSON (PGuard x) = toJSON x


instance FromJSON PactValue where
  parseJSON v =
    (PLiteral <$> parseJSON v) <|>
    (PList <$> parseJSON v) <|>
    (PGuard <$> parseJSON v) <|>
    (PObject <$> parseJSON v)

instance Pretty PactValue where
  pretty (PLiteral l) = pretty l
  pretty (PObject l) = pretty l
  pretty (PList l) = pretty (V.toList l)
  pretty (PGuard l) = pretty l

-- | Strict conversion.
toPactValue :: Term Name -> Either Text PactValue
toPactValue (TLiteral l _) = pure $ PLiteral l
toPactValue (TObject (Object o _ _ _) _) = PObject <$> traverse toPactValue o
toPactValue (TList l _ _) = PList <$> V.mapM toPactValue l
toPactValue (TGuard x _) = PGuard <$> traverse toPactValue x
toPactValue t = Left $ "Unable to convert Term: " <> renderCompactText t

fromPactValue :: PactValue -> Term Name
fromPactValue (PLiteral l) = TLiteral l def
fromPactValue (PObject o) = TObject (Object (fmap fromPactValue o) TyAny def def) def
fromPactValue (PList l) = TList (fmap fromPactValue l) TyAny def
fromPactValue (PGuard x) = TGuard (fmap fromPactValue x) def

-- | Lenient conversion, implying that conversion back won't necc. succeed.
-- Integers are coerced to Decimal for simple representation.
-- Non-value types are turned into their String representation.
toPactValueLenient :: Term Name -> PactValue
toPactValueLenient t = case toPactValue t of
  Right (PLiteral (LInteger l)) -> PLiteral (LDecimal (fromIntegral l))
  Right v -> v
  Left _ -> PLiteral $ LString $ renderCompactText t



-- Based on: http://wiki.haskell.org/GHC/Memory_Footprint
-- and https://stackoverflow.com/questions/3254758/memory-footprint-of-haskell-data-types
-- Assumes GHC

type Bytes = Int

wordSize32 :: Bytes
wordSize32 = 4

wordSize64 :: Bytes
wordSize64 = 8

-- TODO: switch the word value if running 32-bit vs 64-bit machine?
wordSize :: Int
wordSize = wordSize64


sizeOfPactValue :: PactValue -> Bytes
sizeOfPactValue pv = headerSize + fieldsSize
  where
    headerSize = 1 * wordSize
    fieldsSize = case pv of
      PLiteral l -> (1 * wordSize) + (sizeOfLiteral l)
      PList v -> (1 * wordSize) + (sizeOfVector v)
      PObject o -> (1 * wordSize) + (sizeOfObjectMap o)
      PGuard g -> (1 * wordSize) + (sizeOfGuard g)


sizeOfLiteral :: Literal -> Bytes
sizeOfLiteral l = headerSize + fieldsSize
  where
    headerSize = 1 * wordSize
    fieldsSize = case l of
      LString t -> (1 * wordSize) + (sizeOfText t)
      LInteger i -> (1 * wordSize) + (sizeOfInteger i)
      LDecimal d -> (1 * wordSize) + (sizeOfDecimal d)
      LBool _ -> (1 * wordSize) + 0
      LTime ti -> (1 * wordSize) + (sizeOfTime ti)


sizeOfVector :: Vector PactValue -> Bytes
sizeOfVector v = vectorSize
  where
    vectorSize =
      ((7 + vectorLength) * wordSize) + sizeOfContents
    vectorLength = V.length v
    sizeOfContents = V.foldl' (\acc pv -> acc + (sizeOfPactValue pv)) 0 v


-- A constructor defined with newtype is free
sizeOfObjectMap :: ObjectMap PactValue -> Bytes
sizeOfObjectMap (ObjectMap m) = sizeOfMap m 


sizeOfGuard :: Guard PactValue -> Bytes
sizeOfGuard g = headerSize + fieldsSize
  where
    headerSize = 1 * wordSize
    fieldsSize = case g of
      GPact pg -> (1 * wordSize) + (sizeOfPactGuard pg)
      GKeySet ks -> (1 * wordSize) + (sizeOfKeySet ks)
      GKeySetRef ksr -> (1 * wordSize) + (sizeOfKeySetRef ksr)
      GModule mg -> (1 * wordSize) + (sizeOfModuleGuard mg)
      GUser ug -> (1 * wordSize) + (sizeOfUserGuard ug)


sizeOfPactGuard :: PactGuard -> Bytes
sizeOfPactGuard pg = headerSize + fieldSize
  where
    headerSize = 1 * wordSize
    fieldSize = case pg of
      PactGuard pid pn ->
        (2 * wordSize) + (sizeOfPactId pid) + (sizeOfText pn)

sizeOfKeySet :: KeySet -> Bytes
sizeOfKeySet ks = headerSize + fieldSize
  where
    headerSize = 1 * wordSize
    fieldSize = case ks of
      KeySet pkArr ksPred ->
        (2 * wordSize) + (sizeOfArray sizeOfPublicKey pkArr) + (sizeOfName ksPred)

sizeOfKeySetRef :: KeySetName -> Bytes
sizeOfKeySetRef (KeySetName n) = sizeOfText n

sizeOfModuleGuard :: ModuleGuard -> Bytes
sizeOfModuleGuard mg = headerSize + fieldSize
  where
    headerSize = 1 * wordSize
    fieldSize = case mg of
      ModuleGuard md n ->
        (2 * wordSize) + (sizeOfModuleName md) + (sizeOfText n)

sizeOfUserGuard :: UserGuard PactValue -> Bytes
sizeOfUserGuard ug = headerSize + fieldSize
  where
    headerSize = 1 * wordSize
    fieldSize = case ug of
      UserGuard n arr -> (2 * wordSize) + (sizeOfName n) + (sizeOfArray sizeOfPactValue arr)

sizeOfName :: Name -> Bytes
sizeOfName n = headerSize + fieldSize
  where
    headerSize = 1 * wordSize
    fieldSize = case n of
      QName qn -> (1 * wordSize) + sizeOfQualifiedName qn
      Name bn -> (1 * wordSize) + sizeOfBareName bn

sizeOfQualifiedName :: QualifiedName -> Bytes
sizeOfQualifiedName qn = headerSize + fieldSize
  where
    headerSize = 1 * wordSize
    fieldSize = case qn of
      QualifiedName modName n i ->
        (3 * wordSize) + (sizeOfModuleName modName) + (sizeOfText n) + (sizeOfInfo i)

sizeOfBareName :: BareName -> Bytes
sizeOfBareName bn = headerSize + fieldSize
  where
    headerSize = 1 * wordSize
    fieldSize = case bn of
      BareName n i -> (2 * wordSize) + (sizeOfText n) + (sizeOfInfo i)

sizeOfModuleName :: ModuleName -> Bytes
sizeOfModuleName modName = headerSize + fieldSize
  where
    headerSize = 1 * wordSize
    fieldSize = case modName of
      ModuleName mn namespace ->
        (2 * wordSize) + (sizeOfText mn) + (sizeOfMaybe sizeOfNamespaceName namespace)

sizeOfInfo :: Info -> Bytes
sizeOfInfo (Info may) = sizeOfMaybe (sizeOfTuple sizeOfCode sizeOfParsed) may
      

sizeOfParsed :: Parsed -> Bytes
sizeOfParsed p = headerSize + fieldSize
  where
    headerSize = 1 * wordSize
    fieldSize = case p of
      Parsed d len -> (2 * wordSize) + (sizeOfDelta d) + (sizeOfInt len)


sizeOfDelta :: Delta -> Bytes
sizeOfDelta d = headerSize + fieldSize
  where
    headerSize = 1 * wordSize
    fieldSize = case d of
      -- Most of delta's arguments are unpacked (unboxed) Int64
      -- Most unboxed types take one word
      -- Exceptions being Int64#, Word64#, and Double# which take 2 words on a 32-bit machine.
      -- TODO - Assumes 64-bit machine
      Columns _ _ -> 2 * wordSize
      Tab _ _ _ -> 3 * wordSize
      Lines _ _ _ _ -> 4 * wordSize
      -- bs is only unpacked argument
      Directed bs _ _ _ _ -> (1 * wordSize) + (4 * wordSize) + (sizeOfByteString bs)   


-- A constructor defined with newtype is free
sizeOfCode :: Code -> Bytes
sizeOfCode (Code c) = sizeOfText c

sizeOfTuple :: (a -> Bytes) -> (b -> Bytes) -> (a,b) -> Bytes
sizeOfTuple sizeOfa sizeOfb (a,b) = tupleSize
  where
    tupleSize = (3 * wordSize) + (sizeOfa a) + (sizeOfb b)


sizeOfMaybe :: (a -> Bytes) -> Maybe a -> Bytes
sizeOfMaybe sizeOfa may = headerSize + fieldSize
  where
    headerSize = 1 * wordSize
    fieldSize = case may of
      Just e -> (1 * wordSize) + (sizeOfa e)
      Nothing -> 0

-- A constructor defined with newtype is free
sizeOfNamespaceName :: NamespaceName -> Bytes
sizeOfNamespaceName (NamespaceName n) = sizeOfText n

sizeOfArray :: (a -> Bytes) -> [a] -> Bytes
sizeOfArray sizeOfa arr = arrSize
  where
    arrSize = ((1 + (3 * arrLength)) * wordSize) + sizeOfContents
    arrLength = L.length arr
    sizeOfContents = L.foldl' (\acc e -> acc + sizeOfa e) 0 arr

sizeOfPublicKey :: PublicKey -> Bytes
sizeOfPublicKey (PublicKey bs) = sizeOfByteString bs

sizeOfByteString :: BS.ByteString -> Bytes
sizeOfByteString bs = byteStringSize
  where
    byteStringSize = (9 * wordSize) + byteStringLength
    byteStringLength = BS.length bs

-- A constructor defined with newtype is free
sizeOfPactId :: PactId -> Bytes
sizeOfPactId (PactId t) = sizeOfText t

sizeOfMap :: M.Map FieldKey PactValue -> Bytes
sizeOfMap m = mapSize
  where
    mapSize = (6 * mapLength * wordSize) + sizeOfKeys + sizeOfValues
    mapLength = M.size m
    sizeOfValues = M.foldl' (\acc pv -> acc + (sizeOfPactValue pv)) 0 m
    sizeOfKeys = M.foldlWithKey' (\acc fk _ -> acc + (sizeOfFieldKey fk)) 0 m

-- A constructor defined with newtype is free
sizeOfFieldKey :: FieldKey -> Bytes
sizeOfFieldKey (FieldKey t) = sizeOfText t

sizeOfText :: Text -> Bytes
sizeOfText t = (6 * wordSize) + (2 * (T.length t))

sizeOfByteArray :: BA.ByteArray# -> Bytes
sizeOfByteArray barr = undefined

sizeOfBigNat :: BigNat -> Bytes
sizeOfBigNat bn = undefined
  where
    headerSize = 1 * wordSize
    fieldSize = case bn of
      BN# barr -> 1 * wordSize + (I# $ BA.sizeofByteArray# barr)

sizeOfInteger :: Integer -> Bytes
sizeOfInteger i = headerSize + fieldSize
  where
    headerSize = 1 * wordSize
    fieldSize = case i of
      -- S's argument is unboxed Int
      S# _ -> 1 * wordSize
      Jp# bn -> (1 * wordSize) + sizeOfBigNat bn
      Jn# bn -> (1 * wordSize) + sizeOfBigNat bn


sizeOfInt :: Int -> Bytes
sizeOfInt _ = 2 * wordSize

sizeOfDecimal :: Decimal -> Bytes
sizeOfDecimal = undefined

sizeOfTime :: UTCTime -> Bytes
sizeOfTime = undefined
