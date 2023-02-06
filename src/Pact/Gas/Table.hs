{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE CPP #-}
module Pact.Gas.Table where

import Data.Ratio
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
#if !defined(ghcjs_HOST_OS)
import qualified GHC.Integer.Logarithms as IntLog
import GHC.Int(Int(..))
#endif

import Pact.Types.Continuation
import Pact.Types.Gas
import Pact.Types.RowData
import Pact.Types.SizeOf
import Pact.Types.Term

-- NB: If pact ends up not having any variadic primitives (currently, I didn't spot any thus far, but also the types don't rule it out)
-- then the primTable here could lose the [Term Ref] function parameter, and simply be Map Text Gas.
-- Attributing gas costs to Term Ref on its own is problematic in general -- typically we want to account for those costs when the terms
-- become evaluated, but there may be some cases where the ability to inspect the argument list is helpful, so I'll leave this in for now.

-- The argument lists in the GReduced case (where gas costs are computed after arguments are evaluated) may be more useful, but I wasn't
-- able to find instances where GReduced was in present use, so I've removed it for now.

data GasCostConfig = GasCostConfig
  { _gasCostConfig_primTable :: Map Text Gas
  , _gasCostConfig_selectColumnCost :: Gas -- up-front cost per column in a select operation
  , _gasCostConfig_readColumnCost :: Gas -- cost per column to read a row
  , _gasCostConfig_sortFactor :: Gas
  , _gasCostConfig_distinctFactor :: Gas
  , _gasCostConfig_concatenationFactor :: Gas
  , _gasCostConfig_moduleCost :: Gas
  , _gasCostConfig_moduleMemberCost :: Gas
  , _gasCostConfig_useModuleCost :: Gas
  , _gasCostConfig_interfaceCost :: Gas
  , _gasCostConfig_writeBytesCost :: Gas -- cost per bytes to write to database
  , _gasCostConfig_functionApplicationCost :: Gas
  , _gasCostConfig_defPactCost :: Gas
  , _gasCostConfig_foldDBCost :: Gas
  , _gasCostConfig_principalCost :: Gas
  }

defaultGasConfig :: GasCostConfig
defaultGasConfig = GasCostConfig
  { _gasCostConfig_primTable = defaultGasTable
  , _gasCostConfig_selectColumnCost = 1
  , _gasCostConfig_readColumnCost = 1
  , _gasCostConfig_sortFactor = 1
  , _gasCostConfig_distinctFactor = 1
  , _gasCostConfig_concatenationFactor = 1  -- TODO benchmark
  , _gasCostConfig_moduleCost = 1        -- TODO benchmark
  , _gasCostConfig_moduleMemberCost = 1
  , _gasCostConfig_useModuleCost = 1     -- TODO benchmark
  , _gasCostConfig_interfaceCost = 1     -- TODO benchmark
  , _gasCostConfig_writeBytesCost = 1
  , _gasCostConfig_functionApplicationCost = 1
  , _gasCostConfig_defPactCost = 1   -- TODO benchmark
  , _gasCostConfig_foldDBCost = 1
  , _gasCostConfig_principalCost = 5 -- matches 'hash' cost
  }

defaultGasTable :: Map Text Gas
defaultGasTable =
  Map.fromList
  [("!=", 2)
  ,("&", 1)
  ,("*", 3)
  ,("+", 1)
  ,("-", 1)
  ,("/", 3)
  ,("<", 2)
  ,("<=", 2)
  ,("=", 2)
  ,(">", 2)
  ,(">=", 2)
  ,("^", 4)
  ,("abs", 1)
  ,("add-time", 3)
  ,("and", 1)
  ,("and?", 1)
  ,("at", 2)
  ,("base64-decode", 1)
  ,("base64-encode", 1)
  ,("bind", 4)
  ,("ceiling", 1)
  ,("chain-data", 1)
  ,("compose", 1)
  ,("compose-capability", 2)
  ,("concat", 1)
  ,("constantly", 1)
  ,("contains", 2)
  ,("create-module-guard", 1)
  ,("create-pact-guard", 1)
  ,("create-principal", 1)
  ,("create-user-guard", 1)
  ,("create-capability-guard", 1)
  ,("create-capability-pact-guard", 1)
  ,("days", 4)
  ,("decrypt-cc20p1305", 33)
  ,("diff-time", 8)
  ,("drop", 3)
  ,("emit-event",1)
  ,("enforce", 1)
  ,("enforce-guard", 8)
  ,("enforce-keyset", 8)
  ,("enforce-one", 6)
  ,("enforce-pact-version", 1)
  ,("enumerate", 1)
  ,("exp", 5)
  ,("filter", 3)
  ,("floor", 1)
  ,("fold", 3)
  ,("format", 4)
  ,("format-time", 4)
  ,("hash", 5)
  ,("hours", 4)
  ,("identity", 2)
  ,("if", 1)
  ,("install-capability", 3)
  ,("int-to-str", 1)
  ,("is-charset", 1)
  ,("is-principal",1)
  ,("keys-2", 1)
  ,("keys-all", 1)
  ,("keys-any", 1)
  ,("keyset-ref-guard", 7)
  ,("length", 1)
  ,("ln", 6)
  ,("log", 3)
  ,("make-list",1)
  ,("map", 4)
  ,("zip", 4)
  ,("minutes", 4)
  ,("mod", 1)
  ,("namespace", 12)
  ,("not", 1)
  ,("not?", 1)
  ,("or", 1)
  ,("or?", 1)
  ,("pact-id", 1)
  ,("pact-version", 1)
  ,("parse-time", 2)
  ,("read", 10)
  ,("read-decimal", 1)
  ,("read-integer", 1)
  ,("read-keyset", 1)
  ,("read-msg", 10)
  ,("read-string", 1)
  ,("remove", 2)
  ,("require-capability", 1)
  ,("resume", 2)
  ,("reverse", 2)
  ,("round", 1)
  ,("shift", 1)
  ,("sort", 2)
  ,("sqrt", 6)
  ,("str-to-int", 1)
  ,("str-to-list", 1)
  ,("take", 3)
  ,("time", 2)
  ,("try", 1)
  ,("tx-hash", 1)
  ,("typeof", 2)
  ,("typeof-principal",1)
  ,("distinct", 2)
  ,("validate-keypair", 29)
  ,("validate-principal", 1)
  ,("verify-spv", 100) -- deprecated
  ,("where", 2)
  ,("with-capability", 2)
  ,("with-default-read", 14)
  ,("with-read", 13)
  ,("xor", 1)
  ,("yield", 2)
  ,("|", 1)
  ,("~", 1)
  -- Nested defpacts
  ,("continue",1)
  -- IO
  -- DDL
  ,("create-table", 250)
  -- Registries
  ,("define-keyset", 25)
  ,("define-namespace", 25)
  -- Single row
  ,("insert", 100)
  ,("update", 100)
  ,("write", 100)
  -- Multi row read, tx penalty
  ,("keys", 200)
  ,("select", 200)
  ,("fold-db", 200)

  -- Metadata, tx penalty
  ,("describe-keyset", 100)
  ,("describe-module", 100)
  ,("describe-table", 100)
  ,("list-modules", 100)
  ,("describe-namespace",100)

  -- History, massive tx penalty
  ,("keylog", 100000)
  ,("txids", 100000)
  ,("txlog", 100000)

  -- Zk entries
  -- TODO: adjust gas, this is purely for testing purposes
  ,("scalar-mult", 1)
  ,("point-add", 1)
  ,("pairing-check", 1)
  ]

{-# NOINLINE defaultGasTable #-}

expLengthPenalty :: Integral i => i -> Gas
expLengthPenalty v = let lv = logBase (100::Float) (fromIntegral v) in 1 + floor (lv^(10::Int))
{-# INLINE expLengthPenalty #-}

tableGasModel :: GasCostConfig -> GasModel
tableGasModel gasConfig =
  let run name ga = case ga of
        GUnreduced _ts -> case Map.lookup name (_gasCostConfig_primTable gasConfig) of
          Just g -> g
          Nothing -> error $ "Unknown primitive \"" <> T.unpack name <> "\" in determining cost of GUnreduced"
        GUserApp t -> case t of
          Defpact -> (_gasCostConfig_defPactCost gasConfig) * _gasCostConfig_functionApplicationCost gasConfig
          _ -> _gasCostConfig_functionApplicationCost gasConfig
        GIntegerOpCost i j ->
          intCost i + intCost j
        GMakeList v -> expLengthPenalty v
        GSort len -> expLengthPenalty len
        GDistinct len -> expLengthPenalty len
        GSelect mColumns -> case mColumns of
          Nothing -> 1
          Just [] -> 1
          Just cs -> _gasCostConfig_selectColumnCost gasConfig * (fromIntegral (length cs))
        GSortFieldLookup n ->
          fromIntegral n * _gasCostConfig_sortFactor gasConfig
        GConcatenation i j ->
          fromIntegral (i + j) * _gasCostConfig_concatenationFactor gasConfig
        GFoldDB -> _gasCostConfig_foldDBCost gasConfig
        GPostRead r -> case r of
          ReadData cols -> _gasCostConfig_readColumnCost gasConfig * fromIntegral (Map.size (_objectMap $ _rdData cols))
          ReadKey _rowKey -> _gasCostConfig_readColumnCost gasConfig
          ReadTxId -> _gasCostConfig_readColumnCost gasConfig
          ReadModule _moduleName _mCode ->  _gasCostConfig_readColumnCost gasConfig
          ReadInterface _moduleName _mCode ->  _gasCostConfig_readColumnCost gasConfig
          ReadNamespace _ns ->  _gasCostConfig_readColumnCost gasConfig
          ReadKeySet _ksName _ks ->  _gasCostConfig_readColumnCost gasConfig
          ReadYield (Yield _obj _ _) -> _gasCostConfig_readColumnCost gasConfig * fromIntegral (Map.size (_objectMap _obj))
        GPreWrite w szVer -> case w of
          WriteData _type key obj ->
            (memoryCost szVer key (_gasCostConfig_writeBytesCost gasConfig))
            + (memoryCost szVer obj (_gasCostConfig_writeBytesCost gasConfig))
          WriteTable tableName ->
            (memoryCost szVer tableName (_gasCostConfig_writeBytesCost gasConfig))
          WriteModule _modName _mCode ->
            (memoryCost szVer _modName (_gasCostConfig_writeBytesCost gasConfig))
            + (memoryCost szVer _mCode (_gasCostConfig_writeBytesCost gasConfig))
          WriteInterface _modName _mCode ->
            (memoryCost szVer _modName (_gasCostConfig_writeBytesCost gasConfig))
            + (memoryCost szVer _mCode (_gasCostConfig_writeBytesCost gasConfig))
          WriteNamespace ns ->
            (memoryCost szVer ns (_gasCostConfig_writeBytesCost gasConfig))
          WriteKeySet ksName ks ->
            (memoryCost szVer ksName (_gasCostConfig_writeBytesCost gasConfig))
            + (memoryCost szVer ks (_gasCostConfig_writeBytesCost gasConfig))
          WriteYield obj ->
            (memoryCost szVer (_yData obj) (_gasCostConfig_writeBytesCost gasConfig))
        GModuleMember _module -> _gasCostConfig_moduleMemberCost gasConfig
        GModuleDecl _moduleName _mCode -> (_gasCostConfig_moduleCost gasConfig)
        GUse _moduleName _mHash -> (_gasCostConfig_useModuleCost gasConfig)
          -- The above seems somewhat suspect (perhaps cost should scale with the module?)
        GInterfaceDecl _interfaceName _iCode -> (_gasCostConfig_interfaceCost gasConfig)
        GModuleMemory i -> moduleMemoryCost i
        GPrincipal g -> fromIntegral g * _gasCostConfig_principalCost gasConfig
        GMakeList2 len msz ->
          let glen = fromIntegral len
          in glen + maybe 0 ((* glen) . intCost) msz
        GZKArgs arg -> case arg of
          PointAdd g -> pointAddGas g
          ScalarMult g -> scalarMulGas g
          Pairing np -> pairingGas np
  in GasModel
      { gasModelName = "table"
      , gasModelDesc = "table-based cost model"
      , runGasModel = run
      }
{-# INLINE tableGasModel #-}

pointAddGas :: ZKGroup -> Gas
pointAddGas = \case
  ZKG1 -> 5
  ZKG2 -> 30

scalarMulGas :: ZKGroup -> Gas
scalarMulGas = \case
  ZKG1 -> 360
  ZKG2 -> 1450

pairingGas :: Int -> Gas
pairingGas npairs
  | npairs > 0 = fromIntegral (npairs * slope + intercept)
  | otherwise = 100
  where
  slope = 3760
  intercept = 11600

perByteFactor :: Rational
perByteFactor = 1%10
{-# NOINLINE perByteFactor #-}

memoryCost :: (SizeOf a) => SizeOfVersion -> a -> Gas -> Gas
memoryCost szVer val (Gas cost) = Gas totalCost
  where costFrac = realToFrac cost
        sizeFrac = realToFrac (sizeOf szVer val)
        totalCost = ceiling (perByteFactor * sizeFrac * costFrac)
{-# INLINE memoryCost #-}

-- Slope to costing function,
-- sets a 10mb practical limit on module sizes.
moduleMemFeePerByte :: Rational
moduleMemFeePerByte = 0.006

-- 0.01x+50000 linear costing funciton
moduleMemoryCost :: Bytes -> Gas
moduleMemoryCost sz = ceiling (moduleMemFeePerByte * fromIntegral sz) + 60000
{-# INLINE moduleMemoryCost #-}

-- | Gas model that charges varible (positive) rate per tracked operation
defaultGasModel :: GasModel
defaultGasModel = tableGasModel defaultGasConfig

#if !defined(ghcjs_HOST_OS)
-- | Costing function for binary integer ops
intCost :: Integer -> Gas
intCost !a
  | (abs a) < threshold = 0
  | otherwise =
    let !nbytes = (I# (IntLog.integerLog2# (abs a)) + 1) `quot` 8
    in fromIntegral (nbytes * nbytes `quot` 100)
  where
  threshold :: Integer
  threshold = (10 :: Integer) ^ (30 :: Integer)


_intCost :: Integer -> Int
_intCost !a =
    let !nbytes = (I# (IntLog.integerLog2# (abs a)) + 1) `quot` 8
    in nbytes
#else
intCost :: Integer -> Gas
intCost !a
  | (abs a) < threshold = 0
  | otherwise =
    let !nbytes = (ceiling (logBase @Double 2 (fromIntegral (abs a))) + 1) `quot` 8
    in (nbytes * nbytes) `quot` 100
  where
  threshold :: Integer
  threshold = (10 :: Integer) ^ (30 :: Integer)
#endif

pact421GasModel :: GasModel
pact421GasModel = gasModel { runGasModel = modifiedRunFunction }
  where
  gasModel = tableGasModel gasConfig
  gasConfig = defaultGasConfig { _gasCostConfig_primTable = updTable }
  updTable = Map.union upd defaultGasTable
  unknownOperationPenalty = 1000000
  multiRowOperation = 40000
  upd = Map.fromList
    [("keys",    multiRowOperation)
    ,("select",  multiRowOperation)
    ,("fold-db", multiRowOperation)
    ]
  modifiedRunFunction name ga = case ga of
    GUnreduced _ts -> case Map.lookup name updTable of
      Just g -> g
      Nothing -> unknownOperationPenalty
    _ -> runGasModel defaultGasModel name ga
