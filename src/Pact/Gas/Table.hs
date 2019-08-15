{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Pact.Gas.Table where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T

import Pact.Types.Gas
import Pact.Types.Term

-- NB: If pact ends up not having any variadic primitives (currently, I didn't spot any thus far, but also the types don't rule it out)
-- then the primTable here could lose the [Term Ref] function parameter, and simply be Map Text Gas.
-- Attributing gas costs to Term Ref on its own is problematic in general -- typically we want to account for those costs when the terms
-- become evaluated, but there may be some cases where the ability to inspect the argument list is helpful, so I'll leave this in for now.

-- The argument lists in the GReduced case (where gas costs are computed after arguments are evaluated) may be more useful, but I wasn't
-- able to find instances where GReduced was in present use, so I've removed it for now.

data GasCostConfig = GasCostConfig
  { _gasCostConfig_primTable :: Map Text ([Term Ref] -> Gas)
  , _gasCostConfig_selectColumnCost :: Gas -- up-front cost per column in a select operation
  , _gasCostConfig_readColumnCost :: Gas -- cost per column to read a row
  , _gasCostConfig_sortFactor :: Gas
  , _gasCostConfig_moduleCost :: Gas
  , _gasCostConfig_moduleMemberCost :: Gas
  , _gasCostConfig_useModuleCost :: Gas
  , _gasCostConfig_interfaceCost :: Gas
  , _gasCostConfig_writeCost :: Gas
  , _gasCostConfig_functionApplicationCost :: Gas
  }

defaultGasConfig :: GasCostConfig
defaultGasConfig = GasCostConfig
  { _gasCostConfig_primTable = defaultGasTable
  , _gasCostConfig_selectColumnCost = 1
  , _gasCostConfig_readColumnCost = 1
  , _gasCostConfig_sortFactor = 1
  , _gasCostConfig_moduleCost = 1
  , _gasCostConfig_moduleMemberCost = 1
  , _gasCostConfig_useModuleCost = 1
  , _gasCostConfig_interfaceCost = 1
  , _gasCostConfig_writeCost = 1
  , _gasCostConfig_functionApplicationCost = 1
  }

defaultGasTable :: Map Text ([Term Ref] -> Gas)
defaultGasTable =
  Map.fromList
    [ ("describe-module", const 1)
    , ("sort", const 1)
    , ("keys", const 1)
    , ("map", const 1)
    , ("!=", const 1)
    , ("with-capability",const 1)
    , ("create-pact-guard", const 1)
    , ("/", const 1)
    , ("ln", const 1)
    , ("enforce-pact-version", const 1)
    , ("or?", const 1)
    , ("txids", const 1)
    , ("or", const 1)
    , ("yield", const 1)
    , ("diff-time", const 1)
    , ("minutes", const 1)
    , ("make-list", const 1)
    , ("reverse", const 1)
    , ("filter", const 1)
    , ("select", const 1)
    , ("compose", const 1)
    , ("contains", const 1)
    , ("with-default-read", const 1)
    , ("create-module-guard", const 1)
    , (">=", const 1)
    , ("keys-2", const 1)
    , ("take", const 1)
    , ("format-time", const 1)
    , ("constantly", const 1)
    , ("enforce-keyset", const 1)
    , ("identity", const 1)
    , ("describe-table", const 1)
    , ("enforce", const 1)
    , ("pact-id", const 1)
    , ("keyset-ref-guard", const 1)
    , ("add-time", const 1)
    , ("update", const 1)
    , ("read-integer", const 1)
    , ("resume", const 1)
    , ("typeof", const 1)
    , ("pact-version", const 1)
    , ("and?", const 1)
    , ("log", const 1)
    , ("read-decimal", const 1)
    , ("read-string", const 1)
    , ("length", const 1)
    , ("sqrt", const 1)
    , ("<", const 1)
    , ("days", const 1)
    , ("create-table", const 1)
    , ("keys-any", const 1)
    , ("insert", const 1)
    , ("if", const 1)
    , ("enforce-one", const 1)
    , ("*", const 1)
    , ("not?", const 1)
    , ("read-keyset", const 1)
    , ("read", const 1)
    , ("list", const 1)
    , ("round", const 1)
    , ("hash", const 1)
    , ("exp", const 1)
    , ("not", const 1)
    , ("at", const 1)
    , ("where", const 1)
    , ("-", const 1)
    , ("str-to-int", const 1)
    , ("=", const 1)
    , ("describe-keyset", const 1)
    , ("time", const 1)
    , ("with-read", const 1)
    , ("write", const 1)
    , ("define-keyset", const 1)
    , ("parse-time", const 1)
    , ("create-user-guard", const 1)
    , ("floor", const 1)
    , ("abs", const 1)
    , ("keys-all", const 1)
    , ("txlog", const 1)
    , ("require-capability", const 1)
    , ("keylog", const 1)
    , ("ceiling", const 1)
    , ("hours", const 1)
    , ("+", const 1)
    , ("format", const 1)
    , ("and", const 1)
    , ("tx-hash", const 1)
    , (">", const 1)
    , ("enforce-guard", const 1)
    , ("mod", const 1)
    , ("remove", const 1)
    , ("^", const 1)
    , ("list-modules", const 1)
    , ("read-msg", const 1)
    , ("fold", const 1)
    , ("bind", const 1)
    , ("<=", const 1)
    , ("drop", const 1)
    ]

tableGasModel :: GasCostConfig -> GasModel
tableGasModel gasConfig =
  let run name ga = case ga of
        GPostRead r -> case r of
          ReadData cols -> _gasCostConfig_readColumnCost gasConfig * fromIntegral (Map.size (_objectMap cols))
          ReadKey _rowKey -> _gasCostConfig_readColumnCost gasConfig
          ReadTxId -> _gasCostConfig_readColumnCost gasConfig
        GSelect mColumns _expression _tableTerm -> _gasCostConfig_selectColumnCost gasConfig * case mColumns of
          Nothing -> 1 -- not sure what to do here
          Just cs -> fromIntegral (length cs)
        GSortFieldLookup n -> fromIntegral n * _gasCostConfig_sortFactor gasConfig
        GUnreduced ts -> case Map.lookup name (_gasCostConfig_primTable gasConfig) of
          Just g -> g ts
          Nothing -> error $ "Unknown primitive \"" <> T.unpack name <> "\" in determining cost of GUnreduced"
        GWrite _writeType _tableTerm _objectTerm -> _gasCostConfig_writeCost gasConfig
        GUse _moduleName _mHash -> _gasCostConfig_useModuleCost gasConfig
          -- The above seems somewhat suspect (perhaps cost should scale with the module?)
        GModuleDecl _module -> _gasCostConfig_moduleCost gasConfig
        GInterfaceDecl _module -> _gasCostConfig_interfaceCost gasConfig
        GModuleMember _module -> _gasCostConfig_moduleMemberCost gasConfig
        GUserApp -> _gasCostConfig_functionApplicationCost gasConfig
  in GasModel
      { gasModelName = "table"
      , gasModelDesc = "table-based cost model"
      , runGasModel = run
      }
