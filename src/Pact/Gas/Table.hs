{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Pact.Gas.Table where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T

import Pact.Types.PactValue
import Data.Int (Int64)
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
  , _gasCostConfig_writeBytesCost :: Gas -- cost per bytes to write a row
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
  , _gasCostConfig_writeBytesCost = 1
  , _gasCostConfig_functionApplicationCost = 1
  }

defaultGasTable :: Map Text ([Term Ref] -> Gas)
defaultGasTable =
  Map.fromList
  [
   -- General native functions
    ("at",                   const 2)
   ,("bind",                 const 5)
   ,("chain-data",           const 1)
   ,("compose",              const 1)
   ,("constantly",           const 1)
   ,("contains",             const 1)
   ,("define-namespace",     const 2000)
   ,("drop",                 const 3)
   ,("enforce",              const 1)
   ,("enforce-one",          const 6)
   ,("enforce-pact-version", const 1)
   ,("filter",               const 3)
   ,("fold",                 const 3)
   ,("format",               const 4)
   ,("hash",                 const 6)
   ,("identity",             const 2)
   ,("if",                   const 1)
   ,("install-capability",   const 1) -- TODO benchmark
   ,("int-to-str",           const 1)
   ,("is-charset",           const 1) -- TODO benchmark
   ,("length",               const 1)
   ,("list-modules",         const 12)
   ,("make-list",            const 1)
   ,("map",                  const 3)
   ,("namespace",            const 13)
   ,("pact-id",              const 1)
   ,("pact-version",         const 1)
   ,("read-decimal",         const 1)
   ,("read-integer",         const 1)
   ,("read-msg",             const 1)
   ,("read-string",          const 1)
   ,("remove",               const 3)
   ,("resume",               const 2)
   ,("reverse",              const 2)
   ,("sort",                 const 2)
   ,("str-to-int",           const 1)
   ,("take",                 const 3)
   ,("try",                  const 1)
   ,("tx-hash",              const 1)
   ,("typeof",               const 2)
   ,("where",                const 2)
   ,("yield",                const 3)

   -- Operators native functions
   ,("!=",      const 2)
   ,("&",       const 1)
   ,("*",       const 3)
   ,("+",       const 1)
   ,("-",       const 1)
   ,("/",       const 2)
   ,("<",       const 2)
   ,("<=",      const 2)
   ,("=",       const 2)
   ,(">",       const 2)
   ,(">=",      const 2)
   ,("^",       const 4)
   ,("abs",     const 1)
   ,("and",     const 1)
   ,("and?",    const 1)
   ,("ceiling", const 5)
   ,("exp",     const 4)
   ,("floor",   const 5)
   ,("ln",      const 6)
   ,("log",     const 3)
   ,("mod",     const 1)
   ,("not",     const 1)
   ,("not?",    const 1)
   ,("or",      const 1)
   ,("or?",     const 1)
   ,("round",   const 5)
   ,("shift",   const 1)
   ,("sqrt",    const 5)
   ,("xor",     const 1)
   ,("|",       const 1)
   ,("~",       const 1)

   -- Time native functions
   ,("add-time",    const 3)
   ,("days",        const 4)
   ,("diff-time",   const 9)
   ,("format-time", const 4)
   ,("hours",       const 4)
   ,("minutes",     const 3)
   ,("parse-time",  const 2)
   ,("time",        const 2)

   -- Commitments native functions
   ,("decrypt-cc20p1305", const 34)
   ,("validate-keypair",  const 28)

   -- Keyset native functions
   ,("define-keyset",  const 2000)
   ,("enforce-keyset", const 11)
   ,("keys-2",         const 1)
   ,("keys-all",       const 1)
   ,("keys-any",       const 1)
   ,("read-keyset",    const 1)

   -- Database native functions
   ,("create-table",      const 1000)
   ,("describe-keyset",   const 10)
   ,("describe-module",   const 10)
   ,("describe-table",    const 3)
   ,("insert",            const 3000)
   ,("keylog",            const 100)
   ,("keys",              const 100)
   ,("read",              const 15)
   ,("select",            const 100)
   ,("txids",             const 100)
   ,("txlog",             const 7)
   ,("update",            const 3000)
   ,("with-default-read", const 20)
   ,("with-read",         const 20)
   ,("write",             const 3000)

   -- Capabilities native functions
   ,("compose-capability",  const 1)
   ,("create-module-guard", const 1)
   ,("create-pact-guard",   const 1)
   ,("create-user-guard",   const 1)
   ,("enforce-guard",       const 11)
   ,("keyset-ref-guard",    const 1)
   ,("require-capability",  const 1)
   ,("with-capability",     const 2)

   -- Native Constants + Schemas
   ,("CHARSET_ASCII",       const 1)
   ,("CHARSET_LATIN1",      const 1)
   ,("public-chain-data",   const 1)

   -- Deprecated functions
   ,("verify-spv",          const 1)
   ,("list",                const 1)
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
        GWrite _writeType _tableTerm _objPactValues ->
          (_gasCostConfig_writeCost gasConfig) +
          (memoryCost _objPactValues (_gasCostConfig_writeBytesCost gasConfig))
        GUse _moduleName _mHash -> _gasCostConfig_useModuleCost gasConfig
          -- The above seems somewhat suspect (perhaps cost should scale with the module?)
        GModuleDecl _module -> _gasCostConfig_moduleCost gasConfig
        GInterfaceDecl _module -> _gasCostConfig_interfaceCost gasConfig
        --GNamespaceDecl ns -> memoryCost ns (_gasCostConfig_writeBytesCost gasConfig)
        GKeySetDecl ksName ks ->
          (memoryCost ks (_gasCostConfig_writeBytesCost gasConfig)) +
          (memoryCost ksName (_gasCostConfig_writeBytesCost gasConfig))
        GModuleMember _module -> _gasCostConfig_moduleMemberCost gasConfig
        GUserApp -> _gasCostConfig_functionApplicationCost gasConfig
  in GasModel
      { gasModelName = "table"
      , gasModelDesc = "table-based cost model"
      , runGasModel = run
      }


costPerByte :: Int64
costPerByte = 1

memoryCost :: (SizeOf a) => a -> Gas -> Gas
memoryCost val (Gas factor) = Gas $ (sizeOf val) * factor * costPerByte
