module Pact.Persist.IR where

import Pact.Types.Lang
import Data.Text(Text)
import qualified Data.Vector as V

data PIRModRef =
  PIRModRef !ModuleName !(Maybe [ModuleName])
  deriving Show

data PIRValue
  = PIRNativeV !NativeDefName
  | PIRLiteralV Literal
  | PIRListV (V.Vector PIRValue)
  | PIRModRefV PIRModRef
  | PIRObjectV (ObjectMap PIRValue)
  | PIRGuardV (Guard PIRValue)
  deriving Show

data PIRArg name tyname =
  PIRArg name (Type tyname)
  deriving Show

data PIRBindPair name tyname info =
  PIRBindPair (PIRArg name tyname) (PIRTerm name tyname info)
  deriving Show

data PIRType n
  = PIRAnyType
  | PIRTyVar n
  | PIRPrimType PrimType
  | PIRListType (PIRType n)
  | PIRSchemaType SchemaType (PIRType n) SchemaPartial
  | PIRFun [PIRArg n n] (PIRType n)
  deriving Show

data PIRTerm name tyname info
  = PIRVar !name info
  | PIRLet ![PIRBindPair name tyname info] !(PIRTerm name tyname info) info
  | PIRLam !name !(PIRType tyname) !(PIRTerm name tyname info) info
  | PIRApp (PIRTerm name tyname info) [PIRTerm name tyname info] info
  | PIRList [PIRTerm name tyname info] info
  deriving Show

data PIRDefCapMeta name tyname info
  = PIRDefCapManaged (Maybe (Text, PIRTerm name tyname info))
  | PIRDefCapEvent
  deriving (Show)

data PIRDef name tyname info = PIRDef
  { _dDefName :: !DefName
  , _dModule :: !ModuleName
  , _dDefType :: !DefType
  , _dFunType :: !(PIRType tyname)
  , _dDefBody :: !(PIRTerm name tyname info)
  , _dMeta :: !Meta
  , _dDefMeta :: !(Maybe (PIRDefCapMeta name tyname info))
  , _dInfo :: info
  } deriving (Show)

data PIRSchema name tyname info
  = PIRSchema
  { _tSchemaName :: !Text
  , _tModule :: !(Maybe ModuleName)
  , _tMeta :: !Meta
  , _tFields :: ![PIRArg name tyname]
  , _tInfo :: !info
  }


data PIRToplevel name tyname info =
  PIRTLDef (PIRDef name tyname info)

