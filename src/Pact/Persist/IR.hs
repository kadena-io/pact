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

data PIRType n info
  = PIRAnyType
  | PIRTyVar n
  | PIRPrimType PrimType
  | PIRListType (PIRType n info)
  | PIRSchemaType SchemaType (PIRSchema n n info) SchemaPartial
  | PIRFun [PIRArg n n] (PIRType n info)
  deriving Show

data PIRTerm name tyname info
  = PIRVar !name info
  | PIRLet ![PIRBindPair name tyname info] !(PIRTerm name tyname info) info
  | PIRLam !name !(PIRType tyname info) !(PIRTerm name tyname info) info
  | PIRDynamic ()
  | PIRApp !(PIRTerm name tyname info) [PIRTerm name tyname info] info
  | PIRList [PIRTerm name tyname info] info
  | PIRLit !Literal info
  | PIRConst (ConstVal (PIRTerm name tyname info))
  deriving Show

data PIRDefCapMeta name tyname info
  = PIRDefCapManaged (Maybe (Text, PIRTerm name tyname info))
  | PIRDefCapEvent
  deriving (Show)

data PIRDef name tyname info
  = PIRDef
  { _pirdDefName :: !DefName
  , _pirdModule :: !ModuleName
  , _pirdDefType :: !DefType
  , _pirdFunType :: !(PIRType tyname info)
  , _pirdDefBody :: !(PIRTerm name tyname info)
  , _pirdMeta :: !Meta
  , _pirdDefMeta :: !(Maybe (PIRDefCapMeta name tyname info))
  , _pirdInfo :: info
  } deriving (Show)

data PIRSchema name tyname info
  = PIRSchema
  { _pirSSchemaName :: !Text
  , _pirSModule :: !(Maybe ModuleName)
  , _pirSMeta :: !Meta
  , _pirSFields :: ![PIRArg name tyname]
  , _pirSInfo :: !info
  } deriving Show

data PIRToplevel name tyname info
  = PIRTLDef (PIRDef name tyname info)
  | PIRTLSchema (PIRSchema name tyname info)
  
