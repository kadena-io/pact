{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- Required for GHC >= 9
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- |
-- Module      :  Pact.Types.Term
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Term and related types.
--

module Pact.Types.Term
 ( Meta(..),mDocs,mModel,
   PublicKeyText(..),
   KeySet(..), mkKeySet,
   KeySetName(..),
   PactGuard(..),
   PactId(..),
   UserGuard(..),
   ModuleGuard(..),
   CapabilityGuard(..),
   Guard(..),_GPact,_GKeySet,_GKeySetRef,_GModule,_GUser,
   DefType(..),_Defun,_Defpact,_Defcap,
   defTypeRep,
   FunApp(..),faDefType,faDocs,faInfo,faModule,faName,faTypes,
   Ref'(..),_Direct,_Ref,Ref,
   NativeDFun(..),
   BindType(..),
   BindPair(..),bpArg,bpVal,toBindPairs,
   Module(..),mName,mGovernance,mMeta,mCode,mHash,mBlessed,mInterfaces,mImports,
   Interface(..),interfaceCode, interfaceMeta, interfaceName, interfaceImports,
   ModuleDef(..),_MDModule,_MDInterface,moduleDefName,moduleDefCode,moduleDefMeta,
   Governance(..),
   ModuleHash(..), mhHash,
   ConstVal(..),constTerm,
   Use(..),
   App(..),appFun,appArgs,appInfo,
   Def(..),dDefBody,dDefName,dDefType,dMeta,dFunType,dInfo,dModule,dDefMeta,
   Lam(..), lamArg, lamBindBody, lamTy, lamInfo,
   DefMeta(..),
   DefcapMeta(..),
   Example(..),
   derefDef,
   ObjectMap(..),objectMapToListWith,
   Object(..),oObject,oObjectType,oInfo,oKeyOrder,
   FieldKey(..),
   Step(..),sEntity,sExec,sRollback,sInfo,
   ModRef(..),modRefName,modRefSpec,modRefInfo,modRefKeyValues_,
   modRefTy,
   Term(..),
   tApp,tBindBody,tBindPairs,tBindType,tConstArg,tConstVal,
   tDef,tMeta,tFields,tFunTypes,tHash,tInfo,tGuard,
   tListType,tList,tLiteral,tModuleBody,tModuleDef,tModule,tUse,
   tNativeDocs,tNativeFun,tNativeName,tNativeExamples,
   tNativeTopLevelOnly,tObject,tSchemaName,
   tTableName,tTableType,tVar,tStep,tModuleName,
   tDynModRef,tDynMember,tModRef,tLam,
   _TModule, _TList, _TDef, _TNative, _TConst, _TApp,
   _TVar, _TBinding, _TLam, _TObject, _TSchema,
   _TLiteral, _TGuard, _TUse, _TStep, _TModRef,
   _TTable, _TDynamic,
   ToTerm(..),
   toTermList,toTObject,toTObjectMap,toTList,toTListV,
   typeof,typeof',guardTypeOf,
   canUnifyWith,
   prettyTypeTerm,
   pattern TLitString,pattern TLitInteger,pattern TLitBool,
   tLit,tStr,termEq,termEq1,termRefEq,canEq,refEq,
   Gas(..),
   module Pact.Types.Names
   ) where

import Bound
import Control.Applicative
import Control.DeepSeq
import Control.Lens hiding ((.=), DefName(..), elements)
import Control.Monad
import Data.Aeson hiding (pairs,Object, (<?>))
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as A
import qualified Data.Aeson.KeyMap as A
import qualified Data.Aeson.Types as A
import Data.Decimal
import Data.Default
import Data.Eq.Deriving
import Data.Foldable
import Data.Functor.Classes (Eq1(..), Show1(..))
import Data.Function
import Data.Int (Int64)
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Pact.Time (UTCTime)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word64, Word32)
import GHC.Generics (Generic)
import Prelude
import Text.Show.Deriving

import Pact.Types.Exp
import Pact.Types.Info
import Pact.Types.KeySet
import Pact.Types.Names
import Pact.Types.Pretty hiding (dot)
import Pact.Types.SizeOf
import Pact.Types.Type
import Pact.Types.Util

import Pact.Types.Term.Internal

import qualified Pact.JSON.Encode as J

-- -------------------------------------------------------------------------- --
-- The following types have cyclic dependencies. There must be no TH splice
-- in between the definitions of these types.
--
-- * FunApp
-- * Ref' d (and Ref)
-- * NativeDFun
-- * Def n
-- * Object n
-- * Term n

-- -------------------------------------------------------------------------- --
-- FunApp

-- | Capture function application metadata
data FunApp = FunApp
  { _faInfo :: Info
  , _faName :: !Text
  , _faModule :: !(Maybe ModuleName)
  , _faDefType :: !DefType
  , _faTypes :: !(FunTypes (Term Name))
  , _faDocs :: !(Maybe Text)
  } deriving (Generic)

deriving instance (Show1 Term) => Show FunApp
deriving instance (Eq1 Term) => Eq FunApp
instance NFData FunApp

instance J.Encode FunApp where
  build o = J.object
    [ "defType" J..= _faDefType o
    , "types" J..= J.Array (_faTypes o)
    , "name" J..= _faName o
    , "module" J..= _faModule o
    , "docs" J..= _faDocs o
    , "info" J..= _faInfo o
    ]
  {-# INLINEABLE build #-}

instance FromJSON FunApp where parseJSON = lensyParseJSON 3
instance HasInfo FunApp where getInfo = _faInfo

-- -------------------------------------------------------------------------- --
-- Ref'

type Ref = Ref' (Term Name)

-- | Variable type for an evaluable 'Term'.
data Ref' d =
  -- | "Reduced" (evaluated) or native (irreducible) term.
  Direct !d |
  -- | Unevaulated/un-reduced term, never a native.
  Ref !(Term (Ref' d))
  deriving (Functor,Foldable,Traversable,Generic)

deriving instance (Eq1 Term, Eq d) => Eq (Ref' d)
deriving instance (Show1 Term, Show d) => Show (Ref' d)

instance NFData d => NFData (Ref' d)

instance Pretty d => Pretty (Ref' d) where
  pretty (Direct tm) = pretty tm
  pretty (Ref tm)    = pretty tm

instance HasInfo n => HasInfo (Ref' n) where
  getInfo (Direct d) = getInfo d
  getInfo (Ref r) = getInfo r

instance (SizeOf d) => SizeOf (Ref' d)

-- -------------------------------------------------------------------------- --
-- NativeDFun

data NativeDFun = NativeDFun
  { _nativeName :: !NativeDefName
  , _nativeFun :: !(forall m . Monad m => FunApp -> [Term Ref] -> m (Gas,Term Name))
  }

instance Eq NativeDFun where a == b = _nativeName a == _nativeName b
instance Show NativeDFun where
  showsPrec p (NativeDFun name _) = showParen (p > 10) $
    showString "NativeDFun " . showsPrec 11 name . showString " _"

instance NFData NativeDFun where
  rnf (NativeDFun n _f) = seq n ()

-- -------------------------------------------------------------------------- --
-- Def

data Def n = Def
  { _dDefName :: !DefName
  , _dModule :: !ModuleName
  , _dDefType :: !DefType
  , _dFunType :: !(FunType (Term n))
  , _dDefBody :: !(Scope Int Term n)
  , _dMeta :: !Meta
  , _dDefMeta :: !(Maybe (DefMeta (Term n)))
  , _dInfo :: !Info
  } deriving (Functor,Foldable,Traversable,Generic)

deriving instance (Show1 Term, Show n) => Show (Def n)
deriving instance (Eq1 Term, Eq n) => Eq (Def n)
instance NFData (Term n) => NFData (Def n)
instance (Eq1 Term, Eq n) => Ord (Def n) where
  a `compare` b = nm a `compare` nm b
    where nm d = (_dModule d, _dDefName d)

instance HasInfo (Def n) where getInfo = _dInfo

instance Pretty (Term n) => Pretty (Def n) where
  pretty Def{..} = parensSep $
    [ prettyString (defTypeRep _dDefType)
    , pretty _dModule <> "." <> pretty _dDefName <> ":" <> pretty (_ftReturn _dFunType)
    , parensSep $ pretty <$> _ftArgs _dFunType
    ] ++ maybe [] (\docs -> [pretty docs]) (_mDocs _dMeta)
    ++ maybe [] (pure . pretty) _dDefMeta

instance J.Encode n => J.Encode (Def n) where
  build o = J.object
    [ "defType" J..= _dDefType o
    , "defMeta" J..= _dDefMeta o
    , "funType" J..= _dFunType o
    , "defName" J..= _dDefName o
    , "defBody" J..= _dDefBody o
    , "module" J..= _dModule o
    , "meta" J..= _dMeta o
    , "info" J..= _dInfo o
    ]
  {-# INLINEABLE build #-}

instance FromJSON n => FromJSON (Def n) where parseJSON = lensyParseJSON 2

derefDef :: Def n -> Name
derefDef Def{..} = QName $ QualifiedName _dModule (asString _dDefName) _dInfo

instance (SizeOf n) => SizeOf (Def n)

-- -------------------------------------------------------------------------- --
-- Lam

data Lam n
  = Lam
  { _lamArg :: !Text
  , _lamTy  :: !(FunType (Term n))
  , _lamBindBody :: !(Scope Int Term n)
  , _lamInfo :: !Info
  } deriving (Functor,Foldable,Traversable,Generic)

deriving instance (Show1 Term, Show n) => Show (Lam n)
deriving instance (Eq1 Term, Eq n) => Eq (Lam n)

instance HasInfo (Lam n) where getInfo = _lamInfo

instance Pretty n => Pretty (Lam n) where
  pretty (Lam arg ty _ _) =
    pretty arg <> ":" <> pretty (_ftReturn ty) <+> "lambda" <> parensSep (pretty <$> _ftArgs ty) <+> "..."

instance NFData n => NFData (Lam n)

instance J.Encode n => J.Encode (Lam n) where
  build o = J.object
    [ "amArg" J..= _lamArg o
    , "amInfo" J..= _lamInfo o
    , "amBindBody" J..= _lamBindBody o
    , "amTy" J..= _lamTy o
    ]
  {-# INLINEABLE build #-}

instance FromJSON n => FromJSON (Lam n) where
  parseJSON = lensyParseJSON 2

instance (SizeOf n) => SizeOf (Lam n)

-- -------------------------------------------------------------------------- --
-- Object

-- | Full Term object.
data Object n = Object
  { _oObject :: !(ObjectMap (Term n))
  , _oObjectType :: !(Type (Term n))
  , _oKeyOrder :: !(Maybe [FieldKey])
  , _oInfo :: !Info
  } deriving (Functor,Foldable,Traversable,Generic)

deriving instance (Show1 Term, Show n) => Show (Object n)
deriving instance (Eq1 Term, Eq n) => Eq (Object n)

instance HasInfo (Object n) where getInfo = _oInfo

instance Pretty n => Pretty (Object n) where
  pretty (Object bs _ Nothing _) = pretty bs
  pretty (Object (ObjectMap om) _ (Just ko) _) =
    annotate Val $ commaBraces $
    map (\(k,v) -> pretty k <> ": " <> pretty v) $
    sortBy (compare `on` (keyOrder . fst)) $
    M.toList om
    where keyOrder f = elemIndex f ko

instance NFData n => NFData (Object n)

instance J.Encode n => J.Encode (Object n) where
  build o = J.object
    [ "obj" J..= _oObject o
    , "keyorder" J..?= (J.Array <$> _oKeyOrder o)
    , "type" J..= _oObjectType o
    , "i" J..= _oInfo o
    ]
  {-# INLINEABLE build #-}

instance FromJSON n => FromJSON (Object n) where
  parseJSON = withObject "Object" $ \o ->
    Object <$> o .: "obj" <*> o .: "type" <*> o .:? "keyorder" <*> o .: "i"

instance (SizeOf n) => SizeOf (Object n)

-- -------------------------------------------------------------------------- --
-- Term

-- | Pact evaluable term.
data Term n =
    TModule {
      _tModuleDef :: !(ModuleDef (Term n))
    , _tModuleBody :: !(Scope () Term n)
    , _tInfo :: !Info
    } |
    TList {
      _tList :: !(Vector (Term n))
    , _tListType :: !(Type (Term n))
    , _tInfo :: !Info
    } |
    TDef {
      _tDef :: !(Def n)
    , _tInfo :: !Info
    } |
    TNative {
      _tNativeName :: !NativeDefName
    , _tNativeFun :: !NativeDFun
    , _tFunTypes :: !(FunTypes (Term n))
    , _tNativeExamples :: ![Example]
    , _tNativeDocs :: !Text
    , _tNativeTopLevelOnly :: !Bool
    , _tInfo :: !Info
    } |
    TConst {
      _tConstArg :: !(Arg (Term n))
    , _tModule :: !(Maybe ModuleName)
    , _tConstVal :: !(ConstVal (Term n))
    , _tMeta :: !Meta
    , _tInfo :: !Info
    } |
    TApp {
      _tApp :: !(App (Term n))
    , _tInfo :: !Info
    } |
    TVar {
      _tVar :: !n
    , _tInfo :: !Info
    } |
    TBinding {
      _tBindPairs :: ![BindPair (Term n)]
    , _tBindBody :: !(Scope Int Term n)
    , _tBindType :: !(BindType (Type (Term n)))
    , _tInfo :: !Info
    } |
    TLam {
      _tLam :: Lam n
    , _tInfo :: !Info
    } |
    TObject {
      _tObject :: !(Object n)
    , _tInfo :: !Info
    } |
    TSchema {
      _tSchemaName :: !TypeName
    , _tModule :: !(Maybe ModuleName)
    , _tMeta :: !Meta
    , _tFields :: ![Arg (Term n)]
    , _tInfo :: !Info
    } |
    TLiteral {
      _tLiteral :: !Literal
    , _tInfo :: !Info
    } |
    TGuard {
      _tGuard :: !(Guard (Term n))
    , _tInfo :: !Info
    } |
    TUse {
      _tUse :: !Use
    , _tInfo :: !Info
    } |
    TStep {
      _tStep :: !(Step (Term n))
    , _tMeta :: !Meta
    , _tInfo :: !Info
    } |
    TModRef {
      _tModRef :: !ModRef
    , _tInfo :: !Info
    } |
    TTable {
      _tTableName :: !TableName
    , _tModuleName :: !ModuleName
    , _tHash :: !ModuleHash
    , _tTableType :: !(Type (Term n))
    , _tMeta :: !Meta
    , _tInfo :: !Info
    } |
    TDynamic {
      _tDynModRef :: !(Term n)
    , _tDynMember :: !(Term n)
    , _tInfo :: !Info
    }
    deriving (Functor,Foldable,Traversable,Generic)

deriving instance (Show1 Term, Show n) => Show (Term n)
deriving instance (Eq1 Term, Eq n) => Eq (Term n)
instance NFData n => NFData (Term n)

instance HasInfo (Term n) where
  getInfo t = case t of
    TApp{..} -> getInfo _tApp
    TBinding{..} -> _tInfo
    TConst{..} -> _tInfo
    TDef{..} -> getInfo _tDef
    TGuard{..} -> _tInfo
    TList{..} -> _tInfo
    TLiteral{..} -> _tInfo
    TModule{..} -> _tInfo
    TNative{..} -> _tInfo
    TObject{..} -> getInfo _tObject
    TSchema{..} -> _tInfo
    TLam{..} -> _tInfo
    TStep{..} -> _tInfo
    TTable{..} -> _tInfo
    TUse{..} -> getInfo _tUse
    TVar{..} -> _tInfo
    TDynamic{..} -> _tInfo
    TModRef{..} -> _tInfo

instance Pretty n => Pretty (Term n) where
  pretty = \case
    TModule{..} -> pretty _tModuleDef
    TList{..} -> bracketsSep $ pretty <$> V.toList _tList
    TDef{..} -> pretty _tDef
    TNative{..} -> annotate Header ("native `" <> pretty _tNativeName <> "`")
      <> nest 2 (
         line
      <> line <> fillSep (pretty <$> T.words _tNativeDocs)
      <> line
      <> line <> annotate Header "Type:"
      <> line <> align (vsep (prettyFunType <$> toList _tFunTypes))
      <> examples
      ) where examples = case _tNativeExamples of
                [] -> mempty
                exs ->
                     line <> line <> annotate Header "Examples:"
                  <> line <> align (vsep (pretty <$> exs))
    TConst{..} -> "constant "
        <> maybe "" ((<>) "." .  pretty) _tModule
        <> pretty _tConstArg
        <> " " <> pretty _tMeta
    TApp a _ -> pretty a
    TVar n _ -> pretty n
    TBinding pairs body BindLet _i -> parensSep
      [ "let"
      , parensSep $ fmap pretty pairs
      , pretty $ unscope body
      ]
    TBinding pairs body (BindSchema _) _i -> parensSep
      [ commaBraces $ fmap pretty pairs
      , pretty $ unscope body
      ]
    TLam lam _ -> pretty lam
    TObject o _ -> pretty o
    TLiteral l _ -> annotate Val $ pretty l
    TGuard k _ -> pretty k
    TUse u _ -> pretty u
    TStep s _meta _i -> pretty s
    TSchema{..} -> parensSep
      [ "defschema"
      , pretty _tSchemaName
      , pretty _tMeta
      , prettyList _tFields
      ]
    TTable{..} -> parensSep
      [ "deftable"
      , pretty _tTableName <> ":" <> pretty (fmap prettyTypeTerm _tTableType)
      , pretty _tMeta
      ]
    TDynamic ref var _i -> pretty ref <> "::" <> pretty var
    TModRef mr _ -> pretty mr
    where
      prettyFunType (FunType as r) = pretty (FunType (map (fmap prettyTypeTerm) as) (prettyTypeTerm <$> r))

prettyTypeTerm :: Term n -> SpecialPretty (Term n)
prettyTypeTerm TSchema{..} = SPSpecial ("{" <> asString _tSchemaName <> "}")
prettyTypeTerm t = SPNormal t

instance SizeOf1 Term where
  sizeOf1 ver = \case
    TModule defn body info ->
      constructorCost 3 + sizeOf ver defn + sizeOf ver body + sizeOf ver info
    TList li typ info ->
       constructorCost 3 + sizeOf ver li + sizeOf ver typ + sizeOf ver info
    TDef defn info ->
      constructorCost 2 + sizeOf ver defn + sizeOf ver info
    -- note: we actually strip docs and examples
    -- post fork
    TNative name _defun ftyps examples docs tlo info ->
      constructorCost 7 + sizeOf ver name + sizeOf ver ftyps + sizeOf ver examples +
        sizeOf ver docs + sizeOf ver tlo + sizeOf ver info
    TConst arg mname cval meta info  ->
      constructorCost 5 + sizeOf ver arg + sizeOf ver mname + sizeOf ver cval + sizeOf ver meta + sizeOf ver info
    TApp app info ->
      constructorCost 2 + sizeOf ver app + sizeOf ver info
    TVar v info ->
      constructorCost 2 + sizeOf ver v + sizeOf ver info
    TBinding bps body btyp info ->
      constructorCost 4 + sizeOf ver bps + sizeOf ver body + sizeOf ver btyp + sizeOf ver info
    TLam lam info ->
      constructorCost 2 + sizeOf ver lam + sizeOf ver info
    TObject obj info ->
      constructorCost 2 + sizeOf ver obj + sizeOf ver info
    TSchema tn mn meta args info ->
      constructorCost 5 + sizeOf ver tn + sizeOf ver mn + sizeOf ver meta + sizeOf ver args + sizeOf ver info
    TLiteral lit info ->
      constructorCost 2 + sizeOf ver lit + sizeOf ver info
    TGuard g info ->
      constructorCost 2 + sizeOf ver g + sizeOf ver info
    TUse u info->
      constructorCost 2 + sizeOf ver u + sizeOf ver info
    TStep step meta info ->
      constructorCost 3 + sizeOf ver step + sizeOf ver meta + sizeOf ver info
    TModRef mr info ->
      constructorCost 2 + sizeOf ver mr + sizeOf ver info
    TTable tn mn hs typ meta info ->
      constructorCost 6 + sizeOf ver tn + sizeOf ver mn + sizeOf ver hs + sizeOf ver typ
        + sizeOf ver meta + sizeOf ver info
    TDynamic e1 e2 info ->
      constructorCost 3 + sizeOf ver e1 + sizeOf ver e2 + sizeOf ver info

instance (SizeOf a) => SizeOf (Term a) where
  sizeOf = sizeOf1

instance Applicative Term where
    pure a = TVar a def
    (<*>) = ap

instance Monad Term where
    return = pure
    TModule m b i >>= f = TModule (fmap (>>= f) m) (b >>>= f) i
    TList bs t i >>= f = TList (V.map (>>= f) bs) (fmap (>>= f) t) i
    TDef (Def n m dt ft b d dm i) i' >>= f =
      TDef (Def n m dt (fmap (>>= f) ft) (b >>>= f) d (fmap (fmap (>>= f)) dm) i) i'
    TNative n fn t exs d tl i >>= f = TNative n fn (fmap (fmap (>>= f)) t) exs d tl i
    TConst d m c t i >>= f = TConst (fmap (>>= f) d) m (fmap (>>= f) c) t i
    TApp a i >>= f = TApp (fmap (>>= f) a) i
    TVar n i >>= f = (f n) { _tInfo = i }
    TBinding bs b c i >>= f =
      TBinding (map (fmap (>>= f)) bs) (b >>>= f) (fmap (fmap (>>= f)) c) i
    TLam (Lam arg ty b i) i' >>= f =
      TLam (Lam arg (fmap (>>= f) ty) (b >>>= f) i) i'
    TObject (Object bs t kf oi) i >>= f =
      TObject (Object (fmap (>>= f) bs) (fmap (>>= f) t) kf oi) i
    TLiteral l i >>= _ = TLiteral l i
    TGuard g i >>= f = TGuard (fmap (>>= f) g) i
    TUse u i >>= _ = TUse u i
    TStep (Step ent e r si) meta i >>= f =
      TStep (Step (fmap (>>= f) ent) (e >>= f) (fmap (>>= f) r) si) meta i
    TSchema {..} >>= f =
      TSchema _tSchemaName _tModule _tMeta (fmap (fmap (>>= f)) _tFields) _tInfo
    TTable {..} >>= f =
      TTable _tTableName _tModuleName _tHash (fmap (>>= f) _tTableType) _tMeta _tInfo
    TDynamic r m i >>= f = TDynamic (r >>= f) (m >>= f) i
    TModRef mr i >>= _ = TModRef mr i

-- | JSON Properties Vocabulary for Term
--
data TermProperties
  = TermArgs
  | TermBody
  | TermConstArg
  | TermConstVal
  | TermDefBody
  | TermDefMeta
  | TermDefName
  | TermDefType
  | TermDynMem
  | TermDynRef
  | TermFields
  | TermFun
  | TermFunType
  | TermGuard
  | TermHash
  | TermI
  | TermInfo
  | TermLamArg
  | TermLamBindBody
  | TermLamInfo
  | TermLamTy
  | TermList
  | TermLiteral
  | TermMeta
  | TermModName
  | TermModRefInfo
  | TermModRefName
  | TermModRefSpec
  | TermModule
  | TermName
  | TermNatDocs
  | TermNatExamples
  | TermNatFunTypes
  | TermNatTopLevel
  | TermObjectKeyorder
  | TermObjectObj
  | TermPairs
  | TermType
  | TermUnknown !String
  | TermUseImports
  | TermVar

  deriving (Show, Eq, Ord)

prop :: IsString a => Semigroup a => TermProperties -> a
prop TermArgs = "args"
prop TermBody = "body"
prop TermConstArg = "arg"
prop TermConstVal = "val"
prop TermDefBody = "defBody"
prop TermDefMeta = "defMeta"
prop TermDefName = "defName"
prop TermDefType = "defType"
prop TermDynMem = "dmem"
prop TermDynRef = "dref"
prop TermFields = "fields"
prop TermFun = "fun"
prop TermFunType = "funType"
prop TermGuard = "guard"
prop TermHash = "hash"
prop TermI = "i"
prop TermInfo = "info"
prop TermLamArg = "amArg"
prop TermLamBindBody = "amBindBody"
prop TermLamInfo = "amInfo"
prop TermLamTy = "amTy"
prop TermList = "list"
prop TermLiteral = "lit"
prop TermMeta = "meta"
prop TermModName = "modname"
prop TermModRefInfo = "refInfo"
prop TermModRefName = "refName"
prop TermModRefSpec = "refSpec"
prop TermModule = "module"
prop TermName = "name"
prop TermNatDocs = "docs"
prop TermNatExamples = "examples"
prop TermNatFunTypes = "types"
prop TermNatTopLevel = "tl"
prop TermObjectKeyorder = "keyorder"
prop TermObjectObj = "obj"
prop TermPairs = "pairs"
prop TermType = "type"
prop (TermUnknown t) = "UNKNOWN_TERM[" <> fromString t <> "]"
prop TermUseImports = "imports"
prop TermVar = "var"
{-# INLINE prop #-}

unprop :: IsString a => Eq a => Show a => a -> TermProperties
unprop "args" = TermArgs
unprop "body" = TermBody
unprop "arg" = TermConstArg
unprop "val" = TermConstVal
unprop "defBody" = TermDefBody
unprop "defMeta" = TermDefMeta
unprop "defName" = TermDefName
unprop "defType" = TermDefType
unprop "dmem" = TermDynMem
unprop "dref" = TermDynRef
unprop "fields" = TermFields
unprop "fun" = TermFun
unprop "funType" = TermFunType
unprop "guard" = TermGuard
unprop "hash" = TermHash
unprop "i" = TermI
unprop "info" = TermInfo
unprop "amArg" = TermLamArg
unprop "amBindBody" = TermLamBindBody
unprop "amInfo" = TermLamInfo
unprop "amTy" = TermLamTy
unprop "list" = TermList
unprop "lit" = TermLiteral
unprop "meta" = TermMeta
unprop "modname" = TermModName
unprop "refInfo" = TermModRefInfo
unprop "refName" = TermModRefName
unprop "refSpec" = TermModRefSpec
unprop "module" = TermModule
unprop "name" = TermName
unprop "docs" = TermNatDocs
unprop "examples" = TermNatExamples
unprop "types" = TermNatFunTypes
unprop "tl" = TermNatTopLevel
unprop "keyorder" = TermObjectKeyorder
unprop "obj" = TermObjectObj
unprop "pairs" = TermPairs
unprop "type" = TermType
unprop "imports" = TermUseImports
unprop "var" = TermVar
unprop t = TermUnknown (show t)
{-# INLINE unprop #-}

instance J.Encode n => J.Encode (Term n) where
  build = \case
    (TModule d b i) -> J.object
      [ prop TermBody J..= b
      , prop TermModule J..= d
      , inf i
      ]
    (TList ts ty i) -> J.object
      [ prop TermList J..= J.Array ts
      , prop TermType J..= ty
      , inf i
      ]
    (TDef d _i) -> J.build d
    -- TNative intentionally not marshallable
    (TNative n _fn tys _exs d tl i) -> J.object
      [ prop TermNatFunTypes J..= J.Array tys
      , prop TermName J..= n
      , prop TermFun J..= J.null {- TODO fn -}
      , prop TermNatTopLevel J..= tl
      , prop TermNatExamples J..= J.null {- TODO exs -}
      , prop TermNatDocs J..= d
      , inf i
      ]
    (TConst d m c met i) -> J.object
      [ prop TermModName J..= m
      , prop TermConstArg J..= d
      , prop TermMeta J..= met
      , prop TermConstVal J..= c
      , inf i
      ]
    (TApp a _i) -> J.build a
    (TVar n i) -> J.object
      [ prop TermVar J..= n
      , inf i
      ]
    (TBinding bs b c i) -> J.object
      [ prop TermBody J..= b
      , prop TermPairs J..= J.Array bs
      , prop TermType J..= c
      , inf i
      ]
    (TObject o _i) -> J.build o
    (TLiteral l i) -> J.object
      [ inf i
      , prop TermLiteral J..= l
      ]
    (TLam tlam _i) -> J.build tlam
    (TGuard k i) -> J.object
      [ prop TermGuard J..= k
      , inf i
      ]
    (TUse u _i) -> J.build u
    (TStep s tmeta i) -> J.object
      [ prop TermBody J..= s
      , prop TermMeta J..= tmeta
      , inf i
      ]
    (TSchema sn smod smeta sfs i) -> J.object
      [ prop TermModName J..= smod
      , prop TermName J..= sn
      , prop TermMeta J..= smeta
      , inf i
      , prop TermFields J..= J.Array sfs
      ]
    (TTable tn tmod th tty tmeta i) -> J.object
      [ prop TermHash J..= th
      , prop TermModName J..= tmod
      , prop TermName J..= tn
      , prop TermMeta J..= tmeta
      , prop TermType J..= tty
      , inf i
      ]
    (TDynamic r m i) -> J.object
     [ prop TermDynRef J..= r
     , inf i
     , prop TermDynMem J..= m
     ]
    (TModRef mr _i) -> J.build mr
   where
    inf i = "i" J..= i

instance FromJSON n => FromJSON (Term n) where

  -- "info" and "i" may be optional, so we don't consider those for matching
  --
  parseJSON v = flip (A.<?>) (A.Key "Term") $ case propsWithoutOptionals of
    [TermBody, TermModule] ->  wo "Module" $ \o -> TModule
      <$> o .: p TermModule
      <*> o .: p TermBody
      <*> inf o
    [TermList, TermType] -> wo "List" $ \o -> TList
      <$> o .: p TermList
      <*> o .: p TermType
      <*> inf o
    [TermDefBody, TermDefMeta, TermDefName, TermDefType, TermFunType, TermMeta, TermModule] -> parseWithInfo TDef

    -- TNative intentionally not marshallable
    -- [TermFun, TermName, TermNatDocs, TermNatExamples, TermNatFunTypes, TermNatTopLevel] ->
    --   wo "Native" $ \o -> TNative
    --     <$> o .: p TermName
    --     <*> error "not supported" -- TermFun serialized as Null
    --     <*> o .: p TermFunType
    --     <*> return [] -- TermNatExamples serialized as Null
    --     <*> o .: p TermNatDocs
    --     <*> o .: p TermNatTopLevel
    --     <*> inf o

    [TermConstArg, TermConstVal, TermMeta, TermModName] -> wo "Const" $ \o -> TConst
      <$> o .: p TermConstArg
      <*> o .: p TermModName
      <*> o .: p TermConstVal
      <*> o .: p TermMeta
      <*> inf o
    [TermArgs, TermFun] -> parseWithInfo TApp
    [TermVar] -> wo "Var" $ \o -> TVar
        <$>  o .: p TermVar
        <*> inf o
    [TermBody, TermPairs, TermType] -> wo "Binding" $ \o -> TBinding
      <$> o .: p TermPairs
      <*> o .: p TermBody
      <*> o .: p TermType
      <*> inf o
    [TermObjectObj, TermType] -> parseWithInfo TObject -- FIXME keyorder is optional
    [TermLiteral] -> wo "Literal" $ \o -> TLiteral
      <$> o .: p TermLiteral
      <*> inf o
    [TermGuard] -> wo "Guard" $ \o -> TGuard
      <$> o .: p TermGuard
      <*> inf o
    [TermHash, TermModule, TermUseImports] -> parseWithInfo TUse
    [TermLamArg, TermLamBindBody, TermLamInfo, TermLamTy] -> parseWithInfo TLam
    [TermBody, TermMeta] -> wo "Step" $ \o -> TStep
      <$> o .: p TermBody
      <*> o .: p TermMeta
      <*> inf o
     --  parseWithInfo TStep
    [TermFields, TermMeta, TermModName, TermName] -> wo "Schema" $ \o -> TSchema
      <$>  o .: p TermName
      <*> o .: p TermModName
      <*> o .: p TermMeta
      <*> o .: p TermFields
      <*> inf o
    [TermHash, TermMeta, TermModName, TermName, TermType] -> wo "Table" $ \o -> TTable
      <$>  o .: p TermName
      <*> o .: p TermModName
      <*> o .: p TermHash
      <*> o .: p TermType
      <*> o .: p TermMeta
      <*> inf o
    [TermDynMem, TermDynRef] -> wo "Dynamic" $ \o -> TDynamic
      <$> o .: p TermDynRef
      <*> o .: p TermDynMem
      <*> inf o
    [TermModRefInfo, TermModRefName, TermModRefSpec] -> parseWithInfo TModRef
    _ -> fail $ "unexpected properties for Term: "
      <> "[" <> T.unpack (T.intercalate "," (props v)) <> "]"
      <> ", " <> show propsWithoutOptionals
      <> ", " <> show (J.encode v)
    -- A.<?> A.Key (A.fromText $ "Term[" <> T.intercalate "," (props v) <> "]")
   where
    p = prop
    inf o = o .:? "i" .!= Info Nothing
    wo n f = withObject n f v
    props (A.Object m) = A.toText <$> A.keys m
    props _ = []

    propsWithoutOptionals = sort $ unprop
        <$> filter (\x -> x `notElem` ["i", "info", "keyorder"]) (props v)

    parseWithInfo :: HasInfo a => FromJSON a => (a -> Info -> Term n) -> A.Parser (Term n)
    parseWithInfo f = (\a -> f a $ getInfo a) <$> parseJSON v

-- -------------------------------------------------------------------------- --
-- ToTerm

class ToTerm a where
    toTerm :: a -> Term m
instance ToTerm Bool where toTerm = tLit . LBool
instance ToTerm Integer where toTerm = tLit . LInteger
instance ToTerm Int where toTerm = tLit . LInteger . fromIntegral
instance ToTerm Decimal where toTerm = tLit . LDecimal
instance ToTerm Text where toTerm = tLit . LString
instance ToTerm KeySet where toTerm k = TGuard (GKeySet k) def
instance ToTerm Literal where toTerm = tLit
instance ToTerm UTCTime where toTerm = tLit . LTime
instance ToTerm Word32 where toTerm = tLit . LInteger . fromIntegral
instance ToTerm Word64 where toTerm = tLit . LInteger . fromIntegral
instance ToTerm Int64 where toTerm = tLit . LInteger . fromIntegral
instance ToTerm TableName where toTerm = tLit . LString . asString
instance ToTerm PactId where toTerm (PactId t) = toTerm t

toTermList :: (ToTerm a,Foldable f) => Type (Term b) -> f a -> Term b
toTermList ty l = TList (V.map toTerm (V.fromList (toList l))) ty def

-- | Convenience for OverloadedStrings annoyances
tStr :: Text -> Term n
tStr = toTerm

-- -------------------------------------------------------------------------- --
-- Miscelaneous Functions

tLit :: Literal -> Term n
tLit = (`TLiteral` def)
{-# INLINE tLit #-}

-- Support 'termEq' in Ref'
refEq :: Eq1 Guard => Eq n => Ref' (Term n) -> Ref' (Term n) -> Bool
refEq a b = case (a,b) of
  (Direct x,Direct y) -> termEq x y
  (Ref x,Ref y) -> termEq1 refEq x y
  _ -> False

toTObject :: Type (Term n) -> Info -> [(FieldKey,Term n)] -> Term n
toTObject ty i = toTObjectMap ty i . ObjectMap . M.fromList

toTObjectMap :: Type (Term n) -> Info -> ObjectMap (Term n) -> Term n
toTObjectMap ty i m = TObject (Object m ty def i) i

toTList :: Type (Term n) -> Info -> [Term n] -> Term n
toTList ty i = toTListV ty i . V.fromList

toTListV :: Type (Term n) -> Info -> V.Vector (Term n) -> Term n
toTListV ty i vs = TList vs ty i

guardTypeOf :: Guard a -> GuardType
guardTypeOf g = case g of
  GKeySet{} -> GTyKeySet
  GKeySetRef{} -> GTyKeySetName
  GPact {} -> GTyPact
  GUser {} -> GTyUser
  GModule {} -> GTyModule
  GCapability {} -> GTyCapability

-- | Return a Pact type, or a String description of non-value Terms.
-- Does not handle partial schema types.
typeof :: Term a -> Either Text (Type (Term a))
typeof t = case t of
      TLiteral l _ -> Right $ TyPrim $ litToPrim l
      TModule{}-> Left "module"
      TList {..} -> Right $ TyList _tListType
      TDef{..} -> Right $ TyFun (_dFunType _tDef)
      TNative {} -> Left "defun"
      TConst {..} -> Left $ "const:" <> _aName _tConstArg
      TApp {} -> Left "app"
      TVar {} -> Left "var"
      TBinding {..} -> case _tBindType of
        BindLet -> Left "let"
        BindSchema bt -> Right $ TySchema TyBinding bt def
      -- This will likely change later on.
      TLam{..} -> Right $ TyFun (_lamTy _tLam)
      TObject (Object {..}) _ -> Right $ TySchema TyObject _oObjectType def
      TGuard {..} -> Right $ TyPrim $ TyGuard $ Just $ guardTypeOf _tGuard
      TUse {} -> Left "use"
      TStep {} -> Left "step"
      TSchema {..} -> Left $ "defobject:" <> asString _tSchemaName
      TTable {..} -> Right $ TySchema TyTable _tTableType def
      TDynamic {} -> Left "dynamic"
      TModRef m _ -> Right $ modRefTy m
{-# INLINE typeof #-}

-- | Populate 'TyModule' using a 'ModRef'
modRefTy :: ModRef -> Type (Term a)
modRefTy (ModRef _mn is _) = TyModule $ fmap (map (\i -> TModRef (ModRef i Nothing def) def)) is

-- | Return string type description.
typeof' :: Pretty a => Term a -> Text
typeof' = either id renderCompactText . typeof

pattern TLitString :: Text -> Term t
pattern TLitString s <- TLiteral (LString s) _
pattern TLitInteger :: Integer -> Term t
pattern TLitInteger i <- TLiteral (LInteger i) _
pattern TLitBool :: Bool -> Term t
pattern TLitBool b <- TLiteral (LBool b) _


-- | Equality dictionary for term-level equality
--
canEq :: Term n -> Term n -> Bool
canEq TList{} TList{} = True
canEq TObject{} TObject{} = True
canEq TLiteral{} TLiteral{} = True
canEq TTable{} TTable{} = True
canEq TSchema{} TSchema{} = True
canEq TGuard{} TGuard{} = True
canEq TModRef{} TModRef{} = True
canEq _ _ = False

-- | Support pact `=` for value-level terms
-- and TVar for types.
termEq :: Eq1 Guard => Eq n => Term n -> Term n -> Bool
termEq = termEq1 (==)

-- | Support 'termEq' for 'Term Ref'
termRefEq :: Eq1 Guard => Term Ref -> Term Ref -> Bool
termRefEq = termEq1 refEq

-- | Lifted version; support pact `=` for value-level terms
-- and TVar for types.
termEq1 :: Eq1 Guard => (n -> n -> Bool) -> Term n -> Term n -> Bool
termEq1 eq (TList a _ _) (TList b _ _) = length a == length b && and (V.zipWith (termEq1 eq) a b)
termEq1 eq (TObject (Object (ObjectMap a) _ _ _) _) (TObject (Object (ObjectMap b) _ _ _) _) =
  -- O(3n), 2x M.toList + short circuiting walk
  M.size a == M.size b && go (M.toList a) (M.toList b) True
    where go _ _ False = False
          go ((k1,v1):r1) ((k2,v2):r2) _ = go r1 r2 $ k1 == k2 && termEq1 eq v1 v2
          go [] [] _ = True
          go _ _ _ = False
termEq1 _ (TLiteral a _) (TLiteral b _) = a == b
termEq1 eq (TGuard a _) (TGuard b _) = liftEq (termEq1 eq) a b
termEq1 eq (TTable a b c d x _) (TTable e f g h y _) = a == e && b == f && c == g && liftEq (termEq1 eq) d h && x == y
termEq1 eq (TSchema a b c d _) (TSchema e f g h _) = a == e && b == f && c == g && length d == length h && and (zipWith (argEq1 (termEq1 eq)) d h)
termEq1 eq (TVar a _) (TVar b _) = eq a b
termEq1 _ (TModRef (ModRef am as _) _) (TModRef (ModRef bm bs _) _) = am == bm && as == bs
termEq1 _ _ _ = False

-- | Workhorse runtime typechecker on `Type (Term n)` to specialize
-- 'unifiesWith' on Term/'termEq'.
-- First argument is spec, second is value.
canUnifyWith :: Eq1 Guard => Eq n => Type (Term n) -> Type (Term n) -> Bool
canUnifyWith = unifiesWith termEq

-- -------------------------------------------------------------------------- --
-- Lenses

makeLenses ''FunApp
makePrisms ''Ref'
makeLenses ''Def
makeLenses ''Lam
makeLenses ''Object
makeLenses ''Term
makePrisms ''Term

-- This noop TH splice is required to ensure that all types that are defined
-- above in this module are available in the type environment of the following
-- TH splices.
--
return []

-- -------------------------------------------------------------------------- --
-- Eq1 Instances

instance Eq1 Def where
  liftEq = $(makeLiftEq ''Def)
instance Eq1 Lam where
  liftEq = $(makeLiftEq ''Lam)
instance Eq1 Object where
  liftEq = $(makeLiftEq ''Object)
instance Eq1 Term where
  liftEq = $(makeLiftEq ''Term)

-- -------------------------------------------------------------------------- --
-- Show1 Instances

instance Show1 Def where
  liftShowsPrec = $(makeLiftShowsPrec ''Def)
instance Show1 Lam where
  liftShowsPrec = $(makeLiftShowsPrec ''Lam)
instance Show1 Object where
  liftShowsPrec = $(makeLiftShowsPrec ''Object)
instance Show1 Term where
  liftShowsPrec = $(makeLiftShowsPrec ''Term)

