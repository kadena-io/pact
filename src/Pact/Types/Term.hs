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
 ( Namespace(..), nsName, nsUser, nsAdmin,
   Meta(..),mDocs,mModel,
   PublicKey(..),
   KeySet(..), mkKeySet,
   KeySetName(..),
   PactGuard(..),
   PactId(..),
   UserGuard(..),
   ModuleGuard(..),
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
   ModRef(..),modRefName,modRefSpec,modRefInfo,
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
import Control.Lens hiding ((.=), DefName(..))
import Control.Monad
#if MIN_VERSION_aeson(1,4,3)
import Data.Aeson hiding (pairs,Object, (<?>))
#else
import Data.Aeson hiding (pairs,Object)
#endif
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import Data.Decimal
import Data.Default
import Data.Eq.Deriving
import Data.Foldable
import Data.Functor.Classes (Eq1(..), Show1(..))
import Data.Function
import qualified Data.HashSet as HS
import Data.Hashable
import Data.Int (Int64)
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Serialize (Serialize)
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Pact.Time (UTCTime)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word64, Word32)
import GHC.Generics (Generic)
import Prelude
import Test.QuickCheck hiding (Success)
import Text.Show.Deriving

import Pact.Types.Codec
import Pact.Types.Exp
import Pact.Types.Hash
import Pact.Types.Info
import Pact.Types.KeySet
import Pact.Types.Names
import Pact.Types.Pretty hiding (dot)
import Pact.Types.SizeOf
import Pact.Types.Type
import Pact.Types.Util

-- -------------------------------------------------------------------------- --
-- Meta

data Meta = Meta
  { _mDocs  :: !(Maybe Text) -- ^ docs
  , _mModel :: ![Exp Info]   -- ^ models
  } deriving (Eq, Show, Generic)

instance Pretty Meta where
  pretty (Meta (Just doc) model) = dquotes (pretty doc) <> line <> prettyModel model
  pretty (Meta Nothing    model) = prettyModel model

instance NFData Meta

instance SizeOf Meta

prettyModel :: [Exp Info] -> Doc
prettyModel []    = mempty
prettyModel props = "@model " <> list (pretty <$> props)

metaProperties :: JsonProperties Meta
metaProperties o =
  [ "model" .= _mModel o
  , "docs" .= _mDocs o
  ]
{-# INLINE metaProperties #-}

instance ToJSON Meta where
  toJSON = enableToJSON "Pact.Types.Term.Meta" . lensyToJSON 2
  toEncoding = A.pairs . mconcat . metaProperties
  {-# INLINE toJSON #-}
  {-# INLINE toEncoding #-}


instance FromJSON Meta where parseJSON = lensyParseJSON 2

instance Default Meta where def = Meta def def

instance Semigroup Meta where
  (Meta d m) <> (Meta d' m') = Meta (d <> d') (m <> m')

instance Monoid Meta where
  mempty = Meta Nothing []



-- -------------------------------------------------------------------------- --
-- PactId

newtype PactId = PactId Text
    deriving (Eq,Ord,Show,Pretty,AsString,IsString,FromJSON,ToJSON,Generic,NFData,SizeOf)

instance Arbitrary PactId where
  arbitrary = PactId <$> hashToText <$> arbitrary

-- -------------------------------------------------------------------------- --
-- UserGuard

data UserGuard a = UserGuard
  { _ugFun :: !Name
  , _ugArgs :: ![a]
  } deriving (Eq,Generic,Show,Functor,Foldable,Traversable,Ord)

instance (Arbitrary a) => Arbitrary (UserGuard a) where
  arbitrary = UserGuard <$> arbitrary <*> arbitrary

instance NFData a => NFData (UserGuard a)

instance Pretty a => Pretty (UserGuard a) where
  pretty UserGuard{..} = "UserGuard" <+> commaBraces
    [ "fun: " <> pretty _ugFun
    , "args: " <> pretty _ugArgs
    ]

instance (SizeOf p) => SizeOf (UserGuard p) where
  sizeOf (UserGuard n arr) =
    (constructorCost 2) + (sizeOf n) + (sizeOf arr)

userGuardProperties :: ToJSON a => JsonProperties (UserGuard a)
userGuardProperties o =
  [ "args" .= _ugArgs o
  , "fun" .= _ugFun o
  ]
{-# INLINE userGuardProperties #-}

instance ToJSON a => ToJSON (UserGuard a) where
  toJSON = enableToJSON "Pact.Types.Term.UserGuard" . lensyToJSON 3
  toEncoding = A.pairs . mconcat . userGuardProperties
  {-# INLINE toJSON #-}
  {-# INLINE toEncoding #-}

instance FromJSON a => FromJSON (UserGuard a) where parseJSON = lensyParseJSON 3

-- -------------------------------------------------------------------------- --
-- DefType

data DefType
  = Defun
  | Defpact
  | Defcap
  deriving (Eq,Show,Generic, Bounded, Enum)

instance FromJSON DefType
instance ToJSON DefType
instance NFData DefType

instance SizeOf DefType where
  sizeOf _ = 0

defTypeRep :: DefType -> String
defTypeRep Defun = "defun"
defTypeRep Defpact = "defpact"
defTypeRep Defcap = "defcap"

instance Pretty DefType where
  pretty = prettyString . defTypeRep

-- -------------------------------------------------------------------------- --
-- Gas

-- | Gas compute cost unit.
newtype Gas = Gas Int64
  deriving (Eq,Ord,Num,Real,Integral,Enum,ToJSON,FromJSON,Generic,NFData)

instance Show Gas where show (Gas g) = show g

instance Pretty Gas where
  pretty (Gas i) = pretty i

instance Semigroup Gas where
  (Gas a) <> (Gas b) = Gas $ a + b

instance Monoid Gas where
  mempty = 0

-- -------------------------------------------------------------------------- --
-- BindType

-- | Binding forms.
data BindType n =
  -- | Normal "let" bind
  BindLet |
  -- | Schema-style binding, with string value for key
  BindSchema { _bType :: !n }
  deriving (Eq,Functor,Foldable,Traversable,Ord,Show,Generic)

instance (Pretty n) => Pretty (BindType n) where
  pretty BindLet = "let"
  pretty (BindSchema b) = "bind" <> pretty b

instance ToJSON n => ToJSON (BindType n) where
  toJSON BindLet = "let"
  toJSON (BindSchema s) = object [ "bind" .= s ]

  toEncoding BindLet = toEncoding ("let" :: String)
  toEncoding (BindSchema s) = A.pairs ("bind" .= s)

  {-# INLINE toJSON #-}
  {-# INLINE toEncoding #-}

instance FromJSON n => FromJSON (BindType n) where
  parseJSON v =
    withThisText "BindLet" "let" v (pure BindLet) <|>
    withObject "BindSchema" (\o -> BindSchema <$> o .: "bind") v

instance NFData n => NFData (BindType n)

instance (SizeOf n) => SizeOf (BindType n)

-- -------------------------------------------------------------------------- --
-- BindPair

data BindPair n = BindPair
  { _bpArg :: !(Arg n)
  , _bpVal :: !n }
  deriving (Eq,Show,Functor,Traversable,Foldable,Generic)

toBindPairs :: BindPair n -> (Arg n,n)
toBindPairs (BindPair a v) = (a,v)

instance Pretty n => Pretty (BindPair n) where
  pretty (BindPair arg body) = pretty arg <+> pretty body

instance NFData n => NFData (BindPair n)

bindPairProperties :: ToJSON n => JsonProperties (BindPair n)
bindPairProperties o =
  [ "arg" .= _bpArg o
  , "val" .= _bpVal o
  ]
{-# INLINE bindPairProperties #-}

instance ToJSON n => ToJSON (BindPair n) where
  toJSON = enableToJSON "Pact.Types.Term.BindPair n" . lensyToJSON 3
  toEncoding = A.pairs . mconcat . bindPairProperties
  {-# INLINE toJSON #-}
  {-# INLINE toEncoding #-}

instance FromJSON n => FromJSON (BindPair n) where parseJSON = lensyParseJSON 3

instance (SizeOf n) => SizeOf (BindPair n)

-- -------------------------------------------------------------------------- --
-- App

data App t = App
  { _appFun :: !t
  , _appArgs :: ![t]
  , _appInfo :: !Info
  } deriving (Functor,Foldable,Traversable,Eq,Show,Generic)

instance HasInfo (App t) where getInfo = _appInfo

appProperties :: ToJSON t => JsonProperties (App t)
appProperties o =
  [ "args" .= _appArgs o
  , "fun" .= _appFun o
  , "info" .= _appInfo o
  ]
{-# INLINE appProperties #-}

instance ToJSON t => ToJSON (App t) where
  toJSON = enableToJSON "Pact.Types.Term.App t" . lensyToJSON 4
  toEncoding = A.pairs . mconcat . appProperties
  {-# INLINE toJSON #-}
  {-# INLINE toEncoding #-}

instance FromJSON t => FromJSON (App t) where parseJSON = lensyParseJSON 4

instance Pretty n => Pretty (App n) where
  pretty App{..} = parensSep $ pretty _appFun : map pretty _appArgs

instance NFData t => NFData (App t)

instance (SizeOf t) => SizeOf (App t)

-- -------------------------------------------------------------------------- --
-- Governance

newtype Governance g = Governance { _gGovernance :: Either KeySetName g }
  deriving (Eq,Ord,Functor,Foldable,Traversable,Show,NFData, Generic)

instance (SizeOf g) => SizeOf (Governance g)

instance Pretty g => Pretty (Governance g) where
  pretty = \case
    Governance (Left  k) -> pretty k
    Governance (Right r) -> pretty r

governanceProperties :: ToJSON g => JsonProperties (Governance g)
governanceProperties (Governance (Left ks)) = [ "keyset" .= ks ]
governanceProperties (Governance (Right c)) = [ "capability" .= c ]
{-# INLINE governanceProperties #-}

instance ToJSON g => ToJSON (Governance g) where
  toJSON = enableToJSON "Pact.Types.Term.Governance g" . object . governanceProperties
  toEncoding = A.pairs . mconcat . governanceProperties
  {-# INLINE toJSON #-}
  {-# INLINE toEncoding #-}

instance FromJSON g => FromJSON (Governance g) where
  parseJSON = withObject "Governance" $ \o ->
    Governance <$> (Left <$> o .: "keyset" <|>
                    Right <$> o .: "capability")

-- -------------------------------------------------------------------------- --
-- ModuleHash

-- | Newtype wrapper differentiating 'Hash'es from module hashes
--
newtype ModuleHash = ModuleHash { _mhHash :: Hash }
  deriving (Eq, Ord, Show, Generic, Hashable, Serialize, AsString, Pretty, ToJSON, FromJSON, ParseText)
  deriving newtype (NFData, SizeOf)

instance Arbitrary ModuleHash where
  -- Coin contract is about 20K characters
  arbitrary = ModuleHash <$> resize 20000 arbitrary

-- -------------------------------------------------------------------------- --
-- DefcapMeta

-- | Metadata specific to Defcaps.
data DefcapMeta n =
  DefcapManaged
  { _dcManaged :: !(Maybe (Text,n))
    -- ^ "Auto" managed or user-managed by (param,function)
  } |
  DefcapEvent
    -- ^ Eventing defcap.
  deriving (Functor,Foldable,Traversable,Generic,Eq,Show,Ord)

instance NFData n => NFData (DefcapMeta n)
instance Pretty n => Pretty (DefcapMeta n) where
  pretty (DefcapManaged m) = case m of
    Nothing -> tag
    Just (p,f) -> tag <> " " <> pretty p <> " " <> pretty f
    where
      tag = "@managed"
  pretty DefcapEvent = "@event"

defcapMetaManagedProperties :: ToJSON n => JsonProperties (Maybe (Text,n))
defcapMetaManagedProperties (Just (p,f)) =
  [ "managedParam" .= p
  , "managerFun" .= f
  ]
defcapMetaManagedProperties Nothing = [ "managerAuto" .= True ]
{-# INLINE defcapMetaManagedProperties #-}

instance ToJSON n => ToJSON (DefcapMeta n) where
  toJSON = enableToJSON "Pact.Types.Term.DefcapMeta n" . \case
    (DefcapManaged c) -> object $ defcapMetaManagedProperties c
    DefcapEvent -> "event"
  toEncoding (DefcapManaged c) = A.pairs . mconcat $ defcapMetaManagedProperties c
  toEncoding DefcapEvent = toEncoding ("event" :: String)
  {-# INLINE toJSON #-}
  {-# INLINE toEncoding #-}

instance FromJSON n => FromJSON (DefcapMeta n) where
  parseJSON v = parseUser v <|> parseAuto v <|> parseEvent v
    where
      parseUser = withObject "DefcapMeta" $ \o -> (DefcapManaged . Just) <$>
        ((,) <$> o .: "managedParam" <*> o .: "managerFun")
      parseAuto = withObject "DefcapMeta" $ \o -> do
        b <- o .: "managerAuto"
        if b then pure (DefcapManaged Nothing)
        else fail "Expected True"
      parseEvent = withText "DefcapMeta" $ \t ->
        if t == "event" then pure DefcapEvent
        else fail "Expected 'event'"

instance (SizeOf a) => SizeOf (DefcapMeta a)

-- | Def metadata specific to 'DefType'.
-- Currently only specified for Defcap.
data DefMeta n =
  DMDefcap !(DefcapMeta n)
  deriving (Functor,Foldable,Traversable,Generic,Eq,Show,Ord)
instance NFData n => NFData (DefMeta n)
instance Pretty n => Pretty (DefMeta n) where
  pretty (DMDefcap m) = pretty m
instance ToJSON n => ToJSON (DefMeta n) where
  toJSON (DMDefcap m) = toJSON m
  toEncoding (DMDefcap m) = toEncoding m
  {-# INLINE toJSON #-}
  {-# INLINE toEncoding #-}
instance FromJSON n => FromJSON (DefMeta n) where
  parseJSON = fmap DMDefcap . parseJSON

instance (SizeOf a) => SizeOf (DefMeta a)

-- -------------------------------------------------------------------------- --
-- ConstVal

data ConstVal n =
  CVRaw { _cvRaw :: !n } |
  CVEval { _cvRaw :: !n
         , _cvEval :: !n }
  deriving (Eq,Functor,Foldable,Traversable,Generic,Show)

instance NFData n => NFData (ConstVal n)

constValProperties :: ToJSON n => JsonProperties (ConstVal n)
constValProperties (CVRaw n) = [ "raw" .= n ]
constValProperties (CVEval n m) = [ "raw" .= n, "eval" .= m ]
{-# INLINE constValProperties #-}

instance ToJSON n => ToJSON (ConstVal n) where
  toJSON = enableToJSON "Pact.Types.Term.ConstVal n" . object . constValProperties
  toEncoding = A.pairs . mconcat . constValProperties
  {-# INLINE toJSON #-}
  {-# INLINE toEncoding #-}

instance FromJSON n => FromJSON (ConstVal n) where
  parseJSON v =
    withObject "CVEval"
     (\o -> CVEval <$> o .: "raw" <*> o .: "eval") v <|>
    withObject "CVRaw"
     (\o -> CVRaw <$> o .: "raw") v

-- | A term from a 'ConstVal', preferring evaluated terms when available.
constTerm :: ConstVal a -> a
constTerm (CVRaw raw) = raw
constTerm (CVEval _raw eval) = eval

instance (SizeOf n) => SizeOf (ConstVal n)

-- -------------------------------------------------------------------------- --
-- Example

data Example
  = ExecExample !Text
  -- ^ An example shown as a good execution
  | ExecErrExample !Text
  -- ^ An example shown as a failing execution
  | LitExample !Text
  -- ^ An example shown as a literal
  deriving (Eq, Show, Generic)

instance Pretty Example where
  pretty = \case
    ExecExample    str -> annotate Example    $ "> " <> pretty str
    ExecErrExample str -> annotate BadExample $ "> " <> pretty str
    LitExample     str -> annotate Example    $ pretty str

instance IsString Example where
  fromString = ExecExample . fromString

instance NFData Example

instance SizeOf Example

-- -------------------------------------------------------------------------- --
-- FieldKey

-- | Label type for objects.
newtype FieldKey = FieldKey Text
  deriving (Eq,Ord,IsString,AsString,ToJSON,FromJSON,Show,NFData,Generic,ToJSONKey,SizeOf)
instance Pretty FieldKey where
  pretty (FieldKey k) = dquotes $ pretty k

instance Arbitrary FieldKey where
  arbitrary = resize 50 (FieldKey <$> genBareText)

-- -------------------------------------------------------------------------- --
-- Step

data Step n = Step
  { _sEntity :: !(Maybe n)
  , _sExec :: !n
  , _sRollback :: !(Maybe n)
  , _sInfo :: !Info
  } deriving (Eq,Show,Generic,Functor,Foldable,Traversable)
instance NFData n => NFData (Step n)

stepProperties :: ToJSON n => JsonProperties (Step n)
stepProperties o =
  [ "exec" .= _sExec o
  , "rollback" .= _sRollback o
  , "entity" .= _sEntity o
  , "info" .= _sInfo o
  ]
{-# INLINE stepProperties #-}

instance ToJSON n => ToJSON (Step n) where
  toJSON = enableToJSON "Pact.Types.Term.Step n" . lensyToJSON 2
  toEncoding = A.pairs . mconcat . stepProperties
  {-# INLINE toJSON #-}
  {-# INLINE toEncoding #-}

instance FromJSON n => FromJSON (Step n) where parseJSON = lensyParseJSON 2
instance HasInfo (Step n) where getInfo = _sInfo
instance Pretty n => Pretty (Step n) where
  pretty = \case
    Step mEntity exec Nothing _i -> parensSep $
      [ "step"
      ] ++ maybe [] (\entity -> [pretty entity]) mEntity ++
      [ pretty exec
      ]
    Step mEntity exec (Just rollback) _i -> parensSep $
      [ "step-with-rollback"
      ] ++ maybe [] (\entity -> [pretty entity]) mEntity ++
      [ pretty exec
      , pretty rollback
      ]

instance (SizeOf n) => SizeOf (Step n)

-- -------------------------------------------------------------------------- --
-- ModRef

-- | A reference to a module or interface.
data ModRef = ModRef
    { _modRefName :: !ModuleName
      -- ^ Fully-qualified module name.
    , _modRefSpec :: !(Maybe [ModuleName])
      -- ^ Specification: for modules, 'Just' implemented interfaces;
      -- for interfaces, 'Nothing'.
    , _modRefInfo :: !Info
    } deriving (Eq,Show,Generic)
instance NFData ModRef
instance HasInfo ModRef where getInfo = _modRefInfo
instance Pretty ModRef where
  pretty (ModRef mn _sm _i) = pretty mn

-- Note that this encoding is not used in 'PactValue'.
--
modRefProperties :: JsonProperties ModRef
modRefProperties o =
  [ "refSpec" .= _modRefSpec o
  , "refInfo" .= _modRefInfo o
  , "refName" .= _modRefName o
  ]
{-# INLINE modRefProperties #-}

instance ToJSON ModRef where
  toJSON = enableToJSON "Pact.Types.Term.ModRef" . lensyToJSON 4
  toEncoding = A.pairs . mconcat . modRefProperties
  {-# INLINE toJSON #-}
  {-# INLINE toEncoding #-}

instance FromJSON ModRef where parseJSON = lensyParseJSON 4
instance Ord ModRef where
  (ModRef a b _) `compare` (ModRef c d _) = (a,b) `compare` (c,d)
instance Arbitrary ModRef where
  arbitrary = ModRef <$> arbitrary <*> arbitrary <*> pure def
instance SizeOf ModRef where
  sizeOf (ModRef n s _) = constructorCost 1 + sizeOf n + sizeOf s

-- -------------------------------------------------------------------------- --
-- ModuleGuard

data ModuleGuard = ModuleGuard
  { _mgModuleName :: !ModuleName
  , _mgName :: !Text
  } deriving (Eq,Generic,Show,Ord)

instance Arbitrary ModuleGuard where
  arbitrary = ModuleGuard <$> arbitrary <*> genBareText

instance NFData ModuleGuard

instance Pretty ModuleGuard where
  pretty ModuleGuard{..} = "ModuleGuard" <+> commaBraces
    [ "module: " <> pretty _mgModuleName
    , "name: " <> pretty _mgName
    ]

instance SizeOf ModuleGuard where
  sizeOf (ModuleGuard md n) =
    (constructorCost 2) + (sizeOf md) + (sizeOf n)

moduleGuardProperties :: JsonProperties ModuleGuard
moduleGuardProperties o =
  [ "moduleName" .= _mgModuleName o
  , "name" .= _mgName o
  ]
{-# INLINE moduleGuardProperties #-}

instance ToJSON ModuleGuard where
  toJSON = enableToJSON "Pact.Types.Term.ModuleGuard" . lensyToJSON 3
  toEncoding = A.pairs . mconcat . moduleGuardProperties
  {-# INLINE toJSON #-}
  {-# INLINE toEncoding #-}

instance FromJSON ModuleGuard where parseJSON = lensyParseJSON 3

-- -------------------------------------------------------------------------- --
-- PactGuard

data PactGuard = PactGuard
  { _pgPactId :: !PactId
  , _pgName :: !Text
  } deriving (Eq,Generic,Show,Ord)

instance Arbitrary PactGuard where
  arbitrary = PactGuard <$> arbitrary <*> genBareText

instance NFData PactGuard

instance Pretty PactGuard where
  pretty PactGuard{..} = "PactGuard" <+> commaBraces
    [ "pactId: " <> pretty _pgPactId
    , "name: "   <> pretty _pgName
    ]

instance SizeOf PactGuard where
  sizeOf (PactGuard pid pn) =
    (constructorCost 2) + (sizeOf pid) + (sizeOf pn)

pactGuardProperties :: JsonProperties PactGuard
pactGuardProperties o =
  [ "pactId" .= _pgPactId o
  , "name" .= _pgName o
  ]
{-# INLINE pactGuardProperties #-}

instance ToJSON PactGuard where
  toJSON = enableToJSON "Pact.Types.Term.PactGuard" . lensyToJSON 3
  toEncoding = A.pairs . mconcat . pactGuardProperties
  {-# INLINE toJSON #-}
  {-# INLINE toEncoding #-}

instance FromJSON PactGuard where parseJSON = lensyParseJSON 3

-- -------------------------------------------------------------------------- --
-- ObjectMap

-- | Simple dictionary for object values.
newtype ObjectMap v = ObjectMap { _objectMap :: (M.Map FieldKey v) }
  deriving (Eq,Ord,Show,Functor,Foldable,Traversable,Generic,SizeOf)

instance NFData v => NFData (ObjectMap v)

-- potentially long output due to constrained recursion of PactValue
instance (Eq v, FromJSON v, ToJSON v, Arbitrary v) => Arbitrary (ObjectMap v) where
  arbitrary = ObjectMap <$> M.fromList <$> listOf1 arbitrary

-- | O(n) conversion to list. Adapted from 'M.toAscList'
objectMapToListWith :: (FieldKey -> v -> r) -> ObjectMap v -> [r]
objectMapToListWith f (ObjectMap m) = M.foldrWithKey (\k x xs -> (f k x):xs) [] m

instance Pretty v => Pretty (ObjectMap v) where
  pretty om = annotate Val $ commaBraces $
    objectMapToListWith (\k v -> pretty k <> ": " <> pretty v) om

instance ToJSON v => ToJSON (ObjectMap v) where
  toJSON (ObjectMap om) = toJSON $ M.mapKeys asString om
  toEncoding (ObjectMap om) = toEncoding $ M.mapKeys asString om
  {-# INLINE toJSON #-}
  {-# INLINE toEncoding #-}

instance FromJSON v => FromJSON (ObjectMap v)
  where parseJSON v = flip (withObject "ObjectMap") v $ \_ ->
          ObjectMap . M.mapKeys FieldKey <$> parseJSON v

instance Default (ObjectMap v) where
  def = ObjectMap M.empty

-- -------------------------------------------------------------------------- --
-- Use

data Use = Use
  { _uModuleName :: !ModuleName
  , _uModuleHash :: !(Maybe ModuleHash)
  , _uImports :: !(Maybe (Vector Text))
  , _uInfo :: !Info
  } deriving (Show, Eq, Generic)

instance HasInfo Use where getInfo = _uInfo

instance Pretty Use where
  pretty Use{..} =
    let args = pretty _uModuleName : maybe [] (\mh -> [pretty mh]) _uModuleHash
    in parensSep $ "use" : args

useProperties :: JsonProperties Use
useProperties o =
  [ "hash" .= _uModuleHash o
  , "imports" .= _uImports o
  , "module" .= _uModuleName o
  ,  "i" .= _uInfo o
  ]

instance ToJSON Use where
  toJSON = enableToJSON "Pact.Types.Term.Use" . object . useProperties
  toEncoding = A.pairs . mconcat . useProperties
  {-# INLINE toJSON #-}
  {-# INLINE toEncoding #-}

instance FromJSON Use where
  parseJSON = withObject "Use" $ \o ->
    Use <$> o .: "module"
        <*> o .:? "hash"
        <*> o .:? "imports"
        <*> o .: "i"

instance NFData Use
instance SizeOf Use

-- -------------------------------------------------------------------------- --
-- Guard

data Guard a
  = GPact !PactGuard
  | GKeySet !KeySet
  | GKeySetRef !KeySetName
  | GModule !ModuleGuard
  | GUser !(UserGuard a)
  deriving (Eq,Show,Generic,Functor,Foldable,Traversable,Ord)

-- potentially long output due to constrained recursion of PactValue
instance (Arbitrary a) => Arbitrary (Guard a) where
  arbitrary = oneof
    [ GPact <$> arbitrary
    , GKeySet <$> arbitrary
    , GKeySetRef <$> arbitrary
    , GModule <$> arbitrary
    , GUser <$> arbitrary ]

instance NFData a => NFData (Guard a)

instance Pretty a => Pretty (Guard a) where
  pretty (GPact g)      = pretty g
  pretty (GKeySet g)    = pretty g
  pretty (GKeySetRef g) = pretty g
  pretty (GUser g)      = pretty g
  pretty (GModule g)    = pretty g

instance (SizeOf p) => SizeOf (Guard p) where
  sizeOf (GPact pg) = (constructorCost 1) + (sizeOf pg)
  sizeOf (GKeySet ks) = (constructorCost 1) + (sizeOf ks)
  sizeOf (GKeySetRef ksr) = (constructorCost 1) + (sizeOf ksr)
  sizeOf (GModule mg) = (constructorCost 1) + (sizeOf mg)
  sizeOf (GUser ug) = (constructorCost 1) + (sizeOf ug)

keyNamef :: Text
keyNamef = "keysetref"

instance ToJSON a => ToJSON (Guard a) where
  toJSON (GKeySet k) = enableToJSON "Pact.Types.Term.Guard a" $ toJSON k
  toJSON (GKeySetRef n) = enableToJSON "Pact.Types.Term.Guard a" $ object [ keyNamef .= n ]
  toJSON (GPact g) = enableToJSON "Pact.Types.Term.Guard a" $ toJSON g
  toJSON (GModule g) = enableToJSON "Pact.Types.Term.Guard a" $ toJSON g
  toJSON (GUser g) = enableToJSON "Pact.Types.Term.Guard a" $ toJSON g

  toEncoding (GKeySet k) = toEncoding k
  toEncoding (GKeySetRef n) = A.pairs (keyNamef .= n)
  toEncoding (GPact g) = toEncoding g
  toEncoding (GModule g) = toEncoding g
  toEncoding (GUser g) = toEncoding g

  {-# INLINE toJSON #-}
  {-# INLINE toEncoding #-}

instance FromJSON a => FromJSON (Guard a) where
  parseJSON v =
      (GKeySet <$> parseJSON v) <|>
      (withObject "KeySetRef" $ \o ->
          GKeySetRef . KeySetName <$> o .: keyNamef) v <|>
      (GPact <$> parseJSON v) <|>
      (GModule <$> parseJSON v) <|>
      (GUser <$> parseJSON v)
  {-# INLINE parseJSON #-}

-- -------------------------------------------------------------------------- --
-- Module

data Module g = Module
  { _mName :: !ModuleName
  , _mGovernance :: !(Governance g)
  , _mMeta :: !Meta
  , _mCode :: !Code
  , _mHash :: !ModuleHash
  , _mBlessed :: !(HS.HashSet ModuleHash)
  , _mInterfaces :: ![ModuleName]
  , _mImports :: ![Use]
  } deriving (Eq,Functor,Foldable,Traversable,Show,Generic)

instance NFData g => NFData (Module g)

instance Pretty g => Pretty (Module g) where
  pretty Module{..} = parensSep
    [ "module" , pretty _mName , pretty _mGovernance , pretty _mHash ]

moduleProperties :: ToJSON g => JsonProperties (Module g)
moduleProperties o =
  [ "hash" .= _mHash o
  , "blessed" .= _mBlessed o
  , "interfaces" .= _mInterfaces o
  , "imports" .= _mImports o
  , "name" .= _mName o
  , "code" .= _mCode o
  , "meta" .= _mMeta o
  , "governance" .= _mGovernance o
  ]
{-# INLINE moduleProperties #-}

instance ToJSON g => ToJSON (Module g) where
  toJSON = enableToJSON "Pact.Types.Term.Module g" . lensyToJSON 2
  toEncoding = A.pairs . mconcat . moduleProperties
  {-# INLINE toJSON #-}
  {-# INLINE toEncoding #-}

instance FromJSON g => FromJSON (Module g) where parseJSON = lensyParseJSON 2

instance (SizeOf g) => SizeOf (Module g)

-- -------------------------------------------------------------------------- --
-- Interface

data Interface = Interface
  { _interfaceName :: !ModuleName
  , _interfaceCode :: !Code
  , _interfaceMeta :: !Meta
  , _interfaceImports :: ![Use]
  } deriving (Eq,Show,Generic)
instance Pretty Interface where
  pretty Interface{..} = parensSep [ "interface", pretty _interfaceName ]

interfaceProperties :: JsonProperties Interface
interfaceProperties o =
  [ "imports" .= _interfaceImports o
  , "name" .= _interfaceName o
  , "code" .= _interfaceCode o
  , "meta" .= _interfaceMeta o
  ]
{-# INLINE interfaceProperties #-}

instance ToJSON Interface where
  toJSON = enableToJSON "Pact.Types.Term.Interface" . lensyToJSON 10
  toEncoding = A.pairs . mconcat . interfaceProperties
  {-# INLINE toJSON #-}
  {-# INLINE toEncoding #-}

instance FromJSON Interface where parseJSON = lensyParseJSON 10

instance NFData Interface

instance SizeOf Interface

-- -------------------------------------------------------------------------- --
-- Namespace

data Namespace a = Namespace
  { _nsName :: !NamespaceName
  , _nsUser :: !(Guard a)
  , _nsAdmin :: !(Guard a)
  } deriving (Eq, Show, Generic)

instance (Arbitrary a) => Arbitrary (Namespace a) where
  arbitrary = Namespace <$> arbitrary <*> arbitrary <*> arbitrary

instance Pretty (Namespace a) where
  pretty Namespace{..} = "(namespace " <> prettyString (asString' _nsName) <> ")"

instance (SizeOf n) => SizeOf (Namespace n) where
  sizeOf (Namespace name ug ag) =
    (constructorCost 3) + (sizeOf name) + (sizeOf ug) + (sizeOf ag)

namespaceProperties :: ToJSON a => JsonProperties (Namespace a)
namespaceProperties o =
  [ "admin" .= _nsAdmin o
  , "user" .= _nsUser o
  , "name" .= _nsName o
  ]
{-# INLINE namespaceProperties #-}

instance ToJSON a => ToJSON (Namespace a) where
  toJSON = enableToJSON "Pact.Types.Term.Namespace a" . lensyToJSON 3
  toEncoding = A.pairs . mconcat . namespaceProperties
  {-# INLINE toJSON #-}
  {-# INLINE toEncoding #-}

instance FromJSON a => FromJSON (Namespace a) where parseJSON = lensyParseJSON 3

instance (NFData a) => NFData (Namespace a)

-- -------------------------------------------------------------------------- --
-- ModuleDef

data ModuleDef g
  = MDModule !(Module g)
  | MDInterface !Interface
 deriving (Eq,Functor,Foldable,Traversable,Show,Generic)

instance NFData g => NFData (ModuleDef g)

instance Pretty g => Pretty (ModuleDef g) where
  pretty = \case
    MDModule    m -> pretty m
    MDInterface i -> pretty i

instance ToJSON g => ToJSON (ModuleDef g) where
  toJSON (MDModule m) = enableToJSON "Pact.Types.Term.ModuleDef g" $ toJSON m
  toJSON (MDInterface i) = enableToJSON "Pact.Types.Term.ModuleDef g" $ toJSON i

  toEncoding (MDModule m) = toEncoding m
  toEncoding (MDInterface i) = toEncoding i

  {-# INLINE toJSON #-}
  {-# INLINE toEncoding #-}

instance FromJSON g => FromJSON (ModuleDef g) where
  parseJSON v = MDModule <$> parseJSON v <|> MDInterface <$> parseJSON v

instance (SizeOf g) => SizeOf (ModuleDef g)

moduleDefName :: ModuleDef g -> ModuleName
moduleDefName (MDModule m) = _mName m
moduleDefName (MDInterface m) = _interfaceName m

moduleDefCode :: ModuleDef g -> Code
moduleDefCode (MDModule m) = _mCode m
moduleDefCode (MDInterface m) = _interfaceCode m

moduleDefMeta :: ModuleDef g -> Meta
moduleDefMeta (MDModule m) = _mMeta m
moduleDefMeta (MDInterface m) = _interfaceMeta m


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
    { _faInfo :: !Info
    , _faName :: !Text
    , _faModule :: !(Maybe ModuleName)
    , _faDefType :: !DefType
    , _faTypes :: !(FunTypes (Term Name))
    , _faDocs :: !(Maybe Text)
    } deriving (Generic)

deriving instance (Show1 Term) => Show FunApp
deriving instance (Eq1 Term) => Eq FunApp
instance NFData FunApp

funAppProperties :: JsonProperties FunApp
funAppProperties o =
  [ "defType" .= _faDefType o
  , "types" .= _faTypes o
  , "name" .= _faName o
  , "module" .= _faModule o
  , "docs" .= _faDocs o
  , "info" .= _faInfo o
  ]
{-# INLINE funAppProperties #-}

instance ToJSON FunApp where
  toJSON = enableToJSON "Pact.Types.Term.FunApp" . lensyToJSON 3
  toEncoding = A.pairs . mconcat . funAppProperties
  {-# INLINE toJSON #-}
  {-# INLINE toEncoding #-}

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

defProperties :: ToJSON n => JsonProperties (Def n)
defProperties o =
  [ "defType" .= _dDefType o
  , "defMeta" .= _dDefMeta o
  , "funType" .= _dFunType o
  , "defName" .= _dDefName o
  , "defBody" .= _dDefBody o
  , "module" .= _dModule o
  , "meta" .= _dMeta o
  , "info" .= _dInfo o
  ]
{-# INLINE defProperties #-}

instance ToJSON n => ToJSON (Def n) where
  toJSON = enableToJSON "Pact.Types.Term.Def n" . lensyToJSON 2
  toEncoding = A.pairs . mconcat . defProperties
  {-# INLINE toJSON #-}
  {-# INLINE toEncoding #-}

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
    pretty arg <> ":" <> pretty (_ftReturn ty) <+> "lambda" <> (parensSep $ pretty <$> _ftArgs ty) <+> "..."

instance NFData n => NFData (Lam n)

-- FIXME Is the prefix correct?
--
lamProperties :: ToJSON n => JsonProperties (Lam n)
lamProperties o =
  [ "amArg" .= _lamArg o
  , "amInfo" .= _lamInfo o
  , "amBindBody" .= _lamBindBody o
  , "amTy" .= _lamTy o
  ]
{-# INLINE lamProperties #-}

instance ToJSON n => ToJSON (Lam n) where
  toJSON = enableToJSON "Pact.Types.Term.Lam n" . lensyToJSON 2
  toEncoding = A.pairs . mconcat . lamProperties
  {-# INLINE toJSON #-}
  {-# INLINE toEncoding #-}

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

objectProperties :: ToJSON n => JsonMProperties (Object n)
objectProperties o = mconcat
  [ "obj" .= _oObject o
  , "keyorder" .?= _oKeyOrder o
  , "type" .= _oObjectType o
  , "i" .= _oInfo o
  ]
{-# INLINE objectProperties #-}

instance ToJSON n => ToJSON (Object n) where
  toJSON = enableToJSON "Pact.Types.Term.Object n" . A.Object . objectProperties
  toEncoding = A.pairs . objectProperties
  {-# INLINE toJSON #-}
  {-# INLINE toEncoding #-}

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
      _tDef :: Def n
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
  sizeOf1 = \case
    TModule defn body info ->
      constructorCost 3 + sizeOf defn + sizeOf body + sizeOf info
    TList li typ info ->
       constructorCost 3 + sizeOf li + sizeOf typ + sizeOf info
    TDef defn info ->
      constructorCost 2 + sizeOf defn + sizeOf info
    -- note: we actually strip docs and examples
    -- post fork
    TNative name _defun ftyps examples docs tlo info ->
      constructorCost 7 + sizeOf name + sizeOf ftyps + sizeOf examples +
        sizeOf docs + sizeOf tlo + sizeOf info
    TConst arg mname cval meta info  ->
      constructorCost 5 + sizeOf arg + sizeOf mname + sizeOf cval + sizeOf meta + sizeOf info
    TApp app info ->
      constructorCost 2 + sizeOf app + sizeOf info
    TVar v info ->
      constructorCost 2 + sizeOf v + sizeOf info
    TBinding bps body btyp info ->
      constructorCost 4 + sizeOf bps + sizeOf body + sizeOf btyp + sizeOf info
    TLam lam info ->
      constructorCost 2 + sizeOf lam + sizeOf info
    TObject obj info ->
      constructorCost 2 + sizeOf obj + sizeOf info
    TSchema tn mn meta args info ->
      constructorCost 5 + sizeOf tn + sizeOf mn + sizeOf meta + sizeOf args + sizeOf info
    TLiteral lit info ->
      constructorCost 2 + sizeOf lit + sizeOf info
    TGuard g info ->
      constructorCost 2 + sizeOf g + sizeOf info
    TUse u info->
      constructorCost 2 + sizeOf u + sizeOf info
    TStep step meta info ->
      constructorCost 3 + sizeOf step + sizeOf meta + sizeOf info
    TModRef mr info ->
      constructorCost 2 + sizeOf mr + sizeOf info
    TTable tn mn hs typ meta info ->
      constructorCost 6 + sizeOf tn + sizeOf mn + sizeOf hs + sizeOf typ
        + sizeOf meta + sizeOf info
    TDynamic e1 e2 info ->
      constructorCost 3 + sizeOf e1 + sizeOf e2 + sizeOf info

instance (SizeOf a) => SizeOf (Term a) where
  sizeOf t = sizeOf1 t

instance Applicative Term where
    pure = return
    (<*>) = ap

instance Monad Term where
    return a = TVar a def
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
  = TermModuleDef
  | TermBody
  | TermMeta
  | TermList
  | TermType
  | TermNatName
  | TermNatFun
  | TermNatFunTypes
  | TermNatExamples
  | TermNatDocs
  | TermNatTopLevel
  | TermConstArg
  | TermModName
  | TermConstVal
  | TermVar
  | TermPairs
  | TermLiteral
  | TermGuard
  | TermSchemaName
  | TermFields
  | TermTblName
  | TermHash
  | TermDynRef
  | TermDynMem

prop :: IsString a => TermProperties -> a
prop TermModuleDef = "module"
prop TermBody = "body"
prop TermMeta = "meta"
prop TermList = "list"
prop TermType = "type"
prop TermNatName = "name"
prop TermNatFun = "fun"
prop TermNatFunTypes = "types"
prop TermNatExamples = "examples"
prop TermNatDocs = "docs"
prop TermNatTopLevel = "tl"
prop TermConstArg = "arg"
prop TermModName = "modname"
prop TermConstVal = "val"
prop TermVar = "var"
prop TermPairs = "pairs"
prop TermLiteral = "lit"
prop TermGuard = "guard"
prop TermSchemaName = "name"
prop TermFields = "fields"
prop TermTblName = "name"
prop TermHash = "hash"
prop TermDynRef = "dref"
prop TermDynMem = "dmem"
{-# INLINE prop #-}

instance ToJSON n => ToJSON (Term n) where
    toJSON = termEnc object toJSON
    toEncoding = termEnc (A.pairs . mconcat) toEncoding

termEnc
  :: KeyValue kv
  => ToJSON n
  => ([kv] -> e)
  -> (forall a . ToJSON a => a -> e)
  -> Term n
  -> e
termEnc kv val = \case
  (TModule d b i) -> kv
    [ p TermModuleDef .= d
    , p TermBody .= b, inf i
    ]
  (TList ts ty i) -> kv
    [ p TermList .= ts
    , p TermType .= ty, inf i
    ]
  (TDef d _i) -> val d
  -- TNative intentionally not marshallable
  (TNative n _fn tys _exs d tl i) -> kv
    [ p TermNatName .= n
    , p TermNatFun .= Null {- TODO fn -}
    , p TermNatFunTypes .= tys
    , p TermNatExamples .= Null {- TODO exs -}
    , p TermNatDocs .= d
    , p TermNatTopLevel .= tl
    , inf i
    ]
  (TConst d m c met i) -> kv
    [ p TermConstArg .= d
    , p TermModName .= m
    , p TermConstVal .= c
    , p TermMeta .= met, inf i
    ]
  (TApp a _i) -> val a
  (TVar n i) -> kv
    [ p TermVar .= n
    , inf i
    ]
  (TBinding bs b c i) -> kv
    [ p TermPairs .= bs
    , p TermBody .= b
    , p TermType .= c
    , inf i
    ]
  (TObject o _i) -> val o
  (TLiteral l i) -> kv
    [ p TermLiteral .= l
    , inf i
    ]
  (TLam tlam _i) -> val tlam
  (TGuard k i) -> kv
    [ p TermGuard .= k
    , inf i
    ]
  (TUse u _i) -> val u
  (TStep s tmeta i) -> kv
    [ p TermBody .= s
    , p TermMeta .= tmeta
    , inf i
    ]
  (TSchema sn smod smeta sfs i) -> kv
    [ p TermSchemaName .= sn
    , p TermModName .= smod
    , p TermMeta .= smeta
    , p TermFields .= sfs
    , inf i
    ]
  (TTable tn tmod th tty tmeta i) -> kv
    [ p TermTblName .= tn
    , p TermModName .= tmod
    , p TermHash .= th
    , p TermType .= tty
    , p TermMeta .= tmeta
    , inf i
    ]
  (TDynamic r m i) -> kv
   [ p TermDynRef .= r
   , p TermDynMem .= m
   , inf i
   ]
  (TModRef mr _i) -> val mr
 where
  p = prop
  inf i = "i" .= i

instance FromJSON n => FromJSON (Term n) where
  parseJSON v
    = wo "Module"
      (\o -> TModule
        <$> o .: p TermModuleDef
        <*> o .: p TermBody
        <*> inf o
      )
    <|> wo "List"
      (\o -> TList
        <$> o .: p TermList
        <*> o .: p TermType
        <*> inf o
      )
    <|> parseWithInfo TDef
    -- TNative intentionally not marshallable
    <|> wo "Const"
      (\o -> TConst
        <$> o .: p TermConstArg
        <*> o .: p TermModName
        <*> o .: p TermConstVal
        <*> o .: p TermMeta
        <*> inf o
      )
    <|> parseWithInfo TApp
    <|> wo "Var"
      (\o -> TVar
        <$>  o .: p TermVar
        <*> inf o
      )
    <|> wo "Binding"
        (\o -> TBinding
          <$> o .: p TermPairs
          <*> o .: p TermBody
          <*> o .: p TermType
          <*> inf o
        )
    <|> parseWithInfo TObject
    <|> wo "Literal"
      (\o -> TLiteral
        <$> o .: p TermLiteral
        <*> inf o
      )
    <|> wo "Guard"
      (\o -> TGuard
        <$> o .: p TermGuard
        <*> inf o
      )
    <|> parseWithInfo TUse
    <|> parseWithInfo TLam
    <|> wo "Step"
        (\o -> TStep
          <$> o .: p TermBody
          <*> o .: p TermMeta
          <*> inf o
        )
   --  parseWithInfo TStep
    <|> wo "Schema"
        (\o -> TSchema
          <$>  o .: p TermSchemaName
          <*> o .: p TermModName
          <*> o .: p TermMeta
          <*> o .: p TermFields
          <*> inf o
        )
    <|> wo "Table"
        (\o -> TTable
          <$>  o .: p TermTblName
          <*> o .: p TermModName
          <*> o .: p TermHash
          <*> o .: p TermType
          <*> o .: p TermMeta
          <*> inf o
        )
    <|> wo "Dynamic"
        (\o -> TDynamic
          <$> o .: p TermDynRef
          <*> o .: p TermDynMem
          <*> inf o
        )
    <|> parseWithInfo TModRef
   where
    p = prop
    inf o = o .: "i"
    wo n f = withObject n f v

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
termEq a b = termEq1 (==) a b

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
          go ((k1,v1):r1) ((k2,v2):r2) _ = go r1 r2 $ k1 == k2 && (termEq1 eq v1 v2)
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

makeLenses ''Namespace
makeLenses ''Meta
makeLenses ''Module
makeLenses ''Interface
makePrisms ''ModuleDef
makeLenses ''App
makePrisms ''DefType
makeLenses ''BindPair
makeLenses ''Step
makeLenses ''ModuleHash
makeLenses ''ModRef
makePrisms ''Guard

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

instance Eq1 Guard where
  liftEq = $(makeLiftEq ''Guard)
instance Eq1 UserGuard where
  liftEq = $(makeLiftEq ''UserGuard)
instance Eq1 BindPair where
  liftEq = $(makeLiftEq ''BindPair)
instance Eq1 App where
  liftEq = $(makeLiftEq ''App)
instance Eq1 BindType where
  liftEq = $(makeLiftEq ''BindType)
instance Eq1 ConstVal where
  liftEq = $(makeLiftEq ''ConstVal)
instance Eq1 DefcapMeta where
  liftEq = $(makeLiftEq ''DefcapMeta)
instance Eq1 DefMeta where
  liftEq = $(makeLiftEq ''DefMeta)
instance Eq1 ModuleDef where
  liftEq = $(makeLiftEq ''ModuleDef)
instance Eq1 Module where
  liftEq = $(makeLiftEq ''Module)
instance Eq1 Governance where
  liftEq = $(makeLiftEq ''Governance)
instance Eq1 ObjectMap where
  liftEq = $(makeLiftEq ''ObjectMap)
instance Eq1 Step where
  liftEq = $(makeLiftEq ''Step)

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

instance Show1 Guard where
  liftShowsPrec = $(makeLiftShowsPrec ''Guard)
instance Show1 UserGuard where
  liftShowsPrec = $(makeLiftShowsPrec ''UserGuard)
instance Show1 BindPair where
  liftShowsPrec = $(makeLiftShowsPrec ''BindPair)
instance Show1 App where
  liftShowsPrec = $(makeLiftShowsPrec ''App)
instance Show1 ObjectMap where
  liftShowsPrec = $(makeLiftShowsPrec ''ObjectMap)
instance Show1 BindType where
  liftShowsPrec = $(makeLiftShowsPrec ''BindType)
instance Show1 ConstVal where
  liftShowsPrec = $(makeLiftShowsPrec ''ConstVal)
instance Show1 DefcapMeta where
  liftShowsPrec = $(makeLiftShowsPrec ''DefcapMeta)
instance Show1 DefMeta where
  liftShowsPrec = $(makeLiftShowsPrec ''DefMeta)
instance Show1 ModuleDef where
  liftShowsPrec = $(makeLiftShowsPrec ''ModuleDef)
instance Show1 Module where
  liftShowsPrec = $(makeLiftShowsPrec ''Module)
instance Show1 Governance where
  liftShowsPrec = $(makeLiftShowsPrec ''Governance)
instance Show1 Step where
  liftShowsPrec = $(makeLiftShowsPrec ''Step)

instance Show1 Def where
  liftShowsPrec = $(makeLiftShowsPrec ''Def)
instance Show1 Lam where
  liftShowsPrec = $(makeLiftShowsPrec ''Lam)
instance Show1 Object where
  liftShowsPrec = $(makeLiftShowsPrec ''Object)
instance Show1 Term where
  liftShowsPrec = $(makeLiftShowsPrec ''Term)
