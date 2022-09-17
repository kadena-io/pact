{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module: Pact.Types.Term.Internal
-- Copyright: Copyright © 2022 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Pact.Types.Term.Internal
( Meta(..)
, mDocs
, mModel
, PublicKey(..)
, KeySet(..)
, mkKeySet
, KeySetName(..)
, PactGuard(..)
, PactId(..)
, UserGuard(..)
, ModuleGuard(..)
, Guard(..)
, _GPact
, _GKeySet
, _GKeySetRef
, _GModule
, _GUser
, DefType(..)
, _Defun
, _Defpact
, _Defcap
, defTypeRep
, BindType(..)
, BindPair(..)
, bpArg
, bpVal
, toBindPairs
, Module(..)
, mName
, mGovernance
, mMeta
, mCode
, mHash
, mBlessed
, mInterfaces
, mImports
, Interface(..)
, interfaceCode
, interfaceMeta
, interfaceName
, interfaceImports
, ModuleDef(..)
, _MDModule
, _MDInterface
, moduleDefName
, moduleDefCode
, moduleDefMeta
, Governance(..)
, ModuleHash(..)
, mhHash
, ConstVal(..)
, constTerm
, Use(..)
, App(..)
, appFun
, appArgs
, appInfo
, DefMeta(..)
, DefcapMeta(..)
, Example(..)
, ObjectMap(..)
, objectMapToListWith
, FieldKey(..)
, Step(..)
, sEntity
, sExec
, sRollback
, sInfo
, ModRef(..)
, modRefName
, modRefSpec
, modRefInfo
, modRefProperties_
, Gas(..)
) where

import Control.Applicative
import Control.DeepSeq
import Control.Lens hiding ((.=), DefName(..), elements)
import Control.Monad
import Data.Aeson hiding (pairs,Object, (<?>))
import qualified Data.Aeson as A
import Data.Bifunctor
import Data.Default
import Data.Eq.Deriving
import Data.Foldable
import Data.Function
import Data.Functor.Classes (Eq1(..), Show1(..))
import qualified Data.HashSet as HS
import Data.Hashable
import qualified Data.HashMap.Strict as HM
import Data.Int (Int64)
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Serialize (Serialize)
import Data.String
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics (Generic)
import Prelude
import Text.Show.Deriving

-- internal modules

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

import Pact.Utils.Json

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
    deriving (Eq,Ord,Show,Pretty,AsString,IsString,FromJSON,ToJSON, FromJSONKey, ToJSONKey, Generic,NFData,SizeOf)

-- -------------------------------------------------------------------------- --
-- UserGuard

data UserGuard a = UserGuard
  { _ugFun :: !Name
  , _ugArgs :: ![a]
  } deriving (Eq,Generic,Show,Functor,Foldable,Traversable,Ord)

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

  {-# INLINEABLE toJSON #-}
  {-# INLINEABLE toEncoding #-}

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
  {-# INLINEABLE toJSON #-}
  {-# INLINEABLE toEncoding #-}

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
  {-# INLINEABLE toJSON #-}
  {-# INLINEABLE toEncoding #-}

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
  {-# INLINEABLE toJSON #-}
  {-# INLINEABLE toEncoding #-}

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
  {-# INLINEABLE toJSON #-}
  {-# INLINEABLE toEncoding #-}

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

-- -------------------------------------------------------------------------- --
-- DefMeta

-- | Def metadata specific to 'DefType'. Currently only specified for Defcap.
--
newtype DefMeta n = DMDefcap (DefcapMeta n)
  deriving (Functor,Foldable,Traversable,Generic,Eq,Show,Ord)

instance NFData n => NFData (DefMeta n)

instance Pretty n => Pretty (DefMeta n) where
  pretty (DMDefcap m) = pretty m

instance ToJSON n => ToJSON (DefMeta n) where
  toJSON (DMDefcap m) = toJSON m
  toEncoding (DMDefcap m) = toEncoding m
  {-# INLINEABLE toJSON #-}
  {-# INLINEABLE toEncoding #-}

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
  {-# INLINEABLE toJSON #-}
  {-# INLINEABLE toEncoding #-}

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
  {-# INLINEABLE toJSON #-}
  {-# INLINEABLE toEncoding #-}

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
  {-# INLINEABLE toJSON #-}
  {-# INLINEABLE toEncoding #-}

instance FromJSON ModRef where parseJSON = lensyParseJSON 4
instance Ord ModRef where
  (ModRef a b _) `compare` (ModRef c d _) = (a,b) `compare` (c,d)
instance SizeOf ModRef where
  sizeOf (ModRef n s _) = constructorCost 1 + sizeOf n + sizeOf s

-- | This JSON encoding omits the @refInfo@ property when it is the default Value.
--
-- This is different from the encoding of the 'ToJSON' instance of ModRef which
-- always includes the @refInfo@ property.
--
-- The alternative encoding is used by the 'ToJSON' instances of 'PactValue' and
-- 'OldPactValue'.
--
modRefProperties_ :: JsonMProperties ModRef
modRefProperties_ o = mconcat
    [ "refSpec" .= _modRefSpec o
    , "refInfo" .?= if refInfo /= def then Just refInfo else Nothing
      -- this property is different from the instance Pact.Types.Term.ModRef
    , "refName" .= _modRefName o
    ]
 where
  refInfo = _modRefInfo o
{-# INLINEABLE modRefProperties_ #-}

-- -------------------------------------------------------------------------- --
-- ModuleGuard

data ModuleGuard = ModuleGuard
  { _mgModuleName :: !ModuleName
  , _mgName :: !Text
  } deriving (Eq,Generic,Show,Ord)

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
{-# INLINEABLE moduleGuardProperties #-}

instance ToJSON ModuleGuard where
  toJSON = enableToJSON "Pact.Types.Term.ModuleGuard" . lensyToJSON 3
  toEncoding = A.pairs . mconcat . moduleGuardProperties
  {-# INLINEABLE toJSON #-}
  {-# INLINEABLE toEncoding #-}

instance FromJSON ModuleGuard where parseJSON = lensyParseJSON 3

-- -------------------------------------------------------------------------- --
-- PactGuard

data PactGuard = PactGuard
  { _pgPactId :: !PactId
  , _pgName :: !Text
  } deriving (Eq,Generic,Show,Ord)

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
  {-# INLINEABLE toJSON #-}
  {-# INLINEABLE toEncoding #-}

instance FromJSON PactGuard where parseJSON = lensyParseJSON 3

-- -------------------------------------------------------------------------- --
-- ObjectMap

-- | Simple dictionary for object values.
newtype ObjectMap v = ObjectMap { _objectMap :: M.Map FieldKey v }
  deriving (Eq,Ord,Show,Functor,Foldable,Traversable,Generic,SizeOf)

instance NFData v => NFData (ObjectMap v)

-- | O(n) conversion to list. Adapted from 'M.toAscList'
objectMapToListWith :: (FieldKey -> v -> r) -> ObjectMap v -> [r]
objectMapToListWith f (ObjectMap m) = M.foldrWithKey (\k x xs -> (f k x):xs) [] m

instance Pretty v => Pretty (ObjectMap v) where
  pretty om = annotate Val $ commaBraces $
    objectMapToListWith (\k v -> pretty k <> ": " <> pretty v) om

-- FIXME the order depends on the hash function that is used by HM.HashMap in
-- aeson.
--
instance ToJSON v => ToJSON (ObjectMap v) where
  toJSON (ObjectMap om) = toJSON $ M.mapKeys asString om
  toEncoding (ObjectMap om) = toEncoding
    $ HM.fromList
    $ first (LegacyHashed . asString)
    <$> M.toList om
  {-# INLINEABLE toJSON #-}
  {-# INLINEABLE toEncoding #-}

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
  {-# INLINEABLE toJSON #-}
  {-# INLINEABLE toEncoding #-}

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

keyNamef :: AesonKey
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

  {-# INLINEABLE toJSON #-}
  {-# INLINEABLE toEncoding #-}

instance FromJSON a => FromJSON (Guard a) where
  parseJSON v =
      (GKeySet <$> parseJSON v) <|>
      (withObject "KeySetRef" $ \o -> GKeySetRef <$> o .: keyNamef) v <|>
      (GPact <$> parseJSON v) <|>
      (GModule <$> parseJSON v) <|>
      (GUser <$> parseJSON v)
  {-# INLINEABLE parseJSON #-}

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
  {-# INLINEABLE toJSON #-}
  {-# INLINEABLE toEncoding #-}

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
  {-# INLINEABLE toJSON #-}
  {-# INLINEABLE toEncoding #-}

instance FromJSON Interface where parseJSON = lensyParseJSON 10

instance NFData Interface

instance SizeOf Interface

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

  {-# INLINEABLE toJSON #-}
  {-# INLINEABLE toEncoding #-}

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
-- Lenses

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

