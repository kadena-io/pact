{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Pact.Types.Term.Internal
( Meta(..)
, mDocs
, mModel
, PublicKeyText(..)
, KeySet(..)
, mkKeySet
, KeySetName(..)
, PactGuard(..)
, PactId(..)
, UserGuard(..)
, ModuleGuard(..)
, CapabilityGuard(..)
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
, modRefKeyValues_
, Gas(..)
, MilliGas(..)
, gasToMilliGas
, milliGasToGas
, millisPerGas
) where

import Control.Applicative
import Control.DeepSeq
import Control.Lens hiding ((.=), DefName(..), elements)
import Control.Monad
import Data.Aeson hiding (pairs,Object, (<?>))
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as A
import Data.Default
import Data.Eq.Deriving
import Data.Foldable
import Data.Function
import Data.Functor.Classes (Eq1(..), Show1(..))
import qualified Data.HashSet as HS
import Data.Hashable
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

import Pact.JSON.Legacy.Hashable
import qualified Pact.JSON.Legacy.HashMap as LHM
import Pact.JSON.Legacy.Utils

import qualified Pact.JSON.Encode as J

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

instance J.Encode Meta where
  build o = J.object
    [ "model" J..= J.array (_mModel o)
    , "docs" J..= _mDocs o
    ]
  {-# INLINE build #-}

instance FromJSON Meta where parseJSON = lensyParseJSON 2

instance Default Meta where def = Meta def def

instance Semigroup Meta where
  (Meta d m) <> (Meta d' m') = Meta (d <> d') (m <> m')

instance Monoid Meta where
  mempty = Meta Nothing []

-- -------------------------------------------------------------------------- --
-- PactId

newtype PactId = PactId Text
  deriving (Eq,Ord,Show,Generic)
  deriving newtype (Pretty,AsString,IsString,FromJSON,FromJSONKey,NFData,SizeOf,LegacyHashable,J.Encode)

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
  sizeOf ver (UserGuard n arr) =
    constructorCost 2 + sizeOf ver n + sizeOf ver arr


instance J.Encode a => J.Encode (UserGuard a) where
  build o = J.object
    [ "args" J..= J.array (_ugArgs o)
    , "fun" J..= _ugFun o
    ]
  {-# INLINE build #-}

instance FromJSON a => FromJSON (UserGuard a) where parseJSON = lensyParseJSON 3

-- -------------------------------------------------------------------------- --
-- DefType

data DefType
  = Defun
  | Defpact
  | Defcap
  deriving (Eq,Show,Generic, Bounded, Enum)

instance FromJSON DefType
instance NFData DefType

instance J.Encode DefType where
  build Defun = J.text "Defun"
  build Defpact = J.text "Defpact"
  build Defcap = J.text "Defcap"
  {-# INLINE build #-}

instance SizeOf DefType where
  sizeOf _ _ = 0

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
  deriving (Eq,Ord,Generic)
  deriving newtype (Num,Real,Integral,Enum,FromJSON,NFData)
  deriving J.Encode via (J.Aeson Int64)

instance Show Gas where show (Gas g) = show g

instance Pretty Gas where
  pretty (Gas i) = pretty i

instance Semigroup Gas where
  (Gas a) <> (Gas b) = Gas $ a + b

instance Monoid Gas where
  mempty = 0

-- | Gas compute cost unit that represents smaller units of compute time.
-- 1 Gas = 1000 MilliGas, and in terms of compute time, 1 Gas = 2500 ns,
-- thus 1 MilliGas = 2.5ns
newtype MilliGas
  = MilliGas Int64
  deriving (Eq, Ord, NFData, Generic)

instance Semigroup MilliGas where
  (MilliGas a) <> (MilliGas b) = MilliGas (a + b)

instance Monoid MilliGas where
  mempty = MilliGas 0

millisPerGas :: Int64
millisPerGas = 1000

gasToMilliGas :: Gas -> MilliGas
gasToMilliGas (Gas n) = MilliGas (n * millisPerGas)

milliGasToGas :: MilliGas -> Gas
milliGasToGas (MilliGas n) = Gas (n `div` millisPerGas)


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

instance J.Encode n => J.Encode (BindType n) where
  build BindLet = J.text "let"
  build (BindSchema s) = J.object [ "bind" J..= s ]
  {-# INLINEABLE build #-}

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

instance J.Encode n => J.Encode (BindPair n) where
  build o = J.object
    [ "arg" J..= _bpArg o
    , "val" J..= _bpVal o
    ]
  {-# INLINEABLE build #-}

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


instance J.Encode t => J.Encode (App t) where
  build o = J.object
    [ "args" J..= J.Array (_appArgs o)
    , "fun" J..= _appFun o
    , "info" J..= _appInfo o
    ]
  {-# INLINEABLE build #-}

instance FromJSON t => FromJSON (App t) where
  parseJSON = withObject "App" $ \o -> App
    <$> o .: "fun"
    <*> o .: "args"
    <*> o .:? "info" .!= Info Nothing
  {-# INLINE parseJSON #-}

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

instance J.Encode g => J.Encode (Governance g) where
  build (Governance (Left ks)) = J.object [ "keyset" J..= ks ]
  build (Governance (Right c)) = J.object [ "capability" J..= c ]
  {-# INLINEABLE build #-}

instance FromJSON g => FromJSON (Governance g) where
  parseJSON = withObject "Governance" $ \o ->
    Governance <$> (Left <$> o .: "keyset" <|>
                    Right <$> o .: "capability")

-- -------------------------------------------------------------------------- --
-- ModuleHash

-- | Newtype wrapper differentiating 'Hash'es from module hashes
--
newtype ModuleHash = ModuleHash { _mhHash :: Hash }
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (Hashable, Serialize, AsString, Pretty, FromJSON, ParseText, J.Encode)
  deriving newtype (LegacyHashable)
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

instance J.Encode n => J.Encode (DefcapMeta n) where
  build (DefcapManaged (Just (p,f))) = J.object
    [ "managedParam" J..= p
    , "managerFun" J..= f
    ]
  build (DefcapManaged Nothing) = J.object [ "managerAuto" J..= True ]
  build DefcapEvent = J.text "event"
  {-# INLINEABLE build #-}

instance FromJSON n => FromJSON (DefcapMeta n) where
  parseJSON v = parseUser v <|> parseAuto v <|> parseEvent v
    where
      parseUser = withObject "DefcapMeta" $ \o -> DefcapManaged . Just <$>
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

instance J.Encode n => J.Encode (DefMeta n) where
  build (DMDefcap m) = J.build m
  {-# INLINEABLE build #-}

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

instance J.Encode n => J.Encode (ConstVal n) where
  build (CVRaw n) = J.object [ "raw" J..= n ]
  build (CVEval n m) = J.object [ "raw" J..= n, "eval" J..= m ]
  {-# INLINEABLE build #-}

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
  deriving (Show,Eq,Ord,Generic)
  deriving newtype (IsString,AsString,FromJSON,NFData,SizeOf,LegacyHashable,J.Encode)

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

instance J.Encode n => J.Encode (Step n) where
  build o = J.object
    [ "exec" J..= _sExec o
    , "rollback" J..= _sRollback o
    , "entity" J..= _sEntity o
    , "info" J..= _sInfo o
    ]
  {-# INLINEABLE build #-}

instance FromJSON n => FromJSON (Step n) where
  parseJSON = withObject "Step" $ \o -> Step
    <$> o .: "entity"
    <*> o .: "exec"
    <*> o .: "rollback"
    <*> o .:? "info" .!= Info Nothing
  {-# INLINE parseJSON #-}

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

instance J.Encode ModRef where
  build o = J.object
    [ "refSpec" J..= (J.Array <$> _modRefSpec o)
    , "refInfo" J..= _modRefInfo o
    , "refName" J..= _modRefName o
    ]
  {-# INLINEABLE build #-}

instance FromJSON ModRef where parseJSON = lensyParseJSON 4
instance Ord ModRef where
  (ModRef a b _) `compare` (ModRef c d _) = (a,b) `compare` (c,d)
instance SizeOf ModRef where
  sizeOf ver (ModRef n s _) = constructorCost 1 + sizeOf ver n + sizeOf ver s

-- | This JSON encoding omits the @refInfo@ property when it is the default Value.
--
-- This is different from the encoding of the 'ToJSON' instance of ModRef which
-- always includes the @refInfo@ property.
--
-- The alternative encoding is used by the 'ToJSON' instances of 'PactValue' and
-- 'OldPactValue'.
--
modRefKeyValues_ :: ModRef -> [Maybe J.KeyValue]
modRefKeyValues_ o =
    [ "refSpec" J..= (J.Array <$> _modRefSpec o)
    , "refInfo" J..?= if refInfo /= def then Just refInfo else Nothing
      -- this property is different from the instance Pact.Types.Term.ModRef
    , "refName" J..= _modRefName o
    ]
 where
  refInfo = _modRefInfo o
{-# INLINEABLE modRefKeyValues_ #-}

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
  sizeOf ver (ModuleGuard md n) =
    constructorCost 2 + sizeOf ver md + sizeOf ver n

instance J.Encode ModuleGuard where
  build o = J.object
    [ "moduleName" J..= _mgModuleName o
    , "name" J..= _mgName o
    ]
  {-# INLINABLE build #-}

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
  sizeOf ver (PactGuard pid pn) =
    (constructorCost 2) + (sizeOf ver pid) + (sizeOf ver pn)

instance J.Encode PactGuard where
  build o = J.object
    [ "pactId" J..= _pgPactId o
    , "name" J..= _pgName o
    ]
  {-# INLINABLE build #-}

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

instance J.Encode v => J.Encode (ObjectMap v) where
  build (ObjectMap om) = J.build $ legacyMap asString om
  {-# INLINEABLE build #-}

instance FromJSON v => FromJSON (ObjectMap v) where
  parseJSON v = flip (withObject "ObjectMap") v $ \_ ->
    ObjectMap . M.mapKeys FieldKey <$> parseJSON v
  {-# INLINE parseJSON #-}

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

instance J.Encode Use where
  build o = J.object
    [ "hash" J..= _uModuleHash o
    , "imports" J..= (J.Array <$> _uImports o)
    , "module" J..= _uModuleName o
    ,  "i" J..= _uInfo o
    ]
  {-# INLINABLE build #-}

instance FromJSON Use where
  parseJSON = withObject "Use" $ \o ->
    Use <$> o .: "module"
        <*> o .:? "hash"
        <*> o .:? "imports"
        <*> o .:? "i" .!= Info Nothing

instance NFData Use
instance SizeOf Use

-- -------------------------------------------------------------------------- --
-- CapabilityGuard

-- | Capture a capability to be required as a guard,
-- with option to be further constrained to a defpact execution context.
data CapabilityGuard n = CapabilityGuard
    { _cgName :: !QualifiedName
    , _cgArgs :: ![n]
    , _cgPactId :: !(Maybe PactId)
    }
  deriving (Eq,Show,Generic,Functor,Foldable,Traversable,Ord)

instance NFData a => NFData (CapabilityGuard a)

instance Pretty a => Pretty (CapabilityGuard a) where
  pretty CapabilityGuard{..} = "CapabilityGuard" <+> commaBraces
    [ "name: " <> pretty _cgName
    , "args: " <> pretty _cgArgs
    , "pactId: " <> pretty _cgPactId
    ]

instance (SizeOf a) => SizeOf (CapabilityGuard a) where
  sizeOf ver CapabilityGuard{..} =
    (constructorCost 2) + (sizeOf ver _cgName) + (sizeOf ver _cgArgs) + (sizeOf ver _cgPactId)

instance J.Encode a => J.Encode (CapabilityGuard a) where
  build o = J.object
    [ "cgPactId" J..= _cgPactId o
    , "cgArgs" J..= J.Array (_cgArgs o)
    , "cgName" J..= _cgName o
    ]
  {-# INLINABLE build #-}

instance FromJSON a => FromJSON (CapabilityGuard a) where
  parseJSON = lensyParseJSON 1
  {-# INLINE parseJSON #-}

-- -------------------------------------------------------------------------- --
-- Guard

data Guard a
  = GPact !PactGuard
  | GKeySet !KeySet
  | GKeySetRef !KeySetName
  | GModule !ModuleGuard
  | GUser !(UserGuard a)
  | GCapability !(CapabilityGuard a)
  deriving (Eq,Show,Generic,Functor,Foldable,Traversable,Ord)

instance NFData a => NFData (Guard a)

instance Pretty a => Pretty (Guard a) where
  pretty (GPact g) = pretty g
  pretty (GKeySet g) = pretty g
  pretty (GKeySetRef g) = pretty g
  pretty (GUser g) = pretty g
  pretty (GModule g) = pretty g
  pretty (GCapability g) = pretty g

instance (SizeOf p) => SizeOf (Guard p) where
  sizeOf ver (GPact pg) = (constructorCost 1) + (sizeOf ver pg)
  sizeOf ver (GKeySet ks) = (constructorCost 1) + (sizeOf ver ks)
  sizeOf ver (GKeySetRef ksr) = (constructorCost 1) + (sizeOf ver ksr)
  sizeOf ver (GModule mg) = (constructorCost 1) + (sizeOf ver mg)
  sizeOf ver (GUser ug) = (constructorCost 1) + (sizeOf ver ug)
  sizeOf ver (GCapability g) = (constructorCost 1) + (sizeOf ver g)

keyNamef :: Key
keyNamef = "keysetref"

data GuardProperty
  = GuardArgs
  | GuardCgArgs
  | GuardCgName
  | GuardCgPactId
  | GuardFun
  | GuardKeys
  | GuardKeysetref
  | GuardKsn
  | GuardModuleName
  | GuardName
  | GuardNs
  | GuardPactId
  | GuardPred
  | GuardUnknown !String
  deriving (Show, Eq, Ord)

_gprop :: IsString a => Semigroup a => GuardProperty -> a
_gprop GuardArgs = "args"
_gprop GuardCgArgs = "cgArgs"
_gprop GuardCgName = "cgName"
_gprop GuardCgPactId = "cgPactId"
_gprop GuardFun = "fun"
_gprop GuardKeys = "keys"
_gprop GuardKeysetref = "keysetref"
_gprop GuardKsn = "ksn"
_gprop GuardModuleName = "moduleName"
_gprop GuardName = "name"
_gprop GuardNs = "ns"
_gprop GuardPactId = "pactId"
_gprop GuardPred = "pred"
_gprop (GuardUnknown t) = "UNKNOWN_GUARD[" <> fromString t <> "]"

ungprop :: IsString a => Eq a => Show a => a -> GuardProperty
ungprop "args" = GuardArgs
ungprop "cgArgs" = GuardCgArgs
ungprop "cgName" = GuardCgName
ungprop "cgPactId" = GuardCgPactId
ungprop "fun" = GuardFun
ungprop "keys" = GuardKeys
ungprop "keysetref" = GuardKeysetref
ungprop "ksn" = GuardKsn
ungprop "moduleName" = GuardModuleName
ungprop "name" = GuardName
ungprop "ns" = GuardNs
ungprop "pactId" = GuardPactId
ungprop "pred" = GuardPred
ungprop t = GuardUnknown (show t)


instance J.Encode a => J.Encode (Guard a) where
  build (GKeySet k) = J.build k
  build (GKeySetRef n) = J.object ["keysetref" J..= n]
  build (GPact g) = J.build g
  build (GModule g) = J.build g
  build (GUser g) = J.build g
  build (GCapability g) = J.build g
  {-# INLINEABLE build #-}

instance FromJSON a => FromJSON (Guard a) where
  parseJSON v = case props v of
    [GuardKeys, GuardPred] -> GKeySet <$> parseJSON v
    [GuardKeysetref] -> flip (withObject "KeySetRef") v $ \o ->
        GKeySetRef <$> o .: keyNamef
    [GuardName, GuardPactId] -> GPact <$> parseJSON v
    [GuardModuleName, GuardName] -> GModule <$> parseJSON v
    [GuardArgs, GuardFun] -> GUser <$> parseJSON v
    [GuardCgArgs, GuardCgName, GuardCgPactId] -> GCapability <$> parseJSON v
    _ -> fail $ "unexpected properties for Guard: "
      <> show (props v)
      <> ", " <> show (J.encode v)
   where
    props (A.Object o) = sort $ ungprop <$> A.keys o
    props _ = []
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

instance J.Encode g => J.Encode (Module g) where
  build o = J.object
    [ "hash" J..= _mHash o
    , "blessed" J..= J.Array (LHM.sort $ HS.toList $ _mBlessed o)
    , "interfaces" J..= J.Array (_mInterfaces o)
    , "imports" J..= J.Array (_mImports o)
    , "name" J..= _mName o
    , "code" J..= _mCode o
    , "meta" J..= _mMeta o
    , "governance" J..= _mGovernance o
    ]
  {-# INLINEABLE build #-}

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

instance J.Encode Interface where
  build o = J.object
    [ "imports" J..= J.Array (_interfaceImports o)
    , "name" J..= _interfaceName o
    , "code" J..= _interfaceCode o
    , "meta" J..= _interfaceMeta o
    ]
  {-# INLINEABLE build #-}

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

instance J.Encode g => J.Encode (ModuleDef g) where
  build (MDModule m) = J.build m
  build (MDInterface i) = J.build i
  {-# INLINEABLE build #-}

instance FromJSON g => FromJSON (ModuleDef g) where
  -- parseJSON v = MDModule <$> parseJSON v <|> MDInterface <$> parseJSON v
  parseJSON v
    = MDModule <$> parseJSON v
    <|> MDInterface <$> parseJSON v

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
instance Eq1 CapabilityGuard where
  liftEq = $(makeLiftEq ''CapabilityGuard)
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
instance Show1 CapabilityGuard where
  liftShowsPrec = $(makeLiftShowsPrec ''CapabilityGuard)
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

