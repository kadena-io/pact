{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
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
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DerivingStrategies #-}

-- |
-- Module      :  Pact.Types.Term
-- Copyright   :  (C) 2019 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>,
--                Emily Pillmore <emily@kadena.io>
--
-- Term and related types.
--
module Pact.Types.Term
 ( Namespace(..),
   NamespaceName(..),
   Meta(..),mDocs,mModel,
   PublicKey(..),
   KeySet(..),
   KeySetName(..),
   PactGuard(..),
   PactId(..),
   UserGuard(..),
   ModuleGuard(..),
   Guard(..),
   DefType(..),_Defun,_Defpact,_Defcap,
   defTypeRep,
   NativeDefName(..),DefName(..),
   FunApp(..),faModule,faDefType,
   Ref'(..),_Direct,_Ref,Ref,
   NativeDFun(..),
   BindType(..),
   BindPair(..),toBindPairs,
   TableName(..),
   Module(..),mName,mMeta,
   Interface(..),interfaceName,
   ModuleDef(..),moduleDefName,moduleDefCode,moduleDefMeta,
   Governance(..),
   ModuleName(..), mnName, mnNamespace,
   ModuleHash(..),
   Name(..),parseName,
   ConstVal(..),constTerm,
   Use(..),
   App(..),appFun,appArgs,appInfo,
   Def(..),dDefBody,dDefName,dDefType,dMeta,dFunType,dInfo,dModule,
   Example(..),
   derefDef,
   ObjectMap(..),objectMapToListWith,
   Object(..),
   FieldKey(..),
   Step(..),
   Term(..), _TModule, _TList, _TDef, _TNative, _TConst, _TApp, _TVar, _TBinding,
   _TObject, _TSchema, _TLiteral, _TGuard, _TUse, _TStep, _TTable,
   ToTerm(..), toTermList,toTObject,toTObjectMap,toTList,toTListV,
   typeof,typeof',guardTypeOf,
   pattern TLitString,pattern TLitInteger,pattern TLitBool,
   tLit,tStr,termEq,canEq,
   Gas(..),
   PList(..), plList,
   Native(..), nfTopLevelOnly,
   PConst(..),pcModule,
   Binding(..),
   PSchema(..), psModule, psInfo, psMeta,
   PTable(..), ptModule, ptInfo
   ) where

import Bound
import Control.Applicative
import Control.DeepSeq
import Control.Lens (Lens', lens, makeLenses, makePrisms)
import Control.Monad
import qualified Data.Aeson as A
#if MIN_VERSION_aeson(1,4,3)
import Data.Aeson hiding (pairs,Object, (<?>))
#else
import Data.Aeson hiding (pairs,Object)
#endif
import qualified Data.Attoparsec.Text as AP
import qualified Data.ByteString.UTF8 as BS
import Data.Decimal
import Data.Default
import Data.Eq.Deriving
import Data.Foldable
import Data.Function
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Hashable
import Data.Int (Int64)
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Serialize (Serialize)
import Data.String
import Data.Text (Text,pack)
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Thyme (UTCTime)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word64, Word32)
import GHC.Generics (Generic)
import Prelude
import Text.Show.Deriving
import Text.Trifecta (ident,TokenParsing,(<?>),dot,eof)

import Pact.Types.Codec
import Pact.Types.Exp
import Pact.Types.Hash
import Pact.Types.Info
import Pact.Types.Parser
import Pact.Types.Pretty hiding (dot)
import Pact.Types.Type
import Pact.Types.Util

data Meta = Meta
  { _mDocs  :: !(Maybe Text) -- ^ docs
  , _mModel :: ![Exp Info]   -- ^ models
  } deriving (Eq, Show, Generic)

instance Pretty Meta where
  pretty (Meta (Just doc) model) = dquotes (pretty doc) <> line <> prettyModel model
  pretty (Meta Nothing    model) = prettyModel model

instance NFData Meta

prettyModel :: [Exp Info] -> Doc
prettyModel []    = mempty
prettyModel props = "@model " <> list (pretty <$> props)

instance ToJSON Meta where toJSON = lensyToJSON 2
instance FromJSON Meta where parseJSON = lensyParseJSON 2

instance Default Meta where def = Meta def def

instance Semigroup Meta where
  (Meta d m) <> (Meta d' m') = Meta (d <> d') (m <> m')

instance Monoid Meta where
  mempty = Meta Nothing []

newtype PublicKey = PublicKey { _pubKey :: BS.ByteString }
  deriving (Eq,Ord,Generic,IsString,AsString,Show)

instance Serialize PublicKey
instance NFData PublicKey
instance FromJSON PublicKey where
  parseJSON = withText "PublicKey" (return . PublicKey . encodeUtf8)
instance ToJSON PublicKey where
  toJSON = toJSON . decodeUtf8 . _pubKey

instance Pretty PublicKey where
  pretty (PublicKey s) = prettyString (BS.toString s)

-- | KeySet pairs keys with a predicate function name.
data KeySet = KeySet {
      _ksKeys :: ![PublicKey]
    , _ksPredFun :: !Name
    } deriving (Eq,Generic,Show,Ord)

instance NFData KeySet

instance Pretty KeySet where
  pretty (KeySet ks f) = "KeySet" <+> commaBraces
    [ "keys: " <> prettyList ks
    , "pred: " <> pretty f
    ]

-- | allow `{ "keys": [...], "pred": "..." }`, `{ "keys": [...] }`, and just `[...]`,
-- | the latter cases defaulting to "keys-all"
instance FromJSON KeySet where
    parseJSON v = withObject "KeySet" (\o ->
                    KeySet <$> o .: "keys" <*>
                    (fromMaybe defPred <$> o .:? "pred")) v <|>
                  (KeySet <$> parseJSON v <*> pure defPred)
      where defPred = Name "keys-all" def
instance ToJSON KeySet where
    toJSON (KeySet k f) = object ["keys" .= k, "pred" .= f]


newtype KeySetName = KeySetName Text
    deriving (Eq,Ord,IsString,AsString,ToJSON,FromJSON,Show,NFData,Generic)

instance Pretty KeySetName where pretty (KeySetName s) = "'" <> pretty s

newtype PactId = PactId Text
    deriving (Eq,Ord,Show,Pretty,AsString,IsString,FromJSON,ToJSON,Generic,NFData,ToTerm)

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

instance ToJSON PactGuard where toJSON = lensyToJSON 3
instance FromJSON PactGuard where parseJSON = lensyParseJSON 3

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

instance ToJSON ModuleGuard where toJSON = lensyToJSON 3
instance FromJSON ModuleGuard where parseJSON = lensyParseJSON 3

data UserGuard = UserGuard
  { _ugData :: !(Object Name)
  , _ugPredFun :: !Name
  } deriving (Eq,Generic,Show)

instance NFData UserGuard

instance Pretty UserGuard where
  pretty UserGuard{..} = "UserGuard" <+> commaBraces
    [ "data: " <> pretty _ugData
    , "pred: " <> pretty _ugPredFun
    ]

instance ToJSON UserGuard where toJSON = lensyToJSON 3
instance FromJSON UserGuard where parseJSON = lensyParseJSON 3

data Guard
  = GPact !PactGuard
  | GKeySet !KeySet
  | GKeySetRef !KeySetName
  | GModule !ModuleGuard
  | GUser !UserGuard
  deriving (Eq,Show,Generic)

instance NFData Guard

instance Pretty Guard where
  pretty (GPact g)      = pretty g
  pretty (GKeySet g)    = pretty g
  pretty (GKeySetRef g) = pretty g
  pretty (GUser g)      = pretty g
  pretty (GModule g)    = pretty g


guardCodec :: Codec Guard
guardCodec = Codec enc dec
  where
    enc (GKeySet k) = toJSON k
    enc (GKeySetRef n) = object [ keyNamef .= n ]
    enc (GPact g) = toJSON g
    enc (GModule g) = toJSON g
    enc (GUser g) = toJSON g
    {-# INLINE enc #-}
    dec v =
      (GKeySet <$> parseJSON v) <|>
      (withObject "KeySetRef" $ \o ->
          GKeySetRef . KeySetName <$> o .: keyNamef) v <|>
      (GPact <$> parseJSON v) <|>
      (GModule <$> parseJSON v) <|>
      (GUser <$> parseJSON v)
    {-# INLINE dec #-}
    keyNamef = "keysetref"


instance ToJSON Guard where toJSON = encoder guardCodec
instance FromJSON Guard where parseJSON = decoder guardCodec

data DefType
  = Defun
  | Defpact
  | Defcap
  deriving (Eq,Show,Generic, Bounded, Enum)

instance FromJSON DefType
instance ToJSON DefType
instance NFData DefType

defTypeRep :: DefType -> String
defTypeRep Defun = "defun"
defTypeRep Defpact = "defpact"
defTypeRep Defcap = "defcap"

instance Pretty DefType where
  pretty = prettyString . defTypeRep

newtype NativeDefName = NativeDefName Text
    deriving (Eq,Ord,IsString,ToJSON,FromJSON,AsString,Show,NFData)

instance Pretty NativeDefName where
  pretty (NativeDefName name) = pretty name

-- | Capture function application metadata
data FunApp = FunApp {
      _faInfo :: !Info
    , _faName :: !Text
    , _faModule :: !(Maybe ModuleName)
    , _faDefType :: !DefType
    , _faTypes :: !(FunTypes (Term Name))
    , _faDocs :: !(Maybe Text)
    } deriving (Show,Eq,Generic)
instance ToJSON FunApp where toJSON = lensyToJSON 3
instance FromJSON FunApp where parseJSON = lensyParseJSON 3
instance GetInfo FunApp where getInfo = _faInfo

faModule :: Lens' FunApp (Maybe ModuleName)
faModule = lens _faModule (\t b -> t { _faModule = b })

faDefType :: Lens' FunApp DefType
faDefType = lens _faDefType (\t b -> t { _faDefType = b })

-- | Variable type for an evaluable 'Term'.
data Ref' d =
  -- | "Reduced" (evaluated) or native (irreducible) term.
  Direct d |
  -- | Unevaulated/un-reduced term, never a native.
  Ref (Term (Ref' d))
  deriving (Eq,Show,Functor,Foldable,Traversable,Generic)

instance NFData d => NFData (Ref' d)

type Ref = Ref' (Term Name)

instance Pretty d => Pretty (Ref' d) where
  pretty (Direct tm) = pretty tm
  pretty (Ref tm)    = pretty tm

instance GetInfo n => GetInfo (Ref' n) where
  getInfo (Direct d) = getInfo d
  getInfo (Ref r) = getInfo r

-- | Gas compute cost unit.
newtype Gas = Gas Int64
  deriving (Eq,Ord,Num,Real,Integral,Enum,Show,ToJSON,FromJSON,Generic)

instance Pretty Gas where
  pretty (Gas i) = pretty i

instance Semigroup Gas where
  (Gas a) <> (Gas b) = Gas $ a + b

instance Monoid Gas where
  mempty = 0

data NativeDFun = NativeDFun
  { _nativeName :: NativeDefName
  , _nativeFun :: forall m . Monad m => FunApp -> [Term Ref] -> m (Gas,Term Name)
  }

instance Eq NativeDFun where a == b = _nativeName a == _nativeName b
instance Show NativeDFun where
  showsPrec p (NativeDFun name _) = showParen (p > 10) $
    showString "NativeDFun " . showsPrec 11 name . showString " _"

instance NFData NativeDFun where
  rnf (NativeDFun n _f) = seq n ()

-- | Binding forms.
data BindType n =
  -- | Normal "let" bind
  BindLet |
  -- | Schema-style binding, with string value for key
  BindSchema { _bType :: n }
  deriving (Eq,Functor,Foldable,Traversable,Ord,Show,Generic)

instance (Pretty n) => Pretty (BindType n) where
  pretty BindLet = "let"
  pretty (BindSchema b) = "bind" <> pretty b

instance ToJSON n => ToJSON (BindType n) where
  toJSON BindLet = "let"
  toJSON (BindSchema s) = object [ "bind" .= s ]

instance FromJSON n => FromJSON (BindType n) where
  parseJSON v =
    withThisText "BindLet" "let" v (pure BindLet) <|>
    withObject "BindSchema" (\o -> BindSchema <$> o .: "bind") v

instance NFData n => NFData (BindType n)


data BindPair n = BindPair
  { _bpArg :: Arg n
  , _bpVal :: n }
  deriving (Eq,Show,Functor,Traversable,Foldable,Generic)

toBindPairs :: BindPair n -> (Arg n,n)
toBindPairs (BindPair a v) = (a,v)

instance Pretty n => Pretty (BindPair n) where
  pretty (BindPair arg body) = pretty arg <+> pretty body

instance NFData n => NFData (BindPair n)

instance ToJSON n => ToJSON (BindPair n) where toJSON = lensyToJSON 3
instance FromJSON n => FromJSON (BindPair n) where parseJSON = lensyParseJSON 3

newtype TableName = TableName Text
    deriving (Eq,Ord,IsString,ToTerm,AsString,Hashable,Show,NFData,ToJSON,FromJSON)
instance Pretty TableName where pretty (TableName s) = pretty s

data ModuleName = ModuleName
  { _mnName      :: Text
  , _mnNamespace :: Maybe NamespaceName
  } deriving (Eq, Ord, Generic, Show)

instance Hashable ModuleName where
  hashWithSalt s (ModuleName n Nothing)   =
    s `hashWithSalt` (0::Int) `hashWithSalt` n
  hashWithSalt s (ModuleName n (Just ns)) =
    s `hashWithSalt` (1::Int) `hashWithSalt` n `hashWithSalt` ns

instance NFData ModuleName

instance AsString ModuleName where
  asString (ModuleName n Nothing) = n
  asString (ModuleName n (Just (NamespaceName ns))) = ns <> "." <> n

instance IsString ModuleName where
  fromString = coalesce . T.splitOn "." . pack
    where
      coalesce l = case l of
        [ns,n] -> ModuleName n (Just (NamespaceName ns))
        [n]    -> ModuleName n Nothing
        _      -> ModuleName (pack . show $ l) (Just . NamespaceName $ "Err: malformed name")

instance Pretty ModuleName where
  pretty (ModuleName n Nothing)   = pretty n
  pretty (ModuleName n (Just ns)) = pretty ns <> "." <> pretty n

instance ToJSON ModuleName where toJSON = lensyToJSON 3
instance FromJSON ModuleName where parseJSON = lensyParseJSON 3

newtype DefName = DefName Text
    deriving (Eq,Ord,IsString,ToJSON,FromJSON,AsString,Hashable,Pretty,Show,NFData)

-- | A named reference from source.
data Name
  = QName { _nQual :: ModuleName, _nName :: Text, _nInfo :: Info }
  | Name { _nName :: Text, _nInfo :: Info }
  deriving (Generic, Show)

instance GetInfo Name where
  getInfo (QName _ _ i) = i
  getInfo (Name _ i) = i

instance Pretty Name where
  pretty = \case
    QName modName nName _ -> pretty modName <> "." <> pretty nName
    Name nName _          -> pretty nName

instance AsString Name where asString = renderCompactText

instance ToJSON Name where
  toJSON = toJSON . renderCompactString

instance FromJSON Name where
  parseJSON = withText "Name" $ \t -> case parseName def t of
    Left s  -> fail s
    Right n -> return n

instance NFData Name

parseName :: Info -> Text -> Either String Name
parseName i = AP.parseOnly (nameParser i <* eof)


nameParser :: (TokenParsing m, Monad m) => Info -> m Name
nameParser i = do
  a <- ident style
  b <- optional (dot *> (ident style))
  case b of
    Nothing -> return (Name a i)
    Just b' -> do
      c <- optional (dot *> ident style)
      case c of
        Nothing -> return (QName (ModuleName a Nothing) b' i) <?> "qualified name"
        Just c' -> return (QName (ModuleName b' (Just . NamespaceName $ a)) c' i)

instance Hashable Name where
  hashWithSalt s (Name t _) = s `hashWithSalt` (0::Int) `hashWithSalt` t
  hashWithSalt s (QName q n _) = s `hashWithSalt` (1::Int) `hashWithSalt` q `hashWithSalt` n
instance Eq Name where
  (QName a b _) == (QName c d _) = (a,b) == (c,d)
  (Name a _) == (Name b _) = a == b
  _ == _ = False
instance Ord Name where
  (QName a b _) `compare` (QName c d _) = (a,b) `compare` (c,d)
  (Name a _) `compare` (Name b _) = a `compare` b
  Name {} `compare` QName {} = LT
  QName {} `compare` Name {} = GT


data Use = Use
  { _uModuleName :: !ModuleName
  , _uModuleHash :: !(Maybe ModuleHash)
  , _uInfo :: !Info
  } deriving (Eq, Show, Generic)

instance GetInfo Use where getInfo = _uInfo

instance Pretty Use where
  pretty Use{..} =
    let args = pretty _uModuleName : maybe [] (\mh -> [pretty mh]) _uModuleHash
    in parensSep $ "use" : args

instance ToJSON Use where
  toJSON Use{..} =
    object $ ("module" .= _uModuleName) : ("i" .= _uInfo) :
    (maybe [] (return . ("hash" .=)) _uModuleHash)
instance FromJSON Use where
  parseJSON = withObject "Use" $ \o ->
    Use <$> o .: "module"
        <*> o .:? "hash"
        <*> o .: "i"

instance NFData Use

data App t = App
  { _appFun :: !t
  , _appArgs :: ![t]
  , _appInfo :: !Info
  } deriving (Functor,Foldable,Traversable,Eq,Show,Generic)

instance GetInfo (App t) where getInfo = _appInfo

instance ToJSON t => ToJSON (App t) where toJSON = lensyToJSON 4
instance FromJSON t => FromJSON (App t) where parseJSON = lensyParseJSON 4

instance Pretty n => Pretty (App n) where
  pretty App{..} = parensSep $ pretty _appFun : map pretty _appArgs

instance NFData t => NFData (App t)


newtype Governance g = Governance { _gGovernance :: Either KeySetName g }
  deriving (Eq,Ord,Functor,Foldable,Traversable,Show,NFData)

instance Pretty g => Pretty (Governance g) where
  pretty = \case
    Governance (Left  k) -> pretty k
    Governance (Right r) -> pretty r

instance ToJSON g => ToJSON (Governance g) where
  toJSON (Governance g) = case g of
    Left ks -> object [ "keyset" .= ks ]
    Right c -> object [ "capability" .= c ]
instance FromJSON g => FromJSON (Governance g) where
  parseJSON = withObject "Governance" $ \o ->
    Governance <$> (Left <$> o .: "keyset" <|>
                    Right <$> o .: "capability")

-- | Newtype wrapper differentiating 'Hash'es from module hashes
--
newtype ModuleHash = ModuleHash { _mhHash :: Hash }
  deriving (Eq, Ord, Show, Generic, Hashable, Serialize, AsString, Pretty, ToJSON, FromJSON, ParseText)
  deriving newtype (NFData)

data Module g = Module
  { _mName :: !ModuleName
  , _mGovernance :: !(Governance g)
  , _mMeta :: !Meta
  , _mCode :: !Code
  , _mHash :: !ModuleHash
  , _mBlessed :: !(HS.HashSet ModuleHash)
  , _mInterfaces :: [ModuleName]
  , _mImports :: [Use]
  , _mInfo :: !Info
  } deriving (Eq,Functor,Foldable,Traversable,Show,Generic)

instance NFData g => NFData (Module g)

instance Pretty g => Pretty (Module g) where
  pretty Module{..} = parensSep
    [ "module" , pretty _mName , pretty _mGovernance , pretty _mHash ]

instance ToJSON g => ToJSON (Module g) where toJSON = lensyToJSON 2
instance FromJSON g => FromJSON (Module g) where parseJSON = lensyParseJSON 2

instance GetInfo (Module g) where
  getInfo = _mInfo

mName :: Lens' (Module n) ModuleName
mName = lens _mName (\t b -> t { _mName = b })

mMeta :: Lens' (Module n) Meta
mMeta = lens _mMeta (\t b -> t { _mMeta = b })

data Interface = Interface
  { _interfaceName :: !ModuleName
  , _interfaceCode :: !Code
  , _interfaceMeta :: !Meta
  , _interfaceImports :: [Use]
  , _interfaceInfo :: !Info
  } deriving (Eq,Show,Generic)
instance Pretty Interface where
  pretty Interface{..} = parensSep [ "interface", pretty _interfaceName ]

instance ToJSON Interface where toJSON = lensyToJSON 10
instance FromJSON Interface where parseJSON = lensyParseJSON 10

instance NFData Interface

instance GetInfo Interface where
  getInfo = _interfaceInfo

interfaceName :: Lens' Interface ModuleName
interfaceName = lens _interfaceName (\t b -> t { _interfaceName = b })

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
  toJSON (MDModule m) = toJSON m
  toJSON (MDInterface i) = toJSON i

instance FromJSON g => FromJSON (ModuleDef g) where
  parseJSON v = MDModule <$> parseJSON v <|> MDInterface <$> parseJSON v

instance GetInfo (ModuleDef g) where
  getInfo (MDModule m) = _mInfo m
  getInfo (MDInterface i) = _interfaceInfo i

moduleDefName :: ModuleDef g -> ModuleName
moduleDefName (MDModule m) = _mName m
moduleDefName (MDInterface m) = _interfaceName m

moduleDefCode :: ModuleDef g -> Code
moduleDefCode (MDModule m) = _mCode m
moduleDefCode (MDInterface m) = _interfaceCode m

moduleDefMeta :: ModuleDef g -> Meta
moduleDefMeta (MDModule m) = _mMeta m
moduleDefMeta (MDInterface m) = _interfaceMeta m

data Def n = Def
  { _dDefName :: !DefName
  , _dModule :: !ModuleName
  , _dDefType :: !DefType
  , _dFunType :: !(FunType (Term n))
  , _dDefBody :: !(Scope Int Term n)
  , _dMeta :: !Meta
  , _dInfo :: !Info
  } deriving (Functor,Foldable,Traversable,Generic)

deriving instance Show n => Show (Def n)
deriving instance Eq n => Eq (Def n)
instance NFData n => NFData (Def n)

instance GetInfo (Def n) where getInfo = _dInfo

instance Pretty n => Pretty (Def n) where
  pretty Def{..} = parensSep $
    [ prettyString (defTypeRep _dDefType)
    , pretty _dModule <> "." <> pretty _dDefName <> ":" <> pretty (_ftReturn _dFunType)
    , parensSep $ pretty <$> _ftArgs _dFunType
    ] ++ maybe [] (\docs -> [pretty docs]) (_mDocs _dMeta)

instance (ToJSON n, FromJSON n) => ToJSON (Def n) where toJSON = lensyToJSON 2
instance (ToJSON n, FromJSON n) => FromJSON (Def n) where parseJSON = lensyParseJSON 2

derefDef :: Def Ref -> Name
derefDef Def{..} = QName _dModule (asString _dDefName) _dInfo



newtype NamespaceName = NamespaceName Text
  deriving (Eq, Ord, Show, FromJSON, ToJSON, IsString, AsString, Hashable, Pretty, Generic, NFData)

data Namespace = Namespace
  { _nsName   :: NamespaceName
  , _nsGuard  :: Guard
  } deriving (Eq, Show, Generic)

instance Pretty Namespace where
  pretty Namespace{..} = "(namespace " <> prettyString (asString' _nsName) <> ")"

instance ToJSON Namespace where toJSON = lensyToJSON 3
instance FromJSON Namespace where parseJSON = lensyParseJSON 3

instance NFData Namespace

data ConstVal n =
  CVRaw { _cvRaw :: !n } |
  CVEval { _cvRaw :: !n
         , _cvEval :: !n }
  deriving (Eq,Functor,Foldable,Traversable,Generic,Show)

instance NFData n => NFData (ConstVal n)

instance ToJSON n => ToJSON (ConstVal n) where
  toJSON (CVRaw n) = object [ "raw" .= n ]
  toJSON (CVEval n m) = object [ "raw" .= n, "eval" .= m ]

instance FromJSON n => FromJSON (ConstVal n) where
  parseJSON v =
    (withObject "CVEval"
     (\o -> CVEval <$> o .: "raw" <*> o .: "eval") v) <|>
    (withObject "CVRaw"
     (\o -> CVRaw <$> o .: "raw") v)

-- | A term from a 'ConstVal', preferring evaluated terms when available.
constTerm :: ConstVal a -> a
constTerm (CVRaw raw) = raw
constTerm (CVEval _raw eval) = eval

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

-- | Label type for objects.
newtype FieldKey = FieldKey Text
  deriving (Eq,Ord,IsString,AsString,ToJSON,FromJSON,Show,NFData,Generic,ToJSONKey)
instance Pretty FieldKey where
  pretty (FieldKey k) = dquotes $ pretty k

-- | Simple dictionary for object values.
newtype ObjectMap v = ObjectMap { _objectMap :: (M.Map FieldKey v) }
  deriving (Eq,Ord,Show,Functor,Foldable,Traversable,Generic)

instance NFData v => NFData (ObjectMap v)

-- | O(n) conversion to list. Adapted from 'M.toAscList'
objectMapToListWith :: (FieldKey -> v -> r) -> ObjectMap v -> [r]
objectMapToListWith f (ObjectMap m) = M.foldrWithKey (\k x xs -> (f k x):xs) [] m

instance Pretty v => Pretty (ObjectMap v) where
  pretty om = annotate Val $ commaBraces $
    objectMapToListWith (\k v -> pretty k <> ": " <> pretty v) om

instance ToJSON v => ToJSON (ObjectMap v)
  where toJSON om =
          object $ objectMapToListWith (\k v -> (asString k,toJSON v)) om

instance FromJSON v => FromJSON (ObjectMap v)
  where parseJSON = withObject "ObjectMap" $ \o ->
          ObjectMap . M.fromList <$>
            traverse (\(k,v) -> (FieldKey k,) <$> parseJSON v) (HM.toList o)

instance Default (ObjectMap v) where
  def = ObjectMap M.empty


-- | Full Term object.
data Object n = Object
  { _oObject :: !(ObjectMap (Term n))
  , _oObjectType :: !(Type (Term n))
  , _oKeyOrder :: Maybe [FieldKey]
  , _oInfo :: !Info
  } deriving (Functor,Foldable,Traversable,Eq,Show,Generic)

instance GetInfo (Object n) where getInfo = _oInfo
instance Pretty n => Pretty (Object n) where
  pretty (Object bs _ Nothing _) = pretty bs
  pretty (Object (ObjectMap om) _ (Just ko) _) =
    annotate Val $ commaBraces $
    map (\(k,v) -> pretty k <> ": " <> pretty v) $
    sortBy (compare `on` (keyOrder . fst)) $
    M.toList om
    where keyOrder f = elemIndex f ko

instance NFData n => NFData (Object n)

instance (ToJSON n, FromJSON n) => ToJSON (Object n) where
  toJSON Object{..} = object $
    [ "obj" .= _oObject
    , "type" .= _oObjectType
    , "i" .= _oInfo ] ++
    maybe [] (pure . ("keyorder" .=)) _oKeyOrder

instance (ToJSON n, FromJSON n) => FromJSON (Object n) where
  parseJSON = withObject "Object" $ \o ->
    Object <$> o .: "obj" <*> o .: "type" <*> o .:? "keyorder" <*> o .: "i"

data Step n = Step
  { _sEntity :: !(Maybe n)
  , _sExec :: !n
  , _sRollback :: !(Maybe n)
  , _sInfo :: !Info
  } deriving (Eq,Show,Generic,Functor,Foldable,Traversable)
instance NFData n => NFData (Step n)
instance ToJSON n => ToJSON (Step n) where toJSON = lensyToJSON 2
instance FromJSON n => FromJSON (Step n) where parseJSON = lensyParseJSON 2
instance GetInfo (Step n) where getInfo = _sInfo
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

data PList n = PList
  { _plList :: !(Vector (Term n))
  , _plListType :: Type (Term n)
  , _plInfo :: !Info
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)


instance GetInfo (PList n) where getInfo = _plInfo
instance (FromJSON n, ToJSON n) => ToJSON (PList n) where
  toJSON PList{..} = object
    [ "list" .= _plList
    , "type" .= _plListType
    , "i" .= _plInfo
    ]
instance (FromJSON n, ToJSON n) => FromJSON (PList n) where
  parseJSON = withObject "PList" $ \o -> PList
    <$> o .: "list"
    <*> o .: "type"
    <*> o .: "i"

instance NFData n => NFData (PList n)
instance Pretty n => Pretty (PList n) where
  pretty l = bracketsSep $ pretty <$> V.toList (_plList l)

plList :: Lens' (PList n) (Vector (Term n))
plList = lens _plList (\t b -> t { _plList = b })

data Native n = Native
  { _nfName :: !NativeDefName
  , _nfFun :: !NativeDFun
  , _nfFunTypes :: FunTypes (Term n)
  , _nfExamples :: ![Example]
  , _nfDocs :: Text
  , _nfTopLevelOnly :: Bool
  , _nfInfo :: !Info
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

instance GetInfo (Native n) where getInfo = _nfInfo
instance NFData n => NFData (Native n)
instance (FromJSON n, ToJSON n) => ToJSON (Native n) where
  toJSON Native{..} = object
    [ "name" .= _nfName
    , "fun"  .= Null -- TODO fn
    , "types" .= _nfFunTypes
    , "examples" .= Null -- TODO examples
    , "docs" .= _nfDocs
    , "tl" .= _nfTopLevelOnly
    , "i" .= _nfInfo
    ]
-- instance (FromJSON n, ToJSON n) => FromJSON (Native n) where
--   parseJSON = withObject "Native" $ \o -> Native
--     <$> o .: "name"
--     <*> o .: "fun"
--     <*> o .: "types"
--     <*> o .: "examples"
--     <*> o .: "docs"
--     <*> o .: "tl"
--     <*> o .: "i"

instance Pretty n => Pretty (Native n) where
  pretty Native{..} = annotate Header ("native `" <> pretty _nfName <> "`")
      <> nest 2 (line
                 <> line
                 <> fillSep (pretty <$> T.words _nfDocs)
                 <> line
                 <> line
                 <> annotate Header "Type:"
                 <> line
                 <> align (vsep (pretty <$> toList _nfFunTypes))
                 <> examples)
    where examples = case _nfExamples of
            [] -> mempty
            exs -> line
              <> line
              <> annotate Header "Examples:"
              <> line
              <> align (vsep (pretty <$> exs))

nfTopLevelOnly :: Lens' (Native n) Bool
nfTopLevelOnly = lens _nfTopLevelOnly (\t b -> t { _nfTopLevelOnly = b })

data PConst n = PConst
  { _pcConstArg :: !(Arg (Term n))
  , _pcModule :: !ModuleName
  , _pcConstVal :: !(ConstVal (Term n))
  , _pcMeta :: !Meta
  , _pcInfo :: !Info
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

instance GetInfo (PConst n) where getInfo = _pcInfo
instance NFData n => NFData (PConst n)
instance (FromJSON n, ToJSON n) => ToJSON (PConst n) where
  toJSON PConst{..} = object
    [ "arg" .= _pcConstArg
    , "modname" .= _pcModule
    , "val" .= _pcConstVal
    , "meta" .= _pcMeta
    , "i" .= _pcInfo
    ]
instance (FromJSON n, ToJSON n) => FromJSON (PConst n) where
  parseJSON = withObject "PConst" $ \o -> PConst
    <$> o .: "arg"
    <*> o .: "modname"
    <*> o .: "val"
    <*> o .: "meta"
    <*> o .: "i"

instance Pretty n => Pretty (PConst n) where
  pretty PConst{..} = "constant "
    <> pretty _pcModule
    <> "."
    <> pretty _pcConstArg
    <> " "
    <> pretty _pcMeta

pcModule :: Lens' (PConst n) ModuleName
pcModule = lens _pcModule (\t b -> t { _pcModule = b })

data Binding n = Binding
  { _bdPairs :: ![BindPair (Term n)]
  , _bdBody :: !(Scope Int Term n)
  , _bdType :: BindType (Type (Term n))
  , _bdInfo :: !Info
  } deriving (Functor, Foldable, Traversable, Generic)

deriving instance Eq n => Eq (Binding n)
deriving instance Show n => Show (Binding n)
instance GetInfo (Binding n) where getInfo = _bdInfo
instance NFData n => NFData (Binding n)
-- (\o -> TBinding <$> o .: pairs <*> o .: body <*> o .: type' <*> inf' o)
instance (FromJSON n, ToJSON n) => ToJSON (Binding n) where
  toJSON (Binding ps b t i) = object
    [ "pairs" .= ps
    , "body" .= b
    , "type" .= t
    , "i" .= i
    ]
instance (FromJSON n, ToJSON n) => FromJSON (Binding n) where
  parseJSON = withObject "Binding" $ \o -> Binding
    <$> o .: "pairs"
    <*> o .: "body"
    <*> o .: "type"
    <*> o .: "i"

instance Pretty n => Pretty (Binding n) where
  pretty (Binding pairs body bf _) = case bf of
    BindLet -> parensSep
      [ "let"
      , parensSep $ fmap pretty pairs
      , pretty $ unscope body
      ]
    BindSchema _ -> parensSep
      [ commaBraces $ fmap pretty pairs
      , pretty $ unscope body
      ]

data PSchema n = PSchema
  { _psName :: !TypeName
  , _psModule :: !ModuleName
  , _psMeta :: !Meta
  , _psFields :: ![Arg (Term n)]
  , _psInfo :: !Info
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

instance GetInfo (PSchema n) where getInfo = _psInfo
instance NFData n => NFData (PSchema n)
instance (FromJSON n, ToJSON n) => ToJSON (PSchema n) where
  toJSON (PSchema n m meta f i) = object
    [ "name" .= n
    , "modname" .= m
    , "meta" .= meta
    , "fields" .= f
    , "i" .= i
    ]
instance (FromJSON n, ToJSON n) => FromJSON (PSchema n) where
  parseJSON = withObject "PSchema" $ \o -> PSchema
    <$> o .: "name"
    <*> o .: "modname"
    <*> o .: "meta"
    <*> o .: "fields"
    <*> o .: "i"

instance Pretty n => Pretty (PSchema n) where
  pretty PSchema{..} = parensSep
      [ "defschema"
      , pretty _psName
      , pretty _psMeta
      , prettyList _psFields
      ]

psModule :: Lens' (PSchema n) ModuleName
psModule = lens _psModule (\t b -> t { _psModule = b })

psMeta :: Lens' (PSchema n) Meta
psMeta = lens _psMeta (\t b -> t { _psMeta = b })

psInfo :: Lens' (PSchema n) Info
psInfo = lens _psInfo (\t b -> t { _psInfo = b })

data PTable n = PTable
  { _ptTableName :: !TableName
  , _ptModule :: ModuleName
  , _ptHash :: !ModuleHash
  , _ptTableType :: !(Type (Term n))
  , _ptMeta :: !Meta
  , _ptInfo :: !Info
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

ptModule :: Lens' (PTable n) ModuleName
ptModule = lens _ptModule (\t b -> t { _ptModule = b })

ptInfo :: Lens' (PTable n) Info
ptInfo = lens _ptInfo (\t b -> t { _ptInfo = b })

instance GetInfo (PTable n) where getInfo = _ptInfo
instance NFData n => NFData (PTable n)
instance (FromJSON n, ToJSON n) => ToJSON (PTable n) where
  toJSON PTable{..} = object
    [ "name" .= _ptTableName
    , "modname" .= _ptModule
    , "hash" .= _ptHash
    , "type" .= _ptTableType
    , "meta" .= _ptMeta
    , "i" .= _ptInfo
    ]
instance (FromJSON n, ToJSON n) => FromJSON (PTable n) where
  parseJSON = withObject "PTable" $ \o -> PTable
    <$> o .: "name"
    <*> o .: "modname"
    <*> o .: "hash"
    <*> o .: "type"
    <*> o .: "meta"
    <*> o .: "i"

-- | Pact evaluable term.
data Term n
  = TModule (ModuleDef (Term n)) !(Scope () Term n)
  | TList (PList n)
  | TDef !(Def n)
  | TNative (Native n)
  | TConst (PConst n)
  | TApp !(App (Term n))
  | TVar !n !Info
  | TBinding !(Binding n)
  | TObject !(Object n)
  | TSchema !(PSchema n)
  | TLiteral !Literal !Info
  | TGuard !Guard !Info
  | TUse !Use
  | TStep (Step (Term n))
  | TTable (PTable n)
  deriving (Functor,Foldable,Traversable,Generic)

deriving instance Show n => Show (Term n)
deriving instance Eq n => Eq (Term n)
instance NFData n => NFData (Term n)

instance GetInfo (Term n) where
  getInfo t = case t of
    TApp a -> getInfo a
    TBinding b -> getInfo b
    TConst c -> getInfo c
    TDef d -> getInfo d
    TGuard _ i -> i
    TList l -> getInfo l
    TLiteral _ i -> i
    TModule m _ -> getInfo m
    TNative n -> getInfo n
    TObject o -> getInfo o
    TSchema s -> getInfo s
    TStep s -> getInfo s
    TTable tt -> getInfo tt
    TUse u -> getInfo u
    TVar _ i -> i

instance Pretty n => Pretty (Term n) where
  pretty = \case
    TModule md _ -> pretty md
    TList l -> pretty l
    TDef d -> pretty d
    TNative n -> pretty n
    TConst c -> pretty c
    TApp a -> pretty a
    TVar n _ -> pretty n
    TBinding b -> pretty b
    TObject o -> pretty o
    TLiteral l _ -> annotate Val $ pretty l
    TGuard k _ -> pretty k
    TUse u -> pretty u
    TStep s -> pretty s
    TSchema s -> pretty s
    TTable PTable{..} -> parensSep
      [ "deftable"
      , pretty _ptTableName <> ":" <> pretty _ptTableType
      , pretty _ptMeta
      ]

instance Applicative Term where
    pure = return
    (<*>) = ap

instance Monad Term where
    return a = TVar a def
    TModule m b >>= f = TModule (fmap (>>= f) m) (b >>>= f)
    TList (PList l ts i) >>= f = TList $ PList (V.map (>>= f) l) (fmap (>>= f) ts) i
    TDef d >>= f = TDef $ d { _dFunType = (fmap (>>= f) $ _dFunType d), _dDefBody = ((_dDefBody d) >>>= f) }
    TNative n >>= f = TNative $ n { _nfFunTypes = (fmap (fmap (>>= f)) $ _nfFunTypes n) }
    TConst (PConst d m c t i) >>= f = TConst $ PConst (fmap (>>= f) d) m (fmap (>>= f) c) t i
    TApp a >>= f = TApp (fmap (>>= f) a)
    TVar n _ >>= f = f n
    TBinding (Binding bs b c i) >>= f =
      TBinding $ Binding (map (fmap (>>= f)) bs) (b >>>= f) (fmap (fmap (>>= f)) c) i
    TObject (Object bs t kf oi) >>= f = TObject (Object (fmap (>>= f) bs) (fmap (>>= f) t) kf oi)
    TStep (Step ent e r si) >>= f = TStep (Step (fmap (>>= f) ent) (e >>= f) (fmap (>>= f) r) si)
    TSchema ps >>= f = TSchema $ ps { _psFields = (fmap (fmap (>>= f)) $ _psFields ps) }
    TTable pt >>= f = TTable $ pt { _ptTableType = (fmap (>>= f) $ _ptTableType pt) }
    TLiteral l i >>= _ = TLiteral l i
    TGuard k i >>= _ = TGuard k i
    TUse u >>= _ = TUse u



termCodec :: (ToJSON n,FromJSON n) => Codec (Term n)
termCodec = Codec enc dec
  where
    enc t = case t of
      TModule md b -> object [ "module" .= md, "body" .= b ]
      TList l -> toJSON l
      TDef d -> toJSON d
      -- TODO native is all statically-declared
      TNative n -> toJSON n
      TConst pc -> toJSON pc
      TApp a -> toJSON a
      TVar v i -> object [ "var" .= v, "i" .= i ]
      TBinding b -> toJSON b
      TObject o -> toJSON o
      TLiteral l i -> object [ "lit" .= l, "i" .= i ]
      TGuard g i -> object [ "guard" .= g, "i" .= i ]
      TUse u -> toJSON u
      TStep s -> toJSON s
      TSchema s -> toJSON s
      TTable tt -> toJSON tt
    dec v =
      let
        k f = f <$> parseJSON v
        wo s p = withObject s p v
      in
        wo "TModule" (\o -> TModule <$> o .: "module" <*> o .: "body")
      <|> k TList
      <|> k TDef
      -- <|> k TNative
      <|> k TConst
      <|> k TApp
      <|> wo "TVar" (\o -> TVar <$> o .: "var" <*> o .: "i")
      <|> k TBinding
      <|> k TObject
      <|> wo "TLiteral" (\o -> TLiteral <$> o .: "lit" <*> o .: "i")
      <|> wo "TGuard" (\o -> TGuard <$> o .: "guard" <*> o .: "i")
      <|> k TUse
      <|> k TStep
      <|> k TSchema
      <|> k TTable


instance (ToJSON n, FromJSON n) => FromJSON (Term n) where
  parseJSON = decoder termCodec

instance (ToJSON n, FromJSON n) => ToJSON (Term n) where
  toJSON = encoder termCodec

class ToTerm a where
    toTerm :: a -> Term m
instance ToTerm Bool where toTerm = tLit . LBool
instance ToTerm Integer where toTerm = tLit . LInteger
instance ToTerm Int where toTerm = tLit . LInteger . fromIntegral
instance ToTerm Decimal where toTerm = tLit . LDecimal
instance ToTerm Text where toTerm = tLit . LString
instance ToTerm KeySet where toTerm k = TGuard (GKeySet k) def
instance ToTerm Guard where toTerm = (`TGuard` def)
instance ToTerm Literal where toTerm = tLit
instance ToTerm UTCTime where toTerm = tLit . LTime
instance ToTerm Word32 where toTerm = tLit . LInteger . fromIntegral
instance ToTerm Word64 where toTerm = tLit . LInteger . fromIntegral
instance ToTerm Int64 where toTerm = tLit . LInteger . fromIntegral


toTObject :: Type (Term n) -> Info -> [(FieldKey,Term n)] -> Term n
toTObject ty i = toTObjectMap ty i . ObjectMap . M.fromList

toTObjectMap :: Type (Term n) -> Info -> ObjectMap (Term n) -> Term n
toTObjectMap ty i m = TObject (Object m ty def i)

toTList :: Type (Term n) -> Info -> [Term n] -> Term n
toTList ty i = toTListV ty i . V.fromList

toTListV :: Type (Term n) -> Info -> V.Vector (Term n) -> Term n
toTListV ty i vs = TList $ PList vs ty i

toTermList :: (ToTerm a,Foldable f) => Type (Term b) -> f a -> Term b
toTermList ty l = TList $ PList (V.map toTerm (V.fromList (toList l))) ty def

guardTypeOf :: Guard -> GuardType
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
      TModule{} -> Left "module"
      TList l -> Right $ TyList $ _plListType l
      TDef d -> Left $ pack $ defTypeRep (_dDefType d)
      TNative{} -> Left "defun"
      TConst pc -> Left $ "const:" <> _aName (_pcConstArg pc)
      TApp{} -> Left "app"
      TVar{} -> Left "var"
      TBinding b -> case _bdType b of
        BindLet -> Left "let"
        BindSchema bt -> Right $ TySchema TyBinding bt def
      TObject o -> Right $ TySchema TyObject (_oObjectType o) def
      TGuard g _ -> Right $ TyPrim $ TyGuard $ Just $ guardTypeOf g
      TUse{} -> Left "use"
      TStep{} -> Left "step"
      TSchema ps -> Left $ "defobject:" <> asString (_psName ps)
      TTable pt -> Right $ TySchema TyTable (_ptTableType pt) def
{-# INLINE typeof #-}

-- | Return string type description.
typeof' :: Pretty a => Term a -> Text
typeof' = either id renderCompactText . typeof

pattern TLitString :: Text -> Term t
pattern TLitString s <- TLiteral (LString s) _
pattern TLitInteger :: Integer -> Term t
pattern TLitInteger i <- TLiteral (LInteger i) _
pattern TLitBool :: Bool -> Term t
pattern TLitBool b <- TLiteral (LBool b) _


tLit :: Literal -> Term n
tLit = (`TLiteral` def)
{-# INLINE tLit #-}

-- | Convenience for OverloadedStrings annoyances
tStr :: Text -> Term n
tStr = toTerm

-- | Equality dictionary for term-level equality
--
canEq :: Term n -> Term n -> Bool
canEq TList{} TList{} = True
canEq TObject{} TObject{} = True
canEq TLiteral{} TLiteral{} = True
canEq TTable{} TTable{} = True
canEq TSchema{} TSchema{} = True
canEq TGuard{} TGuard{} = True
canEq _ _ = False

-- | Support pact `=` for value-level terms
termEq :: Eq n => Term n -> Term n -> Bool
termEq (TList (PList l _ _)) (TList (PList l' _ _)) =
  length l == length l' && and (V.zipWith termEq l l')
termEq (TObject (Object (ObjectMap a) _ _ _)) (TObject (Object (ObjectMap b) _ _ _)) =
  -- O(3n), 2x M.toList + short circuiting walk
  M.size a == M.size b && go (M.toList a) (M.toList b) True
    where go _ _ False = False
          go ((k1,v1):r1) ((k2,v2):r2) _ = go r1 r2 $ k1 == k2 && v1 `termEq` v2
          go [] [] _ = True
          go _ _ _ = False
termEq (TLiteral a _) (TLiteral b _) = a == b
termEq (TGuard a _) (TGuard b _) = a == b
termEq (TTable (PTable a b c d x _)) (TTable (PTable e f g h y _)) =
  a == e && b == f && c == g && d == h && x == y
termEq (TSchema (PSchema a b c d _)) (TSchema (PSchema e f g h _)) =
  a == e && b == f && c == g && d == h
termEq _ _ = False


makePrisms ''Term
makePrisms ''Ref'
makePrisms ''DefType

makeLenses ''Meta
makeLenses ''App
makeLenses ''Def
makeLenses ''ModuleName

deriveEq1 ''Term
deriveEq1 ''BindPair
deriveEq1 ''App
deriveEq1 ''BindType
deriveEq1 ''ConstVal
deriveEq1 ''Def
deriveEq1 ''ModuleDef
deriveEq1 ''Module
deriveEq1 ''Governance
deriveEq1 ''ObjectMap
deriveEq1 ''Object
deriveEq1 ''Step
deriveEq1 ''PList
deriveEq1 ''PConst
deriveEq1 ''PSchema
deriveEq1 ''PTable
deriveEq1 ''Binding
deriveEq1 ''Native

deriveShow1 ''Term
deriveShow1 ''BindPair
deriveShow1 ''App
deriveShow1 ''ObjectMap
deriveShow1 ''Object
deriveShow1 ''BindType
deriveShow1 ''ConstVal
deriveShow1 ''Def
deriveShow1 ''ModuleDef
deriveShow1 ''Module
deriveShow1 ''Governance
deriveShow1 ''Step
deriveShow1 ''PList
deriveShow1 ''PConst
deriveShow1 ''PSchema
deriveShow1 ''PTable
deriveShow1 ''Binding
deriveShow1 ''Native

-- | Demonstrate Term/Bound JSON marshalling with nested bound and free vars.
_roundtripJSON :: String
_roundtripJSON | r == (Success tmod) = show r
               | otherwise = error ("Mismatch: " ++ show r ++ ", " ++ show tmod)
  where
    r = fromJSON v
    v = toJSON tmod

    tmod = TModule
           (MDModule (Module "foo" (Governance (Right (tStr "hi")))
                      def "" (ModuleHash pactInitialHash) HS.empty [] [] def))
           (abstract (const (Just ()))
            (toTList TyAny def
             [tlet1]))

    tlet1 = TBinding $ Binding []
           (abstract (\b -> if b == na then Just 0 else Nothing)
            (toTList TyAny def
             [(TVar na def),tlet2])) -- bound var + let
           BindLet def

    tlet2 = TBinding $ Binding []
           (abstract (\b -> if b == nb then Just 0 else Nothing)
            (toTList TyAny def
             [(TVar na def),(TVar nb def)])) -- free var + bound var
           BindLet def

    na = Name "a" def
    nb = Name "b" def
