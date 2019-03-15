{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      :  Pact.Types.Term
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Term and related types.
--

module Pact.Types.Term
 ( Namespace(..), nsName, nsGuard,
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
   FunApp(..),faDefType,faDocs,faInfo,faModule,faName,faTypes,
   Ref(..),_Direct,_Ref,
   NativeDFun(..),
   BindType(..),
   TableName(..),
   Module(..),mName,mGovernance,mMeta,mCode,mHash,mBlessed,mInterfaces,mImports,
   Interface(..),interfaceCode, interfaceMeta, interfaceName, interfaceImports,
   ModuleDef(..),_MDModule,_MDInterface,moduleDefName,moduleDefCode,moduleDefMeta,
   Governance(..),
   ModuleName(..), mnName, mnNamespace,
   Name(..),parseName,
   ConstVal(..),
   Use(..),
   App(..),appFun,appArgs,appInfo,
   Def(..),dDefBody,dDefName,dDefType,dMeta,dFunType,dInfo,dModule,
   Example(..),
   derefDef,
   Object(..),oObject,oObjectType,oInfo,
   FieldKey(..),
   Term(..),
   tApp,tBindBody,tBindPairs,tBindType,tConstArg,tConstVal,
   tDef,tMeta,tFields,tFunTypes,tHash,tInfo,tGuard,
   tListType,tList,tLiteral,tModuleBody,tModuleDef,tModule,tUse,
   tNativeDocs,tNativeFun,tNativeName,tNativeExamples,tNativeTopLevelOnly,tObject,tSchemaName,
   tStepEntity,tStepExec,tStepRollback,tTableName,tTableType,tValue,tVar,
   ToTerm(..),
   toTermList,toTObject,toTList,
   typeof,typeof',guardTypeOf,
   pattern TLitString,pattern TLitInteger,pattern TLitBool,
   tLit,tStr,termEq,
   Gas(..)
   ) where


import Control.Lens (makeLenses,makePrisms, (<&>))
import Control.Applicative
import Data.List
import Control.Monad
import Prelude
import Control.Arrow ((***),first)
import Data.Functor.Classes
import Bound
import Data.Text (Text,pack)
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Aeson hiding (pairs,Object)
import qualified Data.Aeson as A
import qualified Data.ByteString.UTF8 as BS
import Data.String
import Data.Default
import Data.Thyme
import GHC.Generics (Generic)
import Data.Decimal
import Data.Hashable
import Data.Foldable
import qualified Data.Attoparsec.Text as AP
import Text.Trifecta (ident,TokenParsing,(<?>),dot,eof)
import Control.DeepSeq
import Data.Maybe
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import Data.Int (Int64)
import Data.Serialize (Serialize)
import Data.Eq.Deriving
import Text.Show.Deriving
import Data.Word (Word64)


import Pact.Types.Parser
import Pact.Types.Pretty hiding (dot)
import Pact.Types.Util
import Pact.Types.Info
import Pact.Types.Type
import Pact.Types.Exp

data Meta = Meta
  { _mDocs  :: !(Maybe Text) -- ^ docs
  , _mModel :: ![Exp Info]   -- ^ models
  } deriving (Eq, Show, Generic)

instance Pretty Meta where
  pretty (Meta (Just doc) model) = pretty doc <> line <> prettyModel model
  pretty (Meta Nothing    model) = prettyModel model

prettyModel :: [Exp Info] -> Doc
prettyModel []    = mempty
prettyModel props = "@model " <> list (pretty <$> props)

instance ToJSON Meta where
  toJSON Meta {..} = object
    [ "docs" .= _mDocs, "model" .= toJSON (show <$> _mModel) ]

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

instance Pretty PublicKey where pretty (PublicKey s) = pretty (BS.toString s)

-- | KeySet pairs keys with a predicate function name.
data KeySet = KeySet {
      _ksKeys :: ![PublicKey]
    , _ksPredFun :: !Name
    } deriving (Eq,Generic,Show)

instance Pretty KeySet where
  pretty (KeySet ks f) = "KeySet" <+> commaBraces
    [ "keys: " <> pretty ks
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
    deriving (Eq,Ord,IsString,AsString,ToJSON,FromJSON,Show)

instance Pretty KeySetName where pretty (KeySetName s) = "'" <> pretty s

newtype PactId = PactId Word64
    deriving (Eq,Ord,Enum,Num,Real,Integral,Bounded,Default,FromJSON,ToJSON,Generic)
instance Show PactId where show (PactId p) = show p
instance Pretty PactId where pretty = viaShow
instance NFData PactId

data PactGuard = PactGuard
  { _pgPactId :: !PactId
  , _pgName :: !Text
  } deriving (Eq,Generic,Show)

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
  } deriving (Eq,Generic,Show)

instance Pretty ModuleGuard where
  pretty ModuleGuard{..} = "ModuleGuard" <+> commaBraces
    [ "module: " <> pretty _mgModuleName
    , "name: " <> pretty _mgName
    ]

instance ToJSON ModuleGuard where toJSON = lensyToJSON 3
instance FromJSON ModuleGuard where parseJSON = lensyParseJSON 3

data UserGuard = UserGuard
  { _ugData :: !(Term Name) -- TODO when Term is safe, use "object" type
  , _ugPredFun :: !Name
  } deriving (Eq,Generic,Show)

instance Pretty UserGuard where
  pretty UserGuard{..} = "UserGuard" <+> commaBraces
    [ "data: " <> pretty _ugData
    , "pred: " <> pretty _ugPredFun
    ]

instance ToJSON UserGuard where toJSON = lensyToJSON 3
instance FromJSON UserGuard where parseJSON = lensyParseJSON 3

data Guard
  = GPact PactGuard
  | GKeySet KeySet
  | GKeySetRef KeySetName
  | GModule ModuleGuard
  | GUser UserGuard
  deriving (Eq,Show)

instance Pretty Guard where
  pretty (GPact g)      = pretty g
  pretty (GKeySet g)    = pretty g
  pretty (GKeySetRef g) = pretty g
  pretty (GUser g)      = pretty g
  pretty (GModule g)    = pretty g

instance ToJSON Guard where
  toJSON (GPact g) = toJSON g
  toJSON (GKeySet g) = toJSON g
  toJSON (GKeySetRef g) = toJSON g
  toJSON (GUser g) = toJSON g
  toJSON (GModule m) = toJSON m

instance FromJSON Guard where
  parseJSON v =
    (GPact <$> parseJSON v) <|>
    (GKeySet <$> parseJSON v) <|>
    (GUser <$> parseJSON v) <|>
    (GKeySetRef <$> parseJSON v) <|>
    (GModule <$> parseJSON v)

data DefType
  = Defun
  | Defpact
  | Defcap
  deriving (Eq,Show,Generic)

defTypeRep :: DefType -> String
defTypeRep Defun = "defun"
defTypeRep Defpact = "defpact"
defTypeRep Defcap = "defcap"

instance Pretty DefType where
  pretty = pretty . defTypeRep

newtype NativeDefName = NativeDefName Text
    deriving (Eq,Ord,IsString,ToJSON,AsString,Show)

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
    } deriving Show

-- | Variable type for an evaluable 'Term'.
data Ref =
  -- | "Reduced" (evaluated) or native (irreducible) term.
  Direct (Term Name) |
  -- | Unevaulated/un-reduced term, never a native.
  Ref (Term Ref)
  deriving (Eq,Show)

instance Pretty Ref where
  pretty (Direct tm) = pretty tm
  pretty (Ref tm)    = pretty tm

-- | Gas compute cost unit.
newtype Gas = Gas Int64
  deriving (Eq,Ord,Num,Real,Integral,Enum,Show,ToJSON,FromJSON)

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

-- | Binding forms.
data BindType n =
  -- | Normal "let" bind
  BindLet |
  -- | Schema-style binding, with string value for key
  BindSchema { _bType :: n }
  deriving (Eq,Functor,Foldable,Traversable,Ord,Show)

instance (Pretty n) => Pretty (BindType n) where
  pretty BindLet = "let"
  pretty (BindSchema b) = "bind" <> pretty b

newtype TableName = TableName Text
    deriving (Eq,Ord,IsString,ToTerm,AsString,Hashable,Show)
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

instance ToJSON ModuleName where
  toJSON ModuleName{..} = object
    [ "name"      .= _mnName
    , "namespace" .= _mnNamespace
    ]

instance FromJSON ModuleName where
  parseJSON = withObject "ModuleName" $ \o -> ModuleName
    <$> o .:  "name"
    <*> o .:? "namespace"

newtype DefName = DefName Text
    deriving (Eq,Ord,IsString,ToJSON,FromJSON,AsString,Hashable,Pretty,Show)

-- | A named reference from source.
data Name
  = QName { _nQual :: ModuleName, _nName :: Text, _nInfo :: Info }
  | Name { _nName :: Text, _nInfo :: Info }
  deriving (Generic, Show)

instance Pretty Name where
  pretty = \case
    QName modName nName _ -> pretty modName <> "." <> pretty nName
    Name nName _          -> pretty nName

instance ToJSON Name where
  toJSON = toJSON . renderCompactString

instance FromJSON Name where
  parseJSON = withText "Name" $ \t -> case parseName def t of
    Left s  -> fail s
    Right n -> return n

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
  , _uModuleHash :: !(Maybe Hash)
  , _uInfo :: !Info
  } deriving (Eq, Show)

instance Pretty Use where
  pretty Use{..} =
    let args = pretty _uModuleName : maybe [] (\mh -> [pretty mh]) _uModuleHash
    in parensSep $ "use" : args

instance ToJSON Use where
  toJSON Use{..} =
    object $ ("module" .= _uModuleName) : (maybe [] (return . ("hash" .=)) _uModuleHash)
instance FromJSON Use where
  parseJSON = withObject "Use" $ \o ->
    Use <$> o .: "module"
        <*> o .:? "hash"
        <*> pure def

data App t = App
  { _appFun :: !t
  , _appArgs :: ![t]
  , _appInfo :: !Info
  } deriving (Functor,Foldable,Traversable,Eq,Show)

instance Pretty n => Pretty (App n) where
  pretty App{..} = parensSep $ pretty _appFun : map pretty _appArgs


newtype Governance g = Governance { _gGovernance :: Either KeySetName g }
  deriving (Eq,Ord,Functor,Foldable,Traversable,Show)

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


data Module g = Module
  { _mName :: !ModuleName
  , _mGovernance :: !(Governance g)
  , _mMeta :: !Meta
  , _mCode :: !Code
  , _mHash :: !Hash
  , _mBlessed :: !(HS.HashSet Hash)
  , _mInterfaces :: [ModuleName]
  , _mImports :: [Use]
  } deriving (Eq,Functor,Foldable,Traversable,Show)

instance Pretty g => Pretty (Module g) where
  pretty Module{..} = parensSep
    [ "module" , pretty _mName , pretty _mGovernance , pretty _mHash ]

instance ToJSON g => ToJSON (Module g) where
  toJSON Module{..} = object
    [ "name" .= _mName
    , "governance" .= _mGovernance
    , "meta" .= _mMeta
    , "code" .= _mCode
    , "hash" .= _mHash
    , "blessed" .= _mBlessed
    , "interfaces" .= _mInterfaces
    , "imports" .= _mImports
    ]

instance FromJSON g => FromJSON (Module g) where
  parseJSON = withObject "Module" $ \o -> Module
    <$> o .: "name"
    <*> o .: "governance"
    <*> pure (Meta Nothing []) {- o .:? "meta" -}
    <*> o .: "code"
    <*> o .: "hash"
    <*> (HS.fromList <$> o .: "blessed")
    <*> o .: "interfaces"
    <*> o .: "imports"

data Interface = Interface
  { _interfaceName :: !ModuleName
  , _interfaceCode :: !Code
  , _interfaceMeta :: !Meta
  , _interfaceImports :: [Use]
  } deriving (Eq,Show)
instance Pretty Interface where
  pretty Interface{..} = parensSep [ "interface", pretty _interfaceName ]

instance ToJSON Interface where
  toJSON Interface{..} = object
    [ "name" .= _interfaceName
    , "code" .= _interfaceCode
    , "meta" .= _interfaceMeta
    , "imports" .= _interfaceImports
    ]

instance FromJSON Interface where
  parseJSON = withObject "Interface" $ \o -> Interface
    <$> o .: "name"
    <*> o .: "code"
    <*> pure (Meta Nothing [])
    <*> o .: "imports"


data ModuleDef g
  = MDModule !(Module g)
  | MDInterface !Interface
 deriving (Eq,Functor,Foldable,Traversable,Show)

instance Pretty g => Pretty (ModuleDef g) where
  pretty = \case
    MDModule    m -> pretty m
    MDInterface i -> pretty i

instance ToJSON g => ToJSON (ModuleDef g) where
  toJSON (MDModule m) = toJSON m
  toJSON (MDInterface i) = toJSON i

instance FromJSON g => FromJSON (ModuleDef g) where
  parseJSON v = MDModule <$> parseJSON v <|> MDInterface <$> parseJSON v

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
  } deriving (Functor,Foldable,Traversable,Eq,Show)

instance Pretty n => Pretty (Def n) where
  pretty Def{..} = parensSep $
    [ pretty $ defTypeRep _dDefType
    , pretty _dModule <> "." <> pretty _dDefName <> ":" <> pretty (_ftReturn _dFunType)
    , parensSep $ pretty <$> _ftArgs _dFunType
    ] ++ maybe [] (\docs -> [pretty docs]) (_mDocs _dMeta)

derefDef :: Def Ref -> Name
derefDef Def{..} = QName _dModule (asString _dDefName) _dInfo



newtype NamespaceName = NamespaceName Text
  deriving (Eq, Ord, Show, FromJSON, ToJSON, IsString, AsString, Hashable, Pretty, Generic)

data Namespace = Namespace
  { _nsName   :: NamespaceName
  , _nsGuard  :: Guard
  } deriving (Eq, Show)

instance Pretty Namespace where
  pretty Namespace{..} = "(namespace " <> pretty (asString' _nsName) <> ")"

instance FromJSON Namespace where
  parseJSON = withObject "Namespace" $ \o -> Namespace
    <$> o .: "name"
    <*> o .: "guard"

instance ToJSON Namespace where
  toJSON Namespace{..} = object
    [ "name"   .= _nsName
    , "guard"  .= _nsGuard
    ]

data ConstVal n =
  CVRaw { _cvRaw :: !n } |
  CVEval { _cvRaw :: !n
         , _cvEval :: !n }
  deriving (Eq,Functor,Foldable,Traversable,Generic,Show)

-- Note: I believe this entire thing could be derived automatically if we go
-- rid of bound (@Scope@). We currently need this instance to satisfy the
-- 'Show' instance for 'Scope':
-- @(Show b, Show1 f, Show a) => Show (Scope b f a)@.
instance Show1 Term where
  liftShowsPrec :: forall a.
    (Int -> a -> ShowS) -> ([a] -> ShowS) -> Int -> Term a -> ShowS
  liftShowsPrec showsA showListA p tm = showParen (p > 10) $ case tm of
    TModule{..} ->
        showString "TModule "
      . shows2 11 _tModuleDef
      . showChar ' '
      . shows1 11 _tModuleBody
      . showChar ' '
      . showsPrec 11 _tInfo
    TList{..} ->
        showString "TList "
      . showList1 _tList
      . showChar ' '
      . shows2 11 _tListType
      . showChar ' '
      . showsPrec 11 _tInfo
    TDef{..} ->
        showString "TDef "
      . shows1 11 _tDef
      . showChar ' '
      . showsPrec 11 _tInfo
    TNative{..} ->
        showString "TNative "
      . showsPrec 11 _tNativeName
      . showChar ' '
      . showsPrec 11 _tNativeFun
      . showChar ' '
      . shows3 11 _tFunTypes
      . showChar ' '
      . showsPrec 11 _tNativeExamples
      . showChar ' '
      . showsPrec 11 _tNativeDocs
      . showChar ' '
      . showsPrec 11 _tNativeTopLevelOnly
      . showChar ' '
      . showsPrec 11 _tInfo
    TConst{..} ->
        showString "TConst "
      . shows2 11 _tConstArg
      . showChar ' '
      . showsPrec 11 _tModule
      . showChar ' '
      . shows2 11 _tConstVal
      . showChar ' '
      . showsPrec 11 _tMeta
      . showChar ' '
      . showsPrec 11 _tInfo
    TApp{..} ->
        showString "TApp "
      . shows2 11 _tApp
      . showChar ' '
      . showsPrec 11 _tInfo
    TVar{..} ->
        showString "TVar "
      . showsA 11 _tVar
      . showChar ' '
      . showsPrec 11 _tInfo
    TBinding{..} ->
        showString "TBinding"
      . liftShowList2 shows2 showList2 shows1 showList1 _tBindPairs
      . shows1 11 _tBindBody
      . shows3 11 _tBindType
      . showsPrec 11 _tInfo
    TObject{..} ->
        showString "TObject"
      . shows1 11 _tObject
      . showChar ' '
      . showsPrec 11 _tInfo
    TSchema{..} ->
        showString "TSchema "
      . showsPrec 11 _tSchemaName
      . showChar ' '
      . showsPrec 11 _tModule
      . showChar ' '
      . showsPrec 11 _tMeta
      . showChar ' '
      . showList2 _tFields
      . showChar ' '
      . showsPrec 11 _tInfo
    TLiteral{..} ->
        showString "TLiteral "
      . showsPrec 11 _tLiteral
      . showChar ' '
      . showsPrec 11 _tInfo
    TGuard{..} ->
        showString "TGuard "
      . showsPrec 11 _tGuard
      . showChar ' '
      . showsPrec 11 _tInfo
    TUse{..} ->
        showString "TUse "
      . showsPrec 11 _tUse
      . showChar ' '
      . showsPrec 11 _tInfo
    TValue{..} ->
        showString "TValue "
      . showsPrec 11 _tValue
      . showChar ' '
      . showsPrec 11 _tInfo
    TStep{..} ->
        showString "TStep "
      . shows2 11 _tStepEntity
      . shows1 11 _tStepExec
      . shows2 11 _tStepRollback
      . showsPrec 11 _tInfo
    TTable{..} ->
        showString "TTable "
      . showsPrec 11 _tTableName
      . showChar ' '
      . showsPrec 11 _tModule
      . showChar ' '
      . showsPrec 11 _tHash
      . showChar ' '
      . shows2
        11 _tTableType
      . showChar ' '
      . showsPrec 11 _tMeta
      . showChar ' '
      . showsPrec 11 _tInfo
    where shows1 :: Show1 f => Int -> f a -> ShowS
          shows1 = liftShowsPrec showsA showListA
          showList1 :: Show1 f => [f a] -> ShowS
          showList1 = liftShowList showsA showListA
          shows2 :: (Show1 f, Show1 g) => Int -> f (g a) -> ShowS
          shows2 = liftShowsPrec shows1 showList1
          showList2 :: (Show1 f, Show1 g) => [f (g a)] -> ShowS
          showList2 = liftShowList shows1 showList1
          shows3 :: (Show1 f, Show1 g, Show1 h) => Int -> f (g (h a)) -> ShowS
          shows3 = liftShowsPrec shows2 showList2

data Example
  = ExecExample !Text
  -- ^ An example shown as a good execution
  | ExecErrExample !Text
  -- ^ An example shown as a failing execution
  | LitExample !Text
  -- ^ An example shown as a literal
  deriving (Eq, Show)

instance Pretty Example where
  pretty = \case
    ExecExample    str -> annotate Example    $ "> " <> pretty str
    ExecErrExample str -> annotate BadExample $ "> " <> pretty str
    LitExample     str -> annotate Example    $ pretty str

instance IsString Example where
  fromString = ExecExample . fromString

newtype FieldKey = FieldKey Text
  deriving (Eq,Ord,IsString,AsString,ToJSON,FromJSON,Show)
instance Pretty FieldKey where
  pretty (FieldKey k) = dquotes $ pretty k

data Object n = Object
  { _oObject :: ![(FieldKey,Term n)]
  , _oObjectType :: !(Type (Term n))
  , _oInfo :: !Info
  } deriving (Functor,Foldable,Traversable,Eq,Show)
instance Pretty n => Pretty (Object n) where
  pretty (Object bs _ _) = annotate Val $ commaBraces $
      fmap (\(a, b) -> pretty a <> ": " <> pretty b) bs

-- | Pact evaluable term.
data Term n =
    TModule {
      _tModuleDef :: ModuleDef (Term n)
    , _tModuleBody :: !(Scope () Term n)
    , _tInfo :: !Info
    } |
    TList {
      _tList :: ![Term n]
    , _tListType :: Type (Term n)
    , _tInfo :: !Info
    } |
    TDef {
      _tDef :: Def n
    , _tInfo :: !Info
    } |
    TNative {
      _tNativeName :: !NativeDefName
    , _tNativeFun :: !NativeDFun
    , _tFunTypes :: FunTypes (Term n)
    , _tNativeExamples :: ![Example]
    , _tNativeDocs :: Text
    , _tNativeTopLevelOnly :: Bool
    , _tInfo :: !Info
    } |
    TConst {
      _tConstArg :: !(Arg (Term n))
    , _tModule :: !ModuleName
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
      _tBindPairs :: ![(Arg (Term n),Term n)]
    , _tBindBody :: !(Scope Int Term n)
    , _tBindType :: BindType (Type (Term n))
    , _tInfo :: !Info
    } |
    TObject {
      _tObject :: !(Object n)
    , _tInfo :: !Info
    } |
    TSchema {
      _tSchemaName :: !TypeName
    , _tModule :: !ModuleName
    , _tMeta :: !Meta
    , _tFields :: ![Arg (Term n)]
    , _tInfo :: !Info
    } |
    TLiteral {
      _tLiteral :: !Literal
    , _tInfo :: !Info
    } |
    TGuard {
      _tGuard :: !Guard
    , _tInfo :: !Info
    } |
    TUse {
      _tUse :: Use
    , _tInfo :: Info
    } |
    TValue {
      _tValue :: !Value
    , _tInfo :: !Info
    } |
    TStep {
      _tStepEntity :: !(Maybe (Term n))
    , _tStepExec :: !(Term n)
    , _tStepRollback :: !(Maybe (Term n))
    , _tInfo :: !Info
    } |
    TTable {
      _tTableName :: !TableName
    , _tModule :: ModuleName
    , _tHash :: !Hash
    , _tTableType :: !(Type (Term n))
    , _tMeta :: !Meta
    , _tInfo :: !Info
    }
    deriving (Functor,Foldable,Traversable,Eq,Show)

instance Pretty n => Pretty (Term n) where
  pretty = \case
    TModule{..} -> pretty _tModuleDef
    TList{..} -> bracketsSep $ pretty <$> _tList
    TDef{..} -> pretty _tDef
    TNative{..} -> annotate Header ("native `" <> pretty _tNativeName <> "`")
      <> nest 2 (
         line
      <> line <> fillSep (pretty <$> T.words _tNativeDocs)
      <> examples
      <> line
      <> line <> annotate Header "Type:"
      <> line <> align (vsep (pretty <$> toList _tFunTypes))
      ) where examples = case _tNativeExamples of
                [] -> mempty
                exs ->
                     line <> line <> annotate Header "Examples:"
                  <> line <> align (vsep (pretty <$> exs))
    TConst{..} -> "constant " <> pretty _tModule <> "." <> pretty _tConstArg
      <> " " <> pretty _tMeta
    TApp a _ -> pretty a
    TVar n _ -> pretty n
    TBinding pairs body BindLet _i -> parensSep
      [ "let"
      , parensSep $ pairs <&> \(arg, body') -> pretty arg <+> pretty body'
      , pretty $ unscope body
      ]
    TBinding pairs body (BindSchema _) _i -> parensSep
      [ commaBraces $ pairs <&> \(arg, body') -> pretty arg <+> pretty body'
      , pretty $ unscope body
      ]
    TObject o _ -> pretty o
    TLiteral l _ -> annotate Val $ pretty l
    TGuard k _ -> pretty k
    TUse u _ -> pretty u
    TValue v _ -> annotate Val $ pretty v
    TStep mEntity exec Nothing _i -> parensSep $
      [ "step"
      ] ++ maybe [] (\entity -> [pretty entity]) mEntity ++
      [ pretty exec
      ]
    TStep mEntity exec (Just rollback) _i -> parensSep $
      [ "step-with-rollback"
      ] ++ maybe [] (\entity -> [pretty entity]) mEntity ++
      [ pretty exec
      , pretty rollback
      ]
    TSchema{..} -> parensSep
      [ "defschema"
      , pretty _tSchemaName
      , pretty _tMeta
      , pretty _tFields
      ]
    TTable{..} -> parensSep
      [ "deftable"
      , pretty _tTableName <> ":" <> pretty _tTableType
      , pretty _tMeta
      ]

-- We currently need this instance to satisfy the 'Eq instance for 'Scope':
-- @(Monad f, Eq b, Eq1 f) => Eq1 (Scope b f)@
instance Eq1 Term where
  liftEq eq (TModule a b c) (TModule m n o) =
    liftEq (liftEq eq) a m && liftEq eq b n && c == o
  liftEq eq (TList a b c) (TList m n o) =
    liftEq (liftEq eq) a m && liftEq (liftEq eq) b n && c == o
  liftEq eq (TDef (Def a b c d e f g) i) (TDef (Def m n o p q r s) t) =
    a == m && b == n && c == o && liftEq (liftEq eq) d p && liftEq eq e q && f == r && g == s && i == t
  liftEq eq (TConst a b c d e) (TConst m n o q r) =
    liftEq (liftEq eq) a m && b == n && liftEq (liftEq eq) c o && d == q && e == r
  liftEq eq (TApp (App a b c) d) (TApp (App m n o) p) =
    liftEq eq a m && liftEq (liftEq eq) b n && c == o && d == p
  liftEq eq (TVar a b) (TVar m n) =
    eq a m && b == n
  liftEq eq (TBinding a b c d) (TBinding m n o p) =
    liftEq (\(w,x) (y,z) -> liftEq (liftEq eq) w y && liftEq eq x z) a m &&
    liftEq eq b n && liftEq (liftEq (liftEq eq)) c o && d == p
  liftEq eq (TObject (Object a b c) d) (TObject (Object m n o) p) =
    liftEq (\(w,x) (y,z) -> w == y && liftEq eq x z) a m && liftEq (liftEq eq) b n && c == o && d == p
  liftEq _ (TLiteral a b) (TLiteral m n) =
    a == m && b == n
  liftEq _ (TGuard a b) (TGuard m n) =
    a == m && b == n
  liftEq _ (TUse a b) (TUse m n) =
    a == m && b == n
  liftEq _ (TValue a b) (TValue m n) =
    a == m && b == n
  liftEq eq (TStep a b c d) (TStep m n o p) =
    liftEq (liftEq eq) a m && liftEq eq b n && liftEq (liftEq eq) c o && d == p
  liftEq eq (TSchema a b c d e) (TSchema m n o p q) =
    a == m && b == n && c == o && liftEq (liftEq (liftEq eq)) d p && e == q
  liftEq eq (TTable a b c d e f) (TTable m n o p q r) =
    a == m && b == n && c == o && liftEq (liftEq eq) d p && e == q && f == r
  liftEq _ _ _ = False


instance Applicative Term where
    pure = return
    (<*>) = ap

instance Monad Term where
    return a = TVar a def
    TModule m b i >>= f = TModule (fmap (>>= f) m) (b >>>= f) i
    TList bs t i >>= f = TList (map (>>= f) bs) (fmap (>>= f) t) i
    TDef (Def n m dt ft b d i) i' >>= f = TDef (Def n m dt (fmap (>>= f) ft) (b >>>= f) d i) i'
    TNative n fn t exs d tl i >>= f = TNative n fn (fmap (fmap (>>= f)) t) exs d tl i
    TConst d m c t i >>= f = TConst (fmap (>>= f) d) m (fmap (>>= f) c) t i
    TApp a i >>= f = TApp (fmap (>>= f) a) i
    TVar n i >>= f = (f n) { _tInfo = i }
    TBinding bs b c i >>= f = TBinding (map (fmap (>>= f) *** (>>= f)) bs) (b >>>= f) (fmap (fmap (>>= f)) c) i
    TObject (Object bs t oi) i >>= f = TObject (Object (map (id *** (>>= f)) bs) (fmap (>>= f) t) oi) i
    TLiteral l i >>= _ = TLiteral l i
    TGuard k i >>= _ = TGuard k i
    TUse u i >>= _ = TUse u i
    TValue v i >>= _ = TValue v i
    TStep ent e r i >>= f = TStep (fmap (>>= f) ent) (e >>= f) (fmap (>>= f) r) i
    TSchema {..} >>= f = TSchema _tSchemaName _tModule _tMeta (fmap (fmap (>>= f)) _tFields) _tInfo
    TTable {..} >>= f = TTable _tTableName _tModule _tHash (fmap (>>= f) _tTableType) _tMeta _tInfo


instance FromJSON (Term n) where
    parseJSON (Number n) = return $ TLiteral (LInteger (round n)) def
    parseJSON (Bool b) = return $ toTerm b
    parseJSON (String s) = return $ toTerm s
    parseJSON (Array a) = toTList TyAny def . toList <$> mapM parseJSON a
    parseJSON (A.Object o) = toTObject TyAny def <$> mapM (traverse parseJSON . first FieldKey) (HM.toList o)
    parseJSON v = return $ toTerm v
    {-# INLINE parseJSON #-}

instance Pretty n => ToJSON (Term n) where
    toJSON (TLiteral l _) = toJSON l
    toJSON (TValue v _) = v
    toJSON (TGuard k _) = toJSON k
    toJSON (TObject (Object kvs _ _) _) =
        object $ map (asString *** toJSON) kvs
    toJSON (TList ts _ _) = toJSON ts
    toJSON t = toJSON (renderCompactText t)
    {-# INLINE toJSON #-}

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
instance ToTerm Value where toTerm = (`TValue` def)
instance ToTerm UTCTime where toTerm = tLit . LTime
instance ToTerm PactId where toTerm = tLit . LInteger . fromIntegral


toTObject :: Type (Term n) -> Info -> [(FieldKey,Term n)] -> Term n
toTObject ty i ps = TObject (Object ps ty i) i

toTList :: Type (Term n) -> Info -> [Term n] -> Term n
toTList ty i vs = TList vs ty i

toTermList :: (ToTerm a,Foldable f) => Type (Term b) -> f a -> Term b
toTermList ty l = TList (map toTerm (toList l)) ty def

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
      TModule {} -> Left "module"
      TList {..} -> Right $ TyList _tListType
      TDef {..} -> Left $ pack $ defTypeRep (_dDefType _tDef)
      TNative {..} -> Left "defun"
      TConst {..} -> Left $ "const:" <> _aName _tConstArg
      TApp {..} -> Left "app"
      TVar {..} -> Left "var"
      TBinding {..} -> case _tBindType of
        BindLet -> Left "let"
        BindSchema bt -> Right $ TySchema TyBinding bt def
      TObject (Object {..}) _ -> Right $ TySchema TyObject _oObjectType def
      TGuard {..} -> Right $ TyPrim $ TyGuard $ Just $ guardTypeOf _tGuard
      TUse {} -> Left "use"
      TValue {} -> Right $ TyPrim TyValue
      TStep {} -> Left "step"
      TSchema {..} -> Left $ "defobject:" <> asString _tSchemaName
      TTable {..} -> Right $ TySchema TyTable _tTableType def
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

-- | Support pact `=` for value-level terms
termEq :: Eq n => Term n -> Term n -> Bool
termEq (TList a _ _) (TList b _ _) = length a == length b && and (zipWith termEq a b)
termEq (TObject (Object a _ _) _) (TObject (Object b _ _) _) = length a == length b && all (lkpEq b) a
    where lkpEq [] _ = False
          lkpEq ((k',v'):ts) p@(k,v) | k == k' && termEq v v' = True
                                     | otherwise = lkpEq ts p
termEq (TLiteral a _) (TLiteral b _) = a == b
termEq (TGuard a _) (TGuard b _) = a == b
termEq (TValue a _) (TValue b _) = a == b
termEq (TTable a b c d x _) (TTable e f g h y _) = a == e && b == f && c == g && d == h && x == y
termEq (TSchema a b c d _) (TSchema e f g h _) = a == e && b == f && c == g && d == h
termEq _ _ = False


makeLenses ''Term
makeLenses ''Namespace
makeLenses ''FunApp
makePrisms ''Ref
makeLenses ''Meta
makeLenses ''Module
makeLenses ''Interface
makePrisms ''ModuleDef
makeLenses ''App
makeLenses ''Def
makeLenses ''ModuleName
makePrisms ''DefType
makeLenses ''Object

deriveEq1 ''App
deriveEq1 ''BindType
deriveEq1 ''ConstVal
deriveEq1 ''Def
deriveEq1 ''ModuleDef
deriveEq1 ''Module
deriveEq1 ''Governance
deriveEq1 ''Object

deriveShow1 ''App
deriveShow1 ''Object
deriveShow1 ''BindType
deriveShow1 ''ConstVal
deriveShow1 ''Def
deriveShow1 ''ModuleDef
deriveShow1 ''Module
deriveShow1 ''Governance
