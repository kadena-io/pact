{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
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
   DefType(..),
   defTypeRep,
   NativeDefName(..),DefName(..),
   FunApp(..),faDefType,faDocs,faInfo,faModule,faName,faTypes,
   Ref(..),
   NativeDFun(..),
   BindType(..),
   TableName(..),
   Module(..),mName,mKeySet,mMeta,mCode,mHash,mBlessed,mInterfaces,mImports,
   interfaceCode, interfaceMeta, interfaceName, interfaceImports,
   ModuleName(..), mnName, mnNamespace,
   Name(..),parseName,
   ConstVal(..),
   Use(..),
   App(..),appFun,appArgs,appInfo,
   Def(..),dDefBody,dDefName,dDefType,dMeta,dFunType,dInfo,dModule,
   Term(..),
   tApp,tBindBody,tBindPairs,tBindType,tConstArg,tConstVal,
   tDef,tMeta,tFields,tFunTypes,tHash,tInfo,tGuard,
   tListType,tList,tLiteral,tModuleBody,tModuleDef,tModule,tUse,
   tNativeDocs,tNativeFun,tNativeName,tNativeTopLevelOnly,tObjectType,tObject,tSchemaName,
   tStepEntity,tStepExec,tStepRollback,tTableName,tTableType,tValue,tVar,
   ToTerm(..),
   toTermList,toTObject,toTList,
   typeof,typeof',guardTypeOf,
   pattern TLitString,pattern TLitInteger,pattern TLitBool,
   tLit,tStr,termEq,abbrev,
   Gas(..)
   ) where


import Control.Lens (makeLenses)
import Control.Applicative
import Data.List
import Control.Monad
import Prelude
import Control.Arrow ((***),first)
import Data.Functor.Classes
import Bound
import Data.Text (Text,pack,unpack)
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Aeson
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString.Lazy.UTF8 as BSL
import Data.String
import Data.Default
import Data.Thyme
import GHC.Generics (Generic)
import Data.Decimal
import Data.Hashable
import Data.Foldable
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import Text.PrettyPrint.ANSI.Leijen hiding ((<>),(<$>),dot)
import qualified Data.Attoparsec.Text as AP
import Text.Trifecta (ident,TokenParsing,(<?>),dot,eof)
import Control.DeepSeq
import Data.Maybe
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import Data.Int (Int64)
import Data.Serialize (Serialize)


import Pact.Types.Parser
import Pact.Types.Util
import Pact.Types.Info
import Pact.Types.Type
import Pact.Types.Exp


data Meta = Meta
  { _mDocs  :: !(Maybe Text) -- ^ docs
  , _mModel :: ![Exp Info]   -- ^ models
  } deriving (Eq, Show, Generic)

instance ToJSON Meta where
  toJSON Meta {..} = object
    [ "docs" .= _mDocs, "model" .= toJSON (show <$> _mModel) ]

instance Default Meta where def = Meta def def

instance Semigroup Meta where
  (Meta d m) <> (Meta d' m') = Meta (d <> d') (m <> m')

instance Monoid Meta where
  mempty = Meta Nothing []

newtype PublicKey = PublicKey { _pubKey :: BS.ByteString }
  deriving (Eq,Ord,Generic,IsString,AsString)

instance Serialize PublicKey
instance NFData PublicKey
instance FromJSON PublicKey where
  parseJSON = withText "PublicKey" (return . PublicKey . encodeUtf8)
instance ToJSON PublicKey where
  toJSON = toJSON . decodeUtf8 . _pubKey
instance Show PublicKey where show (PublicKey s) = show (BS.toString s)

-- | KeySet pairs keys with a predicate function name.
data KeySet = KeySet {
      _ksKeys :: ![PublicKey]
    , _ksPredFun :: !Name
    } deriving (Eq,Generic)
instance Show KeySet where
  show (KeySet ks f) =
    "KeySet { keys: " ++ show ks ++ ", pred: " ++ show f ++ " }"

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
    deriving (Eq,Ord,IsString,AsString,ToJSON,FromJSON)
instance Show KeySetName where show (KeySetName s) = show s

newtype PactId = PactId Text
    deriving (Eq,Ord,IsString,ToTerm,AsString,ToJSON,FromJSON,Default)
instance Show PactId where show (PactId s) = show s

data PactGuard = PactGuard
  { _pgPactId :: !PactId
  , _pgName :: !Text
  } deriving (Eq,Generic)

instance Show PactGuard where
  show PactGuard{..} =
    "PactGuard { pactId: " ++ show _pgPactId ++ ", name: " ++ show _pgName ++ "}"

instance ToJSON PactGuard where toJSON = lensyToJSON 3
instance FromJSON PactGuard where parseJSON = lensyParseJSON 3

data ModuleGuard = ModuleGuard
  { _mgModuleName :: !ModuleName
  , _mgName :: !Text
  } deriving (Eq,Generic)

instance Show ModuleGuard where
  show ModuleGuard{..} =
    "ModuleGuard { module: " ++ show _mgModuleName ++ ", name: " ++ show _mgName ++ "}"

instance ToJSON ModuleGuard where toJSON = lensyToJSON 3
instance FromJSON ModuleGuard where parseJSON = lensyParseJSON 3

data UserGuard = UserGuard
  { _ugData :: !(Term Name) -- TODO when Term is safe, use "object" type
  , _ugPredFun :: !Name
  } deriving (Eq,Generic)

instance Show UserGuard where
  show UserGuard{..} =
    "UserGuard { data: " ++ show _ugData ++ ", pred: " ++ show _ugPredFun ++ "}"

instance ToJSON UserGuard where toJSON = lensyToJSON 3
instance FromJSON UserGuard where parseJSON = lensyParseJSON 3

data Guard
  = GPact PactGuard
  | GKeySet KeySet
  | GKeySetRef KeySetName
  | GModule ModuleGuard
  | GUser UserGuard
  deriving (Eq)

instance Show Guard where
  show (GPact g) = show g
  show (GKeySet g) = show g
  show (GKeySetRef g) = show g
  show (GUser g) = show g
  show (GModule g) = show g

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

newtype NativeDefName = NativeDefName Text
    deriving (Eq,Ord,IsString,ToJSON,AsString)
instance Show NativeDefName where show (NativeDefName s) = show s

-- | Capture function application metadata
data FunApp = FunApp {
      _faInfo :: !Info
    , _faName :: !Text
    , _faModule :: !(Maybe ModuleName)
    , _faDefType :: !DefType
    , _faTypes :: !(FunTypes (Term Name))
    , _faDocs :: !(Maybe Text)
    }

instance Show FunApp where
  show FunApp {..} =
    "(" ++ defTypeRep _faDefType ++ " " ++ maybeDelim "." _faModule ++
    unpack _faName ++ " " ++ showFunTypes _faTypes ++ ")"

-- | Variable type for an evaluable 'Term'.
data Ref =
  -- | "Reduced" (evaluated) or native (irreducible) term.
  Direct (Term Name) |
  -- | Unevaulated/un-reduced term, never a native.
  Ref (Term Ref)
               deriving (Eq)
instance Show Ref where
    show (Direct t) = abbrev t
    show (Ref t) = abbrev t

-- | Gas compute cost unit.
newtype Gas = Gas Int64
  deriving (Eq,Ord,Num,Real,Integral,Enum)
instance Show Gas where show (Gas g) = show g

instance Semigroup Gas where
  (Gas a) <> (Gas b) = Gas $ a + b

instance Monoid Gas where
  mempty = 0

data NativeDFun = NativeDFun
  { _nativeName :: NativeDefName
  , _nativeFun :: forall m . Monad m => FunApp -> [Term Ref] -> m (Gas,Term Name)
  }

instance Eq NativeDFun where a == b = _nativeName a == _nativeName b
instance Show NativeDFun where show a = show $ _nativeName a

-- | Binding forms.
data BindType n =
  -- | Normal "let" bind
  BindLet |
  -- | Schema-style binding, with string value for key
  BindSchema { _bType :: n }
  deriving (Eq,Functor,Foldable,Traversable,Ord)
instance (Show n) => Show (BindType n) where
  show BindLet = "let"
  show (BindSchema b) = "bind" ++ show b
instance (Pretty n) => Pretty (BindType n) where
  pretty BindLet = "let"
  pretty (BindSchema b) = "bind" PP.<> pretty b

instance Eq1 BindType where
  liftEq _ BindLet BindLet = True
  liftEq eq (BindSchema a) (BindSchema m) = eq a m
  liftEq _ _ _ = False

newtype TableName = TableName Text
    deriving (Eq,Ord,IsString,ToTerm,AsString,Hashable)
instance Show TableName where show (TableName s) = show s

-- TODO: We need a more expressive ADT that can handle modules _and_ interfaces
-- DESNote(emily): -XDerivingVia would eliminate many of these instances when we bump
data ModuleName = ModuleName
  { _mnName      :: Text
  , _mnNamespace :: Maybe NamespaceName
  } deriving (Eq, Ord, Generic)

instance Hashable ModuleName where
  hashWithSalt s (ModuleName n Nothing)   =
    s `hashWithSalt` (0::Int) `hashWithSalt` n
  hashWithSalt s (ModuleName n (Just ns)) =
    s `hashWithSalt` (1::Int) `hashWithSalt` n `hashWithSalt` ns

instance Show ModuleName where
  show = unpack . asString

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
  pretty (ModuleName n Nothing) = pretty n
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
    deriving (Eq,Ord,IsString,ToJSON,FromJSON,AsString,Hashable,Pretty)
instance Show DefName where show (DefName s) = show s

-- | A named reference from source.
data Name
  = QName { _nQual :: ModuleName, _nName :: Text, _nInfo :: Info }
  | Name { _nName :: Text, _nInfo :: Info }
  deriving (Generic)

instance Show Name where
  show (QName q n _) = asString' q ++ "." ++ unpack n
  show (Name n _) = unpack n
instance ToJSON Name where toJSON = toJSON . show
instance FromJSON Name where
  parseJSON = withText "Name" $ \t -> case parseName def t of
    Left s -> fail s
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
  } deriving (Eq,Generic)
instance Show Use where
  show Use {..} = "(use " ++ show _uModuleName ++ maybeDelim " " _uModuleHash ++ ")"

data App t = App
  { _appFun :: !t
  , _appArgs :: ![t]
  , _appInfo :: !Info
  } deriving (Functor,Foldable,Traversable,Eq)
instance Show n => Show (App n) where
  show App{..} = "(" ++ unwords (show _appFun:map show _appArgs) ++ ")"

-- TODO: We need a more expressive, safer ADT for this.
data Module
  = Module
  { _mName :: !ModuleName
  , _mKeySet :: !KeySetName
  , _mMeta :: !Meta
  , _mCode :: !Code
  , _mHash :: !Hash
  , _mBlessed :: !(HS.HashSet Hash)
  , _mInterfaces :: [ModuleName]
  , _mImports :: [Use]
  }
  | Interface
  { _interfaceName :: !ModuleName
  , _interfaceCode :: !Code
  , _interfaceMeta :: !Meta
  , _interfaceImports :: [Use]
  } deriving Eq

instance Show Module where
  show m = case m of
    Module{..} -> "(Module " ++ asString' _mName ++ " '" ++ asString' _mKeySet ++ " " ++ show _mHash ++ ")"
    Interface{..} -> "(Interface " ++ asString' _interfaceName ++ ")"

instance ToJSON Module where
  toJSON Module{..} = object
    [ "name" .= _mName
    , "keyset" .= _mKeySet
    , "meta" .= _mMeta
    , "code" .= _mCode
    , "hash" .= _mHash
    , "blessed" .= _mBlessed
    , "interfaces" .= _mInterfaces
    ]
  toJSON Interface{..} = object
    [ "name" .= _interfaceName
    , "code" .= _interfaceCode
    , "meta" .= _interfaceMeta
    ]

instance FromJSON Module where
  parseJSON = withObject "Module" $ \o -> Module
    <$> o .: "name"
    <*> o .: "keyset"
    <*> pure (Meta Nothing []) {- o .:? "meta" -}
    <*> o .: "code"
    <*> o .: "hash"
    <*> (HS.fromList <$> o .: "blessed")
    <*> o .: "interfaces"
    <*> pure []

data Def n = Def
  { _dDefName :: !DefName
  , _dModule :: !ModuleName
  , _dDefType :: !DefType
  , _dFunType :: !(FunType (Term n))
  , _dDefBody :: !(Scope Int Term n)
  , _dMeta :: !Meta
  , _dInfo :: !Info
  } deriving (Functor,Foldable,Traversable,Eq)
instance (Show n) => Show (Def n) where
  show Def{..} = "(" ++ unwords
    [ defTypeRep _dDefType
    , asString' _dModule ++ "." ++ asString' _dDefName ++ ":" ++ show (_ftReturn _dFunType)
    , "(" ++ unwords (map show (_ftArgs _dFunType)) ++ ")"] ++
    maybeDelim " " (_mDocs _dMeta) ++ ")"

newtype NamespaceName = NamespaceName Text
  deriving (Eq, Ord, Show, FromJSON, ToJSON, IsString, AsString, Hashable, Pretty, Generic)

data Namespace = Namespace
  { _nsName   :: NamespaceName
  , _nsGuard  :: Guard
  } deriving Eq

instance Show Namespace where
  show Namespace{..} = "(namespace " ++ asString' _nsName ++ ")"

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
  deriving (Eq,Functor,Foldable,Traversable,Generic)

instance Show o => Show (ConstVal o) where
  show (CVRaw r) = show r
  show (CVEval _ e) = show e

instance Eq1 ConstVal where
  liftEq eq (CVRaw a) (CVRaw b) = eq a b
  liftEq eq (CVEval a c) (CVEval b d) = eq a b && eq c d
  liftEq _ _ _ = False

-- | Pact evaluable term.
data Term n =
    TModule {
      _tModuleDef :: Module
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
      _tObject :: ![(Term n,Term n)]
    , _tObjectType :: !(Type (Term n))
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
    deriving (Functor,Foldable,Traversable,Eq)

instance Show n => Show (Term n) where
    show TModule {..} =
      "(TModule " ++ show _tModuleDef ++ " " ++ show (unscope _tModuleBody) ++ ")"
    show (TList bs _ _) = "[" ++ unwords (map show bs) ++ "]"
    show TDef {..} = show _tDef
    show TNative {..} =
      "(TNative " ++ asString' _tNativeName ++ " " ++ showFunTypes _tFunTypes ++ " " ++ unpack _tNativeDocs ++ ")"
    show TConst {..} =
      "(TConst " ++ asString' _tModule ++ "." ++ show _tConstArg ++ " " ++ show _tMeta ++ ")"
    show (TApp a _) = show a
    show (TVar n _) = "(TVar " ++ show n ++ ")"
    show (TBinding bs b c _) = "(TBinding " ++ show bs ++ " " ++ show (unscope b) ++ " " ++ show c ++ ")"
    show (TObject bs _ _) =
      "{" ++ intercalate ", " (map (\(a,b) -> show a ++ ": " ++ show b) bs) ++ "}"
    show (TLiteral l _) = show l
    show (TGuard k _) = show k
    show (TUse u _) = show u
    show (TValue v _) = BSL.toString $ encode v
    show (TStep ent e r _) =
      "(TStep " ++ show ent ++ " " ++ show e ++ maybeDelim " " r ++ ")"
    show TSchema {..} =
      "(TSchema " ++ asString' _tModule ++ "." ++ asString' _tSchemaName ++ " " ++
      show _tFields ++ " " ++ show _tMeta ++ ")"
    show TTable {..} =
      "(TTable " ++ asString' _tModule ++ "." ++ asString' _tTableName ++ ":" ++ show _tTableType
      ++ " " ++ show _tMeta ++ ")"

showParamType :: Show n => Type n -> String
showParamType TyAny = ""
showParamType t = ":" ++ show t

--deriveEq1 ''Term
-- instance Show1 Term
instance Eq1 Term where
  liftEq eq (TModule a b c) (TModule m n o) =
    a == m && liftEq eq b n && c == o
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
  liftEq eq (TObject a b c) (TObject m n o) =
    liftEq (\(w,x) (y,z) -> liftEq eq w y && liftEq eq x z) a m && liftEq (liftEq eq) b n && c == o
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
    TModule m b i >>= f = TModule m (b >>>= f) i
    TList bs t i >>= f = TList (map (>>= f) bs) (fmap (>>= f) t) i
    TDef (Def n m dt ft b d i) i' >>= f = TDef (Def n m dt (fmap (>>= f) ft) (b >>>= f) d i) i'
    TNative n fn t d tl i >>= f = TNative n fn (fmap (fmap (>>= f)) t) d tl i
    TConst d m c t i >>= f = TConst (fmap (>>= f) d) m (fmap (>>= f) c) t i
    TApp a i >>= f = TApp (fmap (>>= f) a) i
    TVar n i >>= f = (f n) { _tInfo = i }
    TBinding bs b c i >>= f = TBinding (map (fmap (>>= f) *** (>>= f)) bs) (b >>>= f) (fmap (fmap (>>= f)) c) i
    TObject bs t i >>= f = TObject (map ((>>= f) *** (>>= f)) bs) (fmap (>>= f) t) i
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
    parseJSON (Object o) = toTObject TyAny def <$> mapM (traverse parseJSON . first toTerm) (HM.toList o)
    parseJSON v = return $ toTerm v
    {-# INLINE parseJSON #-}

instance Show n => ToJSON (Term n) where
    toJSON (TLiteral l _) = toJSON l
    toJSON (TValue v _) = v
    toJSON (TGuard k _) = toJSON k
    toJSON (TObject kvs _ _) =
        object $ map (kToJSON *** toJSON) kvs
            where kToJSON (TLitString s) = s
                  kToJSON t = pack (abbrev t)
    toJSON (TList ts _ _) = toJSON ts
    toJSON t = toJSON (abbrev t)
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


toTObject :: Type (Term n) -> Info -> [(Term n,Term n)] -> Term n
toTObject ty i ps = TObject ps ty i

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
        BindSchema bt -> Right $ TySchema TyBinding bt
      TObject {..} -> Right $ TySchema TyObject _tObjectType
      TGuard {..} -> Right $ TyPrim $ TyGuard $ Just $ guardTypeOf _tGuard
      TUse {} -> Left "use"
      TValue {} -> Right $ TyPrim TyValue
      TStep {} -> Left "step"
      TSchema {..} -> Left $ "defobject:" <> asString _tSchemaName
      TTable {..} -> Right $ TySchema TyTable _tTableType
{-# INLINE typeof #-}

-- | Return string type description.
typeof' :: Show a => Term a -> Text
typeof' = either id (pack . show) . typeof

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
termEq (TObject a _ _) (TObject b _ _) = length a == length b && all (lkpEq b) a
    where lkpEq [] _ = False
          lkpEq ((k',v'):ts) p@(k,v) | termEq k k' && termEq v v' = True
                                     | otherwise = lkpEq ts p
termEq (TLiteral a _) (TLiteral b _) = a == b
termEq (TGuard a _) (TGuard b _) = a == b
termEq (TValue a _) (TValue b _) = a == b
termEq (TTable a b c d x _) (TTable e f g h y _) = a == e && b == f && c == g && d == h && x == y
termEq (TSchema a b c d _) (TSchema e f g h _) = a == e && b == f && c == g && d == h
termEq _ _ = False




abbrev :: Show t => Term t -> String
abbrev (TModule m _ _) =
  case m of
    Module{..} -> "<module " ++ asString' _mName ++ ">"
    Interface{..} -> "<interface " ++ asString' _interfaceName ++ ">"
abbrev (TList bs tl _) = "<list(" ++ show (length bs) ++ ")" ++ showParamType tl ++ ">"
abbrev TDef {..} = "<defun " ++ asString' (_dDefName _tDef) ++ ">"
abbrev TNative {..} = "<native " ++ asString' _tNativeName ++ ">"
abbrev TConst {..} = "<defconst " ++ show _tConstArg ++ ">"
abbrev TApp {..} = "<app " ++ abbrev (_appFun _tApp) ++ ">"
abbrev TBinding {} = "<binding>"
abbrev TObject {..} = "<object" ++ showParamType _tObjectType ++ ">"
abbrev (TLiteral l _) = show l
abbrev TGuard {} = "<guard>"
abbrev (TUse (Use m h _) _) = "<use '" ++ show m ++ maybeDelim " " h ++ ">"
abbrev (TVar s _) = show s
abbrev (TValue v _) = show v
abbrev TStep {} = "<step>"
abbrev TSchema {..} = "<defschema " ++ asString' _tSchemaName ++ ">"
abbrev TTable {..} = "<deftable " ++ asString' _tTableName ++ ">"



makeLenses ''Term
makeLenses ''Namespace
makeLenses ''FunApp
makeLenses ''Meta
makeLenses ''Module
makeLenses ''App
makeLenses ''Def
makeLenses ''ModuleName
