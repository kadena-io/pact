{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      :  Pact.Types.Lang
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Language types: 'Exp', 'Term', 'Type'.
--

module Pact.Types.Lang
 (
   Parsed(..),
   Code(..),
   Info(..),
   renderInfo,
   ModuleName(..),
   Name(..),
   Literal(..),
   simpleISO8601,formatLTime,
   TypeName(..),
   Arg(..),aInfo,aName,aType,
   FunType(..),ftArgs,ftReturn,
   FunTypes,funTypes,showFunTypes,
   PrimType(..),
   litToPrim,
   tyInteger,tyDecimal,tyTime,tyBool,tyString,
   tyList,tyObject,tyValue,tyKeySet,tyTable,
   SchemaType(..),
   TypeVarName(..),typeVarName,
   TypeVar(..),tvName,tvConstraint,
   Type(..),tyFunType,tyListType,tySchema,tySchemaType,tyUser,tyVar,
   mkTyVar,mkTyVar',mkSchemaVar,
   isAnyTy,isVarTy,isUnconstrainedTy,canUnifyWith,
   Exp(..),eLiteral,eAtom,eBinding,eList,eLitListType,eObject,eParsed,eQualifier,eSymbol,eType,
   _ELiteral,_ESymbol,_EAtom,_EList,_EObject,_EBinding,
   PublicKey(..),
   KeySet(..),
   KeySetName(..),
   DefType(..),
   defTypeRep,
   NativeDefName(..),
   FunApp(..),faDefType,faDocs,faInfo,faModule,faName,faTypes,
   Ref(..),
   NativeDFun(..),
   BindType(..),
   TableName(..),
   Module(..),
   ConstVal(..),
   Term(..),
   tAppArgs,tAppFun,tBindBody,tBindPairs,tBindType,tBlessed,tConstArg,tConstVal,
   tDefBody,tDefName,tDefType,tDocs,tFields,tFunTypes,tFunType,tHash,tInfo,tKeySet,
   tListType,tList,tLiteral,tModuleBody,tModuleDef,tModuleName,tModuleHash,tModule,
   tNativeDocs,tNativeFun,tNativeName,tObjectType,tObject,tSchemaName,
   tStepEntity,tStepExec,tStepRollback,tTableName,tTableType,tValue,tVar,
   ToTerm(..),
   toTermList,toTObject,toTList,
   typeof,typeof',
   pattern TLitString,pattern TLitInteger,pattern TLitBool,
   tLit,tStr,termEq,abbrev,
   Text,pack,unpack
   ) where


import Control.Lens hiding ((.=))
import Text.Trifecta.Delta hiding (Columns)
import Control.Applicative
import Data.List
import Control.Monad
import Prelude
import Control.Arrow hiding ((<+>))
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
import Data.Char
import Pact.Types.Time
import Data.Scientific
import GHC.Generics
import Data.Decimal
import Data.Hashable
import Data.List.NonEmpty (NonEmpty (..))
import Data.Foldable
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import Text.PrettyPrint.ANSI.Leijen hiding ((<>),(<$>))
import Data.Monoid
import Data.Semigroup (Semigroup)
import Control.DeepSeq
import Data.Maybe
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM

import Data.Serialize (Serialize)

import Pact.Types.Orphans ()
import Pact.Types.Util
--import Pact.Types.Crypto (Hash(..))

-- | Code location, length from parsing.
data Parsed = Parsed {
  _pDelta :: Delta,
  _pLength :: Int
  } deriving (Eq,Show,Ord,Generic)

instance NFData Parsed
instance Default Parsed where def = Parsed mempty 0
instance HasBytes Parsed where bytes = bytes . _pDelta
instance Pretty Parsed where pretty = pretty . _pDelta


newtype Code = Code { _unCode :: Text }
  deriving (Eq,Ord,IsString,ToJSON,FromJSON,Semigroup,Monoid,Generic,NFData,AsString)
instance Show Code where show = unpack . _unCode
instance Pretty Code where
  pretty (Code c) | T.compareLength c maxLen == GT =
                      text $ unpack (T.take maxLen c <> "...")
                  | otherwise = text $ unpack c
    where maxLen = 30

-- | For parsed items, original code and parse info;
-- for runtime items, nothing
data Info = Info { _iInfo :: !(Maybe (Code,Parsed)) } deriving (Generic)

instance NFData Info
-- show instance uses Trifecta renderings
instance Show Info where
    show (Info Nothing) = ""
    show (Info (Just (r,_d))) = renderCompactString r
instance Eq Info where
    Info Nothing == Info Nothing = True
    Info (Just (_,d)) == Info (Just (_,e)) = d == e
    _ == _ = False
instance Ord Info where
  Info Nothing <= Info Nothing = True
  Info (Just (_,d)) <= Info (Just (_,e)) = d <= e
  Info Nothing <= _ = True
  _ <= Info Nothing = False


instance Default Info where def = Info Nothing

asString' :: AsString a => a -> String
asString' = unpack . asString


-- renderer for line number output.
renderInfo :: Info -> String
renderInfo (Info Nothing) = ""
renderInfo (Info (Just (_,Parsed d _))) =
    case d of
      (Directed f l c _ _) -> asString' f ++ ":" ++ show (succ l) ++ ":" ++ show c
      (Lines l c _ _) -> "<interactive>:" ++ show (succ l) ++ ":" ++ show c
      _ -> "<interactive>:0:0"


newtype ModuleName = ModuleName Text
    deriving (Eq,Ord,IsString,ToJSON,FromJSON,AsString,Hashable,Pretty)
instance Show ModuleName where show (ModuleName s) = show s

-- | A named reference from source.
data Name =
    QName { _nQual :: ModuleName, _nName :: Text, _nInfo :: Info } |
    Name { _nName :: Text, _nInfo :: Info }
         deriving (Generic)
instance Show Name where
    show (QName q n _) = asString' q ++ "." ++ unpack n
    show (Name n _) = unpack n
instance ToJSON Name where toJSON = toJSON . show
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


data Literal =
    LString { _lString :: !Text } |
    LInteger { _lInteger :: !Integer } |
    LDecimal { _lDecimal :: !Decimal } |
    LBool { _lBool :: !Bool } |
    LTime { _lTime :: !UTCTime }
          deriving (Eq,Generic,Ord)


instance Serialize Literal
instance NFData Literal

-- | ISO8601 Thyme format
simpleISO8601 :: String
simpleISO8601 = "%Y-%m-%dT%H:%M:%SZ"

formatLTime :: UTCTime -> Text
formatLTime = pack . formatTime defaultTimeLocale simpleISO8601
{-# INLINE formatLTime #-}


instance Show Literal where
    show (LString s) = show s
    show (LInteger i) = show i
    show (LDecimal r) = show r
    show (LBool b) = map toLower $ show b
    show (LTime t) = show $ formatLTime t
instance ToJSON Literal where
    toJSON (LString s) = String s
    toJSON (LInteger i) = Number (scientific i 0)
    toJSON (LDecimal r) = toJSON (show r)
    toJSON (LBool b) = toJSON b
    toJSON (LTime t) = toJSON (formatLTime t)
    {-# INLINE toJSON #-}


newtype TypeName = TypeName Text
  deriving (Eq,Ord,IsString,AsString,ToJSON,FromJSON,Pretty,Generic,NFData)
instance Show TypeName where show (TypeName s) = show s

-- | Pair a name and a type (arguments, bindings etc)
data Arg o = Arg {
  _aName :: Text,
  _aType :: Type o,
  _aInfo :: Info
  } deriving (Eq,Ord,Functor,Foldable,Traversable,Generic)

instance NFData o => NFData (Arg o)
instance Show o => Show (Arg o) where show (Arg n t _) = unpack n ++ ":" ++ show t
instance (Pretty o) => Pretty (Arg o)
  where pretty (Arg n t _) = pretty n PP.<> colon PP.<> pretty t

instance Eq1 Arg where
  liftEq eq (Arg a b c) (Arg m n o) = a == m && liftEq eq b n && c == o

-- | Function type
data FunType o = FunType {
  _ftArgs :: [Arg o],
  _ftReturn :: Type o
  } deriving (Eq,Ord,Functor,Foldable,Traversable,Generic)

instance NFData o => NFData (FunType o)
instance Show o => Show (FunType o) where
  show (FunType as t) = "(" ++ unwords (map show as) ++ " -> " ++ show t ++ ")"
instance (Pretty o) => Pretty (FunType o) where
  pretty (FunType as t) = parens (hsep (map pretty as) <+> "->" <+> pretty t)

instance Eq1 FunType where
  liftEq eq (FunType a b) (FunType m n) = liftEq (liftEq eq) a m && liftEq eq b n

-- | use NonEmpty for function types
type FunTypes o = NonEmpty (FunType o)

funTypes :: FunType o -> FunTypes o
funTypes ft = ft :| []
showFunTypes :: Show o => FunTypes o -> String
showFunTypes (t :| []) = show t
showFunTypes ts = show (toList ts)

data PrimType =
  TyInteger |
  TyDecimal |
  TyTime |
  TyBool |
  TyString |
  TyValue |
  TyKeySet
  deriving (Eq,Ord,Generic)

instance NFData PrimType

litToPrim :: Literal -> PrimType
litToPrim LString {} = TyString
litToPrim LInteger {} = TyInteger
litToPrim LDecimal {} = TyDecimal
litToPrim LBool {} = TyBool
litToPrim LTime {} = TyTime

tyInteger,tyDecimal,tyTime,tyBool,tyString,tyList,tyObject,tyValue,tyKeySet,tyTable :: Text
tyInteger = "integer"
tyDecimal = "decimal"
tyTime = "time"
tyBool = "bool"
tyString = "string"
tyList = "list"
tyObject = "object"
tyValue = "value"
tyKeySet = "keyset"
tyTable = "table"

instance Show PrimType where
  show TyInteger = unpack tyInteger
  show TyDecimal = unpack tyDecimal
  show TyTime = unpack tyTime
  show TyBool = unpack tyBool
  show TyString = unpack tyString
  show TyValue = unpack tyValue
  show TyKeySet = unpack tyKeySet
instance Pretty PrimType where pretty = text . show

data SchemaType =
  TyTable |
  TyObject |
  TyBinding
  deriving (Eq,Ord,Generic)

instance NFData SchemaType
instance Show SchemaType where
  show TyTable = unpack tyTable
  show TyObject = unpack tyObject
  show TyBinding = "binding"
instance Pretty SchemaType where pretty = text . show

newtype TypeVarName = TypeVarName { _typeVarName :: Text }
  deriving (Eq,Ord,IsString,AsString,ToJSON,FromJSON,Hashable,Pretty,Generic,NFData)
instance Show TypeVarName where show = unpack . _typeVarName

-- | Type variables are namespaced for value types and schema types.
data TypeVar v =
  TypeVar { _tvName :: TypeVarName, _tvConstraint :: [Type v] } |
  SchemaVar { _tvName :: TypeVarName }
  deriving (Functor,Foldable,Traversable,Generic)

instance NFData v => NFData (TypeVar v)
instance Eq (TypeVar v) where
  (TypeVar a _) == (TypeVar b _) = a == b
  (SchemaVar a) == (SchemaVar b) = a == b
  _ == _ = False
instance Ord (TypeVar v) where
  x `compare` y = case (x,y) of
    (TypeVar {},SchemaVar {}) -> LT
    (SchemaVar {},TypeVar {}) -> GT
    (TypeVar a _,TypeVar b _) -> a `compare` b
    (SchemaVar a,SchemaVar b) -> a `compare` b
instance Show v => Show (TypeVar v) where
  show (TypeVar n []) = "<" ++ show n ++ ">"
  show (TypeVar n cs) = "<" ++ show n ++ show cs ++ ">"
  show (SchemaVar n) = "<{" ++ show n ++ "}>"
instance (Pretty v) => Pretty (TypeVar v) where
  pretty (TypeVar n []) = angles (pretty n)
  pretty (TypeVar n cs) = angles (pretty n <+> brackets (hsep (map pretty cs)))
  pretty (SchemaVar n) = angles (braces (pretty n))

instance Eq1 TypeVar where
  liftEq eq (TypeVar a b) (TypeVar m n) = a == m && liftEq (liftEq eq) b n
  liftEq _ (SchemaVar a) (SchemaVar m) = a == m
  liftEq _ _ _ = False


-- | Pact types.
data Type v =
  TyAny |
  TyVar { _tyVar :: TypeVar v } |
  TyPrim PrimType |
  TyList { _tyListType :: Type v } |
  TySchema { _tySchema :: SchemaType, _tySchemaType :: Type v } |
  TyFun { _tyFunType :: FunType v } |
  TyUser { _tyUser :: v }
    deriving (Eq,Ord,Functor,Foldable,Traversable,Generic)

instance Eq1 Type where
  liftEq _ TyAny TyAny = True
  liftEq eq (TyVar a) (TyVar m) = liftEq eq a m
  liftEq _ (TyPrim a) (TyPrim m) = a == m
  liftEq eq (TyList a) (TyList m) = liftEq eq a m
  liftEq eq (TySchema a b) (TySchema m n) = a == m && liftEq eq b n
  liftEq eq (TyFun a) (TyFun b) = liftEq eq a b
  liftEq eq (TyUser a) (TyUser b) = eq a b
  liftEq _ _ _ = False

instance NFData v => NFData (Type v)

instance (Show v) => Show (Type v) where
  show (TyPrim t) = show t
  show (TyList t) | isAnyTy t = unpack tyList
                  | otherwise = "[" ++ show t ++ "]"
  show (TySchema s t) | isAnyTy t = show s
                      | otherwise = show s ++ ":" ++ show t
  show (TyFun f) = show f
  show (TyUser v) = show v
  show TyAny = "*"
  show (TyVar n) = show n

instance (Pretty o) => Pretty (Type o) where
  pretty ty = case ty of
    TyVar n -> pretty n
    TyUser v -> pretty v
    TyFun f -> pretty f
    TySchema s t -> pretty s PP.<> colon PP.<> pretty t
    TyList t -> "list:" PP.<> pretty t
    TyPrim t -> pretty t
    TyAny -> "*"

mkTyVar :: TypeVarName -> [Type n] -> Type n
mkTyVar n cs = TyVar (TypeVar n cs)
mkTyVar' :: TypeVarName -> Type n
mkTyVar' n = mkTyVar n []
mkSchemaVar :: TypeVarName -> Type n
mkSchemaVar n = TyVar (SchemaVar n)

isAnyTy :: Type v -> Bool
isAnyTy TyAny = True
isAnyTy _ = False

isVarTy :: Type v -> Bool
isVarTy TyVar {} = True
isVarTy _ = False

isUnconstrainedTy :: Type v -> Bool
isUnconstrainedTy TyAny = True
isUnconstrainedTy (TyVar (TypeVar _ [])) = True
isUnconstrainedTy _ = False
{-# INLINE isUnconstrainedTy #-}

-- | a `canUnifyWith` b means a "can represent/contains" b
canUnifyWith :: Eq n => Type n -> Type n -> Bool
canUnifyWith TyAny _ = True
canUnifyWith _ TyAny = True
canUnifyWith (TyVar (SchemaVar _)) TyUser {} = True
canUnifyWith (TyVar SchemaVar {}) (TyVar SchemaVar {}) = True
canUnifyWith (TyVar (TypeVar _ ac)) (TyVar (TypeVar _ bc)) = all (`elem` ac) bc
canUnifyWith (TyVar (TypeVar _ cs)) b = null cs || b `elem` cs
canUnifyWith (TyList a) (TyList b) = a `canUnifyWith` b
canUnifyWith (TySchema _ a) (TySchema _ b) = a `canUnifyWith` b
canUnifyWith a b = a == b
{-# INLINE canUnifyWith #-}

makeLenses ''Type
makeLenses ''FunType
makeLenses ''Arg
makeLenses ''TypeVar
makeLenses ''TypeVarName


-- | Pact expressions, with parsing info.
data Exp =
  -- | Literals
  ELiteral { _eLiteral :: !Literal, _eParsed :: !Parsed } |
  -- | Symbol, effectively a string literal
  ESymbol { _eSymbol :: !Text, _eParsed :: !Parsed } |
  -- | Atom, with support for type literals.
  EAtom { _eAtom :: !Text
        , _eQualifier :: !(Maybe Text)
        , _eType :: !(Maybe (Type TypeName))
        , _eParsed :: !Parsed
        } |
  -- | Lists. '_eLitListType' distinguishes literal lists (`[1 2 3]`) from body forms.
  EList { _eList :: ![Exp], _eLitListType :: !(Maybe (Type TypeName)), _eParsed :: !Parsed } |
  -- | Object literals.
  EObject { _eObject :: ![(Exp,Exp)], _eParsed :: !Parsed } |
  -- | Special binding forms.
  EBinding { _eBinding :: ![(Exp,Exp)], _eParsed :: !Parsed }
           deriving (Eq,Generic)

instance NFData Exp

makePrisms ''Exp


maybeDelim :: Show a => String -> Maybe a -> String
maybeDelim d t = maybe "" ((d ++) . show) t


instance Show Exp where
    show (ELiteral i _) = show i
    show (ESymbol s _) = '\'':unpack s
    show (EAtom a q t _) =  unpack a ++ maybeDelim "."  q ++ maybeDelim ": " t
    show (EList ls Nothing _) = "(" ++ unwords (map show ls) ++ ")"
    show (EList ls Just {} _) = "[" ++ unwords (map show ls) ++ "]"
    show (EObject ps _) = "{ " ++ intercalate ", " (map (\(k,v) -> show k ++ ": " ++ show v) ps) ++ " }"
    show (EBinding ps _) = "{ " ++ intercalate ", " (map (\(k,v) -> show k ++ ":= " ++ show v) ps) ++ " }"

$(makeLenses ''Exp)




newtype PublicKey = PublicKey { _pubKey :: BS.ByteString } deriving (Eq,Ord,Generic,IsString,AsString)

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
    , _ksPredFun :: !Text
    } deriving (Eq,Generic)
instance Serialize KeySet
instance Show KeySet where show (KeySet ks f) = "KeySet " ++ show ks ++ " " ++ show f

-- | allow `{ "keys": [...], "pred": "..." }`, `{ "keys": [...] }`, and just `[...]`,
-- | the latter cases defaulting to "keys-all"
instance FromJSON KeySet where
    parseJSON v = withObject "KeySet" (\o ->
                    KeySet <$> o .: "keys" <*>
                    (fromMaybe defPred <$> o .:? "pred")) v <|>
                  (KeySet <$> parseJSON v <*> pure defPred)
      where defPred = "keys-all"
instance ToJSON KeySet where
    toJSON (KeySet k f) = object ["keys" .= k, "pred" .= f]


newtype KeySetName = KeySetName Text
    deriving (Eq,Ord,IsString,AsString,ToJSON,FromJSON)
instance Show KeySetName where show (KeySetName s) = show s


data DefType = Defun | Defpact deriving (Eq,Show)
defTypeRep :: DefType -> String
defTypeRep Defun = "defun"
defTypeRep Defpact = "defpact"

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

data NativeDFun = NativeDFun {
      _nativeName :: NativeDefName,
      _nativeFun :: forall m . Monad m => FunApp -> [Term Ref] -> m (Term Name)
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

data Module = Module {
    _mName :: !ModuleName
  , _mKeySet :: !KeySetName
  , _mDocs :: !(Maybe Text)
  , _mCode :: !Code
  , _mHash :: !Hash
  , _mBlessed :: !(HS.HashSet Hash)
  } deriving (Eq)
instance Show Module where
  show Module {..} =
    "(Module " ++ asString' _mName ++ " '" ++ asString' _mKeySet ++ " " ++ show _mHash ++ ")"
instance ToJSON Module where
  toJSON Module {..} = object $
    ["name" .= _mName, "keyset" .= _mKeySet, "code" .= _mCode, "hash" .= _mHash, "blessed" .= toList _mBlessed ]
    ++ maybe [] (return . ("docs" .=)) _mDocs
instance FromJSON Module where
  parseJSON = withObject "Module" $ \o -> Module <$>
    o .: "name" <*> o .: "keyset" <*> o .:? "docs" <*> o .: "code" <*> o .: "hash" <*> (HS.fromList <$> o .: "blessed")

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
      _tDefName :: !Text
    , _tModule :: !ModuleName
    , _tDefType :: !DefType
    , _tFunType :: !(FunType (Term n))
    , _tDefBody :: !(Scope Int Term n)
    , _tDocs :: !(Maybe Text)
    , _tInfo :: !Info
    } |
    TNative {
      _tNativeName :: !NativeDefName
    , _tNativeFun :: !NativeDFun
    , _tFunTypes :: FunTypes (Term n)
    , _tNativeDocs :: Text
    , _tInfo :: !Info
    } |
    TConst {
      _tConstArg :: !(Arg (Term n))
    , _tModule :: !ModuleName
    , _tConstVal :: !(ConstVal (Term n))
    , _tDocs :: !(Maybe Text)
    , _tInfo :: !Info
    } |
    TApp {
      _tAppFun :: !(Term n)
    , _tAppArgs :: ![Term n]
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
    , _tDocs :: !(Maybe Text)
    , _tFields :: ![Arg (Term n)]
    , _tInfo :: !Info
    } |
    TLiteral {
      _tLiteral :: !Literal
    , _tInfo :: !Info
    } |
    TKeySet {
      _tKeySet :: !KeySet
    , _tInfo :: !Info
    } |
    TUse {
      _tModuleName :: !ModuleName
    , _tModuleHash :: !(Maybe Hash)
    , _tInfo :: !Info
    } |
    TBless {
      _tBlessed :: !Hash
    , _tInfo :: !Info
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
    , _tDocs :: !(Maybe Text)
    , _tInfo :: !Info
    }
    deriving (Functor,Foldable,Traversable,Eq)

instance Show n => Show (Term n) where
    show TModule {..} =
      "(TModule " ++ show _tModuleDef ++ " " ++ show (unscope _tModuleBody) ++ ")"
    show (TList bs _ _) = "[" ++ unwords (map show bs) ++ "]"
    show TDef {..} =
      "(TDef " ++ defTypeRep _tDefType ++ " " ++ asString' _tModule ++ "." ++ unpack _tDefName ++ " " ++
      show _tFunType ++ maybeDelim " " _tDocs ++ ")"
    show TNative {..} =
      "(TNative " ++ asString' _tNativeName ++ " " ++ showFunTypes _tFunTypes ++ " " ++ unpack _tNativeDocs ++ ")"
    show TConst {..} =
      "(TConst " ++ asString' _tModule ++ "." ++ show _tConstArg ++ maybeDelim " " _tDocs ++ ")"
    show (TApp f as _) = "(TApp " ++ show f ++ " " ++ show as ++ ")"
    show (TVar n _) = "(TVar " ++ show n ++ ")"
    show (TBinding bs b c _) = "(TBinding " ++ show bs ++ " " ++ show (unscope b) ++ " " ++ show c ++ ")"
    show (TObject bs _ _) =
      "{" ++ intercalate ", " (map (\(a,b) -> show a ++ ": " ++ show b) bs) ++ "}"
    show (TLiteral l _) = show l
    show (TKeySet k _) = show k
    show (TUse m h _) = "(TUse " ++ show m ++ maybeDelim " " h ++ ")"
    show (TBless hs _) = "(TBless " ++ show hs ++ ")"
    show (TValue v _) = BSL.toString $ encode v
    show (TStep ent e r _) =
      "(TStep " ++ show ent ++ " " ++ show e ++ maybeDelim " " r ++ ")"
    show TSchema {..} =
      "(TSchema " ++ asString' _tModule ++ "." ++ asString' _tSchemaName ++ " " ++
      show _tFields ++ maybeDelim " " _tDocs ++ ")"
    show TTable {..} =
      "(TTable " ++ asString' _tModule ++ "." ++ asString' _tTableName ++ ":" ++ show _tTableType
      ++ maybeDelim " " _tDocs ++ ")"

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
  liftEq eq (TDef a b c d e f g) (TDef m n o p q r s) =
    a == m && b == n && c == o && liftEq (liftEq eq) d p && liftEq eq e q && f == r && g == s
  liftEq eq (TConst a b c d e) (TConst m n o q r) =
    liftEq (liftEq eq) a m && b == n && liftEq (liftEq eq) c o && d == q && e == r
  liftEq eq (TApp a b c) (TApp m n o) =
    liftEq eq a m && liftEq (liftEq eq) b n && c == o
  liftEq eq (TVar a b) (TVar m n) =
    eq a m && b == n
  liftEq eq (TBinding a b c d) (TBinding m n o p) =
    liftEq (\(w,x) (y,z) -> liftEq (liftEq eq) w y && liftEq eq x z) a m &&
    liftEq eq b n && liftEq (liftEq (liftEq eq)) c o && d == p
  liftEq eq (TObject a b c) (TObject m n o) =
    liftEq (\(w,x) (y,z) -> liftEq eq w y && liftEq eq x z) a m && liftEq (liftEq eq) b n && c == o
  liftEq _ (TLiteral a b) (TLiteral m n) =
    a == m && b == n
  liftEq _ (TKeySet a b) (TKeySet m n) =
    a == m && b == n
  liftEq _ (TUse a b c) (TUse m n o) =
    a == m && b == n && c == o
  liftEq _ (TBless a b) (TBless m n) =
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
    TDef n m dt ft b d i >>= f = TDef n m dt (fmap (>>= f) ft) (b >>>= f) d i
    TNative n fn t d i >>= f = TNative n fn (fmap (fmap (>>= f)) t) d i
    TConst d m c t i >>= f = TConst (fmap (>>= f) d) m (fmap (>>= f) c) t i
    TApp af as i >>= f = TApp (af >>= f) (map (>>= f) as) i
    TVar n i >>= f = (f n) { _tInfo = i }
    TBinding bs b c i >>= f = TBinding (map (fmap (>>= f) *** (>>= f)) bs) (b >>>= f) (fmap (fmap (>>= f)) c) i
    TObject bs t i >>= f = TObject (map ((>>= f) *** (>>= f)) bs) (fmap (>>= f) t) i
    TLiteral l i >>= _ = TLiteral l i
    TKeySet k i >>= _ = TKeySet k i
    TUse m h i >>= _ = TUse m h i
    TBless hs i >>= _ = TBless hs i
    TValue v i >>= _ = TValue v i
    TStep ent e r i >>= f = TStep (fmap (>>= f) ent) (e >>= f) (fmap (>>= f) r) i
    TSchema {..} >>= f = TSchema _tSchemaName _tModule _tDocs (fmap (fmap (>>= f)) _tFields) _tInfo
    TTable {..} >>= f = TTable _tTableName _tModule _tHash (fmap (>>= f) _tTableType) _tDocs _tInfo


instance FromJSON (Term n) where
    parseJSON (Number n) = return $ TLiteral (LInteger (round n)) def
    parseJSON (Bool b) = return $ toTerm b
    parseJSON (String s) = return $ toTerm s
    parseJSON (Array a) = (toTList TyAny def . toList) <$> mapM parseJSON a
    parseJSON (Object o) = toTObject TyAny def <$> mapM (traverse parseJSON . first toTerm) (HM.toList o)
    parseJSON v = return $ toTerm v
    {-# INLINE parseJSON #-}

instance Show n => ToJSON (Term n) where
    toJSON (TLiteral l _) = toJSON l
    toJSON (TValue v _) = v
    toJSON (TKeySet k _) = toJSON k
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
instance ToTerm KeySet where toTerm = (`TKeySet` def)
instance ToTerm Literal where toTerm = tLit
instance ToTerm Value where toTerm = (`TValue` def)
instance ToTerm UTCTime where toTerm = tLit . LTime


toTObject :: Type (Term n) -> Info -> [(Term n,Term n)] -> Term n
toTObject ty i ps = TObject ps ty i

toTList :: Type (Term n) -> Info -> [Term n] -> Term n
toTList ty i vs = TList vs ty i

toTermList :: (ToTerm a,Foldable f) => Type (Term b) -> f a -> Term b
toTermList ty l = TList (map toTerm (toList l)) ty def



-- | Return a Pact type, or a String description of non-value Terms.
typeof :: Term a -> Either Text (Type (Term a))
typeof t = case t of
      TLiteral l _ -> Right $ TyPrim $ litToPrim l
      TModule {} -> Left "module"
      TList {..} -> Right $ TyList _tListType
      TDef {..} -> Left $ pack $ defTypeRep _tDefType
      TNative {..} -> Left "defun"
      TConst {..} -> Left $ "const:" <> _aName _tConstArg
      TApp {..} -> Left "app"
      TVar {..} -> Left "var"
      TBinding {..} -> case _tBindType of
        BindLet -> Left "let"
        BindSchema bt -> Right $ TySchema TyBinding bt
      TObject {..} -> Right $ TySchema TyObject _tObjectType
      TKeySet {} -> Right $ TyPrim TyKeySet
      TUse {} -> Left "use"
      TBless {} -> Left "bless"
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
termEq (TKeySet a _) (TKeySet b _) = a == b
termEq (TValue a _) (TValue b _) = a == b
termEq (TTable a b c d x _) (TTable e f g h y _) = a == e && b == f && c == g && d == h && x == y
termEq (TSchema a b c d _) (TSchema e f g h _) = a == e && b == f && c == g && d == h
termEq _ _ = False




abbrev :: Show t => Term t -> String
abbrev (TModule m _ _) = "<module " ++ asString' (_mName m) ++ ">"
abbrev (TList bs tl _) = "<list(" ++ show (length bs) ++ ")" ++ showParamType tl ++ ">"
abbrev TDef {..} = "<defun " ++ unpack _tDefName ++ ">"
abbrev TNative {..} = "<native " ++ asString' _tNativeName ++ ">"
abbrev TConst {..} = "<defconst " ++ show _tConstArg ++ ">"
abbrev t@TApp {} = "<app " ++ abbrev (_tAppFun t) ++ ">"
abbrev TBinding {} = "<binding>"
abbrev TObject {..} = "<object" ++ showParamType _tObjectType ++ ">"
abbrev (TLiteral l _) = show l
abbrev TKeySet {} = "<keyset>"
abbrev (TUse m h _) = "<use '" ++ show m ++ maybeDelim " " h ++ ">"
abbrev TBless {} = "<bless ...>"
abbrev (TVar s _) = show s
abbrev (TValue v _) = show v
abbrev TStep {} = "<step>"
abbrev TSchema {..} = "<defschema " ++ asString' _tSchemaName ++ ">"
abbrev TTable {..} = "<deftable " ++ asString' _tTableName ++ ">"




makeLenses ''Term
makeLenses ''FunApp
