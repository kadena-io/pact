{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

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
   tAppArgs,tAppFun,tBindBody,tBindPairs,tBindType,tConstArg,tConstVal,
   tDefBody,tDefName,tDefType,tDocs,tFields,tFunTypes,tFunType,tInfo,tKeySet,
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
import Data.Thyme
import Data.Thyme.Format.Aeson ()
import System.Locale
import Data.Scientific
import GHC.Generics
import Data.Decimal
import Data.Hashable
import Data.List.NonEmpty (NonEmpty (..))
import Data.Foldable
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import Text.PrettyPrint.ANSI.Leijen hiding ((<>),(<$>))
import Data.Monoid
import Control.DeepSeq
import Data.Maybe
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
  deriving (Eq,Ord,IsString,ToJSON,FromJSON,Monoid,Generic,NFData,AsString)
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
  } deriving (Eq)
instance Show Module where
  show Module {..} =
    "(Module " ++ asString' _mName ++ " '" ++ asString' _mKeySet ++ maybeDelim " " _mDocs ++ ")"
instance ToJSON Module where
  toJSON Module {..} = object $
    ["name" .= _mName, "keyset" .= _mKeySet, "code" .= _mCode, "hash" .= _mHash ]
    ++ maybe [] (return . ("docs" .=)) _mDocs
instance FromJSON Module where
  parseJSON = withObject "Module" $ \o -> Module <$>
    o .: "name" <*> o .: "keyset" <*> o .:? "docs" <*> o .: "code" <*> o .: "hash"

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
  liftEq _ (TValue a b) (TValue m n) =
    a == m && b == n
  liftEq eq (TStep a b c d) (TStep m n o p) =
    liftEq (liftEq eq) a m && liftEq eq b n && liftEq (liftEq eq) c o && d == p
  liftEq eq (TSchema a b c d e) (TSchema m n o p q) =
    a == m && b == n && c == o && liftEq (liftEq (liftEq eq)) d p && e == q
  liftEq eq (TTable a b c d e) (TTable m n o p q) =
    a == m && b == n && liftEq (liftEq eq) c o && d == p && e == q
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
    TValue v i >>= _ = TValue v i
    TStep ent e r i >>= f = TStep (fmap (>>= f) ent) (e >>= f) (fmap (>>= f) r) i
    TSchema {..} >>= f = TSchema _tSchemaName _tModule _tDocs (fmap (fmap (>>= f)) _tFields) _tInfo
    TTable {..} >>= f = TTable _tTableName _tModule (fmap (>>= f) _tTableType) _tDocs _tInfo


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
termEq (TTable a b c d _) (TTable e f g h _) = a == e && b == f && c == g && d == h
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
abbrev (TVar s _) = show s
abbrev (TValue v _) = show v
abbrev TStep {} = "<step>"
abbrev TSchema {..} = "<defschema " ++ asString' _tSchemaName ++ ">"
abbrev TTable {..} = "<deftable " ++ asString' _tTableName ++ ">"


------------------------------------------------------------------------------
--makeLenses ''Type
tyFunType ::
  forall v_a1l9W. Traversal' (Type v_a1l9W) (FunType v_a1l9W)
tyFunType _ TyAny = pure TyAny
tyFunType _ (TyVar x1_a1oNy) = pure (TyVar x1_a1oNy)
tyFunType _ (TyPrim x1_a1oNz) = pure (TyPrim x1_a1oNz)
tyFunType _ (TyList x1_a1oNA) = pure (TyList x1_a1oNA)
tyFunType _ (TySchema x1_a1oNB x2_a1oNC)
  = pure (TySchema x1_a1oNB x2_a1oNC)
tyFunType f_a1oND (TyFun x1_a1oNE)
  = fmap (\ y1_a1oNF -> TyFun y1_a1oNF) (f_a1oND x1_a1oNE)
tyFunType _ (TyUser x1_a1oNG) = pure (TyUser x1_a1oNG)
{-# INLINE tyFunType #-}
tyListType ::
  forall v_a1l9W. Traversal' (Type v_a1l9W) (Type v_a1l9W)
tyListType _ TyAny = pure TyAny
tyListType _ (TyVar x1_a1oNH) = pure (TyVar x1_a1oNH)
tyListType _ (TyPrim x1_a1oNI) = pure (TyPrim x1_a1oNI)
tyListType f_a1oNJ (TyList x1_a1oNK)
  = fmap (\ y1_a1oNL -> TyList y1_a1oNL) (f_a1oNJ x1_a1oNK)
tyListType _ (TySchema x1_a1oNM x2_a1oNN)
  = pure (TySchema x1_a1oNM x2_a1oNN)
tyListType _ (TyFun x1_a1oNO) = pure (TyFun x1_a1oNO)
tyListType _ (TyUser x1_a1oNP) = pure (TyUser x1_a1oNP)
{-# INLINE tyListType #-}
tySchema :: forall v_a1l9W. Traversal' (Type v_a1l9W) SchemaType
tySchema _ TyAny = pure TyAny
tySchema _ (TyVar x1_a1oNQ) = pure (TyVar x1_a1oNQ)
tySchema _ (TyPrim x1_a1oNR) = pure (TyPrim x1_a1oNR)
tySchema _ (TyList x1_a1oNS) = pure (TyList x1_a1oNS)
tySchema f_a1oNT (TySchema x1_a1oNU x2_a1oNV)
  = fmap
      (\ y1_a1oNW -> TySchema y1_a1oNW x2_a1oNV) (f_a1oNT x1_a1oNU)
tySchema _ (TyFun x1_a1oNX) = pure (TyFun x1_a1oNX)
tySchema _ (TyUser x1_a1oNY) = pure (TyUser x1_a1oNY)
{-# INLINE tySchema #-}
tySchemaType ::
  forall v_a1l9W. Traversal' (Type v_a1l9W) (Type v_a1l9W)
tySchemaType _ TyAny = pure TyAny
tySchemaType _ (TyVar x1_a1oNZ) = pure (TyVar x1_a1oNZ)
tySchemaType _ (TyPrim x1_a1oO0) = pure (TyPrim x1_a1oO0)
tySchemaType _ (TyList x1_a1oO1) = pure (TyList x1_a1oO1)
tySchemaType f_a1oO2 (TySchema x1_a1oO3 x2_a1oO4)
  = fmap
      (\ y1_a1oO5 -> TySchema x1_a1oO3 y1_a1oO5) (f_a1oO2 x2_a1oO4)
tySchemaType _ (TyFun x1_a1oO6) = pure (TyFun x1_a1oO6)
tySchemaType _ (TyUser x1_a1oO7) = pure (TyUser x1_a1oO7)
{-# INLINE tySchemaType #-}
tyUser :: forall v_a1l9W. Traversal' (Type v_a1l9W) v_a1l9W
tyUser _ TyAny = pure TyAny
tyUser _ (TyVar x1_a1oO8) = pure (TyVar x1_a1oO8)
tyUser _ (TyPrim x1_a1oO9) = pure (TyPrim x1_a1oO9)
tyUser _ (TyList x1_a1oOa) = pure (TyList x1_a1oOa)
tyUser _ (TySchema x1_a1oOb x2_a1oOc)
  = pure (TySchema x1_a1oOb x2_a1oOc)
tyUser _ (TyFun x1_a1oOd) = pure (TyFun x1_a1oOd)
tyUser f_a1oOe (TyUser x1_a1oOf)
  = fmap (\ y1_a1oOg -> TyUser y1_a1oOg) (f_a1oOe x1_a1oOf)
{-# INLINE tyUser #-}
tyVar ::
  forall v_a1l9W. Traversal' (Type v_a1l9W) (TypeVar v_a1l9W)
tyVar _ TyAny = pure TyAny
tyVar f_a1oOh (TyVar x1_a1oOi)
  = fmap (\ y1_a1oOj -> TyVar y1_a1oOj) (f_a1oOh x1_a1oOi)
tyVar _ (TyPrim x1_a1oOk) = pure (TyPrim x1_a1oOk)
tyVar _ (TyList x1_a1oOl) = pure (TyList x1_a1oOl)
tyVar _ (TySchema x1_a1oOm x2_a1oOn)
  = pure (TySchema x1_a1oOm x2_a1oOn)
tyVar _ (TyFun x1_a1oOo) = pure (TyFun x1_a1oOo)
tyVar _ (TyUser x1_a1oOp) = pure (TyUser x1_a1oOp)
{-# INLINE tyVar #-}


------------------------------------------------------------------------------
--makeLenses ''FunType
ftArgs :: forall o_a1l9Z. Lens' (FunType o_a1l9Z) [Arg o_a1l9Z]
ftArgs f_a1oTk (FunType x1_a1oTl x2_a1oTm)
  = fmap (\ y1_a1oTn -> FunType y1_a1oTn x2_a1oTm) (f_a1oTk x1_a1oTl)
{-# INLINE ftArgs #-}
ftReturn :: forall o_a1l9Z. Lens' (FunType o_a1l9Z) (Type o_a1l9Z)
ftReturn f_a1oTo (FunType x1_a1oTp x2_a1oTq)
  = fmap (\ y1_a1oTr -> FunType x1_a1oTp y1_a1oTr) (f_a1oTo x2_a1oTq)
{-# INLINE ftReturn #-}


------------------------------------------------------------------------------
--makeLenses ''Arg
aInfo :: forall o_a1la0. Lens' (Arg o_a1la0) Info
aInfo f_a1oUk (Arg x1_a1oUl x2_a1oUm x3_a1oUn)
  = fmap
      (\ y1_a1oUo -> Arg x1_a1oUl x2_a1oUm y1_a1oUo) (f_a1oUk x3_a1oUn)
{-# INLINE aInfo #-}
aName :: forall o_a1la0. Lens' (Arg o_a1la0) Text
aName f_a1oUp (Arg x1_a1oUq x2_a1oUr x3_a1oUs)
  = fmap
      (\ y1_a1oUt -> Arg y1_a1oUt x2_a1oUr x3_a1oUs) (f_a1oUp x1_a1oUq)
{-# INLINE aName #-}
aType ::
  forall o_a1la0 o_a1oUj.
  Lens (Arg o_a1la0) (Arg o_a1oUj) (Type o_a1la0) (Type o_a1oUj)
aType f_a1oUu (Arg x1_a1oUv x2_a1oUw x3_a1oUx)
  = fmap
      (\ y1_a1oUy -> Arg x1_a1oUv y1_a1oUy x3_a1oUx) (f_a1oUu x2_a1oUw)
{-# INLINE aType #-}

------------------------------------------------------------------------------
--makeLenses ''TypeVar
tvConstraint ::
  forall v_a1l9X v_a1oVR.
  Traversal (TypeVar v_a1l9X) (TypeVar v_a1oVR) [Type v_a1l9X] [Type v_a1oVR]
tvConstraint f_a1oVS (TypeVar x1_a1oVT x2_a1oVU)
  = fmap (\ y1_a1oVV -> TypeVar x1_a1oVT y1_a1oVV) (f_a1oVS x2_a1oVU)
tvConstraint _ (SchemaVar x1_a1oVW) = pure (SchemaVar x1_a1oVW)
{-# INLINE tvConstraint #-}
tvName :: forall v_a1l9X. Lens' (TypeVar v_a1l9X) TypeVarName
tvName f_a1oVX (TypeVar x1_a1oVY x2_a1oVZ)
  = fmap (\ y1_a1oW0 -> TypeVar y1_a1oW0 x2_a1oVZ) (f_a1oVX x1_a1oVY)
tvName f_a1oW1 (SchemaVar x1_a1oW2)
  = fmap (\ y1_a1oW3 -> SchemaVar y1_a1oW3) (f_a1oW1 x1_a1oW2)
{-# INLINE tvName #-}


------------------------------------------------------------------------------
--makeLenses ''TypeVarName
typeVarName :: Iso' TypeVarName Text
typeVarName = iso (\ (TypeVarName x_a1oX9) -> x_a1oX9) TypeVarName
{-# INLINE typeVarName #-}


------------------------------------------------------------------------------
--makePrisms ''Exp
_ELiteral :: Prism' Exp (Literal, Parsed)
_ELiteral
  = prism
      (\ (x1_a1pim, x2_a1pin) -> ELiteral x1_a1pim x2_a1pin)
      (\ x_a1pio
         -> case x_a1pio of
              ELiteral y1_a1pip y2_a1piq -> Right (y1_a1pip, y2_a1piq)
              _ -> Left x_a1pio )
_ESymbol :: Prism' Exp (Text, Parsed)
_ESymbol
  = prism
      (\ (x1_a1pir, x2_a1pis) -> ESymbol x1_a1pir x2_a1pis)
      (\ x_a1pit
         -> case x_a1pit of
              ESymbol y1_a1piu y2_a1piv -> Right (y1_a1piu, y2_a1piv)
              _ -> Left x_a1pit )
_EAtom ::
  Prism' Exp (Text, Maybe Text, Maybe (Type TypeName), Parsed)
_EAtom
  = prism
      (\ (x1_a1piw, x2_a1pix, x3_a1piy, x4_a1piz)
         -> EAtom x1_a1piw x2_a1pix x3_a1piy x4_a1piz)
      (\ x_a1piA
         -> case x_a1piA of
              EAtom y1_a1piB y2_a1piC y3_a1piD y4_a1piE
                -> Right (y1_a1piB, y2_a1piC, y3_a1piD, y4_a1piE)
              _ -> Left x_a1piA )
_EList :: Prism' Exp ([Exp], Maybe (Type TypeName), Parsed)
_EList
  = prism
      (\ (x1_a1piF, x2_a1piG, x3_a1piH)
         -> EList x1_a1piF x2_a1piG x3_a1piH)
      (\ x_a1piI
         -> case x_a1piI of
              EList y1_a1piJ y2_a1piK y3_a1piL
                -> Right (y1_a1piJ, y2_a1piK, y3_a1piL)
              _ -> Left x_a1piI )
_EObject :: Prism' Exp ([(Exp, Exp)], Parsed)
_EObject
  = prism
      (\ (x1_a1piM, x2_a1piN) -> EObject x1_a1piM x2_a1piN)
      (\ x_a1piO
         -> case x_a1piO of
              EObject y1_a1piP y2_a1piQ -> Right (y1_a1piP, y2_a1piQ)
              _ -> Left x_a1piO )
_EBinding :: Prism' Exp ([(Exp, Exp)], Parsed)
_EBinding
  = prism
      (\ (x1_a1piR, x2_a1piS) -> EBinding x1_a1piR x2_a1piS)
      (\ x_a1piT
         -> case x_a1piT of
              EBinding y1_a1piU y2_a1piV -> Right (y1_a1piU, y2_a1piV)
              _ -> Left x_a1piT )


------------------------------------------------------------------------------
--makeLenses ''Exp
eAtom :: Traversal' Exp Text
eAtom _ (ELiteral x1_a1pxB x2_a1pxC)
  = pure (ELiteral x1_a1pxB x2_a1pxC)
eAtom _ (ESymbol x1_a1pxD x2_a1pxE)
  = pure (ESymbol x1_a1pxD x2_a1pxE)
eAtom f_a1pxF (EAtom x1_a1pxG x2_a1pxH x3_a1pxI x4_a1pxJ)
  = fmap
      (\ y1_a1pxK -> EAtom y1_a1pxK x2_a1pxH x3_a1pxI x4_a1pxJ)
      (f_a1pxF x1_a1pxG)
eAtom _ (EList x1_a1pxL x2_a1pxM x3_a1pxN)
  = pure (EList x1_a1pxL x2_a1pxM x3_a1pxN)
eAtom _ (EObject x1_a1pxO x2_a1pxP)
  = pure (EObject x1_a1pxO x2_a1pxP)
eAtom _ (EBinding x1_a1pxQ x2_a1pxR)
  = pure (EBinding x1_a1pxQ x2_a1pxR)
{-# INLINE eAtom #-}
eBinding :: Traversal' Exp [(Exp, Exp)]
eBinding _ (ELiteral x1_a1pxS x2_a1pxT)
  = pure (ELiteral x1_a1pxS x2_a1pxT)
eBinding _ (ESymbol x1_a1pxU x2_a1pxV)
  = pure (ESymbol x1_a1pxU x2_a1pxV)
eBinding _ (EAtom x1_a1pxW x2_a1pxX x3_a1pxY x4_a1pxZ)
  = pure (EAtom x1_a1pxW x2_a1pxX x3_a1pxY x4_a1pxZ)
eBinding _ (EList x1_a1py0 x2_a1py1 x3_a1py2)
  = pure (EList x1_a1py0 x2_a1py1 x3_a1py2)
eBinding _ (EObject x1_a1py3 x2_a1py4)
  = pure (EObject x1_a1py3 x2_a1py4)
eBinding f_a1py5 (EBinding x1_a1py6 x2_a1py7)
  = fmap
      (\ y1_a1py8 -> EBinding y1_a1py8 x2_a1py7) (f_a1py5 x1_a1py6)
{-# INLINE eBinding #-}
eList :: Traversal' Exp [Exp]
eList _ (ELiteral x1_a1py9 x2_a1pya)
  = pure (ELiteral x1_a1py9 x2_a1pya)
eList _ (ESymbol x1_a1pyb x2_a1pyc)
  = pure (ESymbol x1_a1pyb x2_a1pyc)
eList _ (EAtom x1_a1pyd x2_a1pye x3_a1pyf x4_a1pyg)
  = pure (EAtom x1_a1pyd x2_a1pye x3_a1pyf x4_a1pyg)
eList f_a1pyh (EList x1_a1pyi x2_a1pyj x3_a1pyk)
  = fmap
      (\ y1_a1pyl -> EList y1_a1pyl x2_a1pyj x3_a1pyk) (f_a1pyh x1_a1pyi)
eList _ (EObject x1_a1pym x2_a1pyn)
  = pure (EObject x1_a1pym x2_a1pyn)
eList _ (EBinding x1_a1pyo x2_a1pyp)
  = pure (EBinding x1_a1pyo x2_a1pyp)
{-# INLINE eList #-}
eLitListType :: Traversal' Exp (Maybe (Type TypeName))
eLitListType _ (ELiteral x1_a1pyq x2_a1pyr)
  = pure (ELiteral x1_a1pyq x2_a1pyr)
eLitListType _ (ESymbol x1_a1pys x2_a1pyt)
  = pure (ESymbol x1_a1pys x2_a1pyt)
eLitListType _ (EAtom x1_a1pyu x2_a1pyv x3_a1pyw x4_a1pyx)
  = pure (EAtom x1_a1pyu x2_a1pyv x3_a1pyw x4_a1pyx)
eLitListType f_a1pyy (EList x1_a1pyz x2_a1pyA x3_a1pyB)
  = fmap
      (\ y1_a1pyC -> EList x1_a1pyz y1_a1pyC x3_a1pyB) (f_a1pyy x2_a1pyA)
eLitListType _ (EObject x1_a1pyD x2_a1pyE)
  = pure (EObject x1_a1pyD x2_a1pyE)
eLitListType _ (EBinding x1_a1pyF x2_a1pyG)
  = pure (EBinding x1_a1pyF x2_a1pyG)
{-# INLINE eLitListType #-}
eLiteral :: Traversal' Exp Literal
eLiteral f_a1pyH (ELiteral x1_a1pyI x2_a1pyJ)
  = fmap
      (\ y1_a1pyK -> ELiteral y1_a1pyK x2_a1pyJ) (f_a1pyH x1_a1pyI)
eLiteral _ (ESymbol x1_a1pyL x2_a1pyM)
  = pure (ESymbol x1_a1pyL x2_a1pyM)
eLiteral _ (EAtom x1_a1pyN x2_a1pyO x3_a1pyP x4_a1pyQ)
  = pure (EAtom x1_a1pyN x2_a1pyO x3_a1pyP x4_a1pyQ)
eLiteral _ (EList x1_a1pyR x2_a1pyS x3_a1pyT)
  = pure (EList x1_a1pyR x2_a1pyS x3_a1pyT)
eLiteral _ (EObject x1_a1pyU x2_a1pyV)
  = pure (EObject x1_a1pyU x2_a1pyV)
eLiteral _ (EBinding x1_a1pyW x2_a1pyX)
  = pure (EBinding x1_a1pyW x2_a1pyX)
{-# INLINE eLiteral #-}
eObject :: Traversal' Exp [(Exp, Exp)]
eObject _ (ELiteral x1_a1pyY x2_a1pyZ)
  = pure (ELiteral x1_a1pyY x2_a1pyZ)
eObject _ (ESymbol x1_a1pz0 x2_a1pz1)
  = pure (ESymbol x1_a1pz0 x2_a1pz1)
eObject _ (EAtom x1_a1pz2 x2_a1pz3 x3_a1pz4 x4_a1pz5)
  = pure (EAtom x1_a1pz2 x2_a1pz3 x3_a1pz4 x4_a1pz5)
eObject _ (EList x1_a1pz6 x2_a1pz7 x3_a1pz8)
  = pure (EList x1_a1pz6 x2_a1pz7 x3_a1pz8)
eObject f_a1pz9 (EObject x1_a1pza x2_a1pzb)
  = fmap (\ y1_a1pzc -> EObject y1_a1pzc x2_a1pzb) (f_a1pz9 x1_a1pza)
eObject _ (EBinding x1_a1pzd x2_a1pze)
  = pure (EBinding x1_a1pzd x2_a1pze)
{-# INLINE eObject #-}
eParsed :: Lens' Exp Parsed
eParsed f_a1pzf (ELiteral x1_a1pzg x2_a1pzh)
  = fmap
      (\ y1_a1pzi -> ELiteral x1_a1pzg y1_a1pzi) (f_a1pzf x2_a1pzh)
eParsed f_a1pzj (ESymbol x1_a1pzk x2_a1pzl)
  = fmap (\ y1_a1pzm -> ESymbol x1_a1pzk y1_a1pzm) (f_a1pzj x2_a1pzl)
eParsed f_a1pzn (EAtom x1_a1pzo x2_a1pzp x3_a1pzq x4_a1pzr)
  = fmap
      (\ y1_a1pzs -> EAtom x1_a1pzo x2_a1pzp x3_a1pzq y1_a1pzs)
      (f_a1pzn x4_a1pzr)
eParsed f_a1pzt (EList x1_a1pzu x2_a1pzv x3_a1pzw)
  = fmap
      (\ y1_a1pzx -> EList x1_a1pzu x2_a1pzv y1_a1pzx) (f_a1pzt x3_a1pzw)
eParsed f_a1pzy (EObject x1_a1pzz x2_a1pzA)
  = fmap (\ y1_a1pzB -> EObject x1_a1pzz y1_a1pzB) (f_a1pzy x2_a1pzA)
eParsed f_a1pzC (EBinding x1_a1pzD x2_a1pzE)
  = fmap
      (\ y1_a1pzF -> EBinding x1_a1pzD y1_a1pzF) (f_a1pzC x2_a1pzE)
{-# INLINE eParsed #-}
eQualifier :: Traversal' Exp (Maybe Text)
eQualifier _ (ELiteral x1_a1pzG x2_a1pzH)
  = pure (ELiteral x1_a1pzG x2_a1pzH)
eQualifier _ (ESymbol x1_a1pzI x2_a1pzJ)
  = pure (ESymbol x1_a1pzI x2_a1pzJ)
eQualifier f_a1pzK (EAtom x1_a1pzL x2_a1pzM x3_a1pzN x4_a1pzO)
  = fmap
      (\ y1_a1pzP -> EAtom x1_a1pzL y1_a1pzP x3_a1pzN x4_a1pzO)
      (f_a1pzK x2_a1pzM)
eQualifier _ (EList x1_a1pzQ x2_a1pzR x3_a1pzS)
  = pure (EList x1_a1pzQ x2_a1pzR x3_a1pzS)
eQualifier _ (EObject x1_a1pzT x2_a1pzU)
  = pure (EObject x1_a1pzT x2_a1pzU)
eQualifier _ (EBinding x1_a1pzV x2_a1pzW)
  = pure (EBinding x1_a1pzV x2_a1pzW)
{-# INLINE eQualifier #-}
eSymbol :: Traversal' Exp Text
eSymbol _ (ELiteral x1_a1pzX x2_a1pzY)
  = pure (ELiteral x1_a1pzX x2_a1pzY)
eSymbol f_a1pzZ (ESymbol x1_a1pA0 x2_a1pA1)
  = fmap (\ y1_a1pA2 -> ESymbol y1_a1pA2 x2_a1pA1) (f_a1pzZ x1_a1pA0)
eSymbol _ (EAtom x1_a1pA3 x2_a1pA4 x3_a1pA5 x4_a1pA6)
  = pure (EAtom x1_a1pA3 x2_a1pA4 x3_a1pA5 x4_a1pA6)
eSymbol _ (EList x1_a1pA7 x2_a1pA8 x3_a1pA9)
  = pure (EList x1_a1pA7 x2_a1pA8 x3_a1pA9)
eSymbol _ (EObject x1_a1pAa x2_a1pAb)
  = pure (EObject x1_a1pAa x2_a1pAb)
eSymbol _ (EBinding x1_a1pAc x2_a1pAd)
  = pure (EBinding x1_a1pAc x2_a1pAd)
{-# INLINE eSymbol #-}
eType :: Traversal' Exp (Maybe (Type TypeName))
eType _ (ELiteral x1_a1pAe x2_a1pAf)
  = pure (ELiteral x1_a1pAe x2_a1pAf)
eType _ (ESymbol x1_a1pAg x2_a1pAh)
  = pure (ESymbol x1_a1pAg x2_a1pAh)
eType f_a1pAi (EAtom x1_a1pAj x2_a1pAk x3_a1pAl x4_a1pAm)
  = fmap
      (\ y1_a1pAn -> EAtom x1_a1pAj x2_a1pAk y1_a1pAn x4_a1pAm)
      (f_a1pAi x3_a1pAl)
eType _ (EList x1_a1pAo x2_a1pAp x3_a1pAq)
  = pure (EList x1_a1pAo x2_a1pAp x3_a1pAq)
eType _ (EObject x1_a1pAr x2_a1pAs)
  = pure (EObject x1_a1pAr x2_a1pAs)
eType _ (EBinding x1_a1pAt x2_a1pAu)
  = pure (EBinding x1_a1pAt x2_a1pAu)
{-# INLINE eType #-}


------------------------------------------------------------------------------
--makeLenses ''Term
tAppArgs ::
  forall n_a1pCA. Traversal' (Term n_a1pCA) [Term n_a1pCA]
tAppArgs _ (TModule x1_a1sST x2_a1sSU x3_a1sSV)
  = pure (TModule x1_a1sST x2_a1sSU x3_a1sSV)
tAppArgs _ (TList x1_a1sSW x2_a1sSX x3_a1sSY)
  = pure (TList x1_a1sSW x2_a1sSX x3_a1sSY)
tAppArgs
  _
  (TDef x1_a1sSZ
        x2_a1sT0
        x3_a1sT1
        x4_a1sT2
        x5_a1sT3
        x6_a1sT4
        x7_a1sT5)
  = pure
      (TDef
         x1_a1sSZ x2_a1sT0 x3_a1sT1 x4_a1sT2 x5_a1sT3 x6_a1sT4 x7_a1sT5)
tAppArgs _ (TNative x1_a1sT6 x2_a1sT7 x3_a1sT8 x4_a1sT9 x5_a1sTa)
  = pure (TNative x1_a1sT6 x2_a1sT7 x3_a1sT8 x4_a1sT9 x5_a1sTa)
tAppArgs _ (TConst x1_a1sTb x2_a1sTc x3_a1sTd x4_a1sTe x5_a1sTf)
  = pure (TConst x1_a1sTb x2_a1sTc x3_a1sTd x4_a1sTe x5_a1sTf)
tAppArgs f_a1sTg (TApp x1_a1sTh x2_a1sTi x3_a1sTj)
  = fmap
      (\ y1_a1sTk -> TApp x1_a1sTh y1_a1sTk x3_a1sTj) (f_a1sTg x2_a1sTi)
tAppArgs _ (TVar x1_a1sTl x2_a1sTm) = pure (TVar x1_a1sTl x2_a1sTm)
tAppArgs _ (TBinding x1_a1sTn x2_a1sTo x3_a1sTp x4_a1sTq)
  = pure (TBinding x1_a1sTn x2_a1sTo x3_a1sTp x4_a1sTq)
tAppArgs _ (TObject x1_a1sTr x2_a1sTs x3_a1sTt)
  = pure (TObject x1_a1sTr x2_a1sTs x3_a1sTt)
tAppArgs _ (TSchema x1_a1sTu x2_a1sTv x3_a1sTw x4_a1sTx x5_a1sTy)
  = pure (TSchema x1_a1sTu x2_a1sTv x3_a1sTw x4_a1sTx x5_a1sTy)
tAppArgs _ (TLiteral x1_a1sTz x2_a1sTA)
  = pure (TLiteral x1_a1sTz x2_a1sTA)
tAppArgs _ (TKeySet x1_a1sTB x2_a1sTC)
  = pure (TKeySet x1_a1sTB x2_a1sTC)
tAppArgs _ (TUse x1_a1sTD x2_a1sTE x3_a1sTF)
  = pure (TUse x1_a1sTD x2_a1sTE x3_a1sTF)
tAppArgs _ (TValue x1_a1sTG x2_a1sTH)
  = pure (TValue x1_a1sTG x2_a1sTH)
tAppArgs _ (TStep x1_a1sTI x2_a1sTJ x3_a1sTK x4_a1sTL)
  = pure (TStep x1_a1sTI x2_a1sTJ x3_a1sTK x4_a1sTL)
tAppArgs _ (TTable x1_a1sTM x2_a1sTN x3_a1sTO x4_a1sTP x5_a1sTQ)
  = pure (TTable x1_a1sTM x2_a1sTN x3_a1sTO x4_a1sTP x5_a1sTQ)
{-# INLINE tAppArgs #-}
tAppFun :: forall n_a1pCA. Traversal' (Term n_a1pCA) (Term n_a1pCA)
tAppFun _ (TModule x1_a1sTR x2_a1sTS x3_a1sTT)
  = pure (TModule x1_a1sTR x2_a1sTS x3_a1sTT)
tAppFun _ (TList x1_a1sTU x2_a1sTV x3_a1sTW)
  = pure (TList x1_a1sTU x2_a1sTV x3_a1sTW)
tAppFun
  _
  (TDef x1_a1sTX
        x2_a1sTY
        x3_a1sTZ
        x4_a1sU0
        x5_a1sU1
        x6_a1sU2
        x7_a1sU3)
  = pure
      (TDef
         x1_a1sTX x2_a1sTY x3_a1sTZ x4_a1sU0 x5_a1sU1 x6_a1sU2 x7_a1sU3)
tAppFun _ (TNative x1_a1sU4 x2_a1sU5 x3_a1sU6 x4_a1sU7 x5_a1sU8)
  = pure (TNative x1_a1sU4 x2_a1sU5 x3_a1sU6 x4_a1sU7 x5_a1sU8)
tAppFun _ (TConst x1_a1sU9 x2_a1sUa x3_a1sUb x4_a1sUc x5_a1sUd)
  = pure (TConst x1_a1sU9 x2_a1sUa x3_a1sUb x4_a1sUc x5_a1sUd)
tAppFun f_a1sUe (TApp x1_a1sUf x2_a1sUg x3_a1sUh)
  = fmap
      (\ y1_a1sUi -> TApp y1_a1sUi x2_a1sUg x3_a1sUh) (f_a1sUe x1_a1sUf)
tAppFun _ (TVar x1_a1sUj x2_a1sUk) = pure (TVar x1_a1sUj x2_a1sUk)
tAppFun _ (TBinding x1_a1sUl x2_a1sUm x3_a1sUn x4_a1sUo)
  = pure (TBinding x1_a1sUl x2_a1sUm x3_a1sUn x4_a1sUo)
tAppFun _ (TObject x1_a1sUp x2_a1sUq x3_a1sUr)
  = pure (TObject x1_a1sUp x2_a1sUq x3_a1sUr)
tAppFun _ (TSchema x1_a1sUs x2_a1sUt x3_a1sUu x4_a1sUv x5_a1sUw)
  = pure (TSchema x1_a1sUs x2_a1sUt x3_a1sUu x4_a1sUv x5_a1sUw)
tAppFun _ (TLiteral x1_a1sUx x2_a1sUy)
  = pure (TLiteral x1_a1sUx x2_a1sUy)
tAppFun _ (TKeySet x1_a1sUz x2_a1sUA)
  = pure (TKeySet x1_a1sUz x2_a1sUA)
tAppFun _ (TUse x1_a1sUB x2_a1sUC x3_a1sUD)
  = pure (TUse x1_a1sUB x2_a1sUC x3_a1sUD)
tAppFun _ (TValue x1_a1sUE x2_a1sUF)
  = pure (TValue x1_a1sUE x2_a1sUF)
tAppFun _ (TStep x1_a1sUG x2_a1sUH x3_a1sUI x4_a1sUJ)
  = pure (TStep x1_a1sUG x2_a1sUH x3_a1sUI x4_a1sUJ)
tAppFun _ (TTable x1_a1sUK x2_a1sUL x3_a1sUM x4_a1sUN x5_a1sUO)
  = pure (TTable x1_a1sUK x2_a1sUL x3_a1sUM x4_a1sUN x5_a1sUO)
{-# INLINE tAppFun #-}
tBindBody ::
  forall n_a1pCA. Traversal' (Term n_a1pCA) (Scope Int Term n_a1pCA)
tBindBody _ (TModule x1_a1sUP x2_a1sUQ x3_a1sUR)
  = pure (TModule x1_a1sUP x2_a1sUQ x3_a1sUR)
tBindBody _ (TList x1_a1sUS x2_a1sUT x3_a1sUU)
  = pure (TList x1_a1sUS x2_a1sUT x3_a1sUU)
tBindBody
  _
  (TDef x1_a1sUV
        x2_a1sUW
        x3_a1sUX
        x4_a1sUY
        x5_a1sUZ
        x6_a1sV0
        x7_a1sV1)
  = pure
      (TDef
         x1_a1sUV x2_a1sUW x3_a1sUX x4_a1sUY x5_a1sUZ x6_a1sV0 x7_a1sV1)
tBindBody _ (TNative x1_a1sV2 x2_a1sV3 x3_a1sV4 x4_a1sV5 x5_a1sV6)
  = pure (TNative x1_a1sV2 x2_a1sV3 x3_a1sV4 x4_a1sV5 x5_a1sV6)
tBindBody _ (TConst x1_a1sV7 x2_a1sV8 x3_a1sV9 x4_a1sVa x5_a1sVb)
  = pure (TConst x1_a1sV7 x2_a1sV8 x3_a1sV9 x4_a1sVa x5_a1sVb)
tBindBody _ (TApp x1_a1sVc x2_a1sVd x3_a1sVe)
  = pure (TApp x1_a1sVc x2_a1sVd x3_a1sVe)
tBindBody _ (TVar x1_a1sVf x2_a1sVg)
  = pure (TVar x1_a1sVf x2_a1sVg)
tBindBody f_a1sVh (TBinding x1_a1sVi x2_a1sVj x3_a1sVk x4_a1sVl)
  = fmap
      (\ y1_a1sVm -> TBinding x1_a1sVi y1_a1sVm x3_a1sVk x4_a1sVl)
      (f_a1sVh x2_a1sVj)
tBindBody _ (TObject x1_a1sVn x2_a1sVo x3_a1sVp)
  = pure (TObject x1_a1sVn x2_a1sVo x3_a1sVp)
tBindBody _ (TSchema x1_a1sVq x2_a1sVr x3_a1sVs x4_a1sVt x5_a1sVu)
  = pure (TSchema x1_a1sVq x2_a1sVr x3_a1sVs x4_a1sVt x5_a1sVu)
tBindBody _ (TLiteral x1_a1sVv x2_a1sVw)
  = pure (TLiteral x1_a1sVv x2_a1sVw)
tBindBody _ (TKeySet x1_a1sVx x2_a1sVy)
  = pure (TKeySet x1_a1sVx x2_a1sVy)
tBindBody _ (TUse x1_a1sVz x2_a1sVA x3_a1sVB)
  = pure (TUse x1_a1sVz x2_a1sVA x3_a1sVB)
tBindBody _ (TValue x1_a1sVC x2_a1sVD)
  = pure (TValue x1_a1sVC x2_a1sVD)
tBindBody _ (TStep x1_a1sVE x2_a1sVF x3_a1sVG x4_a1sVH)
  = pure (TStep x1_a1sVE x2_a1sVF x3_a1sVG x4_a1sVH)
tBindBody _ (TTable x1_a1sVI x2_a1sVJ x3_a1sVK x4_a1sVL x5_a1sVM)
  = pure (TTable x1_a1sVI x2_a1sVJ x3_a1sVK x4_a1sVL x5_a1sVM)
{-# INLINE tBindBody #-}
tBindPairs ::
  forall n_a1pCA.
  Traversal' (Term n_a1pCA) [(Arg (Term n_a1pCA), Term n_a1pCA)]
tBindPairs _ (TModule x1_a1sVN x2_a1sVO x3_a1sVP)
  = pure (TModule x1_a1sVN x2_a1sVO x3_a1sVP)
tBindPairs _ (TList x1_a1sVQ x2_a1sVR x3_a1sVS)
  = pure (TList x1_a1sVQ x2_a1sVR x3_a1sVS)
tBindPairs
  _
  (TDef x1_a1sVT
        x2_a1sVU
        x3_a1sVV
        x4_a1sVW
        x5_a1sVX
        x6_a1sVY
        x7_a1sVZ)
  = pure
      (TDef
         x1_a1sVT x2_a1sVU x3_a1sVV x4_a1sVW x5_a1sVX x6_a1sVY x7_a1sVZ)
tBindPairs _ (TNative x1_a1sW0 x2_a1sW1 x3_a1sW2 x4_a1sW3 x5_a1sW4)
  = pure (TNative x1_a1sW0 x2_a1sW1 x3_a1sW2 x4_a1sW3 x5_a1sW4)
tBindPairs _ (TConst x1_a1sW5 x2_a1sW6 x3_a1sW7 x4_a1sW8 x5_a1sW9)
  = pure (TConst x1_a1sW5 x2_a1sW6 x3_a1sW7 x4_a1sW8 x5_a1sW9)
tBindPairs _ (TApp x1_a1sWa x2_a1sWb x3_a1sWc)
  = pure (TApp x1_a1sWa x2_a1sWb x3_a1sWc)
tBindPairs _ (TVar x1_a1sWd x2_a1sWe)
  = pure (TVar x1_a1sWd x2_a1sWe)
tBindPairs f_a1sWf (TBinding x1_a1sWg x2_a1sWh x3_a1sWi x4_a1sWj)
  = fmap
      (\ y1_a1sWk -> TBinding y1_a1sWk x2_a1sWh x3_a1sWi x4_a1sWj)
      (f_a1sWf x1_a1sWg)
tBindPairs _ (TObject x1_a1sWl x2_a1sWm x3_a1sWn)
  = pure (TObject x1_a1sWl x2_a1sWm x3_a1sWn)
tBindPairs _ (TSchema x1_a1sWo x2_a1sWp x3_a1sWq x4_a1sWr x5_a1sWs)
  = pure (TSchema x1_a1sWo x2_a1sWp x3_a1sWq x4_a1sWr x5_a1sWs)
tBindPairs _ (TLiteral x1_a1sWt x2_a1sWu)
  = pure (TLiteral x1_a1sWt x2_a1sWu)
tBindPairs _ (TKeySet x1_a1sWv x2_a1sWw)
  = pure (TKeySet x1_a1sWv x2_a1sWw)
tBindPairs _ (TUse x1_a1sWx x2_a1sWy x3_a1sWz)
  = pure (TUse x1_a1sWx x2_a1sWy x3_a1sWz)
tBindPairs _ (TValue x1_a1sWA x2_a1sWB)
  = pure (TValue x1_a1sWA x2_a1sWB)
tBindPairs _ (TStep x1_a1sWC x2_a1sWD x3_a1sWE x4_a1sWF)
  = pure (TStep x1_a1sWC x2_a1sWD x3_a1sWE x4_a1sWF)
tBindPairs _ (TTable x1_a1sWG x2_a1sWH x3_a1sWI x4_a1sWJ x5_a1sWK)
  = pure (TTable x1_a1sWG x2_a1sWH x3_a1sWI x4_a1sWJ x5_a1sWK)
{-# INLINE tBindPairs #-}
tBindType ::
  forall n_a1pCA.
  Traversal' (Term n_a1pCA) (BindType (Type (Term n_a1pCA)))
tBindType _ (TModule x1_a1sWL x2_a1sWM x3_a1sWN)
  = pure (TModule x1_a1sWL x2_a1sWM x3_a1sWN)
tBindType _ (TList x1_a1sWO x2_a1sWP x3_a1sWQ)
  = pure (TList x1_a1sWO x2_a1sWP x3_a1sWQ)
tBindType
  _
  (TDef x1_a1sWR
        x2_a1sWS
        x3_a1sWT
        x4_a1sWU
        x5_a1sWV
        x6_a1sWW
        x7_a1sWX)
  = pure
      (TDef
         x1_a1sWR x2_a1sWS x3_a1sWT x4_a1sWU x5_a1sWV x6_a1sWW x7_a1sWX)
tBindType _ (TNative x1_a1sWY x2_a1sWZ x3_a1sX0 x4_a1sX1 x5_a1sX2)
  = pure (TNative x1_a1sWY x2_a1sWZ x3_a1sX0 x4_a1sX1 x5_a1sX2)
tBindType _ (TConst x1_a1sX3 x2_a1sX4 x3_a1sX5 x4_a1sX6 x5_a1sX7)
  = pure (TConst x1_a1sX3 x2_a1sX4 x3_a1sX5 x4_a1sX6 x5_a1sX7)
tBindType _ (TApp x1_a1sX8 x2_a1sX9 x3_a1sXa)
  = pure (TApp x1_a1sX8 x2_a1sX9 x3_a1sXa)
tBindType _ (TVar x1_a1sXb x2_a1sXc)
  = pure (TVar x1_a1sXb x2_a1sXc)
tBindType f_a1sXd (TBinding x1_a1sXe x2_a1sXf x3_a1sXg x4_a1sXh)
  = fmap
      (\ y1_a1sXi -> TBinding x1_a1sXe x2_a1sXf y1_a1sXi x4_a1sXh)
      (f_a1sXd x3_a1sXg)
tBindType _ (TObject x1_a1sXj x2_a1sXk x3_a1sXl)
  = pure (TObject x1_a1sXj x2_a1sXk x3_a1sXl)
tBindType _ (TSchema x1_a1sXm x2_a1sXn x3_a1sXo x4_a1sXp x5_a1sXq)
  = pure (TSchema x1_a1sXm x2_a1sXn x3_a1sXo x4_a1sXp x5_a1sXq)
tBindType _ (TLiteral x1_a1sXr x2_a1sXs)
  = pure (TLiteral x1_a1sXr x2_a1sXs)
tBindType _ (TKeySet x1_a1sXt x2_a1sXu)
  = pure (TKeySet x1_a1sXt x2_a1sXu)
tBindType _ (TUse x1_a1sXv x2_a1sXw x3_a1sXx)
  = pure (TUse x1_a1sXv x2_a1sXw x3_a1sXx)
tBindType _ (TValue x1_a1sXy x2_a1sXz)
  = pure (TValue x1_a1sXy x2_a1sXz)
tBindType _ (TStep x1_a1sXA x2_a1sXB x3_a1sXC x4_a1sXD)
  = pure (TStep x1_a1sXA x2_a1sXB x3_a1sXC x4_a1sXD)
tBindType _ (TTable x1_a1sXE x2_a1sXF x3_a1sXG x4_a1sXH x5_a1sXI)
  = pure (TTable x1_a1sXE x2_a1sXF x3_a1sXG x4_a1sXH x5_a1sXI)
{-# INLINE tBindType #-}
tConstArg ::
  forall n_a1pCA. Traversal' (Term n_a1pCA) (Arg (Term n_a1pCA))
tConstArg _ (TModule x1_a1sXJ x2_a1sXK x3_a1sXL)
  = pure (TModule x1_a1sXJ x2_a1sXK x3_a1sXL)
tConstArg _ (TList x1_a1sXM x2_a1sXN x3_a1sXO)
  = pure (TList x1_a1sXM x2_a1sXN x3_a1sXO)
tConstArg
  _
  (TDef x1_a1sXP
        x2_a1sXQ
        x3_a1sXR
        x4_a1sXS
        x5_a1sXT
        x6_a1sXU
        x7_a1sXV)
  = pure
      (TDef
         x1_a1sXP x2_a1sXQ x3_a1sXR x4_a1sXS x5_a1sXT x6_a1sXU x7_a1sXV)
tConstArg _ (TNative x1_a1sXW x2_a1sXX x3_a1sXY x4_a1sXZ x5_a1sY0)
  = pure (TNative x1_a1sXW x2_a1sXX x3_a1sXY x4_a1sXZ x5_a1sY0)
tConstArg
  f_a1sY1
  (TConst x1_a1sY2 x2_a1sY3 x3_a1sY4 x4_a1sY5 x5_a1sY6)
  = fmap
      (\ y1_a1sY7 -> TConst y1_a1sY7 x2_a1sY3 x3_a1sY4 x4_a1sY5 x5_a1sY6)
      (f_a1sY1 x1_a1sY2)
tConstArg _ (TApp x1_a1sY8 x2_a1sY9 x3_a1sYa)
  = pure (TApp x1_a1sY8 x2_a1sY9 x3_a1sYa)
tConstArg _ (TVar x1_a1sYb x2_a1sYc)
  = pure (TVar x1_a1sYb x2_a1sYc)
tConstArg _ (TBinding x1_a1sYd x2_a1sYe x3_a1sYf x4_a1sYg)
  = pure (TBinding x1_a1sYd x2_a1sYe x3_a1sYf x4_a1sYg)
tConstArg _ (TObject x1_a1sYh x2_a1sYi x3_a1sYj)
  = pure (TObject x1_a1sYh x2_a1sYi x3_a1sYj)
tConstArg _ (TSchema x1_a1sYk x2_a1sYl x3_a1sYm x4_a1sYn x5_a1sYo)
  = pure (TSchema x1_a1sYk x2_a1sYl x3_a1sYm x4_a1sYn x5_a1sYo)
tConstArg _ (TLiteral x1_a1sYp x2_a1sYq)
  = pure (TLiteral x1_a1sYp x2_a1sYq)
tConstArg _ (TKeySet x1_a1sYr x2_a1sYs)
  = pure (TKeySet x1_a1sYr x2_a1sYs)
tConstArg _ (TUse x1_a1sYt x2_a1sYu x3_a1sYv)
  = pure (TUse x1_a1sYt x2_a1sYu x3_a1sYv)
tConstArg _ (TValue x1_a1sYw x2_a1sYx)
  = pure (TValue x1_a1sYw x2_a1sYx)
tConstArg _ (TStep x1_a1sYy x2_a1sYz x3_a1sYA x4_a1sYB)
  = pure (TStep x1_a1sYy x2_a1sYz x3_a1sYA x4_a1sYB)
tConstArg _ (TTable x1_a1sYC x2_a1sYD x3_a1sYE x4_a1sYF x5_a1sYG)
  = pure (TTable x1_a1sYC x2_a1sYD x3_a1sYE x4_a1sYF x5_a1sYG)
{-# INLINE tConstArg #-}
tConstVal ::
  forall n_a1pCA. Traversal' (Term n_a1pCA) (ConstVal (Term n_a1pCA))
tConstVal _ (TModule x1_a1sYH x2_a1sYI x3_a1sYJ)
  = pure (TModule x1_a1sYH x2_a1sYI x3_a1sYJ)
tConstVal _ (TList x1_a1sYK x2_a1sYL x3_a1sYM)
  = pure (TList x1_a1sYK x2_a1sYL x3_a1sYM)
tConstVal
  _
  (TDef x1_a1sYN
        x2_a1sYO
        x3_a1sYP
        x4_a1sYQ
        x5_a1sYR
        x6_a1sYS
        x7_a1sYT)
  = pure
      (TDef
         x1_a1sYN x2_a1sYO x3_a1sYP x4_a1sYQ x5_a1sYR x6_a1sYS x7_a1sYT)
tConstVal _ (TNative x1_a1sYU x2_a1sYV x3_a1sYW x4_a1sYX x5_a1sYY)
  = pure (TNative x1_a1sYU x2_a1sYV x3_a1sYW x4_a1sYX x5_a1sYY)
tConstVal
  f_a1sYZ
  (TConst x1_a1sZ0 x2_a1sZ1 x3_a1sZ2 x4_a1sZ3 x5_a1sZ4)
  = fmap
      (\ y1_a1sZ5 -> TConst x1_a1sZ0 x2_a1sZ1 y1_a1sZ5 x4_a1sZ3 x5_a1sZ4)
      (f_a1sYZ x3_a1sZ2)
tConstVal _ (TApp x1_a1sZ6 x2_a1sZ7 x3_a1sZ8)
  = pure (TApp x1_a1sZ6 x2_a1sZ7 x3_a1sZ8)
tConstVal _ (TVar x1_a1sZ9 x2_a1sZa)
  = pure (TVar x1_a1sZ9 x2_a1sZa)
tConstVal _ (TBinding x1_a1sZb x2_a1sZc x3_a1sZd x4_a1sZe)
  = pure (TBinding x1_a1sZb x2_a1sZc x3_a1sZd x4_a1sZe)
tConstVal _ (TObject x1_a1sZf x2_a1sZg x3_a1sZh)
  = pure (TObject x1_a1sZf x2_a1sZg x3_a1sZh)
tConstVal _ (TSchema x1_a1sZi x2_a1sZj x3_a1sZk x4_a1sZl x5_a1sZm)
  = pure (TSchema x1_a1sZi x2_a1sZj x3_a1sZk x4_a1sZl x5_a1sZm)
tConstVal _ (TLiteral x1_a1sZn x2_a1sZo)
  = pure (TLiteral x1_a1sZn x2_a1sZo)
tConstVal _ (TKeySet x1_a1sZp x2_a1sZq)
  = pure (TKeySet x1_a1sZp x2_a1sZq)
tConstVal _ (TUse x1_a1sZr x2_a1sZs x3_a1sZt)
  = pure (TUse x1_a1sZr x2_a1sZs x3_a1sZt)
tConstVal _ (TValue x1_a1sZu x2_a1sZv)
  = pure (TValue x1_a1sZu x2_a1sZv)
tConstVal _ (TStep x1_a1sZw x2_a1sZx x3_a1sZy x4_a1sZz)
  = pure (TStep x1_a1sZw x2_a1sZx x3_a1sZy x4_a1sZz)
tConstVal _ (TTable x1_a1sZA x2_a1sZB x3_a1sZC x4_a1sZD x5_a1sZE)
  = pure (TTable x1_a1sZA x2_a1sZB x3_a1sZC x4_a1sZD x5_a1sZE)
{-# INLINE tConstVal #-}
tDefBody ::
  forall n_a1pCA. Traversal' (Term n_a1pCA) (Scope Int Term n_a1pCA)
tDefBody _ (TModule x1_a1sZF x2_a1sZG x3_a1sZH)
  = pure (TModule x1_a1sZF x2_a1sZG x3_a1sZH)
tDefBody _ (TList x1_a1sZI x2_a1sZJ x3_a1sZK)
  = pure (TList x1_a1sZI x2_a1sZJ x3_a1sZK)
tDefBody
  f_a1sZL
  (TDef x1_a1sZM
        x2_a1sZN
        x3_a1sZO
        x4_a1sZP
        x5_a1sZQ
        x6_a1sZR
        x7_a1sZS)
  = fmap
      (\ y1_a1sZT
         -> TDef
              x1_a1sZM x2_a1sZN x3_a1sZO x4_a1sZP y1_a1sZT x6_a1sZR x7_a1sZS)
      (f_a1sZL x5_a1sZQ)
tDefBody _ (TNative x1_a1sZU x2_a1sZV x3_a1sZW x4_a1sZX x5_a1sZY)
  = pure (TNative x1_a1sZU x2_a1sZV x3_a1sZW x4_a1sZX x5_a1sZY)
tDefBody _ (TConst x1_a1sZZ x2_a1t00 x3_a1t01 x4_a1t02 x5_a1t03)
  = pure (TConst x1_a1sZZ x2_a1t00 x3_a1t01 x4_a1t02 x5_a1t03)
tDefBody _ (TApp x1_a1t04 x2_a1t05 x3_a1t06)
  = pure (TApp x1_a1t04 x2_a1t05 x3_a1t06)
tDefBody _ (TVar x1_a1t07 x2_a1t08) = pure (TVar x1_a1t07 x2_a1t08)
tDefBody _ (TBinding x1_a1t09 x2_a1t0a x3_a1t0b x4_a1t0c)
  = pure (TBinding x1_a1t09 x2_a1t0a x3_a1t0b x4_a1t0c)
tDefBody _ (TObject x1_a1t0d x2_a1t0e x3_a1t0f)
  = pure (TObject x1_a1t0d x2_a1t0e x3_a1t0f)
tDefBody _ (TSchema x1_a1t0g x2_a1t0h x3_a1t0i x4_a1t0j x5_a1t0k)
  = pure (TSchema x1_a1t0g x2_a1t0h x3_a1t0i x4_a1t0j x5_a1t0k)
tDefBody _ (TLiteral x1_a1t0l x2_a1t0m)
  = pure (TLiteral x1_a1t0l x2_a1t0m)
tDefBody _ (TKeySet x1_a1t0n x2_a1t0o)
  = pure (TKeySet x1_a1t0n x2_a1t0o)
tDefBody _ (TUse x1_a1t0p x2_a1t0q x3_a1t0r)
  = pure (TUse x1_a1t0p x2_a1t0q x3_a1t0r)
tDefBody _ (TValue x1_a1t0s x2_a1t0t)
  = pure (TValue x1_a1t0s x2_a1t0t)
tDefBody _ (TStep x1_a1t0u x2_a1t0v x3_a1t0w x4_a1t0x)
  = pure (TStep x1_a1t0u x2_a1t0v x3_a1t0w x4_a1t0x)
tDefBody _ (TTable x1_a1t0y x2_a1t0z x3_a1t0A x4_a1t0B x5_a1t0C)
  = pure (TTable x1_a1t0y x2_a1t0z x3_a1t0A x4_a1t0B x5_a1t0C)
{-# INLINE tDefBody #-}
tDefName :: forall n_a1pCA. Traversal' (Term n_a1pCA) Text
tDefName _ (TModule x1_a1t0D x2_a1t0E x3_a1t0F)
  = pure (TModule x1_a1t0D x2_a1t0E x3_a1t0F)
tDefName _ (TList x1_a1t0G x2_a1t0H x3_a1t0I)
  = pure (TList x1_a1t0G x2_a1t0H x3_a1t0I)
tDefName
  f_a1t0J
  (TDef x1_a1t0K
        x2_a1t0L
        x3_a1t0M
        x4_a1t0N
        x5_a1t0O
        x6_a1t0P
        x7_a1t0Q)
  = fmap
      (\ y1_a1t0R
         -> TDef
              y1_a1t0R x2_a1t0L x3_a1t0M x4_a1t0N x5_a1t0O x6_a1t0P x7_a1t0Q)
      (f_a1t0J x1_a1t0K)
tDefName _ (TNative x1_a1t0S x2_a1t0T x3_a1t0U x4_a1t0V x5_a1t0W)
  = pure (TNative x1_a1t0S x2_a1t0T x3_a1t0U x4_a1t0V x5_a1t0W)
tDefName _ (TConst x1_a1t0X x2_a1t0Y x3_a1t0Z x4_a1t10 x5_a1t11)
  = pure (TConst x1_a1t0X x2_a1t0Y x3_a1t0Z x4_a1t10 x5_a1t11)
tDefName _ (TApp x1_a1t12 x2_a1t13 x3_a1t14)
  = pure (TApp x1_a1t12 x2_a1t13 x3_a1t14)
tDefName _ (TVar x1_a1t15 x2_a1t16) = pure (TVar x1_a1t15 x2_a1t16)
tDefName _ (TBinding x1_a1t17 x2_a1t18 x3_a1t19 x4_a1t1a)
  = pure (TBinding x1_a1t17 x2_a1t18 x3_a1t19 x4_a1t1a)
tDefName _ (TObject x1_a1t1b x2_a1t1c x3_a1t1d)
  = pure (TObject x1_a1t1b x2_a1t1c x3_a1t1d)
tDefName _ (TSchema x1_a1t1e x2_a1t1f x3_a1t1g x4_a1t1h x5_a1t1i)
  = pure (TSchema x1_a1t1e x2_a1t1f x3_a1t1g x4_a1t1h x5_a1t1i)
tDefName _ (TLiteral x1_a1t1j x2_a1t1k)
  = pure (TLiteral x1_a1t1j x2_a1t1k)
tDefName _ (TKeySet x1_a1t1l x2_a1t1m)
  = pure (TKeySet x1_a1t1l x2_a1t1m)
tDefName _ (TUse x1_a1t1n x2_a1t1o x3_a1t1p)
  = pure (TUse x1_a1t1n x2_a1t1o x3_a1t1p)
tDefName _ (TValue x1_a1t1q x2_a1t1r)
  = pure (TValue x1_a1t1q x2_a1t1r)
tDefName _ (TStep x1_a1t1s x2_a1t1t x3_a1t1u x4_a1t1v)
  = pure (TStep x1_a1t1s x2_a1t1t x3_a1t1u x4_a1t1v)
tDefName _ (TTable x1_a1t1w x2_a1t1x x3_a1t1y x4_a1t1z x5_a1t1A)
  = pure (TTable x1_a1t1w x2_a1t1x x3_a1t1y x4_a1t1z x5_a1t1A)
{-# INLINE tDefName #-}
tDefType :: forall n_a1pCA. Traversal' (Term n_a1pCA) DefType
tDefType _ (TModule x1_a1t1B x2_a1t1C x3_a1t1D)
  = pure (TModule x1_a1t1B x2_a1t1C x3_a1t1D)
tDefType _ (TList x1_a1t1E x2_a1t1F x3_a1t1G)
  = pure (TList x1_a1t1E x2_a1t1F x3_a1t1G)
tDefType
  f_a1t1H
  (TDef x1_a1t1I
        x2_a1t1J
        x3_a1t1K
        x4_a1t1L
        x5_a1t1M
        x6_a1t1N
        x7_a1t1O)
  = fmap
      (\ y1_a1t1P
         -> TDef
              x1_a1t1I x2_a1t1J y1_a1t1P x4_a1t1L x5_a1t1M x6_a1t1N x7_a1t1O)
      (f_a1t1H x3_a1t1K)
tDefType _ (TNative x1_a1t1Q x2_a1t1R x3_a1t1S x4_a1t1T x5_a1t1U)
  = pure (TNative x1_a1t1Q x2_a1t1R x3_a1t1S x4_a1t1T x5_a1t1U)
tDefType _ (TConst x1_a1t1V x2_a1t1W x3_a1t1X x4_a1t1Y x5_a1t1Z)
  = pure (TConst x1_a1t1V x2_a1t1W x3_a1t1X x4_a1t1Y x5_a1t1Z)
tDefType _ (TApp x1_a1t20 x2_a1t21 x3_a1t22)
  = pure (TApp x1_a1t20 x2_a1t21 x3_a1t22)
tDefType _ (TVar x1_a1t23 x2_a1t24) = pure (TVar x1_a1t23 x2_a1t24)
tDefType _ (TBinding x1_a1t25 x2_a1t26 x3_a1t27 x4_a1t28)
  = pure (TBinding x1_a1t25 x2_a1t26 x3_a1t27 x4_a1t28)
tDefType _ (TObject x1_a1t29 x2_a1t2a x3_a1t2b)
  = pure (TObject x1_a1t29 x2_a1t2a x3_a1t2b)
tDefType _ (TSchema x1_a1t2c x2_a1t2d x3_a1t2e x4_a1t2f x5_a1t2g)
  = pure (TSchema x1_a1t2c x2_a1t2d x3_a1t2e x4_a1t2f x5_a1t2g)
tDefType _ (TLiteral x1_a1t2h x2_a1t2i)
  = pure (TLiteral x1_a1t2h x2_a1t2i)
tDefType _ (TKeySet x1_a1t2j x2_a1t2k)
  = pure (TKeySet x1_a1t2j x2_a1t2k)
tDefType _ (TUse x1_a1t2l x2_a1t2m x3_a1t2n)
  = pure (TUse x1_a1t2l x2_a1t2m x3_a1t2n)
tDefType _ (TValue x1_a1t2o x2_a1t2p)
  = pure (TValue x1_a1t2o x2_a1t2p)
tDefType _ (TStep x1_a1t2q x2_a1t2r x3_a1t2s x4_a1t2t)
  = pure (TStep x1_a1t2q x2_a1t2r x3_a1t2s x4_a1t2t)
tDefType _ (TTable x1_a1t2u x2_a1t2v x3_a1t2w x4_a1t2x x5_a1t2y)
  = pure (TTable x1_a1t2u x2_a1t2v x3_a1t2w x4_a1t2x x5_a1t2y)
{-# INLINE tDefType #-}
tDocs :: forall n_a1pCA. Traversal' (Term n_a1pCA) (Maybe Text)
tDocs _ (TModule x1_a1t2z x2_a1t2A x3_a1t2B)
  = pure (TModule x1_a1t2z x2_a1t2A x3_a1t2B)
tDocs _ (TList x1_a1t2C x2_a1t2D x3_a1t2E)
  = pure (TList x1_a1t2C x2_a1t2D x3_a1t2E)
tDocs
  f_a1t2F
  (TDef x1_a1t2G
        x2_a1t2H
        x3_a1t2I
        x4_a1t2J
        x5_a1t2K
        x6_a1t2L
        x7_a1t2M)
  = fmap
      (\ y1_a1t2N
         -> TDef
              x1_a1t2G x2_a1t2H x3_a1t2I x4_a1t2J x5_a1t2K y1_a1t2N x7_a1t2M)
      (f_a1t2F x6_a1t2L)
tDocs _ (TNative x1_a1t2O x2_a1t2P x3_a1t2Q x4_a1t2R x5_a1t2S)
  = pure (TNative x1_a1t2O x2_a1t2P x3_a1t2Q x4_a1t2R x5_a1t2S)
tDocs f_a1t2T (TConst x1_a1t2U x2_a1t2V x3_a1t2W x4_a1t2X x5_a1t2Y)
  = fmap
      (\ y1_a1t2Z -> TConst x1_a1t2U x2_a1t2V x3_a1t2W y1_a1t2Z x5_a1t2Y)
      (f_a1t2T x4_a1t2X)
tDocs _ (TApp x1_a1t30 x2_a1t31 x3_a1t32)
  = pure (TApp x1_a1t30 x2_a1t31 x3_a1t32)
tDocs _ (TVar x1_a1t33 x2_a1t34) = pure (TVar x1_a1t33 x2_a1t34)
tDocs _ (TBinding x1_a1t35 x2_a1t36 x3_a1t37 x4_a1t38)
  = pure (TBinding x1_a1t35 x2_a1t36 x3_a1t37 x4_a1t38)
tDocs _ (TObject x1_a1t39 x2_a1t3a x3_a1t3b)
  = pure (TObject x1_a1t39 x2_a1t3a x3_a1t3b)
tDocs
  f_a1t3c
  (TSchema x1_a1t3d x2_a1t3e x3_a1t3f x4_a1t3g x5_a1t3h)
  = fmap
      (\ y1_a1t3i
         -> TSchema x1_a1t3d x2_a1t3e y1_a1t3i x4_a1t3g x5_a1t3h)
      (f_a1t3c x3_a1t3f)
tDocs _ (TLiteral x1_a1t3j x2_a1t3k)
  = pure (TLiteral x1_a1t3j x2_a1t3k)
tDocs _ (TKeySet x1_a1t3l x2_a1t3m)
  = pure (TKeySet x1_a1t3l x2_a1t3m)
tDocs _ (TUse x1_a1t3n x2_a1t3o x3_a1t3p)
  = pure (TUse x1_a1t3n x2_a1t3o x3_a1t3p)
tDocs _ (TValue x1_a1t3q x2_a1t3r)
  = pure (TValue x1_a1t3q x2_a1t3r)
tDocs _ (TStep x1_a1t3s x2_a1t3t x3_a1t3u x4_a1t3v)
  = pure (TStep x1_a1t3s x2_a1t3t x3_a1t3u x4_a1t3v)
tDocs f_a1t3w (TTable x1_a1t3x x2_a1t3y x3_a1t3z x4_a1t3A x5_a1t3B)
  = fmap
      (\ y1_a1t3C -> TTable x1_a1t3x x2_a1t3y x3_a1t3z y1_a1t3C x5_a1t3B)
      (f_a1t3w x4_a1t3A)
{-# INLINE tDocs #-}
tFields ::
  forall n_a1pCA. Traversal' (Term n_a1pCA) [Arg (Term n_a1pCA)]
tFields _ (TModule x1_a1t3D x2_a1t3E x3_a1t3F)
  = pure (TModule x1_a1t3D x2_a1t3E x3_a1t3F)
tFields _ (TList x1_a1t3G x2_a1t3H x3_a1t3I)
  = pure (TList x1_a1t3G x2_a1t3H x3_a1t3I)
tFields
  _
  (TDef x1_a1t3J
        x2_a1t3K
        x3_a1t3L
        x4_a1t3M
        x5_a1t3N
        x6_a1t3O
        x7_a1t3P)
  = pure
      (TDef
         x1_a1t3J x2_a1t3K x3_a1t3L x4_a1t3M x5_a1t3N x6_a1t3O x7_a1t3P)
tFields _ (TNative x1_a1t3Q x2_a1t3R x3_a1t3S x4_a1t3T x5_a1t3U)
  = pure (TNative x1_a1t3Q x2_a1t3R x3_a1t3S x4_a1t3T x5_a1t3U)
tFields _ (TConst x1_a1t3V x2_a1t3W x3_a1t3X x4_a1t3Y x5_a1t3Z)
  = pure (TConst x1_a1t3V x2_a1t3W x3_a1t3X x4_a1t3Y x5_a1t3Z)
tFields _ (TApp x1_a1t40 x2_a1t41 x3_a1t42)
  = pure (TApp x1_a1t40 x2_a1t41 x3_a1t42)
tFields _ (TVar x1_a1t43 x2_a1t44) = pure (TVar x1_a1t43 x2_a1t44)
tFields _ (TBinding x1_a1t45 x2_a1t46 x3_a1t47 x4_a1t48)
  = pure (TBinding x1_a1t45 x2_a1t46 x3_a1t47 x4_a1t48)
tFields _ (TObject x1_a1t49 x2_a1t4a x3_a1t4b)
  = pure (TObject x1_a1t49 x2_a1t4a x3_a1t4b)
tFields
  f_a1t4c
  (TSchema x1_a1t4d x2_a1t4e x3_a1t4f x4_a1t4g x5_a1t4h)
  = fmap
      (\ y1_a1t4i
         -> TSchema x1_a1t4d x2_a1t4e x3_a1t4f y1_a1t4i x5_a1t4h)
      (f_a1t4c x4_a1t4g)
tFields _ (TLiteral x1_a1t4j x2_a1t4k)
  = pure (TLiteral x1_a1t4j x2_a1t4k)
tFields _ (TKeySet x1_a1t4l x2_a1t4m)
  = pure (TKeySet x1_a1t4l x2_a1t4m)
tFields _ (TUse x1_a1t4n x2_a1t4o x3_a1t4p)
  = pure (TUse x1_a1t4n x2_a1t4o x3_a1t4p)
tFields _ (TValue x1_a1t4q x2_a1t4r)
  = pure (TValue x1_a1t4q x2_a1t4r)
tFields _ (TStep x1_a1t4s x2_a1t4t x3_a1t4u x4_a1t4v)
  = pure (TStep x1_a1t4s x2_a1t4t x3_a1t4u x4_a1t4v)
tFields _ (TTable x1_a1t4w x2_a1t4x x3_a1t4y x4_a1t4z x5_a1t4A)
  = pure (TTable x1_a1t4w x2_a1t4x x3_a1t4y x4_a1t4z x5_a1t4A)
{-# INLINE tFields #-}
tFunType ::
  forall n_a1pCA. Traversal' (Term n_a1pCA) (FunType (Term n_a1pCA))
tFunType _ (TModule x1_a1t4B x2_a1t4C x3_a1t4D)
  = pure (TModule x1_a1t4B x2_a1t4C x3_a1t4D)
tFunType _ (TList x1_a1t4E x2_a1t4F x3_a1t4G)
  = pure (TList x1_a1t4E x2_a1t4F x3_a1t4G)
tFunType
  f_a1t4H
  (TDef x1_a1t4I
        x2_a1t4J
        x3_a1t4K
        x4_a1t4L
        x5_a1t4M
        x6_a1t4N
        x7_a1t4O)
  = fmap
      (\ y1_a1t4P
         -> TDef
              x1_a1t4I x2_a1t4J x3_a1t4K y1_a1t4P x5_a1t4M x6_a1t4N x7_a1t4O)
      (f_a1t4H x4_a1t4L)
tFunType _ (TNative x1_a1t4Q x2_a1t4R x3_a1t4S x4_a1t4T x5_a1t4U)
  = pure (TNative x1_a1t4Q x2_a1t4R x3_a1t4S x4_a1t4T x5_a1t4U)
tFunType _ (TConst x1_a1t4V x2_a1t4W x3_a1t4X x4_a1t4Y x5_a1t4Z)
  = pure (TConst x1_a1t4V x2_a1t4W x3_a1t4X x4_a1t4Y x5_a1t4Z)
tFunType _ (TApp x1_a1t50 x2_a1t51 x3_a1t52)
  = pure (TApp x1_a1t50 x2_a1t51 x3_a1t52)
tFunType _ (TVar x1_a1t53 x2_a1t54) = pure (TVar x1_a1t53 x2_a1t54)
tFunType _ (TBinding x1_a1t55 x2_a1t56 x3_a1t57 x4_a1t58)
  = pure (TBinding x1_a1t55 x2_a1t56 x3_a1t57 x4_a1t58)
tFunType _ (TObject x1_a1t59 x2_a1t5a x3_a1t5b)
  = pure (TObject x1_a1t59 x2_a1t5a x3_a1t5b)
tFunType _ (TSchema x1_a1t5c x2_a1t5d x3_a1t5e x4_a1t5f x5_a1t5g)
  = pure (TSchema x1_a1t5c x2_a1t5d x3_a1t5e x4_a1t5f x5_a1t5g)
tFunType _ (TLiteral x1_a1t5h x2_a1t5i)
  = pure (TLiteral x1_a1t5h x2_a1t5i)
tFunType _ (TKeySet x1_a1t5j x2_a1t5k)
  = pure (TKeySet x1_a1t5j x2_a1t5k)
tFunType _ (TUse x1_a1t5l x2_a1t5m x3_a1t5n)
  = pure (TUse x1_a1t5l x2_a1t5m x3_a1t5n)
tFunType _ (TValue x1_a1t5o x2_a1t5p)
  = pure (TValue x1_a1t5o x2_a1t5p)
tFunType _ (TStep x1_a1t5q x2_a1t5r x3_a1t5s x4_a1t5t)
  = pure (TStep x1_a1t5q x2_a1t5r x3_a1t5s x4_a1t5t)
tFunType _ (TTable x1_a1t5u x2_a1t5v x3_a1t5w x4_a1t5x x5_a1t5y)
  = pure (TTable x1_a1t5u x2_a1t5v x3_a1t5w x4_a1t5x x5_a1t5y)
{-# INLINE tFunType #-}
tFunTypes ::
  forall n_a1pCA. Traversal' (Term n_a1pCA) (FunTypes (Term n_a1pCA))
tFunTypes _ (TModule x1_a1t5z x2_a1t5A x3_a1t5B)
  = pure (TModule x1_a1t5z x2_a1t5A x3_a1t5B)
tFunTypes _ (TList x1_a1t5C x2_a1t5D x3_a1t5E)
  = pure (TList x1_a1t5C x2_a1t5D x3_a1t5E)
tFunTypes
  _
  (TDef x1_a1t5F
        x2_a1t5G
        x3_a1t5H
        x4_a1t5I
        x5_a1t5J
        x6_a1t5K
        x7_a1t5L)
  = pure
      (TDef
         x1_a1t5F x2_a1t5G x3_a1t5H x4_a1t5I x5_a1t5J x6_a1t5K x7_a1t5L)
tFunTypes
  f_a1t5M
  (TNative x1_a1t5N x2_a1t5O x3_a1t5P x4_a1t5Q x5_a1t5R)
  = fmap
      (\ y1_a1t5S
         -> TNative x1_a1t5N x2_a1t5O y1_a1t5S x4_a1t5Q x5_a1t5R)
      (f_a1t5M x3_a1t5P)
tFunTypes _ (TConst x1_a1t5T x2_a1t5U x3_a1t5V x4_a1t5W x5_a1t5X)
  = pure (TConst x1_a1t5T x2_a1t5U x3_a1t5V x4_a1t5W x5_a1t5X)
tFunTypes _ (TApp x1_a1t5Y x2_a1t5Z x3_a1t60)
  = pure (TApp x1_a1t5Y x2_a1t5Z x3_a1t60)
tFunTypes _ (TVar x1_a1t61 x2_a1t62)
  = pure (TVar x1_a1t61 x2_a1t62)
tFunTypes _ (TBinding x1_a1t63 x2_a1t64 x3_a1t65 x4_a1t66)
  = pure (TBinding x1_a1t63 x2_a1t64 x3_a1t65 x4_a1t66)
tFunTypes _ (TObject x1_a1t67 x2_a1t68 x3_a1t69)
  = pure (TObject x1_a1t67 x2_a1t68 x3_a1t69)
tFunTypes _ (TSchema x1_a1t6a x2_a1t6b x3_a1t6c x4_a1t6d x5_a1t6e)
  = pure (TSchema x1_a1t6a x2_a1t6b x3_a1t6c x4_a1t6d x5_a1t6e)
tFunTypes _ (TLiteral x1_a1t6f x2_a1t6g)
  = pure (TLiteral x1_a1t6f x2_a1t6g)
tFunTypes _ (TKeySet x1_a1t6h x2_a1t6i)
  = pure (TKeySet x1_a1t6h x2_a1t6i)
tFunTypes _ (TUse x1_a1t6j x2_a1t6k x3_a1t6l)
  = pure (TUse x1_a1t6j x2_a1t6k x3_a1t6l)
tFunTypes _ (TValue x1_a1t6m x2_a1t6n)
  = pure (TValue x1_a1t6m x2_a1t6n)
tFunTypes _ (TStep x1_a1t6o x2_a1t6p x3_a1t6q x4_a1t6r)
  = pure (TStep x1_a1t6o x2_a1t6p x3_a1t6q x4_a1t6r)
tFunTypes _ (TTable x1_a1t6s x2_a1t6t x3_a1t6u x4_a1t6v x5_a1t6w)
  = pure (TTable x1_a1t6s x2_a1t6t x3_a1t6u x4_a1t6v x5_a1t6w)
{-# INLINE tFunTypes #-}
tInfo :: forall n_a1pCA. Lens' (Term n_a1pCA) Info
tInfo f_a1t6x (TModule x1_a1t6y x2_a1t6z x3_a1t6A)
  = fmap
      (\ y1_a1t6B -> TModule x1_a1t6y x2_a1t6z y1_a1t6B)
      (f_a1t6x x3_a1t6A)
tInfo f_a1t6C (TList x1_a1t6D x2_a1t6E x3_a1t6F)
  = fmap
      (\ y1_a1t6G -> TList x1_a1t6D x2_a1t6E y1_a1t6G) (f_a1t6C x3_a1t6F)
tInfo
  f_a1t6H
  (TDef x1_a1t6I
        x2_a1t6J
        x3_a1t6K
        x4_a1t6L
        x5_a1t6M
        x6_a1t6N
        x7_a1t6O)
  = fmap
      (\ y1_a1t6P
         -> TDef
              x1_a1t6I x2_a1t6J x3_a1t6K x4_a1t6L x5_a1t6M x6_a1t6N y1_a1t6P)
      (f_a1t6H x7_a1t6O)
tInfo
  f_a1t6Q
  (TNative x1_a1t6R x2_a1t6S x3_a1t6T x4_a1t6U x5_a1t6V)
  = fmap
      (\ y1_a1t6W
         -> TNative x1_a1t6R x2_a1t6S x3_a1t6T x4_a1t6U y1_a1t6W)
      (f_a1t6Q x5_a1t6V)
tInfo f_a1t6X (TConst x1_a1t6Y x2_a1t6Z x3_a1t70 x4_a1t71 x5_a1t72)
  = fmap
      (\ y1_a1t73 -> TConst x1_a1t6Y x2_a1t6Z x3_a1t70 x4_a1t71 y1_a1t73)
      (f_a1t6X x5_a1t72)
tInfo f_a1t74 (TApp x1_a1t75 x2_a1t76 x3_a1t77)
  = fmap
      (\ y1_a1t78 -> TApp x1_a1t75 x2_a1t76 y1_a1t78) (f_a1t74 x3_a1t77)
tInfo f_a1t79 (TVar x1_a1t7a x2_a1t7b)
  = fmap (\ y1_a1t7c -> TVar x1_a1t7a y1_a1t7c) (f_a1t79 x2_a1t7b)
tInfo f_a1t7d (TBinding x1_a1t7e x2_a1t7f x3_a1t7g x4_a1t7h)
  = fmap
      (\ y1_a1t7i -> TBinding x1_a1t7e x2_a1t7f x3_a1t7g y1_a1t7i)
      (f_a1t7d x4_a1t7h)
tInfo f_a1t7j (TObject x1_a1t7k x2_a1t7l x3_a1t7m)
  = fmap
      (\ y1_a1t7n -> TObject x1_a1t7k x2_a1t7l y1_a1t7n)
      (f_a1t7j x3_a1t7m)
tInfo
  f_a1t7o
  (TSchema x1_a1t7p x2_a1t7q x3_a1t7r x4_a1t7s x5_a1t7t)
  = fmap
      (\ y1_a1t7u
         -> TSchema x1_a1t7p x2_a1t7q x3_a1t7r x4_a1t7s y1_a1t7u)
      (f_a1t7o x5_a1t7t)
tInfo f_a1t7v (TLiteral x1_a1t7w x2_a1t7x)
  = fmap
      (\ y1_a1t7y -> TLiteral x1_a1t7w y1_a1t7y) (f_a1t7v x2_a1t7x)
tInfo f_a1t7z (TKeySet x1_a1t7A x2_a1t7B)
  = fmap (\ y1_a1t7C -> TKeySet x1_a1t7A y1_a1t7C) (f_a1t7z x2_a1t7B)
tInfo f_a1t7D (TUse x1_a1t7E x2_a1t7F x3_a1t7G)
  = fmap
      (\ y1_a1t7H -> TUse x1_a1t7E x2_a1t7F y1_a1t7H) (f_a1t7D x3_a1t7G)
tInfo f_a1t7I (TValue x1_a1t7J x2_a1t7K)
  = fmap (\ y1_a1t7L -> TValue x1_a1t7J y1_a1t7L) (f_a1t7I x2_a1t7K)
tInfo f_a1t7M (TStep x1_a1t7N x2_a1t7O x3_a1t7P x4_a1t7Q)
  = fmap
      (\ y1_a1t7R -> TStep x1_a1t7N x2_a1t7O x3_a1t7P y1_a1t7R)
      (f_a1t7M x4_a1t7Q)
tInfo f_a1t7S (TTable x1_a1t7T x2_a1t7U x3_a1t7V x4_a1t7W x5_a1t7X)
  = fmap
      (\ y1_a1t7Y -> TTable x1_a1t7T x2_a1t7U x3_a1t7V x4_a1t7W y1_a1t7Y)
      (f_a1t7S x5_a1t7X)
{-# INLINE tInfo #-}
tKeySet :: forall n_a1pCA. Traversal' (Term n_a1pCA) KeySet
tKeySet _ (TModule x1_a1t7Z x2_a1t80 x3_a1t81)
  = pure (TModule x1_a1t7Z x2_a1t80 x3_a1t81)
tKeySet _ (TList x1_a1t82 x2_a1t83 x3_a1t84)
  = pure (TList x1_a1t82 x2_a1t83 x3_a1t84)
tKeySet
  _
  (TDef x1_a1t85
        x2_a1t86
        x3_a1t87
        x4_a1t88
        x5_a1t89
        x6_a1t8a
        x7_a1t8b)
  = pure
      (TDef
         x1_a1t85 x2_a1t86 x3_a1t87 x4_a1t88 x5_a1t89 x6_a1t8a x7_a1t8b)
tKeySet _ (TNative x1_a1t8c x2_a1t8d x3_a1t8e x4_a1t8f x5_a1t8g)
  = pure (TNative x1_a1t8c x2_a1t8d x3_a1t8e x4_a1t8f x5_a1t8g)
tKeySet _ (TConst x1_a1t8h x2_a1t8i x3_a1t8j x4_a1t8k x5_a1t8l)
  = pure (TConst x1_a1t8h x2_a1t8i x3_a1t8j x4_a1t8k x5_a1t8l)
tKeySet _ (TApp x1_a1t8m x2_a1t8n x3_a1t8o)
  = pure (TApp x1_a1t8m x2_a1t8n x3_a1t8o)
tKeySet _ (TVar x1_a1t8p x2_a1t8q) = pure (TVar x1_a1t8p x2_a1t8q)
tKeySet _ (TBinding x1_a1t8r x2_a1t8s x3_a1t8t x4_a1t8u)
  = pure (TBinding x1_a1t8r x2_a1t8s x3_a1t8t x4_a1t8u)
tKeySet _ (TObject x1_a1t8v x2_a1t8w x3_a1t8x)
  = pure (TObject x1_a1t8v x2_a1t8w x3_a1t8x)
tKeySet _ (TSchema x1_a1t8y x2_a1t8z x3_a1t8A x4_a1t8B x5_a1t8C)
  = pure (TSchema x1_a1t8y x2_a1t8z x3_a1t8A x4_a1t8B x5_a1t8C)
tKeySet _ (TLiteral x1_a1t8D x2_a1t8E)
  = pure (TLiteral x1_a1t8D x2_a1t8E)
tKeySet f_a1t8F (TKeySet x1_a1t8G x2_a1t8H)
  = fmap (\ y1_a1t8I -> TKeySet y1_a1t8I x2_a1t8H) (f_a1t8F x1_a1t8G)
tKeySet _ (TUse x1_a1t8J x2_a1t8K x3_a1t8L)
  = pure (TUse x1_a1t8J x2_a1t8K x3_a1t8L)
tKeySet _ (TValue x1_a1t8M x2_a1t8N)
  = pure (TValue x1_a1t8M x2_a1t8N)
tKeySet _ (TStep x1_a1t8O x2_a1t8P x3_a1t8Q x4_a1t8R)
  = pure (TStep x1_a1t8O x2_a1t8P x3_a1t8Q x4_a1t8R)
tKeySet _ (TTable x1_a1t8S x2_a1t8T x3_a1t8U x4_a1t8V x5_a1t8W)
  = pure (TTable x1_a1t8S x2_a1t8T x3_a1t8U x4_a1t8V x5_a1t8W)
{-# INLINE tKeySet #-}
tList :: forall n_a1pCA. Traversal' (Term n_a1pCA) [Term n_a1pCA]
tList _ (TModule x1_a1t8X x2_a1t8Y x3_a1t8Z)
  = pure (TModule x1_a1t8X x2_a1t8Y x3_a1t8Z)
tList f_a1t90 (TList x1_a1t91 x2_a1t92 x3_a1t93)
  = fmap
      (\ y1_a1t94 -> TList y1_a1t94 x2_a1t92 x3_a1t93) (f_a1t90 x1_a1t91)
tList
  _
  (TDef x1_a1t95
        x2_a1t96
        x3_a1t97
        x4_a1t98
        x5_a1t99
        x6_a1t9a
        x7_a1t9b)
  = pure
      (TDef
         x1_a1t95 x2_a1t96 x3_a1t97 x4_a1t98 x5_a1t99 x6_a1t9a x7_a1t9b)
tList _ (TNative x1_a1t9c x2_a1t9d x3_a1t9e x4_a1t9f x5_a1t9g)
  = pure (TNative x1_a1t9c x2_a1t9d x3_a1t9e x4_a1t9f x5_a1t9g)
tList _ (TConst x1_a1t9h x2_a1t9i x3_a1t9j x4_a1t9k x5_a1t9l)
  = pure (TConst x1_a1t9h x2_a1t9i x3_a1t9j x4_a1t9k x5_a1t9l)
tList _ (TApp x1_a1t9m x2_a1t9n x3_a1t9o)
  = pure (TApp x1_a1t9m x2_a1t9n x3_a1t9o)
tList _ (TVar x1_a1t9p x2_a1t9q) = pure (TVar x1_a1t9p x2_a1t9q)
tList _ (TBinding x1_a1t9r x2_a1t9s x3_a1t9t x4_a1t9u)
  = pure (TBinding x1_a1t9r x2_a1t9s x3_a1t9t x4_a1t9u)
tList _ (TObject x1_a1t9v x2_a1t9w x3_a1t9x)
  = pure (TObject x1_a1t9v x2_a1t9w x3_a1t9x)
tList _ (TSchema x1_a1t9y x2_a1t9z x3_a1t9A x4_a1t9B x5_a1t9C)
  = pure (TSchema x1_a1t9y x2_a1t9z x3_a1t9A x4_a1t9B x5_a1t9C)
tList _ (TLiteral x1_a1t9D x2_a1t9E)
  = pure (TLiteral x1_a1t9D x2_a1t9E)
tList _ (TKeySet x1_a1t9F x2_a1t9G)
  = pure (TKeySet x1_a1t9F x2_a1t9G)
tList _ (TUse x1_a1t9H x2_a1t9I x3_a1t9J)
  = pure (TUse x1_a1t9H x2_a1t9I x3_a1t9J)
tList _ (TValue x1_a1t9K x2_a1t9L)
  = pure (TValue x1_a1t9K x2_a1t9L)
tList _ (TStep x1_a1t9M x2_a1t9N x3_a1t9O x4_a1t9P)
  = pure (TStep x1_a1t9M x2_a1t9N x3_a1t9O x4_a1t9P)
tList _ (TTable x1_a1t9Q x2_a1t9R x3_a1t9S x4_a1t9T x5_a1t9U)
  = pure (TTable x1_a1t9Q x2_a1t9R x3_a1t9S x4_a1t9T x5_a1t9U)
{-# INLINE tList #-}
tListType ::
  forall n_a1pCA. Traversal' (Term n_a1pCA) (Type (Term n_a1pCA))
tListType _ (TModule x1_a1t9V x2_a1t9W x3_a1t9X)
  = pure (TModule x1_a1t9V x2_a1t9W x3_a1t9X)
tListType f_a1t9Y (TList x1_a1t9Z x2_a1ta0 x3_a1ta1)
  = fmap
      (\ y1_a1ta2 -> TList x1_a1t9Z y1_a1ta2 x3_a1ta1) (f_a1t9Y x2_a1ta0)
tListType
  _
  (TDef x1_a1ta3
        x2_a1ta4
        x3_a1ta5
        x4_a1ta6
        x5_a1ta7
        x6_a1ta8
        x7_a1ta9)
  = pure
      (TDef
         x1_a1ta3 x2_a1ta4 x3_a1ta5 x4_a1ta6 x5_a1ta7 x6_a1ta8 x7_a1ta9)
tListType _ (TNative x1_a1taa x2_a1tab x3_a1tac x4_a1tad x5_a1tae)
  = pure (TNative x1_a1taa x2_a1tab x3_a1tac x4_a1tad x5_a1tae)
tListType _ (TConst x1_a1taf x2_a1tag x3_a1tah x4_a1tai x5_a1taj)
  = pure (TConst x1_a1taf x2_a1tag x3_a1tah x4_a1tai x5_a1taj)
tListType _ (TApp x1_a1tak x2_a1tal x3_a1tam)
  = pure (TApp x1_a1tak x2_a1tal x3_a1tam)
tListType _ (TVar x1_a1tan x2_a1tao)
  = pure (TVar x1_a1tan x2_a1tao)
tListType _ (TBinding x1_a1tap x2_a1taq x3_a1tar x4_a1tas)
  = pure (TBinding x1_a1tap x2_a1taq x3_a1tar x4_a1tas)
tListType _ (TObject x1_a1tat x2_a1tau x3_a1tav)
  = pure (TObject x1_a1tat x2_a1tau x3_a1tav)
tListType _ (TSchema x1_a1taw x2_a1tax x3_a1tay x4_a1taz x5_a1taA)
  = pure (TSchema x1_a1taw x2_a1tax x3_a1tay x4_a1taz x5_a1taA)
tListType _ (TLiteral x1_a1taB x2_a1taC)
  = pure (TLiteral x1_a1taB x2_a1taC)
tListType _ (TKeySet x1_a1taD x2_a1taE)
  = pure (TKeySet x1_a1taD x2_a1taE)
tListType _ (TUse x1_a1taF x2_a1taG x3_a1taH)
  = pure (TUse x1_a1taF x2_a1taG x3_a1taH)
tListType _ (TValue x1_a1taI x2_a1taJ)
  = pure (TValue x1_a1taI x2_a1taJ)
tListType _ (TStep x1_a1taK x2_a1taL x3_a1taM x4_a1taN)
  = pure (TStep x1_a1taK x2_a1taL x3_a1taM x4_a1taN)
tListType _ (TTable x1_a1taO x2_a1taP x3_a1taQ x4_a1taR x5_a1taS)
  = pure (TTable x1_a1taO x2_a1taP x3_a1taQ x4_a1taR x5_a1taS)
{-# INLINE tListType #-}
tLiteral :: forall n_a1pCA. Traversal' (Term n_a1pCA) Literal
tLiteral _ (TModule x1_a1taT x2_a1taU x3_a1taV)
  = pure (TModule x1_a1taT x2_a1taU x3_a1taV)
tLiteral _ (TList x1_a1taW x2_a1taX x3_a1taY)
  = pure (TList x1_a1taW x2_a1taX x3_a1taY)
tLiteral
  _
  (TDef x1_a1taZ
        x2_a1tb0
        x3_a1tb1
        x4_a1tb2
        x5_a1tb3
        x6_a1tb4
        x7_a1tb5)
  = pure
      (TDef
         x1_a1taZ x2_a1tb0 x3_a1tb1 x4_a1tb2 x5_a1tb3 x6_a1tb4 x7_a1tb5)
tLiteral _ (TNative x1_a1tb6 x2_a1tb7 x3_a1tb8 x4_a1tb9 x5_a1tba)
  = pure (TNative x1_a1tb6 x2_a1tb7 x3_a1tb8 x4_a1tb9 x5_a1tba)
tLiteral _ (TConst x1_a1tbb x2_a1tbc x3_a1tbd x4_a1tbe x5_a1tbf)
  = pure (TConst x1_a1tbb x2_a1tbc x3_a1tbd x4_a1tbe x5_a1tbf)
tLiteral _ (TApp x1_a1tbg x2_a1tbh x3_a1tbi)
  = pure (TApp x1_a1tbg x2_a1tbh x3_a1tbi)
tLiteral _ (TVar x1_a1tbj x2_a1tbk) = pure (TVar x1_a1tbj x2_a1tbk)
tLiteral _ (TBinding x1_a1tbl x2_a1tbm x3_a1tbn x4_a1tbo)
  = pure (TBinding x1_a1tbl x2_a1tbm x3_a1tbn x4_a1tbo)
tLiteral _ (TObject x1_a1tbp x2_a1tbq x3_a1tbr)
  = pure (TObject x1_a1tbp x2_a1tbq x3_a1tbr)
tLiteral _ (TSchema x1_a1tbs x2_a1tbt x3_a1tbu x4_a1tbv x5_a1tbw)
  = pure (TSchema x1_a1tbs x2_a1tbt x3_a1tbu x4_a1tbv x5_a1tbw)
tLiteral f_a1tbx (TLiteral x1_a1tby x2_a1tbz)
  = fmap
      (\ y1_a1tbA -> TLiteral y1_a1tbA x2_a1tbz) (f_a1tbx x1_a1tby)
tLiteral _ (TKeySet x1_a1tbB x2_a1tbC)
  = pure (TKeySet x1_a1tbB x2_a1tbC)
tLiteral _ (TUse x1_a1tbD x2_a1tbE x3_a1tbF)
  = pure (TUse x1_a1tbD x2_a1tbE x3_a1tbF)
tLiteral _ (TValue x1_a1tbG x2_a1tbH)
  = pure (TValue x1_a1tbG x2_a1tbH)
tLiteral _ (TStep x1_a1tbI x2_a1tbJ x3_a1tbK x4_a1tbL)
  = pure (TStep x1_a1tbI x2_a1tbJ x3_a1tbK x4_a1tbL)
tLiteral _ (TTable x1_a1tbM x2_a1tbN x3_a1tbO x4_a1tbP x5_a1tbQ)
  = pure (TTable x1_a1tbM x2_a1tbN x3_a1tbO x4_a1tbP x5_a1tbQ)
{-# INLINE tLiteral #-}
tModule :: forall n_a1pCA. Traversal' (Term n_a1pCA) ModuleName
tModule _ (TModule x1_a1tbR x2_a1tbS x3_a1tbT)
  = pure (TModule x1_a1tbR x2_a1tbS x3_a1tbT)
tModule _ (TList x1_a1tbU x2_a1tbV x3_a1tbW)
  = pure (TList x1_a1tbU x2_a1tbV x3_a1tbW)
tModule
  f_a1tbX
  (TDef x1_a1tbY
        x2_a1tbZ
        x3_a1tc0
        x4_a1tc1
        x5_a1tc2
        x6_a1tc3
        x7_a1tc4)
  = fmap
      (\ y1_a1tc5
         -> TDef
              x1_a1tbY y1_a1tc5 x3_a1tc0 x4_a1tc1 x5_a1tc2 x6_a1tc3 x7_a1tc4)
      (f_a1tbX x2_a1tbZ)
tModule _ (TNative x1_a1tc6 x2_a1tc7 x3_a1tc8 x4_a1tc9 x5_a1tca)
  = pure (TNative x1_a1tc6 x2_a1tc7 x3_a1tc8 x4_a1tc9 x5_a1tca)
tModule
  f_a1tcb
  (TConst x1_a1tcc x2_a1tcd x3_a1tce x4_a1tcf x5_a1tcg)
  = fmap
      (\ y1_a1tch -> TConst x1_a1tcc y1_a1tch x3_a1tce x4_a1tcf x5_a1tcg)
      (f_a1tcb x2_a1tcd)
tModule _ (TApp x1_a1tci x2_a1tcj x3_a1tck)
  = pure (TApp x1_a1tci x2_a1tcj x3_a1tck)
tModule _ (TVar x1_a1tcl x2_a1tcm) = pure (TVar x1_a1tcl x2_a1tcm)
tModule _ (TBinding x1_a1tcn x2_a1tco x3_a1tcp x4_a1tcq)
  = pure (TBinding x1_a1tcn x2_a1tco x3_a1tcp x4_a1tcq)
tModule _ (TObject x1_a1tcr x2_a1tcs x3_a1tct)
  = pure (TObject x1_a1tcr x2_a1tcs x3_a1tct)
tModule
  f_a1tcu
  (TSchema x1_a1tcv x2_a1tcw x3_a1tcx x4_a1tcy x5_a1tcz)
  = fmap
      (\ y1_a1tcA
         -> TSchema x1_a1tcv y1_a1tcA x3_a1tcx x4_a1tcy x5_a1tcz)
      (f_a1tcu x2_a1tcw)
tModule _ (TLiteral x1_a1tcB x2_a1tcC)
  = pure (TLiteral x1_a1tcB x2_a1tcC)
tModule _ (TKeySet x1_a1tcD x2_a1tcE)
  = pure (TKeySet x1_a1tcD x2_a1tcE)
tModule _ (TUse x1_a1tcF x2_a1tcG x3_a1tcH)
  = pure (TUse x1_a1tcF x2_a1tcG x3_a1tcH)
tModule _ (TValue x1_a1tcI x2_a1tcJ)
  = pure (TValue x1_a1tcI x2_a1tcJ)
tModule _ (TStep x1_a1tcK x2_a1tcL x3_a1tcM x4_a1tcN)
  = pure (TStep x1_a1tcK x2_a1tcL x3_a1tcM x4_a1tcN)
tModule
  f_a1tcO
  (TTable x1_a1tcP x2_a1tcQ x3_a1tcR x4_a1tcS x5_a1tcT)
  = fmap
      (\ y1_a1tcU -> TTable x1_a1tcP y1_a1tcU x3_a1tcR x4_a1tcS x5_a1tcT)
      (f_a1tcO x2_a1tcQ)
{-# INLINE tModule #-}
tModuleBody ::
  forall n_a1pCA. Traversal' (Term n_a1pCA) (Scope () Term n_a1pCA)
tModuleBody f_a1tcV (TModule x1_a1tcW x2_a1tcX x3_a1tcY)
  = fmap
      (\ y1_a1tcZ -> TModule x1_a1tcW y1_a1tcZ x3_a1tcY)
      (f_a1tcV x2_a1tcX)
tModuleBody _ (TList x1_a1td0 x2_a1td1 x3_a1td2)
  = pure (TList x1_a1td0 x2_a1td1 x3_a1td2)
tModuleBody
  _
  (TDef x1_a1td3
        x2_a1td4
        x3_a1td5
        x4_a1td6
        x5_a1td7
        x6_a1td8
        x7_a1td9)
  = pure
      (TDef
         x1_a1td3 x2_a1td4 x3_a1td5 x4_a1td6 x5_a1td7 x6_a1td8 x7_a1td9)
tModuleBody
  _
  (TNative x1_a1tda x2_a1tdb x3_a1tdc x4_a1tdd x5_a1tde)
  = pure (TNative x1_a1tda x2_a1tdb x3_a1tdc x4_a1tdd x5_a1tde)
tModuleBody _ (TConst x1_a1tdf x2_a1tdg x3_a1tdh x4_a1tdi x5_a1tdj)
  = pure (TConst x1_a1tdf x2_a1tdg x3_a1tdh x4_a1tdi x5_a1tdj)
tModuleBody _ (TApp x1_a1tdk x2_a1tdl x3_a1tdm)
  = pure (TApp x1_a1tdk x2_a1tdl x3_a1tdm)
tModuleBody _ (TVar x1_a1tdn x2_a1tdo)
  = pure (TVar x1_a1tdn x2_a1tdo)
tModuleBody _ (TBinding x1_a1tdp x2_a1tdq x3_a1tdr x4_a1tds)
  = pure (TBinding x1_a1tdp x2_a1tdq x3_a1tdr x4_a1tds)
tModuleBody _ (TObject x1_a1tdt x2_a1tdu x3_a1tdv)
  = pure (TObject x1_a1tdt x2_a1tdu x3_a1tdv)
tModuleBody
  _
  (TSchema x1_a1tdw x2_a1tdx x3_a1tdy x4_a1tdz x5_a1tdA)
  = pure (TSchema x1_a1tdw x2_a1tdx x3_a1tdy x4_a1tdz x5_a1tdA)
tModuleBody _ (TLiteral x1_a1tdB x2_a1tdC)
  = pure (TLiteral x1_a1tdB x2_a1tdC)
tModuleBody _ (TKeySet x1_a1tdD x2_a1tdE)
  = pure (TKeySet x1_a1tdD x2_a1tdE)
tModuleBody _ (TUse x1_a1tdF x2_a1tdG x3_a1tdH)
  = pure (TUse x1_a1tdF x2_a1tdG x3_a1tdH)
tModuleBody _ (TValue x1_a1tdI x2_a1tdJ)
  = pure (TValue x1_a1tdI x2_a1tdJ)
tModuleBody _ (TStep x1_a1tdK x2_a1tdL x3_a1tdM x4_a1tdN)
  = pure (TStep x1_a1tdK x2_a1tdL x3_a1tdM x4_a1tdN)
tModuleBody _ (TTable x1_a1tdO x2_a1tdP x3_a1tdQ x4_a1tdR x5_a1tdS)
  = pure (TTable x1_a1tdO x2_a1tdP x3_a1tdQ x4_a1tdR x5_a1tdS)
{-# INLINE tModuleBody #-}
tModuleDef :: forall n_a1pCA. Traversal' (Term n_a1pCA) Module
tModuleDef f_a1tdT (TModule x1_a1tdU x2_a1tdV x3_a1tdW)
  = fmap
      (\ y1_a1tdX -> TModule y1_a1tdX x2_a1tdV x3_a1tdW)
      (f_a1tdT x1_a1tdU)
tModuleDef _ (TList x1_a1tdY x2_a1tdZ x3_a1te0)
  = pure (TList x1_a1tdY x2_a1tdZ x3_a1te0)
tModuleDef
  _
  (TDef x1_a1te1
        x2_a1te2
        x3_a1te3
        x4_a1te4
        x5_a1te5
        x6_a1te6
        x7_a1te7)
  = pure
      (TDef
         x1_a1te1 x2_a1te2 x3_a1te3 x4_a1te4 x5_a1te5 x6_a1te6 x7_a1te7)
tModuleDef _ (TNative x1_a1te8 x2_a1te9 x3_a1tea x4_a1teb x5_a1tec)
  = pure (TNative x1_a1te8 x2_a1te9 x3_a1tea x4_a1teb x5_a1tec)
tModuleDef _ (TConst x1_a1ted x2_a1tee x3_a1tef x4_a1teg x5_a1teh)
  = pure (TConst x1_a1ted x2_a1tee x3_a1tef x4_a1teg x5_a1teh)
tModuleDef _ (TApp x1_a1tei x2_a1tej x3_a1tek)
  = pure (TApp x1_a1tei x2_a1tej x3_a1tek)
tModuleDef _ (TVar x1_a1tel x2_a1tem)
  = pure (TVar x1_a1tel x2_a1tem)
tModuleDef _ (TBinding x1_a1ten x2_a1teo x3_a1tep x4_a1teq)
  = pure (TBinding x1_a1ten x2_a1teo x3_a1tep x4_a1teq)
tModuleDef _ (TObject x1_a1ter x2_a1tes x3_a1tet)
  = pure (TObject x1_a1ter x2_a1tes x3_a1tet)
tModuleDef _ (TSchema x1_a1teu x2_a1tev x3_a1tew x4_a1tex x5_a1tey)
  = pure (TSchema x1_a1teu x2_a1tev x3_a1tew x4_a1tex x5_a1tey)
tModuleDef _ (TLiteral x1_a1tez x2_a1teA)
  = pure (TLiteral x1_a1tez x2_a1teA)
tModuleDef _ (TKeySet x1_a1teB x2_a1teC)
  = pure (TKeySet x1_a1teB x2_a1teC)
tModuleDef _ (TUse x1_a1teD x2_a1teE x3_a1teF)
  = pure (TUse x1_a1teD x2_a1teE x3_a1teF)
tModuleDef _ (TValue x1_a1teG x2_a1teH)
  = pure (TValue x1_a1teG x2_a1teH)
tModuleDef _ (TStep x1_a1teI x2_a1teJ x3_a1teK x4_a1teL)
  = pure (TStep x1_a1teI x2_a1teJ x3_a1teK x4_a1teL)
tModuleDef _ (TTable x1_a1teM x2_a1teN x3_a1teO x4_a1teP x5_a1teQ)
  = pure (TTable x1_a1teM x2_a1teN x3_a1teO x4_a1teP x5_a1teQ)
{-# INLINE tModuleDef #-}
tModuleHash ::
  forall n_a1pCA. Traversal' (Term n_a1pCA) (Maybe Hash)
tModuleHash _ (TModule x1_a1teR x2_a1teS x3_a1teT)
  = pure (TModule x1_a1teR x2_a1teS x3_a1teT)
tModuleHash _ (TList x1_a1teU x2_a1teV x3_a1teW)
  = pure (TList x1_a1teU x2_a1teV x3_a1teW)
tModuleHash
  _
  (TDef x1_a1teX
        x2_a1teY
        x3_a1teZ
        x4_a1tf0
        x5_a1tf1
        x6_a1tf2
        x7_a1tf3)
  = pure
      (TDef
         x1_a1teX x2_a1teY x3_a1teZ x4_a1tf0 x5_a1tf1 x6_a1tf2 x7_a1tf3)
tModuleHash
  _
  (TNative x1_a1tf4 x2_a1tf5 x3_a1tf6 x4_a1tf7 x5_a1tf8)
  = pure (TNative x1_a1tf4 x2_a1tf5 x3_a1tf6 x4_a1tf7 x5_a1tf8)
tModuleHash _ (TConst x1_a1tf9 x2_a1tfa x3_a1tfb x4_a1tfc x5_a1tfd)
  = pure (TConst x1_a1tf9 x2_a1tfa x3_a1tfb x4_a1tfc x5_a1tfd)
tModuleHash _ (TApp x1_a1tfe x2_a1tff x3_a1tfg)
  = pure (TApp x1_a1tfe x2_a1tff x3_a1tfg)
tModuleHash _ (TVar x1_a1tfh x2_a1tfi)
  = pure (TVar x1_a1tfh x2_a1tfi)
tModuleHash _ (TBinding x1_a1tfj x2_a1tfk x3_a1tfl x4_a1tfm)
  = pure (TBinding x1_a1tfj x2_a1tfk x3_a1tfl x4_a1tfm)
tModuleHash _ (TObject x1_a1tfn x2_a1tfo x3_a1tfp)
  = pure (TObject x1_a1tfn x2_a1tfo x3_a1tfp)
tModuleHash
  _
  (TSchema x1_a1tfq x2_a1tfr x3_a1tfs x4_a1tft x5_a1tfu)
  = pure (TSchema x1_a1tfq x2_a1tfr x3_a1tfs x4_a1tft x5_a1tfu)
tModuleHash _ (TLiteral x1_a1tfv x2_a1tfw)
  = pure (TLiteral x1_a1tfv x2_a1tfw)
tModuleHash _ (TKeySet x1_a1tfx x2_a1tfy)
  = pure (TKeySet x1_a1tfx x2_a1tfy)
tModuleHash f_a1tfz (TUse x1_a1tfA x2_a1tfB x3_a1tfC)
  = fmap
      (\ y1_a1tfD -> TUse x1_a1tfA y1_a1tfD x3_a1tfC) (f_a1tfz x2_a1tfB)
tModuleHash _ (TValue x1_a1tfE x2_a1tfF)
  = pure (TValue x1_a1tfE x2_a1tfF)
tModuleHash _ (TStep x1_a1tfG x2_a1tfH x3_a1tfI x4_a1tfJ)
  = pure (TStep x1_a1tfG x2_a1tfH x3_a1tfI x4_a1tfJ)
tModuleHash _ (TTable x1_a1tfK x2_a1tfL x3_a1tfM x4_a1tfN x5_a1tfO)
  = pure (TTable x1_a1tfK x2_a1tfL x3_a1tfM x4_a1tfN x5_a1tfO)
{-# INLINE tModuleHash #-}
tModuleName :: forall n_a1pCA. Traversal' (Term n_a1pCA) ModuleName
tModuleName _ (TModule x1_a1tfP x2_a1tfQ x3_a1tfR)
  = pure (TModule x1_a1tfP x2_a1tfQ x3_a1tfR)
tModuleName _ (TList x1_a1tfS x2_a1tfT x3_a1tfU)
  = pure (TList x1_a1tfS x2_a1tfT x3_a1tfU)
tModuleName
  _
  (TDef x1_a1tfV
        x2_a1tfW
        x3_a1tfX
        x4_a1tfY
        x5_a1tfZ
        x6_a1tg0
        x7_a1tg1)
  = pure
      (TDef
         x1_a1tfV x2_a1tfW x3_a1tfX x4_a1tfY x5_a1tfZ x6_a1tg0 x7_a1tg1)
tModuleName
  _
  (TNative x1_a1tg2 x2_a1tg3 x3_a1tg4 x4_a1tg5 x5_a1tg6)
  = pure (TNative x1_a1tg2 x2_a1tg3 x3_a1tg4 x4_a1tg5 x5_a1tg6)
tModuleName _ (TConst x1_a1tg7 x2_a1tg8 x3_a1tg9 x4_a1tga x5_a1tgb)
  = pure (TConst x1_a1tg7 x2_a1tg8 x3_a1tg9 x4_a1tga x5_a1tgb)
tModuleName _ (TApp x1_a1tgc x2_a1tgd x3_a1tge)
  = pure (TApp x1_a1tgc x2_a1tgd x3_a1tge)
tModuleName _ (TVar x1_a1tgf x2_a1tgg)
  = pure (TVar x1_a1tgf x2_a1tgg)
tModuleName _ (TBinding x1_a1tgh x2_a1tgi x3_a1tgj x4_a1tgk)
  = pure (TBinding x1_a1tgh x2_a1tgi x3_a1tgj x4_a1tgk)
tModuleName _ (TObject x1_a1tgl x2_a1tgm x3_a1tgn)
  = pure (TObject x1_a1tgl x2_a1tgm x3_a1tgn)
tModuleName
  _
  (TSchema x1_a1tgo x2_a1tgp x3_a1tgq x4_a1tgr x5_a1tgs)
  = pure (TSchema x1_a1tgo x2_a1tgp x3_a1tgq x4_a1tgr x5_a1tgs)
tModuleName _ (TLiteral x1_a1tgt x2_a1tgu)
  = pure (TLiteral x1_a1tgt x2_a1tgu)
tModuleName _ (TKeySet x1_a1tgv x2_a1tgw)
  = pure (TKeySet x1_a1tgv x2_a1tgw)
tModuleName f_a1tgx (TUse x1_a1tgy x2_a1tgz x3_a1tgA)
  = fmap
      (\ y1_a1tgB -> TUse y1_a1tgB x2_a1tgz x3_a1tgA) (f_a1tgx x1_a1tgy)
tModuleName _ (TValue x1_a1tgC x2_a1tgD)
  = pure (TValue x1_a1tgC x2_a1tgD)
tModuleName _ (TStep x1_a1tgE x2_a1tgF x3_a1tgG x4_a1tgH)
  = pure (TStep x1_a1tgE x2_a1tgF x3_a1tgG x4_a1tgH)
tModuleName _ (TTable x1_a1tgI x2_a1tgJ x3_a1tgK x4_a1tgL x5_a1tgM)
  = pure (TTable x1_a1tgI x2_a1tgJ x3_a1tgK x4_a1tgL x5_a1tgM)
{-# INLINE tModuleName #-}
tNativeDocs :: forall n_a1pCA. Traversal' (Term n_a1pCA) Text
tNativeDocs _ (TModule x1_a1tgN x2_a1tgO x3_a1tgP)
  = pure (TModule x1_a1tgN x2_a1tgO x3_a1tgP)
tNativeDocs _ (TList x1_a1tgQ x2_a1tgR x3_a1tgS)
  = pure (TList x1_a1tgQ x2_a1tgR x3_a1tgS)
tNativeDocs
  _
  (TDef x1_a1tgT
        x2_a1tgU
        x3_a1tgV
        x4_a1tgW
        x5_a1tgX
        x6_a1tgY
        x7_a1tgZ)
  = pure
      (TDef
         x1_a1tgT x2_a1tgU x3_a1tgV x4_a1tgW x5_a1tgX x6_a1tgY x7_a1tgZ)
tNativeDocs
  f_a1th0
  (TNative x1_a1th1 x2_a1th2 x3_a1th3 x4_a1th4 x5_a1th5)
  = fmap
      (\ y1_a1th6
         -> TNative x1_a1th1 x2_a1th2 x3_a1th3 y1_a1th6 x5_a1th5)
      (f_a1th0 x4_a1th4)
tNativeDocs _ (TConst x1_a1th7 x2_a1th8 x3_a1th9 x4_a1tha x5_a1thb)
  = pure (TConst x1_a1th7 x2_a1th8 x3_a1th9 x4_a1tha x5_a1thb)
tNativeDocs _ (TApp x1_a1thc x2_a1thd x3_a1the)
  = pure (TApp x1_a1thc x2_a1thd x3_a1the)
tNativeDocs _ (TVar x1_a1thf x2_a1thg)
  = pure (TVar x1_a1thf x2_a1thg)
tNativeDocs _ (TBinding x1_a1thh x2_a1thi x3_a1thj x4_a1thk)
  = pure (TBinding x1_a1thh x2_a1thi x3_a1thj x4_a1thk)
tNativeDocs _ (TObject x1_a1thl x2_a1thm x3_a1thn)
  = pure (TObject x1_a1thl x2_a1thm x3_a1thn)
tNativeDocs
  _
  (TSchema x1_a1tho x2_a1thp x3_a1thq x4_a1thr x5_a1ths)
  = pure (TSchema x1_a1tho x2_a1thp x3_a1thq x4_a1thr x5_a1ths)
tNativeDocs _ (TLiteral x1_a1tht x2_a1thu)
  = pure (TLiteral x1_a1tht x2_a1thu)
tNativeDocs _ (TKeySet x1_a1thv x2_a1thw)
  = pure (TKeySet x1_a1thv x2_a1thw)
tNativeDocs _ (TUse x1_a1thx x2_a1thy x3_a1thz)
  = pure (TUse x1_a1thx x2_a1thy x3_a1thz)
tNativeDocs _ (TValue x1_a1thA x2_a1thB)
  = pure (TValue x1_a1thA x2_a1thB)
tNativeDocs _ (TStep x1_a1thC x2_a1thD x3_a1thE x4_a1thF)
  = pure (TStep x1_a1thC x2_a1thD x3_a1thE x4_a1thF)
tNativeDocs _ (TTable x1_a1thG x2_a1thH x3_a1thI x4_a1thJ x5_a1thK)
  = pure (TTable x1_a1thG x2_a1thH x3_a1thI x4_a1thJ x5_a1thK)
{-# INLINE tNativeDocs #-}
tNativeFun :: forall n_a1pCA. Traversal' (Term n_a1pCA) NativeDFun
tNativeFun _ (TModule x1_a1thL x2_a1thM x3_a1thN)
  = pure (TModule x1_a1thL x2_a1thM x3_a1thN)
tNativeFun _ (TList x1_a1thO x2_a1thP x3_a1thQ)
  = pure (TList x1_a1thO x2_a1thP x3_a1thQ)
tNativeFun
  _
  (TDef x1_a1thR
        x2_a1thS
        x3_a1thT
        x4_a1thU
        x5_a1thV
        x6_a1thW
        x7_a1thX)
  = pure
      (TDef
         x1_a1thR x2_a1thS x3_a1thT x4_a1thU x5_a1thV x6_a1thW x7_a1thX)
tNativeFun
  f_a1thY
  (TNative x1_a1thZ x2_a1ti0 x3_a1ti1 x4_a1ti2 x5_a1ti3)
  = fmap
      (\ y1_a1ti4
         -> TNative x1_a1thZ y1_a1ti4 x3_a1ti1 x4_a1ti2 x5_a1ti3)
      (f_a1thY x2_a1ti0)
tNativeFun _ (TConst x1_a1ti5 x2_a1ti6 x3_a1ti7 x4_a1ti8 x5_a1ti9)
  = pure (TConst x1_a1ti5 x2_a1ti6 x3_a1ti7 x4_a1ti8 x5_a1ti9)
tNativeFun _ (TApp x1_a1tia x2_a1tib x3_a1tic)
  = pure (TApp x1_a1tia x2_a1tib x3_a1tic)
tNativeFun _ (TVar x1_a1tid x2_a1tie)
  = pure (TVar x1_a1tid x2_a1tie)
tNativeFun _ (TBinding x1_a1tif x2_a1tig x3_a1tih x4_a1tii)
  = pure (TBinding x1_a1tif x2_a1tig x3_a1tih x4_a1tii)
tNativeFun _ (TObject x1_a1tij x2_a1tik x3_a1til)
  = pure (TObject x1_a1tij x2_a1tik x3_a1til)
tNativeFun _ (TSchema x1_a1tim x2_a1tin x3_a1tio x4_a1tip x5_a1tiq)
  = pure (TSchema x1_a1tim x2_a1tin x3_a1tio x4_a1tip x5_a1tiq)
tNativeFun _ (TLiteral x1_a1tir x2_a1tis)
  = pure (TLiteral x1_a1tir x2_a1tis)
tNativeFun _ (TKeySet x1_a1tit x2_a1tiu)
  = pure (TKeySet x1_a1tit x2_a1tiu)
tNativeFun _ (TUse x1_a1tiv x2_a1tiw x3_a1tix)
  = pure (TUse x1_a1tiv x2_a1tiw x3_a1tix)
tNativeFun _ (TValue x1_a1tiy x2_a1tiz)
  = pure (TValue x1_a1tiy x2_a1tiz)
tNativeFun _ (TStep x1_a1tiA x2_a1tiB x3_a1tiC x4_a1tiD)
  = pure (TStep x1_a1tiA x2_a1tiB x3_a1tiC x4_a1tiD)
tNativeFun _ (TTable x1_a1tiE x2_a1tiF x3_a1tiG x4_a1tiH x5_a1tiI)
  = pure (TTable x1_a1tiE x2_a1tiF x3_a1tiG x4_a1tiH x5_a1tiI)
{-# INLINE tNativeFun #-}
tNativeName ::
  forall n_a1pCA. Traversal' (Term n_a1pCA) NativeDefName
tNativeName _ (TModule x1_a1tiJ x2_a1tiK x3_a1tiL)
  = pure (TModule x1_a1tiJ x2_a1tiK x3_a1tiL)
tNativeName _ (TList x1_a1tiM x2_a1tiN x3_a1tiO)
  = pure (TList x1_a1tiM x2_a1tiN x3_a1tiO)
tNativeName
  _
  (TDef x1_a1tiP
        x2_a1tiQ
        x3_a1tiR
        x4_a1tiS
        x5_a1tiT
        x6_a1tiU
        x7_a1tiV)
  = pure
      (TDef
         x1_a1tiP x2_a1tiQ x3_a1tiR x4_a1tiS x5_a1tiT x6_a1tiU x7_a1tiV)
tNativeName
  f_a1tiW
  (TNative x1_a1tiX x2_a1tiY x3_a1tiZ x4_a1tj0 x5_a1tj1)
  = fmap
      (\ y1_a1tj2
         -> TNative y1_a1tj2 x2_a1tiY x3_a1tiZ x4_a1tj0 x5_a1tj1)
      (f_a1tiW x1_a1tiX)
tNativeName _ (TConst x1_a1tj3 x2_a1tj4 x3_a1tj5 x4_a1tj6 x5_a1tj7)
  = pure (TConst x1_a1tj3 x2_a1tj4 x3_a1tj5 x4_a1tj6 x5_a1tj7)
tNativeName _ (TApp x1_a1tj8 x2_a1tj9 x3_a1tja)
  = pure (TApp x1_a1tj8 x2_a1tj9 x3_a1tja)
tNativeName _ (TVar x1_a1tjb x2_a1tjc)
  = pure (TVar x1_a1tjb x2_a1tjc)
tNativeName _ (TBinding x1_a1tjd x2_a1tje x3_a1tjf x4_a1tjg)
  = pure (TBinding x1_a1tjd x2_a1tje x3_a1tjf x4_a1tjg)
tNativeName _ (TObject x1_a1tjh x2_a1tji x3_a1tjj)
  = pure (TObject x1_a1tjh x2_a1tji x3_a1tjj)
tNativeName
  _
  (TSchema x1_a1tjk x2_a1tjl x3_a1tjm x4_a1tjn x5_a1tjo)
  = pure (TSchema x1_a1tjk x2_a1tjl x3_a1tjm x4_a1tjn x5_a1tjo)
tNativeName _ (TLiteral x1_a1tjp x2_a1tjq)
  = pure (TLiteral x1_a1tjp x2_a1tjq)
tNativeName _ (TKeySet x1_a1tjr x2_a1tjs)
  = pure (TKeySet x1_a1tjr x2_a1tjs)
tNativeName _ (TUse x1_a1tjt x2_a1tju x3_a1tjv)
  = pure (TUse x1_a1tjt x2_a1tju x3_a1tjv)
tNativeName _ (TValue x1_a1tjw x2_a1tjx)
  = pure (TValue x1_a1tjw x2_a1tjx)
tNativeName _ (TStep x1_a1tjy x2_a1tjz x3_a1tjA x4_a1tjB)
  = pure (TStep x1_a1tjy x2_a1tjz x3_a1tjA x4_a1tjB)
tNativeName _ (TTable x1_a1tjC x2_a1tjD x3_a1tjE x4_a1tjF x5_a1tjG)
  = pure (TTable x1_a1tjC x2_a1tjD x3_a1tjE x4_a1tjF x5_a1tjG)
{-# INLINE tNativeName #-}
tObject ::
  forall n_a1pCA.
  Traversal' (Term n_a1pCA) [(Term n_a1pCA, Term n_a1pCA)]
tObject _ (TModule x1_a1tjH x2_a1tjI x3_a1tjJ)
  = pure (TModule x1_a1tjH x2_a1tjI x3_a1tjJ)
tObject _ (TList x1_a1tjK x2_a1tjL x3_a1tjM)
  = pure (TList x1_a1tjK x2_a1tjL x3_a1tjM)
tObject
  _
  (TDef x1_a1tjN
        x2_a1tjO
        x3_a1tjP
        x4_a1tjQ
        x5_a1tjR
        x6_a1tjS
        x7_a1tjT)
  = pure
      (TDef
         x1_a1tjN x2_a1tjO x3_a1tjP x4_a1tjQ x5_a1tjR x6_a1tjS x7_a1tjT)
tObject _ (TNative x1_a1tjU x2_a1tjV x3_a1tjW x4_a1tjX x5_a1tjY)
  = pure (TNative x1_a1tjU x2_a1tjV x3_a1tjW x4_a1tjX x5_a1tjY)
tObject _ (TConst x1_a1tjZ x2_a1tk0 x3_a1tk1 x4_a1tk2 x5_a1tk3)
  = pure (TConst x1_a1tjZ x2_a1tk0 x3_a1tk1 x4_a1tk2 x5_a1tk3)
tObject _ (TApp x1_a1tk4 x2_a1tk5 x3_a1tk6)
  = pure (TApp x1_a1tk4 x2_a1tk5 x3_a1tk6)
tObject _ (TVar x1_a1tk7 x2_a1tk8) = pure (TVar x1_a1tk7 x2_a1tk8)
tObject _ (TBinding x1_a1tk9 x2_a1tka x3_a1tkb x4_a1tkc)
  = pure (TBinding x1_a1tk9 x2_a1tka x3_a1tkb x4_a1tkc)
tObject f_a1tkd (TObject x1_a1tke x2_a1tkf x3_a1tkg)
  = fmap
      (\ y1_a1tkh -> TObject y1_a1tkh x2_a1tkf x3_a1tkg)
      (f_a1tkd x1_a1tke)
tObject _ (TSchema x1_a1tki x2_a1tkj x3_a1tkk x4_a1tkl x5_a1tkm)
  = pure (TSchema x1_a1tki x2_a1tkj x3_a1tkk x4_a1tkl x5_a1tkm)
tObject _ (TLiteral x1_a1tkn x2_a1tko)
  = pure (TLiteral x1_a1tkn x2_a1tko)
tObject _ (TKeySet x1_a1tkp x2_a1tkq)
  = pure (TKeySet x1_a1tkp x2_a1tkq)
tObject _ (TUse x1_a1tkr x2_a1tks x3_a1tkt)
  = pure (TUse x1_a1tkr x2_a1tks x3_a1tkt)
tObject _ (TValue x1_a1tku x2_a1tkv)
  = pure (TValue x1_a1tku x2_a1tkv)
tObject _ (TStep x1_a1tkw x2_a1tkx x3_a1tky x4_a1tkz)
  = pure (TStep x1_a1tkw x2_a1tkx x3_a1tky x4_a1tkz)
tObject _ (TTable x1_a1tkA x2_a1tkB x3_a1tkC x4_a1tkD x5_a1tkE)
  = pure (TTable x1_a1tkA x2_a1tkB x3_a1tkC x4_a1tkD x5_a1tkE)
{-# INLINE tObject #-}
tObjectType ::
  forall n_a1pCA. Traversal' (Term n_a1pCA) (Type (Term n_a1pCA))
tObjectType _ (TModule x1_a1tkF x2_a1tkG x3_a1tkH)
  = pure (TModule x1_a1tkF x2_a1tkG x3_a1tkH)
tObjectType _ (TList x1_a1tkI x2_a1tkJ x3_a1tkK)
  = pure (TList x1_a1tkI x2_a1tkJ x3_a1tkK)
tObjectType
  _
  (TDef x1_a1tkL
        x2_a1tkM
        x3_a1tkN
        x4_a1tkO
        x5_a1tkP
        x6_a1tkQ
        x7_a1tkR)
  = pure
      (TDef
         x1_a1tkL x2_a1tkM x3_a1tkN x4_a1tkO x5_a1tkP x6_a1tkQ x7_a1tkR)
tObjectType
  _
  (TNative x1_a1tkS x2_a1tkT x3_a1tkU x4_a1tkV x5_a1tkW)
  = pure (TNative x1_a1tkS x2_a1tkT x3_a1tkU x4_a1tkV x5_a1tkW)
tObjectType _ (TConst x1_a1tkX x2_a1tkY x3_a1tkZ x4_a1tl0 x5_a1tl1)
  = pure (TConst x1_a1tkX x2_a1tkY x3_a1tkZ x4_a1tl0 x5_a1tl1)
tObjectType _ (TApp x1_a1tl2 x2_a1tl3 x3_a1tl4)
  = pure (TApp x1_a1tl2 x2_a1tl3 x3_a1tl4)
tObjectType _ (TVar x1_a1tl5 x2_a1tl6)
  = pure (TVar x1_a1tl5 x2_a1tl6)
tObjectType _ (TBinding x1_a1tl7 x2_a1tl8 x3_a1tl9 x4_a1tla)
  = pure (TBinding x1_a1tl7 x2_a1tl8 x3_a1tl9 x4_a1tla)
tObjectType f_a1tlb (TObject x1_a1tlc x2_a1tld x3_a1tle)
  = fmap
      (\ y1_a1tlf -> TObject x1_a1tlc y1_a1tlf x3_a1tle)
      (f_a1tlb x2_a1tld)
tObjectType
  _
  (TSchema x1_a1tlg x2_a1tlh x3_a1tli x4_a1tlj x5_a1tlk)
  = pure (TSchema x1_a1tlg x2_a1tlh x3_a1tli x4_a1tlj x5_a1tlk)
tObjectType _ (TLiteral x1_a1tll x2_a1tlm)
  = pure (TLiteral x1_a1tll x2_a1tlm)
tObjectType _ (TKeySet x1_a1tln x2_a1tlo)
  = pure (TKeySet x1_a1tln x2_a1tlo)
tObjectType _ (TUse x1_a1tlp x2_a1tlq x3_a1tlr)
  = pure (TUse x1_a1tlp x2_a1tlq x3_a1tlr)
tObjectType _ (TValue x1_a1tls x2_a1tlt)
  = pure (TValue x1_a1tls x2_a1tlt)
tObjectType _ (TStep x1_a1tlu x2_a1tlv x3_a1tlw x4_a1tlx)
  = pure (TStep x1_a1tlu x2_a1tlv x3_a1tlw x4_a1tlx)
tObjectType _ (TTable x1_a1tly x2_a1tlz x3_a1tlA x4_a1tlB x5_a1tlC)
  = pure (TTable x1_a1tly x2_a1tlz x3_a1tlA x4_a1tlB x5_a1tlC)
{-# INLINE tObjectType #-}
tSchemaName :: forall n_a1pCA. Traversal' (Term n_a1pCA) TypeName
tSchemaName _ (TModule x1_a1tlD x2_a1tlE x3_a1tlF)
  = pure (TModule x1_a1tlD x2_a1tlE x3_a1tlF)
tSchemaName _ (TList x1_a1tlG x2_a1tlH x3_a1tlI)
  = pure (TList x1_a1tlG x2_a1tlH x3_a1tlI)
tSchemaName
  _
  (TDef x1_a1tlJ
        x2_a1tlK
        x3_a1tlL
        x4_a1tlM
        x5_a1tlN
        x6_a1tlO
        x7_a1tlP)
  = pure
      (TDef
         x1_a1tlJ x2_a1tlK x3_a1tlL x4_a1tlM x5_a1tlN x6_a1tlO x7_a1tlP)
tSchemaName
  _
  (TNative x1_a1tlQ x2_a1tlR x3_a1tlS x4_a1tlT x5_a1tlU)
  = pure (TNative x1_a1tlQ x2_a1tlR x3_a1tlS x4_a1tlT x5_a1tlU)
tSchemaName _ (TConst x1_a1tlV x2_a1tlW x3_a1tlX x4_a1tlY x5_a1tlZ)
  = pure (TConst x1_a1tlV x2_a1tlW x3_a1tlX x4_a1tlY x5_a1tlZ)
tSchemaName _ (TApp x1_a1tm0 x2_a1tm1 x3_a1tm2)
  = pure (TApp x1_a1tm0 x2_a1tm1 x3_a1tm2)
tSchemaName _ (TVar x1_a1tm3 x2_a1tm4)
  = pure (TVar x1_a1tm3 x2_a1tm4)
tSchemaName _ (TBinding x1_a1tm5 x2_a1tm6 x3_a1tm7 x4_a1tm8)
  = pure (TBinding x1_a1tm5 x2_a1tm6 x3_a1tm7 x4_a1tm8)
tSchemaName _ (TObject x1_a1tm9 x2_a1tma x3_a1tmb)
  = pure (TObject x1_a1tm9 x2_a1tma x3_a1tmb)
tSchemaName
  f_a1tmc
  (TSchema x1_a1tmd x2_a1tme x3_a1tmf x4_a1tmg x5_a1tmh)
  = fmap
      (\ y1_a1tmi
         -> TSchema y1_a1tmi x2_a1tme x3_a1tmf x4_a1tmg x5_a1tmh)
      (f_a1tmc x1_a1tmd)
tSchemaName _ (TLiteral x1_a1tmj x2_a1tmk)
  = pure (TLiteral x1_a1tmj x2_a1tmk)
tSchemaName _ (TKeySet x1_a1tml x2_a1tmm)
  = pure (TKeySet x1_a1tml x2_a1tmm)
tSchemaName _ (TUse x1_a1tmn x2_a1tmo x3_a1tmp)
  = pure (TUse x1_a1tmn x2_a1tmo x3_a1tmp)
tSchemaName _ (TValue x1_a1tmq x2_a1tmr)
  = pure (TValue x1_a1tmq x2_a1tmr)
tSchemaName _ (TStep x1_a1tms x2_a1tmt x3_a1tmu x4_a1tmv)
  = pure (TStep x1_a1tms x2_a1tmt x3_a1tmu x4_a1tmv)
tSchemaName _ (TTable x1_a1tmw x2_a1tmx x3_a1tmy x4_a1tmz x5_a1tmA)
  = pure (TTable x1_a1tmw x2_a1tmx x3_a1tmy x4_a1tmz x5_a1tmA)
{-# INLINE tSchemaName #-}
tStepEntity ::
  forall n_a1pCA. Traversal' (Term n_a1pCA) (Maybe (Term n_a1pCA))
tStepEntity _ (TModule x1_a1tmB x2_a1tmC x3_a1tmD)
  = pure (TModule x1_a1tmB x2_a1tmC x3_a1tmD)
tStepEntity _ (TList x1_a1tmE x2_a1tmF x3_a1tmG)
  = pure (TList x1_a1tmE x2_a1tmF x3_a1tmG)
tStepEntity
  _
  (TDef x1_a1tmH
        x2_a1tmI
        x3_a1tmJ
        x4_a1tmK
        x5_a1tmL
        x6_a1tmM
        x7_a1tmN)
  = pure
      (TDef
         x1_a1tmH x2_a1tmI x3_a1tmJ x4_a1tmK x5_a1tmL x6_a1tmM x7_a1tmN)
tStepEntity
  _
  (TNative x1_a1tmO x2_a1tmP x3_a1tmQ x4_a1tmR x5_a1tmS)
  = pure (TNative x1_a1tmO x2_a1tmP x3_a1tmQ x4_a1tmR x5_a1tmS)
tStepEntity _ (TConst x1_a1tmT x2_a1tmU x3_a1tmV x4_a1tmW x5_a1tmX)
  = pure (TConst x1_a1tmT x2_a1tmU x3_a1tmV x4_a1tmW x5_a1tmX)
tStepEntity _ (TApp x1_a1tmY x2_a1tmZ x3_a1tn0)
  = pure (TApp x1_a1tmY x2_a1tmZ x3_a1tn0)
tStepEntity _ (TVar x1_a1tn1 x2_a1tn2)
  = pure (TVar x1_a1tn1 x2_a1tn2)
tStepEntity _ (TBinding x1_a1tn3 x2_a1tn4 x3_a1tn5 x4_a1tn6)
  = pure (TBinding x1_a1tn3 x2_a1tn4 x3_a1tn5 x4_a1tn6)
tStepEntity _ (TObject x1_a1tn7 x2_a1tn8 x3_a1tn9)
  = pure (TObject x1_a1tn7 x2_a1tn8 x3_a1tn9)
tStepEntity
  _
  (TSchema x1_a1tna x2_a1tnb x3_a1tnc x4_a1tnd x5_a1tne)
  = pure (TSchema x1_a1tna x2_a1tnb x3_a1tnc x4_a1tnd x5_a1tne)
tStepEntity _ (TLiteral x1_a1tnf x2_a1tng)
  = pure (TLiteral x1_a1tnf x2_a1tng)
tStepEntity _ (TKeySet x1_a1tnh x2_a1tni)
  = pure (TKeySet x1_a1tnh x2_a1tni)
tStepEntity _ (TUse x1_a1tnj x2_a1tnk x3_a1tnl)
  = pure (TUse x1_a1tnj x2_a1tnk x3_a1tnl)
tStepEntity _ (TValue x1_a1tnm x2_a1tnn)
  = pure (TValue x1_a1tnm x2_a1tnn)
tStepEntity f_a1tno (TStep x1_a1tnp x2_a1tnq x3_a1tnr x4_a1tns)
  = fmap
      (\ y1_a1tnt -> TStep y1_a1tnt x2_a1tnq x3_a1tnr x4_a1tns)
      (f_a1tno x1_a1tnp)
tStepEntity _ (TTable x1_a1tnu x2_a1tnv x3_a1tnw x4_a1tnx x5_a1tny)
  = pure (TTable x1_a1tnu x2_a1tnv x3_a1tnw x4_a1tnx x5_a1tny)
{-# INLINE tStepEntity #-}
tStepExec ::
  forall n_a1pCA. Traversal' (Term n_a1pCA) (Term n_a1pCA)
tStepExec _ (TModule x1_a1tnz x2_a1tnA x3_a1tnB)
  = pure (TModule x1_a1tnz x2_a1tnA x3_a1tnB)
tStepExec _ (TList x1_a1tnC x2_a1tnD x3_a1tnE)
  = pure (TList x1_a1tnC x2_a1tnD x3_a1tnE)
tStepExec
  _
  (TDef x1_a1tnF
        x2_a1tnG
        x3_a1tnH
        x4_a1tnI
        x5_a1tnJ
        x6_a1tnK
        x7_a1tnL)
  = pure
      (TDef
         x1_a1tnF x2_a1tnG x3_a1tnH x4_a1tnI x5_a1tnJ x6_a1tnK x7_a1tnL)
tStepExec _ (TNative x1_a1tnM x2_a1tnN x3_a1tnO x4_a1tnP x5_a1tnQ)
  = pure (TNative x1_a1tnM x2_a1tnN x3_a1tnO x4_a1tnP x5_a1tnQ)
tStepExec _ (TConst x1_a1tnR x2_a1tnS x3_a1tnT x4_a1tnU x5_a1tnV)
  = pure (TConst x1_a1tnR x2_a1tnS x3_a1tnT x4_a1tnU x5_a1tnV)
tStepExec _ (TApp x1_a1tnW x2_a1tnX x3_a1tnY)
  = pure (TApp x1_a1tnW x2_a1tnX x3_a1tnY)
tStepExec _ (TVar x1_a1tnZ x2_a1to0)
  = pure (TVar x1_a1tnZ x2_a1to0)
tStepExec _ (TBinding x1_a1to1 x2_a1to2 x3_a1to3 x4_a1to4)
  = pure (TBinding x1_a1to1 x2_a1to2 x3_a1to3 x4_a1to4)
tStepExec _ (TObject x1_a1to5 x2_a1to6 x3_a1to7)
  = pure (TObject x1_a1to5 x2_a1to6 x3_a1to7)
tStepExec _ (TSchema x1_a1to8 x2_a1to9 x3_a1toa x4_a1tob x5_a1toc)
  = pure (TSchema x1_a1to8 x2_a1to9 x3_a1toa x4_a1tob x5_a1toc)
tStepExec _ (TLiteral x1_a1tod x2_a1toe)
  = pure (TLiteral x1_a1tod x2_a1toe)
tStepExec _ (TKeySet x1_a1tof x2_a1tog)
  = pure (TKeySet x1_a1tof x2_a1tog)
tStepExec _ (TUse x1_a1toh x2_a1toi x3_a1toj)
  = pure (TUse x1_a1toh x2_a1toi x3_a1toj)
tStepExec _ (TValue x1_a1tok x2_a1tol)
  = pure (TValue x1_a1tok x2_a1tol)
tStepExec f_a1tom (TStep x1_a1ton x2_a1too x3_a1top x4_a1toq)
  = fmap
      (\ y1_a1tor -> TStep x1_a1ton y1_a1tor x3_a1top x4_a1toq)
      (f_a1tom x2_a1too)
tStepExec _ (TTable x1_a1tos x2_a1tot x3_a1tou x4_a1tov x5_a1tow)
  = pure (TTable x1_a1tos x2_a1tot x3_a1tou x4_a1tov x5_a1tow)
{-# INLINE tStepExec #-}
tStepRollback ::
  forall n_a1pCA. Traversal' (Term n_a1pCA) (Maybe (Term n_a1pCA))
tStepRollback _ (TModule x1_a1tox x2_a1toy x3_a1toz)
  = pure (TModule x1_a1tox x2_a1toy x3_a1toz)
tStepRollback _ (TList x1_a1toA x2_a1toB x3_a1toC)
  = pure (TList x1_a1toA x2_a1toB x3_a1toC)
tStepRollback
  _
  (TDef x1_a1toD
        x2_a1toE
        x3_a1toF
        x4_a1toG
        x5_a1toH
        x6_a1toI
        x7_a1toJ)
  = pure
      (TDef
         x1_a1toD x2_a1toE x3_a1toF x4_a1toG x5_a1toH x6_a1toI x7_a1toJ)
tStepRollback
  _
  (TNative x1_a1toK x2_a1toL x3_a1toM x4_a1toN x5_a1toO)
  = pure (TNative x1_a1toK x2_a1toL x3_a1toM x4_a1toN x5_a1toO)
tStepRollback
  _
  (TConst x1_a1toP x2_a1toQ x3_a1toR x4_a1toS x5_a1toT)
  = pure (TConst x1_a1toP x2_a1toQ x3_a1toR x4_a1toS x5_a1toT)
tStepRollback _ (TApp x1_a1toU x2_a1toV x3_a1toW)
  = pure (TApp x1_a1toU x2_a1toV x3_a1toW)
tStepRollback _ (TVar x1_a1toX x2_a1toY)
  = pure (TVar x1_a1toX x2_a1toY)
tStepRollback _ (TBinding x1_a1toZ x2_a1tp0 x3_a1tp1 x4_a1tp2)
  = pure (TBinding x1_a1toZ x2_a1tp0 x3_a1tp1 x4_a1tp2)
tStepRollback _ (TObject x1_a1tp3 x2_a1tp4 x3_a1tp5)
  = pure (TObject x1_a1tp3 x2_a1tp4 x3_a1tp5)
tStepRollback
  _
  (TSchema x1_a1tp6 x2_a1tp7 x3_a1tp8 x4_a1tp9 x5_a1tpa)
  = pure (TSchema x1_a1tp6 x2_a1tp7 x3_a1tp8 x4_a1tp9 x5_a1tpa)
tStepRollback _ (TLiteral x1_a1tpb x2_a1tpc)
  = pure (TLiteral x1_a1tpb x2_a1tpc)
tStepRollback _ (TKeySet x1_a1tpd x2_a1tpe)
  = pure (TKeySet x1_a1tpd x2_a1tpe)
tStepRollback _ (TUse x1_a1tpf x2_a1tpg x3_a1tph)
  = pure (TUse x1_a1tpf x2_a1tpg x3_a1tph)
tStepRollback _ (TValue x1_a1tpi x2_a1tpj)
  = pure (TValue x1_a1tpi x2_a1tpj)
tStepRollback f_a1tpk (TStep x1_a1tpl x2_a1tpm x3_a1tpn x4_a1tpo)
  = fmap
      (\ y1_a1tpp -> TStep x1_a1tpl x2_a1tpm y1_a1tpp x4_a1tpo)
      (f_a1tpk x3_a1tpn)
tStepRollback
  _
  (TTable x1_a1tpq x2_a1tpr x3_a1tps x4_a1tpt x5_a1tpu)
  = pure (TTable x1_a1tpq x2_a1tpr x3_a1tps x4_a1tpt x5_a1tpu)
{-# INLINE tStepRollback #-}
tTableName :: forall n_a1pCA. Traversal' (Term n_a1pCA) TableName
tTableName _ (TModule x1_a1tpv x2_a1tpw x3_a1tpx)
  = pure (TModule x1_a1tpv x2_a1tpw x3_a1tpx)
tTableName _ (TList x1_a1tpy x2_a1tpz x3_a1tpA)
  = pure (TList x1_a1tpy x2_a1tpz x3_a1tpA)
tTableName
  _
  (TDef x1_a1tpB
        x2_a1tpC
        x3_a1tpD
        x4_a1tpE
        x5_a1tpF
        x6_a1tpG
        x7_a1tpH)
  = pure
      (TDef
         x1_a1tpB x2_a1tpC x3_a1tpD x4_a1tpE x5_a1tpF x6_a1tpG x7_a1tpH)
tTableName _ (TNative x1_a1tpI x2_a1tpJ x3_a1tpK x4_a1tpL x5_a1tpM)
  = pure (TNative x1_a1tpI x2_a1tpJ x3_a1tpK x4_a1tpL x5_a1tpM)
tTableName _ (TConst x1_a1tpN x2_a1tpO x3_a1tpP x4_a1tpQ x5_a1tpR)
  = pure (TConst x1_a1tpN x2_a1tpO x3_a1tpP x4_a1tpQ x5_a1tpR)
tTableName _ (TApp x1_a1tpS x2_a1tpT x3_a1tpU)
  = pure (TApp x1_a1tpS x2_a1tpT x3_a1tpU)
tTableName _ (TVar x1_a1tpV x2_a1tpW)
  = pure (TVar x1_a1tpV x2_a1tpW)
tTableName _ (TBinding x1_a1tpX x2_a1tpY x3_a1tpZ x4_a1tq0)
  = pure (TBinding x1_a1tpX x2_a1tpY x3_a1tpZ x4_a1tq0)
tTableName _ (TObject x1_a1tq1 x2_a1tq2 x3_a1tq3)
  = pure (TObject x1_a1tq1 x2_a1tq2 x3_a1tq3)
tTableName _ (TSchema x1_a1tq4 x2_a1tq5 x3_a1tq6 x4_a1tq7 x5_a1tq8)
  = pure (TSchema x1_a1tq4 x2_a1tq5 x3_a1tq6 x4_a1tq7 x5_a1tq8)
tTableName _ (TLiteral x1_a1tq9 x2_a1tqa)
  = pure (TLiteral x1_a1tq9 x2_a1tqa)
tTableName _ (TKeySet x1_a1tqb x2_a1tqc)
  = pure (TKeySet x1_a1tqb x2_a1tqc)
tTableName _ (TUse x1_a1tqd x2_a1tqe x3_a1tqf)
  = pure (TUse x1_a1tqd x2_a1tqe x3_a1tqf)
tTableName _ (TValue x1_a1tqg x2_a1tqh)
  = pure (TValue x1_a1tqg x2_a1tqh)
tTableName _ (TStep x1_a1tqi x2_a1tqj x3_a1tqk x4_a1tql)
  = pure (TStep x1_a1tqi x2_a1tqj x3_a1tqk x4_a1tql)
tTableName
  f_a1tqm
  (TTable x1_a1tqn x2_a1tqo x3_a1tqp x4_a1tqq x5_a1tqr)
  = fmap
      (\ y1_a1tqs -> TTable y1_a1tqs x2_a1tqo x3_a1tqp x4_a1tqq x5_a1tqr)
      (f_a1tqm x1_a1tqn)
{-# INLINE tTableName #-}
tTableType ::
  forall n_a1pCA. Traversal' (Term n_a1pCA) (Type (Term n_a1pCA))
tTableType _ (TModule x1_a1tqt x2_a1tqu x3_a1tqv)
  = pure (TModule x1_a1tqt x2_a1tqu x3_a1tqv)
tTableType _ (TList x1_a1tqw x2_a1tqx x3_a1tqy)
  = pure (TList x1_a1tqw x2_a1tqx x3_a1tqy)
tTableType
  _
  (TDef x1_a1tqz
        x2_a1tqA
        x3_a1tqB
        x4_a1tqC
        x5_a1tqD
        x6_a1tqE
        x7_a1tqF)
  = pure
      (TDef
         x1_a1tqz x2_a1tqA x3_a1tqB x4_a1tqC x5_a1tqD x6_a1tqE x7_a1tqF)
tTableType _ (TNative x1_a1tqG x2_a1tqH x3_a1tqI x4_a1tqJ x5_a1tqK)
  = pure (TNative x1_a1tqG x2_a1tqH x3_a1tqI x4_a1tqJ x5_a1tqK)
tTableType _ (TConst x1_a1tqL x2_a1tqM x3_a1tqN x4_a1tqO x5_a1tqP)
  = pure (TConst x1_a1tqL x2_a1tqM x3_a1tqN x4_a1tqO x5_a1tqP)
tTableType _ (TApp x1_a1tqQ x2_a1tqR x3_a1tqS)
  = pure (TApp x1_a1tqQ x2_a1tqR x3_a1tqS)
tTableType _ (TVar x1_a1tqT x2_a1tqU)
  = pure (TVar x1_a1tqT x2_a1tqU)
tTableType _ (TBinding x1_a1tqV x2_a1tqW x3_a1tqX x4_a1tqY)
  = pure (TBinding x1_a1tqV x2_a1tqW x3_a1tqX x4_a1tqY)
tTableType _ (TObject x1_a1tqZ x2_a1tr0 x3_a1tr1)
  = pure (TObject x1_a1tqZ x2_a1tr0 x3_a1tr1)
tTableType _ (TSchema x1_a1tr2 x2_a1tr3 x3_a1tr4 x4_a1tr5 x5_a1tr6)
  = pure (TSchema x1_a1tr2 x2_a1tr3 x3_a1tr4 x4_a1tr5 x5_a1tr6)
tTableType _ (TLiteral x1_a1tr7 x2_a1tr8)
  = pure (TLiteral x1_a1tr7 x2_a1tr8)
tTableType _ (TKeySet x1_a1tr9 x2_a1tra)
  = pure (TKeySet x1_a1tr9 x2_a1tra)
tTableType _ (TUse x1_a1trb x2_a1trc x3_a1trd)
  = pure (TUse x1_a1trb x2_a1trc x3_a1trd)
tTableType _ (TValue x1_a1tre x2_a1trf)
  = pure (TValue x1_a1tre x2_a1trf)
tTableType _ (TStep x1_a1trg x2_a1trh x3_a1tri x4_a1trj)
  = pure (TStep x1_a1trg x2_a1trh x3_a1tri x4_a1trj)
tTableType
  f_a1trk
  (TTable x1_a1trl x2_a1trm x3_a1trn x4_a1tro x5_a1trp)
  = fmap
      (\ y1_a1trq -> TTable x1_a1trl x2_a1trm y1_a1trq x4_a1tro x5_a1trp)
      (f_a1trk x3_a1trn)
{-# INLINE tTableType #-}
tValue :: forall n_a1pCA. Traversal' (Term n_a1pCA) Value
tValue _ (TModule x1_a1trr x2_a1trs x3_a1trt)
  = pure (TModule x1_a1trr x2_a1trs x3_a1trt)
tValue _ (TList x1_a1tru x2_a1trv x3_a1trw)
  = pure (TList x1_a1tru x2_a1trv x3_a1trw)
tValue
  _
  (TDef x1_a1trx
        x2_a1try
        x3_a1trz
        x4_a1trA
        x5_a1trB
        x6_a1trC
        x7_a1trD)
  = pure
      (TDef
         x1_a1trx x2_a1try x3_a1trz x4_a1trA x5_a1trB x6_a1trC x7_a1trD)
tValue _ (TNative x1_a1trE x2_a1trF x3_a1trG x4_a1trH x5_a1trI)
  = pure (TNative x1_a1trE x2_a1trF x3_a1trG x4_a1trH x5_a1trI)
tValue _ (TConst x1_a1trJ x2_a1trK x3_a1trL x4_a1trM x5_a1trN)
  = pure (TConst x1_a1trJ x2_a1trK x3_a1trL x4_a1trM x5_a1trN)
tValue _ (TApp x1_a1trO x2_a1trP x3_a1trQ)
  = pure (TApp x1_a1trO x2_a1trP x3_a1trQ)
tValue _ (TVar x1_a1trR x2_a1trS) = pure (TVar x1_a1trR x2_a1trS)
tValue _ (TBinding x1_a1trT x2_a1trU x3_a1trV x4_a1trW)
  = pure (TBinding x1_a1trT x2_a1trU x3_a1trV x4_a1trW)
tValue _ (TObject x1_a1trX x2_a1trY x3_a1trZ)
  = pure (TObject x1_a1trX x2_a1trY x3_a1trZ)
tValue _ (TSchema x1_a1ts0 x2_a1ts1 x3_a1ts2 x4_a1ts3 x5_a1ts4)
  = pure (TSchema x1_a1ts0 x2_a1ts1 x3_a1ts2 x4_a1ts3 x5_a1ts4)
tValue _ (TLiteral x1_a1ts5 x2_a1ts6)
  = pure (TLiteral x1_a1ts5 x2_a1ts6)
tValue _ (TKeySet x1_a1ts7 x2_a1ts8)
  = pure (TKeySet x1_a1ts7 x2_a1ts8)
tValue _ (TUse x1_a1ts9 x2_a1tsa x3_a1tsb)
  = pure (TUse x1_a1ts9 x2_a1tsa x3_a1tsb)
tValue f_a1tsc (TValue x1_a1tsd x2_a1tse)
  = fmap (\ y1_a1tsf -> TValue y1_a1tsf x2_a1tse) (f_a1tsc x1_a1tsd)
tValue _ (TStep x1_a1tsg x2_a1tsh x3_a1tsi x4_a1tsj)
  = pure (TStep x1_a1tsg x2_a1tsh x3_a1tsi x4_a1tsj)
tValue _ (TTable x1_a1tsk x2_a1tsl x3_a1tsm x4_a1tsn x5_a1tso)
  = pure (TTable x1_a1tsk x2_a1tsl x3_a1tsm x4_a1tsn x5_a1tso)
{-# INLINE tValue #-}
tVar :: forall n_a1pCA. Traversal' (Term n_a1pCA) n_a1pCA
tVar _ (TModule x1_a1tsp x2_a1tsq x3_a1tsr)
  = pure (TModule x1_a1tsp x2_a1tsq x3_a1tsr)
tVar _ (TList x1_a1tss x2_a1tst x3_a1tsu)
  = pure (TList x1_a1tss x2_a1tst x3_a1tsu)
tVar
  _
  (TDef x1_a1tsv
        x2_a1tsw
        x3_a1tsx
        x4_a1tsy
        x5_a1tsz
        x6_a1tsA
        x7_a1tsB)
  = pure
      (TDef
         x1_a1tsv x2_a1tsw x3_a1tsx x4_a1tsy x5_a1tsz x6_a1tsA x7_a1tsB)
tVar _ (TNative x1_a1tsC x2_a1tsD x3_a1tsE x4_a1tsF x5_a1tsG)
  = pure (TNative x1_a1tsC x2_a1tsD x3_a1tsE x4_a1tsF x5_a1tsG)
tVar _ (TConst x1_a1tsH x2_a1tsI x3_a1tsJ x4_a1tsK x5_a1tsL)
  = pure (TConst x1_a1tsH x2_a1tsI x3_a1tsJ x4_a1tsK x5_a1tsL)
tVar _ (TApp x1_a1tsM x2_a1tsN x3_a1tsO)
  = pure (TApp x1_a1tsM x2_a1tsN x3_a1tsO)
tVar f_a1tsP (TVar x1_a1tsQ x2_a1tsR)
  = fmap (\ y1_a1tsS -> TVar y1_a1tsS x2_a1tsR) (f_a1tsP x1_a1tsQ)
tVar _ (TBinding x1_a1tsT x2_a1tsU x3_a1tsV x4_a1tsW)
  = pure (TBinding x1_a1tsT x2_a1tsU x3_a1tsV x4_a1tsW)
tVar _ (TObject x1_a1tsX x2_a1tsY x3_a1tsZ)
  = pure (TObject x1_a1tsX x2_a1tsY x3_a1tsZ)
tVar _ (TSchema x1_a1tt0 x2_a1tt1 x3_a1tt2 x4_a1tt3 x5_a1tt4)
  = pure (TSchema x1_a1tt0 x2_a1tt1 x3_a1tt2 x4_a1tt3 x5_a1tt4)
tVar _ (TLiteral x1_a1tt5 x2_a1tt6)
  = pure (TLiteral x1_a1tt5 x2_a1tt6)
tVar _ (TKeySet x1_a1tt7 x2_a1tt8)
  = pure (TKeySet x1_a1tt7 x2_a1tt8)
tVar _ (TUse x1_a1tt9 x2_a1tta x3_a1ttb)
  = pure (TUse x1_a1tt9 x2_a1tta x3_a1ttb)
tVar _ (TValue x1_a1ttc x2_a1ttd) = pure (TValue x1_a1ttc x2_a1ttd)
tVar _ (TStep x1_a1tte x2_a1ttf x3_a1ttg x4_a1tth)
  = pure (TStep x1_a1tte x2_a1ttf x3_a1ttg x4_a1tth)
tVar _ (TTable x1_a1tti x2_a1ttj x3_a1ttk x4_a1ttl x5_a1ttm)
  = pure (TTable x1_a1tti x2_a1ttj x3_a1ttk x4_a1ttl x5_a1ttm)
{-# INLINE tVar #-}


------------------------------------------------------------------------------
--makeLenses ''FunApp
faDefType :: Lens' FunApp DefType
faDefType
  f_a1uhW
  (FunApp x1_a1uhX x2_a1uhY x3_a1uhZ x4_a1ui0 x5_a1ui1 x6_a1ui2)
  = fmap
      (\ y1_a1ui3
         -> FunApp x1_a1uhX x2_a1uhY x3_a1uhZ y1_a1ui3 x5_a1ui1 x6_a1ui2)
      (f_a1uhW x4_a1ui0)
{-# INLINE faDefType #-}
faDocs :: Lens' FunApp (Maybe Text)
faDocs
  f_a1ui4
  (FunApp x1_a1ui5 x2_a1ui6 x3_a1ui7 x4_a1ui8 x5_a1ui9 x6_a1uia)
  = fmap
      (\ y1_a1uib
         -> FunApp x1_a1ui5 x2_a1ui6 x3_a1ui7 x4_a1ui8 x5_a1ui9 y1_a1uib)
      (f_a1ui4 x6_a1uia)
{-# INLINE faDocs #-}
faInfo :: Lens' FunApp Info
faInfo
  f_a1uic
  (FunApp x1_a1uid x2_a1uie x3_a1uif x4_a1uig x5_a1uih x6_a1uii)
  = fmap
      (\ y1_a1uij
         -> FunApp y1_a1uij x2_a1uie x3_a1uif x4_a1uig x5_a1uih x6_a1uii)
      (f_a1uic x1_a1uid)
{-# INLINE faInfo #-}
faModule :: Lens' FunApp (Maybe ModuleName)
faModule
  f_a1uik
  (FunApp x1_a1uil x2_a1uim x3_a1uin x4_a1uio x5_a1uip x6_a1uiq)
  = fmap
      (\ y1_a1uir
         -> FunApp x1_a1uil x2_a1uim y1_a1uir x4_a1uio x5_a1uip x6_a1uiq)
      (f_a1uik x3_a1uin)
{-# INLINE faModule #-}
faName :: Lens' FunApp Text
faName
  f_a1uis
  (FunApp x1_a1uit x2_a1uiu x3_a1uiv x4_a1uiw x5_a1uix x6_a1uiy)
  = fmap
      (\ y1_a1uiz
         -> FunApp x1_a1uit y1_a1uiz x3_a1uiv x4_a1uiw x5_a1uix x6_a1uiy)
      (f_a1uis x2_a1uiu)
{-# INLINE faName #-}
faTypes :: Lens' FunApp (FunTypes (Term Name))
faTypes
  f_a1uiA
  (FunApp x1_a1uiB x2_a1uiC x3_a1uiD x4_a1uiE x5_a1uiF x6_a1uiG)
  = fmap
      (\ y1_a1uiH
         -> FunApp x1_a1uiB x2_a1uiC x3_a1uiD x4_a1uiE y1_a1uiH x6_a1uiG)
      (f_a1uiA x5_a1uiF)
{-# INLINE faTypes #-}
