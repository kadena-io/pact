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
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

-- |
-- Module      :  Pact.Types
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Main types for the Pact Language.
--

module Pact.Types where


import Control.Lens hiding (op,(.=))
import Text.Trifecta.Delta hiding (Columns)
import Control.Applicative
import Data.List
import Control.Monad
import Prelude hiding (exp)
import Control.Arrow hiding (app)
import Prelude.Extras
import Bound
import Control.Monad.Except
import Control.Monad.State.Strict
import Control.Monad.Reader
import Text.Trifecta (Rendering)
import Text.PrettyPrint.ANSI.Leijen (Pretty,Doc,SimpleDoc,pretty,plain,renderCompact,renderPretty,displayS)
import qualified Data.Map.Strict as M
import qualified Data.HashMap.Strict as HM
import Data.Text (pack,unpack)
import Data.Text.Encoding
import Data.Aeson
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString.Lazy.UTF8 as BSL
import qualified Data.Set as S
import Data.String
import Data.Default
import Data.Char
import Data.Thyme
import Data.Thyme.Format.Aeson ()
import Data.Thyme.Time.Core
import System.Locale
import Data.Scientific
import Data.Word
import Control.Monad.Catch
import GHC.Generics
import Data.Decimal
import Data.Ratio
import qualified Data.Vector as V
import Data.Hashable
import Data.List.NonEmpty (NonEmpty (..))
import Data.Foldable
import Control.Concurrent.MVar
import qualified Text.PrettyPrint.ANSI.Leijen as PP


import Data.Serialize (Serialize)

import Pact.Types.Orphans ()

data RenderColor = RColor | RPlain

renderString :: Pretty a => (Doc -> SimpleDoc) -> RenderColor -> a -> String
renderString renderf colors p = displayS (renderf ((case colors of RColor -> id; RPlain -> plain) (pretty p))) ""

renderCompactString :: Pretty a => a -> String
renderCompactString = renderString renderCompact RPlain

renderPrettyString :: Pretty a => RenderColor -> a -> String
renderPrettyString c a = renderString (renderPretty 0.4 100) c a

data Info = Info { _iInfo :: !(Maybe (Rendering,Delta)) }
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

-- renderer is for line numbers and such
renderInfo :: Info -> String
renderInfo (Info Nothing) = ""
renderInfo (Info (Just (_,d))) =
    case d of
      (Directed f l c _ _) -> BS.toString f ++ ":" ++ show (succ l) ++ ":" ++ show c
      (Lines l c _ _) -> "<interactive>:" ++ show (succ l) ++ ":" ++ show c
      _ -> "<interactive>:0:0"



class AsString a where asString :: a -> String
instance AsString String where asString = id

instance Default Info where def = Info Nothing

simpleISO8601 :: String
simpleISO8601 = "%Y-%m-%dT%H:%M:%SZ"

newtype ModuleName = ModuleName String
    deriving (Eq,Ord,IsString,ToJSON,FromJSON,AsString,Hashable)
instance Show ModuleName where show (ModuleName s) = show s


data Name =
    QName { _nQual :: ModuleName, _nName :: String } |
    Name { _nName :: String }
         deriving (Eq,Ord,Generic)
instance Show Name where
    show (QName q n) = asString q ++ "." ++ n
    show (Name n) = n
instance ToJSON Name where toJSON = toJSON . show
instance Hashable Name


data Literal =
    LString { _lString :: !String } |
    LInteger { _lInteger :: !Integer } |
    LDecimal { _lDecimal :: !Decimal } |
    LBool { _lBool :: !Bool } |
    LTime { _lTime :: !UTCTime }
          deriving (Eq,Generic)
instance Serialize Literal
formatLTime :: UTCTime -> String
formatLTime = formatTime defaultTimeLocale simpleISO8601
{-# INLINE formatLTime #-}

decimalPrec :: Word8
decimalPrec = 8

decimalToScientific :: Decimal -> Scientific
decimalToScientific (Decimal p m) = scientific m (negate (fromIntegral p))

doubleToDecimal :: Double -> Decimal
doubleToDecimal = realFracToDecimal decimalPrec

decimalToRational :: Decimal -> Rational
decimalToRational (Decimal p m) = m % (10 ^ (fromIntegral p :: Integer))


instance Show Literal where
    show (LString s) = show s
    show (LInteger i) = show i
    show (LDecimal r) = show r
    show (LBool b) = map toLower $ show b
    show (LTime t) = show $ formatLTime t
instance ToJSON Literal where
    toJSON (LString s) = String (pack s)
    toJSON (LInteger i) = Number (scientific i 0)
    toJSON (LDecimal r) = toJSON (show r)
    toJSON (LBool b) = toJSON b
    toJSON (LTime t) = toJSON (formatLTime t)
    {-# INLINE toJSON #-}


newtype TypeName = TypeName String
  deriving (Eq,Ord,IsString,AsString,ToJSON,FromJSON)
instance Show TypeName where show (TypeName s) = show s

data Arg o = Arg {
  _aName :: String,
  _aType :: Type (TermType o),
  _aInfo :: Info
  } deriving (Eq,Ord,Functor,Foldable,Traversable)
instance Show o => Show (Arg o) where show (Arg n t _) = n ++ ":" ++ show t

traverseArg :: Applicative m => (a -> m b) -> Arg a -> m (Arg b)
traverseArg f (Arg a t i) = Arg a <$> traverse (traverseTermType f) t <*> pure i

data FunType o = FunType {
  _ftArgs :: [Arg o],
  _ftReturn :: Type (TermType o)
  } deriving (Eq,Ord,Functor,Foldable,Traversable)
instance Show o => Show (FunType o) where
  show (FunType as t) = "(" ++ unwords (map show as) ++ ") -> " ++ show t

traverseFunType :: Applicative m => (a -> m b) -> FunType a -> m (FunType b)
traverseFunType f (FunType as r) = FunType <$> traverse (traverseArg f) as <*> traverse (traverseTermType f) r

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
  TyKeySet |
  TyBinding -- ugly but no use for this yet
  deriving (Eq,Ord)

litToPrim :: Literal -> PrimType
litToPrim LString {} = TyString
litToPrim LInteger {} = TyInteger
litToPrim LDecimal {} = TyDecimal
litToPrim LBool {} = TyBool
litToPrim LTime {} = TyTime

tyInteger,tyDecimal,tyTime,tyBool,tyString,tyList,tyObject,tyValue,tyKeySet,tyTable :: String
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
  show TyInteger = tyInteger
  show TyDecimal = tyDecimal
  show TyTime = tyTime
  show TyBool = tyBool
  show TyString = tyString
  show TyValue = tyValue
  show TyKeySet = tyKeySet
  show TyBinding = "binding"

data SchemaType =
  TyTable |
  TyObject
  deriving (Eq,Ord)
instance Show SchemaType where
  show TyTable = tyTable
  show TyObject = tyObject

data Type o =
  TyAny |
  TySpec { _tySpec :: o } |
  TyVar { _tyId :: String, _tyConstraint :: [o] }
     deriving (Eq,Ord,Functor,Foldable,Traversable)
instance Show o => Show (Type o) where
  show TyAny = "*"
  show (TySpec o) = show o
  show (TyVar i []) = "<" ++ i ++ ">"
  show (TyVar i c) = "<" ++ i ++ "=(" ++ intercalate "|" (map show c) ++ ")>"
instance Default (Type o) where def = TyAny

isAnyTy :: Type n -> Bool
isAnyTy TyAny = True
isAnyTy _ = False

data TermType v =
  TyPrim PrimType |
  TyList { _ttListType :: Type (TermType v) } |
  TySchema { _ttSchema :: SchemaType, _ttSchemaType :: Type v } |
  TyFun { _ttFunType :: FunType v } |
  TyUser { _ttUser :: v } -- reserved for enums etc
    deriving (Eq,Ord,Functor,Foldable,Traversable)
instance (Show v) => Show (TermType v) where
  show (TyPrim t) = show t
  show (TyList t) | isAnyTy t = tyList
                  | otherwise = "[" ++ show t ++ "]"
  show (TySchema s t) | isAnyTy t = show s
                      | otherwise = case s of
                          TyTable -> "table:" ++ show t
                          TyObject -> "{" ++ show t ++ "}"
  show (TyFun f) = show f
  show (TyUser v) = show v

traverseTermType :: Applicative m => (a -> m b) -> TermType a -> m (TermType b)
traverseTermType _ (TyPrim t) = pure $ TyPrim t
traverseTermType f (TyList l) = TyList <$> traverse (traverseTermType f) l
traverseTermType f (TySchema s t) = TySchema s <$> traverse f t
traverseTermType f (TyFun ft) = TyFun <$> traverseFunType f ft
traverseTermType f t@TyUser {} = traverse f t

makeLenses ''Type
makeLenses ''FunType
makeLenses ''Arg
makeLenses ''TermType

instance Show o => PP.Pretty (Type o) where pretty = PP.string . show



data Exp = ELiteral { _eLiteral :: !Literal, _eInfo :: !Info }
           | ESymbol { _eSymbol :: !String, _eInfo :: !Info }
           | EAtom { _eAtom :: !String
                   , _eQualifier :: !(Maybe String)
                   , _eType :: !(Maybe (Type (TermType TypeName)))
                   , _eInfo :: !Info }
           | EList { _eList :: ![Exp], _eInfo :: !Info }
           | EObject { _eObject :: ![(Exp,Exp)], _eInfo :: !Info }
           | EBinding { _eBinding :: ![(Exp,Exp)], _eInfo :: !Info }
           deriving (Eq,Generic)
makePrisms ''Exp


maybeDelim :: Show a => String -> Maybe a -> String
maybeDelim d t = maybe "" ((d ++) . show) t


instance Show Exp where
    show (ELiteral i _) = show i
    show (ESymbol s _) = '\'':s
    show (EAtom a q t _) =  a ++ maybeDelim "."  q ++ maybeDelim ": " t
    show (EList ls _) = "(" ++ unwords (map show ls) ++ ")"
    show (EObject ps _) = "{ " ++ intercalate ", " (map (\(k,v) -> show k ++ ": " ++ show v) ps) ++ " }"
    show (EBinding ps _) = "{ " ++ intercalate ", " (map (\(k,v) -> show k ++ ":= " ++ show v) ps) ++ " }"

$(makeLenses ''Exp)




data PublicKey = PublicKey { _pubKey :: !BS.ByteString } deriving (Eq,Ord,Generic)
instance Serialize PublicKey
instance FromJSON PublicKey where
    parseJSON = withText "PublicKey" (return . PublicKey . encodeUtf8)
instance ToJSON PublicKey where
    toJSON = toJSON . decodeUtf8 . _pubKey
instance Show PublicKey where show (PublicKey s) = show (BS.toString s)

type KeySet = S.Set PublicKey

data PactKeySet = PactKeySet {
      _pksKeys :: ![PublicKey]
    , _pksPredFun :: !String
    } deriving (Eq,Generic)
instance Serialize PactKeySet
instance Show PactKeySet where show (PactKeySet ks f) = "PactKeySet " ++ show ks ++ " " ++ show f
instance FromJSON PactKeySet where
    parseJSON = withObject "PactKeySet" $ \o ->
                PactKeySet <$> o .: "keys" <*> o .: "pred"
instance ToJSON PactKeySet where
    toJSON (PactKeySet k f) = object ["keys" .= k, "pred" .= f]


data RunType = Run | RunTx deriving (Eq,Show)

newtype KeySetName = KeySetName String
    deriving (Eq,Ord,IsString,AsString,ToJSON,FromJSON)
instance Show KeySetName where show (KeySetName s) = show s


data DefType = Defun | Defpact deriving (Eq,Show)
defTypeRep :: DefType -> String
defTypeRep Defun = "defun"
defTypeRep Defpact = "defpact"

newtype NativeDefName = NativeDefName String
    deriving (Eq,Ord,IsString,ToJSON,AsString)
instance Show NativeDefName where show (NativeDefName s) = show s

data FunApp = FunApp {
      _faInfo :: !Info
    , _faName :: !String
    , _faModule :: !(Maybe ModuleName)
    , _faDefType :: !DefType
    , _faTypes :: !(FunTypes (Term Name))
    , _faDocs :: !(Maybe String)
    }

instance Show FunApp where
  show FunApp {..} =
    "(" ++ defTypeRep _faDefType ++ " " ++ maybeDelim "." _faModule ++
    _faName ++ " " ++ showFunTypes _faTypes ++ ")"



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

data BindCtx = BindLet | BindKV deriving (Eq,Show)


newtype TableName = TableName String
    deriving (Eq,Ord,IsString,ToTerm,AsString,Hashable)
instance Show TableName where show (TableName s) = show s

data Module = Module {
    _mName :: !ModuleName
  , _mKeySet :: !KeySetName
  , _mDocs :: !(Maybe String)
  , _mCode :: !Exp
  } deriving (Eq)
instance Show Module where
  show Module {..} =
    "(Module " ++ asString _mName ++ " '" ++ asString _mKeySet ++ maybeDelim " " _mDocs ++ ")"
instance ToJSON Module where
  toJSON Module {..} = object $
    ["name" .= _mName, "keyset" .= _mKeySet, "code" .= show _mCode ]
    ++ maybe [] (return . ("docs" .=)) _mDocs
instance FromJSON Module where
  parseJSON = withObject "Module" $ \o -> Module <$>
    o .: "name" <*> o .: "keyset" <*> o .:? "docs" <*> pure (ELiteral (LString "Code persist TODO") def)


data Term n =
    TModule {
      _tModuleDef :: Module
    , _tModuleBody :: !(Scope () Term n)
    , _tInfo :: !Info
    } |
    TList {
      _tList :: ![Term n]
    , _tListType :: Type (TermType (Term n))
    , _tInfo :: !Info
    } |
    TDef {
      _tDefName :: !String
    , _tModule :: !ModuleName
    , _tDefType :: !DefType
    , _tFunType :: !(FunType (Term n))
    , _tDefBody :: !(Scope Int Term n)
    , _tDocs :: !(Maybe String)
    , _tInfo :: !Info
    } |
    TNative {
      _tNativeName :: !NativeDefName
    , _tNativeFun :: !NativeDFun
    , _tFunTypes :: FunTypes (Term n)
    , _tNativeDocs :: String
    , _tInfo :: !Info
    } |
    TConst {
      _tConstArg :: !(Arg (Term n))
    , _tModule :: !ModuleName
    , _tConstVal :: !(Term n)
    , _tDocs :: !(Maybe String)
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
    , _tBindCtx :: BindCtx
    , _tInfo :: !Info
    } |
    TObject {
      _tObject :: ![(Term n,Term n)]
    , _tUserType :: !(Type (Term n))
    , _tInfo :: !Info
    } |
    TUserType {
      _tUserTypeName :: !TypeName
    , _tModule :: !ModuleName
    , _tDocs :: !(Maybe String)
    , _tFields :: ![Arg (Term n)]
    , _tInfo :: !Info
    } |
    TLiteral {
      _tLiteral :: !Literal
    , _tInfo :: !Info
    } |
    TKeySet {
      _tKeySet :: !PactKeySet
    , _tInfo :: !Info
    } |
    TUse {
      _tModuleName :: !ModuleName
    , _tInfo :: !Info
    } |
    TValue {
      _tValue :: !Value
    , _tInfo :: !Info
    } |
    TStep {
      _tStepEntity :: !(Term n)
    , _tStepExec :: !(Term n)
    , _tStepRollback :: !(Maybe (Term n))
    , _tInfo :: !Info
    } |
    TTable {
      _tTableName :: !TableName
    , _tModule :: ModuleName
    , _tTableType :: !(Type (Term n))
    , _tDocs :: !(Maybe String)
    , _tInfo :: !Info
    }
    deriving (Functor,Foldable,Traversable,Eq)

instance Show n => Show (Term n) where
    show TModule {..} =
      "(TModule " ++ show _tModuleDef ++ " " ++ show _tModuleBody ++ ")"
    show (TList bs t _) = "[" ++ unwords (map show bs) ++ "]:" ++ show t
    show TDef {..} =
      "(TDef " ++ defTypeRep _tDefType ++ " " ++ asString _tModule ++ "." ++ _tDefName ++ " " ++
      show _tFunType ++ maybeDelim " " _tDocs ++ ")"
    show TNative {..} =
      "(TNative " ++ asString _tNativeName ++ " " ++ showFunTypes _tFunTypes ++ " " ++ _tNativeDocs ++ ")"
    show TConst {..} =
      "(TConst " ++ asString _tModule ++ "." ++ show _tConstArg ++ maybeDelim " " _tDocs ++ ")"
    show (TApp f as _) = "(TApp " ++ show f ++ " " ++ show as ++ ")"
    show (TVar n _) = "(TVar " ++ show n ++ ")"
    show (TBinding bs b c _) = "(TBinding " ++ show bs ++ " " ++ show b ++ " " ++ show c ++ ")"
    show (TObject bs ot _) =
      "{" ++ intercalate ", " (map (\(a,b) -> show a ++ ": " ++ show b) bs) ++ "}:" ++ show ot
    show (TLiteral l _) = show l
    show (TKeySet k _) = show k
    show (TUse m _) = "(TUse " ++ show m ++ ")"
    show (TValue v _) = BSL.toString $ encode v
    show (TStep ent e r _) =
      "(TStep " ++ show ent ++ " " ++ show e ++ maybeDelim " " r ++ ")"
    show TUserType {..} =
      "(TUserType " ++ asString _tModule ++ "." ++ asString _tUserTypeName ++ " " ++
      show _tFields ++ maybeDelim " " _tDocs ++ ")"
    show TTable {..} =
      "(TTable " ++ asString _tModule ++ "." ++ asString _tTableName ++ ":" ++ show _tTableType
      ++ maybeDelim " " _tDocs ++ ")"


instance Show1 Term
instance Eq1 Term

instance Applicative Term where
    pure = return
    (<*>) = ap

instance Monad Term where
    return a = TVar a def
    TModule m b i >>= f = TModule m (b >>>= f) i
    TList bs t i >>= f = TList (map (>>= f) bs) (fmap (fmap (>>= f)) t) i
    TDef n m dt ft b d i >>= f = TDef n m dt (fmap (>>= f) ft) (b >>>= f) d i
    TNative n fn t d i >>= f = TNative n fn (fmap (fmap (>>= f)) t) d i
    TConst d m c t i >>= f = TConst (fmap (>>= f) d) m (c >>= f) t i
    TApp af as i >>= f = TApp (af >>= f) (map (>>= f) as) i
    TVar n i >>= f = (f n) { _tInfo = i }
    TBinding bs b c i >>= f = TBinding (map (fmap (>>= f) *** (>>= f)) bs) (b >>>= f) c i
    TObject bs t i >>= f = TObject (map ((>>= f) *** (>>= f)) bs) (fmap (>>= f) t) i
    TLiteral l i >>= _ = TLiteral l i
    TKeySet k i >>= _ = TKeySet k i
    TUse m i >>= _ = TUse m i
    TValue v i >>= _ = TValue v i
    TStep ent e r i >>= f = TStep (ent >>= f) (e >>= f) (fmap (>>= f) r) i
    TUserType {..} >>= f = TUserType _tUserTypeName _tModule _tDocs (fmap (fmap (>>= f)) _tFields) _tInfo
    TTable {..} >>= f = TTable _tTableName _tModule (fmap (>>= f) _tTableType) _tDocs _tInfo


instance FromJSON (Term n) where
    parseJSON (Number n) = return $ TLiteral (LInteger (round n)) def
    parseJSON (Bool b) = return $ toTerm b
    parseJSON (String s) = return $ toTerm (unpack s)
    parseJSON v = return $ toTerm v
    {-# INLINE parseJSON #-}

instance Show n => ToJSON (Term n) where
    toJSON (TLiteral l _) = toJSON l
    toJSON (TValue v _) = v
    toJSON (TKeySet k _) = toJSON k
    toJSON (TObject kvs _ _) =
        object $ map (kToJSON *** toJSON) kvs
            where kToJSON (TLitString s) = pack s
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
instance ToTerm String where toTerm = tLit . LString
instance ToTerm PactKeySet where toTerm = (`TKeySet` def)
instance ToTerm Literal where toTerm = tLit
instance ToTerm Value where toTerm = (`TValue` def)
instance ToTerm UTCTime where toTerm = tLit . LTime

toTermList :: (ToTerm a,Foldable f) => Type (TermType (Term b)) -> f a -> Term b
toTermList ty l = TList (map toTerm (toList l)) ty def




typeof :: Term a -> Either String (TermType (Term a))
typeof t = case t of
      TLiteral l _ -> Right $ TyPrim $ litToPrim l
      TModule {} -> Left "module"
      TList {..} -> Right $ TyList _tListType
      TDef {..} -> Left $ defTypeRep _tDefType
      TNative {..} -> Left "defun"
      TConst {..} -> Left $ "const:" ++ _aName _tConstArg
      TApp {..} -> Left "app"
      TVar {..} -> Left "var"
      TBinding {} -> Right $ TyPrim TyBinding
      TObject {..} -> Right $ TySchema TyObject _tUserType
      TKeySet {} -> Right $ TyPrim TyKeySet
      TUse {} -> Left "use"
      TValue {} -> Right $ TyPrim TyValue
      TStep {} -> Left "step"
      TUserType {..} -> Left $ "defobject:" ++ asString _tUserTypeName
      TTable {..} -> Right $ TySchema TyTable _tTableType



pattern TLitString s <- TLiteral (LString s) _
pattern TLitInteger i <- TLiteral (LInteger i) _

tLit :: Literal -> Term n
tLit = (`TLiteral` def)
{-# INLINE tLit #-}

-- | Convenience for OverloadedStrings annoyances
tStr :: String -> Term n
tStr = toTerm

-- | Support pact `=`, only for list, object, literal, keyset
termEq :: Eq n => Term n -> Term n -> Bool
termEq (TList a _ _) (TList b _ _) = length a == length b && and (zipWith termEq a b)
termEq (TObject a _ _) (TObject b _ _) = length a == length b && all (lkpEq b) a
    where lkpEq [] _ = False
          lkpEq ((k',v'):ts) p@(k,v) | termEq k k' && termEq v v' = True
                                     | otherwise = lkpEq ts p
termEq (TLiteral a _) (TLiteral b _) = a == b
termEq (TKeySet a _) (TKeySet b _) = a == b
termEq _ _ = False




abbrev :: Show t => Term t -> String
abbrev t@TModule {} = "<module " ++ show (_tModuleName t) ++ ">"
abbrev (TList bs _ _) = concatMap abbrev bs
abbrev TDef {..} = "<defun " ++ _tDefName ++ ">"
abbrev TNative {..} = "<native " ++ asString _tNativeName ++ ">"
abbrev TConst {..} = "<defconst " ++ show _tConstArg ++ ">"
abbrev t@TApp {} = "<app " ++ abbrev (_tAppFun t) ++ ">"
abbrev TBinding {} = "<binding>"
abbrev TObject {} = "<object>"
abbrev (TLiteral l _) = show l
abbrev TKeySet {} = "<keyset>"
abbrev (TUse m _) = "<use '" ++ show m ++ ">"
abbrev (TVar s _) = show s
abbrev (TValue v _) = show v
abbrev TStep {} = "<step>"
abbrev TUserType {..} = "<defobject " ++ asString _tUserTypeName ++ ">"
abbrev TTable {..} = "<deftable " ++ asString _tTableName ++ ">"




makeLenses ''Term
makeLenses ''FunApp

data SyntaxError = SyntaxError Info String
instance Show SyntaxError where show (SyntaxError i s) = show i ++ ": Syntax error: " ++ s

data PactError =
    EvalError Info String |
    ArgsError FunApp [Term Name] String |
    TxFailure String

instance Show PactError where
    show (EvalError i s) = show i ++ ": " ++ s
    show (ArgsError FunApp {..} args s) =
        show _faInfo ++ ": " ++ s ++ ", received [" ++ intercalate "," (map abbrev args) ++ "] for " ++ showFunTypes _faTypes
    show (TxFailure s) = "Failure: " ++ s

evalError :: MonadError PactError m => Info -> String -> m a
evalError i = throwError . EvalError i

evalError' :: MonadError PactError m => FunApp -> String -> m a
evalError' = evalError . _faInfo

failTx :: MonadError PactError m => String -> m a
failTx = throwError . TxFailure


argsError :: (MonadError PactError m) => FunApp -> [Term Name] -> m a
argsError i as = throwError $ ArgsError i as "Invalid arguments"

argsError' :: (MonadError PactError m) => FunApp -> [Term Ref] -> m a
argsError' i as = throwError $ ArgsError i (map (toTerm.abbrev) as) "Invalid arguments"


data Persistable =
    PLiteral Literal |
    PKeySet PactKeySet |
    PValue Value
    deriving (Eq,Generic)
instance Serialize Persistable
instance Show Persistable where
    show (PLiteral l) = show l
    show (PKeySet k) = show k
    show (PValue v) = BSL.toString $ encode v
instance ToTerm Persistable where
    toTerm (PLiteral l) = toTerm l
    toTerm (PKeySet k) = toTerm k
    toTerm (PValue v) = toTerm v
instance ToJSON Persistable where
    toJSON (PLiteral (LString s)) = String (pack s)
    toJSON (PLiteral (LBool b)) = Bool b
    toJSON (PLiteral (LInteger n)) = Number (fromIntegral n)
    toJSON (PLiteral (LDecimal (Decimal dp dm))) =
        Array (V.fromList [Number (fromIntegral dp),Number (fromIntegral dm)])
    toJSON (PLiteral (LTime t)) =
        let (UTCTime (ModifiedJulianDay d) s) = unUTCTime t
        in Array (V.fromList ["t",Number (fromIntegral d),Number (fromIntegral (toMicroseconds s))])
    toJSON (PKeySet k) = toJSON k
    toJSON (PValue v) = Array (V.fromList ["v",v])
instance FromJSON Persistable where
    parseJSON (String s) = return (PLiteral (LString (unpack s)))
    parseJSON (Number n) = return (PLiteral (LInteger (round n)))
    parseJSON (Bool b) = return (PLiteral (LBool b))
    parseJSON v@(Object _) = PKeySet <$> parseJSON v
    parseJSON Null = return (PValue Null)
    parseJSON va@(Array a) =
        case V.toList a of
          [Number dp,Number dm] -> return (PLiteral (LDecimal (Decimal (truncate dp) (truncate dm))))
          [String typ,Number d,Number s] | typ == "t" -> return $ PLiteral $ LTime $ mkUTCTime
                                                         (ModifiedJulianDay (truncate d))
                                                         (fromMicroseconds (truncate s))
          [String typ,v] | typ == "v" -> return (PValue v)
          _ -> return (PValue va)


newtype ColumnId = ColumnId String
    deriving (Eq,Ord,IsString,ToTerm,AsString,ToJSON,FromJSON,Default)
instance Show ColumnId where show (ColumnId s) = show s

type PactObject = [(ColumnId,Persistable)]

newtype RowKey = RowKey String
    deriving (Eq,Ord,IsString,ToTerm,AsString)
instance Show RowKey where show (RowKey s) = show s


newtype Columns v = Columns { _columns :: M.Map ColumnId v }
    deriving (Eq,Show,Generic)
instance (ToJSON v) => ToJSON (Columns v) where
    toJSON (Columns m) = object . map (\(k,v) -> pack (asString k) .= toJSON v) . M.toList $ m
instance (FromJSON v) => FromJSON (Columns v) where
    parseJSON = withObject "Columns" $ \o ->
                (Columns . M.fromList) <$>
                 forM (HM.toList o)
                  (\(k,v) -> ((,) <$> pure (ColumnId (unpack k)) <*> parseJSON v))

makeLenses ''Columns

data Domain k v where
    UserTables :: !TableName -> Domain RowKey (Columns Persistable)
    KeySets :: Domain KeySetName PactKeySet
    Modules :: Domain ModuleName Module
deriving instance Eq (Domain k v)
deriving instance Show (Domain k v)
instance AsString (Domain k v) where
    asString (UserTables t) = asString t
    asString KeySets = "SYS:KeySets"
    asString Modules = "SYS:Modules"

data TxLog =
    TxLog {
      _txDomain :: !String
    , _txKey :: !String
    , _txValue :: !Value
    } deriving (Eq,Show)
makeLenses ''TxLog

instance ToJSON TxLog where
    toJSON (TxLog d k v) =
        object ["table" .= d, "key" .= k, "value" .= v]
instance FromJSON TxLog where
    parseJSON = withObject "TxLog" $ \o ->
                TxLog <$> o .: "table" <*> o .: "key" <*> o .: "value"

data WriteType = Insert|Update|Write deriving (Eq,Show)

type Method e a = MVar e -> IO a

data PactDb e = PactDb {
      _readRow :: forall k v . (IsString k,FromJSON v) =>
                  Domain k v -> k -> Method e (Maybe v)
    , _writeRow :: forall k v . (AsString k,ToJSON v) =>
                   WriteType -> Domain k v -> k -> v -> Method e ()
    , _keys ::  TableName -> Method e [RowKey]
    , _txids ::  TableName -> TxId -> Method e [TxId]
    , _createUserTable ::  TableName -> ModuleName -> KeySetName -> Method e ()
    , _getUserTableInfo ::  TableName -> Method e (ModuleName,KeySetName)
    , _beginTx :: Method e ()
    , _commitTx ::  TxId -> Method e ()
    , _rollbackTx :: Method e ()
    , _getTxLog :: forall k v . (IsString k,FromJSON v) =>
                   Domain k v -> TxId -> Method e [TxLog]
}



newtype TxId = TxId Word64
    deriving (Eq,Ord,Enum,Num,Real,Integral,Bounded,Default,FromJSON,ToJSON)
instance Show TxId where show (TxId s) = show s
instance ToTerm TxId where toTerm = tLit . LInteger . fromIntegral

data PactStep = PactStep {
      _psStep :: !Int
    , _psRollback :: !Bool
    , _psTxId :: !TxId
} deriving (Eq,Show)

type ModuleData = (Module,HM.HashMap String Ref)

data RefStore = RefStore {
      _rsNatives :: HM.HashMap String Ref
    , _rsModules :: HM.HashMap ModuleName ModuleData
    } deriving (Eq,Show)
makeLenses ''RefStore
instance Default RefStore where def = RefStore HM.empty HM.empty

data EvalEnv e = EvalEnv {
      _eeRefStore :: !RefStore
    , _eeMsgSigs :: !KeySet
    , _eeMsgBody :: !Value
    , _eeTxId :: !TxId
    , _eeEntity :: !String
    , _eePactStep :: !(Maybe PactStep)
    , _eePactDbVar :: MVar e
    , _eePactDb :: PactDb e
    } -- deriving (Eq,Show)
makeLenses ''EvalEnv

data StackFrame = StackFrame {
      _sfName :: !String
    , _sfLoc :: !Info
    , _sfApp :: Maybe (FunApp,[String])
    }
instance Show StackFrame where
    show (StackFrame n i a) = renderInfo i ++ ": " ++ n ++ f a
        where f Nothing = ""
              f (Just (dd,as)) = ", " ++ show dd ++ ", values=" ++ show as
makeLenses ''StackFrame


data PactYield = PactYield {
      _pyNextStep :: !(Maybe Int)
    , _pyFailStep :: !(Maybe Int)
    } deriving (Eq,Show)
instance Default PactYield where def = PactYield def def


data RefState = RefState {
      _rsLoaded :: HM.HashMap Name Ref
    , _rsLoadedModules :: HM.HashMap ModuleName Module
    , _rsNew :: [(ModuleName,ModuleData)]
    }
                deriving (Eq,Show)
makeLenses ''RefState
instance Default RefState where def = RefState HM.empty HM.empty def

data EvalState = EvalState {
      _evalRefs :: !RefState
    , _evalCallStack :: ![StackFrame]
    , _evalYield :: !(Maybe PactYield)
    }
makeLenses ''EvalState
instance Show EvalState where
    show (EvalState m y _) = "EvalState " ++ show m ++ " " ++ show y
instance Default EvalState where def = EvalState def def def

newtype Eval e a =
    Eval { unEval :: ExceptT PactError (ReaderT (EvalEnv e) (StateT EvalState IO)) a }
    deriving (Functor,Applicative,Monad,MonadError PactError,MonadState EvalState,
                     MonadReader (EvalEnv e),MonadThrow,MonadCatch,MonadIO)

{-# INLINE runEval #-}
runEval :: EvalState -> EvalEnv e -> Eval e a ->
           IO (Either PactError a,EvalState)
runEval s env act = runStateT (runReaderT (runExceptT (unEval act)) env) s


call :: StackFrame -> Eval e a -> Eval e a
call s act = do
  evalCallStack %= (s:)
  r <- act
  evalCallStack %= \st -> case st of (_:as) -> as; [] -> []
  return r
{-# INLINE call #-}

method :: (PactDb e -> Method e a) -> Eval e a
method f = do
  EvalEnv {..} <- ask
  liftIO $ f _eePactDb _eePactDbVar


readRow :: (IsString k,FromJSON v) => Domain k v -> k -> Eval e (Maybe v)
readRow d k = method $ \db -> _readRow db d k

writeRow :: (AsString k,ToJSON v) => WriteType -> Domain k v -> k -> v -> Eval e ()
writeRow w d k v = method $ \db -> _writeRow db w d k v

keys :: TableName -> Eval e [RowKey]
keys t = method $ \db -> _keys db t

txids :: TableName -> TxId -> Eval e [TxId]
txids tn tid = method $ \db -> _txids db tn tid

createUserTable :: TableName -> ModuleName -> KeySetName -> Eval e ()
createUserTable t m k = method $ \db -> _createUserTable db t m k

getUserTableInfo :: TableName -> Eval e (ModuleName,KeySetName)
getUserTableInfo t = method $ \db -> _getUserTableInfo db t

beginTx :: Eval e ()
beginTx = method $ \db -> _beginTx db

commitTx :: TxId -> Eval e ()
commitTx t = method $ \db -> _commitTx db t

rollbackTx :: Eval e ()
rollbackTx = method $ \db -> _rollbackTx db

getTxLog :: (IsString k,FromJSON v) => Domain k v -> TxId -> Eval e [TxLog]
getTxLog d t = method $ \db -> _getTxLog db d t

{-# INLINE readRow #-}
{-# INLINE writeRow #-}
{-# INLINE createUserTable #-}
{-# INLINE getUserTableInfo #-}
{-# INLINE commitTx #-}
{-# INLINE beginTx #-}
{-# INLINE rollbackTx #-}
{-# INLINE getTxLog #-}
{-# INLINE keys #-}
{-# INLINE txids #-}

-- | Pure version of 'modifyMVar_'
modifyMVar' :: MVar a -> (a -> a) -> IO ()
modifyMVar' mv f = modifyMVar_ mv (\ps -> return (f ps))
{-# INLINE modifyMVar' #-}

-- | Modify the target of a lens.
modifyingMVar :: MVar s -> Lens' s a -> (a -> IO a) -> IO ()
modifyingMVar mv l f = modifyMVar_ mv $ \ps -> (\b -> set l b ps) <$> f (view l ps)
{-# INLINE modifyingMVar #-}

-- | Lens view into mvar.
useMVar :: MVar s -> Getting a s a -> IO a
useMVar e l = view l <$> readMVar e
{-# INLINE useMVar #-}

newtype PactDbException = PactDbException String deriving (Eq,Show)
instance Exception PactDbException

throwDbError :: MonadThrow m => String -> m a
throwDbError s = throwM $ PactDbException s
