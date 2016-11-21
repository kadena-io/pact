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
import Control.Concurrent.MVar


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

data FunArg = FunArg {
  _faName :: String,
  _faType :: Type
  } deriving (Eq,Show,Ord)


data FunType = FunType {
  _ftArgs :: [FunArg],
  _ftReturn :: Type
  } deriving (Eq,Show,Ord)

data Type =
    TyInteger |
    TyDecimal |
    TyTime |
    TyBool |
    TyString |
    TyList { _tlType :: Maybe Type } |
    TyObject { _toType :: Maybe TypeName } |
    TyValue |
    TyKeySet |
    TyFun { _tfType :: [FunType] }
    deriving (Eq,Ord)

tyInteger,tyDecimal,tyTime,tyBool,tyString,tyList,tyObject,tyValue,tyKeySet :: String
tyInteger = "integer"
tyDecimal = "decimal"
tyTime = "time"
tyBool = "bool"
tyString = "string"
tyList = "list"
tyObject = "object"
tyValue = "value"
tyKeySet = "keyset"

instance Show Type where
  show TyInteger = tyInteger
  show TyDecimal = tyDecimal
  show TyTime = tyTime
  show TyBool = tyBool
  show TyString = tyString
  show TyList {..} = maybe tyList (\t -> "[" ++ show t ++ "]") _tlType
  show TyObject {..} = maybe tyObject (\t -> "{" ++ asString t ++ "}") _toType
  show TyValue = tyValue
  show TyKeySet = tyKeySet
  show TyFun {..} = if null _tfType then "function" else "function: " ++ show _tfType




data Exp = ELiteral { _eLiteral :: !Literal, _eInfo :: !Info }
           | ESymbol { _eSymbol :: !String, _eInfo :: !Info }
           | EAtom { _eAtom :: !String
                   , _eQualifier :: !(Maybe String)
                   , _eType :: !(Maybe Type)
                   , _eInfo :: !Info }
           | EList { _eList :: ![Exp], _eInfo :: !Info }
           | EObject { _eObject :: ![(Exp,Exp)], _eInfo :: !Info }
           | EBinding { _eBinding :: ![(Exp,Exp)], _eInfo :: !Info }
           deriving (Eq)
makePrisms ''Exp

instance Show Exp where
    show (ELiteral i _) = show i
    show (ESymbol s _) = '\'':s
    show (EAtom a q t _) =  a ++ maybe "" ("." ++) q ++ maybe "" ((": " ++) . show) t
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


data DefType = Defun | Defpact | Defconst deriving (Eq,Show)
defTypeRep :: DefType -> String
defTypeRep Defun = "defun"
defTypeRep Defpact = "defpact"
defTypeRep Defconst = "defconst"

data DefData = DefData {
      _dName :: !String
    , _dType :: !DefType
    , _dModule :: !(Maybe ModuleName)
    , _dArgs :: ![String]
    , _dDocs :: !(Maybe String)
} deriving (Eq)

instance Show DefData where
    show (DefData n Defconst _ _ d) =
        "(defconst " ++ n ++ maybe "" ((" " ++) . show) d ++ ")"
    show (DefData n t _ as d) =
              "(" ++ defTypeRep t ++ " " ++ n ++
              " (" ++ intercalate "," as ++ ")" ++ maybe "" ((" " ++) . show) d ++ ")"

defName :: DefData -> String
defName (DefData n _ (Just mn) _ _) = asString mn ++ "." ++ n
defName (DefData n _ _ _ _) = n

newtype NativeDefName = NativeDefName String
    deriving (Eq,Ord,IsString,ToJSON,AsString)
instance Show NativeDefName where show (NativeDefName s) = show s

data FunApp = FunApp {
      _faInfo :: !Info
    , _faDef :: !DefData
    }

funAppInfo :: FunApp -> Info
funAppInfo = _faInfo


data Ref = Direct (Term Name) |
           Ref (Term Ref)
               deriving (Eq)
instance Show Ref where
    show (Direct t) = abbrev t
    show (Ref t) = abbrev t

data NativeDFun = NativeDFun {
      _nativeDefName :: NativeDefName,
      _nativeDef :: forall m . Monad m => FunApp -> [Term Ref] -> m (Term Name)
    }
instance Eq NativeDFun where a == b = _nativeDefName a == _nativeDefName b
instance Show NativeDFun where show a = show $ _nativeDefName a

data BindCtx = BindLet | BindKV deriving (Eq,Show)


data Term n =
    TModule {
      _tModuleName :: !ModuleName
    , _tModuleKeySet :: !KeySetName
    , _tModuleDocs :: !(Maybe String)
    , _tModuleBody :: !(Scope () Term n)
    , _tCode :: Exp
    , _tInfo :: !Info
    } |
    TList {
      _tList :: ![Term n]
    , _tType :: Maybe Type
    , _tInfo :: !Info
    } |
    TDef {
      _tDefData :: !DefData
    , _tDefBody :: !(Scope Int Term n)
    , _tDefExp :: Exp
    , _tFunType :: [FunType]
    , _tInfo :: !Info
    } |
    TNative {
      _tDefData :: !DefData
    , _tDefNative :: !NativeDFun
    , _tFunType :: [FunType]
    , _tInfo :: !Info
    } |
    TConst {
      _tDefData :: !DefData
    , _tDefConst :: !(Term n)
    , _tType :: Maybe Type
    , _tInfo :: !Info
    } |
    TApp {
      _tAppFun :: !(Term n)
    , _tAppArgs :: ![Term n]
    , _tInfo :: !Info
    } |
    TVar {
      _tVar :: !n
    , _tType :: Maybe Type
    , _tInfo :: !Info
    } |
    TBinding {
      _tBindPairs :: ![(String,Term n)]
    , _tBindBody :: !(Scope Int Term n)
    , _tBindCtx :: BindCtx
    , _tInfo :: !Info
    } |
    TObject {
      _tObject :: ![(Term n,Term n)]
    , _tUserType :: Maybe TypeName
    , _tInfo :: !Info
    } |
    TLiteral {
      _tLiteral :: !Literal
    , _tInfo :: !Info
    } |
    TKeySet { _tKeySet :: !PactKeySet, _tInfo :: !Info } |
    TUse { _tModuleName :: !ModuleName, _tInfo :: !Info } |
    TValue { _tValue :: !Value, _tInfo :: !Info } |
    TStep {
      _tStepEntity :: !(Term n)
    , _tStepExec :: !(Term n)
    , _tStepRollback :: !(Maybe (Term n))
    , _tInfo :: !Info
    }
    deriving (Functor,Foldable,Traversable,Eq)


instance Show n => Show (Term n) where
    show (TModule n k d b _ _) =
        "(TModule " ++ show n ++ " " ++ show k ++ " " ++ show d ++ " " ++ show b ++ ")"
    show (TList bs t _) = "[" ++ unwords (map show bs) ++ "]" ++ maybe "" ((":" ++) . show) t
    show (TDef di _ _ ft _) = show di ++ intercalate "," (map ((":" ++) . show) ft )
    show (TNative di _ ft _) = show di ++ intercalate "," (map ((":" ++) . show) ft )
    show (TConst di _ t _) = show di ++ maybe "" ((":" ++) . show) t
    show (TApp f as _) = "(TApp " ++ show f ++ " " ++ show as ++ ")"
    show (TVar n t _) = "(TVar " ++ show n ++ maybe "" ((":" ++) . show) t ++ ")"
    show (TBinding bs b c _) = "(TBinding " ++ show bs ++ " " ++ show b ++ " " ++ show c ++ ")"
    show (TObject bs ot _) = "{" ++ intercalate ", " (map (\(a,b) -> show a ++ ": " ++ show b) bs) ++ "}" ++
                             maybe "" ((":" ++) . show) ot ++ ")"
    show (TLiteral l _) = show l
    show (TKeySet k _) = show k
    show (TUse m _) = "(TUse " ++ show m ++ ")"
    show (TValue v _) = BSL.toString $ encode v
    show (TStep ent e r _) =
        "(TStep " ++ show ent ++ " " ++ show e ++ maybe "" ((" " ++) . show) r ++ ")"


instance Show1 Term
instance Eq1 Term

instance Applicative Term where
    pure = return
    (<*>) = ap

instance Monad Term where
    return a = TVar a def def
    TModule n k ds b c i >>= f = TModule n k ds (b >>>= f) c i
    TList bs t i >>= f = TList (map (>>= f) bs) t i
    TDef d b e t i >>= f = TDef d (b >>>= f) e t i
    TNative d n t i >>= _ = TNative d n t i
    TConst d c t i >>= f = TConst d (c >>= f) t i
    TApp af as i >>= f = TApp (af >>= f) (map (>>= f) as) i
    TVar n _ i >>= f = (f n) { _tInfo = i }
    TBinding bs b c i >>= f = TBinding (map (second (>>= f)) bs) (b >>>= f) c i
    TObject bs t i >>= f = TObject (map ((>>= f) *** (>>= f)) bs) t i
    TLiteral l i >>= _ = TLiteral l i
    TKeySet k i >>= _ = TKeySet k i
    TUse m i >>= _ = TUse m i
    TValue v i >>= _ = TValue v i
    TStep ent e r i >>= f = TStep (ent >>= f) (e >>= f) (fmap (>>= f) r) i


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

typeof :: Term a -> Either String Type
typeof t = case t of
      TLiteral l _ ->
          case l of
            LInteger {} -> Right TyInteger
            LDecimal {} -> Right TyDecimal
            LString {} -> Right TyString
            LBool {} -> Right TyBool
            LTime {} -> Right TyTime
      TModule {} -> Left "module"
      TList {..} -> Right $ TyList _tType
      TDef {..} -> Right $ TyFun _tFunType
      TNative {..} -> Right $ TyFun _tFunType
      TConst {..} -> maybe (Left "const") Right _tType
      TApp {..} -> Left "app"
      TVar {..} -> maybe (Left "var") Right _tType
      TBinding {} -> Left "binding"
      TObject {..} -> Right $ TyObject _tUserType
      TKeySet {} -> Right TyKeySet
      TUse {} -> Left "use"
      TValue {} -> Right TyValue
      TStep {} -> Left "step"





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
abbrev t@TDef {} = "<defun " ++ _dName (_tDefData t) ++ ">"
abbrev t@TNative {} = "<native " ++ _dName (_tDefData t) ++ ">"
abbrev t@TConst {} = "<defconst " ++ _dName (_tDefData t) ++ ">"
abbrev t@TApp {} = "<app " ++ abbrev (_tAppFun t) ++ ">"
abbrev TBinding {} = "<binding>"
abbrev TObject {} = "<object>"
abbrev (TLiteral l _) = show l
abbrev TKeySet {} = "<keyset>"
abbrev (TUse m _) = "<use '" ++ show m ++ ">"
abbrev (TVar s _ _) = show s
abbrev (TValue v _) = show v
abbrev TStep {} = "<step>"




makeLenses ''DefData
makeLenses ''Term


data PactError =
    EvalError Info String |
    ArgsError FunApp [Term Name] String |
    TxFailure String

instance Show PactError where
    show (EvalError i s) = show i ++ ": " ++ s
    show (ArgsError (FunApp i (DefData fn _ _ as _)) args s) =
        show i ++ ": " ++ show fn ++ ": " ++ s ++ ", expected [" ++ intercalate "," as ++ "]" ++
                 if null args then "" else ", received [" ++ intercalate "," (map abbrev args) ++ "]"
    show (TxFailure s) = "Failure: " ++ s

evalError :: MonadError PactError m => Info -> String -> m a
evalError i = throwError . EvalError i

evalError' :: MonadError PactError m => FunApp -> String -> m a
evalError' = evalError . funAppInfo

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

newtype TableName = TableName String
    deriving (Eq,Ord,IsString,ToTerm,AsString,Hashable)
instance Show TableName where show (TableName s) = show s
newtype RowKey = RowKey String
    deriving (Eq,Ord,IsString,ToTerm,AsString)
instance Show RowKey where show (RowKey s) = show s

data Module = Module {
      _modName :: !ModuleName
    , _modKeySet :: !KeySetName
    , _modCode :: !String
    } deriving (Eq)
instance Show Module where
    show (Module n ks ds) = "Module " ++ show n ++ " " ++ show ks ++ " " ++ show ds
instance ToJSON Module where
    toJSON (Module n k ds) =
        object [ "name" .= n, "keyset" .= k,
                 "code" .= toJSON ds ]
instance FromJSON Module where
    parseJSON = withObject "Module" $ \o ->
                Module <$> o .: "name" <*> o .: "keyset" <*> o .: "code"


makeLenses ''Module


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


data RefStore = RefStore {
      _rsNatives :: HM.HashMap String Ref
    , _rsModules :: HM.HashMap ModuleName (HM.HashMap String Ref)
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

maybeModuleName :: Term n -> Maybe ModuleName
maybeModuleName = firstOf (tDefData.dModule._Just)

data StackFrame = StackFrame {
      _sTermModule :: Maybe ModuleName
    , _sLoc :: !Info
    , _sName :: !String
    , _sArgs :: ![(String, String)]
    }
instance Show StackFrame where
    show (StackFrame _ i n as) = renderInfo i ++ ": " ++ n ++ args
        where args | null as = ""
                   | otherwise = " [" ++ intercalate "," (map (\(k,v) -> k ++ "=" ++ v) as) ++ "]"
makeLenses ''StackFrame


data PactYield = PactYield {
      _pyNextStep :: !(Maybe Int)
    , _pyFailStep :: !(Maybe Int)
    } deriving (Eq,Show)
instance Default PactYield where def = PactYield def def


data RefState = RefState {
      _rsLoaded :: HM.HashMap Name Ref
    , _rsNew :: [(ModuleName,HM.HashMap String Ref)]
    }
                deriving (Eq,Show)
makeLenses ''RefState
instance Default RefState where def = RefState HM.empty def

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
