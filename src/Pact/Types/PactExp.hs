{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      :  Pact.Types.PactExp
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- PactExp, the output of the Pact parser, and Literal.
--

module Pact.Types.PactExp
 (
   Parsed(..),
   Code(..),
   Info(..),
   renderInfo,
   renderParsed,
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
   PactExp(..),eLiteral,eAtom,eBinding,eList,eLitListType,eObject,eParsed,eQualifier,eSymbol,eType,
   _ELiteral,_ESymbol,_EAtom,_EList,_EObject,_EBinding,
   pattern EList',pattern ELitList,pattern ELitString,pattern EAtom',
   IsLiteralList(..)
   ) where


import Control.Lens (makeLenses,makePrisms)
import Control.Applicative
import Data.List
import Control.Monad
import Prelude
import Data.Text (Text,pack,unpack)
import Data.Aeson
import Data.String
import Data.Default
import Data.Char
import Data.Thyme
import System.Locale
import Data.Scientific
import GHC.Generics (Generic)
import Data.Decimal
import Data.Hashable
import Text.PrettyPrint.ANSI.Leijen hiding ((<>),(<$>))
import Control.DeepSeq
import qualified Data.Attoparsec.Text as AP
import Text.Trifecta (try,ident,TokenParsing,(<?>))
import Data.Serialize (Serialize)

import Pact.Types.Util
import Pact.Types.Parser (style, qualified)
import Pact.Types.Info
import Pact.Types.Type


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
instance FromJSON Name where
  parseJSON = withText "Name" $ \t -> case AP.parseOnly (parseName def) t of
    Left s -> fail s
    Right n -> return n

parseName :: (TokenParsing m, Monad m) => Info -> m Name
parseName i = do
  a <- ident style
  try (qualified >>= \qn -> return (QName (ModuleName a) qn i) <?> "qualified name") <|>
    return (Name a i)


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


litToPrim :: Literal -> PrimType
litToPrim LString {} = TyString
litToPrim LInteger {} = TyInteger
litToPrim LDecimal {} = TyDecimal
litToPrim LBool {} = TyBool
litToPrim LTime {} = TyTime

data IsLiteralList
  = IsLiteralList
  | IsntLiteralList
  deriving (Eq, Generic)

instance NFData IsLiteralList

-- | Pact expressions, with parsing info.
data PactExp =
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
  EList { _eList :: ![PactExp], _eLitListType :: !IsLiteralList, _eParsed :: !Parsed } |
  -- | Object literals.
  EObject { _eObject :: ![(PactExp,PactExp)], _eParsed :: !Parsed } |
  -- | Special binding forms.
  EBinding { _eBinding :: ![(PactExp,PactExp)], _eParsed :: !Parsed }
           deriving (Eq,Generic)

instance NFData PactExp


pattern EList' :: [PactExp] -> PactExp
pattern EList' ls <- EList ls IsntLiteralList _
pattern ELitList :: [PactExp] -> PactExp
pattern ELitList ls <- EList ls IsLiteralList _
pattern EAtom' :: Text -> PactExp
pattern EAtom' tag <- EAtom tag Nothing Nothing _
pattern ELitString :: Text -> PactExp
pattern ELitString s <- ELiteral (LString s) _

makePrisms ''PactExp




instance Show PactExp where
    show (ELiteral i _) = show i
    show (ESymbol s _) = '\'':unpack s
    show (EAtom a q t _) =  unpack a ++ maybeDelim "."  q ++ maybeDelim ": " t
    show (EList ls IsntLiteralList _) = "(" ++ unwords (map show ls) ++ ")"
    show (EList ls IsLiteralList _) = "[" ++ unwords (map show ls) ++ "]"
    show (EObject ps _) = "{ " ++ intercalate ", " (map (\(k,v) -> show k ++ ": " ++ show v) ps) ++ " }"
    show (EBinding ps _) = "{ " ++ intercalate ", " (map (\(k,v) -> show k ++ ":= " ++ show v) ps) ++ " }"

$(makeLenses ''PactExp)
