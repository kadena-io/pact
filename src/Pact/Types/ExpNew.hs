{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      :  Pact.Types.Exp
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Exp, the output of the Pact parser, and Literal.
--

module Pact.Types.ExpNew
 (
   Parsed(..),
   Code(..),
   Info(..),
   renderInfo,
   renderParsed,
   ModuleName(..),
   Name(..),
   Literal(..),
   _LString,_LInteger,_LDecimal,_LBool,_LTime,
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
   LiteralExp(..),AtomExp(..),ListExp(..),SeparatorExp(..),
   Exp(..),ExpInfo(..),
   _ELiteral,_EAtom,_EList,_ESeparator,
   ListDelimiter(..),listDelims,enlist,
   Separator(..)
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
import Pact.Types.Parser
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

makePrisms ''Literal

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

data ListDelimiter = Parens|Brackets|Braces deriving (Eq,Show,Ord,Generic,Bounded,Enum)
instance NFData ListDelimiter

listDelims :: ListDelimiter -> (Text,Text)
listDelims Parens = ("(",")")
listDelims Brackets = ("[","]")
listDelims Braces = ("{","}")

enlist :: ListDelimiter -> ((Text,Text) -> a) -> a
enlist d f = f (listDelims d)

data Separator = Colon|ColonEquals|Comma deriving (Eq,Ord,Generic,Bounded,Enum)
instance NFData Separator
instance Show Separator where
  show Colon = ":"
  show ColonEquals = ":="
  show Comma = ","

class ExpInfo b a | a -> b where
  eInfo :: a -> b

instance ExpInfo Info Info where
  eInfo = id

data LiteralExp i = LiteralExp
  { _litLiteral :: !Literal
  , _litInfo :: !i
  } deriving (Eq,Ord,Generic,Functor,Foldable,Traversable)
instance Show (LiteralExp i) where show LiteralExp{..} = show _litLiteral
instance ExpInfo i (LiteralExp i) where
  eInfo = _litInfo
instance NFData i => NFData (LiteralExp i)

data AtomExp i = AtomExp
  { _atomAtom :: !Text
  , _atomQualifiers :: ![Text]
  , _atomInfo :: i
  } deriving (Eq,Ord,Generic,Functor,Foldable,Traversable)
instance Show (AtomExp i) where
  show AtomExp{..} = intercalate "." (map unpack $ _atomQualifiers ++ [_atomAtom])
instance ExpInfo i (AtomExp i) where
  eInfo = _atomInfo
instance NFData i => NFData (AtomExp i)

data ListExp i = ListExp
  { _listList :: ![(Exp i)]
  , _listDelimiter :: !ListDelimiter
  , _listInfo :: !i
  } deriving (Eq,Ord,Generic,Functor,Foldable,Traversable)
instance Show (ListExp i) where
  show ListExp{..} =
    enlist _listDelimiter $ \(o,c) ->
      unpack o ++ unwords (map show _listList) ++ unpack c
instance ExpInfo i (ListExp i) where
  eInfo = _listInfo
instance NFData i => NFData (ListExp i)

data SeparatorExp i = SeparatorExp
  { _sepSeparator :: !Separator
  , _sepInfo :: !i
  } deriving (Eq,Ord,Generic,Functor,Foldable,Traversable)
instance Show (SeparatorExp i) where show (SeparatorExp{..}) = show _sepSeparator
instance ExpInfo i (SeparatorExp i) where
  eInfo = _sepInfo
instance NFData i => NFData (SeparatorExp i)


-- | Pact syntax expressions
data Exp i =
  ELiteral (LiteralExp i) |
  EAtom (AtomExp i) |
  EList (ListExp i) |
  ESeparator (SeparatorExp i)
  deriving (Eq,Ord,Generic,Functor,Foldable,Traversable)

instance NFData i => NFData (Exp i)
instance ExpInfo i (Exp i) where
  eInfo e = case e of
    ELiteral i -> eInfo i
    EAtom a -> eInfo a
    EList l -> eInfo l
    ESeparator s -> eInfo s


makePrisms ''Exp


instance Show (Exp i) where
  show e = case e of
    ELiteral i -> show i
    EAtom a -> show a
    EList l -> show l
    ESeparator s -> show s
