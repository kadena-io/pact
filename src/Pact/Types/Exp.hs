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
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      :  Pact.Types.Exp
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Exp, the output of the Pact parser, and Literal.
--

module Pact.Types.Exp
 (
   Literal(..),
   _LString,_LInteger,_LDecimal,_LBool,_LTime,
   simpleISO8601,formatLTime,
   litToPrim,
   LiteralExp(..),AtomExp(..),ListExp(..),SeparatorExp(..),
   Exp(..),
   _ELiteral,_EAtom,_EList,_ESeparator,
   ListDelimiter(..),enlist,
   Separator(..),
   pattern CommaExp,
   pattern ColonExp
   ) where


import Control.Lens (makePrisms)
import Data.List
import Control.Monad
import Prelude
import Data.Text (Text,pack)
import Data.Aeson
import Data.Thyme
import System.Locale
import Data.Scientific
import GHC.Generics (Generic)
import Data.Decimal
import Control.DeepSeq
import Data.Serialize (Serialize)
import Data.String (IsString)

import Pact.Types.Info
import Pact.Types.Pretty
import Pact.Types.Type


data Literal =
    LString { _lString :: !Text } |
    LInteger { _lInteger :: !Integer } |
    LDecimal { _lDecimal :: !Decimal } |
    LBool { _lBool :: !Bool } |
    LTime { _lTime :: !UTCTime }
        deriving (Eq,Generic,Ord,Show)

instance Serialize Literal
instance NFData Literal

makePrisms ''Literal

-- | ISO8601 Thyme format
simpleISO8601 :: String
simpleISO8601 = "%Y-%m-%dT%H:%M:%SZ"

formatLTime :: UTCTime -> Text
formatLTime = pack . formatTime defaultTimeLocale simpleISO8601
{-# INLINE formatLTime #-}

instance Pretty Literal where
    pretty (LString s)   = dquotes $ pretty s
    pretty (LInteger i)  = pretty i
    pretty (LDecimal r)  = viaShow r
    pretty (LBool True)  = "true"
    pretty (LBool False) = "false"
    pretty (LTime t)     = dquotes $ pretty $ formatLTime t

instance ToJSON Literal where
    toJSON (LString s)  = String s
    toJSON (LInteger i) = Number (scientific i 0)
    toJSON (LDecimal r) = toJSON (show r)
    toJSON (LBool b)    = toJSON b
    toJSON (LTime t)    = toJSON (formatLTime t)
    {-# INLINE toJSON #-}


litToPrim :: Literal -> PrimType
litToPrim LString {} = TyString
litToPrim LInteger {} = TyInteger
litToPrim LDecimal {} = TyDecimal
litToPrim LBool {} = TyBool
litToPrim LTime {} = TyTime

data ListDelimiter = Parens|Brackets|Braces deriving (Eq,Show,Ord,Generic,Bounded,Enum)
instance NFData ListDelimiter

listDelims :: IsString a => ListDelimiter -> (a, a)
listDelims Parens   = ("(",")")
listDelims Brackets = ("[","]")
listDelims Braces   = ("{","}")

enlist :: ListDelimiter -> ((Text,Text) -> a) -> a
enlist d f = f (listDelims d)

data Separator = Colon|ColonEquals|Comma deriving (Eq,Ord,Generic,Bounded,Enum,Show)
instance NFData Separator
instance Pretty Separator where
  pretty Colon = ":"
  pretty ColonEquals = ":="
  pretty Comma = ","

data LiteralExp i = LiteralExp
  { _litLiteral :: !Literal
  , _litInfo :: !i
  } deriving (Eq,Ord,Generic,Functor,Foldable,Traversable,Show)
instance HasInfo (LiteralExp Info) where
  getInfo = _litInfo
instance NFData i => NFData (LiteralExp i)

instance Pretty (LiteralExp i) where
  pretty (LiteralExp l _) = pretty l

data AtomExp i = AtomExp
  { _atomAtom :: !Text
  , _atomQualifiers :: ![Text]
  , _atomInfo :: i
  } deriving (Eq,Ord,Generic,Functor,Foldable,Traversable,Show)
instance HasInfo (AtomExp Info) where
  getInfo = _atomInfo
instance NFData i => NFData (AtomExp i)

instance Pretty (AtomExp i) where
  pretty (AtomExp atom qs _)
    = mconcat $ punctuate dot $ fmap pretty $ qs ++ [atom]

data ListExp i = ListExp
  { _listList :: ![(Exp i)]
  , _listDelimiter :: !ListDelimiter
  , _listInfo :: !i
  } deriving (Eq,Ord,Generic,Functor,Foldable,Traversable,Show)
instance HasInfo (ListExp Info) where
  getInfo = _listInfo
instance NFData i => NFData (ListExp i)

instance Pretty (ListExp i) where
  pretty (ListExp exps delim _) =
    let (l, r) = listDelims delim
    in encloseSep l r space $ fmap pretty exps

data SeparatorExp i = SeparatorExp
  { _sepSeparator :: !Separator
  , _sepInfo :: !i
  } deriving (Eq,Ord,Generic,Functor,Foldable,Traversable,Show)
instance HasInfo (SeparatorExp Info) where
  getInfo = _sepInfo
instance NFData i => NFData (SeparatorExp i)

instance Pretty (SeparatorExp i) where
  pretty (SeparatorExp sep' _) = pretty sep'

-- | Pact syntax expressions
data Exp i =
  ELiteral (LiteralExp i) |
  EAtom (AtomExp i) |
  EList (ListExp i) |
  ESeparator (SeparatorExp i)
  deriving (Eq,Ord,Generic,Functor,Foldable,Traversable,Show)

instance Pretty (Exp i) where
  pretty = \case
    ELiteral l   -> pretty l
    EAtom a      -> pretty a
    EList l      -> pretty l
    ESeparator s -> pretty s

instance NFData i => NFData (Exp i)
instance HasInfo (Exp Info) where
  getInfo e = case e of
    ELiteral i -> getInfo i
    EAtom a -> getInfo a
    EList l -> getInfo l
    ESeparator s -> getInfo s

makePrisms ''Exp

pattern CommaExp :: Exp t
pattern CommaExp <- ESeparator (SeparatorExp Comma _i)

pattern ColonExp :: Exp t
pattern ColonExp <- ESeparator (SeparatorExp Colon _i)
