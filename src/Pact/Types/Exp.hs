{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

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
   genLiteralString,
   genLiteralInteger,
   genLiteralDecimal,
   genLiteralBool,
   genLiteralTime,
   genRoundtripableTimeUTCTime,
   simpleISO8601,formatLTime,
   litToPrim,
   LiteralExp(..),AtomExp(..),ListExp(..),SeparatorExp(..),
   Exp(..),
   _ELiteral,_EAtom,_EList,_ESeparator,
   ListDelimiter(..),enlist,
   Separator(..),
   pattern CommaExp,
   pattern ColonExp,
   ParsedCode(..)
   ) where


import Control.Applicative
import Control.Lens (makePrisms)
import Data.List
import Control.Monad
import Prelude
import Data.Text (Text,pack)
import Data.Aeson
import Data.Maybe (fromMaybe)
import Data.Monoid (Any(..))
import GHC.Generics (Generic)
import Data.Decimal
import Control.DeepSeq
import Data.Ratio ((%), denominator)
import Data.Serialize (Serialize)
import Data.String (IsString, fromString)
import Test.QuickCheck
import Test.QuickCheck.Instances ()

import Pact.Types.Info
import Pact.Types.Pretty
import Pact.Types.SizeOf
import Pact.Time (UTCTime, fromPosixTimestampMicros, formatTime, toPosixTimestampMicros)
import Pact.Types.Type
import Pact.Types.Codec
import Pact.Types.Util (genBareText, JsonProperties, JsonMProperties, enableToJSON, (.?=))

import qualified Pact.JSON.Encode as J


-- | Custom generator of arbitrary UTCTime from
-- years 1000-01-1 to 2100-12-31
genArbitraryUTCTime :: Gen UTCTime
genArbitraryUTCTime = fromPosixTimestampMicros
    <$> choose (-30610224000000000, 4133894400000000)

genLiteralString :: Gen Literal
genLiteralString = LString <$> resize 100 genBareText

genLiteralInteger :: Gen Literal
genLiteralInteger =
  LInteger <$> resize (fromInteger ((snd jsIntegerBounds) * 10)) arbitrary

genLiteralDecimal :: Gen Literal
genLiteralDecimal = LDecimal <$> resize 1000 arbitrary

genLiteralBool :: Gen Literal
genLiteralBool = LBool <$> arbitrary

genLiteralTime :: Gen Literal
genLiteralTime = LTime <$> genRoundtripableTimeUTCTime

-- | Generate a an arbitrary UTCTime value that can roundtrip via 'Pact.Types.Codec.timeCodec'.
--
-- See the documentation of 'Pact.Types.Codec.timeCodec' for details.
--
genRoundtripableTimeUTCTime :: Gen UTCTime
genRoundtripableTimeUTCTime = do
  t <- genArbitraryUTCTime
  if denom1000 t == 1 && denom t /= 1
    then genRoundtripableTimeUTCTime
    else return t
 where
  -- This works around a bug in the time codec
  denom1000 = denominator @Integer . (% 1000) . fromIntegral . toPosixTimestampMicros
  denom = denominator @Integer . (% 1000000) . fromIntegral . toPosixTimestampMicros

data Literal =
    LString { _lString :: !Text } |
    LInteger { _lInteger :: !Integer } |
    LDecimal { _lDecimal :: !Decimal } |
    LBool { _lBool :: !Bool } |
    LTime { _lTime :: !UTCTime }
        deriving (Eq,Generic,Ord,Show)

-- | Custom generator of potentially large, arbitrary Literals
instance Arbitrary Literal where
  arbitrary =
    oneof
    [ genLiteralString
    , genLiteralInteger
    , genLiteralDecimal
    , genLiteralBool
    , genLiteralTime
    ]

instance Serialize Literal
instance NFData Literal

instance SizeOf Literal where
  sizeOf ver (LString t) = (constructorCost 1) + (sizeOf ver t)
  sizeOf ver (LInteger i) = (constructorCost 1) + (sizeOf ver i)
  sizeOf ver (LDecimal d) = (constructorCost 1) + (sizeOf ver d)
  sizeOf _ (LBool _) = (constructorCost 1) + 0
  sizeOf ver (LTime ti) = (constructorCost 1) + (sizeOf ver ti)

makePrisms ''Literal

-- | ISO8601 Thyme format
simpleISO8601 :: String
simpleISO8601 = "%Y-%m-%dT%H:%M:%SZ"

formatLTime :: UTCTime -> Text
formatLTime = pack . formatTime simpleISO8601
{-# INLINE formatLTime #-}

-- | Pretty is supposed to match 1-1 with Pact representation
-- for true literals, while time emits a 'simpleISO8601' string.
instance Pretty Literal where
    pretty (LString s)   = dquotes $ pretty s
    pretty (LInteger i)  = pretty i
    pretty (LDecimal d@(Decimal e _))
      | e == 0 = viaShow d <> ".0"
      | otherwise = viaShow d
    pretty (LBool True)  = "true"
    pretty (LBool False) = "false"
    pretty (LTime t)     = dquotes $ pretty $ formatLTime t

instance ToJSON Literal where
    toJSON (LString s) = enableToJSON "Pact.Types.Exp.Literal" $ toJSON s
    toJSON (LInteger i) = enableToJSON "Pact.Types.Exp.Literal" $ valueEncoder integerCodec i
    toJSON (LDecimal r) = enableToJSON "Pact.Types.Exp.Literal" $ valueEncoder decimalCodec r
    toJSON (LBool b) = enableToJSON "Pact.Types.Exp.Literal" $ toJSON b
    toJSON (LTime t) = enableToJSON "Pact.Types.Exp.Literal" $ valueEncoder timeCodec t

    toEncoding (LString s) = toEncoding s
    toEncoding (LInteger i) = aesonEncoder integerCodec i
    toEncoding (LDecimal r) = aesonEncoder decimalCodec r
    toEncoding (LBool b) = toEncoding b
    toEncoding (LTime t) = aesonEncoder timeCodec t

    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

instance J.Encode Literal where
    build (LString s) = J.build s
    build (LInteger i) = encoder integerCodec i
    build (LDecimal r) = encoder decimalCodec r
    build (LBool b) = J.build b
    build (LTime t) = encoder timeCodec t
    {-# INLINE build #-}


instance FromJSON Literal where
  parseJSON n@Number{} = LDecimal <$> decoder decimalCodec n
  parseJSON (String s) = pure $ LString s
  parseJSON (Bool b) = pure $ LBool b
  parseJSON o@Object {} =
    (LInteger <$> decoder integerCodec o) <|>
    (LTime <$> decoder timeCodec o) <|>
    (LDecimal <$> decoder decimalCodec o)
  parseJSON _t = fail "Literal parse failed"

litToPrim :: Literal -> PrimType
litToPrim LString {} = TyString
litToPrim LInteger {} = TyInteger
litToPrim LDecimal {} = TyDecimal
litToPrim LBool {} = TyBool
litToPrim LTime {} = TyTime

data ListDelimiter = Parens|Brackets|Braces deriving (Eq,Show,Ord,Generic,Bounded,Enum)
instance NFData ListDelimiter
instance ToJSON ListDelimiter where
  toJSON Parens = enableToJSON "Pact.Types.Exp.ListDelimiter" $ "()"
  toJSON Brackets = enableToJSON "Pact.Types.Exp.ListDelimiter" $ "[]"
  toJSON Braces = enableToJSON "Pact.Types.Exp.ListDelimiter" $ "{}"

  toEncoding Parens = toEncoding @String "()"
  toEncoding Brackets = toEncoding @String "[]"
  toEncoding Braces = toEncoding @String "{}"

  {-# INLINE toJSON #-}
  {-# INLINE toEncoding #-}

instance J.Encode ListDelimiter where
  build Parens = J.build @Text "()"
  build Brackets = J.build @Text "[]"
  build Braces = J.build @Text "{}"
  {-# INLINE build #-}

instance Arbitrary ListDelimiter where
  arbitrary = elements [ Parens, Brackets, Braces ]

instance FromJSON ListDelimiter where
  parseJSON = withText "ListDelimiter" $ \t -> case t of
    "()" -> pure Parens
    "[]" -> pure Brackets
    "{}" -> pure Braces
    _ -> fail "Invalid ListDelimiter"

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
instance ToJSON Separator where
  toJSON Colon = enableToJSON "Pact.Types.Exp.Separator" ":"
  toJSON ColonEquals = enableToJSON "Pact.Types.Exp.Separator" ":="
  toJSON Comma = enableToJSON "Pact.Types.Exp.Separator" ","
  toEncoding Colon = toEncoding @String ":"
  toEncoding ColonEquals = toEncoding @String ":="
  toEncoding Comma = toEncoding @String ","

instance J.Encode Separator where
  build Colon = J.build @Text ":"
  build ColonEquals = J.build @Text ":="
  build Comma = J.build @Text ","
  {-# INLINE build #-}

instance FromJSON Separator where
  parseJSON = withText "Separator" $ \t -> case t of
    ":" -> pure Colon
    ":=" -> pure ColonEquals
    "," -> pure Comma
    _ -> fail "Invalid separator"

instance Arbitrary Separator where
  arbitrary = elements [Colon, ColonEquals, Comma]

expInfoField :: IsString a => a
expInfoField = fromString "i"

data LiteralExp i = LiteralExp
  { _litLiteral :: !Literal
  , _litInfo :: !i
  } deriving (Eq,Ord,Generic,Functor,Foldable,Traversable,Show)
instance HasInfo (LiteralExp Info) where
  getInfo = _litInfo
instance NFData i => NFData (LiteralExp i)

instance Arbitrary i => Arbitrary (LiteralExp i) where
  arbitrary = LiteralExp <$> arbitrary <*> arbitrary

literalExpProperties :: ToJSON i => JsonProperties (LiteralExp i)
literalExpProperties o =
  [ expInfoField .= _litInfo o
  , "lit" .= _litLiteral o
  ]
{-# INLINE literalExpProperties #-}

instance ToJSON i => ToJSON (LiteralExp i) where
  toJSON = enableToJSON "Pact.Types.Exp.LiteralExp i" . object . literalExpProperties
  toEncoding = pairs . mconcat . literalExpProperties
  {-# INLINE toJSON #-}
  {-# INLINE toEncoding #-}

instance J.Encode i => J.Encode (LiteralExp i) where
  build o = J.object
    [ expInfoField J..= _litInfo o
    , "lit" J..= _litLiteral o
    ]
  {-# INLINE build #-}

instance FromJSON i => FromJSON (LiteralExp i) where
  parseJSON = withObject "LiteralExp" $ \o ->
    LiteralExp <$> o .: "lit" <*> o .: expInfoField

instance Pretty (LiteralExp i) where
  pretty (LiteralExp l _) = pretty l

data AtomExp i = AtomExp
  { _atomAtom :: !Text
  , _atomQualifiers :: ![Text]
  , _atomDynamic :: !Bool
  , _atomInfo :: !i
  } deriving (Eq,Ord,Generic,Functor,Foldable,Traversable,Show)
instance HasInfo (AtomExp Info) where
  getInfo = _atomInfo
instance NFData i => NFData (AtomExp i)

instance Arbitrary i => Arbitrary (AtomExp i) where
  arbitrary = AtomExp <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

atomExpProperties :: ToJSON i => JsonMProperties (AtomExp i)
atomExpProperties o = mconcat
  [ "atom" .= _atomAtom o
  , "dyn" .?= if _atomDynamic o then Just True else Nothing
  , "q" .= _atomQualifiers o
  , expInfoField .= _atomInfo o
  ]

instance ToJSON i => ToJSON (AtomExp i) where
  toJSON = enableToJSON "Pact.Types.Exp.AtomExp i" . Object . atomExpProperties
  toEncoding = pairs . atomExpProperties
  {-# INLINE toJSON #-}
  {-# INLINE toEncoding #-}

instance J.Encode i => J.Encode (AtomExp i) where
  build o = J.object
    [ "atom" J..= _atomAtom o
    , "dyn" J..??= Any (_atomDynamic o)
    , "q" J..= J.array (_atomQualifiers o)
    , expInfoField J..= _atomInfo o
    ]
  {-# INLINE build #-}

instance FromJSON i => FromJSON (AtomExp i) where
  parseJSON = withObject "AtomExp" $ \o ->
    AtomExp <$> o .: "atom" <*> o .: "q"
    <*> (fromMaybe False <$> o .:? "dyn")
    <*> o .: expInfoField
instance Pretty (AtomExp i) where
  pretty (AtomExp atom qs False _)
    = mconcat $ punctuate dot $ fmap pretty $ qs ++ [atom]
  pretty (AtomExp atom qs True _)
    = mconcat $ punctuate (colon <> colon) $ fmap pretty $ qs ++ [atom]

data ListExp i = ListExp
  { _listList :: ![Exp i]
  , _listDelimiter :: !ListDelimiter
  , _listInfo :: !i
  } deriving (Eq,Ord,Generic,Functor,Foldable,Traversable,Show)
instance HasInfo (ListExp Info) where
  getInfo = _listInfo
instance NFData i => NFData (ListExp i)

instance Arbitrary i => Arbitrary (ListExp i) where
  arbitrary = ListExp <$> arbitrary <*> arbitrary <*> arbitrary

listExpProperties :: ToJSON i => JsonProperties (ListExp i)
listExpProperties o =
  [ "list" .= _listList o
  , "d" .= _listDelimiter o
  , expInfoField .= _listInfo o
  ]
{-# INLINE listExpProperties #-}

instance ToJSON i => ToJSON (ListExp i) where
  toJSON = enableToJSON "Pact.Types.Exp.ListExp i" . object . listExpProperties
  toEncoding = pairs . mconcat . listExpProperties
  {-# INLINEABLE toJSON #-}
  {-# INLINEABLE toEncoding #-}

instance J.Encode i => J.Encode (ListExp i) where
  build o = J.object
    [ "list" J..= J.array (_listList o)
    , "d" J..= _listDelimiter o
    , expInfoField J..= _listInfo o
    ]
  {-# INLINE build #-}

instance FromJSON i => FromJSON (ListExp i) where
  parseJSON = withObject "ListExp" $ \o ->
    ListExp <$> o .: "list" <*> o .: "d" <*> o .: expInfoField

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

instance Arbitrary i => Arbitrary (SeparatorExp i) where
  arbitrary = SeparatorExp <$> arbitrary <*> arbitrary

separatorExpProperties :: ToJSON i => JsonProperties (SeparatorExp i)
separatorExpProperties o =
  [ "sep" .= _sepSeparator o
  , expInfoField .= _sepInfo o
  ]
{-# INLINE separatorExpProperties #-}

instance ToJSON i => ToJSON (SeparatorExp i) where
  toJSON = enableToJSON "Pact.Types.Exp.SeparatorExp i" . object . separatorExpProperties
  toEncoding = pairs . mconcat . separatorExpProperties
  {-# INLINE toJSON #-}
  {-# INLINE toEncoding #-}

instance FromJSON i => FromJSON (SeparatorExp i) where
  parseJSON = withObject "SeparatorExp" $ \o ->
    SeparatorExp <$> o .: "sep" <*> o.: expInfoField

instance J.Encode i => J.Encode (SeparatorExp i) where
  build o = J.object
    [ "sep" J..= _sepSeparator o
    , expInfoField J..= _sepInfo o
    ]
  {-# INLINE build #-}

instance Pretty (SeparatorExp i) where
  pretty (SeparatorExp sep' _) = pretty sep'

-- SizeOf instances
instance SizeOf Separator where
  sizeOf _ _ = 0

instance SizeOf ListDelimiter where
  sizeOf _ _ = 0

instance (SizeOf i) => SizeOf (LiteralExp i)
instance (SizeOf i) => SizeOf (AtomExp i)
instance (SizeOf i) => SizeOf (ListExp i)
instance (SizeOf i) => SizeOf (SeparatorExp i)

-- | Pact syntax expressions
data Exp i =
  ELiteral !(LiteralExp i) |
  EAtom !(AtomExp i) |
  EList !(ListExp i) |
  ESeparator !(SeparatorExp i)
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

instance (SizeOf i) => SizeOf (Exp i)

instance ToJSON i => ToJSON (Exp i) where
  toJSON (ELiteral a) = enableToJSON "Pact.Types.Exp.Exp i" $ toJSON a
  toJSON (EAtom a) = enableToJSON "Pact.Types.Exp.Exp i" $ toJSON a
  toJSON (EList a) = enableToJSON "Pact.Types.Exp.Exp i" $ toJSON a
  toJSON (ESeparator a) = enableToJSON "Pact.Types.Exp.Exp i" $ toJSON a

  toEncoding (ELiteral a) = toEncoding a
  toEncoding (EAtom a) = toEncoding a
  toEncoding (EList a) = toEncoding a
  toEncoding (ESeparator a) = toEncoding a

instance J.Encode i => J.Encode (Exp i) where
  build (ELiteral a) = J.build a
  build (EAtom a) = J.build a
  build (EList a) = J.build a
  build (ESeparator a) = J.build a
  {-# INLINE build #-}

instance FromJSON i => FromJSON (Exp i) where
  parseJSON v =
    (ELiteral <$> parseJSON v) <|>
    (EAtom <$> parseJSON v) <|>
    (EList <$> parseJSON v) <|>
    (ESeparator <$> parseJSON v)

instance Arbitrary i => Arbitrary (Exp i) where
  arbitrary = sized $ \case
    0 -> oneof
      [ ELiteral <$> arbitrary
      , EAtom <$> arbitrary
      , ESeparator <$> arbitrary
      ]
    s -> do
      Positive k <- arbitrary
      resize (s `div` k + 1) $ oneof
        [ ELiteral <$> arbitrary
        , EAtom <$> arbitrary
        , EList <$> arbitrary
        , ESeparator <$> arbitrary
        ]

makePrisms ''Exp

pattern CommaExp :: Exp t
pattern CommaExp <- ESeparator (SeparatorExp Comma _i)

pattern ColonExp :: Exp t
pattern ColonExp <- ESeparator (SeparatorExp Colon _i)

-- | Pair parsed Pact expressions with the original text.
data ParsedCode = ParsedCode
  { _pcCode :: !Text
  , _pcExps :: ![Exp Parsed]
  } deriving (Eq,Show,Generic)
instance NFData ParsedCode
