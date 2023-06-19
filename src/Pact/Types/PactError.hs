{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      :  Pact.Types.PactError
-- Copyright   :  (C) 2019 Stuart Popejoy, Kadena LLC
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- PactError and related types.
--

module Pact.Types.PactError
  ( StackFrame(..), sfName, sfLoc, sfApp
  , UxStackFrame(..)
  , PactError(..)
  , UxPactError(..)
  , PactErrorType(..)
  , RenderedOutput(..)
  , OutputType(..)
  , roText
  , roInfo
  , roType
  , renderWarn
  , renderFatal
  , _OutputFailure
  , _OutputWarning
  , _OutputTrace
  ) where

import Control.Applicative
import Control.Lens hiding ((.=),DefName, elements)
import Control.Monad
import Control.Monad.Catch
import Data.Aeson hiding (Object)
import Data.Attoparsec.Text as AP
import Data.Default
import Data.Text as T (Text, unpack, pack, init)
import Data.Text.Encoding as T
import Control.DeepSeq (NFData)

import GHC.Generics

import Text.Trifecta.Delta

import Test.QuickCheck

import Pact.Types.Lang
import Pact.Types.Orphans ()
import Pact.Types.Pretty

import qualified Pact.JSON.Encode as J

-- -------------------------------------------------------------------------- --
-- Info Parsing Utils

-- | Lenient info parser that is empty on error
--
parseInfo :: Text -> Info
parseInfo t = case parseOnly parseRenderedInfo t of
  Left _e -> def
  Right i -> i

-- | Lame parsing of 'renderInfo' parsing for UX only.
--
parseRenderedInfo :: AP.Parser Info
parseRenderedInfo = peekChar >>= \case
  Nothing -> return (Info Nothing)
    -- NOTE that this will never be parsed when the info value is embedded
    -- into another value (e.g. StackFrame). Instead the parser will fail.
  Just _ -> do
    fOrI <- takeTill isColon
    colon'
    ln <- round <$> scientific
    colon' AP.<?> "second colon"
    cn <- round <$> scientific
    let delt = case fOrI of
          "<interactive>"
            -- heuristic: ln == 0 means Columns ...
            | ln == 0 -> Columns cn 0
            -- otherwise Lines, which reverses the 'succ' above
            | otherwise -> Lines (pred ln) cn 0 0
          f -> Directed (T.encodeUtf8 f) (pred ln) cn 0 0
    return (Info (Just ("",Parsed delt 0)))
  where
    colon' = void $ char ':'
    isColon = (== ':')

arbitraryRenderedInfo :: Gen Info
arbitraryRenderedInfo = Info <$> oneof
  [ Just . ("",) <$> arbitrary
  -- , return Nothing -- broken! this does not roundtrip; instead it results in
  -- a parser failure.
  ]

-- -------------------------------------------------------------------------- --
-- StackFrame

data StackFrame = StackFrame {
      _sfName :: !Text
    , _sfLoc :: !Info
    , _sfApp :: !(Maybe (FunApp,[Text]))
    } deriving (Eq,Generic)

instance NFData StackFrame

instance J.Encode StackFrame where
  build = J.string . show
  {-# INLINE build #-}

instance Show StackFrame where
  show (StackFrame n i app) = renderInfo i ++ ": " ++ case app of
    Nothing -> unpack n
    Just (_,as) -> "(" ++ unpack n ++ concatMap (\a -> " " ++ unpack (asString a)) as ++ ")"

-- | This instance does not roundtrip
--
instance Arbitrary StackFrame where
  arbitrary = StackFrame <$> arbitrary <*> arbitrary <*> arbitrary

-- -------------------------------------------------------------------------- --
-- UxStackFrame

-- | StackFrame with permissive JSON parser for UX only
--
-- BIG HUGE CAVEAT: Back compat requires maintaining the pre-existing
-- 'ToJSON' instance, so this is ONLY for UX coming out of serialized
-- endpoints like `poll` in Chainweb; "Info" and "FunApp" values will
-- be sketchy. As such this is also permissive on failure.
--
newtype UxStackFrame = UxStackFrame { _getUxStackFrame :: StackFrame }
  deriving (Show, Eq)
  deriving newtype (J.Encode)


instance FromJSON UxStackFrame where
  parseJSON = withText "StackFrame" $ \t ->
    case parseOnly parseUxStackFrame t of
      Right sf -> pure $ sf
      Left e -> pure . UxStackFrame $
        StackFrame ("StackFrame parse failed: " <> pack e) def Nothing

instance Arbitrary UxStackFrame where
  arbitrary = fmap UxStackFrame $ StackFrame
    <$> genBareText
    <*> arbitraryRenderedInfo
    <*> oneof
     [ pure Nothing
     , pure uxStackFrameApp
     ]

uxStackFrameApp :: Maybe (FunApp, [a])
uxStackFrameApp = Just (FunApp def "" Nothing Defun (funTypes $ FunType [] TyAny) Nothing ,[])

-- | Attempt to parse 'Show' instance output. Intentionally avoids parsing app args,
-- cramming all of the text into '_sfName'.
--
parseUxStackFrame :: AP.Parser UxStackFrame
parseUxStackFrame = UxStackFrame <$> do
  i <- parseRenderedInfo
    -- embedding this parser is buggy. In case Info is Nothing it will fail.
  void $ string ": "
  parseDeets i <|> justName i
  where
    parseDeets i = do
      void $ char '('
      deets <- T.init <$> takeText
      return $ StackFrame deets i uxStackFrameApp
    justName i = takeText >>= \n -> return $ StackFrame n i Nothing

-- -------------------------------------------------------------------------- --
-- PactErrorType

data PactErrorType
  = EvalError
  | ArgsError
  | DbError
  | TxFailure
  | SyntaxError
  | GasError
  | ContinuationError
  deriving (Show,Eq,Generic)
instance NFData PactErrorType

instance FromJSON PactErrorType

instance J.Encode PactErrorType where
  build EvalError = J.text "EvalError"
  build ArgsError = J.text "ArgsError"
  build DbError = J.text "DbError"
  build TxFailure = J.text "TxFailure"
  build SyntaxError = J.text "SyntaxError"
  build GasError = J.text "GasError"
  build ContinuationError = J.text "ContinuationError"
  {-# INLINE build #-}

instance Arbitrary PactErrorType where
  arbitrary = elements [ EvalError, ArgsError, DbError, TxFailure, SyntaxError, GasError ]

-- -------------------------------------------------------------------------- --
-- PactError

data PactError = PactError
  { peType :: !PactErrorType
  , peInfo :: !Info
  , peCallStack :: ![StackFrame]
  , peDoc :: !Doc }
  deriving (Eq,Generic)

instance NFData PactError
instance Exception PactError

instance J.Encode PactError where
  build o = J.object
    [ "callStack" J..= J.array (peCallStack o)
    , "type" J..= peType o
    , "message" J..= J.string (show (peDoc o))
    , "info" J..= J.string (renderInfo (peInfo o))
    ]
  {-# INLINE build #-}

instance Show PactError where
  show (PactError t i _ s) =
    show i ++ ": Failure: " ++ maybe "" (++ ": ") msg ++ show s
   where
    msg = case t of
      EvalError -> Nothing
      ArgsError -> Nothing
      TxFailure -> Just "Tx Failed"
      DbError -> Just "Database exception"
      SyntaxError -> Just "Syntax error"
      GasError -> Just "Gas Error"
      ContinuationError -> Just "Continuation Error"

instance Arbitrary PactError where
  arbitrary = PactError
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> pure (pretty @String "PRETTY_PRINTER DOC")

-- -------------------------------------------------------------------------- --
-- UxPactError

-- | PactError with permissive JSON parser for UX only
--
-- CAVEAT: this is "UX only" due to issues with Info, StackFrame, and that
-- historically these were ignored here. As such this is a "lenient" parser
-- returning the old values on failure.
--
newtype UxPactError = UxPactError { _getUxPactError :: PactError }
  deriving (Show, Eq)
  deriving newtype (J.Encode)

instance FromJSON UxPactError where
  parseJSON = withObject "PactError" $ \o -> do
    typ <- o .: "type"
    doc <- o .: "message"
    inf <- parseInfo <$> o .: "info"
    sf <- parseSFs <$> o .: "callStack"
    pure . UxPactError $ PactError typ inf sf (prettyString doc)
    where
      parseSFs :: [Text] -> [StackFrame]
      parseSFs sfs = case mapM (parseOnly parseUxStackFrame) sfs of
        Left _e -> []
        Right ss -> _getUxStackFrame <$> ss

instance Arbitrary UxPactError where
  arbitrary = fmap UxPactError $ PactError
    <$> arbitrary
    <*> arbitraryRenderedInfo
    <*> (fmap _getUxStackFrame <$> arbitrary)
    <*> pure (pretty @String "PRETTY_PRINTER DOC")

-- -------------------------------------------------------------------------- --
-- OutputType

data OutputType =
  OutputFailure |
  OutputWarning |
  OutputTrace
  deriving (Show,Eq,Generic)

instance FromJSON OutputType

instance J.Encode OutputType where
  build OutputFailure = J.text "OutputFailure"
  build OutputWarning = J.text "OutputWarning"
  build OutputTrace = J.text "OutputTrace"
  {-# INLINE build #-}

instance Arbitrary OutputType where
  arbitrary = elements [OutputFailure, OutputWarning, OutputTrace]

-- -------------------------------------------------------------------------- --
-- RenderedOutput

-- | Tool warning/error output.
data RenderedOutput = RenderedOutput
  { _roText :: !Text
  , _roInfo :: !Info
  , _roType :: !OutputType }
  deriving (Eq,Show)

instance Pretty RenderedOutput where
  pretty (RenderedOutput t i f) = pretty (renderInfo i) <> ":" <> pretty (show f) <> ": " <> pretty t


instance FromJSON RenderedOutput where
  parseJSON = withObject "RenderedOutput" $ \o -> RenderedOutput
      <$> o .: "text"
      <*> (parseInfo <$> o .: "info")
      <*> o .: "type"

instance J.Encode RenderedOutput where
  build o = J.object
    [ "text" J..= _roText o
    , "type" J..= _roType o
    , "info" J..= J.string (renderInfo (_roInfo o))
    ]
  {-# INLINE build #-}

instance Arbitrary RenderedOutput where
  arbitrary = RenderedOutput <$> arbitrary <*> arbitraryInfo <*> arbitrary
   where
    -- renderInfo and parseInfo ignore the code component of Info
    arbitraryInfo = oneof
      [ pure (Info Nothing)
      , Info . Just . ("",) <$> arbitrary
      ]

renderWarn, renderFatal :: Text -> RenderedOutput
renderWarn t = RenderedOutput t def OutputWarning
renderFatal t = RenderedOutput t def OutputFailure

-- -------------------------------------------------------------------------- --
-- Lenses

makeLenses ''StackFrame
makeLenses ''RenderedOutput
makePrisms ''OutputType
