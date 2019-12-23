{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}

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
  , PactError(..)
  , PactErrorType(..)
  ) where


import Control.Applicative
import Control.Lens hiding ((.=),DefName)
import Control.Monad
import Control.Monad.Catch
import Data.Aeson hiding (Object)
import Data.Attoparsec.Text as AP
import Data.Default
import Data.Text as T (Text, unpack, pack, init)
import Control.DeepSeq (NFData)

import GHC.Generics

import Pact.Types.Lang
import Pact.Types.Orphans ()
import Pact.Types.Pretty

data StackFrame = StackFrame {
      _sfName :: !Text
    , _sfLoc :: !Info
    , _sfApp :: Maybe (FunApp,[Text])
    } deriving (Eq,Generic)
instance NFData StackFrame
instance ToJSON StackFrame where toJSON = toJSON . show

-- | BIG HUGE CAVEAT: Back compat requires maintaining the pre-existing
-- 'ToJSON' instance, so this is ONLY for UX coming out of serialized
-- endpoints like `poll` in Chainweb; "Info" and "FunApp" values will
-- be sketchy. As such this is also permissive on failure.
instance FromJSON StackFrame where
  parseJSON = withText "StackFrame" $ \t -> case parseOnly parseStackFrame t of
    Right sf -> pure sf
    Left e -> pure $ StackFrame ("StackFrame parse failed: " <> pack e) def Nothing


instance Show StackFrame where
    show (StackFrame n i app) = renderInfo i ++ ": " ++ case app of
      Nothing -> unpack n
      Just (_,as) -> "(" ++ unpack n ++ concatMap (\a -> " " ++ unpack (asString a)) as ++ ")"

-- | Attempt to parse 'Show' instance output. Intentionally avoids parsing app args,
-- cramming all of the text into '_sfName'.
parseStackFrame :: AP.Parser StackFrame
parseStackFrame = do
  i <- parseRenderedInfo
  void $ string ": "
  parseDeets i <|> justName i
  where
    parseDeets i = do
      void $ char '('
      deets <- T.init <$> takeText
      return $ StackFrame deets i $
        Just (FunApp def "" Nothing Defun (funTypes $ FunType [] TyAny) Nothing
             ,[])
    justName i = takeText >>= \n -> return $ StackFrame n i Nothing

_parseStackFrame :: Text -> Either String StackFrame
_parseStackFrame = parseOnly parseStackFrame

makeLenses ''StackFrame


data PactErrorType
  = EvalError
  | ArgsError
  | DbError
  | TxFailure
  | SyntaxError
  | GasError
  deriving (Show,Eq,Generic)
instance NFData PactErrorType
instance ToJSON PactErrorType
instance FromJSON PactErrorType

data PactError = PactError
  { peType :: PactErrorType
  , peInfo :: Info
  , peCallStack :: [StackFrame]
  , peDoc :: Doc }
  deriving (Eq,Generic)

instance NFData PactError
instance Exception PactError
instance ToJSON PactError where
  toJSON (PactError t i s d) =
    object [ "type" .= t, "info" .= renderInfo i, "callStack" .= s, "message" .= (show d)]
-- CAVEAT: this is "UX only" due to issues with Info, StackFrame, and that
-- historically these were ignored here. As such this is a "lenient" parser returning
-- the old values on failure.
instance FromJSON PactError where
  parseJSON = withObject "PactError" $ \o -> do
    typ <- o .: "type"
    doc <- o .: "message"
    inf <- parseInf <$> o .: "info"
    sf <- parseSFs <$> o .: "callStack"
    pure $ PactError typ inf sf (prettyString doc)
    where
      parseInf t = case parseOnly parseRenderedInfo t of
        Left _e -> def
        Right i -> i
      parseSFs :: [Text] -> [StackFrame]
      parseSFs sfs = case sequence (map (parseOnly parseStackFrame) sfs) of
        Left _e -> []
        Right ss -> ss

instance Show PactError where
    show (PactError t i _ s) = show i ++ ": Failure: " ++ maybe "" (++ ": ") msg ++ show s
      where msg = case t of
              EvalError -> Nothing
              ArgsError -> Nothing
              TxFailure -> Just "Tx Failed"
              DbError -> Just "Database exception"
              SyntaxError -> Just "Syntax error"
              GasError -> Just "Gas Error"
