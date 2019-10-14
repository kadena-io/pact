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



import Control.Lens hiding ((.=),DefName)
import Control.Monad.Catch
import Data.Aeson hiding (Object)
import Data.Default
import Data.Text (Text, unpack)
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

instance Show StackFrame where
    show (StackFrame n i app) = renderInfo i ++ ": " ++ case app of
      Nothing -> unpack n
      Just (_,as) -> "(" ++ unpack n ++ concatMap (\a -> " " ++ unpack (asString a)) as ++ ")"
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
instance FromJSON PactError where
  parseJSON = withObject "PactError" $ \o -> do
    typ <- o .: "type"
    doc <- o .: "message"
    pure $ PactError typ def def (prettyString doc)

instance Show PactError where
    show (PactError t i _ s) = show i ++ ": Failure: " ++ maybe "" (++ ": ") msg ++ show s
      where msg = case t of
              EvalError -> Nothing
              ArgsError -> Nothing
              TxFailure -> Just "Tx Failed"
              DbError -> Just "Database exception"
              SyntaxError -> Just "Syntax error"
              GasError -> Just "Gas Error"
