{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs #-}

-- |
-- Module      :  Pact.Persist.Taped
-- Copyright   :  (C) 2024 Kadena
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edmund Noble <edmund@kadena.io>
--
-- Tapes for recording and playing back database access.
--
module Pact.Persist.Taped (
   TxTapeElem(..),
   TxTape(..),
   _TxTape,
   TxTapeZipper(..),
   dbFromTape
   )
    where

import Control.Concurrent.MVar
import Control.DeepSeq (NFData(..))
import Control.Exception.Safe
import Control.Lens

import Data.Default
import Data.Maybe
import GHC.Generics (Generic)

import Pact.Types.PactError
import Pact.Types.Pretty
import Pact.Types.Term
import Pact.Types.Persistence
import qualified Pact.JSON.Encode as J
import Data.Aeson
import Data.Text (Text)

-- Other operations not stored on tape because they're not available outside
-- the repl and local modes.
data TxTapeElem
  = forall k v. TxTapeWrite !WriteType !(Domain k v) !k !v
  | forall k v. TxTapeRead !(Domain k v) !k !(Maybe v)
  | forall k v. TxTapeKeys !(Domain k v) ![k]
  | TxTapeCreateTable !TableName !ModuleName

checkDomain
  :: Domain k1 v1
  -> Domain k2 v2
  -> r
  -> ((k1 ~ k2, v1 ~ v2) => r)
  -> r
checkDomain domainActual domainExpected no yes =
  constraintsForDomain domainActual $
    case (domainActual, domainExpected) of
      (UserTables tnActual, UserTables tnExpected)
        | tnActual == tnExpected -> yes
      (KeySets, KeySets) -> yes
      (Modules, Modules) -> yes
      (Namespaces, Namespaces) -> yes
      (Pacts, Pacts) -> yes
      _ -> no

instance Eq TxTapeElem where
  TxTapeWrite wt d k v == TxTapeWrite wt' d' k' v' = constraintsForDomain d $ checkDomain d d' False $
    wt == wt' && k == k' && v == v'
  TxTapeRead d k mv == TxTapeRead d' k' mv' = constraintsForDomain d $ checkDomain d d' False $
    k == k' && mv == mv'
  TxTapeKeys d ks == TxTapeKeys d' ks' = constraintsForDomain d $ checkDomain d d' False $
    ks == ks'
  TxTapeCreateTable tn mn == TxTapeCreateTable tn' mn' =
    tn == tn' && mn == mn'
  _ == _ = False

instance NFData TxTapeElem where
  rnf (TxTapeWrite wt d k v) = constraintsForDomain d $
    rnf wt `seq` rnf d `seq` rnf k `seq` rnf v
  rnf (TxTapeRead d k v) = constraintsForDomain d $
    rnf d `seq` rnf k `seq` rnf v
  rnf (TxTapeKeys d ks) = constraintsForDomain d $
    rnf d `seq` rnf d `seq` rnf ks
  rnf (TxTapeCreateTable tn mn) =
    rnf tn `seq` rnf mn

newtype TxTape = TxTape [TxTapeElem]
  deriving stock (Eq, Generic)
  deriving newtype (Show, NFData)

instance Show TxTapeElem where
  showsPrec :: Int -> TxTapeElem -> ShowS
  showsPrec p (TxTapeWrite wt d k v) = constraintsForDomain d $
    showParen (p > 10)
    $ showString "Write "
    . foldr (\x r -> x . showString " " . r) id
    [ showsPrec 11 wt
    , showsPrec 11 d
    , showsPrec 11 k
    , showsPrec 11 v
    ]
  showsPrec p (TxTapeRead d k mv) = constraintsForDomain d $
    showParen (p > 10)
    $ showString "Read "
    . foldr (\x r -> x . showString " " . r) id
    [ showsPrec 11 d
    , showsPrec 11 k
    , showsPrec 11 mv
    ]
  showsPrec p (TxTapeKeys d ks) = constraintsForDomain d $
    showParen (p > 10)
    $ showString "Keys "
    . foldr (\x r -> x . showString " " . r) id
    [ showsPrec 11 d
    , showsPrec 11 ks
    ]
  showsPrec p (TxTapeCreateTable tn mn) =
    showParen (p > 10)
    $ showString "CreateTable "
    . foldr (\x r -> x . showString " " . r) id
    [ showsPrec 11 tn
    , showsPrec 11 mn
    ]

makePrisms ''TxTape

data TxTapeZipper = TxTapeZipper !TxTape !(Maybe TxTapeElem) !TxTape

dbFromTape :: PactDb TxTapeZipper
dbFromTape = PactDb
  { _readRow = \d k var -> modifyTape var $ doOperation "read" $ \case
    TxTapeRead d' k' mv -> constraintsForDomain d $ do
      checkDomain d d' (domainMismatch "read" d d') $ do
          checkAll mv $ catMaybes
            [ check "key" k k'
            ]
    otherOperation -> mismatchedOperation "read" otherOperation

  , _writeRow = \wt d k v var -> modifyTape var $ doOperation "write" $ \case
    TxTapeWrite wt' d' k' v' -> constraintsForDomain d $ do
      checkDomain d d' (domainMismatch "write" d d') $ do
          checkAll () $ catMaybes
            [ check "key" k k'
            , check "value" v v'
            , check "write type" wt wt'
            ]
    otherOperation -> mismatchedOperation "write" otherOperation

  , _keys = \d var -> modifyTape var $ doOperation "keys" $ \case
    TxTapeKeys d' ks ->
      checkDomain d d' (domainMismatch "keys" d d') $ return ks
    otherOperation -> mismatchedOperation "keys" otherOperation

  , _txids = \_ _ _ -> unsupportedOperation "_txids"
  , _createUserTable = \tn mn var -> modifyTape var $ doOperation "create user table" $ \case
    TxTapeCreateTable tn' mn' -> do
        checkAll () $ catMaybes
            [ check "table name" tn tn'
            , check "module name" mn mn'
            ]
    otherOperation -> mismatchedOperation "create user table" otherOperation
  , _getUserTableInfo = \_ _ -> unsupportedOperation "_getUserTableInfo"
  , _beginTx = \_ _ -> unsupportedOperation "_beginTx"
  , _commitTx = \_ -> unsupportedOperation "_commitTx"
  , _rollbackTx = \_ -> unsupportedOperation "_rollbackTx"
  , _getTxLog = \_ _ _ -> unsupportedOperation "_getTxLog"
  }
  where
  unsupportedOperation operationName =
    throwEvalError $ "dbFromTape: unsupported operation " <> operationName
  mismatchedOperation :: String -> TxTapeElem -> IO r
  mismatchedOperation actualOp expectedOp = throwEvalError $ unwords
    [ "dbFromTape: attempted to", actualOp, "but next operation on tape was", show expectedOp ]
  throwEvalError :: String -> IO r
  throwEvalError msg = throwM $ PactError DbError def [] $ pretty msg
  domainMismatch operation domainActual domainExpected = throwEvalError $ unlines
    [ "dbFromTape: tape mismatch"
    , "operation " <> operation
    , unwords ["domain", "actual", show domainActual, "expected", show domainExpected]
    ]
  check :: (Show a, Eq a) => String -> a -> a -> Maybe String
  check name actual expected
    | actual == expected = Nothing
    | otherwise = Just $ unwords [name, "actual", show actual, "expected", show expected]
  checkAll :: r -> [String] -> IO r
  checkAll r [] = return r
  checkAll _ errs = throwEvalError $ unlines $ "dbFromTape: tape mismatch" : errs
  modifyTape
    :: MVar TxTapeZipper
    -> (TxTapeZipper -> IO (TxTapeZipper, a))
    -> IO a
  modifyTape var k = do
    modifyMVar var $ \tape -> do
      (tape', r) <- k tape
      return (tape', r)
  doOperation :: String -> (TxTapeElem -> IO a) -> TxTapeZipper -> IO (TxTapeZipper, a)
  doOperation msg _ (TxTapeZipper _ Nothing _) =
    throwEvalError $ "dbFromTape: attempted " <> msg <> " beyond tape's end"
  doOperation _ g (TxTapeZipper (TxTape l) (Just c) r) = do
    res <- g c
    return $ (,res) $ case r of
      TxTape (c' : r') ->
        TxTapeZipper (TxTape (c : l)) (Just c') (TxTape r')
      TxTape [] ->
        TxTapeZipper (TxTape (c : l)) Nothing (TxTape [])

domainToJSON :: Domain k v -> [J.KeyValue]
domainToJSON (UserTables (TableName tn)) =
  [ J.KeyValue "domain" (J.text "userTable")
  , J.KeyValue "userTable" (J.build tn)
  ]
domainToJSON KeySets = [J.KeyValue "domain" (J.text "keysets")]
domainToJSON Modules = [J.KeyValue "domain" (J.text "modules")]
domainToJSON Namespaces = [J.KeyValue "domain" (J.text "namespaces")]
domainToJSON Pacts = [J.KeyValue "domain" (J.text "pacts")]

instance J.Encode TxTapeElem where
  build (TxTapeWrite wt d k v) = constraintsForDomain d $
    J.build $ J.Object $
      [ J.KeyValue "op" (J.text "write")
      , J.KeyValue "writeType" (J.build wt)
      , J.KeyValue "key" (J.build k)
      , J.KeyValue "value" (J.build v)
      ] ++ domainToJSON d
  build (TxTapeRead d k mv) = constraintsForDomain d $
    J.build $ J.Object $
      [ J.KeyValue "op" (J.text "read")
      , J.KeyValue "key" (J.build k)
      , J.KeyValue "value" (maybe J.null J.build mv)
      ] ++ domainToJSON d
  build (TxTapeKeys d ks) = constraintsForDomain d $
    J.build $ J.Object $
      [ J.KeyValue "op" (J.text "keys")
      , J.KeyValue "keys" (J.build $ J.Array $ J.build <$> ks)
      ] ++ domainToJSON d
  build (TxTapeCreateTable tn mn) = J.build $ J.Object $
    [ J.KeyValue "op" (J.text "createTable")
    , J.KeyValue "tableName" (J.build tn)
    , J.KeyValue "moduleName" (J.build mn)
    ]

instance FromJSON TxTapeElem where
  parseJSON = withObject "TxTapeElem" $ \o -> do
    operation <- o .: "op"
    case operation :: Text of
      "write" -> parseWrite o
      "read" -> parseRead o
      "keys" -> parseKeys o
      "createTable" -> parseCreateTable o
      _ -> fail "invalid op, expected write, read, keys, or createTable"
    where
    parseWrite o =
      o .: "domain" >>= \case
        SomeDomain d -> constraintsForDomain d $ do
          wt <- o .: "writeType"
          k <- o .: "key"
          v <- o .: "value"
          return $ TxTapeWrite wt d k v
    parseRead o =
      o .: "domain" >>= \case
        SomeDomain d -> constraintsForDomain d $ do
          k <- o .: "key"
          mv <- o .: "value"
          return $ TxTapeRead d k mv
    parseKeys o =
      o .: "domain" >>= \case
        SomeDomain d -> constraintsForDomain d $ do
          ks <- o .: "keys"
          return $ TxTapeKeys d ks
    parseCreateTable o = do
      tn <- o .: "tableName"
      mn <- o .: "moduleName"
      return $ TxTapeCreateTable tn mn
