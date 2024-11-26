


{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Implementation of Hyperlane natives.
module Crypto.Hash.OwneraNatives
  ( OwneraSchemaId(..)
  , verifyOwneraSchemaStructure
  , decodeFinApiData
  , hashListSchema
  , owneraSchemaIdToText
  , textToOwneraSchemaId
  ) where

import Control.Lens ((^?), at, _Just,Prism')
import Control.Monad (when)

import Data.Map (Map,fromList,lookup)
import Data.Text (Text,unpack)
import Data.Text.Lazy (toStrict)

import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Encoding as TL

-- import Pact.Core.Pretty hiding (dot)
import Data.Traversable
import Data.Decimal

import Pact.Types.Pretty

import qualified Data.ByteArray as ByteArray

import Data.ByteString.Builder

-- import Pact.Core.Errors
-- import Pact.Core.PactValue
-- import Pact.Core.Names
import Data.Vector(Vector,toList,(!?))

import Text.Read(readMaybe)

import Pact.Types.PactValue
import Pact.Types.Exp
import Crypto.Hash

import Pact.Types.Term.Internal

data OwneraError
  = OwneraErrorFailedToFindKey FieldKey
    -- ^ An expected key was not found.
  | OwneraErrorFailedToFindHashGroup FieldKey
    -- ^ An expected key was not found.
  | OtherOwneraError Text
  deriving (Eq, Show)

instance Pretty OwneraError where
  pretty = \case
    OwneraErrorFailedToFindKey x -> prettyString "OwneraErrorFailedToFindKey: " <> pretty x
    OwneraErrorFailedToFindHashGroup x ->
       prettyString "OwneraErrorFailedToFindHashGroup: " <> pretty x
    OtherOwneraError x -> pretty x
    
data OwneraSchemaId =
      Deposit
    | PrimarySale
    | SecondarySale
    | Loan
    | Redeem
    | Withdraw

-- Function to convert OwneraSchemaId to lowercase Text with hyphens, using LambdaCase
owneraSchemaIdToText :: OwneraSchemaId -> Text
owneraSchemaIdToText = \case
    Deposit        -> "deposit"
    PrimarySale    -> "primary-sale"
    SecondarySale  -> "secondary-sale"
    Loan           -> "loan"
    Redeem         -> "redeem"
    Withdraw       -> "withdraw"

-- Map to associate Text representation with OwneraSchemaId
owneraSchemaIdMap :: Map Text OwneraSchemaId
owneraSchemaIdMap = fromList
    [ ("deposit", Deposit)
    , ("primary-sale", PrimarySale)
    , ("secondary-sale", SecondarySale)
    , ("loan", Loan)
    , ("redeem", Redeem)
    , ("withdraw", Withdraw)
    ]

owneraOperationNameSchemaIdMap :: Map Text OwneraSchemaId
owneraOperationNameSchemaIdMap = fromList
    [ ("deposit", Deposit)
    , ("issue", PrimarySale)
    , ("transfer", SecondarySale)
    , ("loan", Loan)
    , ("redeem", Redeem)
    , ("withdraw", Withdraw)
    ]

pattern PString :: Text -> PactValue
pattern PString s = PLiteral (LString s)
_PString :: Prism' PactValue Text
_PString = _PLiteral . _LString


pattern PDecimal :: Decimal -> PactValue
pattern PDecimal d = PLiteral (LDecimal d)

_PDecimal :: Prism' PactValue Decimal
_PDecimal = _PLiteral . _LDecimal

pattern PBool :: Bool -> PactValue
pattern PBool b = PLiteral (LBool b)
_PBool :: Prism' PactValue Bool
_PBool = _PLiteral . _LBool


textToOwneraSchemaId :: Text -> Either OwneraError OwneraSchemaId
textToOwneraSchemaId txt =
    case Data.Map.lookup txt owneraSchemaIdMap of
        Just schemaId -> Right schemaId
        Nothing       -> Left $ OtherOwneraError $ "Invalid schema ID: " <> txt


operationToOwneraSchemaId :: Text -> Either OwneraError OwneraSchemaId
operationToOwneraSchemaId txt =
    case Data.Map.lookup txt owneraOperationNameSchemaIdMap of
        Just schemaId -> Right schemaId
        Nothing       -> Left $ OtherOwneraError $ "Invalid operation name: " <> txt


grabStrFieldKey :: Map FieldKey PactValue -> FieldKey ->  Either OwneraError Text
grabStrFieldKey m key = case m ^? at key . _Just . _PString of
  Nothing -> Left (OwneraErrorFailedToFindKey key)
  Just t -> Right t

grabObjFieldKey :: Map FieldKey PactValue -> FieldKey ->  Either OwneraError (Map FieldKey PactValue)
grabObjFieldKey m key = case m ^? at key . _Just . _PObject of
  Nothing -> Left (OwneraErrorFailedToFindKey key)
  Just (ObjectMap t) -> Right t

grabListFieldKey :: Map FieldKey PactValue -> FieldKey ->  Either OwneraError (Vector PactValue)
grabListFieldKey m key = case m ^? at key . _Just . _PList of
  Nothing -> Left (OwneraErrorFailedToFindKey key)
  Just t -> Right t

-- grabHashGroup :: Map FieldKey PactValue -> FieldKey ->  Either OwneraError (Map FieldKey PactValue)
-- grabHashGroup m key = case m ^? at key . _Just of
--   Just (PObject o) -> Right o
--   _ -> Left (OwneraErrorFailedToFindHashGroup key)
  

newtype HashListGroupSchema = HashListGroupSchema [FieldKey]
newtype HashListSchema = HashListSchema [(FieldKey , HashListGroupSchema)]

data HashListData =  HashListData
 { _hldHash :: Text
 , _hldData :: [Text] 
 }

data HashListsData = HashListsData
 { _hlsdSignature :: Text
 , _hlsdHash      :: Text
 , _hlsdData      :: [HashListData]
 }



verifyHashListDataHash :: HashListData -> Either OwneraError ()
verifyHashListDataHash hld =
  let calculatedHash = toStrict $ TL.decodeUtf8 $ toLazyByteString $ byteStringHex $ ByteArray.convert $ hashFinalize $
             hashUpdates (hashInitWith SHA3_256 ) $
                 (map T.encodeUtf8 (_hldData hld))
  in if calculatedHash == _hldHash hld
     then Right ()
     else Left $ OtherOwneraError $ calculatedHash <> " =/= " <>  _hldHash hld 
        
hashListDataAsPactValue :: HashListGroupSchema -> HashListData -> PactValue
hashListDataAsPactValue (HashListGroupSchema xs) (HashListData _ ys) =
   PObject $ ObjectMap $
    fromList $
      map (\x@(FieldKey f , v) ->
            case (f , v) of
              ("amount" , PString s) -> (FieldKey f ,
                                           case (readMaybe $ unpack s) of
                                             Just d -> PDecimal d
                                             _ -> PString s)
              _  -> x)
        (zip xs (fmap PString ys))
  

hashListsDataAsPactValue :: HashListSchema -> HashListsData -> PactValue
hashListsDataAsPactValue (HashListSchema ys) (HashListsData _ _ xs) = 
   PObject $ ObjectMap $
    fromList $
      [ (hgK ,  hashListDataAsPactValue hgS hgV) | ((hgK , hgS) , hgV) <- zip ys xs  ] 

  
hashListSchema :: OwneraSchemaId -> HashListSchema
hashListSchema = HashListSchema . hls .
  \case
    Deposit ->
     [("DHG" ,
          ["nonce"
          ,"operation"
          ,"assetType"
          ,"assetId"
          ,"dstAccountType"
          ,"dstAccount"
          ,"amount"])]
                
    PrimarySale ->
     [("AHG" ,
          [ "nonce"
          , "operation"
          , "assetType"
          , "assetId"
          , "dstAccountType"
          , "dstAccount"
          , "amount"])
          ,
      ("SHG" , 
          ["assetType"
          ,"assetId"
          ,"srcAccountType"
          ,"srcAccount"
          ,"dstAccountType"
          ,"dstAccount"
          , "amount"])]
    SecondarySale  ->
        [(("AHG"),
          ["nonce"
          ,"operation"
          ,"assetType"
          ,"assetId"
          ,"srcAccountType"
          ,"srcAccount"       
          ,"dstAccountType"
          ,"dstAccount"
          ,"amount"])
        ,(("SHG"),
           ["assetType"
           ,"assetId"
           ,"srcAccountType"
           ,"srcAccount"
           ,"dstAccountType"
           ,"dstAccount"
           ,"amount"])]

          
          
     
    Loan           ->
     [("HG",["nonce"
            ,"operation"
            ,"pledgeAssetType"
            ,"pledgeAssetId"
            ,"pledgeBorrowerAccountType"
            ,"pledgeBorrowerAccountId"
            ,"pledgeLenderAccountType"
            ,"pledgeLenderAccountId"
            ,"pledgeAmount"          
            ,"moneyAssetType"
            ,"moneyAssetId"
            ,"moneyLenderAccountType"
            ,"moneyLenderAccountId"
            ,"moneyBorrowerAccountType"
            ,"moneyBorrowerAccountId"
            ,"borrowedMoneyAmount"
            ,"returnedMoneyAmount"
            ,"openTime"
            ,"closeTime"])]
    Redeem         ->
     [("AHG",["nonce"
             ,"operation"
             ,"assetType"
             ,"assetId"
             ,"srcAccountType"
             ,"srcAccount"          
             ,"amount"])

     ,("SHG",["assetType"
             ,"assetId"
             ,"srcAccountType"
             ,"srcAccount"
             ,"dstAccountType"
             ,"dstAccount"
             ,"amount"])]

    Withdraw       ->
     [("HG" , ["nonce"
              ,"operation"
              ,"assetType"
              ,"assetId"
              ,"srcAccountType"
              ,"srcAccount"
              ,"dstAccountType"
              ,"dstAccount"
              ,"amount"])]

 where
   hls :: [(Text , [Text])] -> [(FieldKey , HashListGroupSchema)]
   hls = fmap (\(x , y) -> (FieldKey x , HashListGroupSchema (fmap FieldKey y)))


recogniseSchema :: Map FieldKey PactValue -> Either OwneraError OwneraSchemaId
recogniseSchema obj = do
  tObj <- grabObjFieldKey obj (FieldKey "template")
  hgsLst <- grabListFieldKey tObj (FieldKey "hashGroups")
  case hgsLst !? 0 of
    Just (PObject (ObjectMap hg)) -> do
       fLst <- grabListFieldKey hg (FieldKey "fields")
       case fLst !? 1 of
        Just (PObject (ObjectMap fo)) -> do
          grabStrFieldKey fo (FieldKey "value")
            >>= operationToOwneraSchemaId
        _ -> Left (OtherOwneraError ("unable tor ecognise schema, operation field missing"))
        
    _ -> Left (OtherOwneraError ("unable tor ecognise schema, first hash group missing"))
          
           
         
  
extractOfSchema :: HashListSchema -> Map FieldKey PactValue ->
                          (Either OwneraError HashListsData)
extractOfSchema (HashListSchema hls) dObj = do
   sig <- grabStrFieldKey dObj (FieldKey "signature")
   tObj <- grabObjFieldKey dObj (FieldKey "template")
   _ <- grabStrFieldKey tObj (FieldKey "type")
   h <- grabStrFieldKey tObj (FieldKey "hash")
  
   hgsLst <- grabListFieldKey tObj (FieldKey "hashGroups")
   accumRes <- mapAccumM 
         (curry (\case
             ([] , _) -> Left $
                 OtherOwneraError ("unexpected hash group!")
             (((_  , HashListGroupSchema hgFlds) : flds) , (PObject (ObjectMap fldDataO))) -> do
                  hgFldsVec <- grabListFieldKey fldDataO (FieldKey "fields")
                  h' <- grabStrFieldKey fldDataO (FieldKey "hash")
                  ((,) flds . HashListData h') <$> consumeHashGroupFieldKeys hgFlds hgFldsVec
             (_ , _) -> Left $
                 OtherOwneraError ("hash group must by an object!")
               )) hls hgsLst
   case accumRes of
     ([] , x) -> pure (HashListsData sig h $ toList x)
     (unconsumedHGs , _) -> Left $
       OtherOwneraError ("missing hashGroups: " <> 
                           (renderCompactText (fmap fst unconsumedHGs)))

 where

   
  consumeHashGroupFieldKeys :: [FieldKey] -> Vector PactValue ->
                         Either OwneraError [Text]
  consumeHashGroupFieldKeys flds' vpv =
      mapAccumM (curry (\case
             ([] , _) -> Left $
                 OtherOwneraError ("unexpected field in hash group!")
             ((fld : flds) , (PObject (ObjectMap fobj))) -> do
                  n <- grabStrFieldKey fobj (FieldKey "name")
                  _ <- grabStrFieldKey fobj (FieldKey "type")
                  v <- grabStrFieldKey fobj (FieldKey "value")
                  if (FieldKey n) == fld
                    then pure (flds , v)
                    else Left $ OtherOwneraError
                           ("expected field: " <> n <> " unexpectly got: " <> n)
             ((_ : _) , _) -> Left $
                 OtherOwneraError ("unexpected value!")
               )) flds' vpv
        >>= \case
              ([] , x) -> pure $ toList x
              (flds , _) -> Left $
                         OtherOwneraError ("missing fields in hashGroup: " <>
                           (renderCompactText flds))
      
     

decodeFinApiData :: OwneraSchemaId -> Map FieldKey PactValue -> Either OwneraError PactValue
decodeFinApiData osId pKV = do
   extracted <- extractOfSchema (hashListSchema osId) pKV
   when False $
      mapM_ verifyHashListDataHash (_hlsdData extracted)
   pure $ hashListsDataAsPactValue (hashListSchema osId) extracted
     



verifyOwneraSchemaStructure :: Map FieldKey PactValue -> Either OwneraError PactValue
verifyOwneraSchemaStructure pkV = do
  sId <- recogniseSchema pkV
  d <- decodeFinApiData sId pkV
  pure $ PObject $ ObjectMap $ fromList
       [ ((FieldKey "verified") , (PBool True))
       , ((FieldKey "data")     , d) ]
