{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Pact.Types.Swagger
-- Copyright   :  (C) 2019 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Swagger specification utilities and exports.
--

module Pact.Types.Swagger
  (
    -- | Schemas
    byteBase64url
    -- | ToSchema implementations
  , declareGenericEmpty
  , declareGenericSchema
  , declareGenericString
  , declareSumSchema
  , lensyDeclareNamedSchema
  , namedSchema
    -- | Modifiers
  , fixedLength
  , replaceOrModify
  , toNiceString
  , optConstructor
  , optFieldLabel
  , optionsOf
  , swaggerProperties
  , swaggerDescription
  , swaggerType
  , schemaOf
  , withSchema
    -- | Re-exports
  , ToSchema(..)
    -- | Debugging
  , debugSchema
  ) where

import GHC.Exts (fromList)
import Data.Swagger
import Data.Swagger.Declare
import Data.Swagger.Internal
import Data.Swagger.Internal.TypeShape
import Data.Swagger.Internal.Schema
import GHC.Generics
import Control.Lens (set)
import Data.Text as T
import Pact.Types.Util


-- | 'ToSchema' generic implementation with provided schema.
declareGenericSchema ::
  forall a d f proxy. (Generic a, Rep a ~ D1 d f, Datatype d) =>
  Schema -> proxy a -> Declare (Definitions Schema) NamedSchema
declareGenericSchema s proxy = pure $ genericNameSchema defaultSchemaOptions proxy s

-- | 'ToSchema' generic implementation with empty schema.
declareGenericEmpty ::
  forall a d f proxy. (Generic a, Rep a ~ D1 d f, Datatype d) =>
  proxy a -> Declare (Definitions Schema) NamedSchema
declareGenericEmpty = declareGenericSchema mempty

declareGenericString ::
  forall a d f proxy. (Generic a, Rep a ~ D1 d f, Datatype d) =>
  proxy a -> Declare (Definitions Schema) NamedSchema
declareGenericString = declareGenericSchema (schemaOf $ swaggerType SwaggerString)

-- | 'ToSchema' instance for unprefixing single-constructor field names.
lensyDeclareNamedSchema ::
  (GenericHasSimpleShape a
    "genericDeclareNamedSchemaUnrestricted"
    (GenericShape (Rep a)),
   Generic a, GToSchema (Rep a))
  => Int
  -> proxy a
  -> Declare (Definitions Schema) NamedSchema
lensyDeclareNamedSchema i proxy = named'
  where named' = genericDeclareNamedSchema (fromAesonOptions $ lensyOptions i) proxy
        c =      set description (Just "") . _namedSchemaSchema <$> named'  --{ _schemaDescription = Just "" } }
  --_schemaDescription <$> _namedSchemaSchema <$> genericDeclareNamedSchema (fromAesonOptions $ lensyOptions i) proxy
  --return $ named . set description (Just "")

-- | Source: https://github.com/haskell-servant/servant-swagger/issues/80
declareSumSchema ::
  Text -> [(Text, Referenced Schema)] -> Declare (Definitions Schema) NamedSchema
declareSumSchema n p = pure $ NamedSchema (Just n) $
  schemaOf $ swaggerType SwaggerObject .
      set properties (fromList p) .
      set minProperties (Just 1) .
      set maxProperties (Just 1)



namedSchema :: Text -> Schema -> proxy a -> Declare (Definitions Schema) NamedSchema
namedSchema n s _ = return $ NamedSchema (Just n) s


-- | like 'byteSchema' but with (non-standard) "base64url" in format.
byteBase64url :: Schema
byteBase64url = set type_ SwaggerString . set format (Just "base64url") $ mempty


fixedLength :: Integral i => i -> Schema -> Schema
fixedLength i =
  set maxLength (Just $ fromIntegral i) .
  set minLength (Just $ fromIntegral i)


replaceOrModify :: (Eq a) => (a, a) -> (a -> a) -> a -> a
replaceOrModify (target, replacement) modifier curr
  | (target == curr)  = replacement
  | otherwise         = modifier curr


toNiceString :: Int -> String -> String
toNiceString i = unpack . T.toLower . T.drop i . pack


optConstructor :: (String -> String) -> SchemaOptions -> SchemaOptions
optConstructor f o = o { constructorTagModifier = f }


optFieldLabel :: (String -> String) -> SchemaOptions -> SchemaOptions
optFieldLabel f o = o { fieldLabelModifier = f }

optionsOf :: (SchemaOptions -> SchemaOptions) -> SchemaOptions
optionsOf f = f defaultSchemaOptions

swaggerProperties :: [(Text, Referenced Schema)] -> Schema -> Schema
swaggerProperties p = set properties (fromList p)


swaggerDescription :: Maybe Text -> Schema -> Schema
swaggerDescription = set description


swaggerType :: SwaggerType 'SwaggerKindSchema -> Schema -> Schema
swaggerType = set type_


schemaOf :: (Schema -> Schema) -> Schema
schemaOf f = f mempty


withSchema :: Schema -> (Schema -> Schema) -> Schema
withSchema s f = f s

-- debugSchema
debugSchema :: ToSchema a => proxy a -> NamedSchema
debugSchema = undeclare . declareNamedSchema
