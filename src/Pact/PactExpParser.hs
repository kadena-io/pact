{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
-- |
-- Module      :  Pact.PactExpParser
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Parser to 'PactExp's.
--

module Pact.PactExpParser
    -- (exprs, exprsOnly
    (parseExprs
    ,parseNumber
    ,parseSExp
    ,parseSExps
    ,parseType
    ,spanToParsed
    ) where

import Data.Semigroup
-- import Data.Foldable (asum)
import           Control.Applicative
import           Control.Comonad      (extract)
-- import           Control.Monad
import qualified Data.Attoparsec.Text as AP
import           Data.Decimal
-- import           Data.List            hiding (span)
import           Data.String          (fromString)
import qualified Data.Text            as T
import           Text.Trifecta        (Spanned (..))
import qualified Text.Trifecta        as TF
import           Text.Trifecta.Delta  (bytes)

import           Pact.SExpParser
import           Pact.Types.Lang
import           Pact.Types.SExp

-- import           Prelude              hiding (span)


-- mkList :: [PactExp] -> Parsed -> PactExp
-- mkList exprs' =
--   let lty = case nub (map expPrimTy exprs') of
--         [Just ty] -> ty
--         _         -> TyAny
--   in EList exprs' $ IsLiteralList lty

-- -- | Main parser for Pact expressions.
-- expr :: SExpProcessor PactExp
-- expr = SExpProcessor $ \case

--   -- lists
--   List listTy inner :~ span : input -> retL span input inner $ case listTy of
--     Paren  -> EList <$> many expr <*> pure IsntLiteralList
--     Square -> fmap mkList $ expr `sepBy1` comma <|> many expr

--     Curly -> do
--       ps <- pairs
--       let (ops, kvs) = unzip ps
--       if | all (== ":")  ops -> pure $ EObject  kvs
--          | all (== ":=") ops -> pure $ EBinding kvs
--          | otherwise -> fail $ "Mixed binding/object operators: " ++ show ops

--   -- token patterns
--   Token (Punctuation "'" NoTrailingSpace) :~ span1
--     : Token (Ident ident' _) :~ span2
--     : input
--     -> ret (span1 <> span2) input $ ESymbol ident'

--   Token (String str) :~ span : input
--     -> ret span input $ ELiteral $ LString str
--   Token (Ident bStr _) :~ span : input
--     | Just b <- toBool bStr
--     -> ret span input $ ELiteral $ LBool b
--   Token (Number num) :~ span : input -> ret span input $ case num of
--     Left i  -> ELiteral $ LInteger i
--     Right d -> ELiteral $ LDecimal d

--   Token (Ident a NoTrailingSpace) :~ span1
--     : Token (Punctuation "." NoTrailingSpace) :~ span2
--     : Token (Ident q _) :~ span3
--     : input
--     -> ret (span1 <> span2 <> span3) input $ EAtom a (Just q) Nothing

--   Token (Ident a _) :~ span : input -> case input of
--     Token (Punctuation ":" _) :~ _ : input' -> asum
--       [ do
--         (input', _, ty) <- unP parseType input'
--         ret span input' $ EAtom a Nothing $ Just ty
--       , ret span input $ EAtom a Nothing Nothing
--       ]
--     _ -> ret span input $ EAtom a Nothing Nothing

--   _ -> Nothing

--   where

--     retL span input inner action = do
--       ([], _span, result) <- unP action inner
--       ret span input result

--     ret span input exp = pure (input, Just span, exp (spanToParsed span))

spanToParsed :: TF.Span -> Parsed
spanToParsed (TF.Span start end _bs)
  = Parsed start $ fromIntegral $ bytes end - bytes start

-- pairs :: SExpProcessor [(Text, (PactExp, PactExp))]
-- pairs =
--   let p = do
--         k  <- expr
--         op <- punctuation ":=" <|> colon
--         v  <- expr
--         return (op, (k, v))
--   in p `sepBy` comma

-- expPrimTy :: PactExp -> Maybe (Type TypeName)
-- expPrimTy ELiteral {..} = Just $ TyPrim $ litToPrim _eLiteral
-- expPrimTy ESymbol {}    = Just $ TyPrim TyString
-- expPrimTy _             = Nothing

parseType :: (Alternative m, Monad m) => SExpProcessorT m (Type TypeName)
parseType = SExpProcessor $ \case
  List Square ty :~ span : input -> do
    ([], _, ty') <- unP parseType ty
    pure (input, Just span, TyList ty')
  Token (Ident ty _) :~ span : input
    | Just schemaTy <- schemaPrefix ty
    -> case input of
         List Curly _ :~ _ : _ -> do
           (input, span', userSchema) <- unP parseUserSchema input
           pure (input, Just span <> span', TySchema schemaTy userSchema)
         _ -> pure (input, Just span, TySchema schemaTy TyAny)
  Token (Ident name _) :~ span : input -> (input,Just span,) <$>
    if
    | name == tyInteger -> pure $ TyPrim TyInteger
    | name == tyDecimal -> pure $ TyPrim TyDecimal
    | name == tyTime    -> pure $ TyPrim TyTime
    | name == tyBool    -> pure $ TyPrim TyBool
    | name == tyString  -> pure $ TyPrim TyString
    | name == tyList    -> pure $ TyList TyAny
    | name == tyValue   -> pure $ TyPrim TyValue
    | name == tyKeySet  -> pure $ TyPrim TyKeySet
    | otherwise         -> empty
  input -> unP parseUserSchema input

  where
    schemaPrefix ty
      | ty == tyObject = Just TyObject
      | ty == tyTable  = Just TyTable
      | otherwise      = empty

parseUserSchema :: (Alternative m, Monad m) => SExpProcessorT m (Type TypeName)
parseUserSchema = SExpProcessor $ \case
  List Curly [ Token (Ident name _) :~ _ ] :~ span : input
    -> pure (input, Just span, TyUser $ fromString $ T.unpack name)
  _ -> empty

-- -- | Parse one or more Pact expressions.
-- exprs :: SExpProcessor [PactExp]
-- exprs = some expr

-- -- | Parse one or more Pact expressions and EOF.
-- -- Unnecessary with Atto's 'parseOnly'.
-- exprsOnly :: TF.Parser [PactExp]
-- exprsOnly = do
--   sexps' <- sexps
--   case runP exprs sexps' of
--     ParsedOne pactExps [] -> pure $ extract pactExps
--     ParsedOne _        xs -> TF.raiseErr $ TF.failed $ "leftover sexps from parse " ++ show xs
--     NoParse               -> TF.raiseErr $ TF.failed "no parse"

-- -- | "Production" parser: atto, parse multiple exps.
parseExprs :: Text -> Either String [Spanned SExp]
parseExprs = AP.parseOnly sexps
  -- sexps' <- AP.parseOnly sexps text
  -- case runP exprs sexps' of
  --   ParsedOne pactExps [] -> pure $ extract pactExps
  --   ParsedOne _        xs -> fail $ "leftover sexps from parse " ++ show xs
  --   NoParse               -> fail "no parse"

parseNumber :: Text -> Either String (Either Integer Decimal)
parseNumber text = AP.parseOnly (unSExpParser number) text

parseSExp :: Text -> Either String (Spanned SExp)
parseSExp txt =
  AP.parseOnly (unSExpParser $ TF.whiteSpace *> sexp <* TF.eof) txt

parseSExps :: Text -> Either String [SExp]
parseSExps txt = fmap extract <$>
  AP.parseOnly (unSExpParser $ TF.whiteSpace *> some sexp <* TF.eof) txt

-- _parseF :: TF.Parser a -> FilePath -> IO (TF.Result (a,String))
-- _parseF p fp = readFile fp >>= \s -> fmap (,s) <$> TF.parseFromFileEx p fp

-- _parseAccounts :: IO (TF.Result ([PactExp],String))
-- _parseAccounts = _parseF exprsOnly "examples/accounts/accounts.pact"
