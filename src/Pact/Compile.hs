{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
-- |
-- Module      :  Pact.Compile
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Compiler from 'Exp' -> 'Term Name'
--

module Pact.Compile
    (
     termParse,multiTermParse
    ,MkInfo,mkEmptyInfo,mkStringInfo,mkTextInfo
    )

where

import Data.Semigroup ((<>))
import Control.Comonad (extract)
import Text.Trifecta (Span, Spanned(..), Result(..), _errDoc)
import qualified Text.Trifecta as TF
import Text.Trifecta.Delta (bytes)
import Text.Trifecta.Rendering (span)
import qualified Text.Trifecta.Delta as TF
import Control.Applicative
import Data.List hiding (span)
import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Control.Arrow
import Prelude hiding (exp, span)
import Bound
import Text.PrettyPrint.ANSI.Leijen (putDoc)
import Control.Exception
import Data.String
import Control.Lens hiding (List)
import Data.Maybe
import Data.Default
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.HashSet as HS

import Pact.Types.Lang
import Pact.Types.SExp
import Pact.PactExpParser (parseExprs, spanToParsed)
import Pact.Types.Runtime (PactError(..),PactErrorType(..))
import Pact.Types.Hash
import Pact.SExpParser
import Pact.PactExpParser (parseType)

type MkInfo = Span -> Info

mkEmptyInfo :: MkInfo
mkEmptyInfo e = Info (Just (mempty,spanToParsed e))

mkStringInfo :: String -> MkInfo
mkStringInfo s e = Info (Just (code, parsed))
  where parsed = spanToParsed e
        code = fromString $
          take (_pLength parsed) $
          drop (fromIntegral $ bytes parsed) s

mkTextInfo :: T.Text -> MkInfo
mkTextInfo s e = Info (Just (code, parsed))
  where parsed = spanToParsed e
        code = Code $
          T.take (_pLength parsed) $
          T.drop (fromIntegral $ bytes parsed) s

data TermParseState = TermParseState {
  _csFresh :: Int,
  _csModule :: Maybe (ModuleName,Hash)
  }
instance Default TermParseState where def = TermParseState 0 def
makeLenses ''TermParseState

type TermParse = SExpProcessorT InnerParse

type InnerParse =
  ReaderT MkInfo
  (StateT TermParseState
  (Except PactError))

reserved :: [Text]
reserved = map pack $ words "use module defun defpact step step-with-rollback true false let let* defconst"

-- TODO: naming parseTerm vs termParse is confusing
termParse :: MkInfo -> Spanned SExp -> Either PactError (Term Name)
termParse mi sexp = runExcept (evalStateT (runReaderT (parseTerm sexp) mi) def)

multiTermParse
  :: Traversable t
  => MkInfo -> t (Spanned SExp) -> Either PactError (t (Term Name))
multiTermParse mi exps = sequence $ termParse mi <$> exps


syntaxError
  :: (MonadError PactError m, MonadReader MkInfo m)
  => Span -> String -> m a
syntaxError s str = do
  i <- mkInfo s
  throwError $ PactError SyntaxError i def (pack str)

-- patterns / view patterns

-- toBool :: Text -> Maybe Bool
-- toBool = \case
--   "true"  -> Just True
--   "false" -> Just False
--   _       -> Nothing

pattern LitString :: Text -> SExp
pattern LitString s = Token (String s)

-- TODO remove for viewLitSymbol
pattern LitSymbol :: Text -> Span -> Span -> [Spanned SExp] -> [Spanned SExp]
pattern LitSymbol ident span1 span2 input <-
  Token (Punctuation "'" NoTrailingSpace) :~ span1
    : Token (Ident ident _) :~ span2
    : input

viewLitSymbol = \case
  Token (Punctuation "'" NoTrailingSpace) :~ span1
    : Token (Ident ident _) :~ span2
    : input
    -> Just (ident, span1 <> span2, input)
  _ -> Nothing

-- It's valid to use either a literal string, bareword, or symbol in a `use`
-- statement.
--
-- TODO: deprecate the literal string and symbol forms
viewUseName :: [Spanned SExp] -> Maybe (Spanned Text, [Spanned SExp])
viewUseName = \case
  LitString name :~ s : input                             -> Just (name :~ s, input)
  (viewAtom -> Just (name :~ s, Nothing, Nothing, input)) -> Just (name :~ s, input)
  (viewLitSymbol -> Just (name, s, input))                -> Just (name :~ s, input)
  _                                                       -> Nothing

viewAtom
  :: [Spanned SExp]
  -> Maybe (Spanned Text, Maybe (Spanned Text), Maybe (Spanned (Type TypeName)), [Spanned SExp])
viewAtom = \case
  Token (Ident a NoTrailingSpace) :~ span1
    : Token (Punctuation "." NoTrailingSpace) :~ _
    : Token (Ident q _) :~ span2
    : input
    -> Just (a :~ span1, Just (q :~ span2), Nothing, input)

  Token (Ident a _) :~ s : input -> case input of
    Token (Punctuation ":" _) :~ _ : input' -> msum
      [ do
        (input', Just tyS, ty) <- unP parseType input'
        Just (a :~ s, Nothing, Just (ty :~ tyS), input)
      , Just (a :~ s, Nothing, Nothing, input)
      ]
    _ -> Just (a :~ s, Nothing, Nothing, input)
  _ -> Nothing

-- | Consume a meta-block (returning the leftover body).
--
-- Helper for 'MetaExp'.
viewMeta :: [Spanned SExp] -> (Meta, [Spanned SExp])
viewMeta = \case
  -- Either we encounter a plain docstring:
  LitString docs :~ _ : exps
    -> (Meta (Just docs) Nothing, exps)

  -- ... or some subset of @doc and @model:
  --
  -- TODO: make tag recognition extensible via proper token parsing
  Ident' "@doc" :~ _ : LitString docs :~ _ : Ident' "@model" :~ _ : model :~ _
    : exps
    -> (Meta (Just docs) (Just model), exps)
  Ident' "@model" :~ _ : model :~ _ : Ident' "@doc" :~ _ : LitString docs :~ _
    : exps
    -> (Meta (Just docs) (Just  model), exps)
  Ident' "@doc" :~ _ : LitString docs :~ _ : exps
    -> (Meta (Just docs) Nothing, exps)
  Ident' "@model" :~ _ : model :~ _ : exps
    -> (Meta Nothing (Just model), exps)

  -- ... or neither:
  exps -> (Meta Nothing Nothing, exps)

-- | A meta-annotation, which includes either form:
--
-- * `"docstring"`
-- * `@doc ...` / `@meta ...`
pattern MetaExp :: Meta -> [Spanned SExp] -> [Spanned SExp]
pattern MetaExp dm exps <- (viewMeta -> (dm, exps))

pattern Ident' name <- Token (Ident name _)

-- | A (non-empty) body with a possible meta-annotation
pattern MetaBodyExp :: Meta -> [Spanned SExp] -> [Spanned SExp]
pattern MetaBodyExp meta body <- (viewMetaBody -> Just (meta, body))

-- TODO(joel): uncomment when on modern ghc
-- {-# complete MetaBodyExp, [] #-}

-- | Consume a meta-annotationa and body. Helper for 'MetaBodyExp'.
viewMetaBody :: [Spanned SExp] -> Maybe (Meta, [Spanned SExp])
viewMetaBody = \case
  []                -> Nothing
  MetaExp meta body -> Just (meta, body)
  _                 -> error "the first two patterns are complete"

-- parsing terms

doUse :: TermParse (Term Name)
doUse = SExpProcessor $ \case
  LitString str :~ span1
    : (viewUseName -> Just (name :~ span2, input))
    -> do
      hash <- mkHash "use" name span2
      done (span1 <> span2) $ TUse (ModuleName name) (Just hash)

  LitString str :~ s : input
    -> done s $ TUse (ModuleName str) Nothing

mkHash
  :: (MonadError PactError m, MonadReader MkInfo m)
  => String -> Text -> Span -> m Hash
mkHash msg h s = case fromText' h of
  Left e -> syntaxError s $ msg ++ ": bad hash: " ++ e
  Right mh -> return mh

doModule :: Span -> Span -> TermParse (Term Name)
doModule listSpan atomSpan = SExpProcessor $ \case
  Ident' n :~ span1 : LitSymbol k span2 span3 input
    -> handleModule n k input listSpan atomSpan
  Ident' n :~ span1 : LitString k :~ span2 : input
    -> handleModule n k input listSpan atomSpan
  _ -> syntaxError listSpan "Invalid module definition"

handleModule
  :: Text -> Text -> [Spanned SExp] -> Span -> Span
  -> InnerParse ([Spanned SExp], Maybe Span, Term Name)
handleModule = undefined

-- handleModule name ksName input listInfo atomInfo = case input of
--   MetaBodyExp meta body -> mkModule body meta
--   _                     -> syntaxError atomInfo "Empty module"
--   where
--     defOnly s d = case d of
--       TDef {}    -> return d
--       TNative {} -> return d
--       TConst {}  -> return d
--       TSchema {} -> return d
--       TTable {}  -> return d
--       TUse {}    -> return d
--       TBless {}  -> return d
--       t -> syntaxError s "Only defun, defpact, defconst, deftable, use, bless allowed in module"
--     mkModule :: [Spanned SExp] -> Meta -> InnerParse ([Spanned SExp], Maybe Span, Term Name)
--     mkModule body docs = do
--       cm <- use csModule
--       case cm of
--         Just _ -> syntaxError listInfo "Invalid nested module"
--         Nothing -> do
--           let code = case listInfo of
--                 Info Nothing -> "<code unavailable>"
--                 Info (Just (c,_)) -> c
--               modName = ModuleName name
--               modHash = hash $ encodeUtf8 $ _unCode code
--           csModule .= Just (modName,modHash)
--           bd <- mapNonEmpty "module" (parseTerm >=> defOnly undefined) body listInfo
--           csModule .= Nothing
--           let blessed = HS.fromList $ (`concatMap` bd) $ \t -> case t of
--                 TBless {..} -> [_tBlessed]
--                 _ -> []
--           return $ TModule
--             (Module modName (KeySetName ksName) docs code modHash blessed)
--             (abstract (const Nothing) (TList bd TyAny listInfo)) listInfo


currentModule
  :: (MonadError PactError m, MonadState TermParseState m, MonadReader MkInfo m)
  => Span -> m (ModuleName, Hash)
currentModule s = use csModule >>= \case
  Just cm -> return cm
  Nothing -> syntaxError s "Must be declared within module"

doDef :: DefType -> Span -> Span -> TermParse (Term Name)
doDef defType listSpan nameSpan = SExpProcessor $ \case
  (viewAtom -> Just (dn :~ s, Nothing, ty, List Paren args :~ argsS : MetaBodyExp meta body))
    -> do
      args' <- doArgs argsS args
      let argsn = map (\aa -> Name (_aName aa) (_aInfo aa)) args'
      namei <- mkInfo nameSpan
      dty   <- FunType args' <$> maybeTyVar namei ty
      cm    <- currentModule listSpan
      db    <- abstract (`elemIndex` argsn) <$> runBody body listSpan
      done listSpan $ TDef dn (fst cm) defType dty db meta
  _ -> syntaxError nameSpan "Invalid def"

doArgs :: Span -> [Spanned SExp] -> InnerParse [Arg (Term Name)]
doArgs s = \case
  (viewAtom -> Just (name :~ nameS, Nothing, ty, input)) -> do
    argsInfo <- mkInfo s
    nameInfo <- mkInfo nameS
    tv       <- maybeTyVar nameInfo ty
    args     <- doArgs s input
    pure $ Arg name tv argsInfo : args
  [] -> pure []
  _ -> syntaxError s "Expected unqualified atom"

freshTyVar :: MonadState TermParseState m => m (Type (Term Name))
freshTyVar = do
  c <- state (view csFresh &&& over csFresh succ)
  pure $ mkTyVar (cToTV c) []

cToTV :: Int -> TypeVarName
cToTV n | n < 26 = fromString [toC n]
        | n <= 26 * 26 = fromString [toC (pred (n `div` 26)), toC (n `mod` 26)]
        | otherwise = fromString $ toC (n `mod` 26) : show ((n - (26 * 26)) `div` 26)
  where toC i = toEnum (fromEnum 'a' + i)

_testCToTV :: Bool
_testCToTV = nub vs == vs where vs = take (26*26*26) $ map cToTV [0..]

maybeTyVar
  :: MonadState TermParseState m
  => Info -> Maybe (Spanned (Type TypeName)) -> m (Type (Term Name))
maybeTyVar _ Nothing = freshTyVar
maybeTyVar i (Just (t :~ s)) = return (liftTy i t)

liftTy :: Info -> Type TypeName -> Type (Term Name)
liftTy i = fmap (return . (`Name` i) . asString)

done :: Span -> (Info -> a) -> InnerParse ([Spanned SExp], Maybe Span, a)
done s result = do
  info <- mkInfo s
  pure ([], Just s, result info)

doStep :: Span -> TermParse (Term Name)
doStep s = SExpProcessor $ \case
  [exp] -> do
    exp' <- parseTerm exp
    done s $ TStep Nothing exp' Nothing
  [entity, exp] -> do
    entity' <- parseTerm entity
    exp'    <- parseTerm exp
    done s $ TStep (Just entity') exp' Nothing
  _ -> syntaxError s "Invalid step definition"

doStepRollback :: Span -> TermParse (Term Name)
doStepRollback s = SExpProcessor $ \case
  [exp,rb] -> do
    exp' <- parseTerm exp
    rb'  <- parseTerm rb
    done s $ TStep Nothing exp' (Just rb')
  [entity,exp,rb] -> do
    entity' <- parseTerm entity
    exp'    <- parseTerm exp
    rb'     <- parseTerm rb
    done s $ TStep (Just entity') exp' (Just rb')
  _ -> syntaxError s "Invalid step-with-rollback definition"

letPair :: Spanned SExp -> InnerParse (Arg (Term Name), Term Name)
letPair (List Paren lst@(viewAtom -> Just (name :~ s, Nothing, ty, [v])) :~ e) = do
  let (_ :~ k) = head lst
  ki <- mkInfo k
  (,) <$> (Arg name <$> maybeTyVar ki ty <*> mkInfo e) <*> parseTerm v
letPair (_ :~ t) = syntaxError t "Invalid let pair"

doLet :: Span -> TermParse (Term Name)
doLet s = SExpProcessor $ \case
  List Paren bindings :~ _ : body -> do
    bPairs <- traverse letPair bindings
    let bNames = map (\(aa,_) -> Name (_aName aa) (_aInfo aa)) bPairs
    bs <- abstract (`elemIndex` bNames) <$> runBody body s
    done s $ TBinding bPairs bs BindLet
  _ -> syntaxError s "Invalid let declaration"

-- | let* is a macro to nest a bunch of lets
doLets :: Span -> TermParse (Term Name)
doLets s = SExpProcessor $ \case
  lst@(List Paren bindings :~ _) : body -> case bindings of
    [_] -> unP (doLet s) (lst:body)
    e:es -> do
      let s' = head es ^. span
      unP (doLet s) $
        List Paren [e] :~ (e ^. span)
          : List Paren
            [ Token (Ident "let*" TrailingSpace) :~ s'
            , List Paren es :~ s'
            ] :~ s'
          : body
  _ -> syntaxError s "Invalid let* binding"


doConst :: Span -> TermParse (Term Name)
doConst s = do
  i <- mkInfo s
  SExpProcessor $ \case
    (viewAtom -> Just (name :~ s, Nothing, ty, t : MetaExp meta [])) -> do
      v'      <- parseTerm t
      (cm, _) <- currentModule s
      ty'     <- maybeTyVar i ty
      done s $ TConst (Arg name ty' i) cm (CVRaw v') meta
    _ -> syntaxError s "Invalid defconst"

doSchema :: Span -> TermParse (Term Name)
doSchema s = SExpProcessor $ \case
  (viewAtom -> Just (utn :~ s, Nothing, Nothing, MetaBodyExp meta input)) -> do
    cm <- currentModule s
    fs <- forM input undefined

      -- \a -> case a of
      --   EAtom an Nothing ty _ai -> mkInfo a >>= \ai -> Arg an <$> maybeTyVar ai ty <*> pure ai
      --   _ -> syntaxError i "Invalid schema field definition"
    done s $ TSchema (TypeName utn) (fst cm) meta fs
  _ -> syntaxError s "Invalid schema definition"

doTable :: Span -> TermParse (Term Name)
doTable s = SExpProcessor $ \case
  (viewAtom -> Just (tn :~ s, Nothing, ty, MetaExp meta [])) -> do
    cm <- currentModule s
    i <- mkInfo s
    tty :: Type (Term Name) <- case ty of
      Just (ot@TyUser {} :~ s') -> return $ liftTy i ot
      Nothing -> return TyAny
      _ -> syntaxError s "Invalid table row type, must be an object type e.g. {myobject}"
    done s $ TTable (TableName tn) (fst cm) (snd cm) tty meta
  _ -> syntaxError s "Invalid table definition"

doBless :: Span -> TermParse (Term Name)
doBless outerSpan = SExpProcessor $ \case
  (LitString str :~ s) : input -> do
    h <- mkHash "bless" str s
    done outerSpan $ TBless h
  _ -> syntaxError outerSpan "Invalid bless, must contain valid hash"

mkInfo :: MonadReader MkInfo m => Span -> m Info
mkInfo e = do
  f <- ask
  return (f e)

parseTerm :: Spanned SExp -> InnerParse (Term Name)
parseTerm (List Paren sexps :~ listSpan) = case sexps of
  -- TODO: qualifier case
  ea@(Token (Ident a _) :~ identSpan) : input -> do
    listInfo <- mkInfo listSpan
    atomInfo <- mkInfo identSpan
    -- TODO: not sure about this
    ([], _, result) <- flip unP input $ case a of
      "use"                -> doUse
      "module"             -> doModule listSpan identSpan
      "defun"              -> doDef Defun listSpan identSpan
      "defpact"            -> doDef Defpact listSpan identSpan
      "step"               -> doStep listSpan
      "step-with-rollback" -> doStepRollback listSpan
      "defconst"           -> doConst listSpan
      "defschema"          -> doSchema listSpan
      "deftable"           -> doTable listSpan
      "bless"              -> doBless listSpan
      "let"                -> doLet listSpan
      "let*"               -> doLets listSpan
    pure result

-- parseTerm l@(EList (ea@(EAtom a q Nothing _):rest) IsntLiteralList _) = do
--     li <- mkInfo l
--     ai <- mkInfo ea
--     case (a,q) of
--       (_,_) ->
--         case break (isJust . firstOf _EBinding) rest of
--           (preArgs,be@(EBinding bs _):bbody) ->
--             do
--               as <- mapM parseTerm preArgs
--               bi <- mkInfo be
--               let mkPairs (v,k) = (,) <$> atomVar k <*> parseTerm v
--               bs' <- mapNonEmpty "binding" mkPairs bs li
--               let ks = map (\(aa,_) -> Name (_aName aa) (_aInfo aa)) bs'
--               bdg <- TBinding <$> pure bs' <*>
--                    (abstract (`elemIndex` ks) <$> runBody bbody bi) <*> pure (BindSchema TyAny) <*> pure bi
--               TApp <$> mkVar a q ai <*> pure (as ++ [bdg]) <*> pure li
--           _ -> TApp <$> mkVar a q ai <*> mapM parseTerm rest <*> pure li

parseTerm (List Curly bs :~ s) = do
  i <- mkInfo s
  ps <- pairs
  let (ops, kvs) = unzip ps
  if | all (== ":") ops -> do
        kvs' <- traverse (\(k,v) -> (,) <$> parseTerm k <*> parseTerm v) kvs
        pure $ TObject kvs' TyAny i
     | all (== ":=") ops -> throwError s "Unexpected binding"
     | otherwise -> throwError s $ "Mixed binding/object operators: " ++ show ops
-- parseTerm e@(EBinding _ _i) = syntaxError e "Unexpected binding"
-- parseTerm e@(ESymbol s _i) = TLiteral (LString s) <$> mkInfo e
-- parseTerm e@(ELiteral l _i) = TLiteral l <$> mkInfo e
-- parseTerm e@(EAtom s q t _i) | s `elem` reserved = syntaxError e $ "Unexpected reserved word: " ++ show s
--                     | isNothing t = mkInfo e >>= mkVar s q
--                     | otherwise = syntaxError e "Invalid typed var"
-- parseTerm e@(EList els (IsLiteralList lty) _i) = mkInfo e >>= \i -> TList <$> mapM parseTerm els <*> pure (liftTy i lty) <*> pure i
parseTerm (_ :~ s) = syntaxError s "Unexpected expression"
{-# INLINE parseTerm #-}

pairs
  :: (Alternative m, Monad m)
  => SExpProcessorT m [(Text, (Spanned SExp, Spanned SExp))]
pairs =
  let p = do
        k  <- sexp
        op <- punctuation ":=" <|> colon
        v  <- sexp
        return (op, (k, v))
  in p `sepBy` comma

mkVar :: Text -> Maybe Text -> Info -> TermParse (Term Name)
mkVar s q i = pure $
  TVar (maybe (Name s i) (\qn -> QName (ModuleName s) qn i) q) i
{-# INLINE mkVar #-}

mapNonEmpty
  :: (MonadError PactError m, MonadReader MkInfo m)
  => String -> (a -> m b) -> [a] -> Span -> m [b]
mapNonEmpty str _ [] s  = syntaxError s $ "Empty " ++ str
mapNonEmpty _ act body _ = mapM act body
{-# INLINE mapNonEmpty #-}

runNonEmpty :: String -> [Spanned SExp] -> Span -> InnerParse [Term Name]
runNonEmpty s = mapNonEmpty s parseTerm
{-# INLINE runNonEmpty #-}

-- atomVar :: [Spanned SExp] -> TermParse (Arg (Term Name))
-- atomVar (viewAtom -> Just (a, Nothing, ty, [])) = do
--   i <- mkInfo e
--   Arg a <$> maybeTyVar i ty <*> pure i
-- atomVar _ = syntaxError undefined "Expected unqualified atom"
-- {-# INLINE atomVar #-}

runBody :: [Spanned SExp] -> Span -> InnerParse (Term Name)
runBody bs s = do
  body <- runNonEmpty "body" bs s
  i <- mkInfo s
  pure $ TList body TyAny i
{-# INLINE runBody #-}


_compileAccounts :: IO (Either PactError [Term Name])
_compileAccounts = _parseF "examples/accounts/accounts.pact" >>= _compile

_compile :: Result ([Spanned SExp],String) -> IO (Either PactError [Term Name])
_compile (Failure f) = putDoc (_errDoc f) >> error "Parse failed"
_compile (Success (a,s)) = return $ mapM (termParse (mkStringInfo s)) a


_compileStr :: String -> IO [Term Name]
_compileStr code = do
    r <- _compile ((,code) <$> TF.parseString sexps mempty code)
    case r of Left e -> throwIO $ userError (show e)
              Right t -> return t


_parseF :: FilePath -> IO (TF.Result ([Spanned SExp],String))
_parseF fp = readFile fp >>= \s -> fmap (,s) <$> TF.parseFromFileEx sexps fp

_compileFile :: FilePath -> IO [Term Name]
_compileFile f = do
    p <- _parseF f
    rs <- case p of
            (Failure e) -> putDoc (_errDoc e) >> error "Parse failed"
            (Success (es,s)) -> return $ map (termParse (mkStringInfo s)) es
    case sequence rs of
      Left e -> throwIO $ userError (show e)
      Right ts -> return ts

_atto :: FilePath -> IO [Term Name]
_atto fp = do
  f <- pack <$> readFile fp
  rs <- case parseExprs f of
    Left s -> throwIO $ userError s
    Right es -> return $ map (termParse (mkStringInfo (unpack f))) es
  case sequence rs of
      Left e -> throwIO $ userError (show e)
      Right ts -> return ts
