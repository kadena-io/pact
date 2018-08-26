{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
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

module Pact.CompileNew
    (
     compile,compileExps
    ,MkInfo,mkEmptyInfo,mkStringInfo,mkTextInfo
    )

where

import qualified Text.Trifecta as TF hiding (expected)
import qualified Text.Trifecta.Delta as TF
import Control.Applicative hiding (some,many)
import Text.Megaparsec
import Text.Megaparsec.Char (satisfy)
import Data.Proxy
import Data.Void
import Data.List.NonEmpty (NonEmpty(..),fromList)
import Data.List
import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Control.Arrow ((&&&),second)
import Prelude hiding (exp)
import Bound
import Text.PrettyPrint.ANSI.Leijen (putDoc)
import Control.Exception hiding (try)
import Data.String
import Control.Lens hiding (prism)
import Data.Maybe
import Data.Default
import Data.Text (Text,pack,unpack)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.HashSet as HS
import Data.Semigroup ((<>))

import Pact.Types.Crypto
import Pact.Types.ExpNew
import Pact.ParseNew (exprsOnly,parseExprs)
import Pact.Types.Runtime (PactError(..),PactErrorType(..))
import Pact.Types.Hash
import Pact.Types.TermNew
import Pact.Types.Util

type MkInfo = Parsed -> Info

mkEmptyInfo :: MkInfo
mkEmptyInfo e = Info (Just (mempty,e))

mkStringInfo :: String -> MkInfo
mkStringInfo s d = Info (Just (fromString $ take (_pLength d) $ drop (fromIntegral $ TF.bytes d) s,d))

mkTextInfo :: T.Text -> MkInfo
mkTextInfo s d = Info (Just (Code $ T.take (_pLength d) $ T.drop (fromIntegral $ TF.bytes d) s,d))

data Cursor = Cursor
  { _cParent :: Maybe Cursor
  , _cStream :: [Exp Info] }
makeLenses ''Cursor
instance Default Cursor where def = Cursor def def

data CompileState = CompileState
  { _csFresh :: Int
  , _csModule :: Maybe (ModuleName,Hash)
  }
makeLenses ''CompileState
instance Default CompileState where
  def = CompileState 0 Nothing


-- | copied from Text.Megaparsec.Stream
defaultAdvance1
  :: Pos               -- ^ Tab width (unused)
  -> SourcePos         -- ^ Current position
  -> t                 -- ^ Current token
  -> SourcePos         -- ^ Incremented position
defaultAdvance1 _width (SourcePos n l c) t = npos
  where
    c' = unPos c
    npos = SourcePos n l (c <> pos1)
{-# INLINE defaultAdvance1 #-}

instance Stream Cursor where
  type Token Cursor = Exp Info
  type Tokens Cursor = [Exp Info]
  tokenToChunk Proxy = pure
  tokensToChunk Proxy = id
  chunkToTokens Proxy = id
  chunkLength Proxy = length
  chunkEmpty Proxy = null
  advance1 Proxy = defaultAdvance1
  advanceN Proxy w = foldl' (defaultAdvance1 w)
  take1_ Cursor{..} = case _cStream of
    [] -> Nothing
    (t:ts) -> Just (t, Cursor _cParent ts)
  takeN_ n s@Cursor{..}
    | n <= 0        = Just ([], s)
    | null _cStream = Nothing
    | otherwise = Just $ second (Cursor _cParent) $ splitAt n _cStream
  takeWhile_ f Cursor{..} = second (Cursor _cParent) $ span f _cStream

type Compile a = StateT CompileState (Parsec Void Cursor) a

reserved :: [Text]
reserved = T.words "use module defun defpact step step-with-rollback true false let let* defconst"

runCompile :: (Compile a) -> CompileState -> Exp Info -> Either PactError a
runCompile act s a = either undefined (Right . fst) $
  runParser (runStateT act s) "" (Cursor Nothing [a])
  -- either (Left . getErr) Right $ runExcept (evalStateT act s)
 --  where getErr [] = PactError SyntaxError def def "Compile failed"
 --       getErr as = last as

compile :: MkInfo -> Exp Parsed -> Either PactError (Term Name)
compile mi e = runCompile term def (fmap mi e)

compileExps :: Traversable t => MkInfo -> t (Exp Parsed) -> Either PactError (t (Term Name))
compileExps mi exps = sequence $ compile mi <$> exps

syntaxError :: Info -> String -> Compile a
syntaxError i s = failure (Just (Label (fromList s))) def -- throwError [PactError SyntaxError i def (pack s)]

syntaxError' :: String -> Compile a
syntaxError' s = currentInfo >>= \e -> syntaxError (eInfo e) s

expected :: Text -> Compile a
expected s = syntaxError' $ "Expected " ++ (unpack s)


unexpected' :: Text -> Compile a
unexpected' s = syntaxError' $ "Unexpected " ++ (unpack s)


currentInfo :: Compile Info
currentInfo = return def -- use (csCursor . cInfo)

currentModule :: Compile (ModuleName,Hash)
currentModule = use csModule >>= \m -> case m of
  Just cm -> return cm
  Nothing -> syntaxError' "Must be declared within module"

freshTyVar :: Compile (Type (Term Name))
freshTyVar = do
  c <- state (view csFresh &&& over csFresh succ)
  return $ mkTyVar (cToTV c) []

cToTV :: Int -> TypeVarName
cToTV n | n < 26 = fromString [toC n]
        | n <= 26 * 26 = fromString [toC (pred (n `div` 26)), toC (n `mod` 26)]
        | otherwise = fromString $ toC (n `mod` 26) : show ((n - (26 * 26)) `div` 26)
  where toC i = toEnum (fromEnum 'a' + i)

_testCToTV :: Bool
_testCToTV = nub vs == vs where vs = take (26*26*26) $ map cToTV [0..]


popA :: Text -> Prism' (Exp Info) a -> Compile a
popA ty prism = token test Nothing where -- TODO provide default token
  test i = case firstOf prism i of
    Just a -> Right a
    Nothing -> Left (pure (Tokens (i:|[])),def)

enter :: ListExp Info -> Compile (ListExp Info)
enter l@ListExp{..} = do
  par <- getInput
  setInput $ Cursor (Just par) _listList
  return l

exit :: Compile ()
exit = do
  child <- getInput
  case _cParent child of
    Just p -> setInput p
    Nothing -> failure (Just EndOfInput) def


atom :: Compile (AtomExp Info)
atom = popA "atom" _EAtom

lit :: Compile (LiteralExp Info)
lit = popA "literal" _ELiteral

list :: Compile (ListExp Info)
list = popA "list" _EList

sep :: Compile (SeparatorExp Info)
sep = popA "sep" _ESeparator

lit' :: Text -> Prism' Literal a -> Compile a
lit' ty prism = lit >>= \LiteralExp{..} -> case firstOf prism _litLiteral of
  Just l -> return l
  Nothing -> expected ty

str :: Compile Text
str = lit' "string" _LString

list' :: ListDelimiter -> Compile (ListExp Info)
list' d = list >>= \l@ListExp{..} ->
  if _listDelimiter == d then return l else expected (tShow d)

withList :: ListDelimiter -> (ListExp Info -> Compile a) -> Compile a
withList d act = try $ list' d >>= enter >>= act >>= \a -> exit >> return a

withList' :: ListDelimiter -> Compile a -> Compile a
withList' d = withList d . const

sep' :: Separator -> Compile (SeparatorExp Info)
sep' s = sep >>= \se@SeparatorExp{..} ->
  if _sepSeparator == s then return se else expected (tShow s)

term :: Compile (Term Name)
term =
  literal
  <|> varAtom
  <|> listLiteral
  <|> objectLiteral
  <|> withList' Parens sexp

-- | Handle special forms and apps.
-- TODO: how to indicate an alternative is final once you're in it,
-- ie a failure fails the whole compile instead of tries something else.
-- Special forms (ie `(bless "abc")`) otherwise will alternative over to
-- apps when they fail, giving confusing errors.
sexp :: Compile (Term Name)
sexp = atom >>= \a@AtomExp{..} -> case _atomQualifiers of
  (_:_) -> app a
  [] -> case _atomAtom of
    "use" -> useForm
    "let" -> letForm
    "let*" -> letsForm
    "defconst" -> defconst
    "step" -> step
    "step-with-rollback" -> stepWithRollback
    "bless" -> bless
    "deftable" -> deftable
    _ -> app a

app :: AtomExp Info -> Compile (Term Name)
app a = do
  i <- currentInfo -- note this isn't right, should be sexp info
  v <- varAtom' a
  body <- some (term <|> bindingForm) <* eof
  return $ TApp v body i

-- | Bindings (`{ "column" := binding }`) do not explicitly scope the
-- following body form as a sexp, instead letting the body contents
-- simply follow, showing up as more args to the containing app. Thus, once a
-- binding is encountered, all following terms are subsumed into the
-- binding body, and bound/abstracted etc.
bindingForm :: Compile (Term Name)
bindingForm = do
  let pair = do
        col <- term
        a <- sep' ColonEquals *> arg
        return (a,col)
  (bindings,bi) <- withList Braces $ \ListExp{..} -> do
    ps <- (pair `sepBy` sep' Comma) <* eof
    return (ps,_listInfo)
  let bNames = map (arg2Name . fst) bindings
  body <- abstract (`elemIndex` bNames) <$> bodyForm
  return $ TBinding bindings body (BindSchema TyAny) bi


varAtom :: Compile (Term Name)
varAtom = try $ do
  a@AtomExp{..} <- atom
  when (_atomAtom `elem` reserved) $ unexpected' "reserved word"
  varAtom' a

varAtom' :: AtomExp Info -> Compile (Term Name)
varAtom' AtomExp{..} = do
  n <- case _atomQualifiers of
    [] -> return $ Name _atomAtom _atomInfo
    [q] -> return $ QName (ModuleName q) _atomAtom _atomInfo
    _ -> expected "single qualifier"
  return $ TVar n _atomInfo

bareAtom :: Compile (AtomExp Info)
bareAtom = atom >>= \a@AtomExp{..} -> case _atomQualifiers of
  (_:_) -> expected "unqualified atom"
  [] -> return a

symbol :: Text -> Compile Text
symbol s = bareAtom >>= \AtomExp {..} ->
  if s == _atomAtom then return s else expected ("bareword: " <> s)

listLiteral :: Compile (Term Name)
listLiteral = withList Brackets $ \ListExp{..} -> do
  ls <- (some term <* eof) <|>
        ((term `sepBy` sep' Comma) <* eof)
  return $ TList ls TyAny _listInfo -- TODO capture literal type if any


objectLiteral :: Compile (Term Name)
objectLiteral = withList Braces $ \ListExp{..} -> do
  let pair = do
        key <- term
        val <- sep' Colon *> term
        return (key,val)
  ps <- (pair `sepBy` sep' Comma) <* eof
  return $ TObject ps TyAny _listInfo


literal :: Compile (Term Name)
literal = lit >>= \LiteralExp{..} ->
  return $ TLiteral _litLiteral _litInfo

literal' :: Text -> Prism' Literal a -> Compile (Term Name)
literal' ty prism = literal >>= \t@TLiteral{..} -> case firstOf prism _tLiteral of
  Just _ -> return t
  Nothing -> expected ty

str' :: Compile (Term Name)
str' = literal' "string" _LString

deftable :: Compile (Term Name)
deftable = do
  i <- currentInfo
  (mn,mh) <- currentModule
  AtomExp{..} <- bareAtom
  ty <- optional (typed >>= \t -> case t of
                     TyUser {} -> return t
                     _ -> expected "user type")
  docs <- optional str
  return $ TTable (TableName _atomAtom) mn mh (fromMaybe TyAny ty) (Meta docs Nothing) i


bless :: Compile (Term Name)
bless = TBless <$> hash' <*> currentInfo

defconst :: Compile (Term Name)
defconst = do
  i <- currentInfo
  modName <- fst <$> currentModule
  a <- arg
  doc <- optional str
  v <- term
  return $ TConst a modName (CVRaw v) (Meta doc Nothing) i

step :: Compile (Term Name)
step = do
  i <- currentInfo
  TStep <$> optional str' <*> term <*> pure Nothing <*> pure i

stepWithRollback :: Compile (Term Name)
stepWithRollback = do
  i <- currentInfo
  TStep <$> optional str' <*> term <*> (Just <$> term) <*> pure i


letBindings :: Compile [(Arg (Term Name),Term Name)]
letBindings = withList' Parens $
              some $ withList' Parens $
              (,) <$> arg <*> term

letForm :: Compile (Term Name)
letForm = do
  i <- currentInfo
  bindings <- letBindings
  let bNames = map (arg2Name . fst) bindings
  scope <- abstract (`elemIndex` bNames) <$> bodyForm
  return $ TBinding bindings scope BindLet i

-- | let* is a macro to nest lets for referencing previous
-- bindings.
letsForm :: Compile (Term Name)
letsForm = do
  bindings <- letBindings
  let nest (binding:rest) = do
        let bName = [arg2Name (fst binding)]
        scope <- abstract (`elemIndex` bName) <$> case rest of
          [] -> bodyForm
          _ -> nest rest
        return $ TBinding [binding] scope BindLet (_aInfo (fst binding))
      nest [] = error "invalid state"
  nest bindings

useForm :: Compile (Term Name)
useForm = do
  i <- currentInfo
  modName <- (_atomAtom <$> bareAtom) <|> str <|> expected "bare atom, string, symbol"
  TUse (ModuleName modName) <$> optional hash' <*> pure i

hash' :: Compile Hash
hash' = str >>= \s -> case fromText' s of
  Right h -> return h
  Left e -> syntaxError' $ "bad hash: " ++ e

typedAtom :: Compile (AtomExp Info,Type (Term Name))
typedAtom = (,) <$> bareAtom <*> (typed <|> freshTyVar)

arg :: Compile (Arg (Term Name))
arg = typedAtom >>= \(AtomExp{..},ty) ->
  return $ Arg _atomAtom ty _atomInfo

arg2Name :: Arg n -> Name
arg2Name Arg{..} = Name _aName _aInfo


typed :: Compile (Type (Term Name))
typed = sep' Colon *> parseType

parseType :: Compile (Type (Term Name))
parseType = msum
  [ parseListType
  , parseUserSchemaType
  , parseSchemaType tyObject TyObject
  , parseSchemaType tyTable TyTable
  , TyPrim TyInteger <$ symbol tyInteger
  , TyPrim TyDecimal <$ symbol tyDecimal
  , TyPrim TyTime    <$ symbol tyTime
  , TyPrim TyBool    <$ symbol tyBool
  , TyPrim TyString  <$ symbol tyString
  , TyList TyAny     <$ symbol tyList
  , TyPrim TyValue   <$ symbol tyValue
  , TyPrim TyKeySet  <$ symbol tyKeySet
  ]

parseListType :: Compile (Type (Term Name))
parseListType = withList' Brackets $ TyList <$> parseType <* eof

parseSchemaType :: Text -> SchemaType -> Compile (Type (Term Name))
parseSchemaType tyRep sty = symbol tyRep >>
  (TySchema sty <$> (parseUserSchemaType <|> pure TyAny))


parseUserSchemaType :: Compile (Type (Term Name))
parseUserSchemaType = withList Braces $ \ListExp{..} -> do
  AtomExp{..} <- bareAtom
  return $ TyUser (return $ Name _atomAtom _listInfo)

bodyForm :: Compile (Term Name)
bodyForm = do
  i <- currentInfo
  bs <- some term <* eof
  return $ TList bs TyAny i


{-
doUse :: [Exp] -> Info -> Compile (Term Name)
doUse as i = case as of
  [m] -> mkM m Nothing
  [m,eh@(ELitString h)] -> mkM m . Just =<< mkHash "use" h eh
  _ -> syntaxError i "use requires module name (symbol/string/bare atom) and optional hash"
  where
    mkM m h = case m of
      (ELitString s) -> mk s h -- TODO deprecate
      (ESymbol s _) -> mk s h -- TODO deprecate
      (EAtom s Nothing Nothing _) -> mk s h
      _ -> syntaxError i "use: module name must be symbol/string/bare atom"
    mk s h = return $ TUse (ModuleName s) h i

mkHash :: String -> Text -> Exp -> Compile Hash
mkHash msg h el = case fromText' h of
      Left e -> mkInfo el >>= \i -> syntaxError i $ msg ++ ": bad hash: " ++ e
      Right mh -> return mh

-- | A meta-annotation, which includes either form:
--
-- * `"docstring"`
-- * `@doc ...` / `@meta ...`
pattern MetaExp :: Meta -> [Exp] -> [Exp]
pattern MetaExp dm exps <- (doMeta -> (dm, exps))

-- | Consume a meta-block (returning the leftover body).
--
-- Helper for 'MetaExp'.
doMeta :: [Exp] -> (Meta, [Exp])
doMeta = \case
  -- Either we encounter a plain docstring:
  ELitString docs : exps
    -> (Meta (Just docs) Nothing, exps)

  -- ... or some subset of @doc and @model:
  --
  -- TODO: make tag recognition extensible via proper token parsing
  EAtom' "@doc" : ELitString docs : EAtom' "@model" : model : exps
    -> (Meta (Just docs) (Just model), exps)
  EAtom' "@model" : model : EAtom' "@doc" : ELitString docs : exps
    -> (Meta (Just docs) (Just  model), exps)
  EAtom' "@doc" : ELitString docs : exps
    -> (Meta (Just docs) Nothing, exps)
  EAtom' "@model" : model : exps
    -> (Meta Nothing (Just model), exps)

  -- ... or neither:
  exps -> (Meta Nothing Nothing, exps)

-- | A (non-empty) body with a possible meta-annotation
pattern MetaBodyExp :: Meta -> [Exp] -> [Exp]
pattern MetaBodyExp meta body <- (doMetaBody -> Just (meta, body))

-- TODO(joel): uncomment when on modern ghc
-- {-# complete MetaBodyExp, [] #-}

-- | Consume a meta-annotationa and body. Helper for 'MetaBodyExp'.
doMetaBody :: [Exp] -> Maybe (Meta, [Exp])
doMetaBody exp  = case exp of
  []                -> Nothing
  MetaExp meta body -> Just (meta, body)
  _                 -> error "the first two patterns are complete"

doModule :: [Exp] -> Info -> Info -> Compile (Term Name)
doModule (EAtom n Nothing Nothing _:ESymbol k _:es) li ai =
  handleModule n k es li ai
doModule (EAtom n Nothing Nothing _:ELiteral (LString k) _:es) li ai =
  handleModule n k es li ai
doModule _ li _ = syntaxError li "Invalid module definition"

handleModule :: Text -> Text -> [Exp] -> Info -> Info -> Compile (Term Name)
handleModule n k es li ai =
  case es of
    MetaBodyExp meta body -> mkModule body meta
    _ -> syntaxError ai "Empty module"
    where
      defOnly d = case d of
        TDef {} -> return d
        TNative {} -> return d
        TConst {} -> return d
        TSchema {} -> return d
        TTable {} -> return d
        TUse {} -> return d
        TBless {} -> return d
        t -> syntaxError (_tInfo t) "Only defun, defpact, defconst, deftable, use, bless allowed in module"
      mkModule body docs = do
        cm <- use csModule
        case cm of
          Just _ -> syntaxError li "Invalid nested module"
          Nothing -> do
            let code = case li of
                  Info Nothing -> "<code unavailable>"
                  Info (Just (c,_)) -> c
                modName = ModuleName n
                modHash = hash $ encodeUtf8 $ _unCode code
            csModule .= Just (modName,modHash)
            bd <- mapNonEmpty "module" (run >=> defOnly) body li
            csModule .= Nothing
            let blessed = HS.fromList $ (`concatMap` bd) $ \t -> case t of
                  TBless {..} -> [_tBlessed]
                  _ -> []
            return $ TModule
              (Module modName (KeySetName k) docs code modHash blessed)
              (abstract (const Nothing) (TList bd TyAny li)) li



doDef :: [Exp] -> DefType -> Info -> Info -> Compile (Term Name)
doDef es defType namei i =
    case es of
      (EAtom dn Nothing ty _:EList args Nothing _:MetaBodyExp meta body) ->
          mkDef dn ty args body meta
      _ -> syntaxError namei "Invalid def"
      where
        mkDef dn ty dargs body ddocs = do
          args <- mapM atomVar dargs
          let argsn = map (\aa -> Name (_aName aa) (_aInfo aa)) args
          dty <- FunType <$> pure args <*> maybeTyVar namei ty
          cm <- currentModule i
          db <- abstract (`elemIndex` argsn) <$> runBody body i
          return $ TDef dn (fst cm) defType dty db ddocs i


liftTy :: Info -> Type TypeName -> Type (Term Name)
liftTy i = fmap (return . (`Name` i) . asString)

doStep :: [Exp] -> Info -> Compile (Term Name)
doStep [exp] i = TStep Nothing <$> run exp <*> pure Nothing <*> pure i
doStep [entity,exp] i =
    TStep <$> (Just <$> run entity) <*> run exp <*> pure Nothing <*> pure i
doStep _ i = syntaxError i "Invalid step definition"

doStepRollback :: [Exp] -> Info -> Compile (Term Name)
doStepRollback [exp,rb] i = TStep Nothing <$> run exp <*> (Just <$> run rb) <*> pure i
doStepRollback [entity,exp,rb] i =
    TStep <$> (Just <$> run entity) <*> run exp <*> (Just <$> run rb) <*> pure i
doStepRollback _ i = syntaxError i "Invalid step-with-rollback definition"

letPair :: Exp -> Compile (Arg (Term Name), Term Name)
letPair e@(EList [k@(EAtom s Nothing ty _),v] Nothing _) = do
  ki <- mkInfo k
  (,) <$> (Arg <$> pure s <*> maybeTyVar ki ty <*> mkInfo e) <*> run v
letPair t = syntaxError' t "Invalid let pair"

doLet :: [Exp] -> Info -> Compile (Term Name)
doLet (bindings:body) i = do
  bPairs <-
    case bindings of
      (EList es Nothing _) -> forM es letPair
      t -> syntaxError' t "Invalid let bindings"
  let bNames = map (\(aa,_) -> Name (_aName aa) (_aInfo aa)) bPairs
  bs <- abstract (`elemIndex` bNames) <$> runBody body i
  return $ TBinding bPairs bs BindLet i
doLet _ i = syntaxError i "Invalid let declaration"

-- | let* is a macro to nest a bunch of lets
doLets :: [Exp] -> Info -> Compile (Term Name)
doLets (bindings:body) i =
  case bindings of
      e@(EList [_] Nothing _) -> doLet (e:body) i
      (EList (e:es) Nothing _) -> let e' = head es in
                          doLet [EList [e] Nothing (_eParsed e),
                                 EList (EAtom "let*" Nothing Nothing (_eParsed e'):
                                        EList es Nothing (_eParsed e'):body)
                                   Nothing (_eParsed e')] i
      e -> syntaxError' e "Invalid let* binding"
doLets _ i = syntaxError i "Invalid let declaration"

doConst :: [Exp] -> Info -> Compile (Term Name)
doConst es i = case es of
  EAtom dn Nothing ct _ : t : MetaExp dm []
    -> mkConst dn ct t dm
  _ -> syntaxError i "Invalid defconst"
  where
    mkConst dn ty v docs = do
      v' <- run v
      cm <- currentModule i
      a <- Arg <$> pure dn <*> maybeTyVar i ty <*> pure i
      return $ TConst a (fst cm) (CVRaw v') docs i

doSchema :: [Exp] -> Info -> Compile (Term Name)
doSchema es i = case es of
  (EAtom utn Nothing Nothing _:MetaBodyExp meta as) ->
    mkUT utn as meta
  _ -> syntaxError i "Invalid schema definition"
  where
    mkUT utn as docs = do
      cm <- currentModule i
      fs <- forM as $ \a -> case a of
        EAtom an Nothing ty _ai -> mkInfo a >>= \ai -> Arg an <$> maybeTyVar ai ty <*> pure ai
        _ -> syntaxError i "Invalid schema field definition"
      return $ TSchema (TypeName utn) (fst cm) docs fs i

doTable :: [Exp] -> Info -> Compile (Term Name)
doTable es i = case es of
  EAtom tn Nothing ty _ : MetaExp meta [] -> mkT tn ty meta
  _ -> syntaxError i "Invalid table definition"
  where
    mkT tn ty docs = do
      cm <- currentModule i
      tty :: Type (Term Name) <- case ty of
        Just ot@TyUser {} -> return $ liftTy i ot
        Nothing -> return TyAny
        _ -> syntaxError i "Invalid table row type, must be an object type e.g. {myobject}"
      return $ TTable (TableName tn) (fst cm) (snd cm) tty docs i

doBless :: [Exp] -> Info -> Compile (Term Name)
doBless [he@(ELitString s)] i = mkHash "bless" s he >>= \h -> return $ TBless h i
doBless _ i = syntaxError i "Invalid bless, must contain valid hash"

run :: Exp -> Compile (Term Name)

run l@(EList (ea@(EAtom a q Nothing _):rest) Nothing _) = do
    li <- mkInfo l
    ai <- mkInfo ea
    case (a,q) of
      ("use",Nothing) -> doUse rest li
      ("module",Nothing) -> doModule rest li ai
      ("defun",Nothing) -> doDef rest Defun ai li
      ("defpact",Nothing) -> doDef rest Defpact ai li
      ("step",Nothing) -> doStep rest li
      ("step-with-rollback",Nothing) -> doStepRollback rest li
      ("let",Nothing) -> doLet rest li
      ("let*",Nothing) -> doLets rest li
      ("defconst",Nothing) -> doConst rest li
      ("defschema",Nothing) -> doSchema rest li
      ("deftable",Nothing) -> doTable rest li
      ("bless",Nothing) -> doBless rest li
      (_,_) ->
        case break (isJust . firstOf _EBinding) rest of
          (preArgs,be@(EBinding bs _):bbody) ->
            do
              as <- mapM run preArgs
              bi <- mkInfo be
              let mkPairs (v,k) = (,) <$> atomVar k <*> run v
              bs' <- mapNonEmpty "binding" mkPairs bs li
              let ks = map (\(aa,_) -> Name (_aName aa) (_aInfo aa)) bs'
              bdg <- TBinding <$> pure bs' <*>
                   (abstract (`elemIndex` ks) <$> runBody bbody bi) <*> pure (BindSchema TyAny) <*> pure bi
              TApp <$> mkVar a q ai <*> pure (as ++ [bdg]) <*> pure li
          _ -> TApp <$> mkVar a q ai <*> mapM run rest <*> pure li

run e@(EObject bs _i) = do
  i <- mkInfo e
  TObject <$> mapM (\(k,v) -> (,) <$> run k <*> run v) bs <*> pure TyAny <*> pure i
run e@(EBinding _ _i) = syntaxError' e "Unexpected binding"
run e@(ESymbol s _i) = TLiteral (LString s) <$> mkInfo e
run e@(ELiteral l _i) = TLiteral l <$> mkInfo e
run e@(EAtom s q t _i) | s `elem` reserved = syntaxError' e $ "Unexpected reserved word: " ++ show s
                    | isNothing t = mkInfo e >>= mkVar s q
                    | otherwise = syntaxError' e "Invalid typed var"
run e@(EList els (Just lty) _i) = mkInfo e >>= \i -> TList <$> mapM run els <*> pure (liftTy i lty) <*> pure i
run e = syntaxError' e "Unexpected expression"
{-# INLINE run #-}

mkVar :: Text -> Maybe Text -> Info -> Compile (Term Name)
mkVar s q i = TVar <$> pure (maybe (Name s i) (\qn -> QName (ModuleName s) qn i) q) <*> pure i
{-# INLINE mkVar #-}

mapNonEmpty :: String -> (a -> Compile b) -> [a] -> Info -> Compile [b]
mapNonEmpty s _ [] i = syntaxError i $ "Empty " ++ s
mapNonEmpty _ act body _ = mapM act body
{-# INLINE mapNonEmpty #-}

runNonEmpty :: String -> [Exp] -> Info -> Compile [Term Name]
runNonEmpty s = mapNonEmpty s run
{-# INLINE runNonEmpty #-}

atomVar :: Exp -> Compile (Arg (Term Name))
atomVar e@(EAtom a Nothing ty _i) = mkInfo e >>= \i -> Arg <$> pure a <*> maybeTyVar i ty <*> pure i
atomVar e = syntaxError' e "Expected unqualified atom"
{-# INLINE atomVar #-}

runBody :: [Exp] -> Info -> Compile (Term Name)
runBody bs i = TList <$> runNonEmpty "body" bs i <*> pure TyAny <*> pure i
{-# INLINE runBody #-}

-}

_compileAccounts :: IO (Either PactError [Term Name])
_compileAccounts = _parseF "examples/accounts/accounts.pact" >>= _compile id

_compile :: (CompileState -> CompileState) -> TF.Result ([Exp Parsed],String) -> IO (Either PactError [Term Name])
_compile _ (TF.Failure f) = putDoc (TF._errDoc f) >> error "Parse failed"
_compile sfun (TF.Success (a,s)) = return $ forM a $ \e ->
  runCompile term (sfun def) (mkStringInfo s <$> e)

-- | run a string as though you were in a module (test deftable, etc)
_compileStrInModule :: String -> IO [Term Name]
_compileStrInModule = _compileStr' (\s -> s { _csModule = Just ("mymodule",hash mempty) })

_compileStr :: String -> IO [Term Name]
_compileStr = _compileStr' id

_compileStr' :: (CompileState -> CompileState) -> String -> IO [Term Name]
_compileStr' sfun code = do
    r <- _compile sfun ((,code) <$> _parseS code)
    case r of Left e -> throwIO $ userError (show e)
              Right t -> return t

_parseS :: String -> TF.Result [Exp Parsed]
_parseS = TF.parseString exprsOnly mempty

_parseF :: FilePath -> IO (TF.Result ([Exp Parsed],String))
_parseF fp = readFile fp >>= \s -> fmap (,s) <$> TF.parseFromFileEx exprsOnly fp

_compileFile :: FilePath -> IO [Term Name]
_compileFile f = do
    p <- _parseF f
    rs <- case p of
            (TF.Failure e) -> putDoc (TF._errDoc e) >> error "Parse failed"
            (TF.Success (es,s)) -> return $ map (compile (mkStringInfo s)) es
    case sequence rs of
      Left e -> throwIO $ userError (show e)
      Right ts -> return ts

_atto :: FilePath -> IO [Term Name]
_atto fp = do
  f <- pack <$> readFile fp
  rs <- case parseExprs f of
    Left s -> throwIO $ userError s
    Right es -> return $ map (compile (mkStringInfo (unpack f))) es
  case sequence rs of
      Left e -> throwIO $ userError (show e)
      Right ts -> return ts
