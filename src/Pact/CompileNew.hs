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
import Control.Applicative
import Data.List
import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Control.Arrow ((&&&))
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
import Data.Monoid ((<>))

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
  , _cStream :: [Exp Info]
  , _cInfo :: Info }
makeLenses ''Cursor


data CompileState = CompileState
  { _csFresh :: Int
  , _csModule :: Maybe (ModuleName,Hash)
  , _csCursor :: Cursor
  }
makeLenses ''CompileState

initState e = CompileState 0 Nothing $ Cursor Nothing [e] (eInfo e)

type Compile a = StateT CompileState (Except [PactError]) a

reserved :: [Text]
reserved = T.words "use module defun defpact step step-with-rollback true false let let* defconst"

runCompile :: (Compile a) -> Exp Info -> Either PactError a
runCompile f e = either (Left . getErr) Right $ runExcept (evalStateT f (initState e))
  where getErr [] = PactError SyntaxError def def "Compile failed"
        getErr as = last as

compile :: MkInfo -> Exp Parsed -> Either PactError (Term Name)
compile mi e = runCompile term (fmap mi e)

compileExps :: Traversable t => MkInfo -> t (Exp Parsed) -> Either PactError (t (Term Name))
compileExps mi exps = sequence $ compile mi <$> exps

syntaxError :: Info -> String -> Compile a
syntaxError i s = throwError [PactError SyntaxError i def (pack s)]

syntaxError' :: String -> Compile a
syntaxError' s = currentInfo >>= \e -> syntaxError (eInfo e) s

expected :: Text -> Compile a
expected s = syntaxError' $ "Expected " ++ (unpack s)

unexpected :: Text -> Compile a
unexpected s = syntaxError' $ "Unexpected " ++ (unpack s)



pop :: Compile (Exp Info)
pop = do
  s <- use (csCursor.cStream)
  case s of
    [] -> unexpected "eof"
    (e:es) -> do
      csCursor.cStream .= es
      csCursor.cInfo .= eInfo e
      return e

try :: Compile a -> Compile a
try act = do
  s <- get
  catchError act (\e -> put s >> throwError e)

popA :: Text -> Prism' (Exp Info) a -> Compile a
popA ty prism = try $ pop >>= \e -> case firstOf prism e of
  Just a -> return a
  Nothing -> expected ty

atom :: Compile (AtomExp Info)
atom = popA "atom" _EAtom

currentInfo :: Compile Info
currentInfo = use (csCursor . cInfo)

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


sep' :: Separator -> Compile (SeparatorExp Info)
sep' s = sep >>= \se@SeparatorExp{..} ->
  if _sepSeparator == s then return se else expected (tShow s)

varAtom :: Compile (Term Name)
varAtom = try $ do
  AtomExp{..} <- atom
  when (_atomAtom `elem` reserved) $ unexpected $ "reserved word: " <> _atomAtom
  n <- case _atomQualifiers of
    [] -> return $ Name _atomAtom _atomInfo
    [q] -> return $ QName (ModuleName q) _atomAtom _atomInfo
    _ -> expected "single qualifier"
  return $ TVar n _atomInfo

bareAtom :: Compile (AtomExp Info)
bareAtom = atom >>= \a@AtomExp{..} -> case _atomQualifiers of
  (_:_) -> expected "unqualified atom"
  [] -> return a

eof :: Compile ()
eof = use (csCursor.cStream) >>= \s -> case s of
  [] -> return ()
  _ -> expected "eof"

enter :: ListExp Info -> Compile (ListExp Info)
enter l@ListExp{..} = do
  csCursor %= \par -> Cursor (Just par) _listList _listInfo
  return l

exit :: Compile ()
exit = do
  child <- use csCursor
  case _cParent child of
    Just p -> csCursor .= p
    Nothing -> unexpected "eof (no parent context)"

listLiteral :: Compile (Term Name)
listLiteral = withList Brackets $ \ListExp{..} -> do
  ls <- (some term <* eof) <|>
        ((term `sepBy` sep' Comma) <* eof)
  return $ TList ls TyAny _listInfo -- TODO capture literal type if any


sepBy :: Compile a -> Compile b -> Compile [a]
sepBy p s = go False where
  go False = (p >>= \t -> (t:) <$> go True) <|> return []
  go True = (s >> go False) <|> return []

objectLiteral :: Compile (Term Name)
objectLiteral = withList Braces $ \ListExp{..} -> do
  ps <- (pair Colon `sepBy` sep' Comma) <* eof
  return $ TObject ps TyAny _listInfo

pair :: Separator -> Compile (Term Name,Term Name)
pair s = do
  key <- term
  val <- sep' s *> term
  return (key,val)

term :: Compile (Term Name)
term =
  (lit >>= \LiteralExp{..} -> return $ TLiteral _litLiteral _litInfo)
  <|> varAtom
  <|> listLiteral
  <|> objectLiteral
  <|> withList Parens (\_ -> specialForm <|> app) -- TODO binding

app :: Compile (Term Name)
app = currentInfo >>= \i -> TApp <$> varAtom <*> (many term <* eof) <*> pure i

specialForm :: Compile (Term Name)
specialForm = bareAtom >>= \AtomExp{..} -> case _atomAtom of
    "use" -> doUse
    "let" -> doLet
    u -> unexpected u

doLet :: Compile (Term Name)
doLet = do
  i <- currentInfo
  bindings <- withList Parens $ \_ -> some $ withList Parens $ \_ -> do
    n <- bareAtom
    ty <- typed <|> freshTyVar
    v <- term
    return (Arg (_atomAtom n) ty (_atomInfo n),v)
  let bNames = map (\(aa,_) -> Name (_aName aa) (_aInfo aa)) bindings
  bs <- abstract (`elemIndex` bNames) <$> doBody
  return $ TBinding bindings bs BindLet i

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
parseListType = withList Brackets $ \_ -> TyList <$> parseType <* eof

parseSchemaType :: Text -> SchemaType -> Compile (Type (Term Name))
parseSchemaType tyRep sty = symbol tyRep >>
  (TySchema sty <$> (parseUserSchemaType <|> pure TyAny))


parseUserSchemaType :: Compile (Type (Term Name))
parseUserSchemaType = withList Braces $ \ListExp{..} -> do
  AtomExp{..} <- bareAtom
  return $ TyUser (return $ Name _atomAtom _listInfo)

symbol :: Text -> Compile Text
symbol s = bareAtom >>= \AtomExp {..} ->
  if s == _atomAtom then return s else expected ("bareword: " <> s)


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

doBody :: Compile (Term Name)
doBody = do
  i <- currentInfo
  bs <- some term <* eof
  return $ TList bs TyAny i

doUse :: Compile (Term Name)
doUse = do
  i <- currentInfo
  modName <- (_atomAtom <$> bareAtom) <|> str <|> expected "bare atom, string, symbol"
  modHash <- optional str
  h <- forM modHash $ \hashStr -> case fromText' hashStr of
    Left e -> syntaxError' $ "bad hash: " ++ e
    Right hv -> return hv
  return $ TUse (ModuleName modName) h i


{-
run :: Exp Info -> Compile (Term Name)
run e = TLiteral <$> literal e <*> mkInfo e <|>
  (list Parens e >>= parseSexp) <|>
  (list Brackets e >>= parseListLiteral) <|>
  (list Braces e >>= parseObject)


literal :: Exp Info -> Compile Literal
literal (ELiteral l _) = return l
literal e = expected e "Literal"

separator :: Separator -> Exp Info -> Compile ()
separator s (ESeparator s' _) | s' == s = return ()
separator s e  = expected e (show s)

notSeparator :: Exp Info -> Compile (Exp Info)
notSeparator s@ESeparator {} = unexpected s (show s)
notSeparator s = return s

list :: ListDelimiter -> Exp Info -> Compile ([Exp Info],Info)
list d (EList l d' i) | d' == d = return (l,i)
list d e = expected e $ case d of
  Parens -> "s-expression"
  Braces -> "braced list"
  Brackets -> "bracketed list"

parseObject :: ([Exp Info],Info) -> Compile (Term Name)
parseObject l@(_,i) = parsePairs Colon l >>= \ps ->
  TObject <$> mapM (\(a,b) -> (,) <$> run a <*> run b) ps <*> pure TyAny <*> pure i


pop :: Info -> [a] -> Compile (a,[a])
pop i [] = syntaxError i "End of stream"
pop _i (a:as) = return (a,as)

parsePairs :: Separator -> ([Exp Info],Info) -> Compile [(Exp Info,Exp Info)]
parsePairs pairSep (ls,i) = go ls False where
  go [] _ = return []
  go es False = do
    (key,es1) <- pop i es
    (del,es2) <- pop i es1
    separator pairSep del
    (val,es3) <- pop i es2
    ((key,val):) <$> (go es3 True)
  go es True = do
    (del,es1) <- pop i es
    separator Comma del
    go es1 False


parseSexp :: ([Exp Info],Info) -> Compile (Term Name)
parseSexp (es,i) = fail "doh"

parseListLiteral :: ([Exp Info],Info) -> Compile (Term Name)
parseListLiteral (ls,i) = TList <$> (doList >>= mapM run) <*> pure TyAny <*> pure i
  where doList = sepBy (separator Comma) ls <|> mapM notSeparator ls

sepBy :: (a -> Compile ()) -> [a] -> Compile [a]
sepBy sep es = fst <$> foldM' go ([],False) es
  where go (rs,False) e = return (e:rs,True)
        go (rs,True) e = sep e >> return (rs,False)

-}
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


currentModule :: Info -> Compile (ModuleName,Hash)
currentModule i = use csModule >>= \m -> case m of
  Just cm -> return cm
  Nothing -> syntaxError i "Must be declared within module"

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
_compileAccounts = _parseF "examples/accounts/accounts.pact" >>= _compile

_compile :: TF.Result ([Exp Parsed],String) -> IO (Either PactError [Term Name])
_compile (TF.Failure f) = putDoc (TF._errDoc f) >> error "Parse failed"
_compile (TF.Success (a,s)) = return $ mapM (compile (mkStringInfo s)) a


_compileStr :: String -> IO [Term Name]
_compileStr code = do
    r <- _compile ((,code) <$> _parseS code)
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
