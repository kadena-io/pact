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
{-# LANGUAGE NamedFieldPuns #-}
-- |
-- Module      :  Pact.Compile
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Compiler from 'Exp' -> 'Term Name'
--

module Pact.Compile
  ( compile
  , compileExps
  , MkInfo
  , mkEmptyInfo
  , mkStringInfo
  , mkTextInfo
  ) where

import qualified Text.Trifecta as TF hiding (expected)
import Control.Applicative hiding (some, many)
import Text.Megaparsec as MP
import Data.List
import Control.Monad
import Control.Monad.State
import Control.Arrow ((&&&), first)
import Prelude hiding (exp)
import Bound
import Text.PrettyPrint.ANSI.Leijen (putDoc)
import Control.Exception hiding (try)
import Data.String
import Control.Lens hiding (prism)
import Data.Maybe
import Data.Default
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.HashSet as HS

import Pact.Types.ExpParser
import Pact.Types.Crypto
import Pact.Types.Exp
import Pact.Parse (exprsOnly, parseExprs)
import Pact.Types.Hash
import Pact.Types.Term
import Pact.Types.Util
import Pact.Types.Info
import Pact.Types.Type
import Pact.Types.Runtime (PactError)


data CompileState = CompileState
  { _csFresh :: Int
  , _csModule :: Maybe (ModuleName,Hash)
  }
makeLenses ''CompileState

type Compile a = ExpParse CompileState a

initParseState
  :: Exp Info
  -> ParseState CompileState
initParseState e = ParseState e $ CompileState 0 Nothing


reserved :: [Text]
reserved =
  [ "use"
  , "module"
  , "defun"
  , "defpact"
  , "step"
  , "step-with-rollback"
  , "true"
  , "false"
  , "let"
  , "let*"
  , "defconst"
  ]

compile
  :: MkInfo
  -> Exp Parsed
  -> Either PactError (Term Name)
compile mi e =
  let ei = mi <$> e
  in runCompile term (initParseState ei) ei

compileExps
  :: Traversable t
  => MkInfo
  -> t (Exp Parsed)
  -> Either PactError (t (Term Name))
compileExps = traverse . compile

currentModule :: Compile (ModuleName,Hash)
currentModule =
  use (psUser . csModule) >>= go
  where
    go :: Maybe (ModuleName, Hash) -> ExpParse CompileState (ModuleName, Hash) 
    go m =
      case m of
        Nothing -> context >>= tokenErr' "Must be declared within module"
        Just a -> return a

currentModule' :: Compile ModuleName
currentModule' =
  fst <$> currentModule

freshTyVar :: Compile (Type (Term Name))
freshTyVar = do
  c <- state (view (psUser . csFresh) &&& over (psUser . csFresh) succ)
  return $ mkTyVar (cToTV c) []

cToTV :: Int -> TypeVarName
cToTV n | n < 26 = fromString [toC n]
        | n <= 26 * 26 = fromString [toC (pred (n `div` 26)), toC (n `mod` 26)]
        | otherwise = fromString $ toC (n `mod` 26) : show ((n - (26 * 26)) `div` 26)
  where toC i = toEnum (fromEnum 'a' + i)


term :: Compile (Term Name)
term =
  literal
  <|> varAtom
  <|> withList' Parens (specialForm <|> app)
  <|> listLiteral
  <|> objectLiteral

specialForm :: Compile (Term Name)
specialForm =
  bareAtom >>= go
    where
      go :: AtomExp a -> Compile (Term Name)
      go AtomExp{ _atomAtom = _atom } =
        case _atom of
          "use" -> commit >> useForm
          "let" -> commit >> letForm
          "let*" -> commit >> letsForm
          "defconst" -> commit >> defconst
          "step" -> commit >> step
          "step-with-rollback" -> commit >> stepWithRollback
          "bless" -> commit >> bless
          "deftable" -> commit >> deftable
          "defschema" -> commit >> defschema
          "defun" -> commit >> defun
          "defpact" -> commit >> defpact
          "module" -> commit >> moduleForm
          _ -> expected "special form"


app :: Compile (Term Name)
app = do
  termName <- varAtom
  body <- many (term <|> bindingForm)
  gamma <- contextInfo 
  return $ TApp termName body gamma 

-- | Bindings (`{ "column" := binding }`) do not syntactically scope the
-- following body form as a sexp, instead letting the body contents
-- simply follow, showing up as more args to the containing app. Thus, once a
-- binding is encountered, all following terms are subsumed into the
-- binding body, and bound/abstracted etc.
bindingForm :: Compile (Term Name)
bindingForm = braced >>= uncurry subsumeBody
  where
    pair :: Compile (Arg (Term Name), Term Name)
    pair = do
      col <- term
      a <- sep ColonEquals *> arg
      return (a, col)
      
    braced :: Compile ([(Arg (Term Name), Term Name)], Info)
    braced =  withList' Braces $
      (,) <$> (pair `sepBy1` sep Comma) <*> contextInfo
      
    subsumeBody :: [(Arg (Term Name), Term Name)] -> Info -> Compile (Term Name)
    subsumeBody bindings info =
          TBinding bindings
        <$> abstractBody (map fst bindings)
        <*> return (BindSchema TyAny)
        <*> return info
    
varAtom :: Compile (Term Name)
varAtom = do
  _atom <- atom
  ensureUnreserved _atom
  n <- qualify _atom
  commit *> (return . TVar n . _atomInfo $ _atom)
  where
    qualify :: AtomExp Info -> Compile Name
    qualify AtomExp{..} =
      case _atomQualifiers of 
        [] -> return $ Name _atomAtom _atomInfo
        [q] -> return $ QName (ModuleName q) _atomAtom _atomInfo
        _ -> expected "single qualifier"
        
    ensureUnreserved :: AtomExp Info -> Compile ()
    ensureUnreserved AtomExp{..} =
      when (_atomAtom `elem` reserved) (unexpected' "reserved word")

listLiteral :: Compile (Term Name)
listLiteral = withList Brackets go 
  where 
    go :: ListExp Info -> Compile (Term Name)
    go ListExp{..} = do
      tns <- parseSeparator _listList
      return $ TList tns (listType tns) _listInfo
   
    parseSeparator :: [Exp Info] -> Compile [Term Name]
    parseSeparator (_:CommaExp:_) = term `sepBy` sep Comma
    parseSeparator _ = many term

    listType :: [Term Name] -> Type (Term Name) 
    listType as =
      case nub . map typeof $ as of
        [Right ty] -> ty
        _ -> TyAny
        
objectLiteral :: Compile (Term Name)
objectLiteral = withList Braces go 
  where
    go :: ListExp Info -> Compile (Term Name)
    go ListExp{..} = do
      ps <- pair `sepBy` sep Comma
      return $ TObject ps TyAny _listInfo

    pair :: Compile (Term Name, Term Name)
    pair = do
      key <- term
      value <- sep Colon *> term
      return (key, value)
      
literal :: Compile (Term Name)
literal = lit >>= go 
  where
    go :: LiteralExp Info -> Compile (Term Name)
    go LiteralExp{..} =
      commit >> return (TLiteral _litLiteral _litInfo)

deftable :: Compile (Term Name)
deftable = do 
  cm <- currentModule
  ba <- bareAtom
  m  <-  meta
  ty <-  optional (typed >>= validateType)
  gamma <- contextInfo
  checkTableFailure m
  go cm ba m ty gamma
  where
    go
      :: (ModuleName, Hash)
      -> AtomExp Info
      -> Meta
      -> Maybe (Type (Term Name))
      -> Info
      -> Compile (Term Name)
    go (mn, mh) AtomExp{..} m ty gamma = 
      return $ TTable (TableName _atomAtom) mn mh (fromMaybe TyAny ty) m gamma

    validateType :: Type (Term Name) -> Compile (Type (Term Name))
    validateType t =
      case t of
        TyUser {} -> return t
        _ -> expected "user type"

    checkTableFailure :: Meta -> Compile ()
    checkTableFailure m = 
      when (isJust (m ^. mModel)) $ syntaxError "@model not permitted on tables"
      
bless :: Compile (Term Name)
bless = TBless <$> hash' <*> contextInfo

defconst :: Compile (Term Name)
defconst = do
  modName <- currentModule'
  a <- arg
  v <- term
  m <- meta
  gamma <- contextInfo
  when (isJust (m ^. mModel)) $ syntaxError "@model not permitted on defconst"
  return $ TConst a modName (CVRaw v) m gamma

meta :: Compile Meta
meta =  atPairs
  <|> try docStr
  <|> return def
  where
    docStr :: Compile Meta
    docStr = Meta <$> (Just <$> str) <*> pure Nothing
    docPair :: Compile Text
    docPair = symbol "@doc" >> str
    modelPair :: Compile (Exp Info)
    modelPair = symbol "@model" >> anyExp
    atPairs :: Compile Meta
    atPairs = do
      doc <- optional (try docPair)
      model <- optional (try modelPair)
      case (doc,model) of
        (Nothing,Nothing) -> expected "@doc or @model declarations"
        _ -> return $ Meta doc model

defschema :: Compile (Term Name)
defschema = go
  <$> currentModule'
  <*> (_atomAtom <$> bareAtom)
  <*> meta
  <*> many arg
  <*> contextInfo
  where
    go
      :: ModuleName
      -> Text
      -> Meta
      -> [Arg (Term Name)]
      -> Info
      -> Term Name
    go mn tn m fields gamma =
      TSchema (TypeName tn) mn m fields gamma

defun :: Compile (Term Name)
defun = do
  modName             <- currentModule'
  (defname, returnTy) <- first _atomAtom <$> typedAtom
  args                <- withList' Parens $ many arg
  m                   <- meta
  scopedArgs          <- abstractBody args
  gamma               <- contextInfo
  return $
    TDef defname modName Defun (FunType args returnTy) scopedArgs m gamma
  
defpact :: Compile (Term Name)
defpact = do
  modName <- currentModule'
  (defname,returnTy) <- first _atomAtom <$> typedAtom
  args <- withList' Parens $ many arg
  m <- meta
  (body,bi) <- bodyForm'
  gamma <- contextInfo
  forM_ body checkStep
  return $
    TDef defname modName Defpact (FunType args returnTy)
      (abstractBody' args (TList body TyAny bi)) m gamma
  where
    checkStep :: Term Name -> Compile ()
    checkStep TStep{} = return ()
    checkStep _ = expected "step or step-with-rollback"
      
moduleForm :: Compile (Term Name)
moduleForm = do
  modName' <- _atomAtom <$> bareAtom
  keyset <- str
  m <- meta
  use (psUser . csModule) >>= \cm -> case cm of
    Just {} -> syntaxError "Invalid nested module"
    Nothing -> return ()
  i <- contextInfo
  let code = case i of
        Info Nothing -> "<code unavailable>"
        Info (Just (c,_)) -> c
      modName = ModuleName modName'
      modHash = hash $ encodeUtf8 $ _unCode code
  (psUser . csModule) .= Just (modName,modHash)
  (bd,bi) <- bodyForm'
  eof
  blessed <- fmap (HS.fromList . concat) $ forM bd $ \d -> case d of
    TDef {} -> return []
    TNative {} -> return []
    TConst {} -> return []
    TSchema {} -> return []
    TTable {} -> return []
    TUse {} -> return []
    TBless {..} -> return [_tBlessed]
    _ -> syntaxError "Only defun, defpact, defconst, deftable, use, bless allowed in module"
  return $ TModule
    (Module modName (KeySetName keyset) m code modHash blessed)
    (abstract (const Nothing) (TList bd TyAny bi)) i

step :: Compile (Term Name)
step = do
  cont <- try (TStep <$> (Just <$> term) <*> term) <|>
          (TStep Nothing <$> term)
  cont <$> pure Nothing <*> contextInfo

stepWithRollback :: Compile (Term Name)
stepWithRollback = do
  try (TStep <$> (Just <$> term) <*> term <*> (Just <$> term) <*> contextInfo) <|>
      (TStep Nothing <$> term <*> (Just <$> term) <*> contextInfo)



letBindings :: Compile [(Arg (Term Name),Term Name)]
letBindings = withList' Parens $
              some $ withList' Parens $
              (,) <$> arg <*> term

abstractBody :: [Arg (Term Name)] -> Compile (Scope Int Term Name)
abstractBody args = abstractBody' args <$> bodyForm

abstractBody' :: [Arg (Term Name)] -> Term Name -> Scope Int Term Name
abstractBody' args = abstract (`elemIndex` bNames)
  where bNames = map arg2Name args


letForm :: Compile (Term Name)
letForm = do
  bindings <- letBindings
  TBinding bindings <$> abstractBody (map fst bindings) <*>
    pure BindLet <*> contextInfo

-- | let* is a macro to nest lets for referencing previous
-- bindings.
letsForm :: Compile (Term Name)
letsForm = do
  bindings <- letBindings
  let nest (binding:rest) = do
        let bName = [arg2Name (fst binding)]
        scope <- abstract (`elemIndex` bName) <$> case rest of
          [] -> bodyForm
          _ -> do
            rest' <- nest rest
            pure $ TList [rest'] TyAny def
        TBinding [binding] scope BindLet <$> contextInfo
      nest [] =  syntaxError "letsForm: invalid state (bug)"
  nest bindings

useForm :: Compile (Term Name)
useForm = do
  modName <- (_atomAtom <$> bareAtom) <|> str <|> expected "bare atom, string, symbol"
  TUse (ModuleName modName) <$> optional hash' <*> contextInfo

hash' :: Compile Hash
hash' = str >>= \s -> case fromText' s of
  Right h -> return h
  Left e -> syntaxError $ "bad hash: " ++ e

typedAtom :: Compile (AtomExp Info,Type (Term Name))
typedAtom = (,) <$> bareAtom <*> (typed <|> freshTyVar)

arg :: Compile (Arg (Term Name))
arg = typedAtom >>= \(AtomExp{..},ty) ->
  return $ Arg _atomAtom ty _atomInfo

arg2Name :: Arg n -> Name
arg2Name Arg{..} = Name _aName _aInfo


typed :: Compile (Type (Term Name))
typed = sep Colon *> parseType

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
parseListType = withList' Brackets $ TyList <$> parseType

parseSchemaType :: Text -> SchemaType -> Compile (Type (Term Name))
parseSchemaType tyRep sty = symbol tyRep >>
  (TySchema sty <$> (parseUserSchemaType <|> pure TyAny))


parseUserSchemaType :: Compile (Type (Term Name))
parseUserSchemaType = withList Braces $ \ListExp{..} -> do
  AtomExp{..} <- bareAtom
  return $ TyUser (return $ Name _atomAtom _listInfo)

bodyForm :: Compile (Term Name)
bodyForm = do
  (bs,i) <- bodyForm'
  return $ TList bs TyAny i

bodyForm' :: Compile ([Term Name],Info)
bodyForm' = (,) <$> some term <*> contextInfo




_compileAccounts :: IO (Either PactError [Term Name])
_compileAccounts = _parseF "examples/accounts/accounts.pact" >>= _compile id

_compile :: (ParseState CompileState -> ParseState CompileState) ->
            TF.Result ([Exp Parsed],String) -> IO (Either PactError [Term Name])
_compile _ (TF.Failure f) = putDoc (TF._errDoc f) >> error "Parse failed"
_compile sfun (TF.Success (a,s)) = return $ forM a $ \e ->
  let ei = mkStringInfo s <$> e
  in runCompile term (sfun (initParseState ei)) ei

-- | run a string as though you were in a module (test deftable, etc)
_compileStrInModule :: String -> IO [Term Name]
_compileStrInModule = _compileStr' (set (psUser . csModule) (Just ("mymodule",hash mempty)))

_compileStr :: String -> IO [Term Name]
_compileStr = _compileStr' id

_compileStr' :: (ParseState CompileState -> ParseState CompileState) -> String -> IO [Term Name]
_compileStr' sfun code = do
    r <- _compile sfun ((,code) <$> _parseS code)
    case r of Left e -> throwIO $ userError (show e)
              Right t -> return t

_parseS :: String -> TF.Result [Exp Parsed]
_parseS = TF.parseString exprsOnly mempty
OA
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

_testCToTV :: Bool
_testCToTV = nub vs == vs where vs = take (26*26*26) $ map cToTV [0..]
