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

module Pact.Compile
    (
     compile,compileExps
    ,MkInfo,mkEmptyInfo,mkStringInfo,mkTextInfo
    )

where

import qualified Text.Trifecta as TF hiding (expected)
import Control.Applicative hiding (some,many)
import Text.Megaparsec as MP
import Data.List
import Control.Monad
import Control.Monad.State
import Control.Arrow ((&&&),first)
import Prelude hiding (exp)
import Bound
import Text.PrettyPrint.ANSI.Leijen (putDoc)
import Control.Exception hiding (try)
import Data.String
import Control.Lens hiding (prism)
import Data.Maybe
import Data.Default
import Data.Text (Text,pack,unpack)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM

import Pact.Types.ExpParser
import Pact.Types.Exp
import Pact.Parse (exprsOnly,parseExprs)
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

initParseState :: Exp Info -> ParseState CompileState
initParseState e = ParseState e $ CompileState 0 Nothing

data Reserved =
    RBless
  | RDefconst
  | RDefpact
  | RDefschema
  | RDeftable
  | RDefun
  | RFalse
  | RImplements
  | RInterface
  | RLet
  | RLetStar
  | RModule
  | RStep
  | RStepWithRollback
  | RTrue
  | RUse
  deriving (Eq,Enum,Bounded)

instance AsString Reserved where
  asString a = case a of
    RBless -> "bless"
    RDefconst -> "defconst"
    RDefpact -> "defpact"
    RDefschema -> "defschema"
    RDeftable -> "deftable"
    RDefun -> "defun"
    RFalse -> "false"
    RImplements -> "implements"
    RInterface -> "interface"
    RLet -> "let"
    RLetStar -> "let*"
    RModule -> "module"
    RStep -> "step"
    RStepWithRollback -> "step-with-rollback"
    RTrue -> "true"
    RUse -> "use"

instance Show Reserved where show = unpack . asString

reserveds :: HM.HashMap Text Reserved
reserveds = (`foldMap` [minBound .. maxBound]) $ \r -> HM.singleton (asString r) r

reserved :: [Text]
reserved = HM.keys reserveds

reservedAtom :: Compile Reserved
reservedAtom = bareAtom >>= \AtomExp{..} -> case HM.lookup _atomAtom reserveds of
  Nothing -> expected "reserved word"
  Just r -> return r

compile :: MkInfo -> Exp Parsed -> Either PactError (Term Name)
compile mi e = let ei = mi <$> e in runCompile topLevel (initParseState ei) ei

compileExps :: Traversable t => MkInfo -> t (Exp Parsed) -> Either PactError (t (Term Name))
compileExps mi exps = sequence $ compile mi <$> exps

currentModule :: Compile (ModuleName,Hash)
currentModule = use (psUser . csModule) >>= \m -> case m of
  Just cm -> return cm
  Nothing -> context >>= tokenErr' "Must be declared within module"

currentModule' :: Compile ModuleName
currentModule' = fst <$> currentModule

freshTyVar :: Compile (Type (Term Name))
freshTyVar = do
  c <- state (view (psUser . csFresh) &&& over (psUser . csFresh) succ)
  return $ mkTyVar (cToTV c) []

cToTV :: Int -> TypeVarName
cToTV n | n < 26 = fromString [toC n]
        | n <= 26 * 26 = fromString [toC (pred (n `div` 26)), toC (n `mod` 26)]
        | otherwise = fromString $ toC (n `mod` 26) : show ((n - (26 * 26)) `div` 26)
  where toC i = toEnum (fromEnum 'a' + i)


sexp :: (Compile (Term Name)) -> Compile (Term Name)
sexp body = withList' Parens (body <* eof)

specialFormOrApp :: (Reserved -> Compile (Compile (Term Name))) -> Compile (Term Name)
specialFormOrApp forms = sexp (sf <|> app) where
  sf = reservedAtom >>= forms >>= \a -> commit >> a

specialForm :: (Reserved -> Compile (Compile (Term Name))) -> Compile (Term Name)
specialForm forms = sexp $ reservedAtom >>= forms >>= \a -> commit >> a


topLevel :: Compile (Term Name)
topLevel = specialFormOrApp topLevelForm <|> literals <|> varAtom  where
  topLevelForm r = case r of
    RUse -> return useForm
    RLet -> return letForm
    RLetStar -> return letsForm
    RModule -> return moduleForm
    RInterface -> return interface
    _ -> expected "top-level form (use, let[*], module, interface)"


valueLevel :: Compile (Term Name)
valueLevel = literals <|> varAtom <|> specialFormOrApp valueLevelForm where
  valueLevelForm r = case r of
    RLet -> return letForm
    RLetStar -> return letsForm
    _ -> expected "value level form (let, let*)"

moduleLevel :: Compile (Term Name)
moduleLevel = specialForm $ \r -> case r of
    RUse -> return useForm
    RDefconst -> return defconst
    RBless -> return bless
    RDeftable -> return deftable
    RDefschema -> return defschema
    RDefun -> return defun
    RDefpact -> return defpact
    RImplements -> return implements
    _ -> expected "module level form (use, def..., special form)"


literals :: Compile (Term Name)
literals =
  literal
  <|> listLiteral
  <|> objectLiteral


-- | User-available atoms (excluding reserved words).
userAtom :: Compile (AtomExp Info)
userAtom = do
  a@AtomExp{..} <- bareAtom
  when (_atomAtom `elem` reserved) $ unexpected' "reserved word"
  pure a



app :: Compile (Term Name)
app = do
  v <- varAtom
  body <- many (valueLevel <|> bindingForm)
  TApp v body <$> contextInfo

-- | Bindings (`{ "column" := binding }`) do not syntactically scope the
-- following body form as a sexp, instead letting the body contents
-- simply follow, showing up as more args to the containing app. Thus, once a
-- binding is encountered, all following terms are subsumed into the
-- binding body, and bound/abstracted etc.
bindingForm :: Compile (Term Name)
bindingForm = do
  let pair = do
        col <- valueLevel
        a <- sep ColonEquals *> arg
        return (a,col)
  (bindings,bi) <- withList' Braces $
    (,) <$> pair `sepBy1` sep Comma <*> contextInfo
  TBinding bindings <$> abstractBody valueLevel (map fst bindings) <*>
    pure (BindSchema TyAny) <*> pure bi

varAtom :: Compile (Term Name)
varAtom = do
  AtomExp{..} <- atom
  when (_atomAtom `elem` reserved) $ unexpected' "reserved word"
  n <- case _atomQualifiers of
    [] -> return $ Name _atomAtom _atomInfo
    [q] -> do
      when (q `elem` reserved) $ unexpected' "reserved word"
      return $ QName (ModuleName q) _atomAtom _atomInfo
    _ -> expected "single qualifier"
  commit
  return $ TVar n _atomInfo

listLiteral :: Compile (Term Name)
listLiteral = withList Brackets $ \ListExp{..} -> do
  ls <- case _listList of
    _ : CommaExp : _ -> valueLevel `sepBy` sep Comma
    _                -> many valueLevel
  let lty = case nub (map typeof ls) of
              [Right ty] -> ty
              _ -> TyAny
  pure $ TList ls lty _listInfo

objectLiteral :: Compile (Term Name)
objectLiteral = withList Braces $ \ListExp{..} -> do
  let pair = do
        key <- valueLevel
        val <- sep Colon *> valueLevel
        return (key,val)
  ps <- pair `sepBy` sep Comma
  return $ TObject ps TyAny _listInfo

literal :: Compile (Term Name)
literal = lit >>= \LiteralExp{..} ->
  commit >> return (TLiteral _litLiteral _litInfo)


deftable :: Compile (Term Name)
deftable = do
  (mn,mh) <- currentModule
  AtomExp{..} <- userAtom
  ty <- optional (typed >>= \t -> case t of
                     TyUser {} -> return t
                     _ -> expected "user type")
  m <- meta ModelNotAllowed
  TTable (TableName _atomAtom) mn mh
    (fromMaybe TyAny ty) m <$> contextInfo


bless :: Compile (Term Name)
bless = TBless <$> hash' <*> contextInfo

defconst :: Compile (Term Name)
defconst = do
  modName <- currentModule'
  a <- arg
  v <- valueLevel

  m <- meta ModelNotAllowed
  TConst a modName (CVRaw v) m <$> contextInfo

data ModelAllowed
  = ModelAllowed
  | ModelNotAllowed

data AtPair = DocPair Text | ModelPair [Exp Info] deriving (Eq,Ord)

meta :: ModelAllowed -> Compile Meta
meta modelAllowed = atPairs <|> try docStr <|> return def
  where
    docStr = Meta <$> (Just <$> str) <*> pure []
    docPair = symbol "@doc" >> (DocPair <$> str)
    modelPair = do
      symbol "@model"
      (ListExp exps _ _i, _) <- list' Brackets
      pure (ModelPair exps)
    whenModelAllowed a = case modelAllowed of
      ModelAllowed -> a
      ModelNotAllowed -> unexpected' "@model not allowed in this declaration"
    atPairs = do
      ps <- sort <$> (some (docPair <|> modelPair))
      case ps of
        [DocPair doc] -> return (Meta (Just doc) [])
        [ModelPair es] -> whenModelAllowed $ return (Meta Nothing es)
        [DocPair doc, ModelPair es] -> whenModelAllowed $ return (Meta (Just doc) es)
        _ -> expected $ case modelAllowed of
          ModelNotAllowed -> "@doc declaration"
          ModelAllowed -> "@doc and/or @model declarations"

defschema :: Compile (Term Name)
defschema = do
  modName <- currentModule'
  tn <- _atomAtom <$> userAtom
  m <- meta ModelAllowed
  fields <- many arg
  TSchema (TypeName tn) modName m fields <$> contextInfo

defun :: Compile (Term Name)
defun = do
  modName <- currentModule'
  (defname,returnTy) <- first _atomAtom <$> typedAtom
  args <- withList' Parens $ many arg
  m <- meta ModelAllowed
  TDef defname modName Defun (FunType args returnTy)
    <$> abstractBody valueLevel args <*> pure m <*> contextInfo

defpact :: Compile (Term Name)
defpact = do
  modName <- currentModule'
  (defname,returnTy) <- first _atomAtom <$> typedAtom
  args <- withList' Parens $ many arg
  m <- meta ModelAllowed
  (body,bi) <- bodyForm' $ specialForm $ \r -> case r of
    RStep -> return step
    RStepWithRollback -> return stepWithRollback
    _ -> expected "step or step-with-rollback"
  TDef defname modName Defpact (FunType args returnTy)
    (abstractBody' args (TList body TyAny bi)) m <$> contextInfo

moduleForm :: Compile (Term Name)
moduleForm = do
  modName' <- _atomAtom <$> userAtom
  keyset <- str
  m <- meta ModelAllowed
  use (psUser . csModule) >>= \cm -> case cm of
    Just {} -> syntaxError "Invalid nested module or interface"
    Nothing -> return ()
  i <- contextInfo
  let code = case i of
        Info Nothing -> "<code unavailable>"
        Info (Just (c,_)) -> c
      modName = ModuleName modName'
      modHash = hash $ encodeUtf8 $ _unCode code
  (psUser . csModule) .= Just (modName,modHash)
  (bd,bi) <- bodyForm' moduleLevel
  blessed <- fmap (HS.fromList . concat) $ forM bd $ \d -> case d of
    TDef {} -> return []
    TNative {} -> return []
    TConst {} -> return []
    TSchema {} -> return []
    TTable {} -> return []
    TUse {} -> return []
    TBless {..} -> return [_tBlessed]
    TImplements{} -> return []
    t -> syntaxError $ "Invalid declaration in module scope: " ++ abbrev t
  let interfaces = bd >>= \d -> case d of
        TImplements{..} -> [_tInterfaceName]
        _ -> []
  return $ TModule
    (Module modName (KeySetName keyset) m code modHash blessed interfaces)
    (abstract (const Nothing) (TList bd TyAny bi)) i

implements :: Compile (Term Name)
implements = do
  modName <- currentModule'
  ifName <- (ModuleName . _atomAtom) <$> bareAtom
  info <- contextInfo
  return $ TImplements ifName modName info

interface :: Compile (Term Name)
interface = do
  iname' <- _atomAtom <$> bareAtom
  m <- meta ModelAllowed
  use (psUser . csModule) >>= \ci -> case ci of
    Just {} -> syntaxError "invalid nested interface or module"
    Nothing -> return ()
  info <- contextInfo
  let code = case info of
        Info Nothing -> "<code unavailable>"
        Info (Just (c,_)) -> c
      iname = ModuleName iname'
      ihash = hash $ encodeUtf8 (_unCode code)
  (psUser . csModule) .= Just (iname, ihash)
  bd <- bodyForm $ specialForm $ \r -> case r of
        RDefun -> return emptyDef
        RDefconst -> return defconst
        RUse -> return useForm
        t -> syntaxError $ "Invalid interface declaration: " ++ show (asString t)
  return $ TModule
    (Interface iname code m)
    (abstract (const Nothing) bd) info

emptyDef :: Compile (Term Name)
emptyDef = do
  modName <- currentModule'
  (defName, returnTy) <- first _atomAtom <$> typedAtom
  args <- withList' Parens $ many arg
  m <- meta ModelAllowed
  info <- contextInfo
  return $
    TDef defName modName Defun
    (FunType args returnTy) (abstract (const Nothing) (TList [] TyAny info)) m info


step :: Compile (Term Name)
step = do
  cont <- try (TStep <$> (Just <$> valueLevel) <*> valueLevel) <|>
          (TStep Nothing <$> valueLevel)
  cont <$> pure Nothing <*> contextInfo

stepWithRollback :: Compile (Term Name)
stepWithRollback = do
  try (TStep <$> (Just <$> valueLevel) <*> valueLevel <*>
         (Just <$> valueLevel) <*> contextInfo)
  <|> (TStep Nothing <$> valueLevel <*> (Just <$> valueLevel) <*> contextInfo)



letBindings :: Compile [(Arg (Term Name),Term Name)]
letBindings = withList' Parens $
              some $ withList' Parens $
              (,) <$> arg <*> valueLevel

abstractBody :: Compile (Term Name) -> [Arg (Term Name)] -> Compile (Scope Int Term Name)
abstractBody term args = abstractBody' args <$> bodyForm term

abstractBody' :: [Arg (Term Name)] -> Term Name -> Scope Int Term Name
abstractBody' args = abstract (`elemIndex` bNames)
  where bNames = map arg2Name args


letForm :: Compile (Term Name)
letForm = do
  bindings <- letBindings
  TBinding bindings <$> abstractBody valueLevel (map fst bindings) <*>
    pure BindLet <*> contextInfo

-- | let* is a macro to nest lets for referencing previous
-- bindings.
letsForm :: Compile (Term Name)
letsForm = do
  bindings <- letBindings
  let nest (binding:rest) = do
        let bName = [arg2Name (fst binding)]
        scope <- abstract (`elemIndex` bName) <$> case rest of
          [] -> bodyForm valueLevel
          _ -> do
            rest' <- nest rest
            pure $ TList [rest'] TyAny def
        TBinding [binding] scope BindLet <$> contextInfo
      nest [] =  syntaxError "letsForm: invalid state (bug)"
  nest bindings

useForm :: Compile (Term Name)
useForm = do
  modName <- (_atomAtom <$> userAtom) <|> str <|> expected "bare atom, string, symbol"
  TUse (ModuleName modName) <$> optional hash' <*> contextInfo

hash' :: Compile Hash
hash' = str >>= \s -> case fromText' s of
  Right h -> return h
  Left e -> syntaxError $ "bad hash: " ++ e

typedAtom :: Compile (AtomExp Info,Type (Term Name))
typedAtom = (,) <$> userAtom <*> (typed <|> freshTyVar)

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
  AtomExp{..} <- userAtom
  return $ TyUser (return $ Name _atomAtom _listInfo)

bodyForm :: Compile (Term Name) -> Compile (Term Name)
bodyForm term = do
  (bs,i) <- bodyForm' term
  return $ TList bs TyAny i

bodyForm' :: Compile (Term Name) -> Compile ([Term Name],Info)
bodyForm' term = (,) <$> some term <*> contextInfo

_compileAccounts :: IO (Either PactError [Term Name])
_compileAccounts = _compileF "examples/accounts/accounts.pact"

_compileF :: FilePath -> IO (Either PactError [Term Name])
_compileF f = _parseF f >>= _compile id

_compile :: (ParseState CompileState -> ParseState CompileState) ->
            TF.Result ([Exp Parsed],String) -> IO (Either PactError [Term Name])
_compile _ (TF.Failure f) = putDoc (TF._errDoc f) >> error "Parse failed"
_compile sfun (TF.Success (a,s)) = return $ forM a $ \e ->
  let ei = mkStringInfo s <$> e
  in runCompile topLevel (sfun (initParseState ei)) ei

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
