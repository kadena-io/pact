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
  ( compile,compileExps
  , MkInfo,mkEmptyInfo,mkStringInfo,mkTextInfo
  , Reserved(..)
  ) where

import Prelude hiding (exp)

import Bound

import Control.Applicative hiding (some,many)
import Control.Arrow ((&&&),first)
import Control.Exception hiding (try)
import Control.Lens hiding (prism)
import Control.Monad
import Control.Monad.State

import qualified Data.ByteString as BS
import Data.Default
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import Data.String
import Data.Text (Text,unpack, pack)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.Vector as V

import Text.Megaparsec as MP
import qualified Text.Trifecta as TF hiding (expected)

import Pact.Parse (exprsOnly,parseExprs)
import Pact.Types.Exp
import Pact.Types.ExpParser
import Pact.Types.Hash
import Pact.Types.Info
import Pact.Types.Pretty hiding (nest, sep)
import Pact.Types.PactError
import Pact.Types.Term
import Pact.Types.Type
import Pact.Types.Util


data ModuleState = ModuleState
  { _msName :: ModuleName
  , _msHash :: ModuleHash
  , _msBlessed :: [ModuleHash]
  , _msImplements :: [ModuleName]
  , _msImports :: [Use]
  }
makeLenses ''ModuleState

initModuleState :: ModuleName -> ModuleHash -> ModuleState
initModuleState n h = ModuleState n h def def def

data CompileState = CompileState
  { _csFresh :: Int
  , _csModule :: Maybe ModuleState
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
  | RDefcap
  | RFalse
  | RImplements
  | RInterface
  | RLet
  | RLetStar
  | RLambda
  | RModule
  | RStep
  | RStepWithRollback
  | RTrue
  | RUse
  | RWithCapability
  | RCond
  deriving (Eq,Enum,Bounded)

instance AsString Reserved where
  asString a = case a of
    RBless -> "bless"
    RDefconst -> "defconst"
    RDefpact -> "defpact"
    RDefschema -> "defschema"
    RDeftable -> "deftable"
    RDefun -> "defun"
    RDefcap -> "defcap"
    RFalse -> "false"
    RImplements -> "implements"
    RInterface -> "interface"
    RLet -> "let"
    RLetStar -> "let*"
    RLambda -> "lambda"
    RModule -> "module"
    RStep -> "step"
    RStepWithRollback -> "step-with-rollback"
    RTrue -> "true"
    RUse -> "use"
    RCond -> "cond"
    RWithCapability -> "with-capability"

instance Show Reserved where show = unpack . asString

checkReserved :: Text -> Compile ()
checkReserved t = when (t `elem` reserved) $ unexpected' "reserved word"

reserveds :: HM.HashMap Text Reserved
reserveds = (`foldMap` [minBound .. maxBound]) $ \r -> HM.singleton (asString r) r

reserved :: [Text]
reserved = HM.keys reserveds

reservedAtom :: Compile Reserved
reservedAtom = bareAtom >>= \AtomExp{..} -> case HM.lookup _atomAtom reserveds of
  Nothing -> expected "reserved word"
  Just r -> commit >> return r

compile :: ParseEnv -> MkInfo -> Exp Parsed -> Either PactError (Term Name)
compile pe mi e = runCompile pe topLevel (initParseState ei) ei
 where
  ei = mi <$> e

compileExps :: Traversable t => ParseEnv -> MkInfo -> t (Exp Parsed) -> Either PactError (t (Term Name))
compileExps pe mi exps = traverse (compile pe mi) exps

moduleState :: Compile ModuleState
moduleState = use (psUser . csModule) >>= \case
  Just m' -> return m'
  Nothing -> context >>= tokenErr' "Must be declared within module"

overModuleState :: Lens' ModuleState a -> (a -> a) -> Compile ()
overModuleState l f = do
  m <- moduleState
  psUser . csModule .= Just (over l f m)

withModuleState :: ModuleState -> Compile a -> Compile (a,ModuleState)
withModuleState ms0 act = do
  psUser . csModule .= Just ms0
  a <- act
  ms1 <- state $ \s -> (view (psUser . csModule) s, set (psUser . csModule) Nothing s)
  case ms1 of
    Nothing -> syntaxError "Invalid internal state, module data not found"
    Just ms -> return (a,ms)


currentModuleName :: Compile ModuleName
currentModuleName = _msName <$> moduleState

-- | Construct a potentially namespaced module name from qualified atom
-- also returns TVar for use with modref types
qualifiedModuleName :: Compile (ModuleName,Term Name)
qualifiedModuleName = do
  AtomExp{..} <- atom
  checkReserved _atomAtom
  case _atomQualifiers of
    []  -> return (ModuleName _atomAtom Nothing
                  ,TVar (Name (BareName _atomAtom _atomInfo)) _atomInfo)
    [n] -> do
      checkReserved n
      return (ModuleName _atomAtom (Just . NamespaceName $ n)
             ,TVar (QName (QualifiedName (ModuleName n Nothing) _atomAtom _atomInfo)) _atomInfo)
    _   -> expected "qualified module name reference"

freshTyVar :: Compile (Type (Term Name))
freshTyVar = do
  c <- state (view (psUser . csFresh) &&& over (psUser . csFresh) succ)
  return $ mkTyVar (cToTV c) []

freshNameRaw :: Compile Text
freshNameRaw = do
  c <- state (view (psUser . csFresh) &&& over (psUser . csFresh) succ)
  -- does not collide bc backtick is
  -- not a legal symbol in pact
  pure $ "`af" <> pack (show c)

cToTV :: Int -> TypeVarName
cToTV n | n < 26 = fromString [toC n]
        | n <= 26 * 26 = fromString [toC (pred (n `div` 26)), toC (n `mod` 26)]
        | otherwise = fromString $ toC (n `mod` 26) : show ((n - (26 * 26)) `div` 26)
  where toC i = toEnum (fromEnum 'a' + i)


sexp :: Compile a -> Compile a
sexp body = withList' Parens (body <* eof)

specialFormOrApp :: (Reserved -> Compile (Compile (Term Name))) -> Compile (Term Name)
specialFormOrApp forms = sexp (sf <|> app) where
  sf = reservedAtom >>= forms >>= \a -> commit >> a

specialForm :: (Reserved -> Compile (Compile a)) -> Compile a
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
    RWithCapability -> return withCapability
    RLambda -> return lam
    RCond -> return condForm
    _ -> expected "value level form (let, let*, with-capability, cond)"

moduleLevel :: Compile [Term Name]
moduleLevel = specialForm $ \case
    RUse -> returnl useForm
    RDefconst -> returnl defconst
    RBless -> return (bless >> return [])
    RDeftable -> returnl deftable
    RDefschema -> returnl defschema
    RDefun -> returnl $ defunOrCap Defun
    RDefcap -> returnl $ defunOrCap Defcap
    RDefpact -> returnl defpact
    RImplements -> return $ implements >> return []
    _ -> expected "module level form (use, def..., special form)"
    where returnl a = return (pure <$> a)


literals :: Compile (Term Name)
literals =
  literal
  <|> listLiteral
  <|> tryMay objectLiteral
  where
    tryMay a = narrowTry a (try a)

-- | Bare atoms (excluding reserved words).
userAtom :: Compile (AtomExp Info)
userAtom = do
  a@AtomExp{..} <- bareAtom
  checkReserved _atomAtom
  pure a

-- | Bare atom as an unqualified TVar.
userVar :: Compile (Term Name)
userVar = userAtom >>= \AtomExp{..} ->
  return $ TVar (Name $ BareName _atomAtom _atomInfo) _atomInfo

app :: Compile (Term Name)
app = do
  v <- varAtom
  args <- many (valueLevel <|> bindingForm)
  i <- contextInfo
  return $ TApp (App v args i) i

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
        return $ BindPair a col
  (bindings,bi) <- withList' Braces $
    (,) <$> pair `sepBy1` sep Comma <*> contextInfo
  TBinding bindings <$> abstractBody valueLevel (map _bpArg bindings) <*>
    pure (BindSchema TyAny) <*> pure bi

varAtom :: Compile (Term Name)
varAtom = do
  AtomExp{..} <- atom
  checkReserved _atomAtom
  let var n = TVar n _atomInfo <$ commit
  case (_atomQualifiers,_atomDynamic) of
    ([],_) -> var $ Name $ BareName _atomAtom _atomInfo
    ([q],False) -> do
      checkReserved q
      var $ QName $ QualifiedName (ModuleName q Nothing) _atomAtom _atomInfo
    ([q],True) -> do
      checkReserved q
      commit
      return $ TDynamic
        (TVar (Name (BareName q _atomInfo)) _atomInfo)
        (TVar (DName (DynamicName _atomAtom q mempty _atomInfo)) _atomInfo)
        _atomInfo
    ([ns,q],_) -> do
      checkReserved ns >> checkReserved q
      var $ QName $
        QualifiedName (ModuleName q (Just . NamespaceName $ ns)) _atomAtom _atomInfo
    _ -> expected "bareword or qualified atom"

listLiteral :: Compile (Term Name)
listLiteral = withList Brackets $ \ListExp{..} -> do
  ls <- case _listList of
    _ : CommaExp : _ -> valueLevel `sepBy` sep Comma
    _                -> many valueLevel
  lty <- freshTyVar
  return $ TList (V.fromList ls) lty _listInfo

objectLiteral :: Compile (Term Name)
objectLiteral = withList Braces $ \ListExp{..} -> do
  let pair = do
        key <- FieldKey <$> str
        val <- sep Colon *> valueLevel
        return (key,val)
  ps <- (pair `sepBy` sep Comma) <* eof
  return $ TObject (Object (ObjectMap $ M.fromList ps) TyAny (Just (map fst ps)) _listInfo) _listInfo

literal :: Compile (Term Name)
literal = lit >>= \LiteralExp{..} ->
  commit >> return (TLiteral _litLiteral _litInfo)

-- | Macro to form '(with-capability CAP BODY)' app from
-- '(with-capability (my-cap foo bar) (baz 1) (bof true))'
withCapability :: Compile (Term Name)
withCapability = do
  wcInf <- getInfo <$> current
  let wcVar = TVar (Name $ BareName (asString RWithCapability) wcInf) wcInf
  capApp <- sexp app
  body@(top:_) <- some valueLevel
  i <- contextInfo
  return $ TApp (App wcVar [capApp,TList (V.fromList body) TyAny (_tInfo top)] i) i

deftable :: Compile (Term Name)
deftable = do
  ModuleState{..} <- moduleState
  AtomExp{..} <- userAtom
  ty <- optional (typed >>= \t -> case t of
                     TyUser {} -> return t
                     _ -> expected "user type")
  m <- meta ModelNotAllowed
  TTable (TableName _atomAtom) _msName _msHash
    (fromMaybe TyAny ty) m <$> contextInfo


bless :: Compile ()
bless = do
  h <- hash'
  overModuleState msBlessed (h:)


defconst :: Compile (Term Name)
defconst = do
  modName <- currentModuleName
  a <- arg
  v <- valueLevel
  m <- meta ModelNotAllowed
  TConst a (Just modName) (CVRaw v) m <$> contextInfo

data ModelAllowed
  = ModelAllowed
  | ModelNotAllowed

data AtPair = DocPair Text | ModelPair [Exp Info] deriving (Eq,Ord)

modelOnly :: Compile Meta
modelOnly = do
  symbol "@model"
  (ListExp props _ _i, _) <- list' Brackets
  pure $ Meta Nothing props

meta :: ModelAllowed -> Compile Meta
meta modelAllowed =
  -- hiding labels/errors here because otherwise they hang around for all module errors
  hidden atPairs <|> hidden (try docStr) <|> return def
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
      ps <- sort <$> some (docPair <|> modelPair)
      case ps of
        [DocPair doc] -> return (Meta (Just doc) [])
        [ModelPair es] -> whenModelAllowed $ return (Meta Nothing es)
        [DocPair doc, ModelPair es] -> whenModelAllowed $ return (Meta (Just doc) es)
        _ -> expected $ case modelAllowed of
          ModelNotAllowed -> "@doc declaration"
          ModelAllowed -> "@doc and/or @model declarations"

defschema :: Compile (Term Name)
defschema = do
  modName <- currentModuleName
  tn <- _atomAtom <$> userAtom
  m <- meta ModelAllowed
  fields <- many arg
  TSchema (TypeName tn) (Just modName) m fields <$> contextInfo

defunOrCap :: DefType -> Compile (Term Name)
defunOrCap dt = do
  modName <- currentModuleName
  (defname,returnTy) <- first _atomAtom <$> typedAtom
  args <- withList' Parens $ many arg
  m <- meta ModelAllowed
  dm <- defcapManaged dt
  b <- abstractBody valueLevel args
  i <- contextInfo
  return $ (`TDef` i) $
    Def (DefName defname) modName dt (FunType args returnTy)
      b m dm i

defcapManaged :: DefType -> Compile (Maybe (DefMeta (Term Name)))
defcapManaged dt = case dt of
  Defcap -> optional (doDefcapMeta <|> doEvent)
  _ -> return Nothing
  where
    doDefcapMeta = symbol "@managed" *>
      (DMDefcap . DefcapManaged <$> (doUserMgd <|> doAuto))
    doUserMgd = Just <$> ((,) <$> (_atomAtom <$> userAtom) <*> userVar)
    doAuto = pure Nothing
    doEvent = symbol "@event" *> pure (DMDefcap DefcapEvent)

defpact :: Compile (Term Name)
defpact = do
  modName <- currentModuleName
  (defname,returnTy) <- first _atomAtom <$> typedAtom
  args <- withList' Parens $ many arg
  m <- meta ModelAllowed
  (body,bi) <- bodyForm' $ specialForm $ \case
    RStep -> return step
    RStepWithRollback -> return stepWithRollback
    _ -> expected "step or step-with-rollback"
  case last body of -- note: `last` is safe, since bodyForm uses `some`
    TStep (Step _ _ (Just _) _) _ _ -> syntaxError "rollbacks aren't allowed on the last \
      \step (the last step can never roll back -- once it's executed the pact \
      \is complete)"
    _ -> pure ()
  i <- contextInfo
  abody <- abstractBody' args (TList (V.fromList body) TyAny bi)
  return $ TDef
    (Def (DefName defname) modName Defpact (FunType args returnTy)
      abody
      m Nothing i) i

moduleForm :: Compile (Term Name)
moduleForm = do
  modName' <- _atomAtom <$> userAtom
  gov <- Governance <$> ((Left <$> keysetNameStr) <|> (Right <$> userVar))
  m <- meta ModelAllowed
  use (psUser . csModule) >>= \case
    Just {} -> syntaxError "Invalid nested module or interface"
    Nothing -> return ()
  i <- contextInfo
  let code = case i of
        Info Nothing -> "<code unavailable>"
        Info (Just (c,_)) -> c
      modName = ModuleName modName' Nothing
      modHash = ModuleHash . pactHash . encodeUtf8 . _unCode $ code
  ((bd,bi),ModuleState{..}) <- withModuleState (initModuleState modName modHash) $ bodyForm' moduleLevel
  return $ TModule
    (MDModule $ Module modName gov m code modHash (HS.fromList _msBlessed) _msImplements _msImports)
    (abstract (const Nothing) (TList (V.fromList (concat bd)) TyAny bi)) i


implements :: Compile ()
implements = do
  (ifn,_) <- qualifiedModuleName
  overModuleState msImplements (ifn:)


interface :: Compile (Term Name)
interface = do
  iname' <- _atomAtom <$> bareAtom
  m <- meta ModelAllowed
  use (psUser . csModule) >>= \case
    Just {} -> syntaxError "invalid nested interface or module"
    Nothing -> return ()
  info <- contextInfo
  let code = case info of
        Info Nothing -> "<code unavailable>"
        Info (Just (c,_)) -> c
      iname = ModuleName iname' Nothing
      ihash = ModuleHash . pactHash . encodeUtf8 . _unCode $ code
  (bd,ModuleState{..}) <- withModuleState (initModuleState iname ihash) $
            bodyForm $ specialForm $ \case
              RDefun -> return $ defSig Defun
              RDefconst -> return defconst
              RUse -> return useForm
              RDefpact -> return $ defSig Defpact
              RDefschema -> return defschema
              RDefcap -> return $ defSig Defcap
              t -> syntaxError $ "Invalid interface declaration: " ++ show (asString t)
  return $ TModule
    (MDInterface $ Interface iname code m _msImports)
    (abstract (const Nothing) bd) info

-- | Recognize a signature of a Def.
defSig :: DefType -> Compile (Term Name)
defSig dt = do
  modName <- currentModuleName
  (defName, returnTy) <- first _atomAtom <$> typedAtom
  args <- withList' Parens $ many arg
  m <- meta ModelAllowed
  dm <- defcapManaged dt
  info <- contextInfo
  return $ (`TDef` info) $
    Def (DefName defName) modName dt
      (FunType args returnTy) (abstract (const Nothing) (TList V.empty TyAny info))
      m dm info


step :: Compile (Term Name)
step = do
  m <- option (Meta Nothing []) modelOnly
  cont <- try (Step <$> (Just <$> valueLevel) <*> valueLevel) <|>
          (Step Nothing <$> valueLevel)
  i <- contextInfo
  pure $ TStep (cont Nothing i) m i

stepWithRollback :: Compile (Term Name)
stepWithRollback = do
  i <- contextInfo
  m <- option (Meta Nothing []) modelOnly
  s <- try (Step <$> (Just <$> valueLevel) <*> valueLevel <*>
            (Just <$> valueLevel) <*> pure i)
       <|> (Step Nothing <$> valueLevel <*> (Just <$> valueLevel) <*> pure i)
  return $ TStep s m i

lam :: Compile (Term Name)
lam = do
  name <- freshNameRaw
  tv <- freshTyVar
  args <- withList' Parens $ many arg
  let funTy = FunType args tv
  info <- contextInfo
  lamValue <- Lam name funTy <$> abstractBody valueLevel args <*> pure info
  pure (TLam lamValue info)

letBindings :: Compile [BindPair (Term Name)]
letBindings =
  withList' Parens $ some $ withList' Parens $ do
    info <- contextInfo
    BindPair a@(Arg n ty _) t <- BindPair <$> arg <*> valueLevel
    case t of
      TLam l _ ->
        let l' =  TLam (l & lamArg .~ n & lamTy . ftReturn .~ ty & lamInfo .~ info) info
        in pure (BindPair (Arg n ty info) l')
      _ -> pure (BindPair a t)

abstractBody :: Compile (Term Name) -> [Arg (Term Name)] -> Compile (Scope Int Term Name)
abstractBody term args = abstractBody' args =<< bodyForm term

abstractBody' :: [Arg (Term Name)] -> Term Name -> Compile (Scope Int Term Name)
abstractBody' args body = traverse enrichDynamic $ abstract (`elemIndex` bNames) body
  where
    bNames = map arg2Name args

    modRefArgs = M.fromList $ (`concatMap` args) $ \a ->
      case _aType a of
        TyModule ifaces -> [(_aName a, ifaces)]
        _ -> []

    enrichDynamic n@(DName dyn@(DynamicName _ ref ifs _))
      | S.null ifs = case M.lookup ref modRefArgs of
        Just ifs' -> DName . setIfs dyn . S.fromList <$> traverse ifVarName (fromMaybe [] ifs')
        Nothing -> return n
      | otherwise = return n
    enrichDynamic n = return n

    setIfs s ifs = set dynInterfaces ifs s

    ifVarName (TVar (Name (BareName n _)) _) =
      return $ ModuleName n Nothing
    ifVarName (TVar (QName (QualifiedName (ModuleName ns Nothing) mn _)) _) =
      return $ ModuleName mn (Just $ NamespaceName ns)
    ifVarName _ = expected "interface reference"

condForm :: Compile (Term Name)
condForm = do
  conds <- conds'
  elseCond <- valueLevel
  i <- contextInfo
  let if' = TVar (Name (BareName "if" i)) i
  pure $ foldr (\(cond, act) e -> TApp (App if' [cond, act, e] i) i) elseCond conds
  where
  conds' = some $ withList' Parens $ (,) <$> valueLevel <*> valueLevel

letForm :: Compile (Term Name)
letForm = do
  bindings <- letBindings
  TBinding bindings <$> abstractBody valueLevel (map _bpArg bindings) <*>
    pure BindLet <*> contextInfo

-- | let* is a macro to nest lets for referencing previous
-- bindings.
letsForm :: Compile (Term Name)
letsForm = do
  bindings <- letBindings
  let nest (binding:rest) = do
        scope <- abstractBody' [_bpArg binding] =<< case rest of
          [] -> bodyForm valueLevel
          _ -> do
            rest' <- nest rest
            pure $ TList (V.singleton rest') TyAny def
        TBinding [binding] scope BindLet <$> contextInfo
      nest [] =  syntaxError "letsForm: invalid state (bug)"
  nest bindings

useForm :: Compile (Term Name)
useForm = do
  (mn,_) <- qualifiedModuleName
  i <- contextInfo
  h <- optional hash'
  l <- optional $ withList' Brackets (many userAtom <* eof)

  let v = fmap (V.fromList . fmap _atomAtom) l
      u = Use mn h v i

  -- this is the one place module may not be present, use traversal
  psUser . csModule . _Just . msImports %= (u:)
  return $ TUse u i

hash' :: Compile ModuleHash
hash' = str >>= \s -> case fromText' s of
  Right h -> return (ModuleHash h)
  Left e -> syntaxError $ "bad hash: " ++ e

typedAtom :: Compile (AtomExp Info,Type (Term Name))
typedAtom = (,) <$> userAtom <*> (typed <|> freshTyVar)

arg :: Compile (Arg (Term Name))
arg = typedAtom >>= \(AtomExp{..},ty) ->
  return $ Arg _atomAtom ty _atomInfo

arg2Name :: Arg n -> Name
arg2Name Arg{..} = Name $ BareName _aName _aInfo

typed :: Compile (Type (Term Name))
typed = sep Colon *> parseType

parseType :: Compile (Type (Term Name))
parseType = msum
  [ parseListType
  , parseUserSchemaType
  , parseSchemaType tyObject TyObject
  , parseSchemaType tyTable TyTable
  , parseModuleRefType
  , TyPrim TyInteger <$ symbol tyInteger
  , TyPrim TyDecimal <$ symbol tyDecimal
  , TyPrim TyTime    <$ symbol tyTime
  , TyPrim TyBool    <$ symbol tyBool
  , TyPrim TyString  <$ symbol tyString
  , TyList TyAny     <$ symbol tyList
  , TyPrim (TyGuard $ Just GTyKeySet)  <$ symbol tyKeySet
  , TyPrim (TyGuard Nothing) <$ symbol tyGuard
  ]

parseListType :: Compile (Type (Term Name))
parseListType = withList' Brackets $ TyList <$> parseType

parseSchemaType :: Text -> SchemaType -> Compile (Type (Term Name))
parseSchemaType tyRep sty = symbol tyRep >>
  (TySchema sty <$> (parseUserSchemaType <|> pure TyAny) <*> pure def)

parseModuleRefType :: Compile (Type (Term Name))
parseModuleRefType = symbol "module" >>
  (TyModule . Just <$> withList' Braces
   ((snd <$> qualifiedModuleName) `sepBy1` sep Comma))

parseUserSchemaType :: Compile (Type (Term Name))
parseUserSchemaType = withList Braces $ \ListExp{} -> TyUser <$> varAtom


bodyForm :: Compile (Term Name) -> Compile (Term Name)
bodyForm term = do
  (bs,i) <- bodyForm' term
  return $ TList (V.fromList bs) TyAny i

bodyForm' :: Compile a -> Compile ([a],Info)
bodyForm' term = (,) <$> go <*> contextInfo
  where go = getInput >>= \c -> case _cStream c of
          [] -> pure <$> term -- will fail, similar to 'some'
          es -> replicateM (length es) term

_compileAccounts :: IO (Either PactError [Term Name])
_compileAccounts = _compileF "examples/accounts/accounts.pact"

_compileF :: FilePath -> IO (Either PactError [Term Name])
_compileF f = _parseF f >>= _compile id

handleParseError :: TF.Result a -> IO a
handleParseError (TF.Failure f) = putDoc (TF._errDoc f) >> error "parseFailed"
handleParseError (TF.Success a) = return a

_compileWith :: Compile a -> (ParseState CompileState -> ParseState CompileState) ->
            TF.Result ([Exp Parsed],MkInfo) -> IO (Either PactError [a])
_compileWith parser sfun r = handleParseError r >>= \(a,mk) -> return $ forM a $ \e ->
  let ei = mk <$> e
  in runCompile def parser (sfun (initParseState ei)) ei

_compile :: (ParseState CompileState -> ParseState CompileState) ->
            TF.Result ([Exp Parsed],MkInfo) -> IO (Either PactError [Term Name])
_compile = _compileWith topLevel


-- | run a string as though you were in a module (test deftable, etc)
_compileStrInModule :: String -> IO [Term Name]
_compileStrInModule = fmap concat . _compileStr' moduleLevel
  (set (psUser . csModule) (Just (initModuleState "mymodule" (ModuleHash $ pactHash mempty))))

_compileStr :: String -> IO [Term Name]
_compileStr = _compileStr' topLevel id

_compileStrWith :: Compile a -> String -> IO [a]
_compileStrWith parser code = _compileStr' parser id code

_compileStr' :: Compile a -> (ParseState CompileState -> ParseState CompileState) -> String -> IO [a]
_compileStr' parser sfun code = do
    r <- _compileWith parser sfun $ _parseS code
    case r of Left e -> throwIO $ userError (show e)
              Right t -> return t

_parseS :: String -> TF.Result ([Exp Parsed],MkInfo)
_parseS s = (,mkStringInfo s) <$> TF.parseString exprsOnly mempty s

_parseF :: FilePath -> IO (TF.Result ([Exp Parsed],MkInfo))
_parseF fp = do
  bs <- BS.readFile fp
  let s = decodeUtf8 bs
  fmap (,mkTextInfo s) <$> TF.parseFromFileEx exprsOnly fp

_compileFile :: FilePath -> IO [Term Name]
_compileFile f = do
    p <- _parseF f
    rs <- case p of
            (TF.Failure e) -> putDoc (TF._errDoc e) >> error "Parse failed"
            (TF.Success (es,s)) -> return $ map (compile def s) es
    case sequence rs of
      Left e -> throwIO $ userError (show e)
      Right ts -> return ts

_atto :: FilePath -> IO [Term Name]
_atto fp = do
  f <- decodeUtf8 <$> BS.readFile fp
  rs <- case parseExprs f of
    Left s -> throwIO $ userError s
    Right es -> return $ map (compile def (mkStringInfo (unpack f))) es
  case sequence rs of
      Left e -> throwIO $ userError (show e)
      Right ts -> return ts

_testCToTV :: Bool
_testCToTV = nub vs == vs where vs = take (26*26*26) $ map cToTV [0..]

_unusedLenses :: Compile ()
_unusedLenses = void $ return (set msName,set msHash)
