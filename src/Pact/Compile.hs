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

import Text.Trifecta as TF
import Text.Trifecta.Delta as TF
import Control.Applicative
import Data.List
import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Control.Arrow
import Prelude hiding (exp)
import Bound
import Text.PrettyPrint.ANSI.Leijen (putDoc)
import Control.Exception
import Data.String
import Control.Lens
import Data.Maybe
import Data.Default
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.HashSet as HS
import qualified Data.Map.Strict as M

import Pact.Types.Lang
import Pact.Parse (exprsOnly,parseExprs)
import Pact.Types.Runtime (PactError(..),PactErrorType(..))
import Pact.Types.Hash

type MkInfo = Exp -> Info

mkEmptyInfo :: MkInfo
mkEmptyInfo e = Info (Just (mempty,_eParsed e))

mkStringInfo :: String -> MkInfo
mkStringInfo s e = Info (Just (fromString $ take (_pLength d) $ drop (fromIntegral $ bytes d) s,d))
  where d = _eParsed e

mkTextInfo :: T.Text -> MkInfo
mkTextInfo s e = Info (Just (Code $ T.take (_pLength d) $ T.drop (fromIntegral $ bytes d) s,d))
  where d = _eParsed e

data CompileState = CompileState {
  _csFresh :: Int,
  _csModule :: Maybe (ModuleName,Hash)
  }
instance Default CompileState where def = CompileState 0 def
makeLenses ''CompileState

type Compile a = ReaderT MkInfo (StateT CompileState (Except PactError)) a

reserved :: [Text]
reserved = map pack $ words "use module defun defpact step step-with-rollback true false let let* defconst"

compile :: MkInfo -> Exp -> Either PactError (Term Name)
compile mi e = runExcept (evalStateT (runReaderT (run e) mi) def)

compileExps :: Traversable t => MkInfo -> t Exp -> Either PactError (t (Term Name))
compileExps mi exps = sequence $ compile mi <$> exps


syntaxError :: Info -> String -> Compile a
syntaxError i s = throwError $ PactError SyntaxError i def (pack s)

syntaxError' :: Exp -> String -> Compile a
syntaxError' e s = mkInfo e >>= \i -> syntaxError i s

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

justDocs :: Text -> DocModel
justDocs docs = DocModel (Just docs) M.empty

mkModel :: Exp -> Compile (M.Map Text Exp)
mkModel ps = uncurry M.singleton <$> toMetas ps
  where toMetas (EList' [EAtom' k,v]) = return (k,v)
        toMetas bad = mkInfo bad >>= \i -> syntaxError i "mkModel: malformed model, expected (atom expression) where atom is one of {property, properties, invariant, invariants}"

-- | A meta-annotation, which includes either form:
--
-- * `"docstring"`
-- * `@doc ...` / `@meta ...`
pattern MetaExp :: Compile DocModel -> [Exp] -> [Exp]
pattern MetaExp dm exps <- (doMeta -> (dm, exps))

-- | Consume a meta-block (returning the leftover body).
--
-- Helper for 'MetaExp'.
doMeta :: [Exp] -> (Compile DocModel, [Exp])
doMeta = \case
  -- Either we encounter a plain docstring:
  ELitString docs : exps
    -> (pure (justDocs docs), exps)

  -- ... or some subset of @doc and @model:
  EAtom' "@doc" : ELitString docs : EAtom' "@model" : model : exps
    -> (DocModel (Just docs) <$> mkModel model, exps)
  EAtom' "@model" : model : EAtom' "@doc" : ELitString docs : exps
    -> (DocModel (Just docs) <$> mkModel model, exps)
  EAtom' "@doc" : ELitString docs : exps
    -> (pure (justDocs docs), exps)
  EAtom' "@model" : model : exps
    -> (DocModel Nothing <$> mkModel model, exps)

  -- ... or neither:
  exps -> (pure (DocModel Nothing M.empty), exps)

-- | A (non-empty) body with a possible meta-annotation
pattern MetaBodyExp :: Compile DocModel -> [Exp] -> [Exp]
pattern MetaBodyExp meta body <- (doMetaBody -> Just (meta, body))

-- TODO(joel): uncomment when on modern ghc
-- {-# complete MetaBodyExp, [] #-}

-- | Consume a meta-annotationa and body. Helper for 'MetaBodyExp'.
doMetaBody :: [Exp] -> Maybe (Compile DocModel, [Exp])
doMetaBody exp  = case exp of
  []                    -> Nothing
  MetaExp docModel body -> Just (docModel, body)
  _                     -> error "the first two patterns are complete"

doModule :: [Exp] -> Info -> Info -> Compile (Term Name)
doModule (EAtom n Nothing Nothing _:ESymbol k _:es) li ai =
  handleModule n k es li ai
doModule (EAtom n Nothing Nothing _:ELiteral (LString k) _:es) li ai =
  handleModule n k es li ai
doModule _ li _ = syntaxError li "Invalid module definition"

handleModule :: Text -> Text -> [Exp] -> Info -> Info -> Compile (Term Name)
handleModule n k es li ai =
  case es of
    MetaBodyExp meta body -> mkModule body =<< meta
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
          mkDef dn ty args body =<< meta
      _ -> syntaxError namei "Invalid def"
      where
        mkDef dn ty dargs body ddocs = do
          args <- mapM atomVar dargs
          let argsn = map (\aa -> Name (_aName aa) (_aInfo aa)) args
          dty <- FunType <$> pure args <*> maybeTyVar namei ty
          cm <- currentModule i
          db <- abstract (`elemIndex` argsn) <$> runBody body i
          return $ TDef dn (fst cm) defType dty db ddocs i

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

maybeTyVar :: Info -> Maybe (Type TypeName) -> Compile (Type (Term Name))
maybeTyVar _ Nothing = freshTyVar
maybeTyVar i (Just t) = return (liftTy i t)

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
    -> mkConst dn ct t =<< dm
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
    mkUT utn as =<< meta
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
  EAtom tn Nothing ty _ : MetaExp meta [] -> mkT tn ty =<< meta
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

mkInfo :: Exp -> Compile Info
mkInfo e = ask >>= \f -> return (f e)

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


_compileAccounts :: IO (Either PactError [Term Name])
_compileAccounts = _parseF "examples/accounts/accounts.pact" >>= _compile

_compile :: Result ([Exp],String) -> IO (Either PactError [Term Name])
_compile (Failure f) = putDoc (_errDoc f) >> error "Parse failed"
_compile (Success (a,s)) = return $ mapM (compile (mkStringInfo s)) a


_compileStr :: String -> IO [Term Name]
_compileStr code = do
    r <- _compile ((,code) <$> TF.parseString exprsOnly mempty code)
    case r of Left e -> throwIO $ userError (show e)
              Right t -> return t


_parseF :: FilePath -> IO (TF.Result ([Exp],String))
_parseF fp = readFile fp >>= \s -> fmap (,s) <$> TF.parseFromFileEx exprsOnly fp

_compileFile :: FilePath -> IO [Term Name]
_compileFile f = do
    p <- _parseF f
    rs <- case p of
            (Failure e) -> putDoc (_errDoc e) >> error "Parse failed"
            (Success (es,s)) -> return $ map (compile (mkStringInfo s)) es
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
