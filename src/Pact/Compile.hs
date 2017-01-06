{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
-- |
-- Module      :  Pact.Compile
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Parser and compiler.
--

module Pact.Compile
    (
     expr,exprs
    ,compile
    ,dec
    )

where

import Text.Trifecta as TF hiding (spaces)
import Control.Applicative
import Data.List
import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Control.Arrow
import Prelude hiding (exp)
import Bound
import Text.PrettyPrint.ANSI.Leijen (putDoc)
import Pact.Types
import Control.Exception
import Data.String
import qualified Data.HashSet as HS
import Text.Parser.Token.Highlight
import Control.Lens hiding (op)
import Data.Maybe
import Data.Default
import Data.Decimal

symbols :: CharParsing m => m Char
symbols = oneOf "%#+-_&$@<>=^?*!|/"

style :: CharParsing m => IdentifierStyle m
style = IdentifierStyle "pact"
        (letter <|> symbols)
        (letter <|> digit <|> symbols)
        (HS.fromList ["true","false"])
        Symbol
        ReservedIdentifier

expr :: (Monad m,TokenParsing m,CharParsing m,DeltaParsing m) => m Exp
expr = do
  (!r,!p) <- (,) <$> rend <*> position
  let inf = Info (Just (r,p))
  TF.try (ELiteral . LDecimal <$> neg dec <*> pure inf <?> "Decimal literal")
   <|>
   (ELiteral . LInteger <$> neg natural <*> pure inf <?> "Integer literal")
   <|>
   (ELiteral . LString <$> stringLiteral <*> pure inf <?> "String literal")
   <|>
   (reserve style "true" >> ELiteral (LBool True) <$> pure inf <?> "Boolean true")
   <|>
   (reserve style "false" >> ELiteral (LBool False) <$> pure inf <?> "Boolean false")
   <|>
   (ESymbol <$> (char '\'' >> ident style) <*> pure inf <?> "Symbol literal")
   <|>
   do
     a <- ident style
     TF.try (typed >>= \t -> return (EAtom a Nothing (Just t) inf) <?> "typed atom") <|>
       TF.try (qualified >>= \q -> return (EAtom a (Just q) Nothing inf) <?> "qual atom") <|>
       (return (EAtom a Nothing Nothing inf) <?> "bare atom")
   <|>
   (EList <$> parens (sepBy expr spaces) <*> pure inf <?> "sexp")
   <|>
   do
     is <- brackets (sepBy expr spaces) <?> "list literal"
     return $ EList (EAtom "list" Nothing Nothing def:is) inf
   <|> do
     ps <- pairs
     let ops = map fst ps
         kvs = map snd ps
     if all (== ":") ops then return $ EObject kvs inf
     else if all (== ":=") ops then return $ EBinding kvs inf
          else unexpected $ "Mixed binding/object operators: " ++ show ops

qualified :: (Monad m,TokenParsing m) => m String
qualified = char '.' *> ident style

typed :: (Monad m,TokenParsing m) => m (Type TypeName)
typed = do
  _ <- char ':'
  spaces
  parseType

parseType :: (Monad m,TokenParsing m) => m (Type TypeName)
parseType =
  (char '[' >> parseType >>= \t -> char ']' >> return (TyList t) <?> "typed list") <|>
  (char '{' >> ident style >>= \t -> char '}' >> return (TyUser (fromString t)) <?> "user type") <|>
  symbol tyInteger *> return (TyPrim TyInteger) <|>
  symbol tyDecimal *> return (TyPrim TyDecimal) <|>
  symbol tyTime *> return (TyPrim TyTime) <|>
  symbol tyBool *> return (TyPrim TyBool) <|>
  symbol tyString *> return (TyPrim TyString) <|>
  symbol tyList *> return (TyList TyAny) <|>
  symbol tyObject *> return (TySchema TyObject TyAny) <|>
  symbol tyValue *> return (TyPrim TyValue) <|>
  symbol tyKeySet *> return (TyPrim TyKeySet) <|>
  symbol tyTable *> return (TySchema TyTable TyAny) -- TODO no way to specify table schema



-- | Skip spaces or one-line comments
spaces :: CharParsing m => m ()
spaces = skipMany (skipSome space <|> oneLineComment)
    where oneLineComment = TF.try (string ";") *> skipMany (satisfy (/= '\n'))
{-# INLINE spaces #-}
--space <?> "white space"


{-# INLINE expr #-}

exprs :: (Monad m,TokenParsing m,CharParsing m,DeltaParsing m) => m [Exp]
exprs = some (spaces *> expr <* spaces)

neg :: (Monad m,CharParsing m,Num n) => m n -> m n
neg p = TF.try (char '-' >> (negate <$> p)) <|> p

dec :: (Monad m,CharParsing m) => m Decimal
dec = do
  i <- TF.some TF.digit
  d <- TF.char '.'
  e <- TF.some TF.digit
  return (read (i ++ d:e))

pairs :: (Monad m,TokenParsing m,CharParsing m,DeltaParsing m) =>
         m [(String,(Exp,Exp))]
pairs =
    braces $ (`sepBy` char ',')
    (do
       spaces
       k <- expr
       spaces
       op <- symbol ":=" <|> symbol ":"
       spaces
       v <- expr
       spaces
       return (op,(k,v))
    ) <?> "curly-brace pairs"



parseS :: TF.Parser a -> String -> TF.Result a
parseS p = TF.parseString p mempty

parseF :: TF.Parser a -> FilePath -> IO (TF.Result a)
parseF p fp = parseS p <$> readFile fp


data CompileState = CompileState {
  _csFresh :: Int,
  _csModule :: Maybe ModuleName
  }
instance Default CompileState where def = CompileState 0 def
makeLenses ''CompileState

type Compile a = StateT CompileState (Except SyntaxError) a

reserved :: [String]
reserved = words "use module defun defpact step step-with-rollback true false let let* defconst"

compile :: Exp -> Either SyntaxError (Term Name)
compile e = runExcept (evalStateT (run e) def)


syntaxError :: Info -> String -> Compile a
syntaxError i s = throwError $ SyntaxError i s

doUse :: [Exp] -> Info -> Compile (Term Name)
doUse [ESymbol s _] i = return $ TUse (fromString s) i
doUse _ i = syntaxError i "Use only takes a module symbol name"

doModule :: [Exp] -> Info -> Info -> Exp -> Compile (Term Name)
doModule (EAtom n Nothing Nothing _:ESymbol k _:es) li ai mc =
  case es of
    [] -> syntaxError ai "Empty module"
    (ELiteral (LString docs) _:body) -> mkModule (Just docs) body
    body -> mkModule Nothing body
    where
      defOnly d = case d of
        TDef {} -> return d
        TNative {} -> return d
        TConst {} -> return d
        TUserType {} -> return d
        TTable {} -> return d
        t -> syntaxError (_tInfo t) "Only defun, defpact, defconst, deftable allowed in module"
      mkModule docs body = do
        cm <- use csModule
        case cm of
          Just _ -> syntaxError li "Invalid nested module"
          Nothing -> do
            csModule .= Just (fromString n)
            bd <- mapNonEmpty "module" (run >=> defOnly) body li
            csModule .= Nothing
            return $ TModule
              (Module (fromString n) (fromString k) docs mc)
              (abstract (const Nothing) (TList bd TyAny li)) li

doModule _ li _ _ = syntaxError li "Invalid module definition"

currentModule :: Info -> Compile ModuleName
currentModule i = use csModule >>= \m -> case m of
  Just cm -> return cm
  Nothing -> syntaxError i "Must be declared within module"

doDef :: [Exp] -> DefType -> Info -> Info -> Compile (Term Name)
doDef es defType ai i =
    case es of
      (EAtom dn Nothing ty _:EList args _:ELiteral (LString docs) _:body) ->
          mkDef dn ty args (Just docs) body
      (EAtom dn Nothing ty _:EList args _:body) ->
          mkDef dn ty args Nothing body
      _ -> syntaxError ai "Invalid def"
      where
        mkDef dn ty dargs ddocs body = do
          args <- mapM atomVar dargs
          let argsn = map (Name . _aName) args
          dty <- FunType <$> pure args <*> maybeTyVar ty
          cm <- currentModule i
          db <- abstract (`elemIndex` argsn) <$> runBody body i
          return $ TDef dn cm defType dty db ddocs i

freshTyVar :: Compile (Type (Term Name))
freshTyVar = do
  c <- state (view csFresh &&& over csFresh succ)
  return $ mkTyVar (cToTV c) []

cToTV :: Int -> String
cToTV n | n < 26 = [toC n]
        | n <= 26 * 26 = [toC (pred (n `div` 26)), toC (n `mod` 26)]
        | otherwise = toC (n `mod` 26) : show ((n - (26 * 26)) `div` 26)
  where toC i = toEnum (fromEnum 'a' + i)

_testCToTV :: Bool
_testCToTV = nub vs == vs where vs = take (26*26*26) $ map cToTV [0..]

maybeTyVar :: Maybe (Type TypeName) -> Compile (Type (Term Name))
maybeTyVar Nothing = freshTyVar
maybeTyVar (Just t) = return (fmap (return . Name . asString) t)


doStep :: [Exp] -> Info -> Compile (Term Name)
doStep [entity,exp] i =
    TStep <$> run entity <*> run exp <*> pure Nothing <*> pure i
doStep _ i = syntaxError i "Invalid step definition"

doStepRollback :: [Exp] -> Info -> Compile (Term Name)
doStepRollback [entity,exp,rb] i =
    TStep <$> run entity <*> run exp <*> (Just <$> run rb) <*> pure i
doStepRollback _ i = syntaxError i "Invalid step-with-rollback definition"

letPair :: Exp -> Compile (Arg (Term Name), Term Name)
letPair (EList [EAtom s Nothing ty i,v] _) = (,) <$> (Arg <$> pure s <*> maybeTyVar ty <*> pure i) <*> run v
letPair t = syntaxError (_eInfo t) "Invalid let pair"

doLet :: [Exp] -> Info -> Compile (Term Name)
doLet (bindings:body) i = do
  bPairs <-
    case bindings of
      (EList es _) -> forM es letPair
      t -> syntaxError (_eInfo t) "Invalid let bindings"
  let bNames = map (Name . _aName . fst) bPairs
  bs <- abstract (`elemIndex` bNames) <$> runBody body i
  return $ TBinding bPairs bs BindLet i
doLet _ i = syntaxError i "Invalid let declaration"

-- | let* is a macro to nest a bunch of lets
doLets :: [Exp] -> Info -> Compile (Term Name)
doLets (bindings:body) i =
  case bindings of
      e@(EList [_] _) -> doLet (e:body) i
      (EList (e:es) _) -> let e' = head es in
                          doLet [EList [e] (_eInfo e),
                                 EList (EAtom "let*" Nothing Nothing (_eInfo e'):
                                        EList es (_eInfo e'):body)
                                 (_eInfo e')] i
      e -> syntaxError (_eInfo e) "Invalid let* binding"
doLets _ i = syntaxError i "Invalid let declaration"

doConst :: [Exp] -> Info -> Compile (Term Name)
doConst es i = case es of
  [EAtom dn Nothing ct _,t] -> mkConst dn ct t Nothing
  [EAtom dn Nothing ct _,t,ELiteral (LString docs) _] -> mkConst dn ct t (Just docs)
  _ -> syntaxError i "Invalid defconst"
  where
    mkConst dn ty v docs = do
      v' <- run v
      cm <- currentModule i
      a <- Arg <$> pure dn <*> maybeTyVar ty <*> pure i
      return $ TConst a cm v' docs i

doUserType :: [Exp] -> Info -> Compile (Term Name)
doUserType es i = case es of
  (EAtom utn Nothing Nothing _:ELiteral (LString docs) _:as) -> mkUT utn (Just docs) as
  (EAtom utn Nothing Nothing _:as) -> mkUT utn Nothing as
  _ -> syntaxError i "Invalid object definition"
  where
    mkUT utn docs as = do
      cm <- currentModule i
      fs <- forM as $ \a -> case a of
        EAtom an Nothing ty ai -> Arg an <$> maybeTyVar ty <*> pure ai
        _ -> syntaxError i "Invalid object field definition"
      return $ TUserType (fromString utn) cm docs fs i

doTable :: [Exp] -> Info -> Compile (Term Name)
doTable es i = case es of
  [EAtom tn Nothing ty _] -> mkT tn ty Nothing
  [EAtom tn Nothing ty _,ELiteral (LString docs) _] -> mkT tn ty (Just docs)
  _ -> syntaxError i "Invalid table definition"
  where
    mkT tn ty docs = do
      cm <- currentModule i
      tty :: Type (Term Name) <- case ty of
        Just (TyUser ot) -> return $ TyUser (return (Name (asString ot)))
        Nothing -> return TyAny
        _ -> syntaxError i "Invalid table row type, must be an object type e.g. {myobject}"
      return $ TTable (fromString tn) cm tty docs i

run :: Exp -> Compile (Term Name)

run l@(EList (EAtom a q Nothing ai:rest) li) =
    case (a,q) of
      ("use",Nothing) -> doUse rest li
      ("module",Nothing) -> doModule rest li ai l
      ("defun",Nothing) -> doDef rest Defun ai li
      ("defpact",Nothing) -> doDef rest Defpact ai li
      ("step",Nothing) -> doStep rest li
      ("step-with-rollback",Nothing) -> doStepRollback rest li
      ("let",Nothing) -> doLet rest li
      ("let*",Nothing) -> doLets rest li
      ("defconst",Nothing) -> doConst rest li
      ("defobject",Nothing) -> doUserType rest li
      ("deftable",Nothing) -> doTable rest li
      (_,_) ->
        case break (isJust . firstOf _EBinding) rest of
          (preArgs@(_:_),EBinding bs bi:bbody) ->
            do
              as <- mapM run preArgs
              let mkPairs (v,k) = (,) <$> atomVar k <*> run v
              bs' <- mapNonEmpty "binding" mkPairs bs li
              let ks = map (Name . _aName . fst) bs'
              bdg <- TBinding <$> pure bs' <*>
                   (abstract (`elemIndex` ks) <$> runBody bbody bi) <*> pure BindKV <*> pure bi
              TApp <$> mkVar a q ai <*> pure (as ++ [bdg]) <*> pure li
          _ -> TApp <$> mkVar a q ai <*> mapM run rest <*> pure li

run (EObject bs i) = TObject <$> mapNonEmpty "object" (\(k,v) -> (,) <$> run k <*> run v) bs i <*> pure TyAny <*> pure i
run (EBinding _ i) = syntaxError i "Unexpected binding"
run (ESymbol s i) = return $ TLiteral (LString s) i
run (ELiteral l i) = return $ TLiteral l i
run (EAtom s q t i) | s `elem` reserved = syntaxError i $ "Unexpected reserved word: " ++ s
                    | isNothing t = mkVar s q i
                    | otherwise = syntaxError i "Invalid typed var"
run e = syntaxError (_eInfo e) $ "Unexpected expression: " ++ show e
{-# INLINE run #-}

mkVar :: String -> Maybe String -> Info -> Compile (Term Name)
mkVar s q i = TVar <$> pure (maybe (Name s) (QName $ fromString s) q) <*> pure i
{-# INLINE mkVar #-}

mapNonEmpty :: String -> (a -> Compile b) -> [a] -> Info -> Compile [b]
mapNonEmpty s _ [] i = syntaxError i $ "Empty " ++ s
mapNonEmpty _ act body _ = mapM act body
{-# INLINE mapNonEmpty #-}

runNonEmpty :: String -> [Exp] -> Info -> Compile [Term Name]
runNonEmpty s = mapNonEmpty s run
{-# INLINE runNonEmpty #-}

atomVar :: Exp -> Compile (Arg (Term Name))
atomVar (EAtom a Nothing ty i) = Arg <$> pure a <*> maybeTyVar ty <*> pure i
atomVar e = syntaxError (_eInfo e) "Expected unqualified atom"
{-# INLINE atomVar #-}

runBody :: [Exp] -> Info -> Compile (Term Name)
runBody bs i = TList <$> runNonEmpty "body" bs i <*> pure TyAny <*> pure i
{-# INLINE runBody #-}



_parseAccounts :: IO (Result [Exp])
_parseAccounts = parseF (exprs <* TF.eof) "examples/accounts/accounts.pact"

_compileAccounts :: IO (Either SyntaxError [Term Name])
_compileAccounts = _parseAccounts >>= _compile

_compile :: Result [Exp] -> IO (Either SyntaxError [Term Name])
_compile (Failure f) = putDoc (_errDoc f) >> error "Parse failed"
_compile (Success a) = return $ mapM compile a


_compileStr :: String -> IO [Term Name]
_compileStr code = do
    r <- _compile (parseS exprs code)
    case r of Left e -> throwIO $ userError (show e)
              Right t -> return t

_compileFile :: FilePath -> IO [Term Name]
_compileFile f = do
    p <- parseF exprs f
    rs <- case p of
            (Failure e) -> putDoc (_errDoc e) >> error "Parse failed"
            (Success es) -> return $ map compile es
    case sequence rs of
      Left e -> throwIO $ userError (show e)
      Right ts -> return ts
