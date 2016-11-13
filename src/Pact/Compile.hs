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
import Prelude hiding (exp)
import Bound
import Text.PrettyPrint.ANSI.Leijen (putDoc)
import Pact.Types
import Control.Exception
import Data.String
import qualified Data.HashSet as HS
import Text.Parser.Token.Highlight
import Control.Lens (firstOf)
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
  TF.try (ELiteral . LDecimal <$> dec <*> pure inf <?> "Decimal literal")
   <|>
   (ELiteral . LInteger <$> natural <*> pure inf <?> "Integer literal")
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
     TF.try (char '.' >> ident style >>= \q -> return (EAtom a (Just q) inf) <?> "qualified atom") <|>
       (return (EAtom a Nothing inf) <?> "bare atom")
   <|>
   (EList <$> parens (sepBy expr spaces) <*> pure inf <?> "sexp")
   <|>
   do
     is <- brackets (sepBy expr spaces) <?> "list literal"
     return $ EList (EAtom "list" Nothing def:is) inf
   <|> do
     ps <- pairs
     let ops = map fst ps
         kvs = map snd ps
     if all (== ":") ops then return $ EObject kvs inf
     else if all (== ":=") ops then return $ EBinding kvs inf
          else unexpected $ "Mixed binding/object operators: " ++ show ops

-- | Skip spaces or one-line comments
spaces :: CharParsing m => m ()
spaces = skipMany (skipSome space <|> oneLineComment)
    where oneLineComment = TF.try (string ";") *> skipMany (satisfy (/= '\n'))
{-# INLINE spaces #-}
--space <?> "white space"


{-# INLINE expr #-}

exprs :: (Monad m,TokenParsing m,CharParsing m,DeltaParsing m) => m [Exp]
exprs = some (spaces *> expr <* spaces)



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

reserved :: [String]
reserved = words "use module defun defpact step step-with-rollback true false let let* defconst"

compile :: Exp -> Either (Info,String) (Term Name)
compile = runExcept . run


type Compile a = Except (Info,String) a

doUse :: [Exp] -> Info -> Compile (Term Name)
doUse [ESymbol s _] i = return $ TUse (fromString s) i
doUse _ i = throwError (i,"Use only takes a module symbol name")

doModule :: [Exp] -> Info -> Info -> Exp -> Compile (Term Name)
doModule (EAtom n Nothing _:ESymbol k _:es) li ai mc =
    case es of
      [] -> throwError (ai,"Empty module")
      (ELiteral (LString docs) _:body) -> mkModule (Just docs) body
      body -> mkModule Nothing body
      where
        defOnly d@TDef {} = return d
        defOnly d@TNative {} = return d
        defOnly d@TConst {} = return d
        defOnly t = throwError (_tInfo t,"Only defun/defpact/defconst allowed in module")
        mkModule docs body = do
              bd <- mapNonEmpty "module" (run >=> defOnly) body li
              TModule <$> pure (fromString n) <*> pure (fromString k) <*> pure docs <*>
                      abstract (const Nothing) <$> pure (TList bd li) <*> pure mc <*> pure li
doModule _ li _ _ = throwError (li,"Invalid module definition")

doDef :: [Exp] -> DefType -> Info -> Exp -> Info -> Compile (Term Name)
doDef es defType ai d i =
    case es of
      (EAtom dn Nothing _:EList args _:ELiteral (LString docs) _:body) ->
          mkDef dn args (Just docs) body
      (EAtom dn Nothing _:EList args _:body) ->
          mkDef dn args Nothing body
      _ -> throwError (ai,"Invalid def")
      where
        mkDef dn dargs ddocs body = do
          args <- mapM atomText dargs
          let argsn = map Name args
              defBody = abstract (`elemIndex` argsn) <$> runBody body i
          TDef <$> pure (DefData dn defType Nothing args ddocs)
                   <*> defBody <*> pure d <*> pure i

doStep :: [Exp] -> Info -> Compile (Term Name)
doStep [entity,exp] i =
    TStep <$> run entity <*> run exp <*> pure Nothing <*> pure i
doStep _ i = throwError (i,"Invalid step definition")

doStepRollback :: [Exp] -> Info -> Compile (Term Name)
doStepRollback [entity,exp,rb] i =
    TStep <$> run entity <*> run exp <*> (Just <$> run rb) <*> pure i
doStepRollback _ i = throwError (i,"Invalid step-with-rollback definition")

letPair :: Exp -> Compile (String, Term Name)
letPair (EList [EAtom s Nothing _,v] _) = (,) <$> pure s <*> run v
letPair t = throwError (_eInfo t,"Invalid let pair")

doLet :: [Exp] -> Info -> Compile (Term Name)
doLet (bindings:body) i = do
  bPairs <-
    case bindings of
      (EList es _) -> forM es letPair
      t -> throwError (_eInfo t,"Invalid let bindings")
  let bNames = map (Name . fst) bPairs
  bs <- abstract (`elemIndex` bNames) <$> runBody body i
  return $ TBinding bPairs bs i
doLet _ i = throwError (i,"Invalid let declaration")

-- | let* is a macro to nest a bunch of lets
doLets :: [Exp] -> Info -> Compile (Term Name)
doLets (bindings:body) i =
  case bindings of
      e@(EList [_] _) -> doLet (e:body) i
      (EList (e:es) _) -> let e' = head es in
                          doLet [EList [e] (_eInfo e),
                                 EList (EAtom "let*" Nothing (_eInfo e'):
                                        EList es (_eInfo e'):body)
                                 (_eInfo e')] i
      e -> throwError (_eInfo e,"Invalid let* binding")
doLets _ i = throwError (i,"Invalid let declaration")

doConst :: [Exp] -> Info -> Compile (Term Name)
doConst [EAtom dn Nothing _,t] i =
    run t >>= \t' -> return (TConst (DefData dn Defconst Nothing [] Nothing) t' i)
doConst [EAtom dn Nothing _,t,ELiteral (LString docs) _] i =
    run t >>= \t' -> return (TConst (DefData dn Defconst Nothing [] (Just docs)) t' i)
doConst _ i = throwError (i,"Invalid defconst")


run :: Exp -> Compile (Term Name)

run l@(EList (EAtom a q ai:rest) li) =
    case (a,q) of
      ("use",Nothing) -> doUse rest li
      ("module",Nothing) -> doModule rest li ai l
      ("defun",Nothing) -> doDef rest Defun ai l li
      ("defpact",Nothing) -> doDef rest Defpact ai l li
      ("step",Nothing) -> doStep rest li
      ("step-with-rollback",Nothing) -> doStepRollback rest li
      ("let",Nothing) -> doLet rest li
      ("let*",Nothing) -> doLets rest li
      ("defconst",Nothing) -> doConst rest li
      (_,_) ->
        case break (isJust . firstOf _EBinding) rest of
          (preArgs@(_:_),EBinding bs bi:bbody) ->
            do
              as <- mapM run preArgs
              let mkPairs (v,k) = (,) <$> atomText k <*> run v
              bs' <- mapNonEmpty "binding" mkPairs bs li
              let ks = map (Name . fst) bs'
              bdg <- TBinding <$> pure bs' <*>
                   (abstract (`elemIndex` ks) <$> runBody bbody bi) <*> pure bi
              return $ TApp (mkVar a q ai) (as ++ [bdg]) li

          _ -> TApp <$> pure (mkVar a q ai) <*> mapM run rest <*> pure li

run (EObject bs i) = TObject <$> mapNonEmpty "object" (\(k,v) -> (,) <$> run k <*> run v) bs i <*> pure i
run (EBinding _ i) = throwError (i,"Unexpected binding")
run (ESymbol s i) = return $ TLiteral (LString s) i
run (ELiteral l i) = return $ TLiteral l i
run (EAtom s q i) | s `elem` reserved = throwError (i,"Unexpected reserved word: " ++ s)
                  | otherwise = return $ mkVar s q i
run e = throwError (_eInfo e,"Unexpected expression: " ++ show e)
{-# INLINE run #-}

mkVar :: String -> Maybe String -> Info -> Term Name
mkVar s q i = TVar (maybe (Name s) (QName $ fromString s) q) i
{-# INLINE mkVar #-}

mapNonEmpty :: String -> (a -> Compile b) -> [a] -> Info -> Compile [b]
mapNonEmpty s _ [] i = throwError (i,"Empty " ++ s)
mapNonEmpty _ act body _ = mapM act body
{-# INLINE mapNonEmpty #-}

runNonEmpty :: String -> [Exp] -> Info -> Compile [Term Name]
runNonEmpty s = mapNonEmpty s run
{-# INLINE runNonEmpty #-}

atomText :: Exp -> Compile String
atomText (EAtom a Nothing _) = return a
atomText e = throwError (_eInfo e,"Expected unqualified atom")
{-# INLINE atomText #-}

runBody :: [Exp] -> Info -> Compile (Term Name)
runBody bs i = TList <$> runNonEmpty "body" bs i <*> pure i
{-# INLINE runBody #-}



_parseAccounts :: IO (Result [Exp])
_parseAccounts = parseF (exprs <* TF.eof) "tests/accounts.pact"

-- in GHCI:
-- _parseAccounts >>= _compile
_compile :: Result Exp -> IO (Either (Info,String) (Term Name))
_compile (Failure f) = putDoc (_errDoc f) >> error "Parse failed"
_compile (Success a) = return $ compile a


_compileStr :: String -> IO (Term Name)
_compileStr code = do
    r <- _compile (parseS expr code)
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
