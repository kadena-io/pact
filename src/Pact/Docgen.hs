{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

-- |
-- Module      :  Pact.Docgen
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Generate function reference pandoc markdown.
--

module Pact.Docgen where

import           Control.Lens         (ifor_)
import           Control.Monad
import           Control.Monad.Catch
import           Data.Foldable        (for_)
import           Data.Function
import           Data.List
import           Data.Monoid
import           Data.Text            (replace)
import qualified Data.Text            as T
import           System.IO
import           Text.Trifecta        hiding (err)

import qualified Pact.Analyze.Feature as Analyze
import           Pact.Native
import           Pact.Repl
import           Pact.Repl.Lib
import           Pact.Repl.Types
import           Pact.Types.Lang

main :: IO ()
main = do
  withFile "docs/pact-functions.md"      WriteMode renderFunctions
  withFile "docs/pact-properties-api.md" WriteMode renderProperties

data ExampleType = Exec | ExecErr | Lit

renderFunctions :: Handle -> IO ()
renderFunctions h = do
  hPutStrLn h "# Built-in Functions {#builtins}"

  let renderSection ns = forM_ (map snd $ sortBy (compare `on` fst) ns) $ \t -> renderTerm h t
  forM_ natives $ \(sect,ns) -> do
    hPutStrLn h $ "## " ++ unpack (asString sect) ++ " {#" ++ unpack (asString sect) ++ "}"
    renderSection ns
  hPutStrLn h "## REPL-only functions {#repl-lib}"
  hPutStrLn h ""
  hPutStrLn h "The following functions are loaded magically in the interactive REPL, or in script files \
               \with a `.repl` extension. They are not available for blockchain-based execution."
  hPutStrLn h ""
  renderSection (snd replDefs)

renderTerm :: Show n => Handle -> Term n -> IO ()
renderTerm h TNative {..} = do
  hPutStrLn h ""
  hPutStrLn h $ "### " ++ escapeText (unpack $ asString _tNativeName)
             ++ " {#" ++ escapeAnchor (unpack $ asString _tNativeName) ++ "}"
  hPutStrLn h ""
  forM_ _tFunTypes $ \FunType {..} -> do
    hPutStrLn h $ unwords (map (\(Arg n t _) -> "*" ++ unpack n ++ "*&nbsp;`" ++ show t ++ "`") _ftArgs) ++
      " *&rarr;*&nbsp;`" ++ show _ftReturn ++ "`"
    hPutStrLn h ""
  hPutStrLn h ""
  let noexs = hPutStrLn stderr $ "No examples for " ++ show _tNativeName
  case parseString nativeDocParser mempty (unpack _tNativeDocs) of
    Success (t,es) -> do
         hPutStrLn h t
         if null es then noexs
         else do
           hPutStrLn h "```lisp"
           forM_ es $ \e -> do
             let (et,e') = case head e of
                             '!' -> (ExecErr,drop 1 e)
                             '$' -> (Lit,drop 1 e)
                             _   -> (Exec,e)
             case et of
               Lit -> hPutStrLn h e'
               _ -> do
                 hPutStrLn h $ "pact> " ++ e'
                 r <- evalRepl FailureTest e'
                 case (r,et) of
                   (Right r',_)       -> hPrint h r'
                   (Left err,ExecErr) -> hPutStrLn h err
                   (Left err,_)       -> throwM (userError err)
           hPutStrLn h "```"
    _ -> hPutStrLn h (unpack _tNativeDocs) >> noexs
  hPutStrLn h ""
renderTerm _ _ = return ()

renderProperties :: Handle -> IO ()
renderProperties h = do
  hPutStrLn h "# Property and Invariant Functions {#properties-and-invariants}"
  hPutStrLn h ""
  hPutStrLn h "These are functions available in properties and invariants -- not necessarily in executable Pact code. All of these functions are available in properties, but only a subset are available in invariants. As a general rule, invariants have vocabulary for talking about the shape of data, whereas properties also add vocabulary for talking about function inputs and outputs, and database interactions. Each function also explicitly says whether it's available in just properties, or invariants as well."
  hPutStrLn h ""

  ifor_ Analyze.classFeatures $ \cls features -> do
    let title = T.unpack $ Analyze.classTitle cls
    hPutStrLn h $ "## " <> escapeText title <> " operators {#" <> escapeAnchor title <> "}"
    hPutStrLn h ""

    for_ features $ \feat@(Analyze.Feature (unpack -> sym) _ avail (unpack -> desc) usages) -> do
      -- NOTE: we use the feature name instead of the anchor-escaped symbol due
      -- to overloaded symbols.
      hPutStrLn h $ "### " <> escapeText sym <> " {#" <> show feat <> "}"
      hPutStrLn h ""

      for_ usages $ \(Analyze.Usage (unpack -> template) constraints formType) -> do

        hPutStrLn h "```lisp"
        hPutStrLn h template
        hPutStrLn h "```"
        hPutStrLn h ""

        -- TODO: potentially move some of this presentation logic elsewhere
        case formType of
          Analyze.Sym ty -> do
            hPutStrLn h  $ "* of type " <> showType ty
          Analyze.Fun mBindings argTys retTy -> do
            for_ mBindings $ \case
              Analyze.BindVar (Analyze.Var (unpack -> var)) (showType -> bTy) ->
                hPutStrLn h $ "* binds `" <> var <> "` of type " <> bTy
              Analyze.BindObject ->
                hPutStrLn h $ "* destructures the provided object"
            for_ argTys $ \(Analyze.Var (unpack -> arg), showType -> aTy) ->
              hPutStrLn h $ "* takes `" <> arg <> "` of type " <> aTy
            hPutStrLn h $ "* produces type " <> showType retTy

        ifor_ constraints $ \(Analyze.TypeVar (unpack -> tv)) constraint ->
          hPutStrLn h $ "* where _" <> tv <> "_ is "
                     <> case constraint of
                          Analyze.AnyType          -> "_any type_"
                          Analyze.OneOf [ct1, ct2] -> "of type "
                            <> showConTy ct1 <> " or " <> showConTy ct2
                          Analyze.OneOf (reverse -> last' : xs) -> "of type "
                            <> join (fmap ((<> ", ") . showConTy) (reverse xs))
                            <> " or " <> showConTy last'
                          Analyze.OneOf _ -> "ERROR"
        hPutStrLn h ""

      hPutStrLn h desc
      hPutStrLn h ""

      hPutStrLn h $ "Supported in "
                 <> case avail of
                      Analyze.PropOnly   -> "properties only."
                      Analyze.InvAndProp -> "either invariants or properties."
      hPutStrLn h ""

  where
    showConTy :: Analyze.ConcreteType -> String
    showConTy (Analyze.ConcreteType (unpack -> ct)) = "`" ++ ct ++ "`"

    showType :: Analyze.Type -> String
    showType (Analyze.TyCon ct) = showConTy ct
    showType (Analyze.TyVar (Analyze.TypeVar (unpack -> tv))) = "_" ++ tv ++ "_"

escapeText :: String -> String
escapeText n
  | n == "+" || n == "-"
  = "\\" ++ n
  | otherwise
  = n

escapeAnchor :: String -> String
escapeAnchor = unpack .
  replace "=" "eq" .
  replace "<" "lt" .
  replace ">" "gt" .
  replace "!" "bang" .
  replace "*" "star" .
  replace "+" "plus" .
  replace "/" "slash" .
  replace "^" "hat" .
  (\t -> if T.take 1 t == "-"
         then "minus" <> T.drop 1 t else t) .
  pack

nativeDocParser :: Parser (String,[String])
nativeDocParser = do
  t <- many $ satisfy (/= '`')
  es <- many $ do
    _ <- char '`'
    e <- some (satisfy (/= '`'))
    _ <- char '`'
    _ <- optional spaces
    return e
  return (t,es)
