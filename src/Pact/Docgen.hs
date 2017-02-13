{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Pact.Docgen
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Generate function reference pandoc markdown.
--

module Pact.Docgen where

import Data.Text (replace)
import qualified Data.Text as T
import Text.Trifecta hiding (err)
import System.IO
import Control.Monad
import Data.List
import Data.Function
import Control.Monad.Catch
import Data.Monoid

import Pact.Types.Lang
import Pact.Types.Util
import Pact.Native
import Pact.Repl
import Pact.Repl.Lib

main :: IO ()
main = funDocs

data ExampleType = Exec|ExecErr|Lit

funDocs :: IO ()
funDocs = do
  h <- openFile "docs/pact-functions.md" WriteMode
  let renderSection ns = forM_ (map snd $ sortBy (compare `on` fst) ns) $ \t -> renderDocs h t
  forM_ natives $ \(sect,ns) -> do
    hPutStrLn h $ "## " ++ unpack (asString sect) ++ " {#" ++ unpack (asString sect) ++ "}"
    renderSection ns
  hPutStrLn h "## REPL-only functions {#repl-lib}"
  hPutStrLn h ""
  hPutStrLn h "The following functions are loaded magically in the interactive REPL, or in script files \
               \with a `.repl` extension. They are not available for blockchain-based execution."
  hPutStrLn h ""
  renderSection (snd replDefs)
  hClose h

renderDocs :: Show n => Handle -> Term n -> IO ()
renderDocs h TNative {..} = do
      hPutStrLn h ""
      hPutStrLn h $ "### " ++ escapeIfNecc (unpack $ asString _tNativeName) ++ " {#" ++ sanitize (unpack $ asString _tNativeName) ++ "}"
      hPutStrLn h ""
      forM_ _tFunTypes $ \FunType {..} -> do
        hPutStrLn h $ unwords (map (\(Arg n t _) -> "*" ++ unpack n ++ "*&nbsp;`" ++ show t ++ "`") _ftArgs) ++
          " *&rarr;*&nbsp;`" ++ show _ftReturn ++ "`"
        hPutStrLn h ""
      hPutStrLn h ""
      let noexs = hPutStrLn stderr $ "No examples for " ++ show _tNativeName
      case parseString parseDocs mempty (unpack _tNativeDocs) of
        Success (t,es) -> do
             hPutStrLn h t
             if null es then noexs
             else do
               hPutStrLn h "```lisp"
               forM_ es $ \e -> do
                 let (et,e') = case head e of
                                 '!' -> (ExecErr,drop 1 e)
                                 '$' -> (Lit,drop 1 e)
                                 _ -> (Exec,e)
                 case et of
                   Lit -> hPutStrLn h e'
                   _ -> do
                     hPutStrLn h $ "pact> " ++ e'
                     r <- evalRepl FailureTest e'
                     case (r,et) of
                       (Right r',_) -> hPrint h r'
                       (Left err,ExecErr) -> hPutStrLn h err
                       (Left err,_) -> throwM (userError err)
               hPutStrLn h "```"
        _ -> hPutStrLn h (unpack _tNativeDocs) >> noexs
      hPutStrLn h ""
renderDocs _ _ = return ()


escapeIfNecc :: String -> String
escapeIfNecc n | n == "+" || n == "-" = "\\" ++ n
               | otherwise = n

sanitize :: String -> String
sanitize = unpack .
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

parseDocs :: Parser (String,[String])
parseDocs = do
  t <- many (satisfy (/= '`'))
  es <- many (do
               _ <- char '`'
               e <- some (satisfy (/= '`'))
               _ <- char '`'
               _ <- optional spaces
               return e)
  return (t,es)
