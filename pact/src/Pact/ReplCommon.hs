{-# LANGUAGE CPP             #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      :  Pact.ReplCommon
-- Copyright   :  (C) 2018 Kadena
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- REPL/interactive interpreter for Pact. Includes
-- "Repl lib" automatically for non-blockchain interactive
-- functionality.
--

module Pact.ReplCommon where

import Control.Applicative
import Control.Lens hiding (op)
import Control.Monad.Catch
import Control.Monad.State.Strict
import Data.Aeson hiding ((.=))
import qualified Data.Aeson as A
import qualified Data.ByteString.UTF8 as BS
import Data.Char
import Data.Default
import Data.List
import qualified Data.HashMap.Strict as HM
import Prelude hiding (exp)
import Text.Trifecta as TF hiding (err,try,newline)
import System.IO
import Text.Trifecta.Delta
import Control.Concurrent
import Data.Monoid
import System.FilePath

import Pact.Compile
import Pact.Parse
import Pact.Eval
import Pact.Types.Runtime
import Pact.Native
import Pact.Repl.Lib
import Pact.Types.Logger
import Pact.Repl.Types

evalString :: Bool -> String -> IO Value
evalString showLog cmd = do
  (er,s) <- initReplState StringEval >>= runStateT (evalRepl' cmd)
  return $ object $ case (showLog,er) of
    (False,Right v) -> [ "success" A..= v]
    (True,Right _) -> ["success" A..= trim (_rOut s) ]
    (False,Left e) -> ["failure" A..= e ]
    (True,Left e) -> ["failure" A..= (_rOut s ++ e) ]

initReplState :: MonadIO m => ReplMode -> m ReplState
initReplState m = liftIO initPureEvalEnv >>= \e -> return (ReplState e def m def def)

initPureEvalEnv :: IO (EvalEnv LibState)
initPureEvalEnv = do
  ls <- initLibState neverLog
  set eeTxId (Just 0) <$> initEvalEnv ls repldb

evalRepl' :: String -> Repl (Either String (Term Name))
evalRepl' cmd = useReplLib >> parsedCompileEval cmd (TF.parseString exprsOnly mempty cmd)

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

useReplLib :: Repl ()
useReplLib = id %= setReplLib

setReplLib :: ReplState -> ReplState
setReplLib = rEvalState.evalRefs.rsLoaded %~ HM.union (moduleToMap replDefs)

compileEval :: String -> Exp -> Repl (Either String (Term Name))
compileEval src exp = handleCompile src exp $ \e -> pureEval (_tInfo e) (eval e)

parsedCompileEval :: String -> TF.Result [Exp] -> Repl (Either String (Term Name))
parsedCompileEval src r = do
  let fc (Left e) _ = return $ Left e
      fc _ exp = compileEval src exp
  handleParse r $ \es -> foldM fc (Right (toTerm True)) es

handleParse :: TF.Result [Exp] -> ([Exp] -> Repl (Either String a)) -> Repl (Either String a)
handleParse (TF.Failure e) _ = do
  mode <- use rMode
  outStrLn HErr (renderPrettyString (colors mode) (_errDoc e))
  return (Left (renderCompactString $ _errDoc e))
handleParse (TF.Success es) a = a es

handleCompile :: String -> Exp -> (Term Name -> Repl (Either String a)) -> Repl (Either String a)
handleCompile src exp a =
    case compile (mkStringInfo src) exp of
      Right t -> a t
      Left er -> do
          case _iInfo (peInfo er) of
            Just (_,d) -> do
                        mode <- use rMode
                        outStr HErr (renderPrettyString (colors mode) (_pDelta d))
                        outStrLn HErr $ ": error: " ++ unpack (peText er)
            Nothing -> outStrLn HErr $ "[No location]: " ++ unpack (peText er)
          return (Left $ show er)

pureEval :: Show a => Info -> Eval LibState a -> Repl (Either String a)
pureEval ei e = do
  (ReplState evalE evalS _ _ _) <- get
  er <- try (liftIO $ runEval' evalS evalE e)
  let (r,es) = case er of
                 Left (SomeException ex) -> (Left (EvalError def def (pack $ show ex)),evalS)
                 Right v -> v
  mode <- use rMode
  case r of
    Right a -> do
        doOut ei mode a
        rEvalState .= es
        updateForOp a
    Left err -> do
        serr <- renderErr err
        if mode == FailureTest || mode == StringEval
          then return (Left serr)
          else do
            let cs = peCallStack err
            if null cs
              then outStrLn HErr serr
              else do
              -- synthesize error at outer callsite
              let lastErr = last cs
                  outerErr = err { peInfo = _sfLoc lastErr }
              if peInfo outerErr == peInfo err
                then outStrLn HErr serr
                else do
                renderErr outerErr >>= outStrLn HErr
                outStrLn HErr (" at " ++ serr)
              mapM_ (\c -> outStrLn HErr $ " at " ++ show c) cs
            return (Left serr)

outStr :: Hdl -> String -> Repl ()
outStr h s = use rMode >>= \m -> out m h False s
outStrLn :: Hdl -> String -> Repl ()
outStrLn h s = use rMode >>= \m -> out m h True s

colors :: ReplMode -> RenderColor
colors Interactive = RColor
colors _ = RPlain

doOut :: Show t => Info -> ReplMode -> t -> Repl ()
doOut ei mode a = case mode of
  Interactive -> plainOut
  StringEval -> plainOut
  StdinPipe -> plainOut
  Script True _ -> lineOut
  _ -> return ()
  where
    plainOut = outStrLn HOut (show a)
    lineOut = do
      outStrLn HErr $ renderInfo ei ++ ":Trace: " ++ show a

updateForOp :: a -> Repl (Either String a)
updateForOp a = do
  mv <- use (rEnv.eePactDbVar)
  mode <- use rMode
  op <- liftIO $ modifyMVar mv $ \v -> return (set rlsOp Noop v,view rlsOp v)
  case op of
    Noop -> return (Right a)
    UpdateEnv e -> do
      rEnv %= appEndo e
      return (Right a)
    Load fp reset -> do
                  when reset (initReplState mode >>= put >> void useReplLib)
                  (a <$) <$> loadFile fp
    TcErrors es -> forM_ es (outStrLn HErr) >> return (Right a)
    Print t -> do
      let rep = case t of TLitString s -> unpack s
                          _ -> show t
      outStrLn HOut rep
      return (Right a)

renderErr :: PactError -> Repl String
renderErr a
  | peInfo a == def = do
      m <- use rMode
      let i = case m of
                Script _ f -> Info (Just (mempty,Parsed (Directed (BS.fromString f) 0 0 0 0) 0))
                _ -> Info (Just (mempty,Parsed (Lines 0 0 0 0) 0))
      return $ renderInfo i ++ ":" ++ unpack (peText a)
  | otherwise = return $ renderInfo (peInfo a) ++ ": " ++ unpack (peText a)

out :: ReplMode -> Hdl -> Bool -> String -> Repl ()
out m hdl newline str =
  case m of
    Quiet -> return ()
    StringEval -> rOut %= (\s -> s ++ str ++ (if newline then "\n" else ""))
    _ -> liftIO $ do
           let h = case hdl of HOut -> stdout; HErr -> stderr
           (if newline then hPutStrLn else hPutStr) h str
           hFlush h

-- | load and evaluate a Pact file.
-- Track file and use current file to mangle directory as necessary.
loadFile :: FilePath -> Repl (Either String (Term Name))
loadFile f = do
#if !defined(ghcjs_HOST_OS)
  curFileM <- use rFile
  let computedPath = case curFileM of
        Nothing -> f -- no current file, just use f
        Just curFile
          | isAbsolute f -> f -- absolute always wins
          | takeFileName curFile == curFile -> f -- current with no directory loses
          | otherwise -> combine (takeDirectory curFile) f -- otherwise start with dir of curfile
      restoreFile = rFile .= curFileM
  rFile .= Just computedPath
  catch (do
          pr <- TF.parseFromFileEx exprsOnly computedPath
          src <- liftIO $ readFile computedPath
          when (isPactFile f) $ rEvalState.evalRefs.rsLoaded .= HM.empty
          r <- parsedCompileEval src pr
          when (isPactFile f) $ void useReplLib
          restoreFile
          return r)
         $ \(e :: SomeException) -> do
               restoreFile
               outStrLn HErr $ "load: file load failed: " ++ f ++ ", " ++ show e
               return (Left (show e))
#else
  return $ Left "Error: Load command not available in web version"
#endif

isPactFile :: String -> Bool
isPactFile fp = endsWith fp ".pact"

endsWith :: Eq a => [a] -> [a] -> Bool
endsWith v s = s == reverse (take (length s) (reverse v))
