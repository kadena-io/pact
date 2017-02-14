{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      :  Pact.Repl
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- REPL/interactive interpreter for Pact. Includes
-- "Repl lib" automatically for non-blockchain interactive
-- functionality.
--

module Pact.Repl
    (
     repl,runRepl
    ,evalRepl,ReplMode(..)
    ,execScript,execScript'
    ,initEvalEnv,initPureEvalEnv,initReplState
    ,handleParse,handleCompile
    ,isPactFile
    ,evalString
    ,ReplState(..),rEnv,rEvalState,rMode,rOut
    ) where

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
import Prelude hiding (exp,print,putStrLn,putStr,interact)
import Text.Trifecta as TF hiding (line,err,try,newline)
import System.IO hiding (interact)
import Text.Trifecta.Delta
import Control.Concurrent
import Data.Monoid
import System.FilePath

import Pact.Compile
import Pact.Eval
import Pact.Types.Runtime
import Pact.Native
import Pact.Repl.Lib



data ReplMode =
    Interactive |
    Script String |
    FailureTest |
    Quiet |
    StringEval
    deriving (Eq,Show)

data Hdl = HOut|HErr

data ReplState = ReplState {
      _rEnv :: EvalEnv LibState
    , _rEvalState :: EvalState
    , _rMode :: ReplMode
    , _rOut :: String
    , _rFile :: Maybe FilePath
    }
makeLenses ''ReplState

type Repl a = StateT ReplState IO a

repl :: IO (Either () (Term Name))
repl = initReplState Interactive >>= \s -> runRepl s stdin

isPactFile :: String -> Bool
isPactFile fp = endsWith fp ".pact"

endsWith :: Eq a => [a] -> [a] -> Bool
endsWith v s = s == reverse (take (length s) (reverse v))

runRepl :: ReplState -> Handle -> IO (Either () (Term Name))
runRepl s@ReplState{..} h =
    evalStateT (useReplLib >> loop h (_rMode /= Interactive && _rMode /= FailureTest) Nothing) s

initReplState :: MonadIO m => ReplMode -> m ReplState
initReplState m = liftIO initPureEvalEnv >>= \e -> return (ReplState e def m def def)

initPureEvalEnv :: IO (EvalEnv LibState)
initPureEvalEnv = do
  ls <- initLibState
  set eeTxId (Just 0) <$> initEvalEnv ls repldb

errToUnit :: Functor f => f (Either e a) -> f (Either () a)
errToUnit a = either (const (Left ())) Right <$> a

loop :: Handle -> Bool -> Maybe (Term Name) -> Repl (Either () (Term Name))
loop h abortOnError lastResult = do
  interact $ outStr HOut "pact> "
  isEof <- liftIO (hIsEOF h)
  let retVal = maybe rSuccess (return.Right) lastResult
  if isEof then retVal else do
    line <- trim <$> liftIO (hGetLine h) >>= checkMultiline h
    r <- if null line then rSuccess
         else do
           d <- getDelta
           errToUnit $ parsedCompileEval line $ TF.parseString exprs d line
    case r of
      Left _ | abortOnError -> do
                 outStrLn HErr "Aborting execution"
                 return r
             | otherwise -> loop h abortOnError Nothing
      Right t -> loop h abortOnError (Just t)


getDelta :: Repl Delta
getDelta = do
  m <- use rMode
  case m of
    (Script file) -> return $ Directed (BS.fromString file) 0 0 0 0
    _ -> return mempty

checkMultiline :: Handle -> String -> Repl String
checkMultiline h l | last3 == "<<<" = runMulti allButLast3
                 where revl = reverse l
                       last3 = take 3 revl
                       allButLast3 = reverse (drop 3 revl)
                       runMulti s = do
                         isEof <- liftIO $ hIsEOF h
                         if isEof then return s else do
                           interact $ outStr HOut "> "
                           line <- trim <$> liftIO (hGetLine h)
                           if line == "" then return (trim s) else runMulti (s ++ " " ++ line)
checkMultiline _ l = return l

handleParse :: TF.Result [Exp] -> ([Exp] -> Repl (Either String a)) -> Repl (Either String a)
handleParse (TF.Failure e) _ = do
  mode <- use rMode
  outStrLn HErr (renderPrettyString (colors mode) (_errDoc e))
  return (Left (renderCompactString $ _errDoc e))
handleParse (TF.Success es) a = a es

colors :: ReplMode -> RenderColor
colors Interactive = RColor
colors _ = RPlain

parsedCompileEval :: String -> TF.Result [Exp] -> Repl (Either String (Term Name))
parsedCompileEval src r = do
  let fc (Left e) _ = return $ Left e
      fc _ exp = compileEval src exp
  handleParse r $ \es -> foldM fc (Right (toTerm True)) es


handleCompile :: String -> Exp -> (Term Name -> Repl (Either String a)) -> Repl (Either String a)
handleCompile src exp a =
    case compile (mkStringInfo src) exp of
      Right t -> a t
      Left (SyntaxError i@Info {..} e) -> do
          case _iInfo of
            Just (_,d) -> do
                        mode <- use rMode
                        outStr HErr (renderPrettyString (colors mode) (_pDelta d))
                        outStrLn HErr $ ": error: " ++ e
            Nothing -> outStrLn HErr $ "[No location]: " ++ e
          return (Left $ show i ++ ": " ++ show e)


compileEval :: String -> Exp -> Repl (Either String (Term Name))
compileEval src exp = handleCompile src exp $ \e -> pureEval (eval e)


pureEval :: Show a => Eval LibState a -> Repl (Either String a)
pureEval e = do
  (ReplState evalE evalS _ _ _) <- get
  er <- try (liftIO $ runEval evalS evalE e)
  let (r,es) = case er of
                 Left (SomeException ex) -> (Left (TxFailure (pack $ show ex)),evalS)
                 Right v -> v
  mode <- use rMode
  case r of
    Right a -> do
        when (mode == Interactive || mode == StringEval) $ outStrLn HOut (show a)
        rEvalState .= es
        updateForOp a
    Left err -> do
        serr <- renderErr err
        if mode == FailureTest || mode == StringEval
        then return (Left serr)
        else do
          let cs = _evalCallStack es
          if null cs
          then outStrLn HErr serr
          else do
            outStrLn HErr $ show (head cs) ++ ": " ++ show err
            mapM_ (\c -> outStrLn HErr $ " at " ++ show c) (tail cs)
          return (Left serr)

renderErr :: PactError -> Repl String
renderErr (EvalError i s) = return $ renderInfo i ++ ": " ++ unpack s
renderErr a@(ArgsError f _ _) = return $ renderInfo (_faInfo f) ++ ": " ++ show a
renderErr a = do
  m <- use rMode
  let i = case m of
            Script f -> Info (Just (mempty,Parsed (Directed (BS.fromString f) 0 0 0 0) 0))
            _ -> Info (Just (mempty,Parsed (Lines 0 0 0 0) 0))
  return $ renderInfo i ++ ":" ++ show a


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


-- | load and evaluate a Pact file.
-- Track file and use current file to mangle directory as necessary.
loadFile :: FilePath -> Repl (Either String (Term Name))
loadFile f = do
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
          pr <- TF.parseFromFileEx exprs computedPath
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

out :: ReplMode -> Hdl -> Bool -> String -> Repl ()
out m hdl newline str =
  case m of
    StringEval -> rOut %= (\s -> s ++ str ++ (if newline then "\n" else ""))
    _ -> liftIO $ do
           let h = case hdl of HOut -> stdout; HErr -> stderr
           (if newline then hPutStrLn else hPutStr) h str
           hFlush h


outStr :: Hdl -> String -> Repl ()
outStr h s = use rMode >>= \m -> out m h False s
outStrLn :: Hdl -> String -> Repl ()
outStrLn h s = use rMode >>= \m -> out m h True s


trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace


isInteractive :: Repl Bool
isInteractive = (== Interactive) <$> use rMode

interact :: Repl () -> Repl ()
interact act = isInteractive >>= \b -> when b act

rSuccess :: Monad m => m (Either a (Term Name))
rSuccess = return $ Right $ toTerm True

execScript :: FilePath -> IO (Either () (Term Name))
execScript f = errToUnit (fst <$> execScript' (Script f) f)

execScript' :: ReplMode -> FilePath -> IO (Either String (Term Name),ReplState)
execScript' m fp = do
  s <- initReplState m
  runStateT (useReplLib >> loadFile fp) s


useReplLib :: Repl ()
useReplLib = rEvalState.evalRefs.rsLoaded %= HM.union (moduleToMap replDefs)


evalRepl' :: String -> Repl (Either String (Term Name))
evalRepl' cmd = useReplLib >> parsedCompileEval cmd (TF.parseString exprsOnly mempty cmd)

evalRepl :: ReplMode -> String -> IO (Either String (Term Name))
evalRepl m cmd = initReplState m >>= evalStateT (evalRepl' cmd)

evalString :: Bool -> String -> IO Value
evalString showLog cmd = do
  (er,s) <- initReplState StringEval >>= runStateT (evalRepl' cmd)
  return $ object $ case (showLog,er) of
    (False,Right v) -> [ "success" A..= v]
    (True,Right _) -> ["success" A..= trim (_rOut s) ]
    (False,Left e) -> ["failure" A..= e ]
    (True,Left e) -> ["failure" A..= (_rOut s ++ e) ]



_eval :: String -> IO (Term Name)
_eval cmd = evalRepl (Script "_eval") cmd >>= \r ->
            case r of Left e -> throwM (userError $ " Failure: " ++ show e); Right v -> return v

_run :: String -> IO ()
_run cmd = void $ evalRepl Interactive cmd

_testAccounts :: IO ()
_testAccounts = void $ execScript "examples/accounts/accounts.repl"

_testBench :: IO ()
_testBench = void $ execScript "tests/bench/bench"

_testCP :: IO ()
_testCP = void $ execScript "examples/cp/cp.repl"
