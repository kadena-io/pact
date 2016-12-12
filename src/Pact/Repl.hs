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
     repl,main
    ,runRepl
    ,evalRepl,ReplMode(..)
    ,execScript,execScript'
    ,initEvalEnv
    ,evalString
    ,ReplState(..),rEnv,rEvalState,rMode,rOut,rLine
    ,pactVersion
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
import System.Exit
import Control.Concurrent
import qualified Options.Applicative as O
import Data.Monoid
import System.Directory
import System.FilePath

import Pact.Compile
import Pact.Eval
import Pact.Types
import Pact.Native
import Pact.Repl.Lib


pactVersion :: String
pactVersion = "1.0.1"

data Option =
    OVersion |
    OBuiltins |
    OLoad Bool String |
    ORepl
          deriving (Eq,Show)


replOpts :: O.Parser Option
replOpts =
    O.flag' OVersion (O.short 'v' <> O.long "version" <> O.help "Display version") <|>
    O.flag' OBuiltins (O.short 'b' <> O.long "builtins" <> O.help "List builtins") <|>
    (OLoad
     <$> O.flag False True
         (O.short 'r' <> O.long "findscript" <>
          O.help "For .pact files, attempts to find a file with same basename and extension .repl to execute.")
     <*> O.argument O.str
        (O.metavar "FILE" <> O.help "File path to compile (if .pact extension) or execute.")) <|>
    pure ORepl -- would be nice to bail on unrecognized args here

argParser :: O.ParserInfo Option
argParser = O.info (O.helper <*> replOpts)
            (O.fullDesc <> O.header "The Pact Smart Contract Language Interpreter")

_testArgs :: String -> O.ParserResult Option
_testArgs = O.execParserPure O.defaultPrefs argParser . words


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
    , _rLine :: Int
    }
makeLenses ''ReplState

type Repl a = StateT ReplState IO a

main :: IO ()
main = repl

repl :: IO ()
repl = do
  as <- O.execParser argParser
  let exitEither _ Left {} = exitFailure
      exitEither m (Right t) = m t >> exitSuccess
      exitLoad = exitEither (\_ -> hPutStrLn stderr "Load successful" >> hFlush stderr)
  case as of
    OVersion -> putStrLn $ "pact version " ++ pactVersion
    OBuiltins -> echoBuiltins
    OLoad findScript fp
        | isPactFile fp -> do
            script <- if findScript then locatePactReplScript fp else return Nothing
            case script of
              Just s -> execScript s >>= exitLoad
              Nothing -> compileOnly fp >>= exitLoad
        | otherwise -> execScript fp >>= exitLoad
    ORepl -> initReplState Interactive >>= \s -> runRepl s stdin >>= exitEither (const (return ()))

locatePactReplScript :: FilePath -> IO (Maybe FilePath)
locatePactReplScript fp = do
  let r = dropExtension fp ++ ".repl"
  b <- doesFileExist r
  return $ if b then Just r else Nothing

compileOnly :: String -> IO (Either String [Term Name])
compileOnly fp = do
  !pr <- TF.parseFromFileEx exprsOnly fp
  s <- initReplState (Script fp)
  (`evalStateT` s) $ handleParse pr $ \es -> (sequence <$> forM es (\e -> handleCompile e (return . Right)))


exprsOnly :: Parser [Exp]
exprsOnly = exprs >>= \r -> TF.eof >> return r

isPactFile :: String -> Bool
isPactFile fp = endsWith fp ".pact"


endsWith :: Eq a => [a] -> [a] -> Bool
endsWith v s = s == reverse (take (length s) (reverse v))

echoBuiltins :: IO ()
echoBuiltins = do
  defs <- view (eeRefStore.rsNatives) <$> initPureEvalEnv
  forM_ (sort $ HM.keys defs) putStrLn

runRepl :: ReplState -> Handle -> IO (Either () (Term Name))
runRepl s@ReplState{..} h =
    evalStateT (useReplLib >> loop h (_rMode /= Interactive && _rMode /= FailureTest) Nothing) s

initReplState :: MonadIO m => ReplMode -> m ReplState
initReplState m = liftIO initPureEvalEnv >>= \e -> return (ReplState e def m def 1)

initPureEvalEnv :: IO (EvalEnv LibState)
initPureEvalEnv = do
  ls <- initLibState
  e <- initEvalEnv ls repldb
  (Right rds,_) <- runEval undefined undefined replDefsMap
  return $ over (eeRefStore.rsModules) (HM.insert "repl" rds) e

initEvalEnv :: e -> PactDb e -> IO (EvalEnv e)
initEvalEnv e b = do
  mv <- newMVar e
  (Right nds,_) <- runEval undefined undefined nativeDefs -- can i coerce this??
  return $ EvalEnv (RefStore nds HM.empty) def Null def def def mv b

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
           errToUnit $ parsedCompileEval $ TF.parseString exprs d line
    rLine %= succ
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
                           rLine %= succ
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

parsedCompileEval :: TF.Result [Exp] -> Repl (Either String (Term Name))
parsedCompileEval r = do
  let fc (Left e) _ = return $ Left e
      fc _ exp = compileEval exp
  handleParse r $ \es -> foldM fc (Right (toTerm True)) es


handleCompile :: Exp -> (Term Name -> Repl (Either String a)) -> Repl (Either String a)
handleCompile exp a =
    case compile exp of
      Right t -> a t
      Left (SyntaxError i@Info {..} e) -> do
          case _iInfo of
            Just (_,d) -> do
                        mode <- use rMode
                        outStr HErr (renderPrettyString (colors mode) d)
                        outStrLn HErr $ ": error: " ++ e
            Nothing -> outStrLn HErr $ "[No location]: " ++ e
          return (Left $ show i ++ ": " ++ show e)


compileEval :: Exp -> Repl (Either String (Term Name))
compileEval exp = handleCompile exp $ \e -> pureEval (eval e)


pureEval :: Show a => Eval LibState a -> Repl (Either String a)
pureEval e = do
  (ReplState evalE evalS _ _ _) <- get
  er <- try (liftIO $ runEval evalS evalE e)
  let (r,es) = case er of
                 Left (SomeException ex) -> (Left (TxFailure (show ex)),evalS)
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
          liftIO $ hFlush stderr
          return (Left serr)

renderErr :: PactError -> Repl String
renderErr (EvalError i s) = return $ renderInfo i ++ ":" ++ s
renderErr a@(ArgsError f _ _) = return $ renderInfo (_faInfo f) ++ ":" ++ show a
renderErr a = do
  m <- use rMode
  let i = case m of
            Script f -> Info (Just (mempty,Directed (BS.fromString f) 0 0 0 0))
            _ -> Info (Just (mempty,Lines 0 0 0 0))
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



loadFile :: FilePath -> Repl (Either String (Term Name))
loadFile f = do
  cwd <- liftIO getCurrentDirectory
  catch (do
          pr <- TF.parseFromFileEx exprs f
          liftIO $ setCurrentDirectory (takeDirectory f)
          when (isPactFile f) $ rEvalState.evalRefs.rsLoaded .= HM.empty
          r <- parsedCompileEval pr
          when (isPactFile f) $ void useReplLib
          liftIO $ setCurrentDirectory cwd
          return r)
         $ \(e :: SomeException) -> do
               liftIO $ setCurrentDirectory cwd
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


useReplLib :: Repl (Either String (Term Name))
useReplLib = do
  om <- use rMode
  rMode .= Quiet
  r <- pureEval (eval (TUse "repl" def))
  rMode .= om
  return r

evalRepl' :: String -> Repl (Either String (Term Name))
evalRepl' cmd = useReplLib >> parsedCompileEval (TF.parseString exprsOnly mempty cmd)

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
            case r of Left e -> throwM (userError $ "Failure: " ++ show e); Right v -> return v

_run :: String -> IO ()
_run cmd = void $ evalRepl Interactive cmd

_testAccounts :: IO ()
_testAccounts = void $ execScript "examples/accounts/accounts.repl"

_testBench :: IO ()
_testBench = void $ execScript "tests/bench/bench"

_testCP :: IO ()
_testCP = void $ execScript "examples/cp/cp.repl"
