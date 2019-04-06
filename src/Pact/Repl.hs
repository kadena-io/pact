{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
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
  ( errToUnit
  , execScript
  , execScript'
  , evalPact
  , evalRepl
  , evalRepl'
  , evalString
  , handleCompile
  , handleParse
  , initPureEvalEnv
  , initReplState
  , isPactFile
  , parsedCompileEval
  , rSuccess
  , repl
  , runPipedRepl
  , setReplLib
  , unsetReplLib
  , utf8BytesLength
  ) where

import Control.Applicative
import Control.Lens hiding (op)
import Control.Monad.Catch
import Control.Monad.State.Strict
import Data.Aeson hiding ((.=),Object)
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS
import Data.Char
import Data.Default
import Data.List
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import Prelude hiding (exp)
import Text.Trifecta as TF hiding (line,err,try,newline)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import GHC.Word (Word8)
import System.IO
import Text.Trifecta.Delta
import Control.Concurrent
import Data.Monoid (appEndo)
import System.FilePath

import Pact.Compile
import Pact.Parse
import Pact.Eval
import Pact.Types.Pretty hiding (line)
import Pact.Types.Runtime
import Pact.Native
import Pact.Repl.Lib
import Pact.Types.Logger
import Pact.Repl.Types
import Pact.Types.Hash
import Pact.Gas

-- | for use in GHCI
repl :: IO (Either () (Term Name))
repl = repl' Interactive

repl' :: ReplMode -> IO (Either () (Term Name))
repl' m = initReplState m Nothing >>= \s -> runPipedRepl' (m == Interactive) s stdin

isPactFile :: String -> Bool
isPactFile fp = endsWith fp ".pact"

endsWith :: Eq a => [a] -> [a] -> Bool
endsWith v s = s == reverse (take (length s) (reverse v))

runPipedRepl :: ReplState -> Handle -> IO (Either () (Term Name))
runPipedRepl = runPipedRepl' False

runPipedRepl' :: Bool -> ReplState -> Handle -> IO (Either () (Term Name))
runPipedRepl' p s@ReplState{..} h =
    evalStateT (useReplLib >> pipeLoop p h Nothing) s

initReplState :: MonadIO m => ReplMode -> Maybe String -> m ReplState
initReplState m verifyUri =
  liftIO (initPureEvalEnv verifyUri) >>= \e -> return (ReplState e def m def def def)

initPureEvalEnv :: Maybe String -> IO (EvalEnv LibState)
initPureEvalEnv verifyUri = do
  mv <- initLibState neverLog verifyUri >>= newMVar
  return $ EvalEnv (RefStore nativeDefs mempty) def Null (Just 0)
    def def mv repldb def initialHash freeGasEnv permissiveNamespacePolicy (SPVSupport $ spv mv) def


spv :: MVar (LibState) -> Text -> Object Name -> IO (Either Text (Object Name))
spv mv ty pay = readMVar mv >>= \LibState{..} -> case M.lookup (SPVMockKey (ty,pay)) _rlsMockSPV of
  Nothing -> return $ Left $ "SPV verification failure"
  Just o -> return $ Right o

errToUnit :: Functor f => f (Either e a) -> f (Either () a)
errToUnit a = either (const (Left ())) Right <$> a

toUTF8Bytes :: String -> [Word8]
toUTF8Bytes = BS.unpack . encodeUtf8 . Text.pack

utf8BytesLength :: String -> Int
utf8BytesLength = length . toUTF8Bytes

-- | Main loop for non-interactive (piped) input
pipeLoop :: Bool -> Handle -> Maybe (Term Name) -> Repl (Either () (Term Name))
pipeLoop prompt h lastResult = do
  when prompt $ outStr HOut "pact> "
  isEof <- liftIO (hIsEOF h)
  let retVal = maybe rSuccess (return.Right) lastResult
  if isEof then retVal else do
    line <- trim <$> liftIO (hGetLine h)
    r <- if null line then rSuccess
         else do
           d <- getDelta
           errToUnit $ parsedCompileEval line $ TF.parseString exprsOnly d line
    case r of
      Left _ -> do
        outStrLn HErr "Aborting execution"
        return r
      Right t -> pipeLoop prompt h (Just t)

getDelta :: Repl Delta
getDelta = do
  m <- use rMode
  case m of
    (Script _ file) -> return $ Directed (BS.fromString file) 0 0 0 0
    _ -> return mempty

handleParse :: TF.Result [Exp Parsed] -> ([Exp Parsed] -> Repl (Either String a)) -> Repl (Either String a)
handleParse (TF.Failure e) _ = do
  mode <- use rMode
  let errDoc = _errDoc e
  outStrLn HErr $ renderPrettyString' (colors mode) $ unAnnotate $ fromAnsiWlPprint errDoc
  return $ Left $ renderCompactString' $ unAnnotate $ fromAnsiWlPprint errDoc
handleParse (TF.Success es) a = a es

colors :: ReplMode -> RenderColor
colors Interactive = RColor
colors _ = RPlain

parsedCompileEval :: String -> TF.Result [Exp Parsed] -> Repl (Either String (Term Name))
parsedCompileEval src r = do
  let fc (Left e) _ = return $ Left e
      fc _ exp = compileEval src exp
  handleParse r $ \es -> foldM fc (Right (toTerm True)) es


handleCompile :: String -> Exp Parsed -> (Term Name -> Repl (Either String a)) -> Repl (Either String a)
handleCompile src exp a =
    case compile (mkStringInfo src) exp of
      Right t -> a t
      Left er -> do
          case _iInfo (peInfo er) of
            Just (_,d) -> do
                        mode <- use rMode
                        outStr HErr (renderPrettyString (colors mode) (_pDelta d))
                        outStrLn HErr $ ": error: " ++ renderCompactString' (peDoc er)
            Nothing -> outStrLn HErr $ "[No location]: " ++ renderCompactString' (peDoc er)
          Left <$> renderErr er

compileEval :: String -> Exp Parsed -> Repl (Either String (Term Name))
compileEval src exp = handleCompile src exp $ \e -> pureEval (_tInfo e) (eval e)


pureEval :: Info -> Eval LibState (Term Name) -> Repl (Either String (Term Name))
pureEval ei e = do
  (ReplState evalE evalS _ _ _ _) <- get
  er <- try (liftIO $ runEval' evalS evalE e)
  let (r,es) = case er of
                 Left (SomeException ex) -> (Left (PactError EvalError def def (prettyString (show ex))),evalS)
                 Right v -> v
  mode <- use rMode
  case r of
    Right a -> do
        doOut ei mode a
        case _evalPactExec es of
          Nothing -> return ()
          Just pe -> do
            use (rEnv.eePactDbVar) >>= \mv -> liftIO $ modifyMVar_ mv $ return . over rlsPacts (M.insert (_pePactId pe) pe)
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

doOut :: Info -> ReplMode -> Term Name -> Repl ()
doOut ei mode a = case mode of
  Interactive -> plainOut
  StringEval -> rTermOut %= (a:)
  StdinPipe -> plainOut
  Script True _ -> lineOut
  _ -> return ()
  where
    plainOut = outStrLn HOut $ show $ pretty a
    lineOut = do
      outStrLn HErr $ renderInfo ei ++ ":Trace: " ++ show a

renderErr :: PactError -> Repl String
renderErr a
  | peInfo a == def = do
      m <- use rMode
      let i = case m of
                Script _ f -> Info (Just (mempty,Parsed (Directed (BS.fromString f) 0 0 0 0) 0))
                _ -> Info (Just (mempty,Parsed (Lines 0 0 0 0) 0))
      return $ renderInfo i ++ ":" ++ renderCompactString' (peDoc a)
  | otherwise = return $ renderInfo (peInfo a) ++ ": " ++ renderCompactString' (peDoc a)

updateForOp :: Term Name -> Repl (Either String (Term Name))
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
                  when reset $ do
                    replState <- liftIO $ readMVar mv
                    let verifyUri = _rlsVerifyUri replState
                    (initReplState mode verifyUri >>= put >> void useReplLib)
                  (a <$) <$> loadFile fp
    TcErrors es -> forM_ es (outStrLn HErr) >> return (Right a)
    Print t -> do
      let rep = case t of TLitString s -> unpack s
                          _ -> show t
      outStrLn HOut rep
      return (Right a)
    Tx i t n -> doTx i t n

doTx :: Info -> Tx -> Maybe Text -> Repl (Either String (Term Name))
doTx i t n = do
  e <- case t of
    Begin -> do
      rEnv.eeTxId %= fmap succ
      return $ evalBeginTx i
    Rollback -> return $ evalRollbackTx i
    Commit -> return $ void $ evalCommitTx i
  pureEval i (e >> return (tStr "")) >>= \r -> forM r $ \_ -> do
    case t of
      Commit -> do
        newmods <- use (rEvalState . evalRefs . rsNewModules)
        rEnv . eeRefStore . rsModules %= HM.union newmods
      _ -> return ()
    rEvalState .= def
    useReplLib
    tid <- use $ rEnv . eeTxId
    return $ tStr $ tShow t <> " Tx " <> tShow tid <> maybe "" (": " <>) n


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

out :: ReplMode -> Hdl -> Bool -> String -> Repl ()
out m hdl newline str =
  case m of
    Quiet -> return ()
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


rSuccess :: Monad m => m (Either a (Term Name))
rSuccess = return $ Right $ toTerm True

-- | Workhorse to load script; also checks/reports test failures
execScript :: Bool -> FilePath -> IO (Either () (Term Name))
execScript dolog f = do
  (r,ReplState{..}) <- execScript' (Script dolog f) f
  case r of
    Left _ -> return $ Left ()
    Right t -> do
      LibState{..} <- readMVar $ _eePactDbVar _rEnv
      fs <- fmap sequence $ forM _rlsTests $ \TestResult{..} -> case trFailure of
        Nothing -> return (Just ())
        Just (i,e) -> do
          hPutStrLn stderr $ renderInfo (_faInfo i) ++ ": " ++ unpack e
          return Nothing
      maybe (return $ Left ()) (const (return (Right t))) fs


execScript' :: ReplMode -> FilePath -> IO (Either String (Term Name),ReplState)
execScript' m fp = do
  s <- initReplState m Nothing
  runStateT (useReplLib >> loadFile fp) s


-- | install repl lib functions into monad state
useReplLib :: Repl ()
useReplLib = id %= setReplLib

-- | mutate repl state to install lib functions
setReplLib :: ReplState -> ReplState
setReplLib = over (rEvalState.evalRefs.rsLoaded) $ HM.union (moduleToMap replDefs)

-- | mutate repl state to remove lib functions
unsetReplLib :: ReplState -> ReplState
unsetReplLib = over (rEvalState.evalRefs.rsLoaded) (`HM.difference` (moduleToMap replDefs))

-- | evaluate string in repl monad
evalPact :: String -> Repl (Either String (Term Name))
evalPact cmd = parsedCompileEval cmd (TF.parseString exprsOnly mempty cmd)

-- | evaluate string in repl monad, loading lib functions first.
evalRepl' :: String -> Repl (Either String (Term Name))
evalRepl' cmd = useReplLib >> evalPact cmd

evalRepl :: ReplMode -> String -> IO (Either String (Term Name))
evalRepl m cmd = initReplState m Nothing >>= evalStateT (evalRepl' cmd)

evalString :: Bool -> String -> IO Value
evalString showLog cmd = do
  (er,s) <- initReplState StringEval Nothing >>= runStateT (evalRepl' cmd)
  return $ object $ case (showLog,er) of
    (False,Right v) -> [ "success" A..= v]
    (True,Right _) -> ["success" A..= trim (_rOut s) ]
    (False,Left e) -> ["failure" A..= e ]
    (True,Left e) -> ["failure" A..= (_rOut s ++ e) ]



_eval :: String -> IO (Term Name)
_eval cmd = evalRepl (Script True "_eval") cmd >>= \r ->
            case r of Left e -> throwM (userError $ " Failure: " ++ show e); Right v -> return v

_run :: String -> IO ()
_run cmd = void $ evalRepl Interactive cmd

_testAccounts :: IO ()
_testAccounts = void $ execScript False "examples/accounts/accounts.repl"

_testBench :: IO ()
_testBench = void $ execScript False "tests/bench/bench"

_testCP :: IO ()
_testCP = void $ execScript False "examples/cp/cp.repl"
