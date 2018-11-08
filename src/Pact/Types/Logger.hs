{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Pact.Types.Logger where

import Data.Hashable
import Data.Aeson
import Data.String
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import Data.Default
import GHC.Generics
import Control.Monad.IO.Class
import Control.Monad.Reader
import Prelude hiding (log)
import Control.Arrow
import Data.Maybe
#if !defined(ghcjs_HOST_OS)
import Data.List
import Data.Yaml as Y
#endif
import qualified Data.Text as T

import Pact.Types.Util

newtype LogName = LogName String
  deriving (Eq,Show,Ord,Hashable,IsString,AsString,ToJSON,FromJSON)

data Logger = Logger {
  logName :: LogName,
  logDesc :: Maybe String,
  logLog :: String -> String -> IO ()
  }
instance Show Logger where show l = T.unpack (asString (logName l)) ++ maybe "" (": " ++) (logDesc l)

class MonadIO m => Logging m where
  log :: String -> String -> m ()

logInfo :: Logging m => String -> m ()
logInfo = log "INFO"

logError :: Logging m => String -> m ()
logError = log "ERROR"

logDebug :: Logging m => String -> m ()
logDebug = log "DEBUG"

newtype Loggers = Loggers { newLogger :: LogName -> Logger }

data LogRule = LogRule {
  enable :: Maybe Bool,
  include :: Maybe (HS.HashSet String),
  exclude :: Maybe (HS.HashSet String)
  } deriving (Eq,Generic)
instance ToJSON LogRule
instance FromJSON LogRule
instance Default LogRule where def = LogRule def def def
instance Show LogRule where
  show LogRule {..} = "{ enable = " ++ show (fromMaybe True enable) ++
                      ", include = " ++ show (HS.toList (fromMaybe HS.empty include)) ++
                      ", exclude = " ++ show (HS.toList (fromMaybe HS.empty exclude)) ++ " }"

newtype LogRules = LogRules { logRules :: HM.HashMap LogName LogRule }
  deriving (Eq,Show,Generic)
instance ToJSON LogRules where
  toJSON = toJSON . M.fromList . map (first asString) . HM.toList . logRules
instance FromJSON LogRules where
  parseJSON = fmap (LogRules . HM.fromList . map (first (LogName . T.unpack)) . M.toList) . parseJSON
instance Default LogRules where def = LogRules HM.empty


initLoggers :: (String -> IO ()) -> LoggerLogFun -> LogRules -> Loggers
initLoggers out logFn rules = Loggers $ mkLogger out logFn rules

constLoggers :: Logger -> Loggers
constLoggers l = Loggers $ const l

alwaysLog :: Loggers
alwaysLog = initLoggers putStrLn doLog def

neverLog :: Loggers
neverLog = initLoggers (return . const ()) (\_ _ _ _ -> return ()) def

type LoggerLogFun = (String -> IO ()) -> LogName -> String -> String -> IO ()

mkLogger :: (String -> IO ()) -> LoggerLogFun -> LogRules -> LogName -> Logger
mkLogger out logFn lr ln = case HM.lookup ln (logRules lr) of
  Nothing -> Logger ln Nothing (logFn out ln)
  Just rs | Just False == enable rs -> Logger ln (Just (show rs)) (\_ _ -> return ())
          | otherwise -> Logger ln (Just (show rs)) (logMaybe rs (logFn out ln))

logMaybe :: LogRule -> (String -> String -> IO ()) -> String -> String -> IO ()
logMaybe rule lf cat msg =
  when (included (include rule) && not (excluded (exclude rule))) $ lf cat msg
  where
    included Nothing = True
    included (Just s) = cat `HS.member` s
    excluded Nothing = False
    excluded (Just s) = cat `HS.member` s
{-# INLINE logMaybe #-}

doLog :: LoggerLogFun
doLog lf (LogName n) cat msg = lf $ "[" ++ n ++ "] " ++ cat ++ " " ++ msg
{-# INLINE doLog #-}

newtype LogIO a = Foo { logIO :: (ReaderT Logger IO) a } deriving (MonadReader Logger,MonadIO,Functor,Applicative,Monad)

instance Logging LogIO where
  log c s = ask >>= \l -> liftIO $ logLog l c s

runLogIO :: Logger -> LogIO a -> IO a
runLogIO logger a = runReaderT (logIO a) logger

#if !defined(ghcjs_HOST_OS)
_test :: IO ()
_test = do
  let config = "Default: {}       \n" <>
               "Disabled:         \n" <>
               "  enable: false   \n" <>
               "Enabled:          \n" <>
               "  enable: true    \n" <>
               "IncludeINFO:      \n" <>
               "  include: [INFO] \n" <>
               "ExcludeINFO:      \n" <>
               "  exclude: [INFO]"
      rules = either (error . Y.prettyPrintParseException) id $ Y.decodeEither' config
      loggers = initLoggers putStrLn doLog rules
  forM_ (sort $ HM.keys (logRules rules)) $ \ln -> runLogIO (newLogger loggers ln) _stuff
  runLogIO (newLogger loggers "Unconfigured") _stuff
#endif

_stuff :: LogIO ()
_stuff = ask >>= liftIO . print >> logInfo "hello" >> logError "buhbye"
