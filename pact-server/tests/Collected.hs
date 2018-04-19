module Collected where

import Data.Yaml as Y

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
      rules = either error id $ Y.decodeEither config
      loggers = initLoggers putStrLn doLog rules
  forM_ (sort $ HM.keys (logRules rules)) $ \ln -> runLogIO (newLogger loggers ln) _stuff
  runLogIO (newLogger loggers "Unconfigured") _stuff

_stuff :: LogIO ()
_stuff = ask >>= liftIO . print >> logInfo "hello" >> logError "buhbye"

