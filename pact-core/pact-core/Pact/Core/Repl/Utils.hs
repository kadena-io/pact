{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DerivingVia #-}


module Pact.Core.Repl.Utils
 ( ReplDebugFlag(..)
 , printDebug
 , ReplT(..)
 , replFlagSet
 , runReplT
 , ReplState(..)
 , replFlags
 , replLoaded
 , replPactDb
 , whenReplFlagSet
 , unlessReplFlagSet
 , debugIfFlagSet
 , replCompletion
 ) where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Catch

import Data.IORef
import Data.Set(Set)
import Data.Text(Text)
import Data.List(isPrefixOf)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

import Pact.Core.Info
import Pact.Core.Names
import Pact.Core.Persistence
import Pact.Core.Pretty
import qualified Pact.Core.Typed.Term as Term

import System.Console.Haskeline.Completion

data ReplDebugFlag
  = DebugLexer
  | DebugParser
  | DebugDesugar
  | DebugTypechecker
  | DebugTypecheckerType
  | DebugSpecializer
  deriving (Show, Eq, Ord)

-- | Passed in repl environment
-- Todo: not a `newtype` since there's
-- more fields we can set.
data ReplState b
  = ReplState
  { _replFlags :: Set ReplDebugFlag
  , _replLoaded :: Loaded b LineInfo
  , _replPactDb :: PactDb b LineInfo
  }

makeLenses ''ReplState

printDebug :: Pretty a => a -> ReplDebugFlag -> IO ()
printDebug a = \case
  DebugLexer -> do
    putStrLn "----------- Lexer output --------------"
    print (pretty a)
  DebugParser -> do
    putStrLn "----------- Parser output -------------"
    print (pretty a)
  DebugDesugar -> do
    putStrLn "----------- Desugar output ------------"
    print (pretty a)
  DebugTypechecker -> do
    putStrLn "----------- Typechecker output --------"
    print (pretty a)
  DebugTypecheckerType -> do
    putStrLn "----------- Inferred type output -------"
    print (pretty a)
  DebugSpecializer -> do
    putStrLn "----------- Specializer output --------"
    print (pretty a)

replFlagSet
  :: ReplDebugFlag
  -> ReplT b Bool
replFlagSet flag =
  uses replFlags (Set.member flag)

debugIfFlagSet :: Pretty a => ReplDebugFlag -> a -> ReplT b ()
debugIfFlagSet flag a =
  whenReplFlagSet flag $ liftIO (printDebug a flag)

whenReplFlagSet :: ReplDebugFlag -> ReplT b () -> ReplT b ()
whenReplFlagSet flag ma =
  replFlagSet flag >>= \b -> when b ma

unlessReplFlagSet :: ReplDebugFlag -> ReplT b () -> ReplT b ()
unlessReplFlagSet flag ma =
  replFlagSet flag >>= \b -> unless b ma

newtype ReplT b a
  = ReplT { unReplT :: ReaderT (IORef (ReplState b)) IO a }
  deriving
    ( Functor, Applicative, Monad
    , MonadIO
    , MonadThrow
    , MonadCatch
    , MonadMask)
  via (ReaderT (IORef (ReplState b)) IO)

instance MonadState (ReplState b) (ReplT b)  where
  get = ReplT (ReaderT readIORef)
  put rs = ReplT (ReaderT (`writeIORef` rs))

replCompletion
  :: [Text]
  -- ^ natives
  -> CompletionFunc (ReplT b)
replCompletion natives =
  completeWord (Just '\\') filenameWordBreakChars $ \str -> do
    tlns <- uses (replLoaded . loToplevel) Map.keys
    moduleNames <- uses (replLoaded . loModules) (fmap renderModuleName . Map.keys)
    prefixed <- uses (replLoaded . loModules) toPrefixed
    let allNames = Set.fromList $ fmap T.unpack $ concat [tlns, moduleNames, prefixed, natives]
    pure $ simpleCompletion <$> Set.toList (Set.filter (str `isPrefixOf`) allNames)
  where
  defNames = fmap (_nName . Term.defName) . Term._mDefs . _mdModule
  toPrefixed m =
    concat $ prefixF <$> Map.toList m
  prefixF (mn, ems) = let
    dns = defNames ems
    in fmap ((renderModuleName mn <> ".") <>) dns

runReplT :: IORef (ReplState b) ->  ReplT b a -> IO a
runReplT env (ReplT act) = runReaderT act env
