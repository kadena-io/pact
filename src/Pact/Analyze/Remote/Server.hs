{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}

-- | HTTP Server for GHCJS to use verification from the browser (where we can't
-- run sbv).
module Pact.Analyze.Remote.Server
  ( verifyHandler
  , runServantServer
  ) where

import           Control.Lens               ((^.), (.~), (&))
import           Control.Monad              (void)
import           Control.Monad.Except       (ExceptT(..), runExceptT,
                                             withExceptT)
import           Control.Monad.State.Strict (StateT(..))
import           Control.Monad.IO.Class     (liftIO)
import qualified Data.Aeson                 as A
import qualified Data.ByteString.Lazy.Char8 as BSL8
import           Data.Default               (def)
import           Data.Foldable              (traverse_)
import qualified Data.HashMap.Strict        as HM
import           Data.Monoid                ((<>))
import           Data.String                (IsString, fromString)
import qualified Data.Text                  as T
import           Data.Void                  (Void)
import           Network.Wai.Handler.Warp   (run)
import           Servant
import qualified Text.Megaparsec            as MP
import qualified Text.Megaparsec.Char       as MP

import qualified Pact.Analyze.Check        as Check
import           Pact.Analyze.Remote.Types (Request(..), Response(..),
                                            ClientError(..))
import           Pact.Repl                 (initReplState, evalRepl')
import           Pact.Repl.Types           (LibState, ReplMode(StringEval),
                                            ReplState, rEnv)
import           Pact.Types.Info           (Code(_unCode))
import           Pact.Types.Runtime        (Domain(KeySets), Method,
                                            ModuleData, PactDb(_readRow),
                                            eePactDb, eeRefStore, rsModules)
import           Pact.Types.Term           (ModuleDef(..), moduleDefName, moduleDefCode,
                                            ModuleName(..), Name(..),
                                            KeySet(..))

type VerifyAPI = "verify" :> ReqBody '[JSON] Request :> Post '[JSON] Response

verifyAPI :: Proxy VerifyAPI
verifyAPI = Proxy

runServantServer :: Int -> IO ()
runServantServer port = run port $ serve verifyAPI verifyHandler

verifyHandler :: Request -> Handler Response
verifyHandler req = do
  validReq <- Handler $ withExceptT makeServantErr $ validateRequest req
  liftIO $ runVerification validReq
  where
    makeServantErr (ClientError str) = err400 { errBody = BSL8.pack str }

data ValidRequest
  = ValidRequest (HM.HashMap ModuleName ModuleData) ModuleData

validateRequest :: Request -> ExceptT ClientError IO ValidRequest
validateRequest (Request mods modName) = do
    modsMap <- ExceptT $ liftIO $ loadModules mods
    ExceptT $ pure $
      case HM.lookup modName modsMap of
        Just mod' -> Right $ ValidRequest modsMap mod'
        Nothing   -> Left $ ClientError $
          show modName ++ " not found in list of provided modules: " ++ show (HM.keys modsMap)

initializeRepl :: IO ReplState
initializeRepl = do
  rs <- initReplState StringEval Nothing
  let dbImpl = rs ^. rEnv . eePactDb
      dummyKeySet = KeySet [] (Name "keys-all" def)

      -- Stub out all keyset accesses to return a dummy value. We don't care
      -- about real keys because we're not going to be doing any concrete
      -- execution.
      _readRow' :: forall k v. (IsString k, A.FromJSON v)
                => Domain k v
                -> k
                -> Method LibState (Maybe v)
      _readRow' KeySets _k  = const $ pure $ pure dummyKeySet
      _readRow' domain  key = _readRow dbImpl domain key

  pure $ rs & rEnv . eePactDb .~ dbImpl { _readRow = _readRow' }

replStateModules :: ReplState -> HM.HashMap ModuleName ModuleData
replStateModules replState = replState ^. rEnv . eeRefStore . rsModules

-- | Parser for strings like: @<interactive>:2:2: Module "mod2" not found@
moduleNotFoundP :: MP.Parsec Void String ModuleName
moduleNotFoundP = MP.string "<interactive>:"
               *> digitsP *> MP.char ':'
               *> digitsP *> MP.char ':'
               *> MP.string " Module "
               *> fmap fromString (MP.some $ MP.notChar ' ')
               <* MP.string " not found"
  where
    digitsP :: MP.Parsec Void String ()
    digitsP = void $ MP.some MP.digitChar

loadModules
  :: [ModuleDef a]
  -> IO (Either ClientError (HM.HashMap ModuleName ModuleData))
loadModules mods0 = do
  let -- - try to load the remaining list of modules
      -- - when we hit a failure, see whether it's because we need to load a
      --   certain dependency
      -- - try to move that dependency to the front of the list of modules that
      --   still need to be loaded
      -- - try again. if we have promoted more times than we have modules left,
      --   we've encountered a cycle and exit.
      go mods replState promotionsSinceLastSuccess = do
        (eSuccess, replState') <-
          runStateT (runExceptT (traverse_ loadModule mods)) replState
        case eSuccess of
          Left msg ->
            if promotionsSinceLastSuccess >= length mods
            then pure $ Left $ ClientError "detected cycle in modules"
            else
              case MP.parseMaybe moduleNotFoundP msg of
                Nothing       -> pure $ Left $ ClientError msg
                Just depName -> do
                  let numLoaded = HM.size (replStateModules replState')
                                - HM.size (replStateModules replState)
                  case promoteBy ((== depName) . moduleDefName) (drop numLoaded mods) of
                    Nothing    -> pure $ Left $ ClientError msg
                    Just mods' -> do
                      let promos' = if numLoaded > 0 then 0 else succ promotionsSinceLastSuccess
                      go mods' replState' promos'

          Right () ->
            pure $ Right $ replStateModules replState'

  replState0 <- initializeRepl
  go mods0 replState0 0

  where
    -- Promotes a value to the front of the list if it passes a test.
    promoteBy :: (a -> Bool) -> [a] -> Maybe [a]
    promoteBy isNeedle haystack = case break isNeedle haystack of
      (_others, [])             -> Nothing
      (others, found : others') -> Just $ found : (others ++ others')

    loadModule :: ModuleDef a -> ExceptT String (StateT ReplState IO) ()
    loadModule = void
               . ExceptT
               . evalRepl'
               . T.unpack
               . (\code -> "(begin-tx)" <> code <> "(commit-tx)")
               . _unCode
               . moduleDefCode

runVerification :: ValidRequest -> IO Response
runVerification (ValidRequest modsMap mod') =
  Response . Check.renderVerifiedModule <$> Check.verifyModule modsMap mod'
