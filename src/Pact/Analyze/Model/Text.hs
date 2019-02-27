{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}

-- | Rendering concrete models' execution traces to text, for communication
-- with end users. This relies on the linearization of traces that occurs in
-- 'Pact.Analyze.Model.Graph'.
module Pact.Analyze.Model.Text
  ( showModel
  ) where

import           Control.Lens               (Lens', at, ifoldr, view, (^.))
import           Control.Monad.State.Strict (State, evalState, get, modify)
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Data.SBV                   (SBV, SymVal)
import qualified Data.SBV                   as SBV
import qualified Data.SBV.Internals         as SBVI
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           GHC.Natural                (Natural)

import qualified Pact.Types.Info            as Pact
import qualified Pact.Types.Lang            as Pact
import qualified Pact.Types.Persistence     as Pact
import           Pact.Types.Pretty          hiding (indent)

import           Pact.Analyze.Model.Graph   (linearize)
import           Pact.Analyze.Types

indent1 :: Text -> Text
indent1 = ("  " <>)

indent :: Natural -> Text -> Text
indent 0     = id
indent times = indent (pred times) . indent1

showSbv :: (Pretty a, SymVal a) => SBV a -> Text
showSbv sbv
  = T.pack
  $ renderCompactString'
  $ maybe "[ERROR:symbolic]" pretty (SBV.unliteral sbv)

showS :: (Pretty a, SymVal a) => S a -> Text
showS = showSbv . _sSbv

showTVal :: TVal -> Text
showTVal (ety, av) = case av of
  OpaqueVal   -> "[opaque]"
  AVal _ sval -> case ety of
    EType (ty :: SingTy ty) -> withPretty ty $ withSymVal ty $
      showSbv (SBVI.SBV sval :: SBV (Concrete ty))

showObject :: UObject -> Text
showObject (UObject m) = "{ "
  <> T.intercalate ", "
       (ifoldr (\key val acc -> showObjMapping key val : acc) [] m)
  <> " }"

showObjMapping :: Text -> TVal -> Text
showObjMapping key val = key <> ": " <> showTVal val

showArg :: Located (Unmunged, TVal) -> Text
showArg (Located _ (Unmunged nm, tval)) = nm <> " = " <> showTVal tval

showVar :: Located (Unmunged, TVal) -> Text
showVar (Located _ (Unmunged nm, tval)) = nm <> " := " <> showTVal tval

data ExpectPresent = ExpectPresent | ExpectNotPresent

--
-- TODO: this should display the table name
--
showRead :: Located Access -> Text
showRead (Located _ (Access srk obj suc)) = case SBV.unliteral suc of
  Nothing -> "[ERROR:symbolic]"
  Just True
    -> "read " <> showObject obj <> " for key " <> showS srk <> " succeeds"
  Just False
    -> "read for key " <> showS srk <> " fails because the row was not present"

--
-- TODO: this should display the table name
--
showWrite :: Pact.WriteType -> Located Access -> Text
showWrite writeType (Located _ (Access srk obj suc))
  = let writeTypeT = case writeType of
          Pact.Insert -> "insert"
          Pact.Update -> "update"
          Pact.Write  -> "write"
        expectPresent = case writeType of
          Pact.Insert -> ExpectNotPresent
          Pact.Update -> ExpectPresent
          Pact.Write  -> error "invariant violation: write should never fail"
    in writeTypeT <> " " <> showObject obj <> " to key " <> showS srk <> " "
       <> showDbAccessSuccess suc expectPresent

showDbAccessSuccess :: SBV Bool -> ExpectPresent -> Text
showDbAccessSuccess successSbv expectPresent = case SBV.unliteral successSbv of
  Nothing    -> "[ERROR:symbolic]"
  Just True  -> "succeeds"
  Just False -> case expectPresent of
    ExpectPresent    -> "fails because the row was not present"
    ExpectNotPresent -> "fails because the was already present"

showRn :: S RegistryName -> Text
showRn sRn = case SBV.unliteral (_sSbv sRn) of
  Nothing                -> "[unknown]"
  Just (RegistryName rn) -> "'" <> rn

showFailure :: Recoverability -> Text
showFailure = \case
  Recoverable _ -> "recovered from failure"
  Unrecoverable -> "failed"

showAssert :: Recoverability -> Located (SBV Bool) -> Text
showAssert recov (Located (Pact.Info mInfo) lsb) = case SBV.unliteral lsb of
    Nothing    -> "[ERROR:symbolic assert]"
    Just True  -> "satisfied assertion" <> context
    Just False -> showFailure recov <> " to satisfy assertion" <> context

  where
    context = maybe "" (\(Pact.Code code, _) -> ": " <> code) mInfo

showGE :: Recoverability -> Maybe Provenance -> Located GuardEnforcement -> Text
showGE recov mProv (_located -> GuardEnforcement sg sbool) =
  status <> " " <> guardDescription

  where
    status = case SBV.unliteral sbool of
      Nothing    -> "[ERROR:symbolic auth]"
      Just True  -> "satisfied"
      Just False -> showFailure recov <> " to satisfy"

    guard :: Text
    guard = "guard"

    guardDescription = case mProv of
      Nothing ->
        "unknown " <> guard <> " " <> showS sg
      Just (FromRow _) ->
        error "impossible: FromRow provenance on guard"
      Just (FromCell (OriginatingCell tn cn sRk _)) -> renderCompactText' $
        pretty guard <> " from database at (" <> pretty tn <> ", " <> "'" <>
          pretty cn <> ", " <> viaShow sRk <> ")"
      Just (FromRegistry sRn) ->
        guard <> " named " <> showRn sRn
      Just (FromInput (Unmunged arg)) ->
        guard <> " from argument " <> arg
      Just (FromMetadata sName) ->
        guard <> " from tx metadata attribute named " <> showS sName

showGR :: Recoverability -> Located GrantRequest -> Text
showGR recov (_located -> GrantRequest (CapName (T.pack -> capName)) sbool) =
  let requirement = "requirement of capability " <> capName
  in
    case SBV.unliteral sbool of
      Nothing    -> "[ERROR:symbolic grant request]"
      Just True  -> "satisfied " <> requirement
      Just False -> showFailure recov <> " to satisfy " <> requirement

-- TODO: after factoring Location out of TraceEvent, include source locations
--       in trace
showEvent
  :: Map TagId Provenance
  -> ModelTags 'Concrete
  -> TraceEvent
  -> State Natural [Text]
showEvent ksProvs tags event = do
  lastDepth <- get
  fmap (fmap (indent lastDepth)) $
    case event of
      TraceRead _ (_located -> tid) ->
        pure [display mtReads tid showRead]
      TraceWrite writeType _ (_located -> tid) ->
        pure [display mtWrites tid (showWrite writeType)]
      TraceAssert recov (_located -> tid) ->
        pure [display mtAsserts tid (showAssert recov)]
      TraceGuard recov (_located -> tid) ->
        pure [display mtGuardEnforcements tid (showGE recov $ tid `Map.lookup` ksProvs)]
      TraceRequireGrant recov _capName _bindings (_located -> tid) ->
        pure [display mtGrantRequests tid (showGR recov)]
      TraceSubpathStart _ ->
        pure [] -- not shown to end-users
      TracePushScope _ scopeTy locatedBindings -> do
        let vids = view (located.bVid) <$> locatedBindings
        modify succ
        let displayVids show' =
              (\vid -> indent1 $ display mtVars vid show') <$> vids

            displayCallScope :: Text -> Pact.ModuleName -> Text -> [Text]
            displayCallScope noun modName funName =
              -- TODO(joel): convert all of this to Doc
              let withArgs = case length vids of
                               0 -> ""
                               1 -> " with argument"
                               _ -> " with arguments"
                  headerLine = renderCompactText' $ "entering " <> pretty noun
                            <> " " <> pretty modName <> "." <> pretty funName
                            <> withArgs
                  argLines = case length vids of
                               0 -> []
                               _ -> displayVids showArg ++ [emptyLine]
              in headerLine : argLines

        pure $ case scopeTy of
          LetScope ->
            "let" : displayVids showVar
          ObjectScope ->
            "destructuring object" : displayVids showVar
          FunctionScope modName funName ->
            displayCallScope "function" modName funName
          CapabilityScope modName (CapName capName) ->
            displayCallScope "capability" modName $ T.pack capName

      TracePopScope _ scopeTy tid _ -> do
        modify pred
        let returnOutput =
              [ "returning with " <> display mtReturns tid showTVal
              , emptyLine
              ]
        pure $ case scopeTy of
          LetScope -> []
          ObjectScope -> []
          FunctionScope _ _ -> returnOutput
          CapabilityScope _ _ -> returnOutput

  where
    emptyLine :: Text
    emptyLine = ""

    display
      :: Ord k
      => Lens' (ModelTags 'Concrete) (Map k v)
      -> k
      -> (v -> Text)
      -> Text
    display l ident f = maybe "[ERROR:missing tag]" f $ tags ^. l.at ident

showModel :: Model 'Concrete -> Text
showModel model =
    T.intercalate "\n" $ T.intercalate "\n" . map indent1 <$>
      [ ["Program trace:"]
      , indent1 <$> (concat $ evalState (traverse showEvent' traceEvents) 0)
      , [maybe "\nTransaction aborted." (const "") mRetval]
      ]

  where
    ExecutionTrace traceEvents mRetval = linearize model

    showEvent' = showEvent (model ^. modelGuardProvs) (model ^. modelTags)
