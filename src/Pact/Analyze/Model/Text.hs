{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Pact.Analyze.Model.Text
  ( showModel
  ) where

import           Control.Lens             (Lens', at, ifoldr, (^.))
import qualified Data.Foldable            as Foldable
import           Data.Map.Strict          (Map)
import qualified Data.Map.Strict          as Map
import           Data.Monoid              ((<>))
import           Data.SBV                 (SBV, SymWord)
import qualified Data.SBV                 as SBV
import qualified Data.SBV.Internals       as SBVI
import           Data.Text                (Text)
import qualified Data.Text                as T

import qualified Pact.Types.Info          as Pact
import           Pact.Types.Runtime       (tShow)

import           Pact.Analyze.Model.Graph (linearize)
import           Pact.Analyze.Types

indent :: Text -> Text
indent = ("  " <>)

showSbv :: (Show a, SymWord a) => SBV a -> Text
showSbv sbv = maybe "[ERROR:symbolic]" tShow (SBV.unliteral sbv)

showS :: (Show a, SymWord a) => S a -> Text
showS = showSbv . _sSbv

showTVal :: TVal -> Text
showTVal (ety, av) = case av of
  OpaqueVal   -> "[opaque]"
  AnObj obj   -> showObject obj
  AVal _ sval -> case ety of
    EObjectTy _         -> error "showModel: impossible object type for AVal"
    EType (_ :: Type t) -> showSbv (SBVI.SBV sval :: SBV t)

showObject :: Object -> Text
showObject (Object m) = "{ "
  <> T.intercalate ", "
       (ifoldr (\key val acc -> showObjMapping key val : acc) [] m)
  <> " }"

showObjMapping :: Text -> TVal -> Text
showObjMapping key val = key <> ": " <> showTVal val

showVar :: Located (Text, TVal) -> Text
showVar (Located _ (nm, tval)) = nm <> " := " <> showTVal tval

--
-- TODO: this should display the table name
--
showRead :: Located (S RowKey, Object) -> Text
showRead (Located _ (srk, obj)) = "read " <> showObject obj
                               <> " for key " <> showS srk

--
-- TODO: this should display the table name
--
showWrite :: Located (S RowKey, Object) -> Text
showWrite (Located _ (srk, obj)) = "write " <> showObject obj
                                <> " to key " <> showS srk

showKsn :: S KeySetName -> Text
showKsn sKsn = case SBV.unliteral (_sSbv sKsn) of
  Nothing               -> "[unknown]"
  Just (KeySetName ksn) -> "'" <> ksn

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

showAuth
  :: Recoverability
  -> Maybe Provenance
  -> Located (S KeySet, SBV Bool)
  -> Text
showAuth recov mProv (_located -> (srk, sbool)) =
  status <> " " <> ksDescription

  where
    status = case SBV.unliteral sbool of
      Nothing    -> "[ERROR:symbolic auth]"
      Just True  -> "satisfied"
      Just False -> showFailure recov <> " to satisfy"

    ks :: Text
    ks = showS srk

    ksDescription = case mProv of
      Nothing ->
        "unknown " <> ks
      Just (FromCell (OriginatingCell (TableName tn) (ColumnName cn) sRk _)) ->
        ks <> " from database at ("
          <> T.pack tn <> ", "
          <> "'" <> T.pack cn <> ", "
          <> showS sRk <> ")"
      Just (FromNamedKs sKsn) ->
        ks <> " named " <> showKsn sKsn
      Just (FromInput arg) ->
        ks <> " from argument " <> arg

-- TODO: after factoring Location out of TraceEvent, include source locations
--       in trace
showEvent :: Map TagId Provenance -> ModelTags 'Concrete -> TraceEvent -> [Text]
showEvent ksProvs tags = \case
    TraceRead (_located -> (tid, _)) ->
      [display mtReads tid showRead]
    TraceWrite (_located -> (tid, _)) ->
      [display mtWrites tid showWrite]
    TraceAssert recov (_located -> tid) ->
      [display mtAsserts tid (showAssert recov)]
    TraceAuth recov (_located -> tid) ->
      [display mtAuths tid (showAuth recov $ tid `Map.lookup` ksProvs)]
    TraceBind (_located -> (vid, _, _)) ->
      [display mtVars vid showVar]
    TraceSubpathStart _ ->
      [] -- not shown to end-users

  where
    display
      :: Ord k
      => Lens' (ModelTags 'Concrete) (Map k v)
      -> k
      -> (v -> Text)
      -> Text
    display l ident f = maybe "[ERROR:missing tag]" f $ tags ^. l.at ident

showModel :: Model 'Concrete -> Text
showModel model =
    T.intercalate "\n" $ T.intercalate "\n" . map indent <$>
      [ ["Arguments:"]
      , indent <$> Foldable.toList (showVar <$> (model ^. modelArgs))
      , []
      , ["Program trace:"]
      , indent <$> (showEvent' =<< traceEvents)
      , []
      , ["Result:"]
      , [indent $ maybe
          "Transaction aborted."
          (\tval -> "Return value: " <> showTVal tval)
          mRetval
        ]
      ]

  where
    ExecutionTrace traceEvents mRetval = linearize model

    showEvent' = showEvent (model ^. modelKsProvs) (model ^. modelTags)
