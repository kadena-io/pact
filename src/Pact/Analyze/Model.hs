{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

-- | 'Symbolic' allocation of quantified variables for arguments and tags,
-- for use prior to evaluation; and functions to saturate and show models from
-- Z3 post-evaluation.
module Pact.Analyze.Model
  ( allocArgs
  , allocModelTags
  , saturateModel
  , showModel
  ) where

import           Control.Lens         (Prism', ifoldr, imap, toListOf,
                                       traverseOf, traversed, (<&>), (?~),
                                       (^.), _1, _2)
import           Control.Monad        ((>=>))
import qualified Data.Foldable        as Foldable
import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as Map
import           Data.Monoid          ((<>))
import           Data.SBV             (SBV, SymWord, Symbolic)
import qualified Data.SBV             as SBV
import qualified Data.SBV.Control     as SBV
import qualified Data.SBV.Internals   as SBVI
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Traversable     (for)

import           Pact.Types.Runtime   (tShow)
import qualified Pact.Types.Typecheck as TC

import           Pact.Analyze.Types

alloc :: SymWord a => Symbolic (SBV a)
alloc = SBV.free_

allocSchema :: Schema -> Symbolic Object
allocSchema (Schema fieldTys) = Object <$>
  for fieldTys (\ety -> (ety,) <$> allocAVal ety)

allocAVal :: EType -> Symbolic AVal
allocAVal = \case
  EObjectTy schema -> AnObj <$> allocSchema schema
  EType (_ :: Type t) -> mkAVal . sansProv <$>
    (alloc :: Symbolic (SBV t))

allocArgs :: [Arg] -> Symbolic (Map VarId (Located (Text, TVal)))
allocArgs args = fmap Map.fromList $ for args $ \(Arg nm vid node ety) -> do
  let info = node ^. TC.aId . TC.tiInfo
  av <- allocAVal ety <&> _AVal._1 ?~ FromInput nm
  pure (vid, Located info (nm, (ety, av)))

allocModelTags :: Located ETerm -> ExecutionGraph -> Symbolic ModelTags
allocModelTags locatedTm graph = ModelTags
    <$> allocVars
    <*> allocReads
    <*> allocWrites
    <*> allocAuths
    <*> allocResult
    <*> allocPaths

  where
    -- For the purposes of symbolic value allocation, we just grab all of the
    -- events from the graph indiscriminately:
    events :: [TraceEvent]
    events = toListOf (egEdgeEvents.traverse.traverse) graph

    allocVars :: Symbolic (Map VarId (Located (Text, TVal)))
    allocVars = fmap Map.fromList $
      for (toListOf (traverse._TraceBind) events) $
        \(Located info (vid, nm, ety)) ->
          allocAVal ety <&> \av -> (vid, Located info (nm, (ety, av)))

    allocRowKey :: Symbolic (S RowKey)
    allocRowKey = sansProv <$> alloc

    allocAccesses
      :: Prism' TraceEvent (Located (TagId, Schema))
      -> Symbolic (Map TagId (Located (S RowKey, Object)))
    allocAccesses p = fmap Map.fromList $
      for (toListOf (traverse.p) events) $ \(Located info (tid, schema)) -> do
        srk <- allocRowKey
        obj <- allocSchema schema
        pure (tid, Located info (srk, obj))

    allocReads :: Symbolic (Map TagId (Located (S RowKey, Object)))
    allocReads = allocAccesses _TraceRead

    allocWrites :: Symbolic (Map TagId (Located (S RowKey, Object)))
    allocWrites = allocAccesses _TraceWrite

    allocAuths :: Symbolic (Map TagId (Located (SBV Bool)))
    allocAuths = fmap Map.fromList $
      for (toListOf (traverse._TraceEnforce) events) $ \(Located info tid) ->
        (tid,) . Located info <$> alloc

    allocResult :: Symbolic (Located TVal)
    allocResult = sequence $ locatedTm <&> \case
      ESimple ty _ ->
        let ety = EType ty
        in (ety,) <$> allocAVal ety
      EObject sch _ ->
        (EObjectTy sch,) . AnObj <$> allocSchema sch

    allocPaths :: Symbolic (Map TagId (SBV Bool))
    allocPaths = fmap Map.fromList $
      for (toListOf (traverse._TracePathStart) events) $ \tid ->
        (tid,) <$> alloc

-- NOTE: we indent the entire model two spaces so that the atom linter will
-- treat it as one message.
showModel :: Model -> Text
showModel (Model args (ModelTags vars reads' writes auths res _paths) ksProvs) =
    T.intercalate "\n" $ T.intercalate "\n" . map indent <$>
      [ ["Arguments:"]
      , indent <$> fmapToList showVar args
      , []
      , ["Variables:"]
      , indent <$> fmapToList showVar vars
      , []
      , ["Reads:"]
      , indent <$> fmapToList showAccess reads'
      , []
      , ["Writes:"]
      , indent <$> fmapToList showAccess writes
      , []
      , ["Keysets:"]
      , indent <$> imapToList showAuth auths
      , []
      , ["Result:"]
      , indent <$> [showResult]
      ]

  where
    fmapToList f xs = Foldable.toList $ fmap f xs
    imapToList f xs = Foldable.toList $ imap f xs

    indent :: Text -> Text
    indent = ("  " <>)

    showSbv :: (Show a, SymWord a) => SBV a -> Text
    showSbv sbv = maybe "[symbolic]" tShow (SBV.unliteral sbv)

    showS :: (Show a, SymWord a) => S a -> Text
    showS = showSbv . _sSbv

    showTVal :: TVal -> Text
    showTVal (ety, av) = case av of
      OpaqueVal   -> "[opaque]"
      AnObj obj   -> showObject obj
      AVal _ sval -> case ety of
        EObjectTy _ -> error "showModel: impossible object type for AVal"
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

    showAccess :: Located (S RowKey, Object) -> Text
    showAccess (Located _ (srk, obj)) = showS srk <> " => " <> showObject obj

    showKsn :: S KeySetName -> Text
    showKsn sKsn = case SBV.unliteral (_sSbv sKsn) of
      Nothing               -> "[unknown]"
      Just (KeySetName ksn) -> "'" <> ksn

    showAuth :: TagId -> Located (SBV Bool) -> Text
    showAuth tid lsb = status <> ksDescription
      where
        status = case SBV.unliteral (_located lsb) of
          Nothing    -> "unknown:      "
          Just True  -> "authorized:   "
          Just False -> "unauthorized: "

        ksDescription = case tid `Map.lookup` ksProvs of
          Nothing ->
            "unknown keyset"
          Just (FromCell (OriginatingCell (TableName tn) (ColumnName cn) sRk _dirty)) ->
            "database keyset at ("
              <> T.pack tn <> ", "
              <> "'" <> T.pack cn <> ", "
              <> showS sRk <> ")"
          Just (FromNamedKs sKsn) ->
            "named keyset " <> showKsn sKsn
          Just (FromInput arg) ->
            "argument " <> arg

    showResult :: Text
    showResult = showTVal $ _located res

-- | Builds a new 'Model' by querying the SMT model to concretize the provided
-- symbolic 'Model'.
saturateModel :: Model -> SBV.Query Model
saturateModel =
    traverseOf (modelArgs.traversed.located._2)        fetchTVal   >=>
    traverseOf (modelTags.mtVars.traversed.located._2) fetchTVal   >=>
    traverseOf (modelTags.mtReads.traversed.located)   fetchAccess >=>
    traverseOf (modelTags.mtWrites.traversed.located)  fetchAccess >=>
    traverseOf (modelTags.mtAuths.traversed.located)   fetchSbv    >=>
    traverseOf (modelTags.mtResult.located)            fetchTVal   >=>
    traverseOf (modelKsProvs.traversed)                fetchProv

  where
    fetchTVal :: TVal -> SBV.Query TVal
    fetchTVal (ety, av) = (ety,) <$> go ety av
      where
        go :: EType -> AVal -> SBV.Query AVal
        go (EType (_ :: Type t)) (AVal _mProv sval) = mkAVal' . SBV.literal <$>
          SBV.getValue (SBVI.SBV sval :: SBV t)
        go (EObjectTy _) (AnObj obj) = AnObj <$> fetchObject obj
        go _ _ = error "fetchTVal: impossible"

    -- NOTE: This currently rebuilds an SBV. Not sure if necessary.
    fetchSbv :: (SymWord a, SBV.SMTValue a) => SBV a -> SBV.Query (SBV a)
    fetchSbv = fmap SBV.literal . SBV.getValue

    fetchS :: (SymWord a, SBV.SMTValue a) => S a -> SBV.Query (S a)
    fetchS = traverseOf s2Sbv fetchSbv

    fetchObject :: Object -> SBVI.Query Object
    fetchObject (Object fields) = Object <$> traverse fetchTVal fields

    fetchAccess :: (S RowKey, Object) -> SBV.Query (S RowKey, Object)
    fetchAccess (sRk, obj) = do
      sRk' <- fetchS sRk
      obj' <- fetchObject obj
      pure (sRk', obj')

    fetchProv :: Provenance -> SBV.Query Provenance
    fetchProv = traverseOf (_FromCell.ocRowKey) fetchS
            >=> traverseOf _FromNamedKs         fetchS
