{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Pact.Analyze.Errors where

import qualified Data.SBV.Internals     as SBVI
import           Data.String            (IsString (fromString))
import           Data.Text              (Text)
import qualified Data.Text              as T

import qualified Pact.Types.Persistence as Pact
import           Pact.Types.Lang        (Info, tShow)

import           Pact.Analyze.Types

data AnalyzeFailureNoLoc
  = AtHasNoRelevantFields EType Schema
  | AValUnexpectedlySVal SBVI.SVal
  | AValUnexpectedlyObj Object
  | KeyNotPresent Text Object
  | MalformedLogicalOpExec LogicalOp Int
  | ObjFieldOfWrongType Text EType
  | PossibleRoundoff Text
  | UnsupportedDecArithOp ArithOp
  | UnsupportedIntArithOp ArithOp
  | UnsupportedUnaryOp UnaryArithOp
  | UnsupportedRoundingLikeOp1 RoundingLikeOp
  | UnsupportedRoundingLikeOp2 RoundingLikeOp
  | FailureMessage Text
  | OpaqueValEncountered
  | VarNotInScope Text VarId
  | UnsupportedObjectInDbCell
  | InvalidDbWrite Pact.WriteType Schema Object
  -- For cases we don't handle yet:
  | UnhandledTerm Text
  deriving (Eq, Show)

describeAnalyzeFailureNoLoc :: AnalyzeFailureNoLoc -> Text
describeAnalyzeFailureNoLoc = \case
    -- these are internal errors. not quite as much care is taken on the messaging
    AtHasNoRelevantFields etype schema -> "When analyzing an `at` access, we expected to return a " <> tShow etype <> " but there were no fields of that type in the object with schema " <> tShow schema
    AValUnexpectedlySVal sval -> "in analyzeTermO, found AVal where we expected AnObj" <> tShow sval
    AValUnexpectedlyObj obj -> "in analyzeTerm, found AnObj where we expected AVal" <> tShow obj
    KeyNotPresent key obj -> "key " <> key <> " unexpectedly not found in object " <> tShow obj
    MalformedLogicalOpExec op count -> "malformed logical op " <> tShow op <> " with " <> tShow count <> " args"
    ObjFieldOfWrongType fName fType -> "object field " <> fName <> " of type " <> tShow fType <> " unexpectedly either an object or a ground type when we expected the other"
    PossibleRoundoff msg -> msg
    UnsupportedDecArithOp op -> "unsupported decimal arithmetic op: " <> tShow op
    UnsupportedIntArithOp op -> "unsupported integer arithmetic op: " <> tShow op
    UnsupportedUnaryOp op -> "unsupported unary arithmetic op: " <> tShow op
    UnsupportedRoundingLikeOp1 op -> "unsupported rounding (1) op: " <> tShow op
    UnsupportedRoundingLikeOp2 op -> "unsupported rounding (2) op: " <> tShow op

    -- these are likely user-facing errors
    FailureMessage msg -> msg
    UnhandledTerm termText -> foundUnsupported termText
    VarNotInScope name vid -> "variable not in scope: " <> name <> " (vid " <> tShow vid <> ")"
    --
    -- TODO: maybe we should differentiate between opaque values and type
    -- variables, because the latter would probably mean a problem from type
    -- inference or the need for a type annotation?
    --
    OpaqueValEncountered -> "We encountered an opaque value in analysis. This would be either a JSON value or a type variable. We can't prove properties of these values."
    UnsupportedObjectInDbCell -> "We encountered the use of an object in a DB cell, which we don't yet support. " <> pleaseReportThis
    InvalidDbWrite writeType sch obj -> "invalid " <> tShow writeType <> " of " <> tShow obj <> " to DB row with schema " <> tShow sch

  where
    foundUnsupported :: Text -> Text
    foundUnsupported termText = "You found a term we don't have analysis support for yet. " <> pleaseReportThis <> "\n\n" <> termText

    pleaseReportThis :: Text
    pleaseReportThis = "Please report this as a bug at https://github.com/kadena-io/pact/issues"

instance IsString AnalyzeFailureNoLoc where
  fromString = FailureMessage . T.pack

data AnalyzeFailure = AnalyzeFailure
  { _analyzeFailureParsed :: !Info
  , _analyzeFailure       :: !AnalyzeFailureNoLoc
  }
