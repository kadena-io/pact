{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}

-- | Errors that can occur during symbolic analysis.
module Pact.Analyze.Errors where

import qualified Data.SBV.Internals     as SBVI
import           Data.String            (IsString (fromString))
import           Data.Text              (Text)
import qualified Data.Text              as T

import           Pact.Types.Lang        (Info, tShow)
import qualified Pact.Types.Persistence as Pact

import           Pact.Analyze.Types

data AnalyzeFailureNoLoc
  = AtHasNoRelevantFields EType ESchema
  | AValUnexpectedlySVal SBVI.SVal
  | KeyNotPresent Text EType
  | MalformedLogicalOpExec LogicalOp Int
  | MalformedBitwiseOp BitwiseOp Int
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
  | DisallowedWrite TableName Pact.WriteType DbRestriction
  | DisallowedRead TableName DbRestriction
  -- For cases we don't handle yet:
  | UnhandledTerm Text
  deriving (Eq, Show)

restrictionContextName :: DbRestriction -> Text
restrictionContextName DisallowDb = "pure"
restrictionContextName DisallowWrites = "read-only"

describeAnalyzeFailureNoLoc :: AnalyzeFailureNoLoc -> Text
describeAnalyzeFailureNoLoc = \case
    -- these are internal errors. not quite as much care is taken on the messaging
    AtHasNoRelevantFields etype schema -> "When analyzing an `at` access, we expected to return a " <> tShow etype <> " but there were no fields of that type in the object with schema " <> tShow schema
    AValUnexpectedlySVal sval -> "in evalProp, unexpectedly found AVal: " <> tShow sval
    KeyNotPresent key obj -> "key " <> key <> " unexpectedly not found in object " <> tShow obj
    MalformedLogicalOpExec op count -> "malformed logical op " <> tShow op <> " with " <> tShow count <> " args"
    MalformedBitwiseOp op count -> "malformed bitwise op " <> tShow op <> " with " <> tShow count <> " args"
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
    DisallowedRead tn res -> "Encountered disallowed DB read from table " <> tShow tn <> " in " <> restrictionContextName res <> " context"
    DisallowedWrite tn writeType res -> "Encountered disallowed DB " <> T.toLower (tShow writeType) <> " to table " <> tShow tn <> " in " <> restrictionContextName res <> " context"

  where
    foundUnsupported :: Text -> Text
    foundUnsupported termText = "You found a term we don't have analysis support for yet. " <> pleaseReportThis <> "\n\n" <> termText

    pleaseReportThis :: Text
    pleaseReportThis = "Please report this as a bug at https://github.com/kadena-io/pact/issues"

instance IsString AnalyzeFailureNoLoc where
  fromString = FailureMessage . T.pack

data AnalyzeFailure = AnalyzeFailure
  { _analyzeFailureParsed :: Info
  , _analyzeFailure       :: AnalyzeFailureNoLoc
  }
