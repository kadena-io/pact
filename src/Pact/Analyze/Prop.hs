{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language StandaloneDeriving #-}
module Pact.Analyze.Prop where

import Control.Lens (Lens', lens)
import Data.Aeson
import Data.SBV hiding (Satisfiable)
import qualified Data.SBV.Internals as SBVI
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as T
import Pact.Types.Util (AsString)

wrappedStringFromCW :: (String -> a) -> SBVI.CW -> a
wrappedStringFromCW construct (SBVI.CW _ (SBVI.CWString s)) = construct s
wrappedStringFromCW _ c = error $ "SymWord: Unexpected non-string value: " ++ show c

mkConcreteString :: String -> SBV a
mkConcreteString = SBVI.SBV
                 . SBVI.SVal KString
                 . Left
                 . SBVI.CW KString
                 . SBVI.CWString

newtype KeySetName
  = KeySetName Text
  deriving (Eq,Ord,IsString,AsString,ToJSON,FromJSON)

instance Show KeySetName where show (KeySetName s) = show s

instance SymWord KeySetName where
  mkSymWord = SBVI.genMkSymVar KString
  literal (KeySetName t) = mkConcreteString $ T.unpack t
  fromCW = wrappedStringFromCW $ KeySetName . T.pack

instance HasKind KeySetName where
  kindOf _ = KString

newtype TableName
  = TableName String
  deriving (Eq, Ord, Show)

instance SymWord TableName where
  mkSymWord = SBVI.genMkSymVar KString
  literal (TableName s) = mkConcreteString s
  fromCW = wrappedStringFromCW TableName

instance HasKind TableName where
  kindOf _ = KString

instance IsString TableName where
  fromString = TableName

newtype ColumnName
  = ColumnName String
  deriving (Eq, Ord, Show)

instance SymWord ColumnName where
  mkSymWord = SBVI.genMkSymVar KString
  literal (ColumnName s) = mkConcreteString s
  fromCW = wrappedStringFromCW ColumnName

instance HasKind ColumnName where
  kindOf _ = KString

instance IsString ColumnName where
  fromString = ColumnName

data Prop a where
  -- TX success/failure
  Abort            ::                             Prop Bool
  Success          ::                             Prop Bool

  -- Binding
  -- Var              :: Text       ->               Prop a   -- non-HOAS style
  -- WithArg          :: Text -> (Prop a -> Prop b) -> Prop b -- HOAS style

  -- Logical connectives
  Implies          :: Prop Bool  -> Prop Bool  -> Prop Bool
  Not              :: Prop Bool  ->               Prop Bool
  And              :: Prop Bool  -> Prop Bool  -> Prop Bool
  Or               :: Prop Bool  -> Prop Bool  -> Prop Bool

  -- TODO: Int ops

  -- TODO: String ops (e.g. empty)

  -- DB properties
  TableWrite       :: TableName  ->               Prop Bool -- anything in table is written
  TableRead        :: TableName  ->               Prop Bool -- anything in table is read
  ColumnWrite      :: TableName  -> ColumnName -> Prop Bool -- particular column is written
  CellIncrease     :: TableName  -> ColumnName -> Prop Bool -- any cell at all in col increases
  ColumnConserve   :: TableName  -> ColumnName -> Prop Bool -- sum of all changes in col == 0
  ColumnIncrease   :: TableName  -> ColumnName -> Prop Bool -- sum of all changes in col >  0
  --
  -- TODO: StaleRead?
  --

  -- Authorization
  KsNameAuthorized :: KeySetName ->               Prop Bool -- keyset authorized by name
  --
  -- TODO: row-level keyset enforcement seems like it needs some form of
  --       unification, so that using a variable we can connect >1 domain
  --       property?
  --
  --       e.g.: forall row. RowWrite("balances", r) `Implies` RowKsEnforced(r)
  --
  --       RowKsEnforced  :: RowUid    ->            DomainProperty
  --       RowWrite       :: TableName -> RowUid  -> DomainProperty
  --

deriving instance Eq (Prop Bool)
deriving instance Show (Prop Bool)

data Check where
  Satisfiable :: Prop Bool -> Check
  Valid       :: Prop Bool -> Check
  deriving (Eq, Show)

ckProp :: Lens' Check (Prop Bool)
ckProp = lens getter setter
  where
    getter (Satisfiable p) = p
    getter (Valid p) = p

    setter (Satisfiable _) p = Satisfiable p
    setter (Valid _) p = Valid p
