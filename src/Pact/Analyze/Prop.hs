{-# language FlexibleInstances          #-}
{-# language GADTs                      #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language ScopedTypeVariables        #-}
{-# language StandaloneDeriving         #-}
{-# language TypeOperators              #-}

module Pact.Analyze.Prop where

import Control.Lens (Lens', lens)
import Data.Aeson (ToJSON, FromJSON)
import Data.SBV hiding (Satisfiable)
import qualified Data.SBV.Internals as SBVI
import Data.String (IsString(..))
import Data.Text (Text)
import Data.Typeable (Typeable, (:~:)(Refl), eqT)
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

newtype RowKey
  = RowKey String
  deriving (Eq, Ord, Show)

instance SymWord RowKey where
  mkSymWord = SBVI.genMkSymVar KString
  literal (RowKey s) = mkConcreteString s
  fromCW = wrappedStringFromCW RowKey

instance HasKind RowKey where
  kindOf _ = KString

instance IsString RowKey where
  fromString = RowKey

-- We can't use Proxy because deriving Eq doesn't work
-- We're still 8.0, so we can't use the new TypeRep yet:
data Rep a = Rep deriving (Eq, Show)
data Ty where Ty :: (SymWord t, Typeable t) => Rep t -> Ty
instance Eq Ty where
  Ty (Rep :: Rep a) == Ty (Rep :: Rep b) =
    case eqT :: Maybe (a :~: b) of
      Just Refl -> True
      _ -> False
deriving instance Show Ty

data Prop a where
  -- Literals
  PLit             :: SymWord a => a -> Prop a

  -- TX success/failure
  Abort            :: Prop Bool
  Success          :: Prop Bool

  -- Abstraction
  Forall           :: Text -> Ty -> Prop a -> Prop a
  --Forall         :: (SymWord a, Typeable a) => (Prop a -> Prop b) -> Prop b
  Exists           :: Text -> Ty -> Prop a -> Prop a
  PVar             :: Text ->                 Prop a

  -- -- Logical connectives
  Not              :: Prop Bool  ->               Prop Bool
  And              :: Prop Bool  -> Prop Bool  -> Prop Bool
  Or               :: Prop Bool  -> Prop Bool  -> Prop Bool

  -- TODO: Int ops

  -- TODO: String ops (e.g. empty)

  -- DB properties
  TableWrite       :: TableName  ->                Prop Bool -- anything in table is written
  TableRead        :: TableName  ->                Prop Bool -- anything in table is read
  ColumnWrite      :: TableName  -> ColumnName  -> Prop Bool -- particular column is written
  CellIncrease     :: TableName  -> ColumnName  -> Prop Bool -- any cell at all in col increases
  ColumnConserve   :: TableName  -> ColumnName  -> Prop Bool -- sum of all changes in col == 0
  ColumnIncrease   :: TableName  -> ColumnName  -> Prop Bool -- sum of all changes in col >  0
  RowRead          :: TableName  -> Prop RowKey -> Prop Bool
  RowWrite         :: TableName  -> Prop RowKey -> Prop Bool
  --
  -- TODO: StaleRead?
  --

  -- Authorization
  KsNameAuthorized :: KeySetName ->                              Prop Bool -- keyset authorized by name
  RowEnforced      :: TableName  -> ColumnName -> Prop RowKey -> Prop Bool

deriving instance Eq (Prop a)
deriving instance Show a => Show (Prop a)

instance Boolean (Prop Bool) where
  true = PLit True
  false = PLit False
  bnot p = Not p
  p1 &&& p2 = p1 `And` p2
  p1 ||| p2 = p1 `Or` p2

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
