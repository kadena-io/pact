{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeOperators         #-}

module Pact.Analyze.Term where

import           Data.Data          (Data)
import           Data.SBV           (HasKind, SymWord)
import           Data.Text          (Text)
import           Prelude            hiding (Float)

import           Pact.Analyze.Numerical
import           Pact.Analyze.PrenexNormalize ()
import           Pact.Analyze.Types
import           Pact.Analyze.Util

--
-- TODO: merge this module with Pact.Analyze.Types once we are on GHC 8.2 and
--       can use DeriveAnyClass and GND in the same file.
--

type ETerm = Existential Term

data Term ret where
  PureTerm        :: PureTerm Term a -> Term a

  -- In principle, this should be a pure term, however, the analyze monad needs
  -- to be `Mergeable`. `Analyze` is, but `Query` isn't, due to having
  -- `Symbolic` in its stack.
  --
  -- TODO(joel): In principle this could be pure and applied to all the
  -- languages. Unfortunately, we can't add this to props because `Query` has
  -- `Symbolic` in its stack, so it can't do an `ite`.
  IfThenElse      :: Term Bool -> Term a -> Term a -> Term a

  -- Variable binding
  Let             :: Text -> VarId -> ETerm -> Term a -> Term a

  -- Control flow
  Sequence        :: ETerm     -> Term a ->           Term a

  -- Conditional transaction abort
  Enforce         :: Term Bool -> Term Bool

  -- Keyset access
  ReadKeySet      :: Term String -> Term KeySet
  KsAuthorized    :: TagId -> Term KeySet -> Term Bool
  NameAuthorized  :: TagId -> Term String -> Term Bool

  -- Table access
  Read            :: TagId -> TableName -> Schema      -> Term String -> Term Object
  Write           :: TagId -> TableName -> Term String -> Term Object -> Term String

  PactVersion     :: Term String

  Format          :: Term String         -> [ETerm]     -> Term String
  FormatTime      :: Term String         -> Term Time   -> Term String
  ParseTime       :: Maybe (Term String) -> Term String -> Term Time
  Hash            :: ETerm                              -> Term String

deriving instance Show a => Show (Term a)
deriving instance Show ETerm
deriving instance Show a => Show (PureTerm Term a)

instance S :<: Term where
  inject = PureTerm . Sym
  project = \case
    PureTerm (Sym a) -> Just a
    _                -> Nothing

lit :: SymWord a => a -> Term a
lit = PureTerm . Sym . literalS

instance Numerical Term :<: Term where
  inject = PureTerm . Numerical
  project (PureTerm (Numerical a)) = Just a
  project _                        = Nothing

instance Num (Term Integer) where
  fromInteger = lit . fromInteger
  (+)    = inject ... IntArithOp Add
  (*)    = inject ... IntArithOp Mul
  abs    = inject .   IntUnaryArithOp Abs
  signum = inject .   IntUnaryArithOp Signum
  negate = inject .   IntUnaryArithOp Negate

instance Num (Term Decimal) where
  fromInteger = lit . mkDecimal . fromInteger
  (+)    = inject ... DecArithOp Add
  (*)    = inject ... DecArithOp Mul
  abs    = inject .   DecUnaryArithOp Abs
  signum = inject .   DecUnaryArithOp Signum
  negate = inject .   DecUnaryArithOp Negate

data UserType = UserType
  deriving (Eq, Ord, Read, Data, Show, HasKind, SymWord)
