{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE Rank2Types         #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}

module Pact.Analyze.Term where

import           Data.Data          (Data)
import           Data.Map.Strict    (Map)
import           Data.SBV           (HasKind, SymWord)
import           Data.SBV.Control   (SMTValue)
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

data ETerm where
  -- TODO: remove Show (add constraint c?)
  ETerm   :: (Float a, Show a, SymWord a, SMTValue a)
    => Term a -> Type a -> ETerm
  EObject ::                                    Term Object -> Schema -> ETerm

mapETerm :: (forall a. Term a -> Term a) -> ETerm -> ETerm
mapETerm f term = case term of
  ETerm term' ty    -> ETerm (f term') ty
  EObject term' sch -> EObject (f term') sch

etermEType :: ETerm -> EType
etermEType (ETerm _ ety)   = EType ety
etermEType (EObject _ sch) = EObjectTy sch

data Term ret where
  PureTerm :: PureTerm Term a -> Term a

  -- At holds the schema of the object it's accessing. We do this so we can
  -- determine statically which fields can be accessed.
  Var            :: Text -> VarId                    -> Term a

  --
  -- TODO: we need to allow computed keys here
  --
  LiteralObject  :: Map Text ETerm   -> Term Object

  -- In principle, this should be a pure term, however, the analyze monad needs
  -- to be `Mergeable`. `Analyze` is, but `Query` isn't, due to having
  -- `Symbolic` in its stack.
  IfThenElse     :: Term Bool -> Term a -> Term a -> Term a

  -- ^ should be pure
  -- v term-specific

  -- Variable binding
  Let            :: Text -> VarId -> ETerm -> Term a -> Term a

  -- Control flow
  Sequence       :: ETerm     -> Term a ->           Term a

  -- Conditional transaction abort
  Enforce        :: Term Bool -> Term Bool

  -- Keyset access
  ReadKeySet      :: Term String -> Term KeySet
  KsAuthorized    :: TagId -> Term KeySet -> Term Bool
  NameAuthorized  :: TagId -> Term String -> Term Bool

  -- Table access
  Read           :: TagId -> TableName -> Schema      -> Term String -> Term Object
  Write          :: TagId -> TableName -> Term String -> Term Object -> Term String

  PactVersion     :: Term String

  Format          :: Term String         -> [ETerm]     -> Term String
  FormatTime      :: Term String         -> Term Time   -> Term String
  ParseTime       :: Maybe (Term String) -> Term String -> Term Time
  Hash            :: ETerm                              -> Term String

deriving instance Show a => Show (Term a)
deriving instance Show ETerm
instance Show (PureTerm Term a) where
  showsPrec _ _ = showString "TODO(joel)"

lit :: SymWord a => a -> Term a
lit = PureTerm . Sym . literalS

instance InjectNumerical Term where
  injectNumerical = PureTerm . Numerical
  projectNumerical (PureTerm (Numerical a)) = Just a
  projectNumerical _             = Nothing

instance Num (Term Integer) where
  fromInteger = lit . fromInteger
  (+)    = injectNumerical ... IntArithOp Add
  (*)    = injectNumerical ... IntArithOp Mul
  abs    = injectNumerical .   IntUnaryArithOp Abs
  signum = injectNumerical .   IntUnaryArithOp Signum
  negate = injectNumerical .   IntUnaryArithOp Negate

instance Num (Term Decimal) where
  fromInteger = lit . mkDecimal . fromInteger
  (+)    = injectNumerical ... DecArithOp Add
  (*)    = injectNumerical ... DecArithOp Mul
  abs    = injectNumerical .   DecUnaryArithOp Abs
  signum = injectNumerical .   DecUnaryArithOp Signum
  negate = injectNumerical .   DecUnaryArithOp Negate

data UserType = UserType
  deriving (Eq, Ord, Read, Data, Show, HasKind, SymWord)
