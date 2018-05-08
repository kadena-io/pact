{-# language DeriveAnyClass      #-}
{-# language DeriveDataTypeable  #-}
{-# language FlexibleInstances   #-}
{-# language GADTs               #-}
{-# language Rank2Types          #-}
{-# language StandaloneDeriving  #-}
{-# language TypeOperators       #-}

module Pact.Analyze.Types where

import Data.Data
import Data.Map.Strict (Map)
import Data.SBV hiding (Satisfiable, Unsatisfiable, Unknown, ProofError)
import Pact.Types.Lang hiding (Term, TableName, Type, TObject, EObject, KeySet,
                               TKeySet)

import Pact.Analyze.Prop

data ETerm where
  -- TODO: remove Show (add constraint c?)
  ETerm   :: (Show a, SymWord a) => Term a      -> Type a -> ETerm
  EObject ::                        Term Object -> Schema -> ETerm

mapETerm :: (forall a. Term a -> Term a) -> ETerm -> ETerm
mapETerm f term = case term of
  ETerm term' ty    -> ETerm (f term') ty
  EObject term' sch -> EObject (f term') sch

data Term ret where
  -- Literals
  Literal        :: S a                       -> Term a
  --
  -- TODO: we need to allow computed keys here
  --
  LiteralObject  :: Map String (EType, ETerm) -> Term Object

  -- Variable binding
  Let            :: Text -> ETerm -> Term a  -> Term a
  Var            :: Text ->                     Term a

  -- Control flow
  IfThenElse     :: Term Bool -> Term a -> Term a -> Term a
  Sequence       :: ETerm     -> Term a ->           Term a

  -- Conditional transaction abort
  Enforce        :: Term Bool -> Term Bool

  -- Keyset access
  ReadKeySet      :: Term String -> Term KeySet
  KsAuthorized    :: Term KeySet -> Term Bool
  NameAuthorized  :: Term String -> Term Bool

  -- At holds the schema of the object it's accessing. We do this so we can
  -- determine statically which fields can be accessed.
  At             :: Schema -> Term String -> Term Object -> EType -> Term a

  -- Table access
  Read           :: TableName -> Schema      -> Term String ->           Term Object
  ReadCols       :: TableName -> Schema      -> Term String -> [Text] -> Term Object
  Write          :: TableName -> Term String -> Term Object ->           Term String

  -- Arithmetic ops
  --
  -- We partition the arithmetic operations in to these classes:
  -- * DecArithOp, IntArithOp: binary operators applied to (and returning) the
  --   same type (either integer or decimal).
  --   - Operations: { + - * / ^ log }
  -- * DecUnaryArithOp, IntUnaryArithOp: unary operators applied to and
  --   returning the same type (integer or decimal).
  --   - Operations: { - (negate) sqrt ln exp abs } (also signum even though
  --     it's not in pact)
  -- * DecIntArithOp, IntDecArithOp: binary operators applied to one integer
  --   and one decimal, returning a decimal. These are overloads of the integer
  --   / decimal binary ops.
  --   - Operations: { + - * / ^ log }
  -- * ModOp: Just modulus (oddly, it's the only operator with signature
  --   `integer -> integer -> integer`.
  --   - Operations: { mod }
  --
  -- * RoundingLikeOp1: Rounding decimals to integers.
  --   - Operations: { round floor ceiling }
  -- * RoundingLikeOp2: Rounding decimals to decimals with a specified level of
  --   precision.
  --   - Operations: { round floor ceiling }
  DecArithOp      :: ArithOp        -> Term Decimal -> Term Decimal -> Term Decimal
  IntArithOp      :: ArithOp        -> Term Integer -> Term Integer -> Term Integer
  DecUnaryArithOp :: UnaryArithOp   -> Term Decimal ->                 Term Decimal
  IntUnaryArithOp :: UnaryArithOp   -> Term Integer ->                 Term Integer
  DecIntArithOp   :: ArithOp        -> Term Decimal -> Term Integer -> Term Decimal
  IntDecArithOp   :: ArithOp        -> Term Integer -> Term Decimal -> Term Decimal
  ModOp           :: Term Integer   -> Term Integer ->                 Term Integer
  RoundingLikeOp1 :: RoundingLikeOp -> Term Decimal ->                 Term Integer
  RoundingLikeOp2 :: RoundingLikeOp -> Term Decimal -> Term Integer -> Term Decimal

  -- invariant (inaccessible): a ~ Integer or a ~ Decimal
  --
  -- TODO: possibly split into two constructors, like in Prop:
  --
  AddTime         :: Term Time -> ETerm -> Term Time

  Comparison      :: (Show a, SymWord a) => ComparisonOp -> Term a -> Term a -> Term Bool

  Logical         :: LogicalOp -> [Term Bool] -> Term Bool

  Concat          :: Term String -> Term String -> Term String

  PactVersion     :: Term String

deriving instance Show a => Show (Term a)
deriving instance Show ETerm

lit :: SymWord a => a -> Term a
lit = Literal . literalS

instance Num (Term Integer) where
  fromInteger = Literal . fromInteger
  (+)    = IntArithOp Add
  (*)    = IntArithOp Mul
  abs    = IntUnaryArithOp Abs
  signum = IntUnaryArithOp Signum
  negate = IntUnaryArithOp Negate

instance Num (Term Decimal) where
  fromInteger = lit . mkDecimal . fromInteger
  (+)    = DecArithOp Add
  (*)    = DecArithOp Mul
  abs    = DecUnaryArithOp Abs
  signum = DecUnaryArithOp Signum
  negate = DecUnaryArithOp Negate

data UserType = UserType
  deriving (Eq, Ord, Read, Data, Show, HasKind, SymWord)
