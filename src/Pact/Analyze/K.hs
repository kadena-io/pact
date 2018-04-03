{-# language Rank2Types #-}
module Pact.Analyze.K where

import Data.SBV (SymWord)
import Pact.Types.Lang hiding (Term)
import Pact.Types.Typecheck hiding (UserType)

import Pact.Analyze.Types

-- | 'K' contains a set of continuations for any type we might encounter during
-- translation.
--
-- You can see this in action by searching for @kApply@ / @kExpect@ in the
-- Translate module. @kApply@ is used to produce an @a@ from the given type of
-- @Term@. @kExpect@ is used to induce a failure @Either String@ if the wrong
-- node type is encountered.
--
-- 'K' also shows up in the @Analyze@ module, where we @kExpect@ a given type
-- based on the type-level type of the term we're analyzing, and kick off
-- translation.
data K a = K
  !(Term Bool     -> a)
  !(Term Decimal  -> a)
  !(Term Integer  -> a)
  !(Term String   -> a)
  !(Term Time     -> a)
  !(Term UserType -> a)

instance Functor K where
  fmap f (K b d i s t u) = K (f . b) (f . d) (f . i) (f . s) (f . t) (f . u)

kApplyUniform :: forall a. Node -> K a -> (forall b. Term b) -> a
kApplyUniform node (K fb fd fi fs ft fu) tm = case _aTy node of
  TyPrim TyBool    -> fb tm
  TyPrim TyDecimal -> fd tm
  TyPrim TyInteger -> fi tm
  TyPrim TyString  -> fs tm
  TyPrim TyTime    -> ft tm
  TyUser _         -> fu tm

  -- TODO
  TyPrim TyValue   -> error "unimplemented type analysis"
  TyPrim TyKeySet  -> error "unimplemented type analysis"
  TyAny            -> error "unimplemented type analysis"
  TyVar _v         -> error "unimplemented type analysis"
  TyList _         -> error "unimplemented type analysis"
  TySchema _ _     -> error "unimplemented type analysis"
  TyFun _          -> error "unimplemented type analysis"

preMap :: (forall a. Term a -> Term a) -> K b -> K b
preMap f (K fb fd fi fs ft fu)
  = K (fb . f) (fd . f) (fi . f) (fs . f) (ft . f) (fu . f)

uniformK :: (forall a. (Show a, SymWord a) => Term a -> b) -> K b
uniformK f = K f f f f f f

kExpectInt :: K (Either String (Term Integer))
kExpectInt = K
  (const (Left "expecting int"))
  (const (Left "expecting int"))
  pure
  (const (Left "expecting int"))
  (const (Left "expecting int"))
  (const (Left "expecting int"))

kApplyInt :: K a -> Term Integer -> a
kApplyInt (K _ _ fi _ _ _) i = fi i

kExpectBool :: K (Either String (Term Bool))
kExpectBool = K
  pure
  (const (Left "expecting bool"))
  (const (Left "expecting bool"))
  (const (Left "expecting bool"))
  (const (Left "expecting bool"))
  (const (Left "expecting bool"))

kApplyBool :: K a -> Term Bool -> a
kApplyBool (K fb _ _ _ _ _) b = fb b

kExpectStr :: K (Either String (Term String))
kExpectStr = K
  (const (Left "expecting string"))
  (const (Left "expecting string"))
  (const (Left "expecting string"))
  pure
  (const (Left "expecting string"))
  (const (Left "expecting string"))

kApplyStr :: K a -> Term String -> a
kApplyStr (K _ _ _ fs _ _) s = fs s

kExpectDecimal :: K (Either String (Term Decimal))
kExpectDecimal = K
  (const (Left "expecting decimal"))
  pure
  (const (Left "expecting decimal"))
  (const (Left "expecting decimal"))
  (const (Left "expecting decimal"))
  (const (Left "expecting decimal"))

kApplyDecimal :: K a -> Term Decimal -> a
kApplyDecimal (K _ fd _ _ _ _) d = fd d

kExpectTime :: K (Either String (Term Time))
kExpectTime = K
  (const (Left "expecting time"))
  (const (Left "expecting time"))
  (const (Left "expecting time"))
  (const (Left "expecting time"))
  pure
  (const (Left "expecting time"))

kApplyTime :: K a -> Term Time -> a
kApplyTime (K _ _ _ _ ft _) t = ft t

kExpectUserType :: K (Either String (Term UserType))
kExpectUserType = K
  (const (Left "expecting user type"))
  (const (Left "expecting user type"))
  (const (Left "expecting user type"))
  (const (Left "expecting user type"))
  (const (Left "expecting user type"))
  pure

kApplyUserType :: K a -> Term UserType -> a
kApplyUserType (K _ _ _ _ _ fu) t = fu t
