{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE ViewPatterns          #-}

-- | Cross-cutting utilities shared across symbolic analysis code.
module Pact.Analyze.Util
( (...)
, for2
, astToParsed
, astToInfo
, nodeToParsed
, nodeToInfo
, infoToParsed
, dummyParsed
, dummyInfo
, vacuousMatch
, SnocList(..)
, reversed
, pattern ConsList
, snocList
, snocConsList
, Boolean(..)

-- * SBV backward compatibility
, sbvForall
, sbvForall_
, sbvExists
, sbvExists_
, universal
, universal_
, existential
, existential_
) where

import           Control.Lens         (Iso, Snoc (_Snoc), iso, makeLenses,
                                       prism)
import qualified Data.SBV             as SBV
import qualified Data.Default         as Default
import qualified Data.Foldable        as Foldable
import           GHC.Stack            (HasCallStack)
import           Pact.Types.Lang      (Info (_iInfo), Parsed)
import           Pact.Types.Typecheck (AST (_aNode), Node (_aId), _tiInfo)

-- | Function composition that consumes two args instead of one
(...) :: (a -> b) -> (x -> y -> a) -> x -> y -> b
(...) = fmap . fmap

for2
  :: (Traversable s, Traversable t, Applicative f)
  => s (t a) -> (a -> f b) -> f (s (t b))
for2 = flip (traverse . traverse)

astToParsed :: AST Node -> Parsed
astToParsed = nodeToParsed . _aNode

astToInfo :: AST Node -> Info
astToInfo = nodeToInfo . _aNode

nodeToParsed :: Node -> Parsed
nodeToParsed = infoToParsed . nodeToInfo

nodeToInfo :: Node -> Info
nodeToInfo =  _tiInfo . _aId

infoToParsed :: Info -> Parsed
infoToParsed parsed = case _iInfo parsed of
  Nothing               -> dummyParsed
  Just (_code, parsed') -> parsed'

-- | A 'Parsed' for when there is no location info available.
dummyParsed :: Parsed
dummyParsed = Default.def

-- | An 'Info' for when there is no location info available.
dummyInfo :: Info
dummyInfo = Default.def

vacuousMatch :: HasCallStack => String -> a
vacuousMatch msg = error $ "vacuous match: " ++ msg

-- * SnocList
--
-- For when DList is not a great option because you occasionally need to
-- inspect the last-added item.

newtype SnocList a
  = SnocList { _reversed :: [a] }
  deriving (Eq, Ord, Show)

instance Semigroup (SnocList a) where
  SnocList xs <> SnocList ys = SnocList $ ys ++ xs

instance Monoid (SnocList a) where
  mempty = SnocList []

pattern ConsList :: [a] -> SnocList a
pattern ConsList xs <- SnocList (reverse -> xs)
  where ConsList xs = SnocList $ reverse xs

instance Functor SnocList where
  fmap f (SnocList revXs) = SnocList $ fmap f revXs

instance Foldable SnocList where
  foldMap f (SnocList (reverse -> xs)) = foldMap f xs

instance Traversable SnocList where
  -- Not efficient, but we want to sequence the effects in-order:
  traverse f (SnocList (reverse -> xs)) = SnocList . reverse <$> traverse f xs

snocList :: [a] -> SnocList a
snocList = SnocList . reverse

makeLenses ''SnocList

instance Snoc (SnocList a) (SnocList b) a b where
  _Snoc = prism
    (\(SnocList as,a) -> SnocList (a:as))
    (\(SnocList aas) ->
      case aas of
        (a:as) -> Right (SnocList as, a)
        []     -> Left  (SnocList []))

snocConsList :: Iso (SnocList a) (SnocList b) [a] [b]
snocConsList = iso Foldable.toList snocList

infixl 6 .<+>       -- xor
infixr 3 .&&, .~&   -- and, nand
infixr 2 .||, .~|   -- or, nor
infixr 1 .=>, .<=>  -- implies, iff

class Boolean b where
  sTrue  :: b
  sFalse :: b
  sNot   :: b -> b
  (.&&)  :: b -> b -> b
  (.||)  :: b -> b -> b
  (.~&)  :: b -> b -> b
  (.~|)  :: b -> b -> b
  (.<+>) :: b -> b -> b
  (.=>)  :: b -> b -> b
  (.<=>) :: b -> b -> b
  fromBool :: Bool -> b

  sFalse         = sNot sTrue
  a .|| b        = sNot (sNot a .&& sNot b)
  a .~& b        = sNot (a .&& b)
  a .~| b        = sNot (a .|| b)
  a .<+> b       = (a .&& sNot b) .|| (sNot a .&& b)
  a .<=> b       = (a .&& b) .|| (sNot a .&& sNot b)
  a .=> b        = sNot a .|| b
  fromBool True  = sTrue
  fromBool False = sFalse

instance Boolean Bool where
  sTrue  = True
  sFalse = False
  sNot   = not
  (.&&)  = (&&)

instance Boolean (SBV.SBV Bool) where
  sTrue    = SBV.sTrue
  sFalse   = SBV.sFalse
  sNot     = SBV.sNot
  (.&&)    = (SBV..&&)
  (.||)    = (SBV..||)
  (.~&)    = (SBV..~&)
  (.~|)    = (SBV..~|)
  (.<+>)   = (SBV..<+>)
  (.=>)    = (SBV..=>)
  (.<=>)   = (SBV..<=>)
  fromBool = SBV.fromBool

-- SBV backward compat
--
sbvForall :: SBV.SymVal a => String -> SBV.Symbolic (SBV.SBV a)
sbvForall_ :: SBV.SymVal a => SBV.Symbolic (SBV.SBV a)
sbvExists :: SBV.SymVal a => String -> SBV.Symbolic (SBV.SBV a)
sbvExists_ :: SBV.SymVal a => SBV.Symbolic (SBV.SBV a)
universal :: SBV.Provable a => [String] -> a -> SBV.Symbolic SBV.SBool
universal_ :: SBV.Provable a => a -> SBV.Symbolic SBV.SBool
existential :: SBV.Provable a => [String] -> a -> SBV.Symbolic SBV.SBool
existential_ :: SBV.Provable a => a -> SBV.Symbolic SBV.SBool

#if !MIN_VERSION_sbv(8,17,5)
sbvForall = SBV.forall
sbvForall_ = SBV.forall_

sbvExists = SBV.exists
sbvExists_ = SBV.exists_

universal = SBV.forAll
universal_ = SBV.forAll_

existential = SBV.forSome
existential_ = SBV.forSome_
#else
sbvForall = SBV.sbvForall
sbvForall_ = SBV.sbvForall_

sbvExists = SBV.sbvExists
sbvExists_ = SBV.sbvExists_

universal = SBV.universal
universal_ = SBV.universal_

existential = SBV.existential
existential_ = SBV.existential_
#endif
