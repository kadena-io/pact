{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{- |
Module: Pact.Analyze

= Pact analysis tools developer guide

== Introduction and motivation

The pact analysis tools serve to verify properties of pact programs (with the properties also written in pact). The analysis tools themselves aren't formally verified, nor is the implementation of pact itself.

== Concepts

- /function property/ and /invariants/: There are two classes of properties we can verify in pact. We’ve come to call them /function properties/ (sometimes just /properties/) and /schema invariants/ (usually just /invariants/). A /function property/ states something about the relationship between the inputs to a function, its result, the environment in which it is run, and its effects on DB state. An /invariant/ states something that must always be true of the data in DB tables described by some schema.
- /SMT/, /z3/, and /sbv/: /SMT/ (SAT Modulo Theories) is the method we use for solving constraints. /z3/ is the tool we use to solve SMT problems, and /sbv/ is the Haskell library we use to interface with z3.
- /symbolic evaluation/ and /concrete evaluation/: /Symbolic evaluation/ is the translation of a pact program (or property or invariant) to (z3) constraints (via sbv). /Concrete evaluation/ is, well, just evaluation.
- /proof/ and /sat/: SBV \/ z3 can run in two modes — /proof/ or /sat(isfaction)/. These are /dual/:

    - A /falsifying model/ of some system of constraints `c` is exactly a /satisfying model/ for `not c`.
    - The lack of a satisfying model for `c` is a refutation of `c`.


== Quantifiers

Contrary to the situation in logic, there are, in some sense, three types of quantifiers we use in pact analysis: 'forAll', 'exists', and 'free'. The universal and existential quantifiers should be familiar. So what’s the deal with 'free'?

'free' creates a free variable, which is universal in a proof, but existential in sat. We use this for function inputs and /tags/. If we consider a system of constraints produced from e.g. a pact function and a property of that function, in /sat/ mode we attempt to assign a value to a free variable as we satisfy the entire system of constraints, and in /proof/ mode we attempt to show that there is no way to satisfy the constraint system for all possible values of a free variable.

The universal and existential quantifiers are used in properties (where a user writes @forall@ or  @exists@ in a property).

In general 'free' is the only quantifier used in 'Term's and it should only ever be used in models (see "Pact.Analyze.Model.Tags"). 'forAll' \/ 'exists' are only used in 'Prop's and should only ever be used in 'Prop's (see "Pact.Analyze.Eval.Prop"). <https://github.com/LeventErkok/sbv/issues/459#issuecomment-468856234 more info>

=== Prenex conversion

One tricky note with quantifiers is that SBV expects formulas to always be in prenex normal form, meaning all the quantifiers prefix the formula. Happily, it’s always possible to do this and the conversion is relatively straightforward. See https://github.com/LeventErkok/sbv/issues/256 for an example of a formula that will give the wrong answer if not converted first. We test an equivalent formula in the analysis spec (@(exists i. i > 0) /\ not (exists i. i > 0)@).


== Tagging, model generation, and execution traces

A model is an assignment of concrete values to symbolic variables. Pact’s symbolic analysis is primarily concerned with /proof/ rather than /sat/, so our models typically correspond to falsifying assignments to symbolic variables — a set of assignments which invalidate a property or invariant.

A model is necessary to report back to the user explaining why a property or invariant is invalid — we provide the user a counterexample to validity of the system of constraints in such a case.

Whenever we have a symbolic value like an @'SBV' 'Integer'@, we can call 'getValue' to extract the concrete value for that symbolic value in the model.

In the course of symbolic evaluation, we use 'free' to produce variables for each input argument to a function, and recursively evaluate all sub-expressions of the function body to produce a symbolic variable corresponding to the output of the function. Using the inputs and output we can state a proposition that we wish to validate. If we receive a counter-example at this point, we only have the ability to extract the input arguments and return value — these are undoubtedly helpful to the user, but not enough to paint the full picture of precisely /why/ a proposition fails to hold with respect to a nontrivial function that e.g. calls out to other functions; or uses keysets, guards, capabilities, the database, @enforce-one@, etc.

To paint this fuller picture, it is helpful to capture intermediate values, values read from the database, whether a row already existed before being written, whether a particular @enforce@ment call succeeded, etc. What we call “tagging” is a system by which we use 'free' to create extra variables that we can use to access this information during model extraction.

We generate unique (tag) identifiers during our translation phase ("Pact.Analyze.Translate") for each (abstract) syntax construct that we’d like to be able to produce one (or more!) values during model extraction. Then in our allocation phase ("Pact.Analyze.Model.Tags"), we call 'free' repeatedly to create variables for each tag. Then symbolic evaluation proceeds: when we produce intermediate symbolic variables, we look up the corresponding symbolic tag variable via tag ID and produce equality constraints between the intermediate and tag variables, capturing the values. Lastly, if we need to produce a model, we can retrieve concrete assignments to each of these “tagged” symbolic variables (using 'SBV.getValue' in "Pact.Analyze.Model.Tags") and re-associate them with their 'Term's.

These extra intermediate values are very useful for generating useful output, but the above isn’t sufficient to display a linear trace through the program. This is because while concrete evaluation runs straight-line through a program, symbolic evaluation effectively executes the entire program simultaneously (as a logical system of constraints). From our symbolic semantics, we must construct a simulated linear execution trace that would match a counterexample being run concretely. To accomplish this, we build a graph (using @alga@) during translation ("Pact.Analyze.Translate") of all possible execution paths that could occur during a simulated concrete run of the program. We tag each graph edge with a boolean symbolic variable for whether that edge is reachable from its predecessor, and during model extraction, we can observe the edges through which a concrete run of the program would occur. We associate “trace events” with each graph edge to track effects and intermediate values that would occur along an execution path. Then during model extraction, we can “linearize” a trace ("Pact.Analyze.Model.Graph") to provide an intuitive transcript of a counterexample run to the user. We also have the ability to render such execution graphs to DOT or PNG format in "Pact.Analyze.Model.Dot".

== Basic types

The most important types are in "Pact.Analyze.Types.Types". Let’s start with the type of all types that can appear in a pact program.

@
data 'Ty'
  = 'TyInteger'
  | 'TyBool'
  | 'TyStr'
  | 'TyTime'
  | 'TyDecimal'
  | 'TyGuard'
  | 'TyAny'
  | 'TyList' 'Ty'
  | 'TyObject' [ ('Symbol', 'Ty') ]
@

This mostly matches 'Type' from "Pact.Types.Type". Notes:

1. Objects are structural.
2. We use 'Symbol' for keys in objects. This is our only choice because we use 'Ty' as a type index.
3. We don’t model type variables. All of our types are concrete.
4. We don’t model function types.

@
data family 'Sing' :: k -> Type
@

'Sing' is a kind-indexed data family. A data family is a family of datatype definitions. In pact analysis we have four members of this family (we mention all of them here, but you can find them by searching for @"data instance Sing"@. We use singleton symbols ('SingSymbol'), lists ('SingList'),  types ('SingTy'), and flags (unnamed, also less important).

'Sing' is straight out of the <http://hackage.haskell.org/package/singletons singletons> playbook. We don’t use @singletons@ (mostly to avoid depending on it, but this is exactly what you’d see there).

A singleton symbol allows us to map between strings at the value and type levels:

@
data instance 'Sing' (sym :: 'Symbol') where
  'SSymbol' :: 'KnownSymbol' sym => 'Sing' sym

type 'SingSymbol' (x :: 'Symbol') = 'Sing' x
@

We also have singleton lists, which are used as object schemas.

@
data instance 'Sing' (n :: [('Symbol', 'Ty')]) where
  'SingList' :: 'HList' 'Sing' n -> 'Sing' n

type 'SingList' (a :: [('Symbol', 'Ty')]) = 'Sing' a

data HList (f :: 'Ty' -> Type) (tys :: [('Symbol', 'Ty')]) where
  'SNil'  :: 'HList' f '[]
  'SCons' :: ('SingI' ty, 'KnownSymbol' k, 'Typeable' ty)
         => 'Sing' k
         -> f ty
         -> 'HList' f tys
         -> 'HList' f ('(k, ty) ': tys)
@

In general, our singletons provide propositional equality:

@
'singListEq'
  :: forall (a :: [('Symbol', 'Ty')]) (b :: [('Symbol', 'Ty')]).
     'SingList' a -> 'SingList' b -> Maybe (a ':~:' b)
singListEq 'SNil'' 'SNil'' = Just 'Refl'
singListEq ('SingList' ('SCons' k1 v1 n1)) ('SingList' ('SCons' k2 v2 n2)) = do
  'Refl' <- 'eqSym' k1 k2
  'Refl' <- 'singEq' v1 v2
  'Refl' <- 'singListEq' ('SingList' n1) ('SingList' n2)
  pure 'Refl'
singListEq _ _ = Nothing
@

This is the first example we’ll see of the need for existential types (this is the case in general for our singletons):

@
data 'ESchema' where
  'ESchema' :: 'SingList' schema -> 'ESchema'
@

Note that, since this is a singleton, a term-level equality check can defer to propositional equality:

@
instance Eq 'ESchema' where
  'ESchema' ty1 == 'ESchema' ty2 = isJust $ 'singListEq' ty1 ty2
@

The last (and most important) singleton type we’ll discuss is 'SingTy':

@
data instance 'Sing' (a :: Ty) where
  'SInteger'      ::               'Sing' ''TyInteger'
  'SBool'         ::               'Sing' ''TyBool'
  'SStr'          ::               'Sing' ''TyStr'
  'STime'         ::               'Sing' ''TyTime'
  'SDecimal'      ::               'Sing' ''TyDecimal'
  'SGuard'        ::               'Sing' ''TyGuard'
  'SAny'          ::               'Sing' ''TyAny'
  'SList'         :: 'Sing' a     -> 'Sing' (''TyList' a)
  'SObjectUnsafe' :: 'SingList' a -> 'Sing' (''TyObject' a)

type 'SingTy' (a :: 'Ty') = 'Sing' a
@

This provides a connection between term- and type-level (pact) types.

So, why is 'SObjectUnsafe' /unsafe/? There’s an important invariant which can be broken by using 'SObjectUnsafe' directly: /the schema must be sorted/. If this invariant is broken the analysis will fail with a very user-unfriendly message from sbv. We provide two helpers: @pattern 'SObject'@ can be used when pattern-matching instead of 'SObjectUnsafe'. Unfortunately, GHC’s pattern coverage checker fails when it’s used, even though it should be exactly equivalent to 'SObjectUnsafe'. Fortunately, it /is/ safe to pattern match on 'SObjectUnsafe'... it’s just not ideal, you know, littering your code with things called /unsafe/. The other helper we provide (two helpers actually), are for constructing schemas (and objects):

@
'mkSObject' :: 'Sing' schema -> 'Sing' (''TyObject' ('Normalize' schema))
'normalizeSchema' :: 'SingList' schema -> 'SingList' ('Normalize' schema)
@

Note that these helpers are both located in the "Pact.Analyze.Types.ObjUtil" module. This module is responsible for doing type- and term-level construction of schemas (that maintain the connection between type and term). Concretely we’re talking about concatenation, filtering, 'nub` (removing duplicates), sorting, normalization (@nub . quicksort@), and insertion. Extreme care should be taken when modifying this module, as 'sansProof' is used several times, essentially telling GHC "trust me". Getting one of these wrong will lead to a segfault. Note the strong mirroring between the type- and term-level definitions.

=== Why are these types so complicated?

We use a typesafe api to SBV, meaning that the symbolic values we use to interface with SBV, are typed like @'SBV' Integer'@, @'SBV' String@, etc. This means that our terms have to be indexed precisely. This is not hard for the simple types (@integer@, @bool@, etc), but becomes harder for lists and especially for objects.

== Languages

Now that we’ve covered our representation of types (that was the hard part — it’s easier from here on), let’s consider terms. The analysis system has three languages:

- 'Term' is the type of pact terms. This is just our representation of Pact language expressions.
- 'Invariant' is the type of schema invariants, e.g. @(invariant (> x 0))@. These coincide with the pure subset of 'Term'
- 'Prop' is the type of function properties, e.g. @(property (= result (read accounts acct 'before)))@. This includes the pure subset of 'Term' as well as some property-specific constructions.

There are also two more sub-languages which we use internally:

- 'Core' is the core language shared by all three languages. It turns out that this is just the pure subset of all three languages (which happens to be all of 'Invariant'). This includes things like variables; arithmetic; comparisons; string, list, and object manipulations; etc.
- 'Numerical' is the arithmetical subset of 'Core'

Let’s look at 'Core' in more detail:


@
-- A `Core` contains subterms `t` indexed by type `a`
data 'Core' (t :: 'Ty' -> *) (a :: 'Ty') where
  -- we can embed literals and symbolic values
  'Lit' :: 'Concrete' a -> 'Core' t a
  'Sym' :: S ('Concrete' a) -> 'Core' t a

  -- variables are identified by a unique `VarId` and also have a name used for printing to the user
  'Var' :: 'VarId' -> Text -> 'Core' t a

  -- comparisons take two subterms of the same type and are boolean-valued
  'Comparison' :: 'SingTy' a -> 'ComparisonOp' -> t a -> t a -> 'Core' t ''TyBool'

  -- to merge two objects we need both two subterms (`t o1` and `t o2`) and also their types (`SingTy o1` and `SingTy o2`).
  'ObjMerge'    :: 'SingTy' o1 -> 'SingTy' o2 -> t o1 -> t o2 -> 'Core' t o
  -- etc...
@

The other languages build on top of this. For example 'Term':

@
data 'Term' (a :: 'Ty') where
  'CoreTerm'        :: 'Core' 'Term' a -> 'Term' a
  'ReadKeySet'      :: 'Term' ''TyStr' -> 'Term' ''TyGuard'
  -- etc...
@

Unfortunately, since these language types are type-indexed GADTs, we can’t rely on GHC’s 'Show' and 'Eq' deriving and must implement them by hand. This leads to a lot of boilerplate.

=== Existential terms

We often need existentials for all of these. Because they’re all type-indexed in the same way we can use the same 'Existential' type for all three:

@
data 'Existential' (tm :: 'Ty' -> Type) where
  'Some' :: 'SingTy' a -> tm a -> 'Existential' tm

type 'EInvariant' = 'Existential' 'Invariant'
type 'ETerm' = 'Existential' 'Term'
type 'EProp' = 'Existential' 'Prop'
@

All of our languages are instances of 'IsTerm', which contains singleton-equivalents to 'Eq', 'Show', and 'Pretty'.


@
class 'IsTerm' tm where
  'singEqTm'     :: 'SingTy' ty -> tm ty -> tm ty -> Bool
  'singShowsTm'  :: 'SingTy' ty -> Int   -> tm ty -> ShowS
  'singPrettyTm' :: 'SingTy' ty ->          tm ty -> Text
@

These each have a variation:

@
'eqTm'     :: ('SingI' ty, 'IsTerm' tm) => tm ty -> tm ty -> Bool
'showsTm'  :: ('SingI' ty, 'IsTerm' tm) => Int -> tm ty -> ShowS
'prettyTm' :: ('SingI' ty, 'IsTerm' tm) => tm ty -> Text
@

=== Singleton functions

In general it’s possible to convert a @SingTy ty@ argument to a @SingI ty@ constraint and vice-versa:

@
'withSing' :: 'SingTy' a -> ('SingI' a => b) -> b
'sing'     :: 'SingI'  a => 'SingTy' a
@

See 'withSing' for more.

== Evaluation

Evaluation is the translation from a language term (from one of the three languages above) to an @sbv@ value (@sbv@ handles the translation from this value to smt-lib constraints). The most important part of evaluation is the 'Analyzer' typeclass:

@
class (MonadError 'AnalyzeFailure' m, 'S' ':*<:' 'TermOf' m) => 'Analyzer' m where
  type 'TermOf' m   :: 'Ty' -> *
  'eval'            :: 'SingI' a => 'TermOf' m a -> m ('S' ('Concrete' a))
  'throwErrorNoLoc' :: 'AnalyzeFailureNoLoc'   -> m a
  'getVar'          :: 'VarId'                 -> m (Maybe 'AVal')
  'withVar'         :: 'VarId' -> 'AVal' -> m a  -> m a
  'markFailure'     :: 'SBV' 'Bool'              -> m ()
  'withMergeableAnalyzer'
    :: 'SingTy' a
    -> (( 'Mergeable' (m ('S' ('Concrete' a)))
        , 'Mergeable' (m ('SBV' ('Concrete' a)))
        ) => b)
    -> b
@

== Information flow

- Pact typechecker yields @'AST' 'Node'@s
- These are translated (in "Pact.Analyze.Translate") to 'ETerm' (existential term)

    - 'translateNode' is the core
    - 'runTranslation' is the main entry point
    - we use patterns defined in "Pact.Analyze.Patterns"
    - We always translate the body of the function we’re analyzing

- This is orchestrated by 'verifyFunctionInvariants'' and 'verifyFunctionProperty'. "Pact.Analyze.Check" can be thought of as the main entrypoint to symbolic analysis generally.
- Next we evaluate a few things:

    - The body of the function
    - Any properties we’re checking (if in 'verifyFunctionProperty')
    - Any invariants. These are evaluated in 'verifyFunctionInvariants'', but also in 'verifyFunctionProperty' because we enforce invariants on any values that are read from the db in a function body

- In either case we end up with some 'CheckResult's (@type 'CheckResult' = Either 'CheckFailure' 'CheckSuccess'@). These get bundled up in 'ModuleChecks' with any warnings.
- If a property is falsified we end up with an @'Invalid' model :: 'SmtFailure'@ where @model :: 'Model' ''Concrete'@. This model is constructed by the "Pact.Analyze.Model" module.

=== Information flow (properties / invariants)

These are parsed and typechecked from 'Exp's. See "Pact.Analyze.Parse.Prop" and "Pact.Analyze.Parse.Invariant".

== Testing

=== @AnalyzeSpec@
This is a large set of unit tests sanity checking all the major features of analysis.

=== @AnalyzePropertiesSpec@

We spent all this effort proving properties, but by necessity, in a separate implementation from the “official” pact interpreter. So we’re really proving properties of our symbolic interpreter. How do we guarantee that our results agree with the official implementation? By generating random terms and checking that both interpreters evaluate them to the same result. This requires solving several smaller problems:

- generating random types
- generating random terms of a given type
- translating pact terms to analysis (this is already done via the translation machinery)
- translating analysis terms to pact

-}
module Pact.Analyze () where

import Data.SBV
import Data.SBV.Control
import GHC.TypeLits

import Pact.Analyze.Alloc
import Pact.Analyze.Eval
import Pact.Analyze.Check
import Pact.Analyze.Errors
import Pact.Analyze.Patterns
import Pact.Analyze.Translate
import Pact.Analyze.Types
import Pact.Types.Pretty
