![](img/kadena-logo-210px.png)

The Pact property checking system
===

## What is it?

Pact comes equipped with the ability for smart contract authors to express and
automatically check properties -- or, specifications -- of Pact programs.

The Pact property checking system is our response to the current environment of
chaos and uncertainty in the smart contract programming world. Instead of
requiring error-prone smart contract authors to try to imagine all possible
ways an attacker could exploit their smart contract, we can allow them to prove
their code can't be attacked, all without requiring a background in formal
verification.

For example, for an arbitrarily complex Pact program, we might want to
definitively prove that the program only allows "administrators" of the
contract to modify the database -- for all other users, we're guaranteed that
the contract's logic permits read-only access to the DB. We can prove such a
property _statically_, before any code is deployed to the blockchain.

Compared with conventional unit testing, wherein the behavior of a program is
validated for a single combination of inputs and the author hopes this case
generalizes to all inputs, the Pact property checking system _automatically_
checks the code under test against all possible inputs, and therefore all
possible execution paths.

Pact does this by allowing authors to specify _schema invariants_ about columns
in database tables, and to state and prove _properties_ about functions with
respect to the function's arguments and return values, keyset enforcement,
database access, and use of `enforce`.

For those familiar, the Pact's properties correspond to the notion of
"contracts" (note: this is different than "smart contracts"), and Pact's
invariants correspond to a simplified initial step towards refinement types,
from the world of formal verification.

For this initial release we don't yet support 100% of the Pact language, and
the implementation of the property checker _itself_ has not yet been formally
verified, but this is only the first step. We're excited to continue broadening
support for every possible Pact program, eventually prove correctness of the
property checker, and continually enable authors to express ever more
sophisticated properties about their smart contracts over time.

## What do properties and schema invariants look like?

Here's an example of Pact's properties in action -- we state a property before
the definition of the function to which it corresponds. Note that the function
farms out its implementation of keyset enforcement to another function,
`enforce-admin`, and we don't have to be concerned about how that happens to be
implemented. Our property states that if the transaction submitted to the
blockchain runs successfully, it must be the case that the transaction has the
proper signatures to satisfy the keyset named `admins`:

```
(@property (authorized-by 'admins))
(defun read-account (id)
  "Read data for account ID"
  (enforce-admin)
  (read accounts id ['balance 'ccy 'amount]))
```

Next, we see an example of schema invariants. For any table with the following
schema, if our property checker succeeds, we know that all possible code paths
will always maintain the invariant that token balances are always greater than
zero:

```
(@invariant (> balance 0))
(defschema tokens
  username:string
  balance:integer)
```

## How does it work?

Pact's property checker works by realizing the language's semantics in an SMT
("Satisfiability Modulo Theories") solver, and by building and testing the
validity of a model. The SMT solver proves that there is no possible assignment
of values to symbolic variables which can falsify a provided proposition about
some Pact code. Pact currently uses Microsoft's [Z3 theorem
prover](https://github.com/Z3Prover/z3/wiki) to power its property checking
system.

Such a model is built from the combination of the functions in a Pact module,
the properties provided for those functions, and invariants declared on schemas
in the module.

For any function definition in a Pact module, any subsequent call to another
function is inlined. Before any properties are tested, this inlined code must
pass typechecking.

For schema invariants, the property checker takes an inductive approach: it
assumes that the schema invariants _hold_ for the data currently in the
database, and _checks_ that all functions in the module maintain those
invariants for any possible DB modification.

## How do you use it?

After supplying any desired invariant and property annotations in your module,
property checking is run by invoking `verify`:

`(verify 'module-name)`

This will typecheck the code and, if that succeeds, check all invariants and
properties.

## Expressing schema invariants

Schema invariants are formed by the following BNF grammar:

```
<comparator>
  ::= <
    | <=
    | ==
    | !=
    | >=
    | >

<expr>
  ::= <column name>
    | <int literal>
    | <dec literal>
    | ( <comparator> <expr> <expr> )

<invariant>
  ::= ( @invariant <expr> )
```

## Expressing properties

### Arguments, return values, and standard arithmetic and comparison operators

In properties, function arguments can be referred to directly by their names,
and return values can be referred to by the name `result`:

```
(@property (= result (* -1 x)))
(defun negate:integer (x:integer)
  (* x -1))
```

Here you can also see that the standard arithmetic operators on integers and
decimals work as they do in normal Pact code.

We can also define properties in terms of the standard comparison operators:

```
(@property (>= result 0))
(defun abs:integer (x:integer)
  (if (< x 0)
    (negate x)
    x))
```

### Boolean operators

In addition to the standard boolean operators `and`, `or`, and `not`, Pact's
property checking language supports logical implication in the form of `when`,
where `(when x y)` is equivalent to `(or (not x) y)`:

```
(@property (when (< x 0) (> result 0)))
(@property (when (> x 0) (< result 0)))
(@property
  (and
    (when (< x 0) (> result 0))
    (when (> x 0) (< result 0))))
(defun negate:integer (x:integer)
  (* x -1))
```

### Transaction abort and success

By default, every `@property` is predicated on the successful completion of the
transaction which would contain an invocation of the function under test. This
means that properties like the following:

```
(@property (!= result 0))
(defun ensured-positive (val:integer)
  (enforce (> val 0))
  val)
```

will pass due to the use of `enforce`.

At run-time on the blockchain, if an `enforce` call fails, the containing
transaction is aborted. Because `@property` is only concerned with transactions
that succeed, the necessary conditions to pass each `enforce` call are assumed.

<!---

### Valid, satisfiable, and explicit transaction abort/success

TODO: more. talk about @valid, @satisfiable, and the lack of the default
success condition of @property.

Pact's property language supports the notions of `success` and `abort` to
describe whether programs will successfully run to completion within a
transaction on the blockchain:

```
(@valid abort)
(defun failure-guaranteed:bool ()
  (enforce false "cannot pass"))
```

TODO: more

-->

### Keyset Authorization

TODO: name-based
TODO: value-based

### Database access

TODO

### Mass conservation

TODO

### Universal and existential quantification

TODO

## A worked example

TODO ...balance transfer...
