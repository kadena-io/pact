![](img/kadena-logo-210px.png)

The Pact Property Checking System
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
checks the code against all possible inputs, and therefore all
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

Here's an example of Pact's properties in action -- we declare a property
alongside the docstring of the function to which it corresponds. Note that the
function delegates its implementation of keyset enforcement to another
function, `enforce-admin`, and we don't need to be concerned about its
internal details. Our property states that if the transaction
submitted to the blockchain runs successfully, it must be the case that the
transaction has the proper signatures to satisfy the keyset named `admins`:

```lisp
(defun read-account (id)
  @doc   "Read data for account ID"
  @model (properties [(authorized-by 'admins)])

  (enforce-admin)
  (read 'accounts id ['balance 'ccy 'amount]))
```

There's a set of square brackets around our property because Pact allows
multiple properties to be defined simultaneously:

```lisp
(properties [p1 p2 p3 ...])
```

Next, we see an example of schema invariants. For any table with the following
schema, if our property checker succeeds, we know that all possible code paths
will always maintain the invariant that token balances are greater than zero:

```lisp
(defschema tokens
  @doc   "token schema"
  @model (invariants [(> balance 0)])

  username:string
  balance:integer)
```

## How does it work?

Pact's property checker works by realizing the language's semantics in an SMT
("Satisfiability Modulo Theories") solver -- by building a formula for a
program, and testing the validity of that formula. The SMT solver can prove
that there is no possible assignment of values to variables which can falsify a
provided proposition about some Pact code. Pact currently uses Microsoft's [Z3
theorem prover](https://github.com/Z3Prover/z3/wiki) to power its property
checking system.

Such a formula is built from the combination of the functions in a Pact module,
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

```lisp
(verify 'module-name)
```

This will typecheck the code and, if that succeeds, check all invariants and
properties.

## Expressing properties

### Arguments, return values, and standard arithmetic and comparison operators

In properties, we can refer to function arguments directly by their names, and
return values can be referred to by the name `result`:

```lisp
(defun negate:integer (x:integer)
  @doc   "negate a number"
  @model (properties [(= result (* -1 x))])

  (* x -1))
```

Here you can also see that the standard arithmetic operators on integers and
decimals work as they do in normal Pact code.

We can also define properties in terms of the standard comparison operators:

```lisp
(defun abs:integer (x:integer)
  @doc   "absolute value"
  @model (properties [(>= result 0)])

  (if (< x 0)
    (negate x)
    x))
```

### Boolean operators

In addition to the standard boolean operators `and`, `or`, and `not`, Pact's
property checking language supports logical implication in the form of `when`,
where `(when x y)` is equivalent to `(or (not x) y)`. Here we define three
properties at once:

```lisp
(defun negate:integer (x:integer)
  @doc   "negate a number"
  @model (properties
    [(when (< x 0) (> result 0))
     (when (> x 0) (< result 0))
     (and
       (when (< x 0) (> result 0))
       (when (> x 0) (< result 0)))])

  (* x -1))
```

### Transaction abort and success

By default, every property is predicated on the successful completion of the
transaction which would contain an invocation of the function being tested. This
means that properties like the following:

```lisp
(defun ensured-positive (val:integer)
  @doc   "halts when passed a non-positive number"
  @model (properties [(!= result 0)])

  (enforce (> val 0) "val is not positive")
  val)
```

will pass due to the use of `enforce`.

At run-time on the blockchain, if an `enforce` call fails, the containing
transaction is aborted. Because `properties` are only concerned with
transactions that succeed, the necessary conditions to pass each `enforce` call
are assumed.

### More comprehensive properties API documentation

For the full listing of functionality available in properties, see the API
documentation at [Property and Invariant
Functions](http://pact-language.readthedocs.io/en/latest/pact-properties-api.html).

## Expressing schema invariants

Schema invariants are described by a more restricted subset of the
functionality available in property definitions -- effectively the functions
which are not concerned with authorization, DB access, transaction
success/failure, and function arguments and return values. See the API
documentation at
[Property and Invariant
Functions](http://pact-language.readthedocs.io/en/latest/pact-properties-api.html)
for the full listing of functions available in invariant definitions.

<!--- *** This second is disabled until we add `valid`/`satisfiable` alternatives to `property`, which currently assumes tx success ***

### Valid, satisfiable, and explicit transaction abort/success

TODO: more. talk about valid, satisfiable, and the lack of the default
success condition of property.

Pact's property language supports the notions of `success` and `abort` to
describe whether programs will successfully run to completion within a
transaction on the blockchain:

```
(defun failure-guaranteed:bool ()
  ("always fails" (valid abort))
  (enforce false "cannot pass"))
```

TODO: more

-->

### Keyset Authorization

In Pact, keys can be referred to by predefined names (defined by
`define-keyset`) or passed around as values. The property checking system
supports both styles of working with keysets.

For named keysets, the property `authorized-by` holds only if every possible
code path enforces the keyset:

```lisp
(defun admins-only (action:string)
  @doc   "Only admins or super-admins can call this function successfully.
  @model (properties
    [(or (authorized-by 'admins) (authorized-by 'super-admins))
     (when (== "create" action) (authorized-by 'super-admins))])

  (if (== action "create")
    (create)
    (if (== action "update")
      (update)
      (incorrect-action action))))
```

For the common pattern of row-level keyset enforcement, wherein a table might
contain a row for each user, and each user's row contains a keyset that is
authorized when the row is modified, we can ensure this pattern has been
implemented correctly by using the `row-enforced` property.

For the following property to pass, the code must extract the keyset stored in
the `ks` column in the `accounts` table for the row keyed by the variable
`name`, and enforce it using `enforce-keyset`:

```lisp
(row-enforced 'accounts 'ks name)
```

For some examples of `row-enforced` in action, see "A simple balance transfer
example" and the section on "universal and existential quantification" below.

### Database access

To describe database table access, the property language has the following
properties:

- `(table-written 'accounts)` - that any cell of the table `accounts` is written
- `(table-read 'accounts)` - that any cell of the table `accounts` is read
- `(row-written 'accounts k)` - that the row keyed by the variable `k` is written
- `(row-read 'accounts k)` - that the row keyed by the variable `k` is read

For more details, see an example in "universal and existential quantification"
below.

### Mass conservation and column deltas

In some situations, it's desirable that the total sum of the values in a column
remains the same before and after a transaction. Or to put it another way, that
the sum of all updates to a column zeroes-out by the end of a transaction. To
capture this pattern, we have the `conserves-mass` property which takes a table
and column name:

```lisp
(conserves-mass 'accounts 'balance)
```

For an example using this property, see "A simple balance transfer example"
below.

It turns out that `conserves-mass` is actually just a trivial application of
another property called `column-delta`, which returns a numeric value of the
sum of all changes to the column during the transaction. So
`(conserves-mass 'accounts 'balance)` is actually just the same as:

```lisp
(= 0 (column-delta 'accounts 'balance))
```

We can also use `column-delta` to ensure that a column only ever increases
monotonically:

```lisp
(>= 0 (column-delta 'accounts 'balance))
```

or that it increases by a set amount during a transaction:

```lisp
(= 1 (column-delta 'accounts 'balance))
```

`column-delta` is defined in terms of the increase of the column from before to
after the transaction (i.e. `after - before`) -- not an absolute value of
change. So here `1` means an increase of `1` to the column's total sum.

### Universal and existential quantification

In examples like `(row-enforced 'accounts 'ks key)` or
`(row-written 'accounts key)` above, we've so far only referred to function
arguments by the use of the variable named `key`. But what if we wanted to
talk about all possible rows that will be written, if a function doesn't simply
update a single row?

In such a situation we could use universal quantification to talk about _any_
such row:

```lisp
(properties
  [(forall (key:string)
     (when (row-written 'accounts key)
       (row-enforced 'accounts 'ks key)))])
```

This property says that for any possible row written by the function, the
keyset in column `ks` must be enforced for that row.

Likewise instead of quantifying over all possible keys, if we wanted to state
that there merely exists a row that is read during the transaction, we could
use existential quantification like so:

```lisp
(properties
  [(exists (key:string)
     (row-read 'accounts key))])
```

For both universal and existential quantification, note that a type annotation
is required.

### Defining and reusing properties

With `defproperty`, properties can be defined at the module level:

```lisp
(defmodule accounts
  @model
    [(defproperty conserves-mass
       (= (column-delta 'accounts 'balance) 0.0))
     (defproperty auth-required
       (authorized-by 'accounts-admin-keyset))]

  ; ...
  )
```

and then used at the function level by referring to the property's name:

```lisp
(defun read-account (id)
  @model (property auth-required)

  ; ...
  )
```

## A simple balance transfer example

Let's work through an example where we write a function to transfer some amount
of a balance across two accounts for the given table:

```lisp
(defschema account
  @doc "user accounts with balances"

  balance:integer
  ks:keyset)

(deftable accounts:{account})
```

The following code to transfer a balance between two accounts may look correct
at first study, but it turns out that there are number of bugs which we can
eradicate with the help of another property, and by adding an invariant to the
table.

```lisp
(defun transfer (from:string to:string amount:integer)
  @doc   "Transfer money between accounts"
  @model (properties [(row-enforced 'accounts 'ks from)])

  (with-read accounts from { 'balance := from-bal, 'ks := from-ks }
    (with-read accounts to { 'balance := to-bal }
      (enforce-keyset from-ks)
      (enforce (>= from-bal amount) "Insufficient Funds")
      (update accounts from { "balance": (- from-bal amount) })
      (update accounts to   { "balance": (+ to-bal amount) }))))
```

Let's start by adding an invariant that balances can never drop below zero:

```lisp
(defschema account
  @doc   "user accounts with balances"
  @model (invariants [(>= balance 0)])

  balance:integer
  ks:keyset)
```

Now, when we use `verify` to check all properties in this module, Pact's
property checker points out that it's able to falsify the positive balance
invariant by passing in an `amount` of `-1` (when the balance is `0`). In this
case it's actually possible for the "sender" to steal money from anyone else by
tranferring a negative amount! Let's fix that by enforcing `(> amount 0)`, and
try again:

```lisp
(defun transfer (from:string to:string amount:integer)
  @doc   "Transfer money between accounts"
  @model (properties [(row-enforced 'accounts 'ks from)])

  (with-read accounts from { 'balance := from-bal, 'ks := from-ks }
    (with-read accounts to { 'balance := to-bal }
      (enforce-keyset from-ks)
      (enforce (>= from-bal amount) "Insufficient Funds")
      (enforce (> amount 0)         "Non-positive amount")
      (update accounts from { "balance": (- from-bal amount) })
      (update accounts to   { "balance": (+ to-bal amount) }))))
```

The property checker validates the code at this point, but let's add another
property `(conserves-mass 'accounts 'balance)` to ensure that it's not possible
for the function to be used to create or destroy any money:

```lisp
(defun transfer (from:string to:string amount:integer)
  @doc   "Transfer money between accounts"
  @model (properties
    [(row-enforced 'accounts 'ks from)
     (conserves-mass 'accounts 'balance)])

  (with-read accounts from { 'balance := from-bal, 'ks := from-ks }
    (with-read accounts to { 'balance := to-bal }
      (enforce-keyset from-ks)
      (enforce (>= from-bal amount) "Insufficient Funds")
      (enforce (> amount 0)         "Non-positive amount")
      (update accounts from { "balance": (- from-bal amount) })
      (update accounts to   { "balance": (+ to-bal amount) }))))
```

When we run `verify` this time, the property checker finds a bug again -- it's
able to falsify the property when `from` and `to` are set to the same account.
When this is the case, we see that the code actually creates money out of thin
air!

To see how, let's focus on the two `update` calls, where `from` and `to` are
set to the same value, and `from-bal` and `to-bal` are also set to what we'll
call `previous-balance`:

```lisp
(update accounts "alice" { "balance": (- previous-balance amount) })
(update accounts "alice" { "balance": (+ previous-balance amount) })
```

In this scenario, we can see that the second `update` call will completely
overwrite the first one, with the value `(+ previous-balance amount)`. Alice
has effectively created `amount` tokens for free!

We can fix this by adding another `enforce` (with `(!= from to)`) to prevent
this unintended behavior:

```lisp
(defun transfer (from:string to:string amount:integer)
  @doc   "Transfer money between accounts"
  @model (properties
    [(row-enforced 'accounts 'ks from)
     (conserves-mass 'accounts 'balance)])

  (with-read accounts from { 'balance := from-bal, 'ks := from-ks }
    (with-read accounts to { 'balance := to-bal }
      (enforce-keyset from-ks)
      (enforce (>= from-bal amount) "Insufficient Funds")
      (enforce (> amount 0)         "Non-positive amount")
      (enforce (!= from to)         "Sender is the recipient")
      (update accounts from { "balance": (- from-bal amount) })
      (update accounts to   { "balance": (+ to-bal amount) }))))
```

And now we see that finally the property checker verifies that all of the
following are true:

- the sender must be authorized to transfer money,
- it's not possible for a balance to drop below zero, and
- it's not possible for money to be created or destroyed.
