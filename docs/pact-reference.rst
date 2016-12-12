.. figure:: img/kadena-logo-210px.png
   :alt: 

This document is a reference for the Pact smart-contract language,
designed for correct, transactional execution on a `high-performance
blockchain <http://kadena.io>`__. For more background, please see the
`white
paper <http://kadena.io/docs/Kadena-PactWhitepaper-Oct2016.pdf>`__.

Copyright (c) 2016, Stuart Popejoy. All Rights Reserved.

Concepts
========

Execution Modes
---------------

Pact is designed to be used in distinct *execution modes* to address the
performance requirements of rapid linear execution on a blockchain.
These are:

1. Contract definition.
2. Transaction execution.
3. Queries and local execution.

Contract Definition
~~~~~~~~~~~~~~~~~~~

In this mode, a large amount of code is sent into the blockchain to
establish the smart contract, as comprised of code (modules), tables
(data), and keysets (authorization). This can also include
"transactional" (database-modifying) code, for instance to initialize
data.

For a given smart contract, these should all be sent as a single message
into the blockchain, so that any error will rollback the entire smart
contract as a unit.

Keyset definition
^^^^^^^^^^^^^^^^^

`Keysets <#keysets>`__ are customarily defined first, as they are used
to specify admin authorization schemes for modules and tables.

Module declaration
^^^^^^^^^^^^^^^^^^

`Modules <#module>`__ define the API for smart contracts, comprised of
`function <#defun>`__ and `"pact" <#defpact>`__ definitions. When a
module is declared, all references to native functions or functions from
other modules are resolved. Resolution failure results in transaction
rollback.

Modules can be re-defined as controlled by their admin keyset. Module
versioning is not supported, except by including a version sigil in the
module name (e.g., "accounts-v1").

Table Creation
^^^^^^^^^^^^^^

Tables are `created <#create-table>`__ at the same time as modules, due
to the tight relationship of database tables to code modules, as
described in `Table Guards <#tableguards>`__.

There is no restriction on how many tables may be created. However,
table names are not namespaced, so care must be taken to ensure unique
names.

Tables are schema-less, so there is no need to drop or redefine tables.
Table data can be migrated for new module code by simply executing the
necessary data modification.

Transaction Execution
~~~~~~~~~~~~~~~~~~~~~

"Transactions" refer to business events enacted on the blockchain, like
a payment, a sale, or a workflow step of a complex contractual
agreement. A transaction is generally a single call to a module
function. However there is no limit on how many statements can be
executed. Indeed, the difference between "transactions" and "smart
contract definition" is simply the *kind* of code executed, not any
actual difference in the code evaluation.

Queries and Local Execution
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Querying data is generally not a business event, and can involve data
payloads that could impact performance, so querying is carried out as a
*local execution* on the node receiving the message. Historical queries
use a *transaction ID* as a point of reference, to avoid any race
conditions and allow asynchronous query execution.

Transactional vs local execution is accomplished by targeting different
API endpoints; pact code has no ability to distinguish between
transactional and local execution.

Database Interaction
--------------------

Pact presents a database metaphor reflecting the unique requirements of
blockchain execution, which can be adapted to run on different
back-ends.

Atomic execution
~~~~~~~~~~~~~~~~

A single message sent into the blockchain to be evaluated by Pact is
*atomic*: the transaction succeeds as a unit, or does not succeed at
all, known as "transactions" in database literature. There is no
explicit support for rollback handling, except in `multi-step
transactions <#pacts>`__.

Key-Row Model
~~~~~~~~~~~~~

Blockchain execution can be likened to OLTP (online transaction
processing) database workloads, which favor denormalized data written to
a single table. Pact's data-access API reflects this by presenting a
*key-row* model, where a row of column values is accessed by a single
key.

As a result, Pact does not support *joining* tables, which is more
suited for an OLAP (online analytical processing) database, populated
from exports from the Pact database. This does not mean Pact cannot
*record* transactions using relational techniques -- for example, a
Customer table whose keys are used in a Sales table would involve the
code looking up the Customer record before writing to the Sales table.

Schema-less
~~~~~~~~~~~

Pact tables do not declare a schema. Modules are expected to enforce
data access to ensure type safety (through the use of functions like
`is-string <#is-string>`__, `is-decimal <#is-decimal>`__ etc).

No Nulls
~~~~~~~~

Pact has no concept of a NULL value in its database metaphor. The main
function for computing on database results, `with-read <#with-read>`__,
will error if any column value is not found. Authors must ensure that
values are present for any transactional read. This is a safety feature
to ensure *totality* and avoid needless, unsafe control-flow surrounding
null values.

Versioned History
~~~~~~~~~~~~~~~~~

The key-row model is augmented by every change to column values being
versioned by transaction ID. For example, a table with three columns
"name", "age", and "role" might update "name" in transaction #1, and
"age" and "role" in transaction #2. Retreiving historical data will
return just the change to "name" under transaction 1, and the change to
"age" and "role" in transaction #2.

Back-ends
~~~~~~~~~

Pact guarantees identical, correct execution at the smart-contract layer
within the blockchain. As a result, the backing store need not be
identical on different consensus nodes. Pact's implementation allows for
integration of industrial RDBMSs, to assist large migrations onto a
blockchain-based system, by facilitating bulk replication of data to
downstream systems.

Keysets and Authorization
-------------------------

Pact is inspired by Bitcoin scripts to incorporate public-key
authorization directly into smart contract execution and administration.

Keyset definition
~~~~~~~~~~~~~~~~~

Keysets are `defined <#define-keyset>`__ by `reading <#read-keyset>`__
definitions from the message payload. Keysets consist of a list of
public keys and a *keyset predicate*.

Keyset Predicates
~~~~~~~~~~~~~~~~~

A keyset predicate references a function by name which will compare the
public keys in the keyset to the key or keys used to sign the blockchain
message. The function accepts two arguments, "count" and "matched",
where "count" is the number of keys in the keyset and "matched" is how
many keys on the message signature matched a keyset key.

Support for multiple signatures is the responsibility of the blockchain
layer, and is a powerful feature for Bitcoin-style "multisig" contracts
(ie requiring at least two signatures to release funds).

Pact comes with built-in keyset predicates: `keys-all <#keys-all>`__,
`keys-any <#keys-any>`__, `keys-2 <#keys-2>`__. Module authors are free
to define additional predicates.

Key rotation
~~~~~~~~~~~~

Keysets can be rotated, but only by messages authorized against the
current keyset definition and predicate. Once authorized, the keyset can
be easily `redefined <#define-keyset>`__.

Module Table Guards
~~~~~~~~~~~~~~~~~~~

When `creating <#create-table>`__ a table, a module name must also be
specified. By this mechanism, tables are "guarded" or "encapsulated" by
the module, such that direct access to the table via `data-access
functions <#Database>`__ is authorized by the module's admin keyset.
However, *within module functions*, table access is unconstrained. This
gives contract authors great flexibility in designing data access, and
is intended to enshrine the module as the main "user" data access API.

Row-level keysets
~~~~~~~~~~~~~~~~~

Keysets can be stored as a column value in a row, allowing for
*row-level* authorization. The following code indicates how this might
be achieved:

.. code:: lisp

    (defun create-account (id)
      (insert 'accounts id { "balance": 0.0, "keyset": (read-keyset "owner-keyset") }))

    (defun read-balance (id)
      (with-read { "balance":= bal, "keyset":= ks }
        (with-keyset ks
          (format "Your balance is {}" bal))))

In the example, ``create-account`` reads a keyset definition from the
message payload using `read-keyset <#read-keyset>`__ to store as
"keyset" in the table. ``read-balance`` only allows that owner's keyset
to read the balance, by first enforcing the keyset using
`with-keyset <#with-keyset>`__.

Computational Model
-------------------

Here we cover various aspects of Pact's approach to computation.

Turing-Incomplete
~~~~~~~~~~~~~~~~~

Pact is turing-incomplete, in that there is no recursion (recursion is
detected before execution and results in an error) and no ability to
loop indefinitely. Pact does support operation on list structures via
`map <#map>`__, `fold <#fold>`__ and `filter <#filter>`__, but since
there is no ability to define infinite lists, these are necessarily
bounded.

Turing-incompleteness allows Pact module loading to resolve all
references in advance, meaning that instead of addressing functions in a
lookup table, the function definition is directly injected (or
"inlined") into the callsite. This is an example of the performance
advantages of a Turing-incomplete language.

Variables
~~~~~~~~~

Pact allows variable declarations in `let expressions <#let>`__ and
`bindings <#binding>`__. Variables are immutable: they cannot be
re-assigned or modified in-place. The most common variable declaration
occurs in the `with-read <#with-read>`__ function, assigning variables
to column values by name.

Module-global constant values can be declared with
`defconst <#defconst>`__.

Data Types
~~~~~~~~~~

Pact is not a explicitly-typed language, but does use fixed type
representations "under the hood" and does no coercion of types, so is
strongly-typed nonetheless. Authors can employ functions like
`is-string <#is-string>`__, `is-decimal <#is-decimal>`__ to enforce type
safety.

Pact's supported types are:

-  `Strings <#string>`__
-  `Integers <#integer>`__
-  `Decimals <#decimal>`__
-  `Booleans <#boolean>`__
-  `Key sets <#keysets>`__
-  `Lists <#list>`__
-  `Objects <#object>`__
-  `Function <#defun>`__ and `pact <#defpact>`__ definitions
-  `JSON values <#json>`__

`Functions <#defun>`__ may take no arguments and return bare values,
which can function as *constants*.

Performance
~~~~~~~~~~~

Pact is designed to maximize the performance of `transaction
execution <#transactionexec>`__, penalizing queries and module
definition in favor of fast recording of business events on the
blockchain. Some tips for fast execution are:

Single-function transactions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Design transactions so they can be executed with a single function call.

Call with references instead of ``use``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

When calling module functions in transactions, use `reference
syntax <#reference>`__ instead of importing the module with
`use <#use>`__. When defining modules that reference other module
functions, ``use`` is fine, as those references will be inlined at
module definition time.

Hardcoded arguments vs. message values
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

A transaction can encode values directly into the transactional code:

::

    (accounts.transfer "Acct1" "Acct2" 100.00)

or it can read values from the message JSON payload:

::

    (defun transfer-msg ()
      (transfer (read-msg "from") (read-msg "to")
                (read-decimal "amount")))
    ...
    (accounts.transfer-msg)

The latter will execute slightly faster, as there is less code to
interpret at transaction time.

Control Flow
~~~~~~~~~~~~

Pact supports conditionals via `if <#if>`__, bounded looping, and of
course function application.

"If" considered harmful
^^^^^^^^^^^^^^^^^^^^^^^

Consider avoiding ``if`` wherever possible: every branch makes code
harder to understand and more prone to bugs. The best practice is to put
"what am I doing" code in the front-end, and "validate this transaction
which I intend to succeed" code in the smart contract.

Pact's original design left out ``if`` altogether (and looping), but it
was decided that users should be able to judiciously use these features
as necessary.

Use enforce
^^^^^^^^^^^

"If" should never be used to enforce business logic invariants: instead,
`enforce <#enforce>`__ is the right choice, which will fail the
transaction.

Indeed, failure is the only *non-local exit* allowed by Pact. This
reflects Pact's emphasis on *totality*.

Functional Concepts
~~~~~~~~~~~~~~~~~~~

Pact includes the functional-programming "greatest hits":
`map <#map>`__, `fold <#fold>`__ and `filter <#filter>`__. These all
employ `partial application <#partialapplication>`__, where the list
item is appended onto the application arguments in order to serially
execute the function.

.. code:: lisp

    (map (+ 2) [1 2 3])
    (fold (+) ["Concatenate" " " "me"]

Pact also has `compose <#compose>`__, which allows "chaining"
applications in a functional style.

LISP
~~~~

Pact's use of LISP syntax is intended to make the code reflect its
runtime representation directly, allowing contract authors focus
directly on program execution. Pact code is stored in human-readable
form on the ledger, such that the code can be directly verified, but the
use of LISP-style `s-expression syntax <#sexp>`__ allows this code to
execute quickly.

Message Data
~~~~~~~~~~~~

Pact expects code to arrive in a message with a JSON payload and
signatures. Message data is read using `read-msg <#read-msg>`__ and
related functions, while signatures are not directly readable or
writable -- they are evaluated as part of `keyset
predicate <#keysetpredicates>`__ enforcement.

JSON support
^^^^^^^^^^^^

Values returned from Pact transactions are expected to be directly
represented as JSON values.

When reading values from a message via `read-msg <#read-msg>`__, Pact
coerces JSON types as follows:

-  String -> String
-  Number -> Integer (rounded)
-  Boolean -> Boolean
-  Object -> JSON Value
-  Array -> JSON Value
-  Null -> JSON Value

Decimal values are represented as Strings and read using
`read-decimal <#read-decimal>`__.

JSON Objects, Arrays, and Nulls are not coerced, intended for direct
storage and retreival as opaque payloads in the database.

Confidentiality
---------------

Pact is designed to be used in a *confidentiality-preserving*
environment, where messages are only visible to a subset of
participants. This has significant implications for smart contract
execution.

Entities
~~~~~~~~

An *entity* is a business participant that is able or not able to see a
confidential message. An entity might be a company, a group within a
company, or an individual.

Disjoint Databases
~~~~~~~~~~~~~~~~~~

Pact smart contracts operate on messages organized by a blockchain, and
serve to produce a database of record, containing results of
transactional executions. In a confidential environment, different
entities execute different transactions, meaning the resulting databases
are now *disjoint*.

This does not affect Pact execution; however, database data can no
longer enact a "two-sided transaction", meaning we need a new concept to
handle enacting a single transaction over multiple disjoint datasets.

Pacts
~~~~~

Pacts are multi-step sequential transactions that are defined as a
single body of code called a `pact <#defpact>`__. With a pact,
participants ensure they are executing an identical code path, even as
they execute distinct "steps" in that path.

The concept of pacts reflect *coroutines* in software engineering:
functions that can *yield* and *resume* computation "in the middle of"
their body. A `step <#step>`__ in a pact designates a target entity to
execute it, after which the pact "yields" execution, completing the
transaction and initiating a signed "Resume" message into the
blockchain.

The counterparty entity sees this "Resume" message and drops back into
the pact body to find if the next step is targetted for it, if so
executing it.

Since any step can fail, steps can be designed with
`rollbacks <#step-with-rollback>`__ to undo changes if a subsequent step
fails.

Syntax
======

Literals
--------

Strings
~~~~~~~

String literals are created with double-ticks:

::

    pact> "a string"
    "a string"

Strings also support multiline by putting a backslash before and after
whitespace (not interactively).

.. code:: lisp

    (defun id (a)
      "Identity function. \
      \Argument is returned."
      a)

Symbols
~~~~~~~

Symbols are string literals representing some unique item in the
runtime, like a function or a table name. Their representation
internally is simply a string literal so their usage is idiomatic.

Symbols are created with a preceding tick, thus they do no support
whitespace or multiline.

::

    pact> 'a-symbol
    "a-symbol"

Integers
~~~~~~~~

Integer literals are unbounded positive naturals. For negative numbers
use the unary `- <#n->`__ function.

::

    pact> 12345
    12345

Decimals
~~~~~~~~

Decimal literals are positive decimals to exact expressed precision.

::

    pact> 100.25
    100.25
    pact> 356452.23451872
    356452.23451872

Booleans
~~~~~~~~

Booleans are represented by ``true`` and ``false`` literals.

::

    pact> (and true false)
    false

Lists
~~~~~

List literals are created with brackets. This is actually a special
form, which evaluates the `list <#listfun>`__ function.

::

    pact> [1 2 3]
    [1 2 3]
    pact> (= [1 2 3] (list 1 2 3))
    true

Objects
~~~~~~~

Objects are dictionaries, created with curly-braces specifying key-value
pairs using a colon ``:``. For certain applications (database updates),
keys must be strings.

::

    pact> { "foo": (+ 1 2), "bar": "baz" }
    (TObject [("foo",3),("bar","baz")])

Bindings
~~~~~~~~

Bindings are dictionary-like forms, also created with curly braces, to
bind database results to variables using the ``:=`` operator. They are
used in `with-read <#with-read>`__,
`with-default-read <#with-default-read>`__, and `bind <#bind>`__ to
assign variables to named columns in a row, or values in an object.

.. code:: lisp

    (defun check-balance (id)
      (with-read 'accounts id { "balance" := bal }
        (enforce (> bal 0) (format "Account in overdraft: {}" bal))))

Special forms
-------------

defun
~~~~~

``(defun NAME ARGLIST [DOCSTRING] BODY...)``

Define NAME as a function, accepting ARGLIST arguments, with optional
DOCSTRING. Arguments are in scope for BODY, one or more expressions.

.. code:: lisp

    (defun add3 (a b c) (+ a (+ b c)))

    (defun scale3 (a b c s) "multiply sum of A B C times s"
      (* s (add3 a b c)))

defconst
~~~~~~~~

``(defun NAME VALUE [DOCSTRING])``

Define NAME as VALUE, with option DOCSTRING.

.. code:: lisp

    (defconst COLOR_RED="#FF0000" "Red in hex")
    (defconst COLOR_GRN="#00FF00" "Green in hex")
    (defconst PI 3.14159265 "Pi to 8 decimals")

defpact
~~~~~~~

``(defpact NAME ARGLIST [DOCSTRING] STEPS...)``

Define NAME as a *pact*, a multistep computation intended for private
transactions. Identical to `defun <#defun>`__ except body must be
comprised of `steps <#step>`__.

.. code:: lisp

    (defpact payment (payer payer-entity payee
                      payee-entity amount)
      (step-with-rollback payer-entity
        (debit payer amount)
        (credit payer amount))
      (step payee-entity
        (credit payee amount)))

let
~~~

``(let (BINDPAIR [BINDPAIR [...]]) BODY)``

Bind variables in BINDPAIRs to be in scope over BODY. Variables within
BINDPAIRs cannot refer to previously-declared variables in the same let
binding; for this use (let\*){#letstar}.

.. code:: lisp

    (let ((x 2)
          (y 5))
      (* x y))
    > 10

let\*
~~~~~

``(let\* (BINDPAIR [BINDPAIR [...]]) BODY)``

Bind variables in BINDPAIRs to be in scope over BODY. Variables can
reference previously declared BINDPAIRS in the same let. ``let\*`` is
expanded at compile-time to nested ``let`` calls for each BINDPAIR; thus
``let`` is preferred where possible.

.. code:: lisp

    (let* ((x 2)
           (y (* x 10)))
      (+ x y))
    > 22

step
~~~~

``(step ENTITY EXPR)``

Define a step within a *pact*, which can only be executed by nodes
representing ENTITY, in order of execution specified in containing
`defpact <#defpact>`__.

step-with-rollback
~~~~~~~~~~~~~~~~~~

``(step-with-rollback ENTITY EXPR ROLLBACK-EXPR)``

Define a step within a *pact*, which can only be executed by nodes
representing ENTITY, in order of execution specified in containing
`defpact <#defpact>`__. If any subsequent steps fail, ROLLBACK-EXPR will
be executed.

use
~~~

``(use MODULE-SYMBOL)``

Import an existing module into namespace.

.. code:: lisp

    (use 'accounts)
    (transfer "123" "456" 5 (time "2016-07-22T11:26:35Z"))
    "Write succeeded"

module
~~~~~~

``(module NAME KEYSET [DOCSTRING] DEFS...)``

Define and install module NAME, guarded by keyset KEYSET, with optional
DOCSTRING. DEFS must be `defun <#defun>`__ or `defpact <#defpact>`__
expressions only.

.. code:: lisp

    (module accounts 'accounts-admin
      "Module for interacting with accounts"

      (defun create-account (id bal)
       "Create account ID with initial balance BAL"
       (insert 'accounts id { "balance": bal }))

      (defun transfer (from to amount)
       "Transfer AMOUNT from FROM to TO"
       (with-read 'accounts from { "balance": fbal }
        (enforce (<= amount fbal) "Insufficient funds")
         (with-read 'accounts to { "balance": tbal }
          (update 'accounts from { "balance": (- fbal amount) })
          (update 'accounts to { "balance": (+ tbal amount) }))))
    )

Expressions
-----------

Expressions may be `literals <#literals>`__, atoms, s-expressions, or
references.

Atoms
~~~~~

Atoms are non-reserved barewords starting with a letter or allowed
symbol, and containing letters, digits and allowed symbols. Allowed
symbols are ``%#+-_&$@<>=?*!|/``. Atoms must resolve to a variable bound
by a `defun <#defun>`__, `defpact <#defpact>`__, `binding <#binding>`__
form, or to symbols imported into the namespace with `use <#use>`__.

S-expressions
~~~~~~~~~~~~~

S-expressions are formed with parentheses, with the first atom
determining if the expression is a `special form <#special>`__ or a
function application, in which case the first atom must refer to a
definition.

Partial application
^^^^^^^^^^^^^^^^^^^

An application with less than the required arguments is in some contexts
a valid *partial application* of the function. However, this is only
supported in Pact's `functional-style functions <#fp>`__; anywhere else
this will result in a runtime error.

References
~~~~~~~~~~

References are two atoms joined by a dot ``.`` to directly resolve to
module definitions.

::

    pact> accounts.transfer
    "(defun accounts.transfer (src,dest,amount,date) \"transfer AMOUNT from
    SRC to DEST\")"
    pact> transfer
    Eval failure:
    transfer<EOF>: Cannot resolve transfer
    pact> (use 'accounts)
    "Using \"accounts\""
    pact> transfer
    "(defun accounts.transfer (src,dest,amount,date) \"transfer AMOUNT from
    SRC to DEST\")"

References are preferred to ``use`` for transactions, as references
resolve faster. However in module definition, ``use`` is preferred for
legibility.

Built-in Functions
==================

General
-------

at
~~

*idx* ``integer`` *list* ``[<a>]`` *→* ``<a>``

*idx* ``string`` *object* ``object`` *→* ``<a>``

Index LIST at IDX, or get value with key IDX from OBJECT.

.. code:: lisp

    pact> (at 1 [1 2 3])
    2
    pact> (at "bar" { "foo": 1, "bar": 2 })
    2

bind
~~~~

*src* ``object`` *bindings* ``binding`` *body* ``@rest`` *→* ``<a>``

Evaluate SRC which must return an object, using BINDINGS to bind
variables to values used in BODY.

.. code:: lisp

    pact> (bind { "a": 1, "b": 2 } { "a" := a-value } a-value)
    1

compose
~~~~~~~

*x* ``function: (x:<a>) -> <b>`` *y* ``function: (x:<b>) -> <c>``
*value* ``<a>`` *→* ``<c>``

Compose X and Y, such that X operates on VALUE, and Y on the results of
X.

.. code:: lisp

    pact> (filter (compose (length) (< 2)) ["my" "dog" "has" "fleas"])
    ["dog" "has" "fleas"]

drop
~~~~

*count* ``integer`` *list* ``<a => (list|string)>``
*→* ``<a => (list|string)>``

Drop COUNT values from LIST (or string). If negative, drop from end.

.. code:: lisp

    pact> (drop 2 "vwxyz")
    "xyz"
    pact> (drop (- 2) [1 2 3 4 5])
    [1 2 3]

enforce
~~~~~~~

*test* ``bool`` *msg* ``string`` *→* ``bool``

Fail transaction with MSG if TEST fails, or returns true.

.. code:: lisp

    pact> (enforce (!= (+ 2 2) 4) "Chaos reigns")
    <interactive>:1:0:Failure: Chaos reigns

filter
~~~~~~

*app* ``function: (x:<a>) -> bool`` *list* ``[<a>]`` *→* ``[<a>]``

Filter LIST by applying APP to each element to get a boolean determining
inclusion.

.. code:: lisp

    pact> (filter (compose (length) (< 2)) ["my" "dog" "has" "fleas"])
    ["dog" "has" "fleas"]

fold
~~~~

*app* ``function: (x:<b> y:<b>) -> <a>`` *init* ``<a>`` *list* ``[<b>]``
*→* ``<a>``

Iteratively reduce LIST by applying APP to last result and element,
starting with INIT.

.. code:: lisp

    pact> (fold (+) 0 [100 10 5])
    115

format
~~~~~~

*template* ``string`` *vars* ``@rest`` *→* ``string``

Interpolate VARS into TEMPLATE using {}.

.. code:: lisp

    pact> (format "My {} has {}" "dog" "fleas")
    "My dog has fleas"

if
~~

*cond* ``bool`` *then* ``<a>`` *else* ``<a>`` *→* ``<a>``

Test COND, if true evaluate THEN, otherwise evaluate ELSE.

.. code:: lisp

    pact> (if (= (+ 2 2) 4) "Sanity prevails" "Chaos reigns")
    "Sanity prevails"

is-bool
~~~~~~~

*val* ``bool`` *→* ``bool``

Return VAL, enforcing boolean type.

.. code:: lisp

    pact> (is-bool true)
    true

is-decimal
~~~~~~~~~~

*val* ``decimal`` *→* ``decimal``

Return VAL, enforcing decimal type.

.. code:: lisp

    pact> (is-decimal 123.45)
    123.45

is-integer
~~~~~~~~~~

*val* ``integer`` *→* ``integer``

Return VAL, enforcing integer type

.. code:: lisp

    pact> (is-integer 123)
    123
    pact> (is-integer "abc")
    <interactive>:0:0:Not integer: "abc"

is-string
~~~~~~~~~

*val* ``string`` *→* ``string``

Return VAL, enforcing string type.

.. code:: lisp

    pact> (is-string 123)
    <interactive>:0:0:Not string: 123
    pact> (is-string "abc")
    "abc"

is-time
~~~~~~~

*val* ``time`` *→* ``time``

Return VAL, enforcing time type.

.. code:: lisp

    pact> (is-time (time "2016-07-22T11:26:35Z"))
    "2016-07-22T11:26:35Z"

length
~~~~~~

*x* ``<a => (list|string|object)>`` *→* ``integer``

Compute length of X, which can be a list, a string, or an object.

.. code:: lisp

    pact> (length [1 2 3])
    3
    pact> (length "abcdefgh")
    8
    pact> (length { "a": 1, "b": 2 })
    2

list
~~~~

*elems* ``@rest`` *→* ``list``

Create list from ELEMS.

.. code:: lisp

    pact> (list 1 2 3)
    [1 2 3]

list-modules
~~~~~~~~~~~~

*→* ``[string]``

List modules available for loading.

map
~~~

*app* ``function: (x:<b>) -> <a>`` *list* ``[<b>]`` *→* ``[<a>]``

Apply elements in LIST as last arg to APP, returning list of results.

.. code:: lisp

    pact> (map (+ 1) [1 2 3])
    [2 3 4]

pact-txid
~~~~~~~~~

*→* ``integer``

Return reference tx id for pact execution.

read-decimal
~~~~~~~~~~~~

*key* ``string`` *→* ``decimal``

Parse KEY string value from message data body as decimal.

.. code:: lisp

    (defun exec ()
       (transfer (read-msg "from") (read-msg "to") (read-decimal "amount")))

read-integer
~~~~~~~~~~~~

*key* ``string`` *→* ``integer``

Parse KEY string value from message data body as integer.

.. code:: lisp

    (read-integer "age")

read-msg
~~~~~~~~

*key* ``string`` *→* ``<a>``

Read KEY from message data body. Will recognize JSON types as
corresponding Pact type.

.. code:: lisp

    (defun exec ()
       (transfer (read-msg "from") (read-msg "to") (read-decimal "amount")))

remove
~~~~~~

*key* ``string`` *object* ``object`` *→* ``object``

Remove entry for KEY from OBJECT.

.. code:: lisp

    pact> (remove "bar" { "foo": 1, "bar": 2 })
    {"foo": 1})

take
~~~~

*count* ``integer`` *list* ``<a => (list|string)>``
*→* ``<a => (list|string)>``

Take COUNT values from LIST (or string). If negative, take from end.

.. code:: lisp

    pact> (take 2 "abcd")
    "ab"
    pact> (take (- 3) [1 2 3 4 5])
    [3 4 5]

typeof
~~~~~~

*x* ``<a>`` *→* ``string``

Returns type of X as string.

.. code:: lisp

    pact> (typeof "hello")
    "string"

Database
--------

create-table
~~~~~~~~~~~~

*table* ``string`` *module* ``string`` *→* ``string``

Create table TABLE guarded by module MODULE.

.. code:: lisp

    (create-table 'accounts 'accounts-admin)

describe-keyset
~~~~~~~~~~~~~~~

*keyset* ``string`` *→* ``value``

Get metadata for KEYSET

describe-module
~~~~~~~~~~~~~~~

*module* ``string`` *→* ``value``

Get metadata for MODULE

describe-table
~~~~~~~~~~~~~~

*table* ``string`` *→* ``value``

Get metadata for TABLE

insert
~~~~~~

*table* ``string`` *key* ``string`` *object* ``object`` *→* ``string``

Write entry in TABLE for KEY of OBJECT column data, failing if data
already exists for KEY.

.. code:: lisp

    (insert 'accounts { "balance": 0.0, "note": "Created account." })

keys
~~~~

*table* ``string`` *→* ``[string]``

Return all keys in TABLE.

.. code:: lisp

    (keys 'accounts)

read
~~~~

*table* ``string`` *key* ``string`` *→* ``object``

*table* ``string`` *key* ``string`` *columns* ``[string]``
*→* ``object``

Read row from TABLE for KEY returning database record object, or just
COLUMNS if specified.

.. code:: lisp

    (read 'accounts id ['balance 'ccy])

txids
~~~~~

*table* ``string`` *txid* ``integer`` *→* ``[integer]``

Return all txid values greater than or equal to TXID in TABLE.

.. code:: lisp

    (txids 'accounts 123849535)

txlog
~~~~~

*table* ``string`` *txid* ``integer`` *→* ``[value]``

Return all updates to TABLE performed in transaction TXID.

.. code:: lisp

    (txlog 'accounts 123485945)

update
~~~~~~

*table* ``string`` *key* ``string`` *object* ``object`` *→* ``string``

Write entry in TABLE for KEY of OBJECT column data, failing if data does
not exist for KEY.

.. code:: lisp

    (update 'accounts { "balance": (+ bal amount), "change": amount, "note": "credit" })

with-default-read
~~~~~~~~~~~~~~~~~

*table* ``string`` *key* ``string`` *defaults* ``object``
*bindings* ``binding`` *→* ``string``

Special form to read row from TABLE for KEY and bind columns per
BINDINGS over subsequent body statements. If row not found, read columns
from DEFAULTS, an object with matching key names.

.. code:: lisp

    (with-default-read 'accounts id { "balance": 0, "ccy": "USD" } { "balance":= bal, "ccy":= ccy }
       (format "Balance for {} is {} {}" id bal ccy))

with-read
~~~~~~~~~

*table* ``string`` *key* ``string`` *bindings* ``binding``
*→* ``string``

Special form to read row from TABLE for KEY and bind columns per
BINDINGS over subsequent body statements.

.. code:: lisp

    (with-read 'accounts id { "balance":= bal, "ccy":= ccy }
       (format "Balance for {} is {} {}" id bal ccy))

write
~~~~~

*table* ``string`` *key* ``string`` *object* ``object`` *→* ``string``

Write entry in TABLE for KEY of OBJECT column data.

.. code:: lisp

    (write 'accounts { "balance": 100.0 })

Time
----

add-time
~~~~~~~~

*time* ``time`` *seconds* ``decimal`` *→* ``time``

*time* ``time`` *seconds* ``integer`` *→* ``time``

Add SECONDS to TIME; SECONDS can be integer or decimal.

.. code:: lisp

    pact> (add-time (time "2016-07-22T12:00:00Z") 15)
    "2016-07-22T12:00:15Z"

days
~~~~

*n* ``decimal`` *→* ``decimal``

*n* ``integer`` *→* ``decimal``

N days, for use with 'add-time'

.. code:: lisp

    pact> (add-time (time "2016-07-22T12:00:00Z") (days 1))
    "2016-07-23T12:00:00Z"

diff-time
~~~~~~~~~

*time1* ``time`` *time2* ``time`` *→* ``decimal``

Compute difference between TIME1 and TIME2 in seconds.

.. code:: lisp

    pact> (diff-time (parse-time "%T" "16:00:00") (parse-time "%T" "09:30:00"))
    23400

hours
~~~~~

*n* ``decimal`` *→* ``decimal``

*n* ``integer`` *→* ``decimal``

N hours, for use with 'add-time'

.. code:: lisp

    pact> (add-time (time "2016-07-22T12:00:00Z") (hours 1))
    "2016-07-22T13:00:00Z"

minutes
~~~~~~~

*n* ``decimal`` *→* ``decimal``

*n* ``integer`` *→* ``decimal``

N minutes, for use with 'add-time'.

.. code:: lisp

    pact> (add-time (time "2016-07-22T12:00:00Z") (minutes 1))
    "2016-07-22T12:01:00Z"

parse-time
~~~~~~~~~~

*format* ``string`` *utcval* ``string`` *→* ``time``

Construct time from UTCVAL using FORMAT. See `strftime
docs <https://www.gnu.org/software/libc/manual/html_node/Formatting-Calendar-Time.html#index-strftime>`__
for format info.

.. code:: lisp

    pact> (parse-time "%F" "2016-09-12")
    "2016-09-12T00:00:00Z"

time
~~~~

*utcval* ``string`` *→* ``time``

Construct time from UTCVAL using ISO8601 format (%Y-%m-%dT%H:%M:%SZ).

.. code:: lisp

    pact> (time "2016-07-22T11:26:35Z")
    "2016-07-22T11:26:35Z"

Operators
---------

!=
~~

*x* ``<a => (integer|string|time|decimal|bool|list|object|keyset)>``
*y* ``<a => (integer|string|time|decimal|bool|list|object|keyset)>``
*→* ``bool``

True if X does not equal Y.

.. code:: lisp

    pact> (!= "hello" "goodbye")
    true

\*
~~

*x* ``<a => (integer|decimal)>`` *y* ``<a => (integer|decimal)>``
*→* ``<a => (integer|decimal)>``

*x* ``<a => (integer|decimal)>`` *y* ``<b => (integer|decimal)>``
*→* ``decimal``

Multiply X by Y.

.. code:: lisp

    pact> (* 0.5 10.0)
    5
    pact> (* 3 5)
    15

\+
~~

*x* ``<a => (integer|decimal)>`` *y* ``<a => (integer|decimal)>``
*→* ``<a => (integer|decimal)>``

*x* ``<a => (integer|decimal)>`` *y* ``<b => (integer|decimal)>``
*→* ``decimal``

*x* ``<a => (string|list|object)>`` *y* ``<a => (string|list|object)>``
*→* ``<a => (string|list|object)>``

Add numbers, concatenate strings/lists, or merge objects.

.. code:: lisp

    pact> (+ 1 2)
    3
    pact> (+ 5.0 0.5)
    5.5
    pact> (+ "every" "body")
    "everybody"
    pact> (+ [1 2] [3 4])
    [1 2 3 4]
    pact> (+ { "foo": 100 } { "foo": 1, "bar": 2 })
    {"bar": 2, "foo": 100})

\-
~~

*x* ``<a => (integer|decimal)>`` *y* ``<a => (integer|decimal)>``
*→* ``<a => (integer|decimal)>``

*x* ``<a => (integer|decimal)>`` *y* ``<b => (integer|decimal)>``
*→* ``decimal``

*x* ``<a => (integer|decimal)>`` *→* ``<a => (integer|decimal)>``

Negate X, or subtract Y from X.

.. code:: lisp

    pact> (- 1.0)
    -1.0
    pact> (- 3 2)
    1

/
~

*x* ``<a => (integer|decimal)>`` *y* ``<a => (integer|decimal)>``
*→* ``<a => (integer|decimal)>``

*x* ``<a => (integer|decimal)>`` *y* ``<b => (integer|decimal)>``
*→* ``decimal``

Divide X by Y.

.. code:: lisp

    pact> (/ 10.0 2.0)
    5
    pact> (/ 8 3)
    2

<
~

*x* ``<a => (integer|decimal|string|time)>``
*y* ``<a => (integer|decimal|string|time)>`` *→* ``bool``

True if X < Y.

.. code:: lisp

    pact> (< 1 3)
    true
    pact> (< 5.24 2.52)
    false
    pact> (< "abc" "def")
    true

<=
~~

*x* ``<a => (integer|decimal|string|time)>``
*y* ``<a => (integer|decimal|string|time)>`` *→* ``bool``

True if X <= Y.

.. code:: lisp

    pact> (<= 1 3)
    true
    pact> (<= 5.24 2.52)
    false
    pact> (<= "abc" "def")
    true

=
~

*x* ``<a => (integer|string|time|decimal|bool|list|object|keyset)>``
*y* ``<a => (integer|string|time|decimal|bool|list|object|keyset)>``
*→* ``bool``

True if X equals Y.

.. code:: lisp

    pact> (= [1 2 3] [1 2 3])
    true
    pact> (= 'foo "foo")
    true
    pact> (= { 1: 2 } { 1: 2})
    true

>
~

*x* ``<a => (integer|decimal|string|time)>``
*y* ``<a => (integer|decimal|string|time)>`` *→* ``bool``

True if X > Y.

.. code:: lisp

    pact> (> 1 3)
    false
    pact> (> 5.24 2.52)
    true
    pact> (> "abc" "def")
    false

>=
~~

*x* ``<a => (integer|decimal|string|time)>``
*y* ``<a => (integer|decimal|string|time)>`` *→* ``bool``

True if X >= Y.

.. code:: lisp

    pact> (>= 1 3)
    false
    pact> (>= 5.24 2.52)
    true
    pact> (>= "abc" "def")
    false

^
~

*x* ``<a => (integer|decimal)>`` *y* ``<a => (integer|decimal)>``
*→* ``<a => (integer|decimal)>``

*x* ``<a => (integer|decimal)>`` *y* ``<b => (integer|decimal)>``
*→* ``decimal``

Raise X to Y power.

.. code:: lisp

    pact> (^ 2 3)
    8

abs
~~~

*x* ``decimal`` *→* ``decimal``

*x* ``integer`` *→* ``integer``

Absolute value of X.

.. code:: lisp

    pact> (abs (- 10 23))
    13

and
~~~

*x* ``bool`` *y* ``bool`` *→* ``bool``

Boolean logic.

.. code:: lisp

    pact> (and true false)
    false

ceiling
~~~~~~~

*x* ``decimal`` *prec* ``integer`` *→* ``decimal``

*x* ``decimal`` *→* ``integer``

Rounds up value of decimal X as integer, or to PREC precision as
decimal.

.. code:: lisp

    pact> (ceiling 3.5)
    4
    pact> (ceiling 100.15234 2)
    100.16

exp
~~~

*x* ``<a => (integer|decimal)>`` *→* ``<a => (integer|decimal)>``

Exp of X

.. code:: lisp

    pact> (round (exp 3) 6)
    20.085537

floor
~~~~~

*x* ``decimal`` *prec* ``integer`` *→* ``decimal``

*x* ``decimal`` *→* ``integer``

Rounds down value of decimal X as integer, or to PREC precision as
decimal.

.. code:: lisp

    pact> (floor 3.5)
    3
    pact> (floor 100.15234 2)
    100.15

ln
~~

*x* ``<a => (integer|decimal)>`` *→* ``<a => (integer|decimal)>``

Natural log of X.

.. code:: lisp

    pact> (round (ln 60) 6)
    4.094345

log
~~~

*x* ``<a => (integer|decimal)>`` *y* ``<a => (integer|decimal)>``
*→* ``<a => (integer|decimal)>``

*x* ``<a => (integer|decimal)>`` *y* ``<b => (integer|decimal)>``
*→* ``decimal``

Log of Y base X.

.. code:: lisp

    pact> (log 2 256)
    8

mod
~~~

*x* ``integer`` *y* ``integer`` *→* ``integer``

X modulo Y.

.. code:: lisp

    pact> (mod 13 8)
    5

not
~~~

*x* ``bool`` *→* ``bool``

Boolean logic.

.. code:: lisp

    pact> (not (> 1 2))
    true

or
~~

*x* ``bool`` *y* ``bool`` *→* ``bool``

Boolean logic.

.. code:: lisp

    pact> (or true false)
    true

round
~~~~~

*x* ``decimal`` *prec* ``integer`` *→* ``decimal``

*x* ``decimal`` *→* ``integer``

Performs Banker's rounding value of decimal X as integer, or to PREC
precision as decimal.

.. code:: lisp

    pact> (round 3.5)
    4
    pact> (round 100.15234 2)
    100.15

sqrt
~~~~

*x* ``<a => (integer|decimal)>`` *→* ``<a => (integer|decimal)>``

Square root of X.

.. code:: lisp

    pact> (sqrt 25)
    5

KeySets
-------

define-keyset
~~~~~~~~~~~~~

*name* ``string`` *keyset* ``string`` *→* ``string``

Define keyset as NAME with KEYSET. If keyset NAME already exists, keyset
will be enforced before updating to new value.

.. code:: lisp

    (define-keyset 'admin-keyset (read-keyset "keyset"))

keys-2
~~~~~~

*count* ``integer`` *matched* ``integer`` *→* ``bool``

Keyset predicate function to match at least 2 keys in keyset.

.. code:: lisp

    pact> (keys-2 3 1)
    false

keys-all
~~~~~~~~

*count* ``integer`` *matched* ``integer`` *→* ``bool``

Keyset predicate function to match all keys in keyset.

.. code:: lisp

    pact> (keys-all 3 3)
    true

keys-any
~~~~~~~~

*count* ``integer`` *matched* ``integer`` *→* ``bool``

Keyset predicate function to match all keys in keyset.

.. code:: lisp

    pact> (keys-any 10 1)
    true

read-keyset
~~~~~~~~~~~

*key* ``string`` *→* ``keyset``

Read KEY from message data body as keyset ({ "keys": KEYLIST, "pred":
PREDFUN }). PREDFUN should resolve to a keys predicate.

.. code:: lisp

    (read-keyset "admin-keyset")

with-keyset
~~~~~~~~~~~

*keyset-or-name* ``string`` *body* ``@rest`` *→* ``<a>``

Enforce KEYSET-OR-NAME against message keys to run BODY. KEYSET-OR-NAME
can be a symbol of a keyset name or a keyset object.

.. code:: lisp

    (with-keyset 'admin-keyset ...)
    (with-keyset (read-keyset "keyset") ...)

REPL-only functions
-------------------

The following functions are loaded magically in the interactive REPL, or
in script files with a ``.repl`` extension. They are not available for
blockchain-based execution.

begin-tx
~~~~~~~~

*→* ``string``

*name* ``string`` *→* ``string``

Begin transaction with optional NAME.

.. code:: lisp

    (begin-tx "load module")

bench
~~~~~

*exprs* ``@rest`` *→* ``string``

Benchmark execution of EXPRS.

.. code:: lisp

    (bench (+ 1 2))

commit-tx
~~~~~~~~~

*→* ``string``

Commit transaction.

.. code:: lisp

    (commit-tx)

env-data
~~~~~~~~

*json* ``<a => (integer|string|time|decimal|bool|list|object|keyset|value)>``
*→* ``string``

Set transaction JSON data, either as encoded string, or as pact types
coerced to JSON.

.. code:: lisp

    pact> (env-data { "keyset": { "keys": ["my-key" "admin-key"], "pred": "keys-any" } })
    "Setting transaction data"

env-entity
~~~~~~~~~~

*entity* ``string`` *→* ``string``

Set environment confidential ENTITY id.

.. code:: lisp

    (env-entity "my-org")

env-keys
~~~~~~~~

*keys* ``[string]`` *→* ``string``

Set transaction signature KEYS.

.. code:: lisp

    pact> (env-keys ["my-key" "admin-key"])
    "Setting transaction keys"

env-step
~~~~~~~~

*→* ``string``

*step-idx* ``integer`` *→* ``string``

*step-idx* ``integer`` *rollback* ``bool`` *→* ``string``

Modify pact step state. With no arguments, unset step. STEP-IDX sets
step index for current pact execution, ROLLBACK defaults to false.

.. code:: lisp

    (env-step 1)
    (env-step 0 true)

expect
~~~~~~

*doc* ``string`` *expected* ``<a>`` *actual* ``<a>`` *→* ``string``

Evaluate ACTUAL and verify that it equals EXPECTED.

.. code:: lisp

    pact> (expect "Sanity prevails." 4 (+ 2 2))
    "Expect: success: Sanity prevails."

expect-failure
~~~~~~~~~~~~~~

*doc* ``string`` *exp* ``<a>`` *→* ``string``

Evaluate EXP and succeed only if it throws an error.

.. code:: lisp

    pact> (expect-failure "Enforce fails on false" (enforce false "Expected error"))
    "Expect failure: success: Enforce fails on false"

load
~~~~

*file* ``string`` *→* ``string``

*file* ``string`` *reset* ``bool`` *→* ``string``

Load and evaluate FILE, resetting repl state beforehand if optional
NO-RESET is true.

.. code:: lisp

    (load "accounts.repl")

rollback-tx
~~~~~~~~~~~

*→* ``string``

Rollback transaction.

.. code:: lisp

    (rollback-tx)
