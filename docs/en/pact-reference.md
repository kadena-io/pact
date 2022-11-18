![](img/kadena-logo-210px.png)

Pact Smart Contract Language Reference
===

This document is a reference for the Pact smart-contract language, designed for
correct, transactional execution on a [high-performance blockchain](http://kadena.io). For more
background, please see the [white paper](https://docs.kadena.io/basics/whitepapers/pact-smart-contract-language)
or the [pact home page](http://kadena.io/#pactModal).

Copyright (c) 2016 - 2018, Stuart Popejoy. All Rights Reserved.


Rest API
=======
See (https://api.chainweb.com/openapi/pact.html) for latest OpenAPI docs.

### Pact built-in server
Pact ships with a built-in HTTP server and SQLite backend.
To start up the server issue `pact -s config.yaml`, with a suitable config.

### `pact-lang-api` JS Library
The `pact-lang-api` JS library is [available via npm](https://www.npmjs.com/package/pact-lang-api) for web development.


### API request formatter

The `pact` tool accepts the `-a` option to format API request JSON,
using a YAML file describing the request. The output can then be used with a POST tool like
Postman or even piping into `curl`.

For instance, a yaml file called "apireq.yaml" with the following contents:

```yaml
code: "(+ 1 2)"
data:
  name: Stuart
  language: Pact
keyPairs:
  - public: ba54b224d1924dd98403f5c751abdd10de6cd81b0121800bf7bdbdcfaec7388d
    secret: 8693e641ae2bbe9ea802c736f42027b03f86afe63cae315e7169c9c496c17332
```

can be fed into `pact` to obtain a valid API request:

```shell
$ pact -a tests/apireq.yaml -l
{"hash":"444669038ea7811b90934f3d65574ef35c82d5c79cedd26d0931fddf837cccd2c9cf19392bf62c485f33535983f5e04c3e1a06b6b49e045c5160a637db8d7331","sigs":[{"sig":"9097304baed4c419002c6b9690972e1303ac86d14dc59919bf36c785d008f4ad7efa3352ac2b8a47d0b688fe2909dbf392dd162457c4837bc4dc92f2f61fd20d","scheme":"ED25519","pubKey":"ba54b224d1924dd98403f5c751abdd10de6cd81b0121800bf7bdbdcfaec7388d","addr":"ba54b224d1924dd98403f5c751abdd10de6cd81b0121800bf7bdbdcfaec7388d"}],"cmd":"{\"address\":null,\"payload\":{\"exec\":{\"data\":{\"name\":\"Stuart\",\"language\":\"Pact\"},\"code\":\"(+ 1 2)\"}},\"nonce\":\"\\\"2017-09-27 19:42:06.696533 UTC\\\"\"}"}

```

Here's an example of piping into curl, hitting a pact server running on port 8080:

```shell
$ pact -a tests/apireq.yaml -l | curl -d @- http://localhost:8080/api/v1/local
{"status":"success","response":{"status":"success","data":3}}
```


### Request YAML file format {#request-yaml}
Request yaml files takes two forms. An *execution* Request yaml file describes the [exec](https://api.chainweb.com/openapi/pact.html#tag/model-payload) payload.
Meanwhile, a *continuation* Request yaml file describes the [cont](https://api.chainweb.com/openapi/pact.html#tag/model-payload) payload.

#### YAML exec command request
The execution request yaml for a public blockchain takes the following keys:

```yaml
  code: Transaction code
  codeFile: Transaction code file
  data: JSON transaction data
  dataFile: JSON transaction data file
  keyPairs: list of key pairs for signing (use pact -g to generate): [
    public: base 16 public key
    secret: base 16 secret key
    caps: [
      optional managed capabilities
      ]
    ]
  nonce: optional request nonce, will use current time if not provided
  networkId: string identifier for a blockchain network
  publicMeta:
    chainId: string chain id of the chain of execution
    sender: string denoting the sender of the transaction
    gasLimit: integer gas limit
    gasPrice: decimal gas price
    ttl: integer time-to-live value
    creationTime: optional integer tx execution time after offset
  type: exec
```

#### YAML Continuation command request

The continuation request yaml for a public blockchain takes the following keys:

```yaml
  pactTxHash: integer transaction id of pact
  step: integer next step of a pact
  rollback: boolean for rollingback a pact
  proof: string spv proof of continuation (optional, cross-chain only)
  data: JSON transaction data
  dataFile: JSON transaction data file
  keyPairs: list of key pairs for signing (use pact -g to generate): [
    public: string base 16 public key
    secret: string base 16 secret key
    caps: [
      optional managed capabilities
      ]
    ]
  networkId: string identifier for a blockchain network
  publicMeta:
    chainId: string chain id of the chain of execution
    sender: string denoting the sender of the transaction
    gasLimit: integer gas limit
    gasPrice: decimal gas price
    ttl: integer time-to-live value
    creationTime: optional integer tx execution time after offset
  nonce: optional request nonce, will use current time if not provided
  type: cont
```

Note that the optional "proof" field only makes sense when using cross-chain continuations.

Signing Transactions
---

As of Pact 3.5.0, the `pact` command line tool now has several commands to
facilitate signing transactions. Here's a full script showing how these commands
can be used to prepare an unsigned version of the transaction and add signatures
to it. This transcript assumes that the details of the transaction has been
specified in a file called `tx.yaml`.

```
# At some earlier time generate and save some public/private key pairs.
pact -g > alice-key.yaml
pact -g > bob-key.yaml

# Convert a transaction into an unsigned prepared form that is signatures can be added to
pact -u tx.yaml > tx-unsigned.yaml

# Sign the prepared transaction with one or more keys
cat tx-unsigned.yaml | pact add-sig alice-key.yaml > tx-signed-alice.yaml
cat tx-unsigned.yaml | pact add-sig bob-key.yaml > tx-signed-bob.yaml

# Combine the signatures into a fully signed transaction ready to send to the blockchain
pact combine-sigs tx-signed-alice.yaml tx-signed-bob.yaml > tx-final.json

```

The `add-sig` command takes the output of `pact -u` on standard input and one or
more key files as command line arguments. It adds the appropriate signatures to
to the transaction and prints the result to stdout.

The `combine-sigs` command takes multiple unsigned (from `pact -u`) and signed
(from `pact add-sig`) transaction files as command line arguments and outputs
the command and all the signatures on stdout.

Both `add-sig` and `combine-sigs` will output YAML if the output transaction
hasn't accumulated enough signatures to be valid. If all the necessary
signatures are present, then they will output JSON in final form that is ready
to be sent to the blockchain on the [`/send` endpoint](#send). If you would like
to do a test run of the transaction, you can use the `-l` flag to generate
output suitable for use with the [`/local` endpoint](#local).

The above example adds signatures in parallel, but the `add-sig` command can
also be used to add signatures sequentially in separate steps or all at once in
a single step as shown in the following two examples:

```
cat tx-unsigned.yaml | pact add-sig alice-key.yaml | pact add-sig bob-key.yaml
cat tx-unsigned.yaml | pact add-sig alice-key.yaml add-sig bob-key.yaml
```

### Offline Signing with a Cold Wallet

Some cold wallet signing procedures use QR codes to get transaction data on and
off the cold wallet machine. Since QR codes can transmit a fairly limited amount
of information these signing commands are also designed to work with a more
compact data format that doesn't require the full command to generate
signatures. Here's an example of what `tx-unsigned.yaml` might look like in the
above example:

```
hash: KY6RFunty4WazQiCsKsYD-ovu-_XQByfY6scTxi9gQQ
sigs:
  368820f80c324bbc7c2b0610688a7da43e39f91d118732671cd9c7500ff43cca: null
  6be2f485a7af75fedb4b7f153a903f7e6000ca4aa501179c91a2450b777bd2a7: null
cmd: '{"networkId":"mainnet01","payload":{"exec":{"data":{"ks":{"pred":"keys-all","keys":["368820f80c324bbc7c2b0610688a7da43e39f91d118732671cd9c7500ff43cca"]}},"code":"(coin.transfer-create \"alice\" \"bob\" (read-keyset \"ks\") 100.1)\n(coin.transfer \"bob\" \"alice\" 0.1)"}},"signers":[{"pubKey":"6be2f485a7af75fedb4b7f153a903f7e6000ca4aa501179c91a2450b777bd2a7","clist":[{"args":["alice","bob",100.1],"name":"coin.TRANSFER"},{"args":[],"name":"coin.GAS"}]},{"pubKey":"368820f80c324bbc7c2b0610688a7da43e39f91d118732671cd9c7500ff43cca","clist":[{"args":["bob","alice",0.1],"name":"coin.TRANSFER"}]}],"meta":{"creationTime":1580316382,"ttl":7200,"gasLimit":1200,"chainId":"0","gasPrice":1.0e-5,"sender":"alice"},"nonce":"2020-01-29 16:46:22.916695 UTC"}'
```

To get a condensed version for signing on a cold wallet all you have to do is
drop the `cmd` field. This can be done manually or scripted with `cat
tx-unsigned.yaml | grep -v "^cmd:"`. The result would look like this:

```
hash: KY6RFunty4WazQiCsKsYD-ovu-_XQByfY6scTxi9gQQ
sigs:
  368820f80c324bbc7c2b0610688a7da43e39f91d118732671cd9c7500ff43cca: null
  6be2f485a7af75fedb4b7f153a903f7e6000ca4aa501179c91a2450b777bd2a7: null
```

Keep in mind that when you sign these condensed versions, you won't be able to
submit the output directly to the blockchain. You'll have to use `combine-sigs`
to combine those signatures with the original `tx-unsigned.yaml` file which has
the full command.

### Detached Signature Transaction Format

The YAML input expected by `pact -u` is similar to the [Public Blockchain YAML
format](#request-yaml-public-chain) described above with one major difference.
Instead of the `keyPairs` field which requires both the public and secret keys,
`pact -u` expects a `signers` field that only needs a public key. This allows
signatures to be added on incrementally as described above without needing
private keys to all be present when the transaction is constructed.

Here is an example of how the above `tx.yaml` file might look:

```
code: |-
  (coin.transfer-create "alice" "bob" (read-keyset "ks") 100.1)
  (coin.transfer "bob" "alice" 0.1)
data:
  ks:
    keys: [368820f80c324bbc7c2b0610688a7da43e39f91d118732671cd9c7500ff43cca]
    pred: "keys-all"
publicMeta:
  chainId: "0"
  sender: alice
  gasLimit: 1200
  gasPrice: 0.0000000001
  ttl: 7200
networkId: "mainnet01"
signers:
  - public: 6be2f485a7af75fedb4b7f153a903f7e6000ca4aa501179c91a2450b777bd2a7
    caps:
      - name: "coin.TRANSFER"
        args: ["alice", "bob", 100.1]
      - name: "coin.GAS"
        args: []
  - public: 368820f80c324bbc7c2b0610688a7da43e39f91d118732671cd9c7500ff43cca
    caps:
      - name: "coin.TRANSFER"
        args: ["bob", "alice", 0.1]
type: exec
```


Concepts {#concepts}
========

Execution Modes {#execmodes}
---

Pact is designed to be used in distinct *execution modes* to address
the performance requirements of rapid linear execution on a
blockchain. These are:

1. Contract definition.
2. Transaction execution.
3. Queries and local execution.

### Contract Definition {#definitionmode}

In this mode, a large amount of code is sent into the blockchain to
establish the smart contract, as comprised of modules (code), tables
(data), and keysets (authorization). This can also include "transactional"
(database-modifying) code, for instance to initialize data.

For a given smart contract, these should all be sent as a single message
into the blockchain, so that any error will rollback the entire smart contract
as a unit.

#### Keyset definition {#keysetdefinition}

[Keysets](#confidential-keysets) are customarily defined first, as they are used to specify
admin authorization schemes for modules and tables. Definition creates the keysets
in the runtime environment and stores their definition in the global keyset database.

#### Namespace declaration {#namespacedefinition}

[Namespace](#namespaces) declarations provide a unique prefix for modules and interfaces defined within the namespace scope. Namespaces are handled differently in public and private blockchain contexts: in private they are freely definable, and the _root namespace_ (ie, not using a namespace at all) is available for user code. In public blockchains, users are not allowed to use the root namespace (which is reserved for built-in contracts like the coin contract) and must define code within a namespace, which may or may not be definable (ie, users might be restricted to "user" namespaces).

Namespaces are defined using [define-namespace](#define-namespace). Namespaces are "entered" by issuing the [namespace](#namespace) command.

#### Module declaration {#moduledeclaration}

[Modules](#module) contain the API and data definitions for smart contracts. They are comprised of:

- [functions](#defun)
- [schema](#defschema) definitions
- [table](#deftable) definitions
- [pact](#defpact) special functions
- [constant](#defconst) values
- [models](pact-properties.html)
- [capabilities](#caps)
- [imports](#use)
- [implements](#implements)

When a module is declared, all references to native functions, interfaces, or definitions from other modules are resolved. Resolution failure results in transaction rollback.

Modules can be re-defined as controlled by their governance capabilities. Often, such a function is simply a reference to an administrative keyset. Module versioning is not supported, except by including a version sigil in the module name (e.g., "accounts-v1"). However, *module hashes* are a powerful feature for ensuring code safety. When a module is imported with [use](#use), the module hash can be specified, to tie code to a particular release.

As of Pact 2.2, `use` statements can be issued within a module declaration. This combined with module hashes provides a high level of assurance, as updated module code will fail to import if a dependent module has subsequently changed on the chain; this will also propagate changes to the loaded modules' hash, protecting downstream modules from inadvertent changes on update.

Module names must be unique within a namespace.

#### Interface Declaration {#interfacedeclaration}

[Interfaces](#interfaces) contain an API specification and data definitions for smart contracts.
They are comprised of:

- [function](#defun) specifications (i.e. function signatures)
- [constant](#defconst) values
- [schema](#defschema) definitions
- [pact](#defpact) specifications
- [models](pact-properties.html)
- [capabilities](#caps) specifications
- [imports](#use)

Interfaces represent an abstract api that a [module](#module) may implement by issuing an `implements` statement within the module declaration. Interfaces may import definitions from other modules by issuing a [use](#use) declaration, which may be used to construct new constant definitions, or make use of types defined in the imported module. Unlike Modules, Interface versioning is not supported. However, modules may implement multiple interfaces.

Interface names must be unique within a namespace.

#### Table Creation {#tablecreation}

Tables are [created](pact-functions.html#create-table) at the same time as modules. While tables are *defined* in
modules, they are *created* "after" modules, so that the module may be redefined later without
having to necessarily re-create the table.

The relationship of modules to tables is important, as described in [Table Guards](#module-table-guards).

There is no restriction on how many tables may be created. Table names are namespaced with
the module name.

Tables can be typed with a [schema](#defschema).

### Transaction Execution {#transaction-execution}

"Transactions" refer to business events enacted on the blockchain, like a payment, a sale, or
a workflow step of a complex contractual agreement. A transaction is generally a single call to
a module function. However there is no limit on how many statements can be executed. Indeed,
the difference between "transactions" and "smart contract definition" is simply the *kind* of code
executed, not any actual difference in the code evaluation.

### Queries and Local Execution {#queries}

Querying data is generally not a business event, and can involve data payloads that could impact
performance, so querying is carried out as a *local execution* on the node receiving the message.
Historical queries use a *transaction ID* as a point of reference, to avoid any race
conditions and allow asynchronous query execution.

Transactional vs local execution is accomplished by targeting different API endpoints; pact code
has no ability to distinguish between transactional and local execution.

Database Interaction {#dbinteraction}
---

Pact presents a database metaphor reflecting the unique requirements of blockchain execution,
which can be adapted to run on different back-ends.

### Atomic execution {#atomicexecution}

A single message sent into the blockchain to be evaluated by Pact is *atomic*: the transaction
succeeds as a unit, or does not succeed at all, known as "transactions" in database literature.
There is no explicit support for rollback handling, except in [multi-step transactions](#pacts).

### Key-Row Model {#dbkeyrow}

Blockchain execution can be likened to OLTP (online transaction processing) database workloads,
which favor denormalized data written to a single table. Pact's data-access API reflects this
by presenting a *key-row* model, where a row of column values is accessed by a single key.

As a result, Pact does not support *joining* tables, which is more suited for an OLAP (online
analytical processing) database, populated from exports from the Pact database. This does not
mean Pact cannot *record* transactions using relational techniques -- for example, a Customer table whose keys
are used in a Sales table would involve the code looking up the Customer record before writing
to the Sales table.

### Queries and Performance {#queryperformance}

As of Pact 2.3, Pact offers a powerful query mechanism for selecting multiple rows from a table.
While visually similar to SQL, the [select](pact-functions.html#select) and [where](pact-functions.html#where) operations offer a
_streaming interface_ to a table, where the user provides filter functions, and then operates
on the rowset as a list data structure using [sort](pact-functions.html#sort) and other functions.

```lisp

;; the following selects Programmers with salaries >= 90000 and sorts by age descending

(reverse (sort ['age]
  (select 'employees ['first-name,'last-name,'age]
    (and? (where 'title (= "Programmer"))
          (where 'salary (< 90000))))))

;; the same query could be performed on a list with 'filter':

(reverse (sort ['age]
  (filter (and? (where 'title (= "Programmer"))
                (where 'salary (< 90000)))
          employees)))

```

In a transactional setting, Pact database interactions are optimized for single-row reads and writes,
meaning such queries can be slow and prohibitively expensive computationally. However, using the
[local](#local) execution capability, Pact can utilize the user filter functions on the streaming
results, offering excellent performance.

The best practice is therefore to use select operations via local, non-transactional operations,
and avoid using select on large tables in the transactional setting.

### No Nulls {#nonulls}

Pact has no concept of a NULL value in its database metaphor. The main function for computing
on database results, [with-read](pact-functions.html#with-read), will error if any column value is not found.
Authors must ensure that values are present for any transactional read. This is a safety feature
to ensure *totality* and avoid needless, unsafe control-flow surrounding null values.

### Versioned History {#dbversions}

The key-row model is augmented by every change to column values being versioned by transaction ID.
For example, a table with three columns "name", "age", and "role" might update "name" in transaction #1,
and "age" and "role" in transaction #2. Retrieving historical data will return just the change to "name"
under transaction 1, and the change to "age" and "role" in transaction #2.

### Back-ends {#backends}

Pact guarantees identical, correct execution at the smart-contract layer within the blockchain. As
a result, the backing store need not be identical on different consensus nodes.
Pact's implementation allows for integration of industrial RDBMSs, to assist large migrations onto
a blockchain-based system, by facilitating bulk replication of data to downstream systems.

Types and Schemas
---

With Pact 2.0, Pact gains explicit type specification, albeit optional. Pact 1.0 code without
types still functions as before, and writing code without types is attractive for rapid prototyping.

Schemas provide the main impetus for types. A schema [is defined](#defschema) with a list of
columns that can have types (although this is also not required). Tables are then [defined](#deftable)
with a particular schema (again, optional).

Note that schemas also can be used on/specified for object types.

### Runtime Type enforcement

Any types declared in code are enforced at runtime. For table schemas, this means any write
to a table will be typechecked against the schema. Otherwise, if a type specification is
encountered, the runtime enforces the type when the expression is evaluated.

### Static Type Inference on Modules

With the [typecheck](pact-functions.html#typecheck) repl command, the Pact interpreter will analyze a module
and attempt to infer types on every variable, function application or const definition.
Using this in project repl scripts is helpful to aid the developer in adding "just enough types"
to make the typecheck succeed. Successful typechecking is usually a matter of providing
schemas for all tables, and argument types for ancillary functions that call ambiguous or
overloaded native functions.

### Formal Verification

Pact's typechecker is designed to output a fully typechecked and inlined AST for generating
formal proofs in the SMT-LIB2 language. If the typecheck does not succeed, the module is not
considered "provable".

We see, then, that Pact code can move its way up a "safety" gradient, starting with no types,
then with "enough" types, and lastly, with formal proofs.

Note that as of Pact 2.0 the formal verification function is still under development.


Keysets and Authorization {#confidential-keysets}
---

Pact is inspired by Bitcoin scripts to incorporate public-key authorization directly into smart
contract execution and administration. Pact seeks to take this further by making single- and multi-sig
interactions ubiquitous and effortless with the concept of _keysets_, meaning that single-signature
mode is never assumed: anywhere public-key signatures are used, single-sig and multi-sig can interoperate
effortlessly. Finally, all crypto is handled by the Pact runtime to ensure programmers can't make
mistakes "writing their own crypto".

Also see [Guards and Capabilities](#caps) below for how Pact
moves beyond just keyset-based authorization.

### Keyset definition

Keysets are [defined](pact-functions.html#define-keyset) by [reading](pact-functions.html#read-keyset) definitions from the message
payload. Keysets consist of a list of public keys and a *keyset predicate*.

Examples of valid keyset JSON productions:

```javascript
/* examples of valid keysets */
{
  "fully-specified-with-native-pred":
    { "keys": ["abc6bab9b88e08d","fe04ddd404feac2"], "pred": "keys-2" },

  "fully-specified-with-qual-custom":
    { "keys": ["abc6bab9b88e08d","fe04ddd404feac2"], "pred": "my-module.custom-pred" },

  "keysonly":
    { "keys": ["abc6bab9b88e08d","fe04ddd404feac2"] }, /* defaults to "keys-all" pred */

  "keylist": ["abc6bab9b88e08d","fe04ddd404feac2"] /* makes a "keys-all" pred keyset */
}

```

### Keyset Predicates {#keyset-predicates}

A keyset predicate references a function by its (optionally qualified) name, and will compare the public keys in the keyset
to the key or keys used to sign the blockchain message. The function accepts two arguments,
"count" and "matched", where "count" is the number of keys in the keyset and "matched" is how many
keys on the message signature matched a keyset key.

Support for multiple signatures is the responsibility of the blockchain layer, and is a powerful
feature for Bitcoin-style "multisig" contracts (i.e. requiring at least two signatures to release funds).

Pact comes with built-in keyset predicates: [keys-all](pact-functions.html#keys-all), [keys-any](pact-functions.html#keys-any), [keys-2](pact-functions.html#keys-2).
Module authors are free to define additional predicates.

If a keyset predicate is not specified, [keys-all](pact-functions.html#keys-all) is used by default.

### Key rotation {#keyrotation}

Keysets can be rotated, but only by messages authorized against the current keyset definition
and predicate. Once authorized, the keyset can be easily [redefined](#define-keyset).

### Module Table Guards {#tableguards}

When [creating](pact-functions.html#create-table) a table, a module name must also be specified. By this mechanism,
tables are "guarded" or "encapsulated" by the module, such that direct access to the table
via [data-access functions](pact-functions.html#database) is authorized only by the module's governance.
However, *within module functions*, table access is unconstrained. This gives contract authors great
flexibility in designing data access, and is intended to enshrine the module as the main
"user data access API".

See also [module guards](#module-guards) for how this concept can be leveraged to protect more than
just tables.

Note that as of Pact 3.5, the option has been added to selectively allow unguarded reads and transaction history
access in local mode only, at the discretion of the node operator.

### Row-level keysets {#rowlevelkeysets}

Keysets can be stored as a column value in a row, allowing for *row-level* authorization.
The following code indicates how this might be achieved:

```lisp
(defun create-account (id)
  (insert accounts id { "balance": 0.0, "keyset": (read-keyset "owner-keyset") }))

(defun read-balance (id)
  (with-read accounts id { "balance":= bal, "keyset":= ks }
    (enforce-keyset ks)
    (format "Your balance is {}" [bal])))
```

In the example, `create-account` reads a keyset definition from the message payload using [read-keyset](pact-functions.html#read-keyset)
to store as "keyset" in the table. `read-balance` only allows that owner's keyset to read the balance,
by first enforcing the keyset using [enforce-keyset](pact-functions.html#enforce-keyset).

Namespaces {#namespaces}
---

Namespaces are [defined](pact-functions.html#define-namespace) by specifying a namespace name and [associating](pact-functions.html#read-keyset)
a keyset with the namespace. Namespace scope is entered by declaring the namespace environment. All definitions issued after the namespace scope is entered will be accessible by their fully qualified names. These names are of the form _namespace.module.definition_. This form can also be used to access code outside of the current namespace for the purpose of importing module code, or implementing modules:

```lisp
(implements my-namespace.my-interface)
;; or
(use my-namespace.my-module)
```

Code may be appended to the namespace by simply re-entering the namespace and declaring new code definitions. All definitions _must_ occur within a namespace, as the global namespace (the empty namespace) is reserved for Kadena code.

Examples of valid namespace definition and scoping:

#### Example: Defining a namespace

Defining a namespace requires a keyset, and a namespace name of type string:

```lisp
(define-keyset 'my-keyset)
(define-namespace 'my-namespace (read-keyset 'my-keyset))

pact> (namespace 'my-namespace)
"Namespace set to my-namespace"
```

#### Example: Accessing members of a namespace

Members of a namespace may be accessed by their fully-qualified names:

```lisp
pact> (my-namespace.my-module.hello-number 3)
"Hello, your number is 3!"

;; alternatively
pact> (use my-namespace.my-module)
"Using my-namespace.my-module"
pact> (hello-number 3)
"Hello, your number is 3!"

```

#### Example: Importing module code or implementing interfaces at a namespace

Modules may be imported at a namespace, and interfaces my be implemented in a similar way. This allows the user to work with members of a namespace in a much less verbose and cumbersome way.


```lisp
; in my-namespace
(module my-module EXAMPLE_GUARD
  (implements my-other-namespace.my-interface)

  (defcap EXAMPLE_GUARD ()
    (enforce-keyset 'my-keyset))

  (defun hello-number:string (number:integer)
    (format "Hello, your number is {}!" [number]))
)

```

#### Example: appending code to a namespace

If one is simply appending code to an existing namespace, then the namespace prefix in the fully qualified name may be ommitted, as using a namespace works in a similar way to importing a module: all toplevel definitions within a namespace are brought into scope when `(namespace 'my-namespace)` is declared. Continuing from the previous example:

```lisp
pact> (my-other-namespace.my-other-module.more-hello 3)
"Hello, your number is 3! And more hello!"

; alternatively
pact> (namespace 'my-other-namespace)
"Namespace set to my-other-namespace"

pact> (use my-other-module)
"Using my-other-module"

pact> (more-hello 3)
"Hello, your number is 3! And more hello!"

```

Guards, Capabilities and Events {#caps}
---

Pact 3.0 introduces powerful new concepts to allow programmers to express and implement authorization schemes correctly and easily:
_guards_, which generalize keysets, and _capabilities_, which generalize authorizations or rights. In Pact 3.7, capabilities also function as [events](#pact-events).

### Guards
A guard is essentially a predicate function over some environment that enables a pass-fail operation, `enforce-guard`,
to be able to test a rich diversity of conditions.

A keyset is the quintessential guard: it specifies a list of keys,
and a predicate function to verify how many keys were used to sign the current transaction. Enforcement
happens via `enforce-keyset`, causing the transaction to fail if the necessary keys are not found in the signing set.

However, there are other predicates that are equally useful:

- We might want to enforce that a _module_ is the only entity that can perform some function, for instance
to debit some account.

- We might want to ensure that a user has provided some secret, like a hash preimage, as seen in atomic swaps.

- We might want to combine all of the above into a single, enforceable rule: "ensure user A signed the transaction AND
provided a hash preimage AND is only executable by module `foo`".

Finally, we want guards to _interoperate_ with each other, so that smart contract code doesn't have to worry about
what kind of guard is used to mediate access to some resource or right. For instance, it is easy to think of entries
in a ledger having diverse guards, where some tokens are guarded by keysets, while others are autonomously owned by
modules, while others are locked in some kind of escrow transaction: what's important is that the guard always be enforced
for the given account, not what type of guard it is.

Guards address all of these needs. Keysets are now just one type of guard, to which we add module guards,
pact guards, and completely customizable "user guards". You can store any type of guard in the database using the `guard`
type. The `keyset` type is still supported, but developers should switch to `guard` to enjoy the enhanced flexibility.

### Capabilities

Capabilities are a new construct in Pact 3.0 that draws from capability theory to offer a system for managing runtime
user rights in an explicit, literate, and principled fashion.

Simply put, a _capability_ is a "ticket" that when _acquired_ allows the user to perform some sensitive task. If the user
is unable to acquire the ticket, portions of the transaction that demand the ticket will fail.

#### Using capabilities to protect code
Code can demand that a capability be "already granted", that is, make no attempt to acquire the ticket,
but fail if it was not acquired somewhere else. This is done with the construct `require-capability`.

Code can also directly attempt to acquire a capability, but only for a specific _scope_. This is done with the
special form `with-capability`, which, like `with-read`, scopes a body of code. Here, the ticket is granted while
this body of code is executing, and is revoked when the body leaves execution.

#### Expressing capabilities in code: `defcap`
We've described capabilities like a "ticket", so let's continue by adding some attributes to this ticket:

- It needs a general name, like "ALLOW_ENTRY", to identify the operation being protected.
- It needs _parameters_, so that a capability can be granted to a specific entity ("user-id"),
  and/or for a particular amount ("amount" some decimal, "active" flag).
- It needs a _predicate function_ to perform whatever tests govern whether to grant the ticket.

Pact provides the `defcap` construct to do this.

```
(defcap ALLOW_ENTRY (user-id:string)
  "Govern entry operation."
  (with-read table user-id
    { "guard" := guard, "active" := active }
    (enforce-guard guard)
    (enforce active "Only active users allowed entry")))
```

`ALLOW_ENTRY` is the name or _domain_ of the capability. `user-id` is a _parameter_. Together, they
form the _specification_ of a capability. Thus, `(ALLOW_ENTRY 'dave)` and `(ALLOW_ENTRY 'carol)`
describe separate capailities. (Note that capability theory's notion of _designation_ is indicated here,
which we'll return to when we discuss capabilities and signatures).

The body implements the predicate function. It accesses whatever data it needs to perform necessary tests to
protect against improper granting of the ticket. The body can do more than that -- it can import or _compose_
additional capabilities, for instance -- and it can even modify database state. This might be used to
ensure a capability cannot be granted ever again after the first time it is acquired, for example.

To acquire this capability, you would invoke `with-capability`:

```
(defun enter (user-name)
  (with-capability (ALLOW_ENTRY user-name)
    (do-entry user-name)            ;; call "protected" function
    (update-entry-status user-name) ;; update database
  )
  (record-audit "ENTRY" user-name)  ;; some "unsafe" operation
)
```

To demand or _require_ the capability, you would use `require-capability`:

```
(defun do-entry (user-name)
  (require-capability (ALLOW_ENTRY user-name))
  ...
)
```

Requiring capabilities allow for "private" or "restricted" functions than cannot be called directly.
Here we see that `do-entry` can only be called "privately", by code inside the module somewhere.
What's more, it can only be called in an outer operation for this user in particular, "restricting"
it to that user.

#### Composing capabilities

A `defcap` can "import" other capabilities, for modular factoring of guard code, or to "compose"
the outer capability from "smaller", "inner" capabilities.

```
(defcap ALLOW_ENTRY (user-id:string)
  "Govern entry operation."
  (with-read table user-id
    { "guard" := guard, "active" := active }
    (enforce-guard guard)
    (enforce active "Only active users allowed entry")
    (compose-capability DB_LOG) ;; allow db logging while ALLOW_ENTRY is in scope
    ))
```

Composed capabilities are only in scope when their "parent" capability is granted.


### Signature capabilities

In Pact transaction messages, each signer can "scope" their signature to one or more capabilities. This
restricts keyset guard operations on that signature: keysets demanding the scoped signature will only succeed
while the ticket is held, or is in the process of being acquired -- keysets are often checked in
order to grant a capability.

This "scoping" allows the signer to safely call untrusted code. For instance, in the Chainweb gas system,
the "sender" signs the message to fund whatever gas costs are charged for the transaction. By signing
the message, the sender has potentially allowed any code to debit from their account!

With that sender's signature has `(GAS)` added to it, it is scoped within gas payments in the coin contract only.
Third-party code is prohibited from accessing that account during the transaction.

### Signatures and Managed Capabilities

Signature capabilities are also a mechanism to _install_ capabilities, but only if that capability is
_managed_. "Vanilla" capabilities are just tickets to show before you try some protected operation,
but _managed_ capabilities are able to _change the state_ of a capability as it is brought into and
out of scope. The ticket metaphor breaks down here, as this is now a dynamic object that mediates
whether capabilities are acquired.

If a signer attaches a managed capability to their signature list, the capability is "installed",
which is not the same as "granted" or "acquired": if the capability's predicate function allows this signer to
install the capability, the installed version will then govern any code needing the capability to unlock some
protected operation, by means of a _manager function_.

#### Capability management with a manager function
A managed capability allows for safe interoperation with otherwise untrusted code. By signing with a
managed capability, you are _allowing_ some untrusted code to _request_ grant of the capability; if
the capability was not in the signature list, the untrusted code cannot request it.

If the capability _manager function_ doesn't grant the request, the untrusted code fails to execute.
The common usage of this is to grant a payment to third-party code, such that the third-party code
can directly transfer on behalf of the user some amount of coin, but only up to the indicated amount.


#### The TRANSFER managed capability

```
(defcap TRANSFER (sender:string receiver:string amount:decimal)
  @managed amount TRANSFER_mgr
  (compose-capability (DEBIT sender))
  (compose-capability (CREDIT receiver)))

(defun TRANSFER_mgr:decimal (managed:decimal requested:decimal)
  (enforce (>= managed requested) "Transfer quantity exhausted")
  (- managed requested) ;; update managed quantity for next time
)
```

`TRANSFER` allows for `sender` to approve any number of payments to `receiver` up to some `amount`.
Once the amount is exceeded, the capability can no longer be brought into scope.

This allows third-party code to directly enact payments. Managed capabilities are an important feature to allow
smart contracts to directly call some other trusted code in a tightly-constrained context.


#### Automatic "one-shot" capability management
A managed capability that does not specify a manager function is "auto-managed", meaning that
after install, the capability can be granted exactly once for the given parameters. Further attempts
will fail after the initial grant goes out of scope.

In the following example, the capability will have "one-shot" automatic management:

```
(defcap VOTE (member:string)
  @managed
  (validate-member member))
```



### Guards vs Capabilities
Guards and capabilities can be confusing: given we have guards like keysets, what do we need the capability concept
for?

Guards allow us to define a _rule_ that must be satisfied for the transaction to proceed. As such, they really are just
a way to declare a pass-fail condition or predicate. The Pact guard system is flexible enough to express any rule you can code.

Capabilities allow us to declare how that rule is deployed to grant some authority. In doing so, they enumerate the critical rights that are
extended to users of the smart contract, and "protect" code from being called incorrectly.

Note also that **capabilities can only be granted inside the module
code that declares them**, whereas guards are simply data that can be tested anywhere.
This is an important security property, as it ensures an attacker cannot elevate their privileges
from outside the module code.

### Modeling capabilities with `compose-capability`

The only problem with the above code is it pushed the awareness of DEBIT into the `transfer` function, whereas
separation of concerns would better have it housed in `debit`. What's more, we'd like to ensure that `debit` is always called
in a "transfer" capacity, that is, that the corresponding `credit` occurs. Thus, the better way to model this is with
two capabilities, with TRANSFER being a "no-guard" capability that simply encloses `debit` and `credit` calls:

```lisp
(defcap TRANSFER (from to amount)
  (compose-capability (DEBIT from))
  (compose-capability (CREDIT to)))

(defcap DEBIT (from)
  (enforce-guard (at 'guard (read table from))))

(defcap CREDIT (to)
  (check-account-exists to))

(defun transfer (from to amount)
  (with-capability (TRANSFER from to amount)
    (debit from amount)
    (credit to amount)))

(defun debit (user amount)
  (require-capability (DEBIT user))
    (update accounts user ...))

(defun credit (user amount)
  (require-capability (CREDIT user)
    (update accounts user ...)))
```

Thus, `TRANSFER` protects `debit` and `credit` from being used independently, while `DEBIT` governs specifically the
ability to debit, enforcing the guard, while `CREDIT` simply creates a "restricted" capability for `credit`.

### Improving efficiency
Once capabilities are granted they are installed into the pact environment for the scope of the call to `with-capability`;
once that form is exited, the capability is uninstalled. This scoping prevents duplicate testing of the predicate:
**capabilities that have already been acquired (or installed) and are in-scope are not re-evaluated**, either by
acquiring or requiring.

### `defcap` details

Since a `defcap` production both _specifies_ a "domain" of capability instances, and _implements_
the guard function, it has some surprising features. Since capability grant is cached in the environment,
the function does not need to be called when invoked in `with-capability` or `require-capability` asks
for some already-granted ticket.

As a result, **`defcap`s cannot be executed directly**, as arbitrary execution would violate the semantics described here.
This is an important security property as it ensures that the granting code can only be called in approved
contexts, inside the module.

### Testing scoping signatures with capabilities

Scoped signatures can be tested using the
new `env-sigs` REPL function as follows:

```lisp
(module accounts GOV
  ...
  (defcap PAY (sender receiver amount)
    (enforce-keyset (at 'keyset (read accounts sender))))

  (defun pay (sender receiver amount)
    (with-capability (PAY sender receiver amount)
      (transfer sender receiver amount)))
  ...
)

(set-sigs [{'key: "alice", 'caps: ["(accounts.PAY \"alice\" \"bob\" 10.0)"]}])
(accounts.pay "alice" "bob" 10.0) ;; works as the cap match the signature caps

(set-sigs [('key: "alice", 'caps: ["(accounts.PAY \"alice\" "\carol\" 10.0)"]}])
(expect-failure "payment to bob will no longer be able to enforce alice's keyset"
  (accounts.pay "alice" "bob" 10.0))
```



### Guard types
Guards come in five flavors: keyset, keyset reference, module, pact, and user guards.

#### Keyset guards.
These are the classic pact keysets. Using the `keyset` type is the one instance where you can restrict a
guard subtype, otherwise the `guard` type obscures the implementation type to prevent developers from
engaging in guard-specific control flow, which would be against best practices. Again, it is better to
switch to `guard` unless there is a specific need to use keysets.

```lisp
(enforce-guard (read-keyset "keyset"))
```

#### Keyset reference guards
Keysets can be installed into the environment with `define-keyset`, but if you wanted to store a reference
to a defined keyset, you would need to use a `string` type. To make environment keysets interoperate
with concrete keysets and other guards, we introduce the "keyset reference guard" which indicates that a
defined keyset is used instead of a concrete keyset.

```lisp
(enforce-guard (keyset-ref-guard "foo"))

(update accounts user { "guard": (keyset-ref-guard "foo") })
```

#### Module guards {#module-guards}
Module guards are a special guard that when enforced will fail unless:

- the code calling the enforce was called from within the module, or

- module governance is granted to the current transaction.

This is for allowing a module or smart contract to autonomously "own" and manage some asset. As
such it is operationally identical to how module table access is guarded: only module code or
a transaction having module admin can directly write to a module tables, or upgrade the module,
so there is no need to use a module guard for these in-module operations.
A module guard is used to "project" module admin outside of the module (e.g. to own coins
in an external ledger), or "inject" module admin into an internal database representation (e.g.
to own an internally-managed asset alongside other non-module owners).

See [Module Governance](#module-governance) for more information about module admin management.

`create-module-guard` takes a `string` argument to allow naming the guard, to indicate the purpose or
role of the guard.

```lisp
(enforce-guard (create-module-guard "module-owned-asset"))
```

#### Pact guards

Pact guards are a special guard that will only pass if called in the specific `defpact` execution in
which the guard was created.

Imagine an escrow transaction where the funds need to be moved into an escrow account: if modeled as a two-step
pact, the funds can go into a special account named after the pact id, guarded by a pact guard.
This means that only code in a subsequent step of that particular pact execution (ie having the same
pact ID) can pass the guard.

```lisp
(defpact escrow (from to amount)
  (step (with-capability (ESCROW) (init-escrow from amount)))
  (step (with-capability (ESCROW) (complete-escrow to amount))))

(defun init-escrow (from amount)
  (require-capability (ESCROW))
  (create-account (pact-id) (create-pact-guard "escrow"))
  (transfer from (pact-id) amount))

(defun complete-escrow (to amount)
  (require-capability (ESCROW))
  (with-capability (USER_GUARD (pact-id)) ;; enforces guard on account (pact-id)
    (transfer (pact-id) to amount)))
```

Pact guards turn pact executions into autonomous processes that can own assets, and is a powerful
technique for trustless asset management within a multi-step operation.

#### User guards

User guards allow the user to design an arbitrary predicate function to enforce the guard, given some
initial data. For instance, a user guard could be designed to require two separate keysets to be enforced:

```lisp
(defun both-sign (ks1 ks2)
  (enforce-keyset ks1)
  (enforce-keyset ks2))

(defun install-both-guard ()
  (write guard-table "both"
    { "guard":
      (create-user-guard
        (both-sign (read-keyset "ks1) (read-keyset "ks2")))
    }))


(defun enforce-both-guard ()
  (enforce-guard (at "guard" (read guard-table "both"))))
```

User guards can seem similar to capabilities but are different, namely in that they can be stored in the
database and passed around like plain data. Capabilities are in-module rights that can only be enforced
within the declaring module, and offer scoping and the other benefits mentioned above. User guards
are for implementing custom predicate logic that can't be expressed by other built-in guard types.

#### HTLC guard example

The following example shows how a "hash timelock" guard can be made, to implement atomic swaps.

```lisp

(create-hashlock-guard (secret-hash timeout signer-ks)
  (create-user-guard (enforce-hashlock secret-hash timeout signer-ks)))

(defun enforce-hashlock (secret-hash timeout signer-ks)
  (enforce-one [
    (enforce (= (hash (read-msg "secret")) secret-hash))
    (and
      (enforce-keyset signer-ks)
      (enforce (> (at "block-time" (chain-data)) timeout) "Timeout not passed"))
      ]))
```

### Events {#pact-events}

Pact 3.7 introduces [events](#pact-event) which are emitted in the course of a transaction and included in
the transaction receipt to allow for monitoring and proving via SPV that a particular event transpired.

In Pact, events are modeled as capabilities, for the following reasons:
- Capabilities already have the right shape for an event, which is essentially arbitrary data
published under a topic or name. With capabilities, the capability name is the topic, and the
arguments are the data.
- The acquisition of managed capabilities are a bona-fide event. Events complete the managed
lifecycle, where you might install/approve a capability of some quantity on the way in, but
not necessarily see what quantity was used. With events, the output of the actually acquired
capability is present in the receipt.
- Capabilities are protected such that they can only be acquired in module code, which is appropriate
as well for events.

#### The @event metadata tag
Any capability can cause events to be emitted upon acquisition by using the `@event` metadata tag.

```lisp
(defcap BURN(qty:decimal)
  @event
  ...
)
```
`@event` cannot be used alongside `@managed`, because ...

#### Managed capabilities are automatically eventing

Managed capabilites emit events automatically with the parameters specified in acquisition (as
opposed to install). From an eventing point of view, managed capabilities are those capabilities
that can only "happen once". Whereas, a non-managed, eventing capability can fire events an arbitrary
amount of times.

#### Testing for events

Use [env-events](#env-events) to test for emitted events in repl scripts.

Generalized Module Governance {#module-governance}
---

Before Pact 3.0, module upgrade and administration was governed by a defined keyset that is
referenced in the module definition. With Pact 3.0, this `string` value can alternately be an
unqualified bareword that references a `defcap` within the module body. This `defcap` is the
_module governance capability_.

With the introduction of the governance capability syntax, Pact modules now support
_generalized module governance_, allowing for module authors to design any governance scheme
they wish. Examples include tallying a stakeholder vote on an upgrade hash, or enforcing
more than one keyset.

### Keysets vs governance functions

To illustrate, let's consider a module governed by a keyset:

```lisp
(module foo 'foo-keyset ...)
```

This indicates that if a user tried to upgrade the module, or directly write to the module
tables, `'foo-keyset` would be enforced on the transaction signature set.

This can be directly implemented in a governance capability as follows:

```lisp
(module foo GOVERNANCE
  ...
  (defcap GOVERNANCE ()
    (enforce-keyset 'foo-keyset))
  ...
)
```

Note the capability can have whatever name desired; GOVERNANCE is a good idiomatic name however.

### Governance capability and module admin

As a `defcap`, the governance function cannot be called directly by user code. It is automatically
invoked in the following circumstances:

- A module upgrade is being attempted
- Module tables are being directly accessed outside the module code
- A [module guard](#module-guards) for this module is being enforced.

In these cases, the transaction is tested for elevated access to "module admin", defined as
the grant of the _module admin capability_. This capability cannot be expressed in user code,
so it cannot be installed, acquired, required or composed.

However, the implementing capability,
here called `GOVERNANCE`, can be installed or acquired etc. If passed, this gets scoped like
any normal capability, here over some protected code that only module admins can run.


#### Module admin capability scope

The special module admin capability, once automatically installed in the cases described
above, **stays in scope for the rest of the
calling transaction**. This is unlike "user" capabilities, which can only be acquired in
a fixed scope specified by the body of `with-capability`.

This may sound worrisome, but the rationale is that a governance capability once granted
should not be based on some transient fact that can become false during a single transaction.
This is important especially in module upgrades, _which can change the governance capability
itself_: if the module admin was tested again this could cause the upgrade to fail, for instance
when migrating data with direct table rights.

#### Capability risks

Also, this means that, when initially installing a module, _the governance function is not
invoked_. This is different behavior than when a keyset is specified: the keyset must be defined
and it is enforced, to ensure that the keyset actually exists.

Module governance is therefore more "risky" as it can mean that the module cannot be upgraded
if there is a bug in the governance capability. Clearly, care must be taken when implementing
module capabilities, and using the Pact formal verification system is highly recommended here.

### Example: stakeholder upgrade vote

In the following code, a module can be upgraded based on a vote. An upgrade is designed as a Pact
transaction, and its hash and code are distributed to stakeholders, who vote for the upgrade.
Once the upgrade is sent in, the vote is tallied in the governance capability, and if a simple
majority is found, the code is upgraded.

```lisp
(module govtest count-votes
  "Demonstrate programmable governance showing votes \
 \ for upgrade transaction hashes"
  (defschema vote
    vote-hash:string)

  (deftable votes:{vote})

  (defun vote-for-hash (user hsh)
    "Register a vote for a particular transaction hash"
    (write votes user { "vote-hash": hsh })
  )

  (defcap count-votes ()
    "Governance capability to tally votes for the upgrade hash".
    (let* ((h (tx-hash))
           (tally (fold (do-count h)
                        { "for": 0, "against": 0 }
                        (keys votes)))
          )
      (enforce (> (at 'for tally) (at 'against tally))
               (format "vote result: {}, {}" [h tally])))
  )

  (defun do-count (hsh tally u)
    "Add to TALLY if U has voted for HSH"
    (bind tally { "for" := f, "against" := a }
      (with-read votes u { 'vote-hash := v }
        (if (= v hsh)
            { "for": (+ 1 f), "against": a }
          { "for": f, "against": (+ 1 a) })))
  )
```

Interfaces {#interfaces}
---

An interface, as defined in Pact, is a collection of models used for formal verification, constant definitions, and typed function signatures. When a module issues an [implements](#implements), then that module is said to 'implement' said interface, and must provide an implementation . This allows for abstraction in a similar sense to Java's interfaces, Scala's traits, Haskell's typeclasses or OCaML's signatures. Multiple interfaces may be implemented in a given module, allowing for an expressive layering of behaviors.

Interfaces are declared using the `interface` keyword, and providing a name for the interface. Since interfaces cannot be upgraded, and no function implementations exist in an interface aside from constant data, there is no notion of governance that need be applied. Multiple interfaces may be implemented by a single module. If there are conflicting function names among multiple interfaces, then the two interfaces are incompatible, and the user must either inline the code they want, or redefine the interfaces to the point that the conflict is resolved.

Constants declared in an interface can be accessed directly by their fully qualified name `namespace.interface.const`, and so, they do not have the same naming constraints as function signatures.

Additionally, interfaces my make use of module declarations, admitting use of the [use](pact-functions.html#use) keyword, allowing interfaces to import members of other modules. This allows interface signatures to be defined in terms of table types defined in an imported module.

#### Example: Declaring and implementing an interface

```lisp
(interface my-interface
    (defun hello-number:string (number:integer)
      @doc "Return the string \"Hello, $number!\" when given a string"
        )

    (defconst SOME_CONSTANT 3)
)

(module my-module (read-keyset 'my-keyset)
    (implements my-interface)

    (defun hello-number:string (number:integer)
        (format "Hello, {}!" [number]))

    (defun square-three ()
        (* my-interface.SOME_CONSTANT my-interface.SOME_CONSTANT))
)
```

### Declaring models in an interface

[Formal verification](pact-properties.html) is implemented at multiple levels within an interface in order to provide an extra level of security. Models may be declared either within the body of the interface or at the function level in the same way that one would declare them in a module, with the exception that not all models are applicable to an interface. Indeed, since there is no abstract notion of tables for interfaces, abstract table invariants cannot be declared. However, if an interface imports table schema and types from a module via the [use](pact-functions.html#use) keyword, then the interface can define body and function models that apply directly to the concrete table type. Otherwise, all properties are candidates for declaration in an interface.

When models are declared in an interface, they are appeneded to the list of models present in the implementing module at the level of declaration: body-level models are appended to body-level models, and function-level models are appended to function-level models. This allows users to extend the constraints of an interface with models applicable to specific business logic and implementation.

Declaring models shares the same syntax with modules:

#### Example: declaring models, tables, and importing modules in an interface

```lisp
(interface coin-sig

  "Coin Contract Abstract Interface Example"

  (use acct-module)

  (defun transfer:string (from:string to:string amount:integer)
    @doc   "Transfer money between accounts"
    @model [(property (row-enforced accounts "ks" from))
            (property (> amount 0))
            (property (= 0 (column-delta accounts "balance")))
            ]
  )
)
```

Module References {#modrefs}
---

Pact 3.7 gains a form of _genericism_ with _module references_. This is motivated by the desire to interoperate between
modules that implement a common interface, and to be able to treat the indicated module as a data value to gain
_polymorphism_ across modules.

Modules and interfaces thus need to be referenced directly, which is simply accomplished by issuing their name in code.

```lisp
(module foo 'k
  (defun bar () 0))

(namespace ns)

(interface bar
  (defun quux:string ()))

(module zzz 'k
  (implements bar)
  (defun quux:string () "zzz"))

foo ;; module reference to 'foo', of type 'module'
ns.bar ;; module reference to `bar` interface, also of type 'module'
ns.zzz ;; module reference to `zzz` module, of type 'module{ns.bar}'
```

Using a module reference in a function is accomplished by specifying the type of the module reference argument,
and using the [dereference operator](#deref) `::` to invoke a member function of the interfaces specified in the type.

```lisp
(interface baz
  (defun quux:bool (a:integer b:string))
  (defconst ONE 1)
  )
(module impl 'k
  (implements baz)
  (defun quux:bool (a:integer b:string)
    (> (length b) a))
  )

...

(defun foo (bar:module{baz})
  (bar::quux 1 "hi") ;; derefs 'quux' on whatever module is passed in
  bar::ONE             ;; directly references interface const
)

...

(foo impl) ;; 'impl' references the module defined above, of type 'module{baz}'
```

Module references can be used as normal pact values, which includes storage in the database.



Computational Model {#computation}
---

Here we cover various aspects of Pact's approach to computation.

### Turing-Incomplete {#turingincomplete}
Pact is turing-incomplete, in that there is no recursion (recursion is detected before execution
and results in an error) and no ability to loop indefinitely. Pact does support operation on
list structures via [map](pact-functions.html#map), [fold](pact-functions.html#fold) and [filter](pact-functions.html#filter), but since there is no ability
to define infinite lists, these are necessarily bounded.

Turing-incompleteness allows Pact module loading to resolve all references in advance, meaning that
instead of addressing functions in a lookup table, the function definition is directly injected (or "inlined")
into the callsite. This is an example of the performance advantages of a Turing-incomplete language.

### Single-assignment Variables {#variables}

Pact allows variable declarations in [let expressions](#let) and [bindings](#bindings). Variables are
immutable: they cannot be re-assigned, or modified in-place.

A common variable declaration
occurs in the [with-read](pact-functions.html#with-read) function, assigning variables to column values by name. The
[bind](pact-functions.html#bind) function offers this same functionality for objects.

Module-global constant values can be declared with [defconst](#defconst).

### Data Types {#datatypes}

Pact code can be explicitly typed, and is always strongly-typed under the hood as the native
functions perform strict type checking as indicated in their documented type signatures.

Pact's supported types are:

- [Strings](#strings)
- [Integers](#integers)
- [Decimals](#decimals)
- [Booleans](#booleans)
- [Time values](#time)
- [Keysets](#keysets) and [Guards](#guards)
- [Lists](#lists)
- [Objects](#objects)
- [Function](#defun), [pact](#defpact), and [capability](#defcap) definitions
- [Tables](#deftable)
- [Schemas](#defschema)


### Performance {#performance}

Pact is designed to maximize the performance of [transaction execution](#transaction-execution), penalizing
queries and module definition in favor of fast recording of business events on the blockchain.
Some tips for fast execution are:

#### Single-function transactions {#singlefunctiontx}
Design transactions so they can be executed with a single function call.

#### Call with references instead of `use` {#usereferences}
When calling module functions in transactions, use [reference syntax](#references) instead of importing
the module with [use](#use). When defining modules that reference other module functions, `use` is
fine, as those references will be inlined at module definition time.

#### Hardcoded arguments vs. message values {#argsvmsgs}
A transaction can encode values directly into the transactional code:

```
(accounts.transfer "Acct1" "Acct2" 100.00)
```
or it can read values from the message JSON payload:

```
(defun transfer-msg ()
  (transfer (read-msg "from") (read-msg "to")
            (read-decimal "amount")))
...
(accounts.transfer-msg)
```
The latter will execute slightly faster, as there is less code to interpret at transaction time.

#### Types as necessary
With table schemas, Pact will be strongly typed for most use cases, but functions that do not
use the database might still need types. Use the [typecheck](typecheck) REPL function to add
the necessary types. There is a small cost for type enforcement at runtime, and too many type
signatures can harm readability. However types can help document an API, so this is a judgement call.

### Control Flow {#controlflow}
Pact supports conditionals via [if](pact-functions.html#if), bounded looping, and of course function application.

#### Use enforce {#use-the-enforce-luke}
"If" should never be used to enforce business logic invariants: instead, [enforce](pact-functions.html#enforce) is
the right choice, which will fail the transaction.

Indeed, failure is the only *non-local exit* allowed by Pact. This reflects Pact's emphasis on
*totality*.

Note that [enforce-one](pact-functions.html#enforce-one) (added in Pact 2.3) allows for testing a list of enforcements such that
if any pass, the whole expression passes. This is the sole example in Pact of "exception catching"
in that a failed enforcement simply results in the next test being executed, short-circuiting
on success.

#### Use built-in keyset predicates
The built-in keyset functions [keys-all](pact-functions.html#keys-all), [keys-any](pact-functions.html#keys-any), [keys-2](pact-functions.html#keys-2)
are hardcoded in the interpreter to execute quickly. Custom keysets require runtime resolution
which is slower.

### Functional Concepts {#fp}

Pact includes the functional-programming "greatest hits": [map](pact-functions.html#map), [fold](pact-functions.html#fold) and [filter](pact-functions.html#filter).
These all employ [partial application](#partial-application), where the list item is appended onto the application
arguments in order to serially execute the function.

```lisp
(map (+ 2) [1 2 3])
(fold (+) "" ["Concatenate" " " "me"])
```

Pact also has [compose](pact-functions.html#compose), which allows "chaining" applications in a functional style.

### Pure execution {#pure}
In certain contexts Pact can guarantee that computation is "pure", which simply means
that the database state will not be modified. Currently, `enforce`, `enforce-one`
and keyset predicate evaluation are all executed in a pure context. [defconst](#defconst)
memoization is also pure.

### LISP {#lisp}

Pact's use of LISP syntax is intended to make the code reflect its
runtime representation directly, allowing contract authors focus
directly on program execution. Pact code is stored in human-readable
form on the ledger, such that the code can be directly verified, but
the use of LISP-style [s-expression syntax](#sexp) allows this
code to execute quickly.

### Message Data {#messagedata}

Pact expects code to arrive in a message with a JSON payload and signatures. Message data
is read using [read-msg](pact-functions.html#read-msg) and related functions. While signatures are not directly
readable or writable, they are evaluated as part of [keyset predicate](#keysetpredicates)
enforcement.

#### JSON support {#json}

Values returned from Pact transactions are expected to be directly represented as JSON
values.

When reading values from a message via [read-msg](pact-functions.html#read-msg), Pact coerces JSON types
as follows:

- String -> `string`
- Number -> `decimal`
- Boolean -> `bool`
- Object -> `object`
- Array -> `list`

Integer values are represented as objects and read using [read-integer](pact-functions.html#read-integer).


Confidentiality {#confidentiality}
---

Pact is designed to be used in a *confidentiality-preserving* environment, where messages
are only visible to a subset of participants. This has significant implications for
smart contract execution.

### Entities
An *entity* is a business participant that is able or not able to see a confidential message.
An entity might be a company, a group within a company, or an individual.

### Disjoint Databases {#disjointdbs}
Pact smart contracts operate on messages organized by a blockchain, and serve to produce a
database of record, containing results of transactional executions. In a confidential environment,
different entities execute different transactions, meaning the resulting databases are now
*disjoint*.

This does not affect Pact execution; however, database data can no longer enact a "two-sided
transaction", meaning we need a new concept to handle enacting a single transaction over
multiple disjoint datasets.

### Confidential Pacts {#confidential-pacts}

An important feature for confidentiality in Pact is the ability to orchestrate disjoint
transactions in sequence to be executed by targeted entities. This is described in the next section.

Asynchronous Transaction Automation with "Pacts" {#pacts}
---

"Pacts" are multi-stage sequential transactions that are defined as a single body of code called
a [pact](#defpact). Defining a multi-step interaction as a pact ensures that transaction participants will
enact an agreed sequence of operations, and offers a special "execution scope" that can be used
to create and manage data resources only during the lifetime of a given multi-stage interaction.

Pacts are a form of *coroutine*, which is a function that has multiple exit and re-entry points. Pacts
are composed of [steps](#step) such that only a single step is executed in a given blockchain transaction.
Steps can only be executed in strict sequential order.

A pact is defined with arguments, similarly to function definition. However, arguments values are only
evaluated in the execution of the initial step, after which those values are available unchanged to subsequent steps.
To share new values
with subsequent steps, a step can [yield](pact-functions.html#yield) values which the subsequent step can recover using
the special [resume](pact-functions.html#resume) binding form.

Pacts are designed to run in one of two different contexts, private and public. A private pact is
indicated by each step identifying a single entity to execute the step, while public steps do
not have entity indicators. A pact can only be uniformly public or private: if some steps
has entity indicators and others do not, this results in an error at load time.

### Public Pacts
Public pacts are comprised of steps that can only execute in strict sequence. Any enforcement of who can execute a step
happens within the code of the step expression. All steps are "manually" initiated by some participant
in the transaction with CONTINUATION commands sent into the blockchain.

### Private Pacts
Private pacts are comprised of steps that execute in sequence where each step only executes on entity
nodes as selected by the provided 'entity' argument in the step; other entity nodes "skip" the step.
Private pacts are executed automatically by the blockchain platform after the initial step is sent
in, with the executing entity's node automatically sending the CONTINUATION command for the next step.

### Failures, Rollbacks and Cancels

Failure handling is dramatically different in public and private pacts.

In public pacts, a rollback expression is specified to indicate that the pact can be "cancelled" at
this step with a participant sending in a CANCEL message before the next step is executed. Once the last
step of a pact has been executed, the pact will be finished and cannot be rolled back. Failures
in public steps are no different than a failure in a non-pact transaction: all changes are rolled back.
Pacts can therefore only be canceled explicitly and should be modeled to offer all necessary cancel options.

In private pacts, the sequential execution of steps is automated by the blockchain platform itself. A failure
results in a ROLLBACK message being sent from the executing entity node which will trigger any rollback expression
specified in the previous step, to be executed by that step's entity. This failure will then "cascade" to the
previous step as a new ROLLBACK transaction, completing when the first step is rolled back.

### Yield and Resume

A step can yield values to the following step using [yield](pact-functions.html#yield) and [resume](pact-functions.html#resume). In public,
this is an unforgeable value, as it is maintained within the blockchain pact scope. In private, this is
simply a value sent with a RESUME message from the executed entity.

### Pact execution scope and `pact-id`

Every time a pact is initiated, it is given a unique ID which is retrievable using the [pact-id](pact-functions.html#pact-id)
function, which will return the ID of the currently executing pact, or fail if not running within a pact
scope. This mechanism can thus be used to guard access to resources, analogous to the use of keysets and
signatures. One typical use of this is to create escrow accounts that can only be used within the context
of a given pact, eliminating the need for a trusted third party for many use-cases.

### Testing pacts

Pacts
can be tested in repl scripts using the [env-entity](pact-functions.html#env-entity), [env-step](pact-functions.html#env-step)
and [pact-state](pact-functions.html#pact-state) repl functions to simulate pact executions.

It is also possible to simulate pact execution in the pact server API by formatting [continuation Request](#request-yaml)
yaml files into API requests with a `cont` payload.

Dependency Management {#dependency-management}
---
Pact supports a number of features to manage a module's dependencies on other Pact modules.

### Module Hashes
Once loaded, a Pact module is associated with a hash computed from the module's source code text.
This module hash uniquely identifies the version of the module. Hashes are base64url-encoded
BLAKE2 256-bit hashes.
Module hashes can be examined with [describe-module](pact-functions.html#describe-module):

```
pact> (at "hash" (describe-module 'accounts))
"ZHD9IZg-ro1wbx7dXi3Fr-CVmA-Pt71Ov9M1UNhzAkY"
```

### Pinning module versions with `use`
The [use](#use) special form allows
a module hash to be specified, in order to pin the dependency version. When
used within a module declaration, it introduces the dependency
hash value into the module's hash.
This allows a "dependency-only" upgrade to push the upgrade to the module version.

### Inlined Dependencies: "No Leftpad"
When a module is loaded, all references to foreign modules are resolved, and their code is
directly inlined. At this point, upstream definitions are permanent: the only way to upgrade
dependencies is to reload the original module.

This permanence is great for user code: once a module is loaded, an upstream provider cannot change what code
is executed within. However, this creates a big problem
for upstream developers, as they cannot upgrade the downstream code themselves in order to address an exploit, or to
introduce new features.

### Blessing hashes
A trade-off is needed to balance these opposing interests. Pact offers the ability for upstream
code to break downstream dependent code at runtime. Table access is guarded to enforce
that the module hash of the inlined dependency either matches the runtime version, or
is in a set of "blessed" hashes, as specified by [bless](#bless) in the module declaration:

```lisp
(module provider 'keyset
  (bless "ZHD9IZg-ro1wbx7dXi3Fr-CVmA-Pt71Ov9M1UNhzAkY")
  (bless "bctSHEz4N5Y1XQaic6eOoBmjty88HMMGfAdQLPuIGMw")
  ...
)
```

Dependencies with these hashes will continue to function after the module is loaded.
Unrecognized hashes will cause the transaction to fail. However, "pure" code that does
not access the database is unaffected. This prevents a "leftpad situation" where trivial
utility functions can harm downstream code stability.

### Phased upgrades with "v2" modules
Upstream providers can use the bless mechanism to phase in an important upgrade, by renaming
the upgraded module to indicate the new version, and replacing the old module with a new,
empty module that only blesses the last version (and whatever earlier versions desired).
New clients will
fail to import the "v1" code, requiring them to use the new version,
while existing users can continue to use the old version,
presumably up to some advertised time limit. The "empty" module can offer migration
functions to handle migrating user data to the new module, for the user to self-upgrade
in the time window.


Syntax
======

Literals {#literals}
--------

### Strings {#strings}

String literals are created with double-ticks:

```
pact> "a string"
"a string"
```

Strings also support multiline by putting a backslash before and after whitespace (not interactively).

```lisp
(defun id (a)
  "Identity function. \
  \Argument is returned."
  a)
```

### Symbols {#symbols}

Symbols are string literals representing some unique item in the runtime, like a function or a table name.
Their representation internally is simply a string literal so their usage is idiomatic.

Symbols are created with a preceding tick, thus they do not support whitespace nor multiline syntax.

```
pact> 'a-symbol
"a-symbol"
```

### Integers {#integers}

Integer literals are unbounded, and can be positive or negative.

```
pact> 12345
12345
pact> -922337203685477580712387461234
-922337203685477580712387461234
```

### Decimals {#decimals}

Decimal literals have potentially unlimited precision.

```
pact> 100.25
100.25
pact> -356452.234518728287461023856582382983746
-356452.234518728287461023856582382983746
```

### Booleans {#booleans}

Booleans are represented by `true` and `false` literals.

```
pact> (and true false)
false
```

### Lists {#lists}

List literals are created with brackets, and optionally separated with commas.
Uniform literal lists are given a type in parsing.
```
pact> [1 2 3]
[1 2 3]
pact> [1,2,3]
[1 2 3]
pact> (typeof [1 2 3])
"[integer]"
pact> (typeof [1 2 true])
"list"
```

### Objects {#objects}

Objects are dictionaries, created with curly-braces specifying key-value pairs
using a colon `:`.
For certain applications (database updates), keys must be strings.

```
pact> { "foo": (+ 1 2), "bar": "baz" }
{ "foo": 3, "bar": "baz" }
```

### Bindings {#bindings}
Bindings are dictionary-like forms, also created with curly braces, to bind
database results to variables using the `:=` operator.
They are used in [with-read](pact-functions.html#with-read), [with-default-read](pact-functions.html#with-default-read),
[bind](pact-functions.html#bind) and [resume](pact-functions.html#resume) to assign variables to named columns in a row, or values in an object.

```lisp
(defun check-balance (id)
  (with-read accounts id { "balance" := bal }
    (enforce (> bal 0) (format "Account in overdraft: {}" [bal]))))
```

### Lambdas {#lambdas}
Lambdas, or "anonymous functions", allow defining functions to be applied in local scope, as opposed to defining functions at top-level with `defun.`

Lambdas are supported in `let`, `let*`, and as inline arguments to built-in function applications.

```lisp
  ; identity function
  (let ((f (lambda (x) x))) (f a))
  ; native example
  (let ((f (lambda (x) x))) (map (f) [1 2 3]))
  ; Inline native example:
  (map (lambda (x) x) [1 2 3])
```

Type specifiers
-----

Types can be specified in syntax with the colon `:` operator followed by
a type literal or user type specification.

### Type literals

- `string`
- `integer`
- `decimal`
- `bool`
- `time`
- `keyset`
- `list`, or `[type]` to specify the list type
- `object`, which can be further typed with a schema
- `table`, which can be further typed with a schema
- `module`, which must be further typed with required interfaces.

### Schema type literals

A schema defined with [defschema](#defschema) is referenced by name enclosed in curly braces.

```lisp
table:{accounts}
object:{person}
```

### Module type literals

[Module references](#modrefs) are specified by the interfaces they demand as a comma-delimited list.

```
module:{fungible-v2,user.votable}
```


Dereference operator {#deref}
---

The dereference operator `::` allows a member of an interface specified in the type of a
[module reference](#modrefs) to be invoked at run-time.

```lisp
(interface baz
  (defun quux:bool (a:integer b:string))
  (defconst ONE 1)
  )
...
(defun foo (bar:module{baz})
  (bar::quux 1 "hi") ;; invokes 'quux' on whatever module is passed in
  bar::ONE             ;; directly references interface const
)
```

### What can be typed

#### Function arguments and return types
```lisp
(defun prefix:string (pfx:string str:string) (+ pfx str))
```

#### Let variables
```lisp
(let ((a:integer 1) (b:integer 2)) (+ a b))
```

#### Tables and objects
Tables and objects can only take a schema type literal.
```lisp
(deftable accounts:{account})

(defun get-order:{order} (id) (read orders id))
```

#### Consts
```lisp
(defconst PENNY:decimal 0.1)
```


Special forms {#special-forms}
---

### Docs and Metadata
Many special forms like [defun](#defun) accept optional documentation strings,
in the following form:

```lisp
(defun average (a b)
  "take the average of a and b"
  (/ (+ a b) 2))
```

Alternately, users can specify metadata using a special `@`-prefix syntax.
Supported metadata fields are `@doc` to provide a documentation string, and `@model`
that can be used by Pact tooling to verify the correctness of the implementation:

```lisp
(defun average (a b)
  @doc   "take the average of a and b"
  @model (property (= (+ a b) (* 2 result)))
  (/ (+ a b) 2))
```

Indeed, a bare docstring like `"foo"` is actually just a short form for `@doc "foo"`.

Specific information on *Properties* can be found in [The Pact Property Checking System](pact-properties.html).

### bless {#bless}
```
(bless HASH)
```

Within a module declaration, bless a previous version of that module as identified by HASH.
See [Dependency management](#dependency-management) for a discussion of the blessing mechanism.

```lisp
(module provider 'keyset
  (bless "ZHD9IZg-ro1wbx7dXi3Fr-CVmA-Pt71Ov9M1UNhzAkY")
  (bless "bctSHEz4N5Y1XQaic6eOoBmjty88HMMGfAdQLPuIGMw")
  ...
)
```


### defun {#defun}

```lisp
(defun NAME ARGLIST [DOC-OR-META] BODY...)
```

Define NAME as a function, accepting ARGLIST arguments, with optional DOC-OR-META.
Arguments are in scope for BODY, one or more expressions.

```lisp
(defun add3 (a b c) (+ a (+ b c)))

(defun scale3 (a b c s)
  "multiply sum of A B C times s"
  (* s (add3 a b c)))
```

### defcap {#defcap}

```lisp
(defcap NAME ARGLIST [DOC] BODY...)
```

Define NAME as a capability, specified using ARGLIST arguments, with optional DOC.
A `defcap` models a capability token which will be stored in the environment to represent
some ability or right. Code in BODY is only called within special capability-related
functions `with-capability` and `compose-capability` when the token as parameterized
by the arguments supplied is not found in the environment. When executed, arguments are
in scope for BODY, one or more expressions.


```lisp
(defcap USER_GUARD (user)
  "Enforce user account guard
  (with-read accounts user
    { "guard": guard }
    (enforce-guard guard)))
```

### defconst {#defconst}
```lisp
(defconst NAME VALUE [DOC-OR-META])
```

Define NAME as VALUE, with option DOC-OR-META. Value is evaluated upon module load and "memoized".

```lisp
(defconst COLOR_RED="#FF0000" "Red in hex")
(defconst COLOR_GRN="#00FF00" "Green in hex")
(defconst PI 3.14159265 "Pi to 8 decimals")
```

### defpact {#defpact}

```
(defpact NAME ARGLIST [DOC-OR-META] STEPS...)
```


Define NAME as a _pact_, a computation comprised of multiple steps that occur
in distinct transactions.
Identical to [defun](#defun) except body must be comprised of [steps](#step) to be
executed in strict sequential order. Steps must uniformly be "public" (no entity indicator)
or "private" (with entity indicator). With private steps, failures result in a reverse-sequence
"rollback cascade".

```lisp
(defpact payment (payer payer-entity payee
                  payee-entity amount)
  (step-with-rollback payer-entity
    (debit payer amount)
    (credit payer amount))
  (step payee-entity
    (credit payee amount)))
```

Public defpacts may be nested (though the recursion restrictions apply, so it must be a different defpact). They may be kicked off
like a regular function call within a defpact, but are continued after the first step by calling `continue` with the same arguments.

As such, they have the following restrictions:
- The number of steps of the child must match the number of steps of the parent.
- If a parent defpact step has the rollback field, so must the child. If parent steps roll back, so do child steps.
- `continue` must be called with the same continuation arguments as the defpact originally dispatched,
 to support multiple nested defpacts of the same function but with different arguments.

 The following example shows well-formed defpacts with equal number of steps, nested rollbacks and continue:

```
(defpact payment (payer payee amount)
  (step-with-rollback
    (debit payer amount)
    (credit payer amount))
  (step payee-entity
    (credit payee amount)))

...
(defpact split-payment (payer payee1 payee2 amount ratio)
  (step-with-rollback
    (let
      ((payment1 (payment payer payee1 (* amount ratio)))
      (payment2 (payment payer payee2 (* amount (- 1 ratio))))
      )
      "step 0 complete"
    )
    (let
      ((payment1 (continue (payment payer payee1 (* amount ratio))))
       (payment2 (continue (payment payer payee2 (* amount (- 1 ratio)))))
      )
      "step 0 rolled back"
    )
  )
  (step
    (let
      ((payment1 (continue (payment payer payee1 (* amount ratio))))
       (payment2 (continue (payment payer payee2 (* amount (- 1 ratio)))))
      )
      "step 1 complete"
    )
  )
)
```

### defschema {#defschema}

```
(defschema NAME [DOC-OR-META] FIELDS...)
```

Define NAME as a _schema_, which specifies a list of FIELDS. Each field
is in the form `FIELDNAME[:FIELDTYPE]`.

```lisp
(defschema accounts
  "Schema for accounts table".
  balance:decimal
  amount:decimal
  ccy:string
  data)
```

### deftable {#deftable}

```
(deftable NAME[:SCHEMA] [DOC-OR-META])
```

Define NAME as a _table_, used in database functions. Note the
table must still be created with [create-table](pact-functions.html#create-table).

### let {#let}

```
(let (BINDPAIR [BINDPAIR [...]]) BODY)
```

Bind variables in BINDPAIRs to be in scope over BODY. Variables
within BINDPAIRs cannot refer to previously-declared variables in
the same let binding; for this use [let\*](#letstar).

```lisp
(let ((x 2)
      (y 5))
  (* x y))
> 10
```

### let&#42; {#letstar}

```
(let* (BINDPAIR [BINDPAIR [...]]) BODY)
```

Bind variables in BINDPAIRs to be in scope over BODY. Variables
can reference previously declared BINDPAIRS in the same let.
`let*` is expanded at compile-time to nested `let` calls for
each BINDPAIR; thus `let` is preferred where possible.

```lisp
(let* ((x 2)
       (y (* x 10)))
  (+ x y))
> 22
```

### cond; {#cond}

```
(cond (TEST BRANCH) [(TEST2 BRANCH2) [...]] ELSE-BRANCH)
```

Special form/sugar to produce a series of "if-elseif-else" expressions, such that if TEST1 passes, BRANCH1 is evaluated, otherwise followed by evaluating TEST2 -> BRANCH2 etc. ELSE-BRANCH is evaluated if all tests fail.

`cond` is syntactically expanded such that

```lisp
(cond
   (a b)
   (c d)
   (e f)
   g)
```

is expanded to:

```lisp
(if a b (if c d (if e f g)))
```

### step {#step}
```
(step EXPR)
(step ENTITY EXPR)
```

Define a step within a [defpact](#defpact), such that any prior steps will be
executed in prior transactions, and later steps in later transactions.
Including an ENTITY argument indicates that this step is intended for confidential
transactions. Therefore, only the ENTITY would execute the step, and other
participants would "skip" it.

### step-with-rollback {#step-with-rollback}
```
(step-with-rollback EXPR ROLLBACK-EXPR)
(step-with-rollback ENTITY EXPR ROLLBACK-EXPR)
```

Define a step within a [defpact](#defpact) similarly to [step](#step) but specifying ROLLBACK-EXPR.
With ENTITY, ROLLBACK-EXPR will only be executed upon failure of a subsequent step, as part of a reverse-sequence "rollback
cascade" going back from the step that failed to the first step. Without ENTITY,
ROLLBACK-EXPR functions as a "cancel function" to be explicitly executed by a participant.

### use {#use}
```
(use MODULE)
(use MODULE HASH)
(use MODULE IMPORTS)
(use MODULE HASH IMPORTS)
```

Import an existing MODULE into a namespace. Can only be issued at the top-level, or within a module
declaration. MODULE can be a string, symbol or bare atom. With HASH, validate that the imported module's
hash matches HASH, failing if not. Use [describe-module](pact-functions.html#describe-module) to query for the
hash of a loaded module on the chain.

An optional list of IMPORTS consisting of function, constant, and schema names may be supplied. When this explicit import list is present, only those names will be made available for use in the module body. If no list is supplied, then every name in the imported module will be brought into scope. When two modules are defined in the same transaction, all names will be in scope for all modules, and import behavior will be defaulted to the entire module. IMPORTS may only be empty when a module hash is also supplied. If a module hash is not supplied, IMPORTS are required to be either a non-empty list, or left undeclared.

```lisp
(use accounts)
(transfer "123" "456" 5 (time "2016-07-22T11:26:35Z"))
"Write succeeded"
```
```lisp
(use accounts "ToV3sYFMghd7AN1TFKdWk_w00HjUepVlqKL79ckHG_s")
(transfer "123" "456" 5 (time "2016-07-22T11:26:35Z"))
"Write succeeded"
```
```lisp
(use accounts [ transfer example-fun ])
(transfer "123" "456" 5 (time "2016-07-22T11:26:35Z"))
"Write succeeded"
```
```lisp
(use accounts "ToV3sYFMghd7AN1TFKdWk_w00HjUepVlqKL79ckHG_s" [ transfer example-fun ])
(transfer "123" "456" 5 (time "2016-07-22T11:26:35Z"))
"Write succeeded"
```

### interface {#interface}
```
(interface NAME [DOR-OR-META] BODY...)
```

Define and install interface NAME, with optional DOC-OR-META.

BODY is composed of definitions that will be scoped in the module. Valid expressions in a module include:

- [defun](#defun)
- [defconst](#defconst)
- [defschema](#defschema)
- [defpact](#defpact)
- [defcap](#defcap)
- [use](#use)
- [models](#pact-properties.html)

```lisp
(interface coin-sig
  "'coin-sig' represents the Kadena Coin Contract interface. This contract     \
  \provides both the the general interface for a Kadena's token, supplying a   \
  \transfer function, coinbase, account creation and balance query."
  (defun create-account:string (account:string guard:guard)
    @doc "Create an account for ACCOUNT, with GUARD controlling access to the  \
    \account."
    @model [ (property (not (= account ""))) ]
    )
  (defun transfer:string (sender:string receiver:string amount:decimal)
    @doc "Transfer AMOUNT between accounts SENDER and RECEIVER on the same    \
    \chain. This fails if either SENDER or RECEIVER does not exist.           \
    \Create-on-transfer can be done using the 'transfer-and-create' function."
    @model [ (property (> amount 0.0))
             (property (not (= sender receiver)))
           ]
    )
  (defun account-balance:decimal (account:string)
    @doc "Check an account's balance"
    @model [ (property (not (= account ""))) ]
    )
)
```

### module {#module}
```
(module NAME KEYSET-OR-GOVERNANCE [DOC-OR-META] BODY...)
```

Define and install module NAME, with module admin governed by KEYSET-OR-GOVERNANCE, with optional DOC-OR-META.

If KEYSET-OR-GOVERNANCE is a string, it references a keyset that has been installed with `define-keyset`
that will be tested whenever module admin is required. If KEYSET-OR-GOVERNANCE is an unqualified atom, it
references a `defcap` capability which will be acquired if module admin is requested.

BODY is composed of definitions that will be scoped in the module. Valid productions in a module include:

- [defun](#defun)
- [defpact](#defpact)
- [defcap](#defcap)
- [deftable](#deftable)
- [defschema](#defschema)
- [defconst](#defconst)
- [implements](#implements)
- [use](#use)
- [bless](#bless)

```lisp
(module accounts 'accounts-admin
  "Module for interacting with accounts"

  (defun create-account (id bal)
   "Create account ID with initial balance BAL"
   (insert accounts id { "balance": bal }))

  (defun transfer (from to amount)
   "Transfer AMOUNT from FROM to TO"
   (with-read accounts from { "balance": fbal }
    (enforce (<= amount fbal) "Insufficient funds")
     (with-read accounts to { "balance": tbal }
      (update accounts from { "balance": (- fbal amount) })
      (update accounts to { "balance": (+ tbal amount) }))))
)
```

### implements {#implements}
```
(implements INTERFACE)
```

Specify that containing module _implements_ interface INTERFACE. This requires the module to implement
all functions, pacts, and capabilities specified in INTERFACE with identical signatures (same argument
names and declared types).

Note that [models](pact-properties.html) declared for the implemented interface and its members will be
appended to whatever models are declared within the implementing module.

A module thus specified can be used as a [module reference](#modrefs) for the specified interface(s).

Expressions {#expression}
---

Expressions may be [literals](#literals), atoms, s-expressions, or references.


### Atoms {#atom}

Atoms are non-reserved barewords starting with a letter or allowed symbol, and containing letters,
digits and allowed symbols. Allowed symbols are `%#+-_&$@<>=?*!|/`. Atoms must
resolve to a variable bound by a [defun](#defun), [defpact](#defpact), [binding](#bindings) form, [lambda](#lambda) form,
or to symbols imported into the namespace with [use](#use).

### S-expressions {#sexp}

S-expressions are formed with parentheses, with the first atom determining if
the expression is a [special form](#special-forms) or a function application, in
which case the first atom must refer to a definition.

#### Partial application {#partial-application}
An application with less than the required arguments is in some contexts a valid
*partial application* of the function. However, this is only supported in
Pact's [functional-style functions](#fp); anywhere else this will result in a
runtime error.

### References {#references}

References are multiple atoms joined by a dot `.` that directly resolve to definitions found
in other modules.

```
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
```

References are preferred over `use` for transactions, as references resolve faster.
However, when defining a module, `use` is preferred for legibility.

Time formats
===

The [parse-time](pact-functions.html#parse-time) and [format-time](pact-functions.html#format-time)
functions accept format codes that derive from GNU `strftime` with some extensions, as follows:

`%%` - literal `"%"`

`%z` - RFC 822/ISO 8601:1988 style numeric time zone (e.g., `"-0600"` or `"+0100"`)

`%N` - ISO 8601 style numeric time zone (e.g., `"-06:00"` or `"+01:00"`) /EXTENSION/

`%Z` - timezone name

`%c` - The preferred calendar time representation for the current locale. As 'dateTimeFmt' `locale` (e.g. `%a %b %e %H:%M:%S %Z %Y`)

`%R` - same as `%H:%M`

`%T` - same as `%H:%M:%S`

`%X` - The preferred time of day representation for the current locale. As 'timeFmt' `locale` (e.g. `%H:%M:%S`)

`%r` - The complete calendar time using the AM/PM format of the current locale. As 'time12Fmt' `locale` (e.g. `%I:%M:%S %p`)

`%P` - day-half of day from ('amPm' `locale`), converted to lowercase, `"am"`, `"pm"`

`%p` - day-half of day from ('amPm' `locale`), `"AM"`, `"PM"`

`%H` - hour of day (24-hour), 0-padded to two chars, `"00"`–`"23"`

`%k` - hour of day (24-hour), space-padded to two chars, `" 0"`–`"23"`

`%I` - hour of day-half (12-hour), 0-padded to two chars, `"01"`–`"12"`

`%l` - hour of day-half (12-hour), space-padded to two chars, `" 1"`–`"12"`

`%M` - minute of hour, 0-padded to two chars, `"00"`–`"59"`

`%S` - second of minute (without decimal part), 0-padded to two chars, `"00"`–`"60"`

`%v` - microsecond of second, 0-padded to six chars, `"000000"`–`"999999"`. /EXTENSION/

`%Q` - decimal point and fraction of second, up to 6 second decimals, without trailing zeros.
       For a whole number of seconds, `%Q` produces the empty string. /EXTENSION/

`%s` - number of whole seconds since the Unix epoch. For times before
the Unix epoch, this is a negative number. Note that in `%s.%q` and `%s%Q`
the decimals are positive, not negative. For example, 0.9 seconds
before the Unix epoch is formatted as `"-1.1"` with `%s%Q`.

`%D` - same as `%m\/%d\/%y`

`%F` - same as `%Y-%m-%d`

`%x` - as 'dateFmt' `locale` (e.g. `%m\/%d\/%y`)

`%Y` - year, no padding.

`%y` - year of century, 0-padded to two chars, `"00"`–`"99"`

`%C` - century, no padding.

`%B` - month name, long form ('fst' from 'months' `locale`), `"January"`–`"December"`

`%b`, `%h` - month name, short form ('snd' from 'months' `locale`), `"Jan"`–`"Dec"`

`%m` - month of year, 0-padded to two chars, `"01"`–`"12"`

`%d` - day of month, 0-padded to two chars, `"01"`–`"31"`

`%e` - day of month, space-padded to two chars, `" 1"`–`"31"`

`%j` - day of year, 0-padded to three chars, `"001"`–`"366"`

`%G` - year for Week Date format, no padding.

`%g` - year of century for Week Date format, 0-padded to two chars, `"00"`–`"99"`

`%f` - century for Week Date format, no padding. /EXTENSION/

`%V` - week of year for Week Date format, 0-padded to two chars, `"01"`–`"53"`

`%u` - day of week for Week Date format, `"1"`–`"7"`

`%a` - day of week, short form ('snd' from 'wDays' `locale`), `"Sun"`–`"Sat"`

`%A` - day of week, long form ('fst' from 'wDays' `locale`), `"Sunday"`–`"Saturday"`

`%U` - week of year where weeks start on Sunday (as 'sundayStartWeek'), 0-padded to two chars, `"00"`–`"53"`

`%w` - day of week number, `"0"` (= Sunday) – `"6"` (= Saturday)

`%W` - week of year where weeks start on Monday (as 'Data.Thyme.Calendar.WeekdayOfMonth.mondayStartWeek'), 0-padded to two chars, `"00"`–`"53"`

Note: `%q` (picoseconds, zero-padded) does not work properly so not documented here.

## Default format and JSON serialization

The default format is a UTC ISO8601 date+time format: "%Y-%m-%dT%H:%M:%SZ", as accepted by the [time](pact-functions.html#id4)
function. While the time object internally supports up to microsecond resolution, values returned from the Pact
interpreter as JSON will be serialized with the default format. When higher resolution is desired, explicitly format
times with `%v` and related codes.

## Examples

### ISO8601

```
pact> (format-time "%Y-%m-%dT%H:%M:%S%N" (time "2016-07-23T13:30:45Z"))
"2016-07-23T13:30:45+00:00"
```

### RFC822

```
pact> (format-time "%a, %_d %b %Y %H:%M:%S %Z" (time "2016-07-23T13:30:45Z"))
"Sat, 23 Jul 2016 13:30:45 UTC"
```

### YYYY-MM-DD hh:mm:ss.000000

```
pact> (format-time "%Y-%m-%d %H:%M:%S.%v" (add-time (time "2016-07-23T13:30:45Z") 0.001002))
"2016-07-23 13:30:45.001002"
```
