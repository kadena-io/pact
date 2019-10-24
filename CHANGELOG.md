3.3.0
---

* Managed capabilities (see documentation)
* Production Gas model v1
* Added signing utilities
* Interfaces now admit `defcap`, `defschema`, and `defpact` declarations
* Add explicit import system. Users may now import function, constant and schema symbols
* Enforce unique names for in-module declarations
* Typechecking interfaces now succeeds
* Previous block hashes added to public data
* TTL + creation time added to public data
* Added new natives:
  * `is-charset`: provides support for testing string conformity with a given charset.
  * `CHARSET_LATIN1`: built-in constant defining the latin1-charset
  * `CHARSET_ASCII`: built-in constant defining the ascii-charset
  * `test-capability` overload for granted capabilities
  * `expect-failure` overload allowing users to match on substrings of errors
  Currently suppported charsets include: ASCII and ISO 8859-1 (latin1 supplement) character sets
* Bug fix disallowing access to Capabilities via FQN
* Steps with rollbacks now disallowed w/ cross-chain yields
* Network-id requirement in payloads
* Analysis:
  * list and object length analysis
  * `with-default-read` analysis bugs fixed
  * Unbound column invariants
  * vacuous property detection and report

3.2.1
---

* New native function `try` has been added
* Bug fixes to the typechecker
* Improved documentation

3.2.0
---

* All non key-related data for `decrypt-cc20p1305` is now required to be base64-encoded
* Improved documentation
* Analysis:
  * bitwise operators are now supported

3.1.0
---
* Add SPV endorsement functionality to `defpact`s
* '=' now compares only like terms
* User guards can now be created as closures of boolean-valued functions
* Improved swagger documentation
* Improved error messages
* Added new decryption and keypair validation primitives for CHACHA20-POLY1305 Authenticated Encryption with Associated Data (AEAD)
* Added bitwise integer native functions `&`, `~`, `shift`, `xor`, and `|`
* Add user-facing `chain-data` native schema type
* Documentation for namespaces and interfaces
* Add base-64 case to `str-to-int` and improve error messages
* Add `int-to-str` native
* Analysis:
  * traces now render on same line
  * added support pact step signatures, and governance analysis
  * added support for `chain-data` pact native
  * upgrade z3 version support to 4.8.5
  * Scope checking for interfaces

3.0.1
---
* Check for module in stack when enforcing module guard
* Output test failures on error
* fix describe table output and meta pretty instance


3.0.0
---
* Guards and capabilities, with full model-checking support
* Scoped namespaces for modules and keysets
* Adds module governance function
* Migrate all runtime pact state to DB backend: modules, pacts, transaction ID management
* Improved transaction non-malleability: signatories and chain version in payload
* Adds SPV support with configurable backend
* Multiple Cryptographic signature schemes: ED25519, ECDSA
* Adds Table-based Gas model
* Standardize on CommandResult in all API endpoints
* CommandResult adds gas consumed, logs, and metadata fields
* CommandResult's "result" field changed to output reliable, user-friendly Terms and errors.
* Adds continuation proof field to ContMsg
* Adds public metadata to Pact Command API
* Format all hashes in Base64url
* Adds typechecking for lists, `yield`, and `resume`
* Typechecker and analysis support for partial schemas
* Removes support for non-string keys in objects
* Rejects decimal literals with too many digits
* `defpact` can run in local mode
* Disallow rollback on the last step of a pact
* Replaces REPL's `env-step` and `env-pactid` with `continue-pact` and `pact-state`
* Adds REPL functions `chain-data` and `env-chain-data`
* Better compile and typecheck errors
* Pretty printing
* Working Windows build
* Analysis:
  * Adds analysis for non-literal constants
  * Adds pact analysis tools developer guide
  * Adds analysis for `pact-id`, `defpact`, `yield`, and `resume`
  * Improves output from verification
  * Supports analysis of (bounded-size) lists and nested objects
  * Pins z3 to a specific version (4.8.3)
  * Uses timeout of 1 second for all z3 queries
* Swagger for API


2.6.1
---
* Korean readthedocs
* Interfaces copy models onto implementing modules
* Better/tighter compile phase, remove TBless, TImplements
* Fix bug preventing correct reporting of argument values in falsifying models
  from the property checker

2.6.0
---
* Pact Interfaces
* Localized docs in JP, EN
* Add configurable gas limit for `pact -s`
* Remove file server from `pact -s`
* Fix empty list runtime type enforcement bug
* Enforce top-level only for builtins `pact-version`, `enforce-pact-version`, etc
* Various changes to support web repl
* use cryptonite for Blake2 implementation
* new `str-to-int` builtin
* Numerous fixes to pact reference docs
* New compiler using megaparsec
* analysis supports `str-to-int` and `read-integer` built-ins
* new `row-exists`, `read`, `column-of`, `column-written` properties
* `@model` now always takes a list when applied on modules, functions, or schemas.
* properties can now be applied to an entire module
* Fix bug in analysis impl of decimal arithmetic
* Model falsifications are now presented as execution traces with example values

2.5.1
---
* Fix keyset enforce of old keyset

2.5.0
---
* Pluggable gas model with simple fixed-rate implementation
* "Pact" continuation API support
* Allow qualified keyset predicate function names
* Allow commas to delimit list literals
* Improve cp example: authorization, load scripts
* Accept keyset name as string
* Document Pact database backend format
* Nix build support
* Break up Lang and Runtime type files
Analysis:
  * New `@model` / `@doc` syntax.
  * We now display all the argument and variable values, reads, writes,
    keysets, and function result for invalidating models.
  * Introduced `row-read-count` and `row-write-count`, specifying the number of
    times a row is read or written.
  * Table quantification: it's now possible to quantify over tables, though in
    this release you can't yet quantify over columns.
  * Object literals and `at` now work in properties. Object comparison with `=`
    and `!=` also now works.
  * Analysis now typechecks references to tables (ie it knows table schemas and
    the types you can read and write to them) and return types.
  * `verify` now typechecks `defconst`s, so it now checks a superset of
    `typecheck`.
  * We now check each invariant individually and give a separate invalidating
    model for each invariant.
  * Property parse errors are now less confusing and include location info.
  * `read-decimal` and `enforce-one` are now supported.
  * Bug fix: Analysis of `and` and `or` now (correctly) short-circuits.
  * The property checking system is now included in the documentation.
  * `defproperty`: New properties can now be defined at the module level.

2.4.1
---
* Fix support for property checker in Atom

2.4.0
---
* Preview support of of property-based formal verification
* Fix fold type signature
* Refactor ghcjs code into external project
* Various bug fixes in typechecker
* GHC 8.4 support (semigroup instances, statistics-0.14)
* "No-Leftpad" dependency management implementation
* Nested schema support


2.3.8
---
* Pact tool improvements: auto-complete, readline support, better multiline
* Fix documentation for `format` form
* Internal changes: TableName in AST, expose evalRepl', token parsing
* build improvements: weeder, hlint enforcement

2.3.7
---
* `keylog` returns rows indexed by txid

2.3.6
---
* Support for reading JSON arrays/objects to pact lists/objects
* Microsecond support in time objects
* new `keylog` function for obtaining logs by key

2.3.5
---
* Fix issue where extra parens silently ignored in repl scripts/prompt

2.3.4
---
* `pact` tool improvements: better stdin support, '-d' for debug output of load

2.3.3
---
* Bugfix for `and` regression

2.3.2
---
* ghcjs build working, typechecking not supported in browser yet
* native blake2 support, only for ghcjs
* Restore callstack, better rendering, runtime errors at top callsite


2.3.1
---
* `contains` added
* `drop`, `take` for objects
* allow empty objects
* short-circuiting `and`,`or`
* `constantly`, `identity` added


2.3.0
---
* "Pure" execution capability, used in keyset enforcement
* `enforce-one` added
* Pact tests counted in HUnit results
* defpact overhaul to support public/escrow
* New query model: `select`, `where`, `and?/or?/not?`
* `reverse` and `sort` added
* Better typechecking on partial application, TC bugfixes
* BREAKING: `format` takes list instead of var args
* BREAKING: `pact-id` replaces `pact-txid`
* Consts memoized at load
* `enforce-version` added

2.2.3
---
* Pact tool now supports '-a' to format API requests and '-g' to generate ED25519 keypairs
* Fixed error output (was breaking Atom parsing)
* Changelog removed from reference docs

2.2.2
---
* Module Hashes: `use` support
* use accepts barewords
* better output in `describe-module`
* `list-modules` added
* `print` REPL command
* "transactional awareness" in Persist

2.2.1
---
* `yield` and `resume` added for use in defpacts
* `yielded`, `sig-keyset` repl functions
* JSON defaults for keysets, better dispatch of builtin preds
* `pact-version` function added.

2.2.0
---

* Privacy API: `private` endpoint, address fields in command
* Typechecker support for `at`,`filter`,`map`,`fold`,`compose`
* Persist layer standardized for easier extension
* Library features: Interpreter module, NFData all the things, Eval uses exceptions
* Types module breakup


2.1.0
---
* "pact -serve": new REST API server for app development
* pact-lang-api.js javascript package
* `json` repl function, `read-msg` can take zero args to get entire data payload


2.0.0
---
* Support for types: inference, runtime enforcement, typechecking
* Numeric negation literals
* `deftable`, `defschema` in module definitions
* `with-keyset` now `enforce-keyset` with no body


1.0.1
---

* New function `listModules`, improved `describeModule`
* REPL function `load` with optional reset arg
