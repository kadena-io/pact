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
