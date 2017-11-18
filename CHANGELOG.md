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
