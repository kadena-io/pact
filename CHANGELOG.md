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
