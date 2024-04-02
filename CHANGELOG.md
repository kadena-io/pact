4.13.0
---
### Features
- Support for encoding a hyperlane TokenMessage (#1362)

### Bugfixes
- Fixed support for decoding a hyperlane TokenMessage (#1362)

4.12.0
---
### Features
- Support for keccak256 native (#1354)
- Add poseidon hash alias as `hash-poseidon` (#1356)

### Bugfixes
- Fixed parsing of difftime as a property (#1349)

### Misc
- Added pact version command to verify linking (#1350)



4.11.0
---
### Features
- Support for verifier plugins (#1324,#1336)
- Support for `hyperlane-message-id` (#1335)
- Support for decoding a hyperlane TokenMessage (#1344)

### Bugfixes
- Fix bug in enforce-pact-version parser (#1334)
- Fix pact CLI usage output to use correct naming (#1331)
- Fix `step` for `defpact`s in typechecker (#1333)


### Misc
- List-modules moved to local-only (#1326)
- Update pact server typo (#1328)
- Deprecate readthedocs (#1341)

4.10.0
---
### Features
- Webauthn support in key formats (#1318)
- New `poseidon-hack-a-chain` native for the "Poseidon" hashing function (#1313)

### Bugfixes
- Gas model fix for concat (#1317)
- Fix FV `str-to-int` (#1304)

### Misc
- Vendor webauthn dependency

4.9.0
---
### Bugfixes
- Pact value infos fixed (#1287)

### Features
- Webauthn support (#1193)


4.8.0
---
### Bugfixes
- Fix name resolution within module redeploy (#1235)
- Fixed issue with the hash of cap guards, `hash` native and principals (#1273) (#1278) (#1287)
- Fixed error message for calling a non-function value (#1268)

### Eval
- Tweaked the gas of concat (#1269), reverse and format (#1271)
- Allow computation on integers less than 10^80 without gas penalty (#1272)

### Internal changes
- More granular gas units (#1245)
- Remove GHCJS support (#1280)
- GHC 9.6+ version support (#1242)
- Package docs in nix flake (#1258)
- Vendor sources of prettyprinter-1.6.0 (#1263)
- Use MTL 2.3+ (#1261)
- replace cryptonite by crypton (#1240)

### Formal Verification
- Support lambdas in static typechecker (#1232)
- Fix swapped type assignment in fold symeval (#1253)

### Other
- Improve modref docs (#1256)

Thanks to @thomashoneyman for the contribution!

4.7.1
---

- Add feature flag `FlagDisableRuntimeTypeChecking` for disabling runtime typechecking
  introduced in Pact 4.7 (#1231)

### Formal Verification

- Fixed empty list type defaulting to any (#1224)
- Fixed partial biniding (#1127)

### Tests

- Refactored test to avoid `runIO` within tests (#1129)
- Add `CoverageSpec` (#1228)
- Fix `ReplSpec` tests (#1216)
- Cleanup `PactContinuationSpec` (#1222)

### Others

- Cleanup [README.md](README.md) (#1225)
- Bump Z3 versions in GH actions (#1126)

4.7.0
---

### Eval

- Improve advice implementation (#1187)
- Fix inconsistent trace output (#1153)
- Fix coverage, restore defun advice (#1151)
- Create persistence dirs if missing (#1148)
- Differentiate between errors on-chain and off-chain (#1185)
- Don't display function documentation in non-repl context (#1185)
- Runtime type checking on return types now enforced (#1209)

### Natives

- Allow native function versioning in eval (#1195)
- Add new native `dec` for integer to decimal conversion (#1150)
- Implement `shift` in terms of other natives for better gas costing (#1208)
- Intialize body of `env-data` to an empty object instead of Null (#1188)
* `read-*` functions are now recoverable if the key doesn't exist. That is, a call such as `(read-string 'key)` is recoverable if `'key` does not exist in the data payload by surrounding it with `try`. As an example, `(try "string if key isn't present" (read-string "key"))`. This applies to
`read-integer`, `read-string`, `read-decimal`, `read-keyset` and `read-msg` (as long as `read-msg` has a key supplied as an argument).


### Typechecking

- Fix Typechecker treatment of special binding forms (#1212)

### Formal Verification

- Enable warnings during symbolic eval (#1175)
- Remove `emit-event` shim (#1168)
- Remove `hash` shims (#1158)
- Remove `enumerate` shim (#1155)
- Remove `distinct` shim (#1154)
- Remove `describe-namespace` shim (#1156)
- Remove `is-principal`, `typeof-principal`, `create-principal`, and `validate-principal` shims (#1160)
- Remove `format` shim (#1159)

### Documentation

- Remove mention of private defpacts from the reference manual (#1207)
- Add documentation for `enumerate` (#1176)

### Misc

- New PR and Issue templates introduced.

Thanks to everyone for their contributions, especially @CryptoPascal31 for his wonderful feedback and commentary, and welcome to our newest Pact team member, @ak3n!.

4.6.0
---
* Add `DisablePact46` execution flag (#1138)
* Add Zero Knowledge native operations (#1052)
* Add deprecation warning system (#1127)
* Build support for Nix flakes (#1083)
* Passage of time simulation in local pact server build (#1082)
* Fixed parsing failure message for Public Keys (#1097)
* `print` native now pretty prints output using term pretty printing (#1106)
* Typechecker:
  - now handles all const value types (#1116)
  - inlines mock dynamic references (#1120)

* Formal Verification:
  - Prevent usage of `result` fixed binding in `defpact` verification. (#1113)
  - Warning added for invaliding models when decimals are too high a
    precision (#1113)
  - Typechecker now correctly infers function type arguments (#1112)
  - Added debug flag to `verify` native, allowing outputs to be directed to
    SMTLib file on a per-module basis. (#1122)
  - Add shim for `select` in FV (#1090)

Thanks to everyone for their contributions, especially new contributors @qooboodoop and @omahs, and welcome to our newest Pact team member, @rsoeldner!.

4.4.1
---
* Export `ApiReq` constituent data structures (#1055)
* Fix test tree creation to allow tests to run faster, safer to run on a single core,
  and prevents test servers from being exposed to public environs (#1060, #1062)
* Added capability guards (#1057)
* Lazily evaluate `enforce` to reduce gas usage on evaluation (#1069)
* Gas adjustment for various natives (#1071)
* Fix FV regression that disallowed the verification of unary operators like `abs` (#1079)
* Documentation link fixes (thanks @przemeklach! #1075)

4.4
---
* Document VSCode support - thanks JeremeyJS! (#965)
* Documentation fixes - thanks JCM00N and Luzzotica! (#1003, #1021)
* Gas-related bug fixes and adjustments to `format` and `try` (#1020, #1019)
* Defined keysets are now required to be namespaced (#1011)
* Consolidated Pact 4.4 execution config flag (removes need for `DisablePreserveNamespaceUpgrade` flag, #1026)

4.3
---
* Interpreter improvements (#970)
* New `create-principal` and `validate-principal` natives (#969)
* Fixes to name resolution (#987)
* New nested defpacts + `continue` native (#980)

4.2.1
---
* Backport chainweb gasmodel (#978)
* Memory improvements (#964)
* Force eof in parseExprs (#963)
* Fix `pact add-sig` (#981)

4.2.0
---
* Language features:
  * Lambdas: let-bound function definitions (#915)
  * In-line lambdas @ application use sites: (#954)
  * Provide equality for modrefs (#951)

* Bug Fixes
  * Fixed formal verification for take/drop functions (#921)
  * Internal native fix (#945)
  * Fix schema unification on module upgrades (#948)
  * Docs expression evaluation fixed (#946)
  * Allow self modrefs (#950)

* New Natives
  * `zip` native (#922)
  * `cond` syntactic form (#938)
  * `fold-db` native (#941)

* Persistence improvements
  * Versoned PactValue persistence (#926)
  * Fix modref payload serialization, decouple from persistence backcompat (#949)
  * Run Repls with Sqlite backend (#934)

4.1.2
---
* Enforce key format in keyset creation (#918)

4.1.1
---
* Guard for recursion at runtime on user apps (#913)
* FV and TC give real errors (and warnings), improve model failure output (#904)
* Fix nested capability acquisition (#910)
* Add support for empty import lists when module hashes are present (#901)

4.1
---
* [Typechecking/FV] Support dynamic refs in TC/analyze (#894)
  * adds 'env-dynref' REPL builtin for mocking interfaces
  * improved/automatic shimming of natives in TC
* [Code Coverage] Write `coverage/lcov.info` relative to loaded file (#883)
* [Build Tooling]
  * Add build-tool flag to shrink library build (#882,#892,#891,#889)
  * Build-related (#895,#885,#877)
  * CI-related (#880,#890,#881,#884)


4.0.1
---

* Add "advice" plus LCOV code coverage (#861)
  * General advice mechanism for e.g. instrumenting performance bracketing
  * `-c` in pact tool outputs coverage to `coverage/lcov.info` relative to loaded file
* Emit synthetic cross-chain events (#875)
  * Emits `X_YIELD` on cross-chain initiation and `X_RESUME` on cross chain resumption
    for any defpact:
    - `(pact.X_YIELD targetChain:string continuationName:string continuationArgs:[*])`
    - `(pact.X_RESUME sourceChain:string continuationName:string continuationArgs:[*])`
  * Thus a cross-chain transfer send from 0 to 1 would be `(pact.X_YIELD "1" "coin.transfer-crosschain" ["Bob" "Alice" 10.0])`
    on chain 0 then `(pact.X_RESUME "0" "coin.transfer-crosschain" ["Bob" "Alice" 10.0])` on chain 1.
* Support blessed modules and add source chain in crosschain (#862)
  * Blessed modules are now supported for resuming cross-chains initiated pre-upgrade.
  * Initiation of cross-chain captures/transmits source chain in output in field `.continuation.yield.sourceChain`
* New natives (#859,#874,#860,#862)
  * `enumerate` to build sequenced lists
  * `str-to-list` and `concat` to transform strings to arrays and back
  * `distinct` to de-dupe lists
  * `emit-event` to not fire event defcaps when emitting, and allowing arbitrary parameter values
* Miscellaneous
  * Elide info from Modref JSON (#856)
  * Move SigData and related types into library (#852)
  * More robust TH and avoid deprecated use of star (#870)
  * Enable benchmarks in nix ghc build (#869)
  * Remove thyme dependency (#867)
  * Remove swagger support (#866)
  * Add time benchmarks (#868)
  * Remove setup from gas results (#865)
  * Improve gas golden output (#864)
  * Conditional z3 and servant (#850)

3.7
---
* Module References (#817,#839)
* Pact Events:
  * Backward compatible JSON (#820)
  * @event tag for capabilities (#830)
  * `env-events` for testing (#827)
* Fix runtime typechecker for lists (#825)
* `with-applied-env`, `expect-that` testing tools

3.6
---
* Pact events (#819)
* Type-only module reference support (#814,#816)
* Update SBV and other deps (#812)

3.5.2
---
* ApiReq: expose builder, migrate String -> Text (#805)
* Fix install module governance issue

3.5.1
---

* `NFData` instances for common Pact Service types (#800)
* Analysis (#798):
  * Soft support for `is-charset` (analysis does not choke on this primitive)
  * Fix column delta calculation
  * Static Cap tracing
  * Upgraded to SBV 8.6
* Additional benchmarks for 10x operations/tx (#795)
* Fix bad commandline rendering from tool on load failure (#793)
* Better sanitization of sqlite db names (#787)
* Bugfix for `base64` primitives (#788)

3.5.0
---

* Add the commands `sign`, `add-sig`, and `combine-sigs` for working with signatures (#759)
* Decode StackFrame in JSON output (#755)
* CommandResult golden coverage (#754)
* Use CFFI for aeson (upstreamed from chainweb) (#749)
* Add gas logs to eval result (#748)
* Add missing case in Eval (#764)
* Misc updates to docs
* Add "Literate Flags" in execution config (#722)
* Add flag to allow reads in local (#777)

3.4.0
---

Note this is the version that deploys with Chainweb 1.4.x, with a finalized gas model
and managed capability improvements (auto-management, autonomous management)

* Improve ApiReq for creationTime, TTL, haddocks for PublicMeta and friends (#753)
* Allow for flexible interface manager spec, and "auto-managed" caps (#746)
* Cover module persistence with golden test (#745)
* Execution config (#741)
* Fail `define-namespace` on bad atom structure (#737)
* Finalize gas model (#734)
* Use ByteString readFile and explicit UTF8 decoding (#735)
* Enable autonomous managed capabilities (#733)
* benchmark and improve rounding (#732)


3.3.1
---

* `base64-(encode|decode)` natives
* Bugfix: you can now declare user guards as constants
* keysets are now set-based so that keyset equality works regardless of key order
* Bugfix: module guards with managed caps on demand installs works properly
* Signature checks only consider acquired and pending caps
* Better docs all around

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
