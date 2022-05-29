# Pact Core Checklist

## Essential

### Source -> Typed
- [x] Core Parsing new syntax (Expr)
- [x] Core Names (Locally nameless + uniques)
- [ ] Core Renamer
- [ ] Core IR Modules (Parsing, Tc)
- [ ] Core IR Type inference support


## Typed
- [x] Core Typed IR
- [x] Core Type language (Note: potentially `Type` should be 2 different types, for IR and Core)
- [x] Renaming to locally nameless

## Untyped

- [x] Uniques renamer
- [ ] Debruijn transform
- [x] Type inference for term
- [ ] Type inference for modules/defuns (Partally complete)
- [x] Type checking for typed Core
- [ ] Core IIR -> Core Typed IR
- [ ] Core Term Inlining

## Evaluation
- [ ] CEK for Inference IR
- [x] CEK for Typed IR
- [ ] CEK for Untyped IR

# Features
- [ ] Capabilities Support
- [ ] Defpact Support

## Low Prio

- [ ] Constant folding / Propagation
- [ ] JIT

# General Compiler flow

## Source on chain version:

Source --> Name resolution + renaming -> Typecheck (Gas!)
\+ Overload resolution -> Typed core -> Sanity typecheck -> Untyped + onchain persist + execute.

## Typed Core on chain version:

### Off-chain
Source (Frontend of choice) --> Name reso + renaming -> Typecheck (No gas :)
\-> Overload resolution -> Typed Core -> Optional (Optimization)

### On-chain
Typed Core Source --> Name resolution + renaming -> Typecheck (Gas) -> Untyped + onchain persist + execute.
