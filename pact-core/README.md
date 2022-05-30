# Pact Core Checklist

## Essential

### Source -> Typed
- [x] Parsing new syntax (Term)
- [x] Renamer to locally nameless for terms
- [x] Type inference for Term
- [ ] Core IR Modules (Parsing, Tc)
- [ ] Core IR Type inference support
- [x] IR to Typed Core in typechecker
- [ ] Typeclass overload resolution

#### Optional
- [ ] Interpreter for IR


## Typed
- [x] Core Typed IR
- [x] Core Type language (Note: potentially `Type` should be 2 different types, for IR and Core)
- [x] Renaming to locally nameless
- [x] Type checking for typed Core
- [x] CEK Interpreter for Typed corew

## Untyped

- [ ] CEK for untyped core
- [ ] Optimizations/Jit

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
