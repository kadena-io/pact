# Property and Invariant Functions {#properties-and-invariants}

These are functions available in properties and invariants -- not necessarily in executable Pact code. All of these functions are available in properties, but only a subset are available in invariants. As a general rule, invariants have vocabulary for talking about the shape of data, whereas properties also add vocabulary for talking about function inputs and outputs, and database interactions. Each function also explicitly says whether it's available in just properties, or invariants as well.

## Numerical operators {#Numerical}

### \+ {#FAddition}

```lisp
(+ x y)
```

* takes `x`: _a_
* takes `y`: _a_
* produces _a_
* where _a_ is of type `integer` or `decimal`

Addition of integers and decimals.

Supported in either invariants or properties.

### \- {#FSubtraction}

```lisp
(- x y)
```

* takes `x`: _a_
* takes `y`: _a_
* produces _a_
* where _a_ is of type `integer` or `decimal`

Subtraction of integers and decimals.

Supported in either invariants or properties.

### * {#FMultiplication}

```lisp
(* x y)
```

* takes `x`: _a_
* takes `y`: _a_
* produces _a_
* where _a_ is of type `integer` or `decimal`

Multiplication of integers and decimals.

Supported in either invariants or properties.

### / {#FDivision}

```lisp
(/ x y)
```

* takes `x`: _a_
* takes `y`: _a_
* produces _a_
* where _a_ is of type `integer` or `decimal`

Division of integers and decimals.

Supported in either invariants or properties.

### ^ {#FExponentiation}

```lisp
(^ x y)
```

* takes `x`: _a_
* takes `y`: _a_
* produces _a_
* where _a_ is of type `integer` or `decimal`

Exponentiation of integers and decimals.

Supported in either invariants or properties.

### log {#FLogarithm}

```lisp
(log b x)
```

* takes `b`: _a_
* takes `x`: _a_
* produces _a_
* where _a_ is of type `integer` or `decimal`

Logarithm of `x` base `b`.

Supported in either invariants or properties.

### \- {#FNumericNegation}

```lisp
(- x)
```

* takes `x`: _a_
* produces _a_
* where _a_ is of type `integer` or `decimal`

Negation of integers and decimals.

Supported in either invariants or properties.

### sqrt {#FSquareRoot}

```lisp
(sqrt x)
```

* takes `x`: _a_
* produces _a_
* where _a_ is of type `integer` or `decimal`

Square root of integers and decimals.

Supported in either invariants or properties.

### ln {#FNaturalLogarithm}

```lisp
(ln x)
```

* takes `x`: _a_
* produces _a_
* where _a_ is of type `integer` or `decimal`

Logarithm of integers and decimals base e.

Supported in either invariants or properties.

### exp {#FExponential}

```lisp
(exp x)
```

* takes `x`: _a_
* produces _a_
* where _a_ is of type `integer` or `decimal`

Exponential of integers and decimals. e raised to the integer or decimal `x`.

Supported in either invariants or properties.

### abs {#FAbsoluteValue}

```lisp
(abs x)
```

* takes `x`: _a_
* produces _a_
* where _a_ is of type `integer` or `decimal`

Absolute value of integers and decimals.

Supported in either invariants or properties.

### round {#FBankersRound}

```lisp
(round x)
```

* takes `x`: `decimal`
* produces `integer`

```lisp
(round x prec)
```

* takes `x`: `decimal`
* takes `prec`: `integer`
* produces `integer`

Banker's rounding value of decimal `x` as integer, or to `prec` precision as decimal.

Supported in either invariants or properties.

### ceiling {#FCeilingRound}

```lisp
(ceiling x)
```

* takes `x`: `decimal`
* produces `integer`

```lisp
(ceiling x prec)
```

* takes `x`: `decimal`
* takes `prec`: `integer`
* produces `integer`

Rounds the decimal `x` up to the next integer, or to `prec` precision as decimal.

Supported in either invariants or properties.

### floor {#FFloorRound}

```lisp
(floor x)
```

* takes `x`: `decimal`
* produces `integer`

```lisp
(floor x prec)
```

* takes `x`: `decimal`
* takes `prec`: `integer`
* produces `integer`

Rounds the decimal `x` down to the previous integer, or to `prec` precision as decimal.

Supported in either invariants or properties.

### mod {#FModulus}

```lisp
(mod x y)
```

* takes `x`: `integer`
* takes `y`: `integer`
* produces `integer`

Integer modulus

Supported in either invariants or properties.

## Bitwise operators {#Bitwise}

### & {#FBitwiseAnd}

```lisp
(& x y)
```

* takes `x`: `integer`
* takes `y`: `integer`
* produces `integer`

Bitwise and

Supported in either invariants or properties.

### | {#FBitwiseOr}

```lisp
(| x y)
```

* takes `x`: `integer`
* takes `y`: `integer`
* produces `integer`

Bitwise or

Supported in either invariants or properties.

### xor {#FXor}

```lisp
(xor x y)
```

* takes `x`: `integer`
* takes `y`: `integer`
* produces `integer`

Bitwise exclusive-or

Supported in either invariants or properties.

### shift {#FShift}

```lisp
(shift x y)
```

* takes `x`: `integer`
* takes `y`: `integer`
* produces `integer`

Shift `x` `y` bits left if `y` is positive, or right by `-y` bits otherwise.

Supported in either invariants or properties.

### ~ {#FComplement}

```lisp
(~ x)
```

* takes `x`: `integer`
* produces `integer`

Reverse all bits in `x`

Supported in either invariants or properties.

## Logical operators {#Logical}

### > {#FGreaterThan}

```lisp
(> x y)
```

* takes `x`: _a_
* takes `y`: _a_
* produces `bool`
* where _a_ is of type `integer` or `decimal`

True if `x` > `y`

Supported in either invariants or properties.

### < {#FLessThan}

```lisp
(< x y)
```

* takes `x`: _a_
* takes `y`: _a_
* produces `bool`
* where _a_ is of type `integer` or `decimal`

True if `x` < `y`

Supported in either invariants or properties.

### >= {#FGreaterThanOrEqual}

```lisp
(>= x y)
```

* takes `x`: _a_
* takes `y`: _a_
* produces `bool`
* where _a_ is of type `integer` or `decimal`

True if `x` >= `y`

Supported in either invariants or properties.

### <= {#FLessThanOrEqual}

```lisp
(<= x y)
```

* takes `x`: _a_
* takes `y`: _a_
* produces `bool`
* where _a_ is of type `integer` or `decimal`

True if `x` <= `y`

Supported in either invariants or properties.

### = {#FEquality}

```lisp
(= x y)
```

* takes `x`: _a_
* takes `y`: _a_
* produces `bool`
* where _a_ is of type `integer`, `decimal`, `string`, `time`, `bool`, `object`,  or `keyset`

True if `x` = `y`

Supported in either invariants or properties.

### != {#FInequality}

```lisp
(!= x y)
```

* takes `x`: _a_
* takes `y`: _a_
* produces `bool`
* where _a_ is of type `integer`, `decimal`, `string`, `time`, `bool`, `object`,  or `keyset`

True if `x` != `y`

Supported in either invariants or properties.

### and {#FLogicalConjunction}

```lisp
(and x y)
```

* takes `x`: `bool`
* takes `y`: `bool`
* produces `bool`

Short-circuiting logical conjunction

Supported in either invariants or properties.

### or {#FLogicalDisjunction}

```lisp
(or x y)
```

* takes `x`: `bool`
* takes `y`: `bool`
* produces `bool`

Short-circuiting logical disjunction

Supported in either invariants or properties.

### not {#FLogicalNegation}

```lisp
(not x)
```

* takes `x`: `bool`
* produces `bool`

Logical negation

Supported in either invariants or properties.

### when {#FLogicalImplication}

```lisp
(when x y)
```

* takes `x`: `bool`
* takes `y`: `bool`
* produces `bool`

Logical implication. Equivalent to `(or (not x) y)`.

Supported in either invariants or properties.

### and? {#FAndQ}

```lisp
(and? f g a)
```

* takes `f`: _a_ -> `bool`
* takes `g`: _a_ -> `bool`
* takes `a`: _a_
* produces `bool`

`and` the results of applying both `f` and `g` to `a`

Supported in either invariants or properties.

### or? {#FOrQ}

```lisp
(or? f g a)
```

* takes `f`: _a_ -> `bool`
* takes `g`: _a_ -> `bool`
* takes `a`: _a_
* produces `bool`

`or` the results of applying both `f` and `g` to `a`

Supported in either invariants or properties.

### hash {#FBoolHash}

```lisp
(hash s)
```

* takes `s`: `bool`
* produces `string`

BLAKE2b 256-bit hash of bool values

Supported in properties only.

## Object operators {#Object}

### at {#FObjectProjection}

```lisp
(at k o)
```

* takes `k`: `string`
* takes `o`: `object`
* produces _a_

```lisp
(at i l)
```

* takes `i`: `integer`
* takes `o`: `list`
* produces `bool`

projection

Supported in either invariants or properties.

### \+ {#FObjectMerge}

```lisp
(+ x y)
```

* takes `x`: `object`
* takes `y`: `object`
* produces `object`

Object merge

Supported in either invariants or properties.

### drop {#FObjectDrop}

```lisp
(drop keys o)
```

* takes `keys`: [`string`]
* takes `o`: `object`
* produces `object`

drop entries having the specified keys from an object

Supported in either invariants or properties.

### take {#FObjectTake}

```lisp
(take keys o)
```

* takes `keys`: [`string`]
* takes `o`: `object`
* produces `object`

take entries having the specified keys from an object

Supported in either invariants or properties.

### length {#FObjectLength}

```lisp
(length o)
```

* takes `o`: `object`
* produces `integer`

the number of key-value pairs in the object

Supported in either invariants or properties.

## List operators {#List}

### at {#FListProjection}

```lisp
(at k l)
```

* takes `k`: `string`
* takes `l`: [_a_]
* produces _a_

```lisp
(at i l)
```

* takes `i`: `integer`
* takes `o`: `list`
* produces `bool`

projection

Supported in either invariants or properties.

### length {#FListLength}

```lisp
(length s)
```

* takes `s`: [_a_]
* produces `integer`

List length

Supported in either invariants or properties.

### contains {#FContains}

```lisp
(contains x xs)
```

* takes `x`: _a_
* takes `xs`: [_a_]
* produces `bool`

```lisp
(contains k o)
```

* takes `k`: `string`
* takes `o`: `object`
* produces `bool`

```lisp
(contains value string)
```

* takes `value`: `string`
* takes `string`: `string`
* produces `bool`

List / string / object contains

Supported in either invariants or properties.

### enumerate {#FEnumerate}

```lisp
(enumerate from to step)
```

* takes `from`: `integer`
* takes `to`: `integer`
* takes `step`: `integer`
* produces [`integer`]

Returns a sequence of numbers as a list

Supported in either invariants or properties.

### reverse {#FReverse}

```lisp
(reverse xs)
```

* takes `xs`: [_a_]
* produces [_a_]

reverse a list of values

Supported in either invariants or properties.

### sort {#FSort}

```lisp
(sort xs)
```

* takes `xs`: [_a_]
* produces [_a_]

sort a list of values

Supported in either invariants or properties.

### drop {#FListDrop}

```lisp
(drop n xs)
```

* takes `n`: `integer`
* takes `xs`: [_a_]
* produces [_a_]

drop the first `n` values from the beginning of a list (or the end if `n` is negative)

Supported in either invariants or properties.

### take {#FListTake}

```lisp
(take n xs)
```

* takes `n`: `integer`
* takes `xs`: [_a_]
* produces [_a_]

take the first `n` values from `xs` (taken from the end if `n` is negative)

Supported in either invariants or properties.

### make-list {#FMakeList}

```lisp
(make-list n a)
```

* takes `n`: `integer`
* takes `a`: _a_
* produces [_a_]

create a new list with `n` copies of `a`

Supported in either invariants or properties.

### map {#FMap}

```lisp
(map f as)
```

* takes `f`: _a_ -> _b_
* takes `as`: [_a_]
* produces [_b_]

apply `f` to each element in a list

Supported in either invariants or properties.

### filter {#FFilter}

```lisp
(filter f as)
```

* takes `f`: _a_ -> `bool`
* takes `as`: [_a_]
* produces [_a_]

filter a list by keeping the values for which `f` returns `true`

Supported in either invariants or properties.

### distinct {#FDistinct}

```lisp
(distinct xs)
```

* takes `xs`: [_a_]
* produces [_a_]

returns a list of distinct values

Supported in either invariants or properties.

### fold {#FFold}

```lisp
(fold f a bs)
```

* takes `f`: _a_ -> _b_ -> _a_
* takes `a`: _a_
* takes `bs`: [_b_]
* produces [_a_]

reduce a list by applying `f` to each element and the previous result

Supported in either invariants or properties.

### hash {#FListHash}

```lisp
(hash xs)
```

* takes `xs`: [_a_]
* produces `string`
* where _a_ is of type `integer`, `decimal`, `bool`,  or `string`

BLAKE2b 256-bit hash of lists

Supported in properties only.

## String operators {#String}

### length {#FStringLength}

```lisp
(length s)
```

* takes `s`: `string`
* produces `integer`

String length

Supported in either invariants or properties.

### \+ {#FConcatenation}

```lisp
(+ s t)
```

* takes `s`: `string`
* takes `t`: `string`
* produces `string`

```lisp
(+ s t)
```

* takes `s`: [_a_]
* takes `t`: [_a_]
* produces [_a_]

String / list concatenation

Supported in either invariants or properties.

### str-to-int {#FStringToInteger}

```lisp
(str-to-int s)
```

* takes `s`: `string`
* produces `integer`

```lisp
(str-to-int b s)
```

* takes `b`: `integer`
* takes `s`: `string`
* produces `integer`

String to integer conversion

Supported in either invariants or properties.

### take {#FStringTake}

```lisp
(take n s)
```

* takes `n`: `integer`
* takes `s`: `string`
* produces `string`

take the first `n` values from `xs` (taken from the end if `n` is negative)

Supported in either invariants or properties.

### drop {#FStringDrop}

```lisp
(drop n s)
```

* takes `n`: `integer`
* takes `s`: `string`
* produces `string`

drop the first `n` values from `xs` (dropped from the end if `n` is negative)

Supported in either invariants or properties.

### hash {#FStringHash}

```lisp
(hash s)
```

* takes `s`: `string`
* produces `string`

BLAKE2b 256-bit hash of string values

Supported in properties only.

### hash {#FNumericalHash}

```lisp
(hash s)
```

* takes `s`: _a_
* produces `string`
* where _a_ is of type `integer` or `decimal`

BLAKE2b 256-bit hash of numerical values

Supported in properties only.

## Temporal operators {#Temporal}

### add-time {#FTemporalAddition}

```lisp
(add-time t s)
```

* takes `t`: `time`
* takes `s`: _a_
* produces `time`
* where _a_ is of type `integer` or `decimal`

Add seconds to a time

Supported in either invariants or properties.

## Quantification operators {#Quantification}

### forall {#FUniversalQuantification}

```lisp
(forall (x:string) y)
```

* binds `x`: _a_
* takes `y`: _r_
* produces _r_
* where _a_ is _any type_
* where _r_ is _any type_

Bind a universally-quantified variable

Supported in properties only.

### exists {#FExistentialQuantification}

```lisp
(exists (x:string) y)
```

* binds `x`: _a_
* takes `y`: _r_
* produces _r_
* where _a_ is _any type_
* where _r_ is _any type_

Bind an existentially-quantified variable

Supported in properties only.

### column-of {#FColumnOf}

```lisp
(column-of t)
```

* takes `t`: `table`
* produces `type`

The *type* of `column`s for a given `table`. Commonly used in conjunction with quantification; e.g.: `(exists (col:(column-of accounts)) (column-written accounts col))`.

Supported in properties only.

## Transactional operators {#Transactional}

### abort {#FTransactionAborts}

```lisp
abort
```

* of type `bool`

Whether the transaction aborts. This function is only useful when expressing propositions that do not assume transaction success. Propositions defined via `property` implicitly assume transaction success. We will be adding a new mode in which to use this feature in the future -- please let us know if you need this functionality.

Supported in properties only.

### success {#FTransactionSucceeds}

```lisp
success
```

* of type `bool`

Whether the transaction succeeds. This function is only useful when expressing propositions that do not assume transaction success. Propositions defined via `property` implicitly assume transaction success. We will be adding a new mode in which to use this feature in the future -- please let us know if you need this functionality.

Supported in properties only.

### governance-passes {#FGovernancePasses}

```lisp
governance-passes
```

* of type `bool`

Whether the governance predicate passes. For keyset-based governance, this is the same as something like `(authorized-by 'governance-ks-name)`. Pact's property checking system currently does not analyze the body of a capability when it is used for governance due to challenges around capabilities making DB modifications -- the system currently assumes that a capability-based governance predicate is equally capable of succeeding or failing. This feature allows describing the scenarios where the predicate passes or fails.

Supported in properties only.

### result {#FFunctionResult}

```lisp
result
```

* of type _r_
* where _r_ is _any type_

The return value of the function under test

Supported in properties only.

## Database operators {#Database}

### table-written {#FTableWritten}

```lisp
(table-written t)
```

* takes `t`: _a_
* produces `bool`
* where _a_ is of type `table` or `string`

Whether a table is written in the function under analysis

Supported in properties only.

### table-read {#FTableRead}

```lisp
(table-read t)
```

* takes `t`: _a_
* produces `bool`
* where _a_ is of type `table` or `string`

Whether a table is read in the function under analysis

Supported in properties only.

### cell-delta {#FCellDelta}

```lisp
(cell-delta t c r)
```

* takes `t`: _a_
* takes `c`: _b_
* takes `r`: `string`
* produces _c_
* where _a_ is of type `table` or `string`
* where _b_ is of type `column` or `string`
* where _c_ is of type `integer` or `decimal`

The difference in a cell's value before and after the transaction

Supported in properties only.

### column-delta {#FColumnDelta}

```lisp
(column-delta t c)
```

* takes `t`: _a_
* takes `c`: _b_
* produces _c_
* where _a_ is of type `table` or `string`
* where _b_ is of type `column` or `string`
* where _c_ is of type `integer` or `decimal`

The difference in a column's total summed value before and after the transaction

Supported in properties only.

### column-written {#FColumnWritten}

```lisp
(column-written t c)
```

* takes `t`: _a_
* takes `c`: _b_
* produces `bool`
* where _a_ is of type `table` or `string`
* where _b_ is of type `column` or `string`

Whether a column is written to in a transaction

Supported in properties only.

### column-read {#FColumnRead}

```lisp
(column-read t c)
```

* takes `t`: _a_
* takes `c`: _b_
* produces `bool`
* where _a_ is of type `table` or `string`
* where _b_ is of type `column` or `string`

Whether a column is read from in a transaction

Supported in properties only.

### row-read {#FRowRead}

```lisp
(row-read t r)
```

* takes `t`: _a_
* takes `r`: `string`
* produces `bool`
* where _a_ is of type `table` or `string`

Whether a row is read in the function under analysis

Supported in properties only.

### row-written {#FRowWritten}

```lisp
(row-written t r)
```

* takes `t`: _a_
* takes `r`: `string`
* produces `bool`
* where _a_ is of type `table` or `string`

Whether a row is written in the function under analysis

Supported in properties only.

### row-read-count {#FRowReadCount}

```lisp
(row-read-count t r)
```

* takes `t`: _a_
* takes `r`: `string`
* produces `integer`
* where _a_ is of type `table` or `string`

The number of times a row is read during a transaction

Supported in properties only.

### row-write-count {#FRowWriteCount}

```lisp
(row-write-count t r)
```

* takes `t`: _a_
* takes `r`: `string`
* produces `integer`
* where _a_ is of type `table` or `string`

The number of times a row is written during a transaction

Supported in properties only.

### row-exists {#FRowExists}

```lisp
(row-exists t r time)
```

* takes `t`: _a_
* takes `r`: `string`
* takes `time`: one of {"before","after"}
* produces `bool`
* where _a_ is of type `table` or `string`

Whether a row exists before or after a transaction

Supported in properties only.

### read {#FPropRead}

```lisp
(read t r)
```

* takes `t`: _a_
* takes `r`: `string`
* takes `time`: one of {"before","after"}
* produces `object`
* where _a_ is of type `table` or `string`

The value of a read before or after a transaction

Supported in properties only.

## Authorization operators {#Authorization}

### authorized-by {#FAuthorizedBy}

```lisp
(authorized-by k)
```

* takes `k`: `string`
* produces `bool`

Whether the named keyset/guard is satisfied by the executing transaction

Supported in properties only.

### row-enforced {#FRowEnforced}

```lisp
(row-enforced t c r)
```

* takes `t`: _a_
* takes `c`: _b_
* takes `r`: `string`
* produces `bool`
* where _a_ is of type `table` or `string`
* where _b_ is of type `column` or `string`

Whether the keyset in the row is enforced by the function under analysis

Supported in properties only.

## Function operators {#Function}

### identity {#FIdentity}

```lisp
(identity a)
```

* takes `a`: _a_
* produces _a_
* where _a_ is of type `table` or `string`

identity returns its argument unchanged

Supported in either invariants or properties.

### constantly {#FConstantly}

```lisp
(constantly a)
```

* takes `a`: _a_
* takes `b`: _b_
* produces _a_

constantly returns its first argument, ignoring the second

Supported in either invariants or properties.

### compose {#FCompose}

```lisp
(compose f g)
```

* takes `f`: _a_ -> _b_
* takes `g`: _b_ -> _c_
* produces _c_

compose two functions

Supported in either invariants or properties.

## Other operators {#Other}

### where {#FWhere}

```lisp
(where field f obj)
```

* takes `field`: `string`
* takes `f`: _a_ -> `bool`
* takes `obj`: `object`
* produces `bool`

utility for use in `filter` and `select` applying `f` to `field` in `obj`

Supported in either invariants or properties.

### typeof {#FTypeof}

```lisp
(typeof a)
```

* takes `a`: _a_
* produces `string`

return the type of `a` as a string

Supported in either invariants or properties.

