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

## Object operators {#Object}

### at {#FObjectProjection}

```lisp
(at k o)
```

* takes `k`: `string`
* takes `o`: `object`
* produces `bool`

Object projection

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

## String operators {#String}

### length {#FStringLength}

```lisp
(length s)
```

* takes `s`: `string`
* produces `integer`

String length

Supported in either invariants or properties.

### \+ {#FStringConcatenation}

```lisp
(+ s t)
```

* takes `s`: `string`
* takes `t`: `string`
* produces `string`

String concatenation

Supported in either invariants or properties.

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
* takes `time`: one of {"before", "after"}
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
* takes `time`: one of {"before", "after"}
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

Whether the named keyset is enforced by the function under analysis

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

