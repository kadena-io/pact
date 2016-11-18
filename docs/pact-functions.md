## General {#General}

### at {#at}

Args: `idx a`

Index list A at IDX, or get value with key IDX from object A. 
```lisp
pact> (at 1 [1 2 3])
2
pact> (at "bar" { "foo": 1, "bar": 2 })
2
```


### bind {#bind}

Args: `src bindings body`

Evaluate SRC which must return an object, using BINDINGS to bind variables to values in the result. 
```lisp
pact> (bind { "a": 1, "b": 2 } { "a" := a-value } a-value)
1
```


### compose {#compose}

Args: `apps value`

Compose APPS left-to-right, such that element 1 operates on VALUE, 2 on the result, etc.
```lisp
pact> (filter (compose (length) (< 2)) ["my" "dog" "has" "fleas"])
["dog" "has" "fleas"]
```


### drop {#drop}

Args: `count list`

Drop COUNT values from LIST (or string). If negative, drop from end.
```lisp
pact> (drop 2 "vwxyz")
"xyz"
pact> (drop (- 2) [1 2 3 4 5])
[1 2 3]
```


### enforce {#enforce}

Args: `test msg`

Fail transaction with MSG if TEST fails, or returns true. 
```lisp
pact> (enforce (!= (+ 2 2) 4) "Chaos reigns")
<interactive>:1:0:Failure: Chaos reigns
```


### filter {#filter}

Args: `app list`

Filter LIST by applying APP to each element to get a boolean determining inclusion.
```lisp
pact> (filter (compose (length) (< 2)) ["my" "dog" "has" "fleas"])
["dog" "has" "fleas"]
```


### fold {#fold}

Args: `app init list`

Iteratively reduce LIST by applying APP to last result and element, starting with INIT. 
```lisp
pact> (fold (+) 0 [100 10 5])
115
```


### format {#format}

Args: `template vars`

Interpolate VARS into TEMPLATE using {}. 
```lisp
pact> (format "My {} has {}" "dog" "fleas")
"My dog has fleas"
```


### if {#if}

Args: `cond then else`

Test COND, if true evaluate THEN, otherwise evaluate ELSE. 
```lisp
pact> (if (= (+ 2 2) 4) "Sanity prevails" "Chaos reigns")
"Sanity prevails"
```


### is-bool {#is-bool}

Args: `val`

Return VAL, enforcing boolean type. 
```lisp
pact> (is-bool true)
true
```


### is-decimal {#is-decimal}

Args: `val`

Return VAL, enforcing decimal type. 
```lisp
pact> (is-decimal 123.45)
123.45
```


### is-integer {#is-integer}

Args: `val`

Return VAL, enforcing integer type 
```lisp
pact> (is-integer 123)
123
pact> (is-integer "abc")
<interactive>:0:0:Not integer: "abc"
```


### is-string {#is-string}

Args: `val`

Return VAL, enforcing string type. 
```lisp
pact> (is-string 123)
<interactive>:0:0:Not string: 123
pact> (is-string "abc")
"abc"
```


### is-time {#is-time}

Args: `val`

Return VAL, enforcing time type. 
```lisp
pact> (is-time (time "2016-07-22T11:26:35Z"))
"2016-07-22T11:26:35Z"
```


### length {#length}

Args: `a`

Compute length of A, which can be a list, a string, or an object.
```lisp
pact> (length [1 2 3])
3
pact> (length "abcdefgh")
8
pact> (length { "a": 1, "b": 2 })
2
```


### list {#list}

Args: `elems`

Create list from ELEMS. 
```lisp
pact> (list 1 2 3)
[1 2 3]
```


### list-modules {#list-modules}

List modules available for loading.


### map {#map}

Args: `app list`

Apply elements in LIST as last arg to APP, returning list of results. 
```lisp
pact> (map (+ 1) [1 2 3])
[2 3 4]
```


### pact-txid {#pact-txid}

Return reference tx id for pact execution.


### read-decimal {#read-decimal}

Args: `key`

Parse KEY string value from message data body as decimal.
```lisp
(defun exec ()
   (transfer (read-msg "from") (read-msg "to") (read-decimal "amount")))
```


### read-integer {#read-integer}

Args: `key`

Parse KEY string value from message data body as integer. 
```lisp
(read-integer "age")
```


### read-msg {#read-msg}

Args: `key`

Read KEY from message data body. Will recognize JSON types as corresponding Pact type.
```lisp
(defun exec ()
   (transfer (read-msg "from") (read-msg "to") (read-decimal "amount")))
```


### remove {#remove}

Args: `key object`

Remove entry for KEY from OBJECT. 
```lisp
pact> (remove "bar" { "foo": 1, "bar": 2 })
{"foo": 1}
```


### take {#take}

Args: `count list`

Take COUNT values from LIST (or string). If negative, take from end. 
```lisp
pact> (take 2 "abcd")
"ab"
pact> (take (- 3) [1 2 3 4 5])
[3 4 5]
```


### typeof {#typeof}

Args: `a`

Returns type of A as string. 
```lisp
pact> (typeof "hello")
"string"
```

## Database {#Database}

### create-table {#create-table}

Args: `table module`

Create table TABLE guarded by module MODULE. 
```lisp
(create-table 'accounts 'accounts-admin)
```


### describe-keyset {#describe-keyset}

Args: `keyset`

Get metadata for KEYSET


### describe-module {#describe-module}

Args: `module`

Get metadata for MODULE


### describe-table {#describe-table}

Args: `table`

Get metadata for TABLE


### insert {#insert}

Args: `table key object`

Write entry in TABLE for KEY of OBJECT column data, failing if data already exists for KEY.
```lisp
(insert 'accounts { "balance": 0.0, "note": "Created account." })
```


### keys {#keys}

Args: `table`

Return all keys in TABLE. 
```lisp
(keys 'accounts)
```


### read {#read}

Args: `table key colnames...`

Read row from TABLE for KEY returning object of COLNAMES mapped to values, or entire record if not specified. 
```lisp
(read 'accounts id 'balance 'ccy)
```


### txids {#txids}

Args: `table txid`

Return all txid values greater than or equal to TXID in TABLE. 
```lisp
(txids 'accounts 123849535)
```


### txlog {#txlog}

Args: `table txid`

Return all updates to TABLE performed in transaction TXID. 
```lisp
(txlog 'accounts 123485945)
```


### update {#update}

Args: `table key object`

Write entry in TABLE for KEY of OBJECT column data, failing if data does not exist for KEY.
```lisp
(update 'accounts { "balance": (+ bal amount), "change": amount, "note": "credit" })
```


### with-default-read {#with-default-read}

Args: `table key defaults bindings body`

Read row from TABLE for KEY and bind columns per BINDINGS over BODY. If row not found, read columns from DEFAULTS, an object with matching key names. 
```lisp
(with-default-read 'accounts id { "balance": 0, "ccy": "USD" } { "balance":= bal, "ccy":= ccy }
   (format "Balance for {} is {} {}" id bal ccy))
```


### with-read {#with-read}

Args: `table key bindings body`

Read row from TABLE for KEY and bind columns per BINDINGS over BODY.
```lisp
(with-read 'accounts id { "balance":= bal, "ccy":= ccy }
   (format "Balance for {} is {} {}" id bal ccy))
```


### write {#write}

Args: `table key object`

Write entry in TABLE for KEY of OBJECT column data.
```lisp
(write 'accounts { "balance": 100.0 })
```

## Time {#Time}

### add-time {#add-time}

Args: `time seconds`

Add SECONDS to TIME; SECONDS can be integer or decimal. 
```lisp
pact> (add-time (time "2016-07-22T12:00:00Z") 15)
"2016-07-22T12:00:15Z"
```


### days {#days}

Args: `n`

N days, for use with 'add-time' 
```lisp
pact> (add-time (time "2016-07-22T12:00:00Z") (days 1))
"2016-07-23T12:00:00Z"
```


### diff-time {#diff-time}

Args: `time1 time2`

Compute difference between TIME1 and TIME2 in seconds. 
```lisp
pact> (diff-time (parse-time "%T" "16:00:00") (parse-time "%T" "09:30:00"))
23400
```


### hours {#hours}

Args: `n`

N hours, for use with 'add-time' 
```lisp
pact> (add-time (time "2016-07-22T12:00:00Z") (hours 1))
"2016-07-22T13:00:00Z"
```


### minutes {#minutes}

Args: `n`

N minutes, for use with 'add-time'. 
```lisp
pact> (add-time (time "2016-07-22T12:00:00Z") (minutes 1))
"2016-07-22T12:01:00Z"
```


### parse-time {#parse-time}

Args: `format utcval`

Construct time from UTCVAL using FORMAT. See [strftime docs](https://www.gnu.org/software/libc/manual/html_node/Formatting-Calendar-Time.html#index-strftime) for format info. 
```lisp
pact> (parse-time "%F" "2016-09-12")
"2016-09-12T00:00:00Z"
```


### time {#time}

Args: `utcval`

Construct time from UTCVAL using ISO8601 format (%Y-%m-%dT%H:%M:%SZ). 
```lisp
pact> (time "2016-07-22T11:26:35Z")
"2016-07-22T11:26:35Z"
```

## Operators {#Operators}

### != {#bangeq}

Args: `a b`

True if a does not equal b. 
```lisp
pact> (!= "hello" "goodbye")
true
```


### * {#star}

Args: `a b`

Multiply A by B. 
```lisp
pact> (* 0.5 10.0)
5
pact> (* 3 5)
15
```


### \+ {#plus}

Args: `a b`

Add numbers, concatenate strings/lists, or merge objects. 
```lisp
pact> (+ 1 2)
3
pact> (+ 5.0 0.5)
5.5
pact> (+ "every" "body")
"everybody"
pact> (+ [1 2] [3 4])
[1 2 3 4]
pact> (+ { "foo": 100 } { "foo": 1, "bar": 2 })
{"bar": 2, "foo": 100}
```


### \- {#minus}

Args: `a b`

Negate A, or subtract A from B. 
```lisp
pact> (- 1.0)
-1.0
pact> (- 3 2)
1
```


### / {#slash}

Args: `a b`

Divide A by B. 
```lisp
pact> (/ 10.0 2.0)
5
pact> (/ 8 3)
2
```


### < {#lt}

Args: `a b`

True if A < B. 
```lisp
pact> (< 1 3)
true
pact> (< 5.24 2.52)
false
pact> (< "abc" "def")
true
```


### <= {#lteq}

Args: `a b`

True if A <= B. 
```lisp
pact> (<= 1 3)
true
pact> (<= 5.24 2.52)
false
pact> (<= "abc" "def")
true
```


### = {#eq}

Args: `a b`

True if a equals b. 
```lisp
pact> (= [1 2 3] [1 2 3])
true
pact> (= 'foo "foo")
true
pact> (= { 1: 2 } { 1: 2})
true
```


### > {#gt}

Args: `a b`

True if A > B. 
```lisp
pact> (> 1 3)
false
pact> (> 5.24 2.52)
true
pact> (> "abc" "def")
false
```


### >= {#gteq}

Args: `a b`

True if A >= B. 
```lisp
pact> (>= 1 3)
false
pact> (>= 5.24 2.52)
true
pact> (>= "abc" "def")
false
```


### ^ {#hat}

Args: `a b`

Raise A to B power. 
```lisp
pact> (^ 2 3)
8
```


### abs {#abs}

Args: `a`

Absolute value of A. 
```lisp
pact> (abs (- 10 23))
13
```


### and {#and}

Args: `a b`

Boolean logic. 
```lisp
pact> (and true false)
false
```


### ceiling {#ceiling}

Args: `a prec`

Rounds up value of decimal A as integer, or to PREC precision as decimal. 
```lisp
pact> (ceiling 3.5)
4
pact> (ceiling 100.15234 2)
100.16
```


### exp {#exp}

Args: `a`

Exp of A 
```lisp
pact> (round (exp 3) 6)
20.085537
```


### floor {#floor}

Args: `a prec`

Rounds down value of decimal A as integer, or to PREC precision as decimal. 
```lisp
pact> (floor 3.5)
3
pact> (floor 100.15234 2)
100.15
```


### ln {#ln}

Args: `a`

Natural log of A. 
```lisp
pact> (round (ln 60) 6)
4.094345
```


### log {#log}

Args: `a b`

Log of B base A. 
```lisp
pact> (log 2 256)
8
```


### mod {#mod}

Args: `a b`

A modulo B. 
```lisp
pact> (mod 13 8)
5
```


### not {#not}

Args: `a`

Boolean logic. 
```lisp
pact> (not (> 1 2))
true
```


### or {#or}

Args: `a b`

Boolean logic. 
```lisp
pact> (or true false)
true
```


### round {#round}

Args: `a prec`

Performs Banker's rounding value of decimal A as integer, or to PREC precision as decimal. 
```lisp
pact> (round 3.5)
4
pact> (round 100.15234 2)
100.15
```


### sqrt {#sqrt}

Args: `a`

Square root of A. 
```lisp
pact> (sqrt 25)
5
```

## KeySets {#KeySets}

### define-keyset {#define-keyset}

Args: `name keyset`

Define keyset as NAME with KEYSET. If keyset NAME already exists, keyset will be enforced before updating to new value.
```lisp
(define-keyset 'admin-keyset (read-keyset "keyset"))
```


### keys-2 {#keys-2}

Args: `count matched`

Keyset predicate function to match at least 2 keys in keyset. 
```lisp
pact> (keys-2 3 1)
false
```


### keys-all {#keys-all}

Args: `count matched`

Keyset predicate function to match all keys in keyset. 
```lisp
pact> (keys-all 3 3)
true
```


### keys-any {#keys-any}

Args: `count matched`

Keyset predicate function to match all keys in keyset. 
```lisp
pact> (keys-any 10 1)
true
```


### read-keyset {#read-keyset}

Args: `key`

Read KEY from message data body as keyset ({ "keys": KEYLIST, "pred": PREDFUN }). PREDFUN should resolve to a keys predicate. 
```lisp
(read-keyset "admin-keyset")
```


### with-keyset {#with-keyset}

Args: `keyset-or-name body`

Enforce KEYSET-OR-NAME against message keys to run BODY. KEYSET-OR-NAME can be a symbol of a keyset name or a keyset object. 
```lisp
(with-keyset 'admin-keyset ...)
(with-keyset (read-keyset "keyset") ...)
```

## REPL-only functions {#repl-lib}

The following functions are loaded magically in the interactive REPL, or in script files with a `.repl` extension. They are not available for blockchain-based execution.


### begin-tx {#begin-tx}

Args: `name`

Begin transaction with optional NAME. 
```lisp
(begin-tx "load module")
```


### bench {#bench}

Args: `exprs`

Benchmark execution of EXPRS. 
```lisp
(bench (+ 1 2))
```


### commit-tx {#commit-tx}

Commit transaction. 
```lisp
(commit-tx)
```


### env-data {#env-data}

Args: `json`

Set transaction JSON data, either as encoded string, or as pact types coerced to JSON. 
```lisp
pact> (env-data { "keyset": { "keys": ["my-key" "admin-key"], "pred": "keys-any" } })
"Setting transaction data"
```


### env-entity {#env-entity}

Args: `entity`

Set environment confidential ENTITY id. 
```lisp
(env-entity "my-org")
```


### env-keys {#env-keys}

Args: `keys...`

Set transaction signature KEYS. 
```lisp
pact> (env-keys "my-key" "admin-key")
"Setting transaction keys"
```


### env-step {#env-step}

Args: `step-idx rollback`

Modify pact step state. With no arguments, unset step. STEP-IDX sets step index for current pact execution, ROLLBACK defaults to false. 
```lisp
(env-step 1)
(env-step 0 true)
```


### expect {#expect}

Args: `doc expected actual`

Evaluate ACTUAL and verify that it equals EXPECTED. 
```lisp
pact> (expect "Sanity prevails." 4 (+ 2 2))
"Expect: success: Sanity prevails."
```


### expect-failure {#expect-failure}

Args: `doc actual`

Evaluate ACTUAL and succeed only if it throws an error. 
```lisp
pact> (expect-failure "Enforce fails on false" (enforce false "Expected error"))
"Expect failure: success: Enforce fails on false"
```


### load {#load}

Args: `file reset`

Load and evaluate FILE, resetting repl state beforehand if optional NO-RESET is true. 
```lisp
(load "accounts.repl")
```


### rollback-tx {#rollback-tx}

Rollback transaction. 
```lisp
(rollback-tx)
```

