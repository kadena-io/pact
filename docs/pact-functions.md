## General {#General}

### at {#at}

*idx*&nbsp;`integer` *list*&nbsp;`[<l>]` *&rarr;*&nbsp;`<a>`

*idx*&nbsp;`string` *object*&nbsp;`object:<{o}>` *&rarr;*&nbsp;`<a>`


Index LIST at IDX, or get value with key IDX from OBJECT. 
```lisp
pact> (at 1 [1 2 3])
2
pact> (at "bar" { "foo": 1, "bar": 2 })
2
```


### bind {#bind}

*src*&nbsp;`object:<{row}>` *binding*&nbsp;`binding:<{row}>` *body*&nbsp;`*` *&rarr;*&nbsp;`<a>`


Special form evaluates SRC to an object which is bound to with BINDINGS to run BODY. 
```lisp
pact> (bind { "a": 1, "b": 2 } { "a" := a-value } a-value)
1
```


### compose {#compose}

*x*&nbsp;`(x:<a>)-><b>` *y*&nbsp;`(x:<b>)-><c>` *value*&nbsp;`<a>` *&rarr;*&nbsp;`<c>`


Compose X and Y, such that X operates on VALUE, and Y on the results of X. 
```lisp
pact> (filter (compose (length) (< 2)) ["my" "dog" "has" "fleas"])
["dog" "has" "fleas"]:*
```


### drop {#drop}

*count*&nbsp;`integer` *list*&nbsp;`<a[[<l>],string]>` *&rarr;*&nbsp;`<a[[<l>],string]>`


Drop COUNT values from LIST (or string). If negative, drop from end.
```lisp
pact> (drop 2 "vwxyz")
"xyz"
pact> (drop (- 2) [1 2 3 4 5])
[1 2 3]:*
```


### enforce {#enforce}

*test*&nbsp;`bool` *msg*&nbsp;`string` *&rarr;*&nbsp;`bool`


Fail transaction with MSG if TEST fails, or returns true. 
```lisp
pact> (enforce (!= (+ 2 2) 4) "Chaos reigns")
<interactive>:1:0:Failure: Chaos reigns
```


### filter {#filter}

*app*&nbsp;`(x:<a>)->bool` *list*&nbsp;`[<a>]` *&rarr;*&nbsp;`[<a>]`


Filter LIST by applying APP to each element to get a boolean determining inclusion.
```lisp
pact> (filter (compose (length) (< 2)) ["my" "dog" "has" "fleas"])
["dog" "has" "fleas"]:*
```


### fold {#fold}

*app*&nbsp;`(x:<b> y:<b>)-><a>` *init*&nbsp;`<a>` *list*&nbsp;`[<b>]` *&rarr;*&nbsp;`<a>`


Iteratively reduce LIST by applying APP to last result and element, starting with INIT. 
```lisp
pact> (fold (+) 0 [100 10 5])
115
```


### format {#format}

*template*&nbsp;`string` *vars*&nbsp;`*` *&rarr;*&nbsp;`string`


Interpolate VARS into TEMPLATE using {}. 
```lisp
pact> (format "My {} has {}" "dog" "fleas")
"My dog has fleas"
```


### if {#if}

*cond*&nbsp;`bool` *then*&nbsp;`<a>` *else*&nbsp;`<a>` *&rarr;*&nbsp;`<a>`


Test COND, if true evaluate THEN, otherwise evaluate ELSE. 
```lisp
pact> (if (= (+ 2 2) 4) "Sanity prevails" "Chaos reigns")
"Sanity prevails"
```


### length {#length}

*x*&nbsp;`<a[[<l>],string,object:<{o}>]>` *&rarr;*&nbsp;`integer`


Compute length of X, which can be a list, a string, or an object.
```lisp
pact> (length [1 2 3])
3
pact> (length "abcdefgh")
8
pact> (length { "a": 1, "b": 2 })
2
```


### list {#list}

*elems*&nbsp;`*` *&rarr;*&nbsp;`list`


Create list from ELEMS. 
```lisp
pact> (list 1 2 3)
[1 2 3]:*
```


### list-modules {#list-modules}

 *&rarr;*&nbsp;`[string]`


List modules available for loading.


### map {#map}

*app*&nbsp;`(x:<b>)-><a>` *list*&nbsp;`[<b>]` *&rarr;*&nbsp;`[<a>]`


Apply elements in LIST as last arg to APP, returning list of results. 
```lisp
pact> (map (+ 1) [1 2 3])
[2 3 4]:*
```


### pact-txid {#pact-txid}

 *&rarr;*&nbsp;`integer`


Return reference tx id for pact execution.


### read-decimal {#read-decimal}

*key*&nbsp;`string` *&rarr;*&nbsp;`decimal`


Parse KEY string value from message data body as decimal.
```lisp
(defun exec ()
   (transfer (read-msg "from") (read-msg "to") (read-decimal "amount")))
```


### read-integer {#read-integer}

*key*&nbsp;`string` *&rarr;*&nbsp;`integer`


Parse KEY string value from message data body as integer. 
```lisp
(read-integer "age")
```


### read-msg {#read-msg}

*key*&nbsp;`string` *&rarr;*&nbsp;`<a>`


Read KEY from message data body. Will recognize JSON types as corresponding Pact type.
```lisp
(defun exec ()
   (transfer (read-msg "from") (read-msg "to") (read-decimal "amount")))
```


### remove {#remove}

*key*&nbsp;`string` *object*&nbsp;`object:<{o}>` *&rarr;*&nbsp;`object:<{o}>`


Remove entry for KEY from OBJECT. 
```lisp
pact> (remove "bar" { "foo": 1, "bar": 2 })
{"foo": 1}:*
```


### take {#take}

*count*&nbsp;`integer` *list*&nbsp;`<a[[<l>],string]>` *&rarr;*&nbsp;`<a[[<l>],string]>`


Take COUNT values from LIST (or string). If negative, take from end. 
```lisp
pact> (take 2 "abcd")
"ab"
pact> (take (- 3) [1 2 3 4 5])
[3 4 5]:*
```


### typeof {#typeof}

*x*&nbsp;`<a>` *&rarr;*&nbsp;`string`


Returns type of X as string. 
```lisp
pact> (typeof "hello")
"string"
```

## Database {#Database}

### create-table {#create-table}

*table*&nbsp;`table:<{row}>` *&rarr;*&nbsp;`string`


Create table TABLE. 
```lisp
(create-table accounts)
```


### describe-keyset {#describe-keyset}

*keyset*&nbsp;`string` *&rarr;*&nbsp;`value`


Get metadata for KEYSET


### describe-module {#describe-module}

*module*&nbsp;`string` *&rarr;*&nbsp;`value`


Get metadata for MODULE


### describe-table {#describe-table}

*table*&nbsp;`string` *&rarr;*&nbsp;`value`


Get metadata for TABLE


### insert {#insert}

*table*&nbsp;`table:<{row}>` *key*&nbsp;`string` *object*&nbsp;`object:<{row}>` *&rarr;*&nbsp;`string`


Write entry in TABLE for KEY of OBJECT column data, failing if data already exists for KEY.
```lisp
(insert 'accounts { "balance": 0.0, "note": "Created account." })
```


### keys {#keys}

*table*&nbsp;`table:<{row}>` *&rarr;*&nbsp;`[string]`


Return all keys in TABLE. 
```lisp
(keys 'accounts)
```


### read {#read}

*table*&nbsp;`table:<{row}>` *key*&nbsp;`string` *&rarr;*&nbsp;`object:<{row}>`

*table*&nbsp;`table:<{row}>` *key*&nbsp;`string` *columns*&nbsp;`[string]` *&rarr;*&nbsp;`object:<{row}>`


Read row from TABLE for KEY returning database record object, or just COLUMNS if specified. 
```lisp
(read 'accounts id ['balance 'ccy])
```


### txids {#txids}

*table*&nbsp;`table:<{row}>` *txid*&nbsp;`integer` *&rarr;*&nbsp;`[integer]`


Return all txid values greater than or equal to TXID in TABLE. 
```lisp
(txids 'accounts 123849535)
```


### txlog {#txlog}

*table*&nbsp;`table:<{row}>` *txid*&nbsp;`integer` *&rarr;*&nbsp;`[value]`


Return all updates to TABLE performed in transaction TXID. 
```lisp
(txlog 'accounts 123485945)
```


### update {#update}

*table*&nbsp;`table:<{row}>` *key*&nbsp;`string` *object*&nbsp;`object:<{row}>` *&rarr;*&nbsp;`string`


Write entry in TABLE for KEY of OBJECT column data, failing if data does not exist for KEY.
```lisp
(update 'accounts { "balance": (+ bal amount), "change": amount, "note": "credit" })
```


### with-default-read {#with-default-read}

*table*&nbsp;`table:<{row}>` *key*&nbsp;`string` *defaults*&nbsp;`object:<{row}>` *bindings*&nbsp;`binding:<{row}>` *&rarr;*&nbsp;`<a>`


Special form to read row from TABLE for KEY and bind columns per BINDINGS over subsequent body statements. If row not found, read columns from DEFAULTS, an object with matching key names. 
```lisp
(with-default-read 'accounts id { "balance": 0, "ccy": "USD" } { "balance":= bal, "ccy":= ccy }
   (format "Balance for {} is {} {}" id bal ccy))
```


### with-read {#with-read}

*table*&nbsp;`table:<{row}>` *key*&nbsp;`string` *bindings*&nbsp;`binding:<{row}>` *&rarr;*&nbsp;`<a>`


Special form to read row from TABLE for KEY and bind columns per BINDINGS over subsequent body statements.
```lisp
(with-read 'accounts id { "balance":= bal, "ccy":= ccy }
   (format "Balance for {} is {} {}" id bal ccy))
```


### write {#write}

*table*&nbsp;`table:<{row}>` *key*&nbsp;`string` *object*&nbsp;`object:<{row}>` *&rarr;*&nbsp;`string`


Write entry in TABLE for KEY of OBJECT column data.
```lisp
(write 'accounts { "balance": 100.0 })
```

## Time {#Time}

### add-time {#add-time}

*time*&nbsp;`time` *seconds*&nbsp;`decimal` *&rarr;*&nbsp;`time`

*time*&nbsp;`time` *seconds*&nbsp;`integer` *&rarr;*&nbsp;`time`


Add SECONDS to TIME; SECONDS can be integer or decimal. 
```lisp
pact> (add-time (time "2016-07-22T12:00:00Z") 15)
"2016-07-22T12:00:15Z"
```


### days {#days}

*n*&nbsp;`decimal` *&rarr;*&nbsp;`decimal`

*n*&nbsp;`integer` *&rarr;*&nbsp;`decimal`


N days, for use with 'add-time' 
```lisp
pact> (add-time (time "2016-07-22T12:00:00Z") (days 1))
"2016-07-23T12:00:00Z"
```


### diff-time {#diff-time}

*time1*&nbsp;`time` *time2*&nbsp;`time` *&rarr;*&nbsp;`decimal`


Compute difference between TIME1 and TIME2 in seconds. 
```lisp
pact> (diff-time (parse-time "%T" "16:00:00") (parse-time "%T" "09:30:00"))
23400
```


### hours {#hours}

*n*&nbsp;`decimal` *&rarr;*&nbsp;`decimal`

*n*&nbsp;`integer` *&rarr;*&nbsp;`decimal`


N hours, for use with 'add-time' 
```lisp
pact> (add-time (time "2016-07-22T12:00:00Z") (hours 1))
"2016-07-22T13:00:00Z"
```


### minutes {#minutes}

*n*&nbsp;`decimal` *&rarr;*&nbsp;`decimal`

*n*&nbsp;`integer` *&rarr;*&nbsp;`decimal`


N minutes, for use with 'add-time'. 
```lisp
pact> (add-time (time "2016-07-22T12:00:00Z") (minutes 1))
"2016-07-22T12:01:00Z"
```


### parse-time {#parse-time}

*format*&nbsp;`string` *utcval*&nbsp;`string` *&rarr;*&nbsp;`time`


Construct time from UTCVAL using FORMAT. See [strftime docs](https://www.gnu.org/software/libc/manual/html_node/Formatting-Calendar-Time.html#index-strftime) for format info. 
```lisp
pact> (parse-time "%F" "2016-09-12")
"2016-09-12T00:00:00Z"
```


### time {#time}

*utcval*&nbsp;`string` *&rarr;*&nbsp;`time`


Construct time from UTCVAL using ISO8601 format (%Y-%m-%dT%H:%M:%SZ). 
```lisp
pact> (time "2016-07-22T11:26:35Z")
"2016-07-22T11:26:35Z"
```

## Operators {#Operators}

### != {#bangeq}

*x*&nbsp;`<a[integer,string,time,decimal,bool,[<l>],object:<{o}>,keyset]>` *y*&nbsp;`<a[integer,string,time,decimal,bool,[<l>],object:<{o}>,keyset]>` *&rarr;*&nbsp;`bool`


True if X does not equal Y. 
```lisp
pact> (!= "hello" "goodbye")
true
```


### * {#star}

*x*&nbsp;`<a[integer,decimal]>` *y*&nbsp;`<a[integer,decimal]>` *&rarr;*&nbsp;`<a[integer,decimal]>`

*x*&nbsp;`<a[integer,decimal]>` *y*&nbsp;`<b[integer,decimal]>` *&rarr;*&nbsp;`decimal`


Multiply X by Y. 
```lisp
pact> (* 0.5 10.0)
5
pact> (* 3 5)
15
```


### \+ {#plus}

*x*&nbsp;`<a[integer,decimal]>` *y*&nbsp;`<a[integer,decimal]>` *&rarr;*&nbsp;`<a[integer,decimal]>`

*x*&nbsp;`<a[integer,decimal]>` *y*&nbsp;`<b[integer,decimal]>` *&rarr;*&nbsp;`decimal`

*x*&nbsp;`<a[string,[<l>],object:<{o}>]>` *y*&nbsp;`<a[string,[<l>],object:<{o}>]>` *&rarr;*&nbsp;`<a[string,[<l>],object:<{o}>]>`


Add numbers, concatenate strings/lists, or merge objects. 
```lisp
pact> (+ 1 2)
3
pact> (+ 5.0 0.5)
5.5
pact> (+ "every" "body")
"everybody"
pact> (+ [1 2] [3 4])
[1 2 3 4]:*
pact> (+ { "foo": 100 } { "foo": 1, "bar": 2 })
{"bar": 2, "foo": 100}:*
```


### \- {#minus}

*x*&nbsp;`<a[integer,decimal]>` *y*&nbsp;`<a[integer,decimal]>` *&rarr;*&nbsp;`<a[integer,decimal]>`

*x*&nbsp;`<a[integer,decimal]>` *y*&nbsp;`<b[integer,decimal]>` *&rarr;*&nbsp;`decimal`

*x*&nbsp;`<a[integer,decimal]>` *&rarr;*&nbsp;`<a[integer,decimal]>`


Negate X, or subtract Y from X. 
```lisp
pact> (- 1.0)
-1.0
pact> (- 3 2)
1
```


### / {#slash}

*x*&nbsp;`<a[integer,decimal]>` *y*&nbsp;`<a[integer,decimal]>` *&rarr;*&nbsp;`<a[integer,decimal]>`

*x*&nbsp;`<a[integer,decimal]>` *y*&nbsp;`<b[integer,decimal]>` *&rarr;*&nbsp;`decimal`


Divide X by Y. 
```lisp
pact> (/ 10.0 2.0)
5
pact> (/ 8 3)
2
```


### < {#lt}

*x*&nbsp;`<a[integer,decimal,string,time]>` *y*&nbsp;`<a[integer,decimal,string,time]>` *&rarr;*&nbsp;`bool`


True if X < Y. 
```lisp
pact> (< 1 3)
true
pact> (< 5.24 2.52)
false
pact> (< "abc" "def")
true
```


### <= {#lteq}

*x*&nbsp;`<a[integer,decimal,string,time]>` *y*&nbsp;`<a[integer,decimal,string,time]>` *&rarr;*&nbsp;`bool`


True if X <= Y. 
```lisp
pact> (<= 1 3)
true
pact> (<= 5.24 2.52)
false
pact> (<= "abc" "def")
true
```


### = {#eq}

*x*&nbsp;`<a[integer,string,time,decimal,bool,[<l>],object:<{o}>,keyset]>` *y*&nbsp;`<a[integer,string,time,decimal,bool,[<l>],object:<{o}>,keyset]>` *&rarr;*&nbsp;`bool`


True if X equals Y. 
```lisp
pact> (= [1 2 3] [1 2 3])
true
pact> (= 'foo "foo")
true
pact> (= { 1: 2 } { 1: 2})
true
```


### > {#gt}

*x*&nbsp;`<a[integer,decimal,string,time]>` *y*&nbsp;`<a[integer,decimal,string,time]>` *&rarr;*&nbsp;`bool`


True if X > Y. 
```lisp
pact> (> 1 3)
false
pact> (> 5.24 2.52)
true
pact> (> "abc" "def")
false
```


### >= {#gteq}

*x*&nbsp;`<a[integer,decimal,string,time]>` *y*&nbsp;`<a[integer,decimal,string,time]>` *&rarr;*&nbsp;`bool`


True if X >= Y. 
```lisp
pact> (>= 1 3)
false
pact> (>= 5.24 2.52)
true
pact> (>= "abc" "def")
false
```


### ^ {#hat}

*x*&nbsp;`<a[integer,decimal]>` *y*&nbsp;`<a[integer,decimal]>` *&rarr;*&nbsp;`<a[integer,decimal]>`

*x*&nbsp;`<a[integer,decimal]>` *y*&nbsp;`<b[integer,decimal]>` *&rarr;*&nbsp;`decimal`


Raise X to Y power. 
```lisp
pact> (^ 2 3)
8
```


### abs {#abs}

*x*&nbsp;`decimal` *&rarr;*&nbsp;`decimal`

*x*&nbsp;`integer` *&rarr;*&nbsp;`integer`


Absolute value of X. 
```lisp
pact> (abs (- 10 23))
13
```


### and {#and}

*x*&nbsp;`bool` *y*&nbsp;`bool` *&rarr;*&nbsp;`bool`


Boolean logic. 
```lisp
pact> (and true false)
false
```


### ceiling {#ceiling}

*x*&nbsp;`decimal` *prec*&nbsp;`integer` *&rarr;*&nbsp;`decimal`

*x*&nbsp;`decimal` *&rarr;*&nbsp;`integer`


Rounds up value of decimal X as integer, or to PREC precision as decimal. 
```lisp
pact> (ceiling 3.5)
4
pact> (ceiling 100.15234 2)
100.16
```


### exp {#exp}

*x*&nbsp;`<a[integer,decimal]>` *&rarr;*&nbsp;`<a[integer,decimal]>`


Exp of X 
```lisp
pact> (round (exp 3) 6)
20.085537
```


### floor {#floor}

*x*&nbsp;`decimal` *prec*&nbsp;`integer` *&rarr;*&nbsp;`decimal`

*x*&nbsp;`decimal` *&rarr;*&nbsp;`integer`


Rounds down value of decimal X as integer, or to PREC precision as decimal. 
```lisp
pact> (floor 3.5)
3
pact> (floor 100.15234 2)
100.15
```


### ln {#ln}

*x*&nbsp;`<a[integer,decimal]>` *&rarr;*&nbsp;`<a[integer,decimal]>`


Natural log of X. 
```lisp
pact> (round (ln 60) 6)
4.094345
```


### log {#log}

*x*&nbsp;`<a[integer,decimal]>` *y*&nbsp;`<a[integer,decimal]>` *&rarr;*&nbsp;`<a[integer,decimal]>`

*x*&nbsp;`<a[integer,decimal]>` *y*&nbsp;`<b[integer,decimal]>` *&rarr;*&nbsp;`decimal`


Log of Y base X. 
```lisp
pact> (log 2 256)
8
```


### mod {#mod}

*x*&nbsp;`integer` *y*&nbsp;`integer` *&rarr;*&nbsp;`integer`


X modulo Y. 
```lisp
pact> (mod 13 8)
5
```


### not {#not}

*x*&nbsp;`bool` *&rarr;*&nbsp;`bool`


Boolean logic. 
```lisp
pact> (not (> 1 2))
true
```


### or {#or}

*x*&nbsp;`bool` *y*&nbsp;`bool` *&rarr;*&nbsp;`bool`


Boolean logic. 
```lisp
pact> (or true false)
true
```


### round {#round}

*x*&nbsp;`decimal` *prec*&nbsp;`integer` *&rarr;*&nbsp;`decimal`

*x*&nbsp;`decimal` *&rarr;*&nbsp;`integer`


Performs Banker's rounding value of decimal X as integer, or to PREC precision as decimal. 
```lisp
pact> (round 3.5)
4
pact> (round 100.15234 2)
100.15
```


### sqrt {#sqrt}

*x*&nbsp;`<a[integer,decimal]>` *&rarr;*&nbsp;`<a[integer,decimal]>`


Square root of X. 
```lisp
pact> (sqrt 25)
5
```

## Keysets {#Keysets}

### define-keyset {#define-keyset}

*name*&nbsp;`string` *keyset*&nbsp;`string` *&rarr;*&nbsp;`string`


Define keyset as NAME with KEYSET. If keyset NAME already exists, keyset will be enforced before updating to new value.
```lisp
(define-keyset 'admin-keyset (read-keyset "keyset"))
```


### enforce-keyset {#enforce-keyset}

*keyset-or-name*&nbsp;`<k[string,keyset]>` *&rarr;*&nbsp;`bool`


Special form to enforce KEYSET-OR-NAME against message keys before running BODY. KEYSET-OR-NAME can be a symbol of a keyset name or a keyset object. 
```lisp
(with-keyset 'admin-keyset ...)
(with-keyset (read-keyset "keyset") ...)
```


### keys-2 {#keys-2}

*count*&nbsp;`integer` *matched*&nbsp;`integer` *&rarr;*&nbsp;`bool`


Keyset predicate function to match at least 2 keys in keyset. 
```lisp
pact> (keys-2 3 1)
false
```


### keys-all {#keys-all}

*count*&nbsp;`integer` *matched*&nbsp;`integer` *&rarr;*&nbsp;`bool`


Keyset predicate function to match all keys in keyset. 
```lisp
pact> (keys-all 3 3)
true
```


### keys-any {#keys-any}

*count*&nbsp;`integer` *matched*&nbsp;`integer` *&rarr;*&nbsp;`bool`


Keyset predicate function to match all keys in keyset. 
```lisp
pact> (keys-any 10 1)
true
```


### read-keyset {#read-keyset}

*key*&nbsp;`string` *&rarr;*&nbsp;`keyset`


Read KEY from message data body as keyset ({ "keys": KEYLIST, "pred": PREDFUN }). PREDFUN should resolve to a keys predicate. 
```lisp
(read-keyset "admin-keyset")
```

## REPL-only functions {#repl-lib}

The following functions are loaded magically in the interactive REPL, or in script files with a `.repl` extension. They are not available for blockchain-based execution.


### begin-tx {#begin-tx}

 *&rarr;*&nbsp;`string`

*name*&nbsp;`string` *&rarr;*&nbsp;`string`


Begin transaction with optional NAME. 
```lisp
(begin-tx "load module")
```


### bench {#bench}

*exprs*&nbsp;`*` *&rarr;*&nbsp;`string`


Benchmark execution of EXPRS. 
```lisp
(bench (+ 1 2))
```


### commit-tx {#commit-tx}

 *&rarr;*&nbsp;`string`


Commit transaction. 
```lisp
(commit-tx)
```


### env-data {#env-data}

*json*&nbsp;`<a[integer,string,time,decimal,bool,[<l>],object:<{o}>,keyset,value]>` *&rarr;*&nbsp;`string`


Set transaction JSON data, either as encoded string, or as pact types coerced to JSON. 
```lisp
pact> (env-data { "keyset": { "keys": ["my-key" "admin-key"], "pred": "keys-any" } })
"Setting transaction data"
```


### env-entity {#env-entity}

*entity*&nbsp;`string` *&rarr;*&nbsp;`string`


Set environment confidential ENTITY id. 
```lisp
(env-entity "my-org")
```


### env-keys {#env-keys}

*keys*&nbsp;`[string]` *&rarr;*&nbsp;`string`


Set transaction signature KEYS. 
```lisp
pact> (env-keys ["my-key" "admin-key"])
"Setting transaction keys"
```


### env-step {#env-step}

 *&rarr;*&nbsp;`string`

*step-idx*&nbsp;`integer` *&rarr;*&nbsp;`string`

*step-idx*&nbsp;`integer` *rollback*&nbsp;`bool` *&rarr;*&nbsp;`string`


Modify pact step state. With no arguments, unset step. STEP-IDX sets step index for current pact execution, ROLLBACK defaults to false. 
```lisp
(env-step 1)
(env-step 0 true)
```


### expect {#expect}

*doc*&nbsp;`string` *expected*&nbsp;`<a>` *actual*&nbsp;`<a>` *&rarr;*&nbsp;`string`


Evaluate ACTUAL and verify that it equals EXPECTED. 
```lisp
pact> (expect "Sanity prevails." 4 (+ 2 2))
"Expect: success: Sanity prevails."
```


### expect-failure {#expect-failure}

*doc*&nbsp;`string` *exp*&nbsp;`<a>` *&rarr;*&nbsp;`string`


Evaluate EXP and succeed only if it throws an error. 
```lisp
pact> (expect-failure "Enforce fails on false" (enforce false "Expected error"))
"Expect failure: success: Enforce fails on false"
```


### load {#load}

*file*&nbsp;`string` *&rarr;*&nbsp;`string`

*file*&nbsp;`string` *reset*&nbsp;`bool` *&rarr;*&nbsp;`string`


Load and evaluate FILE, resetting repl state beforehand if optional NO-RESET is true. 
```lisp
(load "accounts.repl")
```


### rollback-tx {#rollback-tx}

 *&rarr;*&nbsp;`string`


Rollback transaction. 
```lisp
(rollback-tx)
```


### typecheck {#typecheck}

*module*&nbsp;`string` *&rarr;*&nbsp;`string`

*module*&nbsp;`string` *debug*&nbsp;`bool` *&rarr;*&nbsp;`string`


Typecheck MODULE, optionally enabling DEBUG output.

