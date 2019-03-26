# Built-in Functions {#builtins}
## General {#General}

### at {#at}

*idx*&nbsp;`integer` *list*&nbsp;`[<l>]` *&rarr;*&nbsp;`<a>`

*idx*&nbsp;`string` *object*&nbsp;`object:[]<{o}>` *&rarr;*&nbsp;`<a>`


Index LIST at IDX, or get value with key IDX from OBJECT.
```lisp
pact> (at 1 [1 2 3])
2
pact> (at "bar" { "foo": 1, "bar": 2 })
2
```


### bind {#bind}

*src*&nbsp;`object:[]<{row}>` *binding*&nbsp;`binding:[]<{row}>` *&rarr;*&nbsp;`<a>`


Special form evaluates SRC to an object which is bound to with BINDINGS over subsequent body statements.
```lisp
pact> (bind { "a": 1, "b": 2 } { "a" := a-value } a-value)
1
```


### compose {#compose}

*x*&nbsp;`x:<a> -> <b>` *y*&nbsp;`x:<b> -> <c>` *value*&nbsp;`<a>` *&rarr;*&nbsp;`<c>`


Compose X and Y, such that X operates on VALUE, and Y on the results of X.
```lisp
pact> (filter (compose (length) (< 2)) ["my" "dog" "has" "fleas"])
["dog" "has" "fleas"]
```


### constantly {#constantly}

*value*&nbsp;`<a>` *ignore1*&nbsp;`<b>` *&rarr;*&nbsp;`<a>`

*value*&nbsp;`<a>` *ignore1*&nbsp;`<b>` *ignore2*&nbsp;`<c>` *&rarr;*&nbsp;`<a>`

*value*&nbsp;`<a>` *ignore1*&nbsp;`<b>` *ignore2*&nbsp;`<c>` *ignore3*&nbsp;`<d>` *&rarr;*&nbsp;`<a>`


Lazily ignore arguments IGNORE* and return VALUE.
```lisp
pact> (filter (constantly true) [1 2 3])
[1 2 3]
```


### contains {#contains}

*value*&nbsp;`<a>` *list*&nbsp;`[<a>]` *&rarr;*&nbsp;`bool`

*key*&nbsp;`<a>` *object*&nbsp;`object:[]<{o}>` *&rarr;*&nbsp;`bool`

*value*&nbsp;`string` *string*&nbsp;`string` *&rarr;*&nbsp;`bool`


Test that LIST or STRING contains VALUE, or that OBJECT has KEY entry.
```lisp
pact> (contains 2 [1 2 3])
true
pact> (contains 'name { 'name: "Ted", 'age: 72 })
true
pact> (contains "foo" "foobar")
true
```


### define-namespace {#define-namespace}

*namespace*&nbsp;`string` *guard*&nbsp;`guard` *&rarr;*&nbsp;`string`


Create a namespace called NAMESPACE where ownership and use of the namespace is controlled by GUARD. If NAMESPACE is already defined, then the guard previously defined in NAMESPACE will be enforced, and GUARD will be rotated in its place.
```lisp
(define-namespace 'my-namespace (read-keyset 'my-keyset))
```

Top level only: this function will fail if used in module code.


### drop {#drop}

*count*&nbsp;`integer` *list*&nbsp;`<a[[<l>],string]>` *&rarr;*&nbsp;`<a[[<l>],string]>`

*keys*&nbsp;`[string]` *object*&nbsp;`object:[]<{o}>` *&rarr;*&nbsp;`object:[]<{o}>`


Drop COUNT values from LIST (or string), or entries having keys in KEYS from OBJECT. If COUNT is negative, drop from end.
```lisp
pact> (drop 2 "vwxyz")
"xyz"
pact> (drop (- 2) [1 2 3 4 5])
[1 2 3]
pact> (drop ['name] { 'name: "Vlad", 'active: false})
{"active": false}
```


### enforce {#enforce}

*test*&nbsp;`bool` *msg*&nbsp;`string` *&rarr;*&nbsp;`bool`


Fail transaction with MSG if pure expression TEST is false. Otherwise, returns true.
```lisp
pact> (enforce (!= (+ 2 2) 4) "Chaos reigns")
<interactive>:0:0: Chaos reigns
```


### enforce-one {#enforce-one}

*msg*&nbsp;`string` *tests*&nbsp;`[bool]` *&rarr;*&nbsp;`bool`


Run TESTS in order (in pure context, plus keyset enforces). If all fail, fail transaction. Short-circuits on first success.
```lisp
pact> (enforce-one "Should succeed on second test" [(enforce false "Skip me") (enforce (= (+ 2 2) 4) "Chaos reigns")])
true
```


### enforce-pact-version {#enforce-pact-version}

*min-version*&nbsp;`string` *&rarr;*&nbsp;`bool`

*min-version*&nbsp;`string` *max-version*&nbsp;`string` *&rarr;*&nbsp;`bool`


Enforce runtime pact version as greater than or equal MIN-VERSION, and less than or equal MAX-VERSION. Version values are matched numerically from the left, such that '2', '2.2', and '2.2.3' would all allow '2.2.3'.
```lisp
pact> (enforce-pact-version "2.3")
true
```

Top level only: this function will fail if used in module code.


### filter {#filter}

*app*&nbsp;`x:<a> -> bool` *list*&nbsp;`[<a>]` *&rarr;*&nbsp;`[<a>]`


Filter LIST by applying APP to each element. For each true result, the original value is kept.
```lisp
pact> (filter (compose (length) (< 2)) ["my" "dog" "has" "fleas"])
["dog" "has" "fleas"]
```


### fold {#fold}

*app*&nbsp;`x:<a> y:<b> -> <a>` *init*&nbsp;`<a>` *list*&nbsp;`[<b>]` *&rarr;*&nbsp;`<a>`


Iteratively reduce LIST by applying APP to last result and element, starting with INIT.
```lisp
pact> (fold (+) 0 [100 10 5])
115
```


### format {#format}

*template*&nbsp;`string` *vars*&nbsp;`[*]` *&rarr;*&nbsp;`string`


Interpolate VARS into TEMPLATE using {}.
```lisp
pact> (format "My {} has {}" ["dog" "fleas"])
"My dog has fleas"
```


### hash {#hash}

*value*&nbsp;`<a>` *&rarr;*&nbsp;`string`


Compute BLAKE2b 512-bit hash of VALUE. Strings are converted directly while other values are converted using their JSON representation.
```lisp
pact> (hash "hello")
"e4cfa39a3d37be31c59609e807970799caa68a19bfaa15135f165085e01d41a65ba1e1b146aeb6bd0092b49eac214c103ccfa3a365954bbbe52f74a2b3620c94"
pact> (hash { 'foo: 1 })
"61d3c8775e151b4582ca7f9a885a9b2195d5aa6acc58ddca61a504e9986bb8c06eeb37af722ad848f9009053b6379677bf111e25a680ab41a209c4d56ff1e183"
```


### identity {#identity}

*value*&nbsp;`<a>` *&rarr;*&nbsp;`<a>`


Return provided value.
```lisp
pact> (map (identity) [1 2 3])
[1 2 3]
```


### if {#if}

*cond*&nbsp;`bool` *then*&nbsp;`<a>` *else*&nbsp;`<a>` *&rarr;*&nbsp;`<a>`


Test COND. If true, evaluate THEN. Otherwise, evaluate ELSE.
```lisp
pact> (if (= (+ 2 2) 4) "Sanity prevails" "Chaos reigns")
"Sanity prevails"
```


### length {#length}

*x*&nbsp;`<a[[<l>],string,object:[]<{o}>]>` *&rarr;*&nbsp;`integer`


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

*elems*&nbsp;`*` *&rarr;*&nbsp;`[*]`


Create list from ELEMS. Deprecated in Pact 2.1.1 with literal list support.
```lisp
pact> (list 1 2 3)
[1 2 3]
```


### list-modules {#list-modules}

 *&rarr;*&nbsp;`[string]`


List modules available for loading.

Top level only: this function will fail if used in module code.


### make-list {#make-list}

*length*&nbsp;`integer` *value*&nbsp;`<a>` *&rarr;*&nbsp;`[<a>]`


Create list by repeating VALUE LENGTH times.
```lisp
pact> (make-list 5 true)
[true true true true true]
```


### map {#map}

*app*&nbsp;`x:<b> -> <a>` *list*&nbsp;`[<b>]` *&rarr;*&nbsp;`[<a>]`


Apply APP to each element in LIST, returning a new list of results.
```lisp
pact> (map (+ 1) [1 2 3])
[2 3 4]
```


### namespace {#namespace}

*namespace*&nbsp;`string` *&rarr;*&nbsp;`string`


Set the current namespace to NAMESPACE. All expressions that occur in a current transaction will be contained in NAMESPACE, and once committed, may be accessed via their fully qualified name, which will include the namespace. Subsequent namespace calls in the same tx will set a new namespace for all declarations until either the next namespace declaration, or the end of the tx.
```lisp
(namespace 'my-namespace)
```

Top level only: this function will fail if used in module code.


### pact-id {#pact-id}

 *&rarr;*&nbsp;`integer`


Return ID if called during current pact execution, failing if not.


### pact-version {#pact-version}

 *&rarr;*&nbsp;`string`


Obtain current pact build version.
```lisp
pact> (pact-version)
"2.6.1"
```

Top level only: this function will fail if used in module code.


### read-decimal {#read-decimal}

*key*&nbsp;`string` *&rarr;*&nbsp;`decimal`


Parse KEY string or number value from top level of message data body as decimal.
```lisp
(defun exec ()
   (transfer (read-msg "from") (read-msg "to") (read-decimal "amount")))
```


### read-integer {#read-integer}

*key*&nbsp;`string` *&rarr;*&nbsp;`integer`


Parse KEY string or number value from top level of message data body as integer.
```lisp
(read-integer "age")
```


### read-msg {#read-msg}

 *&rarr;*&nbsp;`<a>`

*key*&nbsp;`string` *&rarr;*&nbsp;`<a>`


Read KEY from top level of message data body, or data body itself if not provided. Coerces value to their corresponding pact type: String -> string, Number -> integer, Boolean -> bool, List -> list, Object -> object. However, top-level values are provided as a 'value' JSON type.
```lisp
(defun exec ()
   (transfer (read-msg "from") (read-msg "to") (read-decimal "amount")))
```


### remove {#remove}

*key*&nbsp;`string` *object*&nbsp;`object:[]<{o}>` *&rarr;*&nbsp;`object:[]<{o}>`


Remove entry for KEY from OBJECT.
```lisp
pact> (remove "bar" { "foo": 1, "bar": 2 })
{"foo": 1}
```


### resume {#resume}

*binding*&nbsp;`binding:[]<{y}>` *body*&nbsp;`*` *&rarr;*&nbsp;`<a>`


Special form binds to a yielded object value from the prior step execution in a pact.


### reverse {#reverse}

*list*&nbsp;`[<a>]` *&rarr;*&nbsp;`[<a>]`


Reverse LIST.
```lisp
pact> (reverse [1 2 3])
[3 2 1]
```


### sort {#sort}

*values*&nbsp;`[<a>]` *&rarr;*&nbsp;`[<a>]`

*fields*&nbsp;`[string]` *values*&nbsp;`[object:[]<{o}>]` *&rarr;*&nbsp;`[object:[]<{o}>]`


Sort a homogeneous list of primitive VALUES, or objects using supplied FIELDS list.
```lisp
pact> (sort [3 1 2])
[1 2 3]
pact> (sort ['age] [{'name: "Lin",'age: 30} {'name: "Val",'age: 25}])
[{"name": "Val","age": 25} {"name": "Lin","age": 30}]
```


### str-to-int {#str-to-int}

*str-val*&nbsp;`string` *&rarr;*&nbsp;`integer`

*base*&nbsp;`integer` *str-val*&nbsp;`string` *&rarr;*&nbsp;`integer`


Compute the integer value of STR-VAL in base 10, or in BASE if specified. STR-VAL must be <= 128 chars in length and BASE must be between 2 and 16. Each digit must be in the correct range for the base.
```lisp
pact> (str-to-int 16 "abcdef123456")
188900967593046
pact> (str-to-int "123456")
123456
```


### take {#take}

*count*&nbsp;`integer` *list*&nbsp;`<a[[<l>],string]>` *&rarr;*&nbsp;`<a[[<l>],string]>`

*keys*&nbsp;`[string]` *object*&nbsp;`object:[]<{o}>` *&rarr;*&nbsp;`object:[]<{o}>`


Take COUNT values from LIST (or string), or entries having keys in KEYS from OBJECT. If COUNT is negative, take from end.
```lisp
pact> (take 2 "abcd")
"ab"
pact> (take (- 3) [1 2 3 4 5])
[3 4 5]
pact> (take ['name] { 'name: "Vlad", 'active: false})
{"name": "Vlad"}
```


### tx-hash {#tx-hash}

 *&rarr;*&nbsp;`string`


Obtain hash of current transaction as a string.
```lisp
pact> (tx-hash)
"786a02f742015903c6c6fd852552d272912f4740e15847618a86e217f71f5419d25e1031afee585313896444934eb04b903a685b1448b755d56f701afe9be2ce"
```


### typeof {#typeof}

*x*&nbsp;`<a>` *&rarr;*&nbsp;`string`


Returns type of X as string.
```lisp
pact> (typeof "hello")
"string"
```


### where {#where}

*field*&nbsp;`string` *app*&nbsp;`x:<a> -> bool` *value*&nbsp;`object:[]<{row}>` *&rarr;*&nbsp;`bool`


Utility for use in 'filter' and 'select' applying APP to FIELD in VALUE.
```lisp
pact> (filter (where 'age (> 20)) [{'name: "Mary",'age: 30} {'name: "Juan",'age: 15}])
[{"name": "Juan","age": 15}]
```


### yield {#yield}

*OBJECT*&nbsp;`object:[]<{y}>` *&rarr;*&nbsp;`object:[]<{y}>`


Yield OBJECT for use with 'resume' in following pact step. The object is similar to database row objects, in that only the top level can be bound to in 'resume'; nested objects are converted to opaque JSON values.
```lisp
(yield { "amount": 100.0 })
```

## Database {#Database}

### create-table {#create-table}

*table*&nbsp;`table:[]<{row}>` *&rarr;*&nbsp;`string`


Create table TABLE.
```lisp
(create-table accounts)
```

Top level only: this function will fail if used in module code.


### describe-keyset {#describe-keyset}

*keyset*&nbsp;`string` *&rarr;*&nbsp;`value`


Get metadata for KEYSET.

Top level only: this function will fail if used in module code.


### describe-module {#describe-module}

*module*&nbsp;`string` *&rarr;*&nbsp;`value`


Get metadata for MODULE. Returns an object with 'name', 'hash', 'blessed', 'code', and 'keyset' fields.
```lisp
(describe-module 'my-module)
```

Top level only: this function will fail if used in module code.


### describe-table {#describe-table}

*table*&nbsp;`table:[]<{row}>` *&rarr;*&nbsp;`value`


Get metadata for TABLE. Returns an object with 'name', 'hash', 'blessed', 'code', and 'keyset' fields.
```lisp
(describe-table accounts)
```

Top level only: this function will fail if used in module code.


### insert {#insert}

*table*&nbsp;`table:[]<{row}>` *key*&nbsp;`string` *object*&nbsp;`object:[]<{row}>` *&rarr;*&nbsp;`string`


Write entry in TABLE for KEY of OBJECT column data, failing if data already exists for KEY.
```lisp
(insert accounts id { "balance": 0.0, "note": "Created account." })
```


### keylog {#keylog}

*table*&nbsp;`table:[]<{row}>` *key*&nbsp;`string` *txid*&nbsp;`integer` *&rarr;*&nbsp;`[object:[]*]`


Return updates to TABLE for a KEY in transactions at or after TXID, in a list of objects indexed by txid.
```lisp
(keylog accounts "Alice" 123485945)
```


### keys {#keys}

*table*&nbsp;`table:[]<{row}>` *&rarr;*&nbsp;`[string]`


Return all keys in TABLE.
```lisp
(keys accounts)
```


### read {#read}

*table*&nbsp;`table:[]<{row}>` *key*&nbsp;`string` *&rarr;*&nbsp;`object:[]<{row}>`

*table*&nbsp;`table:[]<{row}>` *key*&nbsp;`string` *columns*&nbsp;`[string]` *&rarr;*&nbsp;`object:[]<{row}>`


Read row from TABLE for KEY, returning database record object, or just COLUMNS if specified.
```lisp
(read accounts id ['balance 'ccy])
```


### select {#select}

*table*&nbsp;`table:[]<{row}>` *where*&nbsp;`row:object:[]<{row}> -> bool` *&rarr;*&nbsp;`[object:[]<{row}>]`

*table*&nbsp;`table:[]<{row}>` *columns*&nbsp;`[string]` *where*&nbsp;`row:object:[]<{row}> -> bool` *&rarr;*&nbsp;`[object:[]<{row}>]`


Select full rows or COLUMNS from table by applying WHERE to each row to get a boolean determining inclusion.
```lisp
(select people ['firstName,'lastName] (where 'name (= "Fatima")))
(select people (where 'age (> 30)))?
```


### txids {#txids}

*table*&nbsp;`table:[]<{row}>` *txid*&nbsp;`integer` *&rarr;*&nbsp;`[integer]`


Return all txid values greater than or equal to TXID in TABLE.
```lisp
(txids accounts 123849535)
```


### txlog {#txlog}

*table*&nbsp;`table:[]<{row}>` *txid*&nbsp;`integer` *&rarr;*&nbsp;`[value]`


Return all updates to TABLE performed in transaction TXID.
```lisp
(txlog accounts 123485945)
```


### update {#update}

*table*&nbsp;`table:[]<{row}>` *key*&nbsp;`string` *object*&nbsp;`object:[~]<{row}>` *&rarr;*&nbsp;`string`


Write entry in TABLE for KEY of OBJECT column data, failing if data does not exist for KEY.
```lisp
(update accounts id { "balance": (+ bal amount), "change": amount, "note": "credit" })
```


### with-default-read {#with-default-read}

*table*&nbsp;`table:[]<{row}>` *key*&nbsp;`string` *defaults*&nbsp;`object:[~]<{row}>` *bindings*&nbsp;`binding:[~]<{row}>` *&rarr;*&nbsp;`<a>`


Special form to read row from TABLE for KEY and bind columns per BINDINGS over subsequent body statements. If row not found, read columns from DEFAULTS, an object with matching key names.
```lisp
(with-default-read accounts id { "balance": 0, "ccy": "USD" } { "balance":= bal, "ccy":= ccy }
  (format "Balance for {} is {} {}" [id bal ccy]))
```


### with-read {#with-read}

*table*&nbsp;`table:[]<{row}>` *key*&nbsp;`string` *bindings*&nbsp;`binding:[]<{row}>` *&rarr;*&nbsp;`<a>`


Special form to read row from TABLE for KEY and bind columns per BINDINGS over subsequent body statements.
```lisp
(with-read accounts id { "balance":= bal, "ccy":= ccy }
  (format "Balance for {} is {} {}" [id bal ccy]))
```


### write {#write}

*table*&nbsp;`table:[]<{row}>` *key*&nbsp;`string` *object*&nbsp;`object:[]<{row}>` *&rarr;*&nbsp;`string`


Write entry in TABLE for KEY of OBJECT column data.
```lisp
(write accounts id { "balance": 100.0 })
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


### format-time {#format-time}

*format*&nbsp;`string` *time*&nbsp;`time` *&rarr;*&nbsp;`string`


Format TIME using FORMAT. See ["Time Formats" docs](pact-reference.html#time-formats) for supported formats.
```lisp
pact> (format-time "%F" (time "2016-07-22T12:00:00Z"))
"2016-07-22"
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


Construct time from UTCVAL using FORMAT. See ["Time Formats" docs](pact-reference.html#time-formats) for supported formats.
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

*x*&nbsp;`<a[integer,string,time,decimal,bool,[<l>],object:[]<{o}>,keyset]>` *y*&nbsp;`<a[integer,string,time,decimal,bool,[<l>],object:[]<{o}>,keyset]>` *&rarr;*&nbsp;`bool`


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

*x*&nbsp;`<a[string,[<l>],object:[]<{o}>]>` *y*&nbsp;`<a[string,[<l>],object:[]<{o}>]>` *&rarr;*&nbsp;`<a[string,[<l>],object:[]<{o}>]>`


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
{"bar": 2,"foo": 100}
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

*x*&nbsp;`<a[integer,string,time,decimal,bool,[<l>],object:[]<{o}>,keyset]>` *y*&nbsp;`<a[integer,string,time,decimal,bool,[<l>],object:[]<{o}>,keyset]>` *&rarr;*&nbsp;`bool`


True if X equals Y.
```lisp
pact> (= [1 2 3] [1 2 3])
true
pact> (= 'foo "foo")
true
pact> (= { 'a: 2 } { 'a: 2})
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


Boolean logic with short-circuit.
```lisp
pact> (and true false)
false
```


### and? {#and?}

*a*&nbsp;`x:<r> -> bool` *b*&nbsp;`x:<r> -> bool` *value*&nbsp;`<r>` *&rarr;*&nbsp;`bool`


Apply logical 'and' to the results of applying VALUE to A and B, with short-circuit.
```lisp
pact> (and? (> 20) (> 10) 15)
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


Exp of X.
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


Boolean not.
```lisp
pact> (not (> 1 2))
true
```


### not? {#not?}

*app*&nbsp;`x:<r> -> bool` *value*&nbsp;`<r>` *&rarr;*&nbsp;`bool`


Apply logical 'not' to the results of applying VALUE to APP.
```lisp
pact> (not? (> 20) 15)
false
```


### or {#or}

*x*&nbsp;`bool` *y*&nbsp;`bool` *&rarr;*&nbsp;`bool`


Boolean logic with short-circuit.
```lisp
pact> (or true false)
true
```


### or? {#or?}

*a*&nbsp;`x:<r> -> bool` *b*&nbsp;`x:<r> -> bool` *value*&nbsp;`<r>` *&rarr;*&nbsp;`bool`


Apply logical 'or' to the results of applying VALUE to A and B, with short-circuit.
```lisp
pact> (or? (> 20) (> 10) 15)
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

*name*&nbsp;`string` *&rarr;*&nbsp;`string`


Define keyset as NAME with KEYSET, or if unspecified, read NAME from message payload as keyset, similarly to 'read-keyset'. If keyset NAME already exists, keyset will be enforced before updating to new value.
```lisp
(define-keyset 'admin-keyset (read-keyset "keyset"))
```

Top level only: this function will fail if used in module code.


### enforce-keyset {#enforce-keyset}

*guard*&nbsp;`guard` *&rarr;*&nbsp;`bool`

*keysetname*&nbsp;`string` *&rarr;*&nbsp;`bool`


Execute GUARD, or defined keyset KEYSETNAME, to enforce desired predicate logic.
```lisp
(enforce-keyset 'admin-keyset)
(enforce-keyset row-guard)
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


Keyset predicate function to match any (at least 1) key in keyset.
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

## Capabilities {#Capabilities}

### compose-capability {#compose-capability}

*capability*&nbsp;` -> bool` *&rarr;*&nbsp;`bool`


Specifies and requests grant of CAPABILITY which is an application of a 'defcap' production, only valid within a (distinct) 'defcap' body, as a way to compose CAPABILITY with the outer capability such that the scope of the containing 'with-capability' call will "import" this capability. Thus, a call to '(with-capability (OUTER-CAP) OUTER-BODY)', where the OUTER-CAP defcap calls '(compose-capability (INNER-CAP))', will result in INNER-CAP being granted in the scope of OUTER-BODY.
```lisp
(compose-capability (TRANSFER src dest))
```


### create-module-guard {#create-module-guard}

*name*&nbsp;`string` *&rarr;*&nbsp;`guard`


Defines a guard by NAME that enforces the current module admin predicate.


### create-pact-guard {#create-pact-guard}

*name*&nbsp;`string` *&rarr;*&nbsp;`guard`


Defines a guard predicate by NAME that captures the results of 'pact-id'. At enforcement time, the success condition is that at that time 'pact-id' must return the same value. In effect this ensures that the guard will only succeed within the multi-transaction identified by the pact id.


### create-user-guard {#create-user-guard}

*data*&nbsp;`<a>` *predfun*&nbsp;`string` *&rarr;*&nbsp;`guard`


Defines a custom guard predicate, where DATA will be passed to PREDFUN at time of enforcement. PREDFUN is a valid name in the declaring environment. PREDFUN must refer to a pure function or enforcement will fail at runtime.


### enforce-guard {#enforce-guard}

*guard*&nbsp;`guard` *&rarr;*&nbsp;`bool`

*keysetname*&nbsp;`string` *&rarr;*&nbsp;`bool`


Execute GUARD, or defined keyset KEYSETNAME, to enforce desired predicate logic.
```lisp
(enforce-guard 'admin-keyset)
(enforce-guard row-guard)
```


### keyset-ref-guard {#keyset-ref-guard}

*keyset-ref*&nbsp;`string` *&rarr;*&nbsp;`guard`


Creates a guard for the keyset registered as KEYSET-REF with 'define-keyset'. Concrete keysets are themselves guard types; this function is specifically to store references alongside other guards in the database, etc.


### require-capability {#require-capability}

*capability*&nbsp;` -> bool` *&rarr;*&nbsp;`bool`


Specifies and tests for existing grant of CAPABILITY, failing if not found in environment.
```lisp
(require-capability (TRANSFER src dest))
```


### with-capability {#with-capability}

*capability*&nbsp;` -> bool` *body*&nbsp;`[*]` *&rarr;*&nbsp;`<a>`


Specifies and requests grant of CAPABILITY which is an application of a 'defcap' production. Given the unique token specified by this application, ensure that the token is granted in the environment during execution of BODY. 'with-capability' can only be called in the same module that declares the corresponding 'defcap', otherwise module-admin rights are required. If token is not present, the CAPABILITY is evaluated, with successful completion resulting in the installation/granting of the token, which will then be revoked upon completion of BODY. Nested 'with-capability' calls for the same token will detect the presence of the token, and will not re-apply CAPABILITY, but simply execute BODY. 'with-capability' cannot be called from within an evaluating defcap.
```lisp
(with-capability (UPDATE-USERS id) (update users id { salary: new-salary }))
```

## SPV {#SPV}

### verify-spv {#verify-spv}

*type*&nbsp;`string` *payload*&nbsp;`object:[]<in>` *&rarr;*&nbsp;`object:[]<out>`


Performs a platform-specific spv proof of type TYPE on PAYLOAD. The format of the PAYLOAD object depends on TYPE, as does the format of the return object. Platforms such as Chainweb will document the specific payload types and return values.
```lisp
(verify-spv "TXOUT" (read-msg "proof"))
```

## REPL-only functions {#repl-lib}

The following functions are loaded automatically into the interactive REPL, or within script files with a `.repl` extension. They are not available for blockchain-based execution.


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


### continue-pact {#continue-pact}

*pact-id*&nbsp;`integer` *step*&nbsp;`integer` *&rarr;*&nbsp;`string`

*pact-id*&nbsp;`integer` *step*&nbsp;`integer` *rollback*&nbsp;`bool` *&rarr;*&nbsp;`string`

*pact-id*&nbsp;`integer` *step*&nbsp;`integer` *rollback*&nbsp;`bool` *yielded*&nbsp;`object:[]<{y}>` *&rarr;*&nbsp;`string`


Continue previously-initiated pact identified by PACT-ID at STEP, optionally specifying ROLLBACK (default is false), and YIELDED value to be read with 'resume' (if not specified, uses yield in most recent pact exec, if any).
```lisp
(continue-pact 2 1)
(continue-pact 2 1 true)
(continue-pact 2 1 false { "rate": 0.9 })
```


### env-data {#env-data}

*json*&nbsp;`<a[integer,string,time,decimal,bool,[<l>],object:[]<{o}>,keyset,value]>` *&rarr;*&nbsp;`string`


Set transaction JSON data, either as encoded string, or as pact types coerced to JSON.
```lisp
pact> (env-data { "keyset": { "keys": ["my-key" "admin-key"], "pred": "keys-any" } })
"Setting transaction data"
```


### env-entity {#env-entity}

 *&rarr;*&nbsp;`string`

*entity*&nbsp;`string` *&rarr;*&nbsp;`string`


Set environment confidential ENTITY id, or unset with no argument.
```lisp
(env-entity "my-org")
(env-entity)
```


### env-gas {#env-gas}

 *&rarr;*&nbsp;`integer`

*gas*&nbsp;`integer` *&rarr;*&nbsp;`string`


Query gas state, or set it to GAS.


### env-gaslimit {#env-gaslimit}

*limit*&nbsp;`integer` *&rarr;*&nbsp;`string`


Set environment gas limit to LIMIT.


### env-gasmodel {#env-gasmodel}

*model*&nbsp;`string` *&rarr;*&nbsp;`string`


Update gas model to the model named MODEL.


### env-gasprice {#env-gasprice}

*price*&nbsp;`decimal` *&rarr;*&nbsp;`string`


Set environment gas price to PRICE.


### env-gasrate {#env-gasrate}

*rate*&nbsp;`integer` *&rarr;*&nbsp;`string`


Update gas model to charge constant RATE.


### env-hash {#env-hash}

*hash*&nbsp;`string` *&rarr;*&nbsp;`string`


Set current transaction hash. HASH must be a valid BLAKE2b 512-bit hash.
```lisp
pact> (env-hash (hash "hello"))
"Set tx hash to e4cfa39a3d37be31c59609e807970799caa68a19bfaa15135f165085e01d41a65ba1e1b146aeb6bd0092b49eac214c103ccfa3a365954bbbe52f74a2b3620c94"
```


### env-keys {#env-keys}

*keys*&nbsp;`[string]` *&rarr;*&nbsp;`string`


Set transaction signature KEYS.
```lisp
pact> (env-keys ["my-key" "admin-key"])
"Setting transaction keys"
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


### format-address {#format-address}

*scheme*&nbsp;`string` *public-key*&nbsp;`string` *&rarr;*&nbsp;`string`


Transform PUBLIC-KEY into an address (i.e. a Pact Runtime Public Key) depending on its SCHEME.


### json {#json}

*exp*&nbsp;`<a>` *&rarr;*&nbsp;`value`


Encode pact expression EXP as a JSON value. This is only needed for tests, as Pact values are automatically represented as JSON in API output. 
```lisp
pact> (json [{ "name": "joe", "age": 10 } {"name": "mary", "age": 25 }])
[{"age": 10.0,"name": "joe"} {"age": 25.0,"name": "mary"}]
```


### load {#load}

*file*&nbsp;`string` *&rarr;*&nbsp;`string`

*file*&nbsp;`string` *reset*&nbsp;`bool` *&rarr;*&nbsp;`string`


Load and evaluate FILE, resetting repl state beforehand if optional RESET is true.
```lisp
(load "accounts.repl")
```


### mock-spv {#mock-spv}

*type*&nbsp;`string` *payload*&nbsp;`object:[]*` *output*&nbsp;`object:[]*` *&rarr;*&nbsp;`string`


Mock a successful call to 'spv-verify' with TYPE and PAYLOAD to return OUTPUT.
```lisp
(mock-spv "TXOUT" { 'proof: "a54f54de54c54d89e7f" } { 'amount: 10.0, 'account: "Dave", 'chainId: 1 })
```


### pact-state {#pact-state}

 *&rarr;*&nbsp;`object:[]*`

*clear*&nbsp;`bool` *&rarr;*&nbsp;`object:[]*`


Inspect state from most recent pact execution. Returns object with fields 'pactId': pact ID; 'yield': yield result or 'false' if none; 'step': executed step; 'executed': indicates if step was skipped because entity did not match. With CLEAR argument, erases pact from repl state.
```lisp
(pact-state)
(pact-state true)
```


### print {#print}

*value*&nbsp;`<a>` *&rarr;*&nbsp;`string`


Output VALUE to terminal as unquoted, unescaped text.


### rollback-tx {#rollback-tx}

 *&rarr;*&nbsp;`string`


Rollback transaction.
```lisp
(rollback-tx)
```


### sig-keyset {#sig-keyset}

 *&rarr;*&nbsp;`keyset`


Convenience function to build a keyset from keys present in message signatures, using 'keys-all' as the predicate.


### test-capability {#test-capability}

*capability*&nbsp;` -> bool` *&rarr;*&nbsp;`string`


Specify and request grant of CAPABILITY. Once granted, CAPABILITY and any composed capabilities are in scope for the rest of the transaction. Allows direct invocation of capabilities, which is not available in the blockchain environment.
```lisp
(test-capability (MY-CAP))
```


### typecheck {#typecheck}

*module*&nbsp;`string` *&rarr;*&nbsp;`string`

*module*&nbsp;`string` *debug*&nbsp;`bool` *&rarr;*&nbsp;`string`


Typecheck MODULE, optionally enabling DEBUG output.


### verify {#verify}

*module*&nbsp;`string` *&rarr;*&nbsp;`string`


Verify MODULE, checking that all properties hold.

