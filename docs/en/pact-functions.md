# Built-in Functions {#builtins}
## General {#General}

### CHARSET_ASCII {#CHARSET_ASCII}

Constant denoting the ASCII charset

Constant: 
&nbsp;&nbsp;`CHARSET_ASCII:integer = 0`

### CHARSET_LATIN1 {#CHARSET_LATIN1}

Constant denoting the Latin-1 charset ISO-8859-1

Constant: 
&nbsp;&nbsp;`CHARSET_LATIN1:integer = 1`

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


### base64-decode {#base64-decode}

*string*&nbsp;`string` *&rarr;*&nbsp;`string`


Decode STRING from unpadded base64
```lisp
pact> (base64-decode "aGVsbG8gd29ybGQh")
"hello world!"
```


### base64-encode {#base64-encode}

*string*&nbsp;`string` *&rarr;*&nbsp;`string`


Encode STRING as unpadded base64
```lisp
pact> (base64-encode "hello world!")
"aGVsbG8gd29ybGQh"
```


### bind {#bind}

*src*&nbsp;`object:<{row}>` *binding*&nbsp;`binding:<{row}>` *&rarr;*&nbsp;`<a>`


Special form evaluates SRC to an object which is bound to with BINDINGS over subsequent body statements.
```lisp
pact> (bind { "a": 1, "b": 2 } { "a" := a-value } a-value)
1
```


### chain-data {#chain-data}

 *&rarr;*&nbsp;`object:{public-chain-data}`


Get transaction public metadata. Returns an object with 'chain-id', 'block-height', 'block-time', 'prev-block-hash', 'sender', 'gas-limit', 'gas-price', and 'gas-fee' fields.
```lisp
pact> (chain-data)
{"block-height": 0,"block-time": "1970-01-01T00:00:00Z","chain-id": "","gas-limit": 0,"gas-price": 0.0,"prev-block-hash": "","sender": ""}
```


### compose {#compose}

*x*&nbsp;`x:<a> -> <b>` *y*&nbsp;`x:<b> -> <c>` *value*&nbsp;`<a>` *&rarr;*&nbsp;`<c>`


Compose X and Y, such that X operates on VALUE, and Y on the results of X.
```lisp
pact> (filter (compose (length) (< 2)) ["my" "dog" "has" "fleas"])
["dog" "has" "fleas"]
```


### concat {#concat}

*str-list*&nbsp;`[string]` *&rarr;*&nbsp;`string`


Takes STR-LIST and concats each of the strings in the list, returning the resulting string
```lisp
pact> (concat ["k" "d" "a"])
"kda"
pact> (concat (map (+ " ") (str-to-list "abcde")))
" a b c d e"
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

*key*&nbsp;`<a>` *object*&nbsp;`object:<{o}>` *&rarr;*&nbsp;`bool`

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


### continue {#continue}

*value*&nbsp;`*` *&rarr;*&nbsp;`*`


Continue a previously started nested defpact.
```lisp
(continue (coin.transfer-crosschain "bob" "alice" 10.0))
```


### define-namespace {#define-namespace}

*namespace*&nbsp;`string` *user-guard*&nbsp;`guard` *admin-guard*&nbsp;`guard` *&rarr;*&nbsp;`string`


Create a namespace called NAMESPACE where ownership and use of the namespace is controlled by GUARD. If NAMESPACE is already defined, then the guard previously defined in NAMESPACE will be enforced, and GUARD will be rotated in its place.
```lisp
(define-namespace 'my-namespace (read-keyset 'user-ks) (read-keyset 'admin-ks))
```

Top level only: this function will fail if used in module code.


### describe-namespace {#describe-namespace}

*ns*&nbsp;`string` *&rarr;*&nbsp;`object:{described-namespace}`


Describe the namespace NS, returning a row object containing the user and admin guards of the namespace, as well as its name.
```lisp
(describe-namespace 'my-namespace)
```

Top level only: this function will fail if used in module code.


### distinct {#distinct}

*values*&nbsp;`[<a>]` *&rarr;*&nbsp;`[<a>]`


Returns from a homogeneous list of VALUES a list with duplicates removed. The original order of the values is preserved.
```lisp
pact> (distinct [3 3 1 1 2 2])
[3 1 2]
```


### drop {#drop}

*count*&nbsp;`integer` *list*&nbsp;`<a[[<l>],string]>` *&rarr;*&nbsp;`<a[[<l>],string]>`

*keys*&nbsp;`[string]` *object*&nbsp;`object:<{o}>` *&rarr;*&nbsp;`object:<{o}>`


Drop COUNT values from LIST (or string), or entries having keys in KEYS from OBJECT. If COUNT is negative, drop from end. If COUNT exceeds the interval (-2^63,2^63), it is truncated to that range.
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
<interactive>:0:0:Error: Chaos reigns
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


### enumerate {#enumerate}

*from*&nbsp;`integer` *to*&nbsp;`integer` *inc*&nbsp;`integer` *&rarr;*&nbsp;`[integer]`

*from*&nbsp;`integer` *to*&nbsp;`integer` *&rarr;*&nbsp;`[integer]`


Returns a sequence of numbers from FROM to TO (both inclusive) as a list. INC is the increment between numbers in the sequence. If INC is not given, it is assumed to be 1. Additionally, if INC is not given and FROM is greater than TO assume a value for INC of -1. If FROM equals TO, return the singleton list containing FROM, irrespective of INC's value. If INC is equal to zero, this function will return the singleton list containing FROM. If INC is such that FROM + INC > TO (when FROM < TO) or FROM + INC < TO (when FROM > TO) return the singleton list containing FROM. Lastly, if INC is such that FROM + INC < TO (when FROM < TO) or FROM + INC > TO (when FROM > TO), then this function fails.
```lisp
pact> (enumerate 0 10 2)
[0 2 4 6 8 10]
pact> (enumerate 0 10)
[0 1 2 3 4 5 6 7 8 9 10]
pact> (enumerate 10 0)
[10 9 8 7 6 5 4 3 2 1 0]
```


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


Compute BLAKE2b 256-bit hash of VALUE represented in unpadded base64-url. Strings are converted directly while other values are converted using their JSON representation. Non-value-level arguments are not allowed.
```lisp
pact> (hash "hello")
"Mk3PAn3UowqTLEQfNlol6GsXPe-kuOWJSCU0cbgbcs8"
pact> (hash { 'foo: 1 })
"h9BZgylRf_M4HxcBXr15IcSXXXSz74ZC2IAViGle_z4"
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


### int-to-str {#int-to-str}

*base*&nbsp;`integer` *val*&nbsp;`integer` *&rarr;*&nbsp;`string`


Represent integer VAL as a string in BASE. BASE can be 2-16, or 64 for unpadded base64URL. Only positive values are allowed for base64URL conversion.
```lisp
pact> (int-to-str 16 65535)
"ffff"
pact> (int-to-str 64 43981)
"q80"
```


### is-charset {#is-charset}

*charset*&nbsp;`integer` *input*&nbsp;`string` *&rarr;*&nbsp;`bool`


Check that a string INPUT conforms to the a supported character set CHARSET.       Character sets currently supported are: 'CHARSET_LATIN1' (ISO-8859-1), and         'CHARSET_ASCII' (ASCII). Support for sets up through ISO 8859-5 supplement will be added in the future.
```lisp
pact> (is-charset CHARSET_ASCII "hello world")
true
pact> (is-charset CHARSET_ASCII "I am nÖt ascii")
false
pact> (is-charset CHARSET_LATIN1 "I am nÖt ascii, but I am latin1!")
true
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

 *&rarr;*&nbsp;`string`


Return ID if called during current pact execution, failing if not.


### pact-version {#pact-version}

 *&rarr;*&nbsp;`string`


Obtain current pact build version.
```lisp
pact> (pact-version)
"4.7.1"
```

Top level only: this function will fail if used in module code.


### public-chain-data {#public-chain-data}

Schema type for data returned from 'chain-data'.

Fields:
&nbsp;&nbsp;`chain-id:string`
&nbsp;&nbsp;`block-height:integer`
&nbsp;&nbsp;`block-time:time`
&nbsp;&nbsp;`prev-block-hash:string`
&nbsp;&nbsp;`sender:string`
&nbsp;&nbsp;`gas-limit:integer`
&nbsp;&nbsp;`gas-price:decimal`


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


Read KEY from top level of message data body, or data body itself if not provided. Coerces value to their corresponding pact type: String -> string, Number -> integer, Boolean -> bool, List -> list, Object -> object.
```lisp
(defun exec ()
   (transfer (read-msg "from") (read-msg "to") (read-decimal "amount")))
```


### read-string {#read-string}

*key*&nbsp;`string` *&rarr;*&nbsp;`string`


Parse KEY string or number value from top level of message data body as string.
```lisp
(read-string "sender")
```


### remove {#remove}

*key*&nbsp;`string` *object*&nbsp;`object:<{o}>` *&rarr;*&nbsp;`object:<{o}>`


Remove entry for KEY from OBJECT.
```lisp
pact> (remove "bar" { "foo": 1, "bar": 2 })
{"foo": 1}
```


### resume {#resume}

*binding*&nbsp;`binding:<{r}>` *&rarr;*&nbsp;`<a>`


Special form binds to a yielded object value from the prior step execution in a pact. If yield step was executed on a foreign chain, enforce endorsement via SPV.


### reverse {#reverse}

*list*&nbsp;`[<a>]` *&rarr;*&nbsp;`[<a>]`


Reverse LIST.
```lisp
pact> (reverse [1 2 3])
[3 2 1]
```


### sort {#sort}

*values*&nbsp;`[<a>]` *&rarr;*&nbsp;`[<a>]`

*fields*&nbsp;`[string]` *values*&nbsp;`[object:<{o}>]` *&rarr;*&nbsp;`[object:<{o}>]`


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


Compute the integer value of STR-VAL in base 10, or in BASE if specified. STR-VAL can be up to 512 chars in length. BASE must be between 2 and 16, or 64 to perform unpadded base64url conversion. Each digit must be in the correct range for the base.
```lisp
pact> (str-to-int 16 "abcdef123456")
188900967593046
pact> (str-to-int "123456")
123456
pact> (str-to-int 64 "q80")
43981
```


### str-to-list {#str-to-list}

*str*&nbsp;`string` *&rarr;*&nbsp;`[string]`


Takes STR and returns a list of single character strings
```lisp
pact> (str-to-list "hello")
["h" "e" "l" "l" "o"]
pact> (concat (map (+ " ") (str-to-list "abcde")))
" a b c d e"
```


### take {#take}

*count*&nbsp;`integer` *list*&nbsp;`<a[[<l>],string]>` *&rarr;*&nbsp;`<a[[<l>],string]>`

*keys*&nbsp;`[string]` *object*&nbsp;`object:<{o}>` *&rarr;*&nbsp;`object:<{o}>`


Take COUNT values from LIST (or string), or entries having keys in KEYS from OBJECT. If COUNT is negative, take from end. If COUNT exceeds the interval (-2^63,2^63), it is truncated to that range.
```lisp
pact> (take 2 "abcd")
"ab"
pact> (take (- 3) [1 2 3 4 5])
[3 4 5]
pact> (take ['name] { 'name: "Vlad", 'active: false})
{"name": "Vlad"}
```


### try {#try}

*default*&nbsp;`<a>` *action*&nbsp;`<a>` *&rarr;*&nbsp;`<a>`


Attempt a pure ACTION, returning DEFAULT in the case of failure. Pure expressions are expressions which do not do i/o or work with non-deterministic state in contrast to impure expressions such as reading and writing to a table.
```lisp
pact> (try 3 (enforce (= 1 2) "this will definitely fail"))
3
(expect "impure expression fails and returns default" "default" (try "default" (with-read accounts id {'ccy := ccy}) ccy))
```


### tx-hash {#tx-hash}

 *&rarr;*&nbsp;`string`


Obtain hash of current transaction as a string.
```lisp
pact> (tx-hash)
"DldRwCblQ7Loqy6wYJnaodHl30d3j3eH-qtFzfEv46g"
```


### typeof {#typeof}

*x*&nbsp;`<a>` *&rarr;*&nbsp;`string`


Returns type of X as string.
```lisp
pact> (typeof "hello")
"string"
```


### where {#where}

*field*&nbsp;`string` *app*&nbsp;`x:<a> -> bool` *value*&nbsp;`object:<{row}>` *&rarr;*&nbsp;`bool`


Utility for use in 'filter' and 'select' applying APP to FIELD in VALUE.
```lisp
pact> (filter (where 'age (> 20)) [{'name: "Mary",'age: 30} {'name: "Juan",'age: 15}])
[{"name": "Juan","age": 15}]
```


### yield {#yield}

*object*&nbsp;`object:<{y}>` *&rarr;*&nbsp;`object:<{y}>`

*object*&nbsp;`object:<{y}>` *target-chain*&nbsp;`string` *&rarr;*&nbsp;`object:<{y}>`


Yield OBJECT for use with 'resume' in following pact step. With optional argument TARGET-CHAIN, target subsequent step to execute on targeted chain using automated SPV endorsement-based dispatch.
```lisp
(yield { "amount": 100.0 })
(yield { "amount": 100.0 } "some-chain-id")
```


### zip {#zip}

*f*&nbsp;`x:<a> y:<b> -> <c>` *list1*&nbsp;`[<a>]` *list2*&nbsp;`[<b>]` *&rarr;*&nbsp;`[<c>]`


Combine two lists with some function f, into a new list, the length of which is the length of the shortest list.
```lisp
pact> (zip (+) [1 2 3 4] [4 5 6 7])
[5 7 9 11]
pact> (zip (-) [1 2 3 4] [4 5 6])
[-3 -3 -3]
pact> (zip (+) [1 2 3] [4 5 6 7])
[5 7 9]
pact> (zip (lambda (x y) { 'x: x, 'y: y }) [1 2 3 4] [4 5 6 7])
[{"x": 1,"y": 4} {"x": 2,"y": 5} {"x": 3,"y": 6} {"x": 4,"y": 7}]
```

## Database {#Database}

### create-table {#create-table}

*table*&nbsp;`table:<{row}>` *&rarr;*&nbsp;`string`


Create table TABLE.
```lisp
(create-table accounts)
```

Top level only: this function will fail if used in module code.


### describe-keyset {#describe-keyset}

*keyset*&nbsp;`string` *&rarr;*&nbsp;`object:*`


Get metadata for KEYSET.

Top level only: this function will fail if used in module code.


### describe-module {#describe-module}

*module*&nbsp;`string` *&rarr;*&nbsp;`object:*`


Get metadata for MODULE. Returns an object with 'name', 'hash', 'blessed', 'code', and 'keyset' fields.
```lisp
(describe-module 'my-module)
```

Top level only: this function will fail if used in module code.


### describe-table {#describe-table}

*table*&nbsp;`table:<{row}>` *&rarr;*&nbsp;`object:*`


Get metadata for TABLE. Returns an object with 'name', 'hash', 'blessed', 'code', and 'keyset' fields.
```lisp
(describe-table accounts)
```

Top level only: this function will fail if used in module code.


### fold-db {#fold-db}

*table*&nbsp;`table:<{row}>` *qry*&nbsp;`a:string b:object:<{row}> -> bool` *consumer*&nbsp;`a:string b:object:<{row}> -> <b>` *&rarr;*&nbsp;`[<b>]`


Select rows from TABLE using QRY as a predicate with both key and value, and then accumulate results of the query in CONSUMER. Output is sorted by the ordering of keys.
```lisp
(let* 
 ((qry (lambda (k obj) true)) ;; select all rows
  (f (lambda (k obj) [(at 'firstName obj), (at 'b obj)]))
 )
 (fold-db people (qry) (f))
)
```


### insert {#insert}

*table*&nbsp;`table:<{row}>` *key*&nbsp;`string` *object*&nbsp;`object:<{row}>` *&rarr;*&nbsp;`string`


Write entry in TABLE for KEY of OBJECT column data, failing if data already exists for KEY.
```lisp
(insert accounts id { "balance": 0.0, "note": "Created account." })
```


### keylog {#keylog}

*table*&nbsp;`table:<{row}>` *key*&nbsp;`string` *txid*&nbsp;`integer` *&rarr;*&nbsp;`[object:*]`


Return updates to TABLE for a KEY in transactions at or after TXID, in a list of objects indexed by txid.
```lisp
(keylog accounts "Alice" 123485945)
```


### keys {#keys}

*table*&nbsp;`table:<{row}>` *&rarr;*&nbsp;`[string]`


Return all keys in TABLE.
```lisp
(keys accounts)
```


### read {#read}

*table*&nbsp;`table:<{row}>` *key*&nbsp;`string` *&rarr;*&nbsp;`object:<{row}>`

*table*&nbsp;`table:<{row}>` *key*&nbsp;`string` *columns*&nbsp;`[string]` *&rarr;*&nbsp;`object:<{row}>`


Read row from TABLE for KEY, returning database record object, or just COLUMNS if specified.
```lisp
(read accounts id ['balance 'ccy])
```


### select {#select}

*table*&nbsp;`table:<{row}>` *where*&nbsp;`row:object:<{row}> -> bool` *&rarr;*&nbsp;`[object:<{row}>]`

*table*&nbsp;`table:<{row}>` *columns*&nbsp;`[string]` *where*&nbsp;`row:object:<{row}> -> bool` *&rarr;*&nbsp;`[object:<{row}>]`


Select full rows or COLUMNS from table by applying WHERE to each row to get a boolean determining inclusion.
```lisp
(select people ['firstName,'lastName] (where 'name (= "Fatima")))
(select people (where 'age (> 30)))?
```


### txids {#txids}

*table*&nbsp;`table:<{row}>` *txid*&nbsp;`integer` *&rarr;*&nbsp;`[integer]`


Return all txid values greater than or equal to TXID in TABLE.
```lisp
(txids accounts 123849535)
```


### txlog {#txlog}

*table*&nbsp;`table:<{row}>` *txid*&nbsp;`integer` *&rarr;*&nbsp;`[object:*]`


Return all updates to TABLE performed in transaction TXID.
```lisp
(txlog accounts 123485945)
```


### update {#update}

*table*&nbsp;`table:<{row}>` *key*&nbsp;`string` *object*&nbsp;`object:~<{row}>` *&rarr;*&nbsp;`string`


Write entry in TABLE for KEY of OBJECT column data, failing if data does not exist for KEY.
```lisp
(update accounts id { "balance": (+ bal amount), "change": amount, "note": "credit" })
```


### with-default-read {#with-default-read}

*table*&nbsp;`table:<{row}>` *key*&nbsp;`string` *defaults*&nbsp;`object:~<{row}>` *bindings*&nbsp;`binding:~<{row}>` *&rarr;*&nbsp;`<a>`


Special form to read row from TABLE for KEY and bind columns per BINDINGS over subsequent body statements. If row not found, read columns from DEFAULTS, an object with matching key names.
```lisp
(with-default-read accounts id { "balance": 0, "ccy": "USD" } { "balance":= bal, "ccy":= ccy }
  (format "Balance for {} is {} {}" [id bal ccy]))
```


### with-read {#with-read}

*table*&nbsp;`table:<{row}>` *key*&nbsp;`string` *bindings*&nbsp;`binding:<{row}>` *&rarr;*&nbsp;`<a>`


Special form to read row from TABLE for KEY and bind columns per BINDINGS over subsequent body statements.
```lisp
(with-read accounts id { "balance":= bal, "ccy":= ccy }
  (format "Balance for {} is {} {}" [id bal ccy]))
```


### write {#write}

*table*&nbsp;`table:<{row}>` *key*&nbsp;`string` *object*&nbsp;`object:<{row}>` *&rarr;*&nbsp;`string`


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
23400.0
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

*x*&nbsp;`<a[integer,string,time,decimal,bool,[<l>],object:<{o}>,keyset,guard,module{}]>` *y*&nbsp;`<a[integer,string,time,decimal,bool,[<l>],object:<{o}>,keyset,guard,module{}]>` *&rarr;*&nbsp;`bool`


True if X does not equal Y.
```lisp
pact> (!= "hello" "goodbye")
true
```


### & {#&}

*x*&nbsp;`integer` *y*&nbsp;`integer` *&rarr;*&nbsp;`integer`


Compute bitwise X and Y.
```lisp
pact> (& 2 3)
2
pact> (& 5 -7)
1
```


### * {#star}

*x*&nbsp;`<a[integer,decimal]>` *y*&nbsp;`<a[integer,decimal]>` *&rarr;*&nbsp;`<a[integer,decimal]>`

*x*&nbsp;`<a[integer,decimal]>` *y*&nbsp;`<b[integer,decimal]>` *&rarr;*&nbsp;`decimal`


Multiply X by Y.
```lisp
pact> (* 0.5 10.0)
5.0
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
5.0
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

*x*&nbsp;`<a[integer,string,time,decimal,bool,[<l>],object:<{o}>,keyset,guard,module{}]>` *y*&nbsp;`<a[integer,string,time,decimal,bool,[<l>],object:<{o}>,keyset,guard,module{}]>` *&rarr;*&nbsp;`bool`


Compare alike terms for equality, returning TRUE if X is equal to Y. Equality comparisons will fail immediately on type mismatch, or if types are not value types.
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


### dec {#dec}

*x*&nbsp;`integer` *&rarr;*&nbsp;`decimal`


Cast an integer to a decimal value of integer X as decimal.
```lisp
pact> (dec 3)
3.0
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


### shift {#shift}

*x*&nbsp;`integer` *y*&nbsp;`integer` *&rarr;*&nbsp;`integer`


Shift X Y bits left if Y is positive, or right by -Y bits otherwise. Right shifts perform sign extension on signed number types; i.e. they fill the top bits with 1 if the x is negative and with 0 otherwise.
```lisp
pact> (shift 255 8)
65280
pact> (shift 255 -1)
127
pact> (shift -255 8)
-65280
pact> (shift -255 -1)
-128
```


### sqrt {#sqrt}

*x*&nbsp;`<a[integer,decimal]>` *&rarr;*&nbsp;`<a[integer,decimal]>`


Square root of X.
```lisp
pact> (sqrt 25)
5.0
```


### xor {#xor}

*x*&nbsp;`integer` *y*&nbsp;`integer` *&rarr;*&nbsp;`integer`


Compute bitwise X xor Y.
```lisp
pact> (xor 127 64)
63
pact> (xor 5 -7)
-4
```


### | {#|}

*x*&nbsp;`integer` *y*&nbsp;`integer` *&rarr;*&nbsp;`integer`


Compute bitwise X or Y.
```lisp
pact> (| 2 3)
3
pact> (| 5 -7)
-3
```


### ~ {#~}

*x*&nbsp;`integer` *&rarr;*&nbsp;`integer`


Reverse all bits in X.
```lisp
pact> (~ 15)
-16
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


### emit-event {#emit-event}

*capability*&nbsp;` -> bool` *&rarr;*&nbsp;`bool`


Emit CAPABILITY as event without evaluating body of capability. Fails if CAPABILITY is not @managed or @event.
```lisp
(emit-event (TRANSFER "Bob" "Alice" 12.0))
```


### enforce-guard {#enforce-guard}

*guard*&nbsp;`guard` *&rarr;*&nbsp;`bool`

*keysetname*&nbsp;`string` *&rarr;*&nbsp;`bool`


Execute GUARD, or defined keyset KEYSETNAME, to enforce desired predicate logic.
```lisp
(enforce-guard 'admin-keyset)
(enforce-guard row-guard)
```


### install-capability {#install-capability}

*capability*&nbsp;` -> bool` *&rarr;*&nbsp;`string`


Specifies, and provisions install of, a _managed_ CAPABILITY, defined in a 'defcap' in which a '@managed' tag designates a single parameter to be managed by a specified function. After install, CAPABILITY must still be brought into scope using 'with-capability', at which time the 'manager function' is invoked to validate the request. The manager function is of type 'managed:<p> requested:<p> -> <p>', where '<p>' indicates the type of the managed parameter, such that for '(defcap FOO (bar:string baz:integer) @managed baz FOO-mgr ...)', the manager function would be '(defun FOO-mgr:integer (managed:integer requested:integer) ...)'. Any capability matching the 'static' (non-managed) parameters will cause this function to be invoked with the current managed value and that of the requested capability. The function should perform whatever logic, presumably linear, to validate the request, and return the new managed value representing the 'balance' of the request. NOTE that signatures scoped to a managed capability cause the capability to be automatically provisioned for install similarly to one installed with this function.
```lisp
(install-capability (PAY "alice" "bob" 10.0))
```


### require-capability {#require-capability}

*capability*&nbsp;` -> bool` *&rarr;*&nbsp;`bool`


Specifies and tests for existing grant of CAPABILITY, failing if not found in environment.
```lisp
(require-capability (TRANSFER src dest))
```


### with-capability {#with-capability}

*capability*&nbsp;` -> bool` *body*&nbsp;`[*]` *&rarr;*&nbsp;`<a>`


Specifies and requests grant of _acquired_ CAPABILITY which is an application of a 'defcap' production. Given the unique token specified by this application, ensure that the token is granted in the environment during execution of BODY. 'with-capability' can only be called in the same module that declares the corresponding 'defcap', otherwise module-admin rights are required. If token is not present, the CAPABILITY is evaluated, with successful completion resulting in the installation/granting of the token, which will then be revoked upon completion of BODY. Nested 'with-capability' calls for the same token will detect the presence of the token, and will not re-apply CAPABILITY, but simply execute BODY. 'with-capability' cannot be called from within an evaluating defcap. Acquire of a managed capability results in emission of the equivalent event.
```lisp
(with-capability (UPDATE-USERS id) (update users id { salary: new-salary }))
```

## SPV {#SPV}

### verify-spv {#verify-spv}

*type*&nbsp;`string` *payload*&nbsp;`object:<in>` *&rarr;*&nbsp;`object:<out>`


Performs a platform-specific spv proof of type TYPE on PAYLOAD. The format of the PAYLOAD object depends on TYPE, as does the format of the return object. Platforms such as Chainweb will document the specific payload types and return values.
```lisp
(verify-spv "TXOUT" (read-msg "proof"))
```

## Commitments {#Commitments}

### decrypt-cc20p1305 {#decrypt-cc20p1305}

*ciphertext*&nbsp;`string` *nonce*&nbsp;`string` *aad*&nbsp;`string` *mac*&nbsp;`string` *public-key*&nbsp;`string` *secret-key*&nbsp;`string` *&rarr;*&nbsp;`string`


Perform decryption of CIPHERTEXT using the CHACHA20-POLY1305 Authenticated Encryption with Associated Data (AEAD) construction described in IETF RFC 7539. CIPHERTEXT is an unpadded base64url string. NONCE is a 12-byte base64 string. AAD is base64 additional authentication data of any length. MAC is the "detached" base64 tag value for validating POLY1305 authentication. PUBLIC-KEY and SECRET-KEY are base-16 Curve25519 values to form the DH symmetric key.Result is unpadded base64URL.
```lisp
(decrypt-cc20p1305 ciphertext nonce aad mac pubkey privkey)
```


### validate-keypair {#validate-keypair}

*public*&nbsp;`string` *secret*&nbsp;`string` *&rarr;*&nbsp;`bool`


Enforce that the Curve25519 keypair of (PUBLIC,SECRET) match. Key values are base-16 strings of length 32.
```lisp
(validate-keypair pubkey privkey)
```

## Guards {#Guards}

### create-capability-guard {#create-capability-guard}

*capability*&nbsp;` -> bool` *&rarr;*&nbsp;`guard`


Creates a guard that will enforce that CAPABILITY is acquired.
```lisp
(create-capability-guard (BANK_DEBIT 10.0))
```


### create-capability-pact-guard {#create-capability-pact-guard}

*capability*&nbsp;` -> bool` *&rarr;*&nbsp;`guard`


Creates a guard that will enforce that CAPABILITY is acquired and that the currently-executing defpact is operational.
```lisp
(create-capability-pact-guard (ESCROW owner))
```


### create-module-guard {#create-module-guard}

*name*&nbsp;`string` *&rarr;*&nbsp;`guard`


Defines a guard by NAME that enforces the current module admin predicate.


### create-pact-guard {#create-pact-guard}

*name*&nbsp;`string` *&rarr;*&nbsp;`guard`


Defines a guard predicate by NAME that captures the results of 'pact-id'. At enforcement time, the success condition is that at that time 'pact-id' must return the same value. In effect this ensures that the guard will only succeed within the multi-transaction identified by the pact id.


### create-principal {#create-principal}

*guard*&nbsp;`guard` *&rarr;*&nbsp;`string`


Create a principal which unambiguously identifies GUARD.
```lisp
(create-principal (read-keyset 'keyset))
(create-principal (keyset-ref-guard 'keyset))
(create-principal (create-module-guard 'module-guard))
(create-principal (create-user-guard 'user-guard))
(create-principal (create-pact-guard 'pact-guard))
```


### create-user-guard {#create-user-guard}

*closure*&nbsp;` -> bool` *&rarr;*&nbsp;`guard`


Defines a custom guard CLOSURE whose arguments are strictly evaluated at definition time, to be supplied to indicated function at enforcement time.


### is-principal {#is-principal}

*principal*&nbsp;`string` *&rarr;*&nbsp;`bool`


Tell whether PRINCIPAL string conforms to the principal format without proving validity.
```lisp
(enforce   (is-principal 'k:462e97a099987f55f6a2b52e7bfd52a36b4b5b470fed0816a3d9b26f9450ba69)   "Invalid account structure: non-principal account")
```


### keyset-ref-guard {#keyset-ref-guard}

*keyset-ref*&nbsp;`string` *&rarr;*&nbsp;`guard`


Creates a guard for the keyset registered as KEYSET-REF with 'define-keyset'. Concrete keysets are themselves guard types; this function is specifically to store references alongside other guards in the database, etc.


### typeof-principal {#typeof-principal}

*principal*&nbsp;`string` *&rarr;*&nbsp;`string`


Return the protocol type of a given PRINCIPAL value. If input value is not a principal type, then the empty string is returned.
```lisp
(typeof-principal 'k:462e97a099987f55f6a2b52e7bfd52a36b4b5b470fed0816a3d9b26f9450ba69)
```


### validate-principal {#validate-principal}

*guard*&nbsp;`guard` *principal*&nbsp;`string` *&rarr;*&nbsp;`bool`


Validate that PRINCIPAL unambiguously identifies GUARD.
```lisp
(enforce (validate-principal (read-keyset 'keyset) account) "Invalid account ID")
```

## Zk {#Zk}

### pairing-check {#pairing-check}

*points-g1*&nbsp;`[<a>]` *points-g2*&nbsp;`[<b>]` *&rarr;*&nbsp;`bool`


Perform pairing and final exponentiation points in G1 and G2 in BN254, check if the result is 1


### point-add {#point-add}

*type*&nbsp;`string` *point1*&nbsp;`<a>` *point2*&nbsp;`<a>` *&rarr;*&nbsp;`<a>`


Add two points together that lie on the curve BN254. Point addition either in Fq or in Fq2
```lisp
pact> (point-add 'g1 {'x: 1, 'y: 2}  {'x: 1, 'y: 2})
{"x": 1368015179489954701390400359078579693043519447331113978918064868415326638035,"y": 9918110051302171585080402603319702774565515993150576347155970296011118125764}
```


### scalar-mult {#scalar-mult}

*type*&nbsp;`string` *point1*&nbsp;`<a>` *scalar*&nbsp;`integer` *&rarr;*&nbsp;`<a>`


Multiply a point that lies on the curve BN254 by an integer value
```lisp
pact> (scalar-mult 'g1 {'x: 1, 'y: 2} 2)
{"x": 1368015179489954701390400359078579693043519447331113978918064868415326638035,"y": 9918110051302171585080402603319702774565515993150576347155970296011118125764}
```

## REPL-only functions {#repl-lib}

The following functions are loaded automatically into the interactive REPL, or within script files with a `.repl` extension. They are not available for blockchain-based execution.


### begin-tx {#begin-tx}

 *&rarr;*&nbsp;`string`

*name*&nbsp;`string` *&rarr;*&nbsp;`string`


Begin transaction with optional NAME.
```lisp
pact> (begin-tx "load module")
"Begin Tx 0: load module"
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
pact> (begin-tx) (commit-tx)
"Commit Tx 0"
```


### continue-pact {#continue-pact}

*step*&nbsp;`integer` *&rarr;*&nbsp;`string`

*step*&nbsp;`integer` *rollback*&nbsp;`bool` *&rarr;*&nbsp;`string`

*step*&nbsp;`integer` *rollback*&nbsp;`bool` *pact-id*&nbsp;`string` *&rarr;*&nbsp;`string`

*step*&nbsp;`integer` *rollback*&nbsp;`bool` *pact-id*&nbsp;`string` *yielded*&nbsp;`object:<{y}>` *&rarr;*&nbsp;`string`


Continue previously-initiated pact identified STEP, optionally specifying ROLLBACK (default is false), PACT-ID of the pact to be continued (defaults to the pact initiated in the current transaction, if one is present), and YIELDED value to be read with 'resume' (if not specified, uses yield in most recent pact exec, if any).
```lisp
(continue-pact 1)
(continue-pact 1 true)
(continue-pact 1 false "[pact-id-hash]"))
(continue-pact 2 1 false "[pact-id-hash]" { "rate": 0.9 })
```


### env-chain-data {#env-chain-data}

*new-data*&nbsp;`object:~{public-chain-data}` *&rarr;*&nbsp;`string`


Update existing entries of 'chain-data' with NEW-DATA, replacing those items only.
```lisp
pact> (env-chain-data { "chain-id": "TestNet00/2", "block-height": 20 })
"Updated public metadata"
```


### env-data {#env-data}

*json*&nbsp;`<a[integer,string,time,decimal,bool,[<l>],object:<{o}>,keyset]>` *&rarr;*&nbsp;`string`


Set transaction JSON data, either as encoded string, or as pact types coerced to JSON.
```lisp
pact> (env-data { "keyset": { "keys": ["my-key" "admin-key"], "pred": "keys-any" } })
"Setting transaction data"
```


### env-dynref {#env-dynref}

*iface*&nbsp;`module` *impl*&nbsp;`module{}` *&rarr;*&nbsp;`string`

 *&rarr;*&nbsp;`string`


Substitute module IMPL in any dynamic usages of IFACE in typechecking and analysis. With no arguments, remove all substitutions.
```lisp
(env-dynref fungible-v2 coin)
```


### env-enable-repl-natives {#env-enable-repl-natives}

*enable*&nbsp;`bool` *&rarr;*&nbsp;`string`


Control whether REPL native functions are allowed in module code. When enabled, fixture functions like 'env-sigs' are allowed in module code.
```lisp
pact> (env-enable-repl-natives true)
"Repl natives enabled"
```


### env-entity {#env-entity}

 *&rarr;*&nbsp;`string`

*entity*&nbsp;`string` *&rarr;*&nbsp;`string`


Set environment confidential ENTITY id, or unset with no argument.
```lisp
(env-entity "my-org")
(env-entity)
```


### env-events {#env-events}

*clear*&nbsp;`bool` *&rarr;*&nbsp;`[object:*]`


Retreive any accumulated events and optionally clear event state. Object returned has fields 'name' (fully-qualified event name), 'params' (event parameters), 'module-hash' (hash of emitting module).
```lisp
(env-events true)
```


### env-exec-config {#env-exec-config}

*flags*&nbsp;`[string]` *&rarr;*&nbsp;`[string]`

 *&rarr;*&nbsp;`[string]`


Queries, or with arguments, sets execution config flags. Valid flags: ["AllowReadInLocal","DisableHistoryInTransactionalMode","DisableInlineMemCheck","DisableModuleInstall","DisableNewTrans","DisablePact40","DisablePact420","DisablePact43","DisablePact431","DisablePact44","DisablePact45","DisablePact46","DisablePact47","DisablePactEvents","DisableRuntimeReturnTypeChecking","EnforceKeyFormats","OldReadOnlyBehavior","PreserveModuleIfacesBug","PreserveModuleNameBug","PreserveNsModuleInstallBug","PreserveShowDefs"]
```lisp
pact> (env-exec-config ['DisableHistoryInTransactionalMode]) (env-exec-config)
["DisableHistoryInTransactionalMode"]
```


### env-gas {#env-gas}

 *&rarr;*&nbsp;`integer`

*gas*&nbsp;`integer` *&rarr;*&nbsp;`string`


Query gas state, or set it to GAS. Note that certain plaforms may charge additional gas that is not captured by the interpreter gas model, such as an overall transaction-size cost.
```lisp
pact> (env-gasmodel "table") (env-gaslimit 10) (env-gas 0) (map (+ 1) [1 2 3]) (env-gas)
7
```


### env-gaslimit {#env-gaslimit}

*limit*&nbsp;`integer` *&rarr;*&nbsp;`string`


Set environment gas limit to LIMIT.


### env-gaslog {#env-gaslog}

 *&rarr;*&nbsp;`string`


Enable and obtain gas logging. Bracket around the code whose gas logs you want to inspect.
```lisp
pact> (env-gasmodel "table") (env-gaslimit 10) (env-gaslog) (map (+ 1) [1 2 3]) (env-gaslog)
["TOTAL: 7" "map:GUnreduced:currTotalGas=4: 4" "+:GUnreduced:currTotalGas=5: 1" ":GIntegerOpCost:(1, ):(1, ):currTotalGas=5: 0" "+:GUnreduced:currTotalGas=6: 1" ":GIntegerOpCost:(1, ):(2, ):currTotalGas=6: 0" "+:GUnreduced:currTotalGas=7: 1" ":GIntegerOpCost:(1, ):(3, ):currTotalGas=7: 0"]
```


### env-gasmodel {#env-gasmodel}

*model*&nbsp;`string` *&rarr;*&nbsp;`string`

 *&rarr;*&nbsp;`string`

*model*&nbsp;`string` *rate*&nbsp;`integer` *&rarr;*&nbsp;`string`


Update or query current gas model. With just MODEL, "table" is supported; with MODEL and RATE, 'fixed' is supported. With no args, output current model.
```lisp
pact> (env-gasmodel)
"Current gas model is 'fixed 0': constant rate gas model with fixed rate 0"
pact> (env-gasmodel 'table)
"Set gas model to table-based cost model"
pact> (env-gasmodel 'fixed 1)
"Set gas model to constant rate gas model with fixed rate 1"
```


### env-gasprice {#env-gasprice}

*price*&nbsp;`decimal` *&rarr;*&nbsp;`string`


Set environment gas price to PRICE.


### env-gasrate {#env-gasrate}

*rate*&nbsp;`integer` *&rarr;*&nbsp;`string`


Update gas model to charge constant RATE.


### env-hash {#env-hash}

*hash*&nbsp;`string` *&rarr;*&nbsp;`string`


Set current transaction hash. HASH must be an unpadded base64-url encoded BLAKE2b 256-bit hash.
```lisp
pact> (env-hash (hash "hello"))
"Set tx hash to Mk3PAn3UowqTLEQfNlol6GsXPe-kuOWJSCU0cbgbcs8"
```


### env-keys {#env-keys}

*keys*&nbsp;`[string]` *&rarr;*&nbsp;`string`


DEPRECATED in favor of 'set-sigs'. Set transaction signer KEYS. See 'env-sigs' for setting keys with associated capabilities.
```lisp
pact> (env-keys ["my-key" "admin-key"])
"Setting transaction keys"
```


### env-namespace-policy {#env-namespace-policy}

*allow-root*&nbsp;`bool` *ns-policy-fun*&nbsp;`ns:string ns-admin:guard -> bool` *&rarr;*&nbsp;`string`


Install a managed namespace policy specifying ALLOW-ROOT and NS-POLICY-FUN.
```lisp
(env-namespace-policy (my-ns-policy-fun))
```


### env-sigs {#env-sigs}

*sigs*&nbsp;`[object:*]` *&rarr;*&nbsp;`string`


Set transaction signature keys and capabilities. SIGS is a list of objects with "key" specifying the signer key, and "caps" specifying a list of associated capabilities.
```lisp
(env-sigs [{'key: "my-key", 'caps: [(accounts.USER_GUARD "my-account")]}, {'key: "admin-key", 'caps: []}
```


### env-simulate-onchain {#env-simulate-onchain}

*on-chain*&nbsp;`bool` *&rarr;*&nbsp;`string`


Set a flag to simulate on-chain behavior that differs from the repl, in particular for observing things like errors and stack traces.
```lisp
(env-simulate-onchain true)
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

*doc*&nbsp;`string` *err*&nbsp;`string` *exp*&nbsp;`<a>` *&rarr;*&nbsp;`string`


Evaluate EXP and succeed only if it throws an error.
```lisp
pact> (expect-failure "Enforce fails on false" (enforce false "Expected error"))
"Expect failure: success: Enforce fails on false"
pact> (expect-failure "Enforce fails with message" "Expected error" (enforce false "Expected error"))
"Expect failure: success: Enforce fails with message"
```


### expect-that {#expect-that}

*doc*&nbsp;`string` *pred*&nbsp;`value:<a> -> bool` *exp*&nbsp;`<a>` *&rarr;*&nbsp;`string`


Evaluate EXP and succeed if value passes predicate PRED.
```lisp
pact> (expect-that "addition" (< 2) (+ 1 2))
"Expect-that: success: addition"
pact> (expect-that "addition" (> 2) (+ 1 2))
"FAILURE: addition: did not satisfy (> 2) : 3:integer"
```


### format-address {#format-address}

*scheme*&nbsp;`string` *public-key*&nbsp;`string` *&rarr;*&nbsp;`string`


Transform PUBLIC-KEY into an address (i.e. a Pact Runtime Public Key) depending on its SCHEME.


### load {#load}

*file*&nbsp;`string` *&rarr;*&nbsp;`string`

*file*&nbsp;`string` *reset*&nbsp;`bool` *&rarr;*&nbsp;`string`


Load and evaluate FILE, resetting repl state beforehand if optional RESET is true.
```lisp
(load "accounts.repl")
```


### mock-spv {#mock-spv}

*type*&nbsp;`string` *payload*&nbsp;`object:*` *output*&nbsp;`object:*` *&rarr;*&nbsp;`string`


Mock a successful call to 'spv-verify' with TYPE and PAYLOAD to return OUTPUT.
```lisp
(mock-spv "TXOUT" { 'proof: "a54f54de54c54d89e7f" } { 'amount: 10.0, 'account: "Dave", 'chainId: "1" })
```


### pact-state {#pact-state}

 *&rarr;*&nbsp;`object:*`

*clear*&nbsp;`bool` *&rarr;*&nbsp;`object:*`


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
pact> (begin-tx "Third Act") (rollback-tx)
"Rollback Tx 0: Third Act"
```


### sig-keyset {#sig-keyset}

 *&rarr;*&nbsp;`keyset`


Convenience function to build a keyset from keys present in message signatures, using 'keys-all' as the predicate.


### test-capability {#test-capability}

*capability*&nbsp;` -> bool` *&rarr;*&nbsp;`string`


Acquire (if unmanaged) or install (if managed) CAPABILITY. CAPABILITY and any composed capabilities are in scope for the rest of the transaction.
```lisp
(test-capability (MY-CAP))
```


### typecheck {#typecheck}

*module*&nbsp;`string` *&rarr;*&nbsp;`string`

*module*&nbsp;`string` *debug*&nbsp;`bool` *&rarr;*&nbsp;`string`


Typecheck MODULE, optionally enabling DEBUG output.


### verify {#verify}

*module*&nbsp;`string` *debug*&nbsp;`bool` *&rarr;*&nbsp;`string`


Verify MODULE, checking that all properties hold. Optionally, if DEBUG is set to true, write debug output to "pact-verify-MODULE" directory.
```lisp
(verify "module")
(verify "module" true)
```


### with-applied-env {#with-applied-env}

*exec*&nbsp;`<a>` *&rarr;*&nbsp;`<a>`


Evaluate EXEC with any pending environment changes applied. Normally, environment changes must execute at top-level for the change to take effect. This allows scoped application of non-toplevel environment changes.
```lisp
pact> (let ((a 1)) (env-data { 'b: 1 }) (with-applied-env (+ a (read-integer 'b))))
2
```

