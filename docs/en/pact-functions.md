# Built-in Functions {#builtins}
## General {#General}

### CHARSET_ASCII {#CHARSET_ASCII}

Constant denoting the ASCII charset

Value: 
&nbsp;&nbsp;`CHARSET_ASCII:integer`


### CHARSET_LATIN1 {#CHARSET_LATIN1}

Constant denoting the Latin-1 charset ISO-8859-1

Value: 
&nbsp;&nbsp;`CHARSET_LATIN1:integer`


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

*src*&nbsp;`object:<{row}>` *binding*&nbsp;`binding:<{row}>` *&rarr;*&nbsp;`<a>`


Special form evaluates SRC to an object which is bound to with BINDINGS over subsequent body statements.
```lisp
pact> (bind { "a": 1, "b": 2 } { "a" := a-value } a-value)
1
```


### chain-data {#chain-data}

 *&rarr;*&nbsp;`object:{public-chain-data}`


Get transaction public metadata. Returns an object with 'chain-id', 'block-height', 'block-time', 'sender', 'gas-limit', 'gas-price', and 'gas-fee' fields.
```lisp
pact> (chain-data)
{"block-height": 0,"block-time": "1970-01-01T00:00:00Z","chain-id": "","gas-limit": 0,"gas-price": 0,"prev-block-hash": "","sender": ""}
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


### define-namespace {#define-namespace}

*namespace*&nbsp;`string` *guard*&nbsp;`guard` *&rarr;*&nbsp;`string`


Create a namespace called NAMESPACE where ownership and use of the namespace is controlled by GUARD. If NAMESPACE is already defined, then the guard previously defined in NAMESPACE will be enforced, and GUARD will be rotated in its place.
```lisp
(define-namespace 'my-namespace (read-keyset 'my-keyset))
```

Top level only: this function will fail if used in module code.


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
pact> (is-charset 'CHARSET_ASCII "hello world")
