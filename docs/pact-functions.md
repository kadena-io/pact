# Built-in Functions {#builtins}
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


### based-str-to-int {#based-str-to-int}

*base*&nbsp;`integer` *value*&nbsp;`string` *&rarr;*&nbsp;`integer`


Compute the integer value after change of base of a string of length <= 128 chars consisting of base-2 through base-16 (hexadecimal) characters. Only bases 2-16 are supported. 
```lisp
pact> (based-str-to-int 2 "abcdef")
"687"
```


### bind {#bind}

*src*&nbsp;`object:<{row}>` *binding*&nbsp;`binding:<{row}>` *&rarr;*&nbsp;`<a>`


Special form evaluates SRC to an object which is bound to with BINDINGS over subsequent body statements. 
```lisp
pact> (bind { "a": 1, "b": 2 } { "a" := a-value } a-value)
1
```


### compose {#compose}

*x*&nbsp;`(x:<a> -> <b>)` *y*&nbsp;`(x:<b> -> <c>)` *value*&nbsp;`<a>` *&rarr;*&nbsp;`<c>`


Compose X and Y, such that X operates on VALUE, and Y on the results of X. 
```lisp
pact> (filter (compose (length) (< 2)) ["my" "dog" "has" "fleas"])
["dog" "has" "fleas"]
```


### constantly {#constantly}

*value*&nbsp;`<a>` *ignore1*&nbsp;`<b>` *&rarr;*&nbsp;`<a>`

*value*&nbsp;`<a>` *ignore1*&nbsp;`<b>` *ignore2*&nbsp;`<c>` *&rarr;*&nbsp;`<a>`

*value*&nbsp;`<a>` *ignore1*&nbsp;`<b>` *ignore2*&nbsp;`<c>` *ignore3*&nbsp;`<d>` *&rarr;*&nbsp;`<a>`


Ignore (lazily) arguments IGNORE* and return VALUE. 
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


### drop {#drop}

*count*&nbsp;`integer` *list*&nbsp;`<a[[<l>],string]>` *&rarr;*&nbsp;`<a[[<l>],string]>`

*keys*&nbsp;`[string]` *object*&nbsp;`object:<{o}>` *&rarr;*&nbsp;`object:<{o}>`


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


Fail transaction with MSG if pure function TEST fails, or returns true. 
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


### filter {#filter}

*app*&nbsp;`(x:<a> -> bool)` *list*&nbsp;`[<a>]` *&rarr;*&nbsp;`[<a>]`


Filter LIST by applying APP to each element to get a boolean determining inclusion.
```lisp
pact> (filter (compose (length) (< 2)) ["my" "dog" "has" "fleas"])
["dog" "has" "fleas"]
```


### fold {#fold}

*app*&nbsp;`(x:<a> y:<b> -> <a>)` *init*&nbsp;`<a>` *list*&nbsp;`[<b>]` *&rarr;*&nbsp;`<a>`


Iteratively reduce LIST by applying APP to last result and element, starting with INIT. 
```lisp
pact> (fold (+) 0 [100 10 5])
115
```


### format {#format}

*template*&nbsp;`string` *vars*&nbsp;`list` *&rarr;*&nbsp;`string`


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


### hex-str-to-int {#hex-str-to-int}

*value*&nbsp;`string` *&rarr;*&nbsp;`integer`


Compute the integer value of a string of length <= 128 chars consisting of hexadecimal characters. 
```lisp
pact> (hex-str-to-int "abcdef12345")
