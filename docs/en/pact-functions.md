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


Create a namespace called NAMESPACE for a given GUARD. All expressions that occur  in a given transaction will be tied to NAMESPACE, and may be accessed using the toplevel  call (namespace NAMESPACE) when GUARD is in scope. If NAMESPACE is already defined, then  the guard previously defined in NAMESPACE will be enforced, and GUARD will be rotated in  its place. 
```lisp
pact> (define-namespace "my-namespace" 'my-guard)
