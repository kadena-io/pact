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


### chain-data {#chain-data}

 *&rarr;*&nbsp;`object:{public-chain-data}`


Get transaction public metadata. Returns an object with 'chain-id', 'block-height', 'block-time', 'sender', 'gas-limit', 'gas-price', and 'gas-fee' fields.
```lisp
pact> (chain-data)
