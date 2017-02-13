# Pact Smart Contract Language API JavaScript wrappers

This package is here to help make interaction with both Pact's development server & a ScalableBFT cluster running Pact easy.
The API's for each are the same. For information about Pact & ScalableBFT please see [kadena.io](kadena.io) or [github.com/kadena-io/pact](github.com/kadena-io/pact).

For example usage, see [github.com/kadena-io/pact-todomvc](github.com/kadena-io/pact-todomvc)

## Import

`pact-lang-api.js` should work as expected as a regular node dependency.

`<script src="pact-lang-api-global.min.js"></script>` will import the library and inject it as a top-level definition called `Pact`

## Functions

### Crypto

Converting between binary & hex formats:

```
Pact.crypto.binToHex(<Uint8Array>) -> string
Pact.crypto.hexToBin(string) -> Uint8Array
```

Hashing (blake2b):

```
Pact.crypto.hash(string) -> string
```

PPK Signing is done via TweetNacl but with ed25519-donna style keys, represented as hex.
blake2b is used to hash the message (<string>), and TweetNacl then signs the hash.

```
Pact.crypto.genKeyPair() -> {"publicKey": <string>, "secretKey": <string>}
Pact.crypto.sign(<string>, keyPair) -> {"hash": <string>, "sig":<string>, "pubKey":<string>}
Pact.crypto.toTweetNaclSecretKey(keyPair) -> <Uint8Array>
```

### Language Expression Construction

A helper function for constructing native Pact commands.

```
Pact.lang.mkExp(<string>, *args) -> <string>
  ex: mkExp("todos.edit-todo", 1, "bar") -> '(todos.edit-todo 1 "bar")'
```

NB: `JSON.stringify`, which is used here, generally converts floating point numbers correctly but fails for high precision scientific numbers < 0; you will need to manually convert them.
e.g. `JSON.stringify(.0000001) -> '1e-7'` is incorrect as Pact has infinite precision decimals but does not interpret scientific numbers, so the proper conversion would be `JSON.stringify(.0000001) -> '0.0000001'`


### Simple API

A simplified set of functions for working with the api.

```
## send as POST to /api/send
Pact.simple.exec.createCommand([keyPair], <nonce: string>, <pactCode: string>, <envData: object>) -> {"cmds":[...]}

## send as POST to /api/poll
Pact.simple.exec.createPollRequest({"cmds": [...]}) -> {"requestKeys": [...]}

## send as POST to /api/listen
Pact.simple.exec.createListenRequest({"cmds": [...]}) -> {"requestKey": <string>}
```

### Low Level API

Lower level/internal functions to aid in the construction of JSON blobs that the API endpoints expect.
You probably want to use the `Pact.simple` functions instead of these.

```
Pact.api.mkSingleCmd([signatures],{cmd-object}) -> {"hash":<string>, "sigs":[signatures], "cmd":cmd}
Pact.api.mkPublicSend([cmd]) -> {"cmds":[cmd]} \\ send as POST to /api/poll
```
