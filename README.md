The Pact Programming Language
===

This is the source code repository for [Pact](http://kadena.io/pact), a language for developing
smart contracts to run on a [fast deterministic blockchain](http://kadena.io).

Quick Start: Browser
---

The easiest way to try Pact is [in the browser](http://kadena.io/try-pact).

Quick Start: Atom
---

To start writing
smart contracts, the best way to go is to use the [Atom editor](https://atom.io) and install
`language-pact` using the [package manager](http://flight-manual.atom.io/using-atom/sections/atom-packages/).

You'll then need to install the Pact binary. Prebuilt binaries are available for the following OSs:

| OS | Download |
| --- | -------- |
| Mac OS 10.10 (Yosemite) | http://kadena.io/pact/download/osx1010/pact |
| | md5: sdkjfhsdhjf |
| Mac OS 10.11 (El Capitan) | http://kadena.io/pact/download/osx1011/pact |
| | md5: sdkjhsdkjdgsh |

Once you have downloaded the binary, put it somewhere in your PATH.

Test by issuing `pact` in a terminal. Try out some commands:

```
$ pact
pact> (+ 1 2)
3
pact> (+ "hello, " "world")
"hello world"
```

There are [examples](examples/) in this source repository of working smart contracts which you can load into Atom to get started.

Building
---

To build, you'll need a working [Haskell Stack install](https://docs.haskellstack.org/en/stable/README/#how-to-install) . After which, building is as simple as 'stack build'.

To install for use with Atom and the command line, issue 'stack install' and then either add `$HOME/.local/bin` to your path, or symlink `$HOME/.local/bin/pact` somewhere in your PATH.

Builing with GHCJS
---

To build pact for browser use, add `--stack-yaml stack-ghcjs.yaml` to your stack commands. The script [web/build.sh](web/build.sh) builds Pact and minifies it; you'll need the [closure compiler](https://www.npmjs.com/package/google-closure-compiler) and a Java Runtime Environment installed.

More information
---

[Pact Reference Docs](http://pact.readthedocs.io/en/latest/)

[Pact Whitepaper](http://kadena.io/docs/Kadena-PactWhitepaper-Oct2016.pdf)

License
---

This code is distributed under the terms of the BSD3 license. See [LICENSE](LICENSE) for details.
 
