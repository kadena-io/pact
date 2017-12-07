## Note: this is a fork of the original project - probably best to use the original

I am likely to sometimes break the code here :-)

Better to use the original: repository for [Pact](http://kadena.io/pact)

The Pact Programming Language
===


This is a fork of the kadena.io/pact project. Pact is a language for developing
smart contracts to run on a [fast deterministic blockchain](http://kadena.io).

Quick Start: Browser
---

The easiest way to try Pact is [in the browser](http://kadena.io/try-pact).

Quick Start: Atom
---

To start writing
smart contracts, the best way to go is to use the [Atom editor](https://atom.io) and install
`language-pact` using the [package manager](http://flight-manual.atom.io/using-atom/sections/atom-packages/).

Installing Pact with homebrew (OSX only)
---

You'll then need to install the Pact interpreter/REPL. On Mac, the easiest way is with homebrew.

```
brew install kadena-io/pact/pact
```

Installing Pact with Binary Distributions
---

[Prebuilt binaries](http://kadena.io/pact/downloads.html) are
available, or see [building](#Building) below.

Once you have downloaded the binary, put it somewhere in your PATH.


Verifying Install
---

Test by issuing `pact` in a terminal. Try out some commands:

```
$ pact
pact> (+ 1 2)
3
pact> (+ "hello, " "world")
"hello world"
```

There are [examples](examples/) in this source repository of working smart contracts which you can load into Atom to get started.

Quick Start: Pact REST API Server
---

Pact now features a full REST API HTTP server and SQLite database implementation, making
blockchain application development painless and easy. The Pact server simulates a single-node
blockchain environment, with the same API supported by the Kadena ScalableBFT blockchain.

To start the server, issue `pact --serve CONFIG` or `pact -s CONFIG`, where CONFIG is a valid [config.yaml](config.yaml).

#### REST API Docs
The REST API is documented at [http://pact-language.readthedocs.io/en/latest/pact-reference.html#rest-api](http://pact-language.readthedocs.io/en/latest/pact-reference.html#rest-api).


#### Related Projects

* The [pact-lang-api npm package](https://www.npmjs.com/package/pact-lang-api) provides a JavaScript library to aid interaction with the API.

* The [pact-todomvc](https://github.com/kadena-io/pact-todomvc) is a working demonstration.

#### Hosting static files.

The HTTP server will host any static files it finds in its working directory.

#### Config file format

The pact dev server (pact-serve) requires a configuration Yaml file (e.g. server.conf) to operate. The documentation for it is:

```
➜  pact git:(feat/dev-server) pact --serve --help
Config file is YAML format with the following properties:
port       - HTTP server port
persistDir - Directory for database files.
             If ommitted, runs in-memory only.
logDir     - Directory for HTTP logs
pragmas    - SQLite pragmas to use with persistence DBs
verbose    - [True|False] Provide extra logging information
```

#### Replay from disk.

When running pact-serve with persistence enabled, the server automatically replays from the database
`commands.sqlite` in the persist dir. To prevent replay, simply delete this file before starting the server.

Building
---

To build, you'll need a working [Haskell Stack install](https://docs.haskellstack.org/en/stable/README/#how-to-install) . After which, building is as simple as 'stack build'.

To install for use with Atom and the command line, issue 'stack install' and then either add `$HOME/.local/bin` to your path, or symlink `$HOME/.local/bin/pact` somewhere in your PATH.

Building with GHCJS
---

To build pact for browser use, add `--stack-yaml stack-ghcjs.yaml` to your stack commands. The script [web/build.sh](web/build.sh) builds Pact and minifies it; you'll need the [closure compiler](https://www.npmjs.com/package/google-closure-compiler) and a Java Runtime Environment installed.

More information
---

[Pact Homepage](http://kadena.io/pact)

[Pact Reference Docs](http://pact-language.readthedocs.io)

[Pact Whitepaper](http://kadena.io/docs/Kadena-PactWhitepaper.pdf)

License
---

This code is distributed under the terms of the BSD3 license. See [LICENSE](LICENSE) for details.
