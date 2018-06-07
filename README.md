The Pact Programming Language
===

[![Build Status](https://travis-ci.org/kadena-io/pact.svg?branch=master)](https://travis-ci.org/kadena-io/pact)

This is the source code repository for [Pact](http://kadena.io/pact), a language for developing
smart contracts to run on a [fast deterministic blockchain](http://kadena.io).

References
---

[Pact Homepage](http://kadena.io/pact)

[Pact Reference Docs](http://pact-language.readthedocs.io)

[Pact Whitepaper](http://kadena.io/docs/Kadena-PactWhitepaper.pdf)


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

You'll then need to install the Pact interpreter/REPL. On Mac, the easiest way is with [homebrew](https://brew.sh/).

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
"hello, world"
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

Building with Nix / NixOS
---

1. Go to https://nixos.org/nix/, click "Get Nix", follow the instructions to install the Nix package manager
1. Edit `$NIX_CONF_DIR/nix.conf`
1. Set the `binary-caches` and `binary-cache-public-keys` lines as follows:

```
binary-caches = https://pact.cachix.org https://nixcache.reflex-frp.org https://cache.nixos.org/
binary-cache-public-keys = pact.cachix.org-1:cg1bsryGrHnQzqEp52NcHq4mBBL+R25XbR2Q/I/vQ8Y= ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
```

1. Run `nix-build` from the project root

License
---

This code is distributed under the terms of the BSD3 license. See [LICENSE](LICENSE) for details.
