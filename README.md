The Pact Programming Language
===

[![Build Status](https://travis-ci.org/kadena-io/pact.svg?branch=master)](https://travis-ci.org/kadena-io/pact)

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

You'll then need to install the Pact interpreter/REPL. [Prebuilt binaries](http://kadena.io/pact/downloads.html) are
available, or see [building](#Building) below.

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

Quick Start: pact --serve
---

To aid in application development pact has an HTTP server (pact-serve).
To launch the server, call `pact --serve <path/to/server/config.yaml>`

By default, this server will host any static files it finds in the directory that it is called from along with the pact API.
The pact-api consists of:

* `api/v1/send`: submit one-or-more signed pact commands for execution by pact-server. The results of commands are not returned, instead the requestKey needed to poll of the result is.

* `api/v1/poll`: poll pact-server for the result(s) of one-or-more previously submitted commands

* `api/v1/listen`: synchronously poll pact-server for the result of one previously submitted command. This will only return when the result is available.

* `api/v1/local`: synchronously execute a non-transactional command. This is useful for testing commands without impacting the state of the system.

Each pact-api endpoint accepts a POST of a JSON object, the structure of which is outside the scope of a "quick start".
Please refer instead to:

* The `pact-lang-api` npm package, provided to facilitate pact-api interactions. Furthermore, please see the `pact-lang-api.js/readme.md` file for more details about the JSON structure.

* The [pact-todomvc](github.com/kadena-io/pact-todomvc) working demonstration.


Using the Pact Dev Server
---

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

### Initializing the server

When running pact-serve with persistence enabled the development server will automatically replay from disk when it starts.
In this demo, we are persisting to `log/` which causes pact-serve to create or use `log/commands.sqlite` to store Commands and CommandResults.

The first time you run pact-serve the SQLite DBs will be created empty (as no commands have been run yet).
The important thing to note is that until you delete `log/commands.sqlite` (or run pact-serve in memory) pact-serve will replay every command on start up.

If you think of it like a blockchain, deleting the `commands.sqlite` file or running in memory gives Pact a "fresh" chain to work with.

Building
---

To build, you'll need a working [Haskell Stack install](https://docs.haskellstack.org/en/stable/README/#how-to-install) . After which, building is as simple as 'stack build'.

To install for use with Atom and the command line, issue 'stack install' and then either add `$HOME/.local/bin` to your path, or symlink `$HOME/.local/bin/pact` somewhere in your PATH.

Builing with GHCJS
---

To build pact for browser use, add `--stack-yaml stack-ghcjs.yaml` to your stack commands. The script [web/build.sh](web/build.sh) builds Pact and minifies it; you'll need the [closure compiler](https://www.npmjs.com/package/google-closure-compiler) and a Java Runtime Environment installed.

More information
---

[Pact Reference Docs](http://pact-language.readthedocs.io)

[Pact Whitepaper](http://kadena.io/docs/Kadena-PactWhitepaper.pdf)

License
---

This code is distributed under the terms of the BSD3 license. See [LICENSE](LICENSE) for details.
