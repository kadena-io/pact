<p align="center">
<img src="https://i.imgur.com/bAZFAGF.png" width="450" height="243" alt="Kadena" title="Kadena">
</p>

<p>&nbsp;</p>

# The Pact Programming Language

[![Build Status](https://travis-ci.org/kadena-io/pact.svg?branch=master)](https://travis-ci.org/kadena-io/pact)


[Pact](http://kadena.io/pact) is an open-source, Turing-**in**complete smart contract language that has been purpose-built with blockchains first in mind. Pact focuses on facilitating transactional logic with the optimal mix of functionality in authorization, data management, and workflow.

Read the whitepaper:

- [The Pact Smart Contract Language](https://d31d887a-c1e0-47c2-aa51-c69f9f998b07.filesusr.com/ugd/86a16f_442a542b64554cb2a4c1ae7f528ce4c3.pdf)

For additional information, press, and development inquires, please refer to the Kadena [website](https://kadena.io)


## Table Of Contents

- [Tutorials](#tutorials)
- [Documentation](#documentation)
- [Quickstart](#quickstart)
- [Installing Pact](#installing-pact)
  - [Binary Distributions](#binary-distributions)
  - [Instructions for Mac users](#instructions-for-mac-users)
  - [Instructions for Linux users](#instructions-for-linux-users)
  - [Building from Source](#building-from-source)
  - [Verifying Installation](#verifying-installation)
- [Supported Editors](#supported-editors)
  - [Chainweaver](#chainweaver)
  - [In Browser](#in-browser)
  - [Atom](#atom)
  - [Emacs](#emacs)
  - [Vim](#vim)
- [The Pact REST Api](#the-pact-rest-api)
  - [Documentation](#api-documentation)
  - [Pact REST Api Server](#rest-api-server)
  - [Related Projects](#related-projects)
- [License](#license)

## Tutorials

Extensive tutorials on every facet of the Pact language may be found [here](https://pactlang.org). Additionally, users may find example scripts in Kadena's [pact-examples](https://github.com/kadena-io/pact-examples) repository, in the [examples](https://github.com/kadena-io/pact/tree/master/examples) directory, and in production as part of the [Kadena Public Blockchain](https://github.com/kadena-io/chainweb-node/).

## Documentation

The Pact language specification, API documentation, features, motivations, and in-depth tutorial content may be found [here](http://pact-language.readthedocs.io). 

Documentation is always in flux, and corrections to out of date documentation are always appreciated. Feel free to do so if you find such an error by opening an [issue](https://github.com/kadena-io/pact/issues).


## Quickstart

The easiest and quickest way to try Pact is [in the browser](http://pact.kadena.io), or via the Chainweaver [wallet](https://github.com/kadena-io/chainweaver). Both environments support error and warning highlighting, as well as an in-app REPL environment for you to test out your code immediately. 

## Installing Pact


### Binary Distributions

Pact can be installed via binary distribution for Linux or Mac by following the instructions below:

- Install [z3](https://github.com/Z3Prover/z3/wiki).
- Download the [prebuilt binaries](https://github.com/kadena-io/pact/releases) for either Linux or Mac, depending on your OS. 
- Once you've downloaded the binary, make sure that it is marked as executable by running `chmod +x <executable-file>`.
- Put the binary somewhere in your PATH.

Once you have Pact in your path, proceed to validating your installation by trying out [the repl](#verifying-installation).

### Instructions for Mac Users

#### Using Brew

On Mac, the easiest way to install pact is with Homebrew. Make sure that Homebrew has been installed in your machine. Instructions for how to install it can be found [here](https://brew.sh). Once Homebrew is installed, run the following command to install pact:


```bash
brew update
brew install kadena-io/pact/pact
```

If you want to install from source, see [building from source](#building-from-source)

### Instructions for Linux Users

Linux is supported in terms of both the binary distributions (see below) and building from source. For installing `pact` on Linux distributions in the Arch family, refer to [this package on the AUR](https://aur.archlinux.org/packages/pact/). Otherwise, please refer to [building from source](#building-from-source).

### Building from Source

#### Dependencies
- (Mac only) Homebrew: `brew install git`
- (Linux/Mac) [Installer](https://git-scm.com/downloads)

To get the code, you can go [here](https://github.com/kadena-io/pact/releases/). Once you have the code, we can pick a build tool.

#### Building with Cabal

[Cabal](https://www.haskell.org/cabal/) is the preferred way to manage packages by the Haskell community. You will need a version of GHC
installed on your machine to use it.

##### Dependencies

- `ghc >= 8.4` (Haskell compiler) and `cabal >= 2.2` (Haskell build-tool)
  - The easiest way to get this is to install it using (Linux/Mac) [ghcup](https://www.haskell.org/ghcup/) and issue `ghcup install 8.6.5`, followed by `ghcup install-cabal`.
  - ghc may also be installed via brew, issuing `brew install ghc` and `brew install cabal-install`.

(You may also need to install `zlib`, `z3`, and `sqlite`)

To build a Pact binary:

```bash
# Only necessary if you haven't done this recently.
cabal v2-update

# Build the project.
cabal v2-build
```

This will install a runnable version of Pact, which you can run via:

```bash
cabal v2-exec pact
```

Alternatively, `cabal v2-install` will install the binary to `~/.cabal/bin/`, which
you may need to add to your path. Then, you can call `pact` as-is.

#### Building with Stack

Stack is a Haskell build tool that manages compiler and dependency versions for
you. It's easy to install and use.

##### Dependencies

- `stack >= 1.9`
  - (Mac only) Homebrew: `brew install haskell-stack`
  - (Linux/Mac) [Installer](https://docs.haskellstack.org/en/stable/README/)

(You may also need to install `zlib`, `z3`, and `sqlite`)

To build a Pact binary:

```bash
stack build
```

This will compile a runnable version of Pact, which you can run via:

```bash
stack exec pact
```

Alternatively, `stack install` will install the binary to `~/.local/bin/`, which
you may need to add to your path. Then, you can call `pact` as-is.

#### Building with Nix

The fastest way to build and run Pact is to use the Nix package manager
which has binary caching capabilities that allow you to download pre-built
binaries for everything needed by Pact. For detailed instructions see [our
wiki](https://github.com/kadena-io/pact/wiki/Building-Kadena-Projects).

When the build is finished, you can run Pact with the following command:

```bash
./result/ghc/pact/bin/pact
```

### Verifying Installation

Test by issuing `pact` in a terminal or by executing your binary. Try out some commands:

```
$ pact
pact> (+ 1 2)
3
pact> (+ "hello, " "world")
"hello, world"
```


## Supported Editors

Pact is supported by a variety of editors ranging from full-fledged IDE environments to syntax highlighting. 

### Chainweaver

The Chainweaver [wallet](https://github.com/kadena-io/chainweaver) is the Kadena's wallet, offering a seamless IDE experience and wallet in one. It supports a full in-app REPL, code preview, error/warning highlighting, code deployment, key generation, and integration with existing Kadena blockchains. 

### In-Browser

An implementation of Chainweaver exists [in the browser](http://pact.kadena.io), if you do not wish to download the wallet. 

### Atom

For a full-fledged IDE experience, install the [Atom](https://atom.io)  editor along with `language-pact` using the [atom package manager](http://flight-manual.atom.io/using-atom/sections/atom-packages/).

### Emacs

Emacs has [`pact-mode`](https://melpa.org/#/pact-mode) support via MELPA, along with [`flycheck-pact`](https://melpa.org/#/flycheck-pact) for on-the-fly error highlighting. Download `pact-mode` and (optionally) `flycheck-pact` by opening Emacs and issuing `M-x package-list-packages`, syncing MELPA, and installing by name. Then, in your `init.el` or `.emacs`, include 

```elisp
(use-package pact-mode
  :ensure t
  :config
  ;; optionally
  (require 'flycheck-pact))
```

If you've chosen to include `flycheck-pact`, you can start the interactive buffer and trace by calling `flycheck-pact-toggle-trace` and `flycheck-pact-interactive-buffer`.
### Vim

If you are a [vim](https://vim.org) user, the [vim-pact](https://github.com/wsdjeg/vim-pact) plugin provides support for the pact syntax. 

## The Pact REST Api

### Api Documentation

The REST API is documented at [http://pact-language.readthedocs.io/en/latest/pact-reference.html#rest-api](http://pact-language.readthedocs.io/en/latest/pact-reference.html#rest-api).

### Pact REST API Server

Pact features a full REST API HTTP server and SQLite database implementation, making
blockchain application development painless and easy. The Pact server simulates a single-node blockchain environment, with the same API supported by the Kadena ScalableBFT blockchain.

To start the server, issue 

```bash
pact --serve CONFIG
``` 

or 

```
pact -s CONFIG
```

where CONFIG is a valid [config.yaml](config.yaml).


#### Hosting static files.

The HTTP server will host any static files it finds in its working directory.

#### Config file format

The pact dev server (pact-serve) requires a configuration Yaml file (e.g. server.conf) to operate. The documentation for it is:

```
console> pact --serve --help
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

### Related Projects

* The [pact-lang-api](https://www.npmjs.com/package/pact-lang-api) `npm` package provides a JavaScript library to aid interaction with the API.

* The [pact-todomvc](https://github.com/kadena-io/pact-todomvc) is a working demonstration.


## License

This code is distributed under the terms of the BSD3 license. See [LICENSE](LICENSE) for details.
