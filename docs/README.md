# Documentation Guideline

Rendered Documentation: https://pact-language.readthedocs.io/en/stable/

**Contents**

1. Developer Warning
2. Setup
3. File Layout
4. Documentation Workflow
5. Localization

## Developers: Please Read

We use ReadTheDocs for our documentation, which is generated from `.rst` files
found in the `docs/` directory. The implications of this are:

- **Most `.rst` files are autogenerated,** at least in the `en` folder. Manually
  editing these `.rst` files is almost always a mistake.

  - For native functions, you need to change the Haskell code that produces the
    function docs in `Pact.Native` and elsewhere.
  - For everything else, chances are you should be editing a `.md` file such as
    `pact-reference.md`.

- Our ReadTheDocs configuration produces two versions of our docs: `stable` and
  `master`.

  - `stable` is the default for https://pact-language.readthedocs.io/, and only
    updates upon the creation of a new Github release.
  - `latest` updates upon every merge to our Github `master` branch.

## Setup

We use Python, Sphinx, and Pandoc to generate the official docs. To ensure that
everyone is using the same software versions (thereby rendering the docs in the
same way), our scripts use [Nix](https://nixos.org/nix/).

With Nix installed, make sure your Channels are up to date with `nix-channel --update`.

Now you're ready to generate Pact's documentation.

## File Layout

Pact documentation is available in a number of languages. The English versions
are considered the "originals", with all others as translations. Each language's
files are located in `docs/` under a subdirectory marked by their usual locale
name (e.g. `en` for English).

All documentation files are stored as Markdown (`.md`) files. You may also notice
`.rst` files, but these are generated by tooling and **are not to be edited manually**.
This process is detailed in the next section.

English has a number of `.md` files, some hand-written and some auto-generated.
Translations have only one `.md` file each, titled `pact-reference.md`.

## Documentation Workflow

Make sure to download the packages specified in the *Setup* section.

To start, make your documentation changes in the appropriate Markdown file(s).

If you have instead made changes to Pact function signatures or documentation
directly in Haskell code (say in `src/Pact/Native.hs`), then you can regenerate
`docs/en/pact-functions.md` and `docs/en/pact-properties-api.md` by running Pact's test suites.

```
$ stack test --fast
```

Note that manual changes to either of the above files will be overwritten
whenever tests are run.

Now, convert these Markdown files into the `.rst` format that Read the Docs expects.
From the `docs/` directory:

```
$ ./build.sh
```

This script should finish without error, and should create `.rst` files for each available language.

To review your changes, open (for English) `docs/en/_build/html/index.html` in your browser.
You can keep this page open, and then refresh your browser as you make further changes
and rerun `./build.sh`.

Once finished, commit the `.md` files that changed *and* their corresponding `.rst` files.

## Localization

As mentioned in *File Layout*, Pact documentation has several translations.
Unlike the English, no content is auto-generated from Haskell files.
Instead, updates to the English must be manually ported to each translation.

### Editing an existing Translation

If you notice something to improve, edit the single `pact-reference.md` file corresponding
to your language. Then, generate the `.rst` version as described in **Documentation Workflow**,
and submit your changes.

### Adding a new Translation

Thank you! Your effort will help many developers around the world.

For this example, let's assume that Croatian is the language being added.
The locale name for Croatian is `hr`.

1. From `docs/`, create a new subdirectory for your language:

```
$ mkdir hr
```

2. Sphinx needs a `conf.py` file to generate HTML from `.rst` files, one for each language.
   Copy the `conf.py` from one of the other languages (their contents are the same):

```
$ cp en/conf.py hr/
```

3. Each translation shares assets found in `img/`. Create a symlink so that
   Sphinx can find these:

```
$ cd hr/
$ ln -s ../img img
```

4. Add your translation as a file named `pact-reference.md` within your language's subdirectory.

5. Copy `docs/jp/index.rst` to your subdirectory. Within it, change the title line
   to the translation of "Pact Language Reference".

6. Add the following to `docs/work.sh`, matching the other translations:

```bash
# --- Croatian Docs --- #
cd ..
cd hr/
rm -rf _build

pandoc -s -t rst pact-reference.md -o pact-reference.rst
perl -p0777i -e 's/^(\+|\-)\n~/\\\1\n~~/gm' pact-reference.rst
sphinx-build -b html -d _build/doctrees . _build/html
```

7. Generate your `.rst` file with `docs/build.sh`, as usual.

8. Submit your changes to Github. Once approved, our administrators will alter
   our internal Read the Docs configuration to officially recognize your
   translation. You will soon see it on Read the Docs, and will be able to switch
   languages freely. For instance, our new Croatian version would be available
   at https://pact-language.readthedocs.io/hr/latest/ .
