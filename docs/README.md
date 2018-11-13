# Documentation Guideline
[Read the Docs](https://readthedocs.org/) uses the .rst files found in `/pact/docs/` to generate Pact's
documentation website. Pact's current Read the Docs website can be found [here](http://pact-language.readthedocs.io/en/latest/pact-reference.html).

**Contents**
1. Setup
2. Documentation Workflow
3. Native Function Documentation


### 1. Setup
To generate Pact documentation, first download the following packages using your favorite package manager:

* [Sphinx](http://www.sphinx-doc.org/en/master/)
* [Pandoc](https://pandoc.org/installing.html)

For OSX users with Python installed, download Sphinx via
```
$ pip install sphinx sphinx-autobuild
```

And install Pandoc via
```
$ brew install pandoc
```

More Read the Docs setup instructions can be found [here](http://docs.readthedocs.io/en/latest/getting_started.html).


### 2. Documentation Workflow
Make sure to download the packages specified in the Setup section. 

To start, make your documentation changes in the appropriate Markdown file(s).

Then convert these Markdown files into the .rst files Read the Docs expects. To do this, run the following
command from the `docs/` directory:
```
$ ./build.sh
```
This script should finish without throwing an error and should create .rst files for all of the Markdown files.


Now preview and review your changes in the website by running
```
$ open _build/html/index.html
```


Once finished, commit the .md files changed AND their corresponding .rst files. Please be
adviced that you **should not** make any changes to the .rst files themselves. These .rst changes will be erased
the next time `./build.sh` is executed.

### 3. Native Function Documentation
The documentation build script (`pact/docs/build.sh`) automatically generates docs for Pact native functions using the
comments found in their function definitions. To change a native function's documentation, edit or add these comments in
their function definition. **Do not** make any direct changes to the `pact-functions.rst` or `pact-functions.md` files.  