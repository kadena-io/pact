PR checklist:

* [ ] Test coverage for the proposed changes
* [ ] PR description contains example output from repl interaction or a snippet from unit test output
* [ ] New builtins have a FV translation
* [ ] Documentation has been (manually) updated at https://docs.kadena.io/pact
* [ ] Any changes that could be relevant to users [have been recorded in the changelog](https://github.com/kadena-io/pact/blob/master/CHANGELOG.md)
* [ ] In case of  changes to the Pact trace output (`pact -t`), make sure [pact-lsp](https://github.com/kadena-io/pact-lsp) is in sync.

Additionally, please justify why you should or should not do the following:

* [ ] Confirm replay/back compat
* [ ] Benchmark regressions
* [ ] (For Kadena engineers) Run integration-tests against a Chainweb built with this version of Pact
