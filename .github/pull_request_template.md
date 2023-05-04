Pact Pull Request
---

Please include the following checklist in your PR:

* [ ] Test coverage for the proposed changes
* [ ] Documentation has been updated if new natives or FV properties have been added. To generate new documentation, issue `cabal run tests`. If they pass locally, docs are generated.
* [ ] Any changes that could be relevant to users [have been recorded in the changelog](https://github.com/kadena-io/pact/blob/master/CHANGELOG.md)
* [ ] In case of  changes to the Pact trace output (`pact -t`), make sure [pact-lsp](https://github.com/kadena-io/pact-lsp) is in sync.

Additionally, please justify why you should or should not do the following:

* [ ] Confirm replay/back compat
* [ ] Benchmark regressions
* [ ] (For Kadena engineers) Run integration-tests against a Chainweb built with this version of Pact
