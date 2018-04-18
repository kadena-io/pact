#!/bin/sh

set -e

echo ""; echo "Step 1"; echo ""
pact -a examples/accounts/scripts/01-system.yaml | curl -d @- http://localhost:8080/api/v1/send
curl -d '{"requestKeys":["221b1ea8687ca43da7eacdedfffe5a590215c334482b1148d5166e9847ead25e4d3a9c711c85c0b5ab9b739f151b562f37e1e21f2c8bc1004a5765575069cd91"]}' -X POST http://localhost:8080/api/v1/poll

echo ""; echo "Step 2"; echo ""
pact -a examples/accounts/scripts/02-accounts.yaml | curl -d @- http://localhost:8080/api/v1/send
curl -d '{"requestKeys":["1d1d4043dc194c616573895a5ebec0d1ea9898a719651d30e6be5bff386b94e4703afaf998c295a5181ef2eee844a79bdd3466072c30eb4004adf5108addaeee"]}' -X POST http://localhost:8080/api/v1/poll

echo ""; echo "Step 3"; echo ""
pact -a examples/accounts/scripts/03-create.yaml | curl -d @- http://localhost:8080/api/v1/send
curl -d '{"requestKeys":["5f8f4c71413d1b112892c0ae45a72f47d87c0811b97acf244e61b1ee667e85d0a599506db5774364c6e7577e5f48faa2ae323d38749a650bf4f59e8222eaf83e"]}' -X POST http://localhost:8080/api/v1/poll

echo ""; echo "Step 4"; echo ""
pact -a examples/accounts/scripts/04-alice.yaml | curl -d @- http://localhost:8080/api/v1/send
curl -d '{"requestKeys":["e0e26fb33bb20ff48f2c7378ed1f22cbe775f16e67de5f4b6e91de9ce83d13010f1d1da6633d8c0dd7050d4960b2d04dc11f9375ece36f6e66c60f9af5c69ed5"]}' -X POST http://localhost:8080/api/v1/poll

echo ""; echo "Step 5"; echo ""
pact -l -a examples/accounts/scripts/05-bob.yaml | curl -d @- http://localhost:8080/api/v1/local
echo ""
