#!/bin/sh

set -e

JSON="Content-Type: application/json"



echo ""; echo "Step 1"; echo ""
pact -a examples/accounts/scripts/01-system.yaml | curl -H "$JSON" -d @- http://localhost:8080/api/v1/send
sleep 1; echo ""
curl -H "$JSON" -d '{"requestKeys":["020184904af377b619b066a60b06e703f44cb908eb19de69008cfd188ee65725b8c7022818955d90e320e530deba5c6ae7094749efacabeca9037d88016588ce"]}' -X POST http://localhost:8080/api/v1/poll

echo ""; echo "Step 2"; echo ""
pact -a examples/accounts/scripts/02-accounts.yaml | curl -H "$JSON" -d @- http://localhost:8080/api/v1/send
sleep 1; echo ""
curl -H "$JSON" -d '{"requestKeys":["fa6491227ea73ac3cbe00e4b95b9628217ace7a8911e1165caf37440936869121678841eb3d2c2741ba71e4222357b7a853944245a59dcd3bdd4a52a49757c53"]}' -X POST http://localhost:8080/api/v1/poll

echo ""; echo "Step 3"; echo ""
pact -a examples/accounts/scripts/03-create.yaml | curl -H "$JSON" -d @- http://localhost:8080/api/v1/send
sleep 1; echo ""
curl -H "$JSON" -d '{"requestKeys":["b31faa1e98ec1b9a2c489f0c5ea019fcf985aab27c46bb778c3bff5fb0e92da3cb67f54004bcd7cb95831c89d4487c9fd84f7f6fa84c64d2e3916afae7dafa3e"]}' -X POST http://localhost:8080/api/v1/poll

echo ""; echo "Step 4"; echo ""
pact -a examples/accounts/scripts/04-alice.yaml | curl -H "$JSON" -d @- http://localhost:8080/api/v1/send
sleep 1; echo ""
curl -H "$JSON" -d '{"requestKeys":["ec36c9d3dc51fe138b29d9a420b60b82a09bfbb129dd4d8fe2d154799b3a3694fd120aa02bed068f460e61954e8ba1dff7640d439b615c492bf312e2ab8ad5ec"]}' -X POST http://localhost:8080/api/v1/poll

echo ""; echo "Step 5"; echo ""
pact -l -a examples/accounts/scripts/05-bob.yaml | curl -H "$JSON" -d @- http://localhost:8080/api/v1/local
echo ""
