#!/bin/sh

set -e

JSON="Content-Type: application/json"



echo ""; echo "Step 1"; echo ""
pact -a examples/accounts/scripts/01-system.yaml | curl -H "$JSON" -d @- http://localhost:8080/api/v1/send
sleep 1; echo ""
curl -H "$JSON" -d '{"requestKeys":["LkaXHRpZyHd4HocYHO8v4d4rW8pITSE1ODLiraseRNo"]}' -X POST http://localhost:8080/api/v1/poll

echo ""; echo "Step 2"; echo ""
pact -a examples/accounts/scripts/02-accounts.yaml | curl -H "$JSON" -d @- http://localhost:8080/api/v1/send
sleep 1; echo ""
curl -H "$JSON" -d '{"requestKeys":["bctSHEz4N5Y1XQaic6eOoBmjty88HMMGfAdQLPuIGMw"]}' -X POST http://localhost:8080/api/v1/poll

echo ""; echo "Step 3"; echo ""
pact -a examples/accounts/scripts/03-create.yaml | curl -H "$JSON" -d @- http://localhost:8080/api/v1/send
sleep 1; echo ""
curl -H "$JSON" -d '{"requestKeys":["N5xTwxAJTq6mu0aSzjny5p1FsXWiD0cpFI_QwofaVDg"]}' -X POST http://localhost:8080/api/v1/poll

echo ""; echo "Step 4"; echo ""
pact -a examples/accounts/scripts/04-alice.yaml | curl -H "$JSON" -d @- http://localhost:8080/api/v1/send
sleep 1; echo ""
curl -H "$JSON" -d '{"requestKeys":["wZaF1caS607x1aAZyp-ld0P14qd0ytLcwpJPiTamtno"]}' -X POST http://localhost:8080/api/v1/poll

echo ""; echo "Step 5"; echo ""
pact -l -a examples/accounts/scripts/05-bob.yaml | curl -H "$JSON" -d @- http://localhost:8080/api/v1/local
echo ""
