#!/bin/sh

set -e

JSON="Content-Type: application/json"



echo ""; echo "Step 1"; echo ""
pact -a examples/accounts/scripts/01-system.yaml | curl -H "$JSON" -d @- http://localhost:8080/api/v1/send
sleep 1; echo ""
curl -H "$JSON" -d '{"requestKeys":["zaqnRQ0RYzxTccjtYoBvQsDo5K9mxr4TEF-HIYTi5Jo"]}' -X POST http://localhost:8080/api/v1/poll

echo ""; echo "Step 2"; echo ""
pact -a examples/accounts/scripts/02-accounts.yaml | curl -H "$JSON" -d @- http://localhost:8080/api/v1/send
sleep 1; echo ""
curl -H "$JSON" -d '{"requestKeys":["gx60LXLVcNlSQ4XkJUgqwqdZZ-RyaPymVVpWMspAf-Y"]}' -X POST http://localhost:8080/api/v1/poll

echo ""; echo "Step 3"; echo ""
pact -a examples/accounts/scripts/03-create.yaml | curl -H "$JSON" -d @- http://localhost:8080/api/v1/send
sleep 1; echo ""
curl -H "$JSON" -d '{"requestKeys":["CgjWWeA3MBmf3GIyop2CPU7ndhPuxnXFtcGm7-STMUo"]}' -X POST http://localhost:8080/api/v1/poll

echo ""; echo "Step 4"; echo ""
pact -a examples/accounts/scripts/04-alice.yaml | curl -H "$JSON" -d @- http://localhost:8080/api/v1/send
sleep 1; echo ""
curl -H "$JSON" -d '{"requestKeys":["r5L96DVwNKANAedHArQoJc9oxF3hf_EftopCsoaCuuY"]}' -X POST http://localhost:8080/api/v1/poll

echo ""; echo "Step 5"; echo ""
pact -l -a examples/accounts/scripts/05-bob.yaml | curl -H "$JSON" -d @- http://localhost:8080/api/v1/local
echo ""
