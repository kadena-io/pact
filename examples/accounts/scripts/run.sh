#!/bin/sh

set -e
PACT_EXEC=".stack-work/install/x86_64-osx/lts-8.15/8.0.2/bin/pact"

echo ""; echo "Step 1"; echo ""
"$PACT_EXEC" -a examples/accounts/scripts/01-system.yaml | curl -d @- http://localhost:8080/api/v1/send
curl -d '{"requestKeys":["221b1ea8687ca43da7eacdedfffe5a590215c334482b1148d5166e9847ead25e4d3a9c711c85c0b5ab9b739f151b562f37e1e21f2c8bc1004a5765575069cd91"]}' -X POST http://localhost:8080/api/v1/poll

echo ""; echo "Step 2"; echo ""
"$PACT_EXEC" -a examples/accounts/scripts/02-accounts.yaml | curl -d @- http://localhost:8080/api/v1/send
curl -d '{"requestKeys":["efdeea220e8583c91550226efdca6746eeb45e2d7e9fe120b09d2c7686d9d43dc2c80b151b702ac80223d5adf7745e8a475faf72a750503414cbc167c54b4bc5"]}' -X POST http://localhost:8080/api/v1/poll

echo ""; echo "Step 3"; echo ""
"$PACT_EXEC" -a examples/accounts/scripts/03-create.yaml | curl -d @- http://localhost:8080/api/v1/send
curl -d '{"requestKeys":["5f8f4c71413d1b112892c0ae45a72f47d87c0811b97acf244e61b1ee667e85d0a599506db5774364c6e7577e5f48faa2ae323d38749a650bf4f59e8222eaf83e"]}' -X POST http://localhost:8080/api/v1/poll

echo ""; echo "Step 4"; echo ""
"$PACT_EXEC" -a examples/accounts/scripts/04-alice.yaml | curl -d @- http://localhost:8080/api/v1/send
curl -d '{"requestKeys":["8a384a6dd457e4bfcc59d795df1be442faeae4097600b4d0d71083940c8033a9c3db6d8db3af256842525dfe7e4e949ae82058f143b4b61ea15a5af3cf030fcf"]}' -X POST http://localhost:8080/api/v1/poll

echo ""; echo "Step 5"; echo ""
"$PACT_EXEC" -a examples/accounts/scripts/05-bob.yaml | curl -d @- http://localhost:8080/api/v1/send
curl -d '{"requestKeys":["6c7916b948ebfd86bdfe9e1187cb9ce08d50cb4b0fe7699160367d9c7f6f188b622fd61eecc80418f09d6c6b91310e85a137e0324af8a5169ff1c81092988c1f"]}' -X POST http://localhost:8080/api/v1/poll
echo ""

echo ""; echo "Step 6"; echo ""
"$PACT_EXEC" -a examples/accounts/scripts/06-bob-dummy-pact.yaml | curl -d @- http://localhost:8080/api/v1/send
curl -d '{"requestKeys":["dcf3ff36cfd5b7c5cc8e5895755bd52761c0928166370482961e5a59ee83515e4e88a3b418d127b161d3aa45bd1314f2facc8402c2825600844e5f0d6e919f5f"]}' -X POST http://localhost:8080/api/v1/poll
echo ""

echo ""; echo "Step 7"; echo ""
"$PACT_EXEC" -a examples/accounts/scripts/07-alice.yaml | curl -d @- http://localhost:8080/api/v1/send
curl -d '{"requestKeys":["15061ef4067a47f0a09fd2057e038b7380edd440b888379b1c65f0ceaef90ff6c1e3296c1117bca283db2a93ab198bc021684ff1abbdc5b1b9e26b489e305a66"]}' -X POST http://localhost:8080/api/v1/poll
echo ""
