#!/bin/sh

set -e
PACT_EXEC=".stack-work/install/x86_64-osx/lts-8.15/8.0.2/bin/pact"
TEST_DIR="tests/resume/scripts/"

function run_server_test() {
    LOCATION="$TEST_DIR$1"
    apireq=`$PACT_EXEC -a $LOCATION`
    edit=${apireq#"{\"cmds\":[{\"hash\":\""*}
    reqkey=${edit%%"\",\"sigs"*}
    "$PACT_EXEC" -a $LOCATION | curl -d @- http://localhost:8080/api/v1/send
    curl -d "{\"requestKeys\":[\"$reqkey\"]}" -X POST http://localhost:8080/api/v1/poll
}

echo ""; echo "Step 1"; echo ""
run_server_test "01-system.yaml"
echo ""

echo ""; echo "Step 2"; echo ""
run_server_test "02-accounts.yaml"
echo ""

echo ""; echo "Step 3"; echo ""
run_server_test "03-create.yaml"
echo ""

echo ""; echo "Step 4"; echo ""
run_server_test "04-alice.yaml"
echo ""

echo ""; echo "Step 5"; echo ""
run_server_test "05-bob.yaml"
echo ""

echo ""; echo "Step 6"; echo ""
run_server_test "06-bob-dummy-pact.yaml"
echo ""

echo ""; echo "Step 7"; echo ""
run_server_test "07-alice.yaml"
echo ""

echo ""; echo "Step 8"; echo ""
run_server_test "08-bob-cont.yaml"
echo ""

echo ""; echo "Step 9"; echo ""
run_server_test "09-bob-cont.yaml"
echo ""

echo ""; echo "Step 10"; echo ""
run_server_test "10-bob-cont.yaml"
echo ""

echo ""; echo "Step 10"; echo ""
run_server_test "11-alice-cont4.yaml"
echo ""
