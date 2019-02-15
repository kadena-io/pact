#!/bin/sh

# Source: "Complete example" section, https://kobl.one/blog/create-full-ethereum-keypair-and-address/

rm Key pub priv geth address

# Generate the private and public keys
openssl ecparam -name secp256k1 -genkey -noout | openssl ec -text -noout > Key &&

# Extract the public key and remove the EC prefix 0x04
cat Key | grep pub -A 5 | tail -n +2 | tr -d '\n[:space:]:' | sed 's/^04//' > pub &&

# Extract the private key and remove the leading zero byte
cat Key | grep priv -A 3 | tail -n +2 | tr -d '\n[:space:]:' | sed 's/^00//' > priv &&

# Import the private key to geth and get address
yes '' | geth account import priv > geth &&

cat geth | grep "Address" | sed 's/^Address: {//' | sed 's/.$//' > address &&

# Add public key, private key, and address to text file
pubV=`cat pub`
privV=`cat priv`
addrV=`cat address`
echo "$privV, $pubV, $addrV" >> ./tests/Utils/eth-keys.txt &&
echo "Added the following keys \n \
      priv: $privV \n \
      pub:  $pubV \n \
      addr: $addrV \n"

rm Key pub priv geth address
