var blake = require('blakejs');
var nacl = require('tweetnacl');

function PactError(message) {
    this.name = 'PactError';
    this.message = message || 'Pact API Error';
    this.stack = (new Error()).stack;
}
PactError.prototype = Object.create(Error.prototype);
PactError.prototype.constructor = PactError;

var binToHex = function(s) {
  var constructor = s.constructor.name || null;

  if (constructor !== 'Uint8Array') {
    throw new PactError('Expected input to be a Uint8Array');
  }

  return Buffer.from(s).toString('hex');
};

var hashBin = function(s) {
  return blake.blake2b(s);
};

var hash = function(s) {
  return binToHex(hashBin(s));
};

var genKeyPair = function() {
  return nacl.sign.keyPair();
};

var sign = function(msg, keyPair) {
    if (!keyPair.hasOwnProperty("publicKey") || !keyPair.hasOwnProperty("secretKey") ) {
        throw new PactError("Invalid KeyPair: expected to find keys of name 'secretKey' and 'publicKey' as Uint8Arrays: " + JSON.stringify(keyPair));
    }
    var hshBin = hashBin(msg);
    var hsh = binToHex(hshBin);
    var sigBin = nacl.sign.detached(hshBin, keyPair.secretKey);
    return {"hash": hsh, "sig": binToHex(sigBin), "pubKey":binToHex(keyPair.publicKey)};
};

var pullSigAndPubKey = function(s) {
    if (!s.hasOwnProperty("pubKey") || !s.hasOwnProperty("sig")) {
        throw new PactError("Expected to find keys of name 'sig' and 'pubKey' in " + JSON.stringify(s));
    }
    return {"sig":s.sig, "pubKey":s.pubKey};
};

var pullAndCheckHashs = function(sigs) {
    var hsh = sigs[0].hash;
    for(var i = 1; i < sigs.length; i++)
    {
        if(sigs[i].hash !== hsh) {
            throw new PactError('Sigs for different hashes found: ' + JSON.stringify(sigs));
        }
    }
    return hsh;
};

var mkSingleCmd = function(sigs, cmd) {
    // Lower level API constructor
    // Formats a single command from its parts:
    //   sigs: a list of {hash, pubKey, sig}
    //   cmd: the stringified json blob used to create the hash
    // Throws `PactError` if an issue is found
    if (!Array.isArray(sigs)) {
        throw new PactError('Expected the first argument (sigs) to be an array of sigs');
    }
    if (typeof cmd !== 'string') {
        throw new PactError('Expected the second argument (cmd) to be a string');
    }
    return {"hash":pullAndCheckHashs(sigs), "sigs": sigs.map(pullSigAndPubKey), "cmd": cmd};
};

var mkPublicSend = function(cmds) {
    // Lower level API constructor
    // Does the last level of wrapping
    return {"cmds":cmds};
};

var simpleExecCommand = function(keyPairs, requestId, pactCode, envData) {
    // Input: eithe a single or array of keyPairs, a requestId, pactCode, and an envData object
    // Output: a correctly formatted JSON exec msg for pact, send it to /api/public/send
    // Throws PactError on maleformed inputs
    if (typeof requestId !== 'string') {
        throw new PactError('requestId must be a string: ' + JSON.stringify(requestId));
    }
    if (typeof pactCode !== 'string') {
        throw new PactError('pactCode must be a string: ' + JSON.stringify(pactCode));
    }
    var cmd = JSON.stringify({"rid": requestId,
                              "payload": {"exec": {
                                  "code": pactCode,
                                  "data": envData || {}
                              }}});
    var sigs = [];
    if (Array.isArray(keyPairs)) {
        sigs = keyPairs.map(function(kp) {return sign(cmd, kp);});
    } else {
        sigs = [sign(cmd, keyPairs)];
    }
    var hsh = pullAndCheckHashs(sigs);
    return mkPublicSend({"hash": hsh, "sigs":sigs.map(pullSigAndPubKey), "cmd":cmd});
};

module.exports = {
    PactError: PactError,
    binToHex: binToHex,
    hash: hash,
    genKeyPair: genKeyPair,
    sign: sign,
    mkSingleCmd: mkSingleCmd,
    mkPublicSend: mkPublicSend,
    simpleExecCommand: simpleExecCommand
};

//{
//    "cmds": [
//        {
//            "hash": "5fda1d24cae8653c281f0297db9f288f12bc570d1d271d6bd4aaf316f6f3f6528387bb04e512316dbcc7cbde5fbe1f1dcb1948e920b4f4fc5573280bdb017206",
//            "sigs": [
//                {
//                    "sig": "cf84148604ebc7663ae24127cb552d71cfdad1217098ad46a8830ed97ae28f18781d4bc399d4764060f09e2e8f4fa9242b9c97e2496531fb005c2e5f30af2103",
//                    "pubKey": "06c9c56daa8a068e1f19f5578cdf1797b047252e1ef0eb4a1809aa3c2226f61e"
//                }
//            ],
//            "cmd": "{\"rid\":\"hello\",\"payload\":{\"exec\":{\"data\":{},\"code\":\"(+ 1 2)\"}}}"
//        }
//    ]
//}
