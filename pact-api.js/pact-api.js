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
    if (Array.isArray(cmds)) {
        return {"cmds":cmds};
    } else {
        return {"cmds":[cmds]};
    }
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

var unique = function(arr) {
	  var n = {},r=[];
	  for(var i = 0; i < arr.length; i++)
	  {
		    if (!n[arr[i]])
		    {
			      n[arr[i]] = true;
			      r.push(arr[i]);
		    }
	  }
	  return r;
};

var simplePollRequestFromExec = function(execMsg) {
    var cmds = execMsg.cmds || PactError("expected key 'cmds' in object: " + JSON.stringify(execMsg));
    var rks = [];
    if (!cmds.every(function(v){return v.hasOwnProperty("hash");})) {
        throw new PactError('maleformed object, expected "hash" key in every cmd: ' + JSON.stringify(execMsg));
    } else {
        rks = unique(cmds.map(function(v){return v.hash;}));
    }
    return {"requestIds": rks};
};

module.exports = {
    PactError: PactError,
    crypto: {
        binToHex: binToHex,
        hash: hash,
        genKeyPair: genKeyPair,
        sign: sign
    },
    api: {
        mkSingleCmd: mkSingleCmd,
        mkPublicSend: mkPublicSend
    },
    simple: {
        exec: {
            createCommand: simpleExecCommand,
            createPollRequest: simplePollRequestFromExec
        }
    }
};
