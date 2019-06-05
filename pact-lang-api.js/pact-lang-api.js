const blake = require('blakejs');
const nacl = require('tweetnacl');
const base64url = require('base64-url')

var binToHex = function(s) {
  var constructor = s.constructor.name || null;

  if (constructor !== 'Uint8Array') {
    throw new TypeError('Expected Uint8Array');
  }

  return Buffer.from(s).toString('hex');
};

var hexToBin = function(h) {
  if (typeof h !== 'string') { throw new TypeError("Expected string: " + h); }
  return new Uint8Array(Buffer.from(h,'hex'));
};

var hashBin = function(s) {
  return blake.blake2b(s, null, 32);
};

var hash = function(s) {
  return base64url.escape(base64url.encode(hashBin(s)));
};

var base64UrlEncode = function(s) {
  return base64url.escape(base64url.encode(s));
}

var genKeyPair = function() {
  var kp = nacl.sign.keyPair();
  var pubKey = binToHex(kp.publicKey);
  var secKey = binToHex(kp.secretKey).slice(0,64);
  return {"publicKey": pubKey, "secretKey": secKey};
};

var toTweetNaclSecretKey = function(keyPair) {
  if (!keyPair.hasOwnProperty("publicKey") || !keyPair.hasOwnProperty("secretKey") ) {
    throw new TypeError("Invalid KeyPair: expected to find keys of name 'secretKey' and 'publicKey': " + JSON.stringify(keyPair));
  }
  return hexToBin(keyPair.secretKey + keyPair.publicKey);
};

var sign = function(msg, keyPair) {
  if (!keyPair.hasOwnProperty("publicKey") || !keyPair.hasOwnProperty("secretKey") ) {
    throw new TypeError("Invalid KeyPair: expected to find keys of name 'secretKey' and 'publicKey': " + JSON.stringify(keyPair));
  }
  var hshBin = hashBin(msg);
  var hsh = base64UrlEncode(hshBin);
  var sigBin = nacl.sign.detached(hshBin, toTweetNaclSecretKey(keyPair));
  return {"hash": hsh, "sig": binToHex(sigBin), "pubKey":keyPair.publicKey};
};

var pullSigAndPubKey = function(s) {
  if (!s.hasOwnProperty("pubKey") || !s.hasOwnProperty("sig")) {
    throw new TypeError("Expected to find keys of name 'sig' and 'pubKey' in " + JSON.stringify(s));
  }
  return {"sig":s.sig, "pubKey":s.pubKey};
};


var pullSig = function(s) {
  if (!s.hasOwnProperty("sig")) {
    throw new TypeError("Expected to find keys of name 'sig' in " + JSON.stringify(s));
  }
  return {"sig":s.sig};
};

var pullPubKeyAndAddr = function(s) {
  if (!s.hasOwnProperty("pubKey")) {
    throw new TypeError("Expected to find keys of name 'pubKey' in " + JSON.stringify(s));
  }
  return {"pubKey":s.pubKey, "addr":s.pubKey, "scheme": "ED25519"};
};


var pullAndCheckHashs = function(sigs) {
  var hsh = sigs[0].hash;
  for(var i = 1; i < sigs.length; i++)
  {
    if(sigs[i].hash !== hsh) {
      throw new Error('Sigs for different hashes found: ' + JSON.stringify(sigs));
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
        throw new TypeError('Expected the first argument (sigs) to be an array of sigs');
    }
    if (typeof cmd !== 'string') {
        throw new TypeError('Expected the second argument (cmd) to be a string');
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

var mkSigner = function(kp) {
  return {
    "pubKey": kp.publicKey,
    "addr": kp.publicKey,
    "scheme": "ED25519"
  }
}

var simpleExecCommand = function(keyPairs, nonce, pactCode, envData, meta) {
  // Input: eithe a single or array of keyPairs, a nonce, pactCode, and an envData object
  // Output: a correctly formatted JSON exec msg for pact, send it to /api/public/send
  // Throws PactError on maleformed inputs
  if (typeof nonce !== 'string') {
    throw new TypeError('nonce must be a string: ' + JSON.stringify(nonce));
  }
  if (typeof pactCode !== 'string') {
    throw new TypeError('pactCode must be a string: ' + JSON.stringify(pactCode));
  }
  var kpArray = [];
  if (Array.isArray(keyPairs)) {
    kpArray = keyPairs;
  } else {
    kpArray = [keyPairs];
  }
  var signers = kpArray.map(mkSigner);
  var cmdJSON = {"nonce": nonce,
                            "payload": {
                              "exec": {
                                "code": pactCode,
                                "data": envData || {}
                              }
                            },
                            "signers": signers,
                            "meta": meta
                            };
  var cmd = JSON.stringify(cmdJSON);
  var sigs = [];
  if (Array.isArray(keyPairs)) {
    sigs = keyPairs.map(function(kp) {return sign(cmd, kp);});
  } else {
    sigs = [sign(cmd, keyPairs)];
  }
  var hsh = pullAndCheckHashs(sigs);
  return mkPublicSend({"hash": hsh, "sigs":sigs.map(pullSig), "cmd":cmd});
};

var unique = function(arr) {
  var n = {},r=[];
  for(var i = 0; i < arr.length; i++)
  var hsh = eckHashs(sigs);
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
  var cmds = execMsg.cmds || TypeError("expected key 'cmds' in object: " + JSON.stringify(execMsg));
  var rks = [];
  if (!cmds.every(function(v){return v.hasOwnProperty("hash");})) {
    throw new TypeError('maleformed object, expected "hash" key in every cmd: ' + JSON.stringify(execMsg));
  } else {
    rks = unique(cmds.map(function(v){return v.hash;}));
  }
  return {"requestKeys": rks};
};

var simpleListenRequestFromExec = function(execMsg) {
  var cmds = execMsg.cmds || TypeError("expected key 'cmds' in object: " + JSON.stringify(execMsg));
  var rks = [];
  if (!cmds.every(function(v){return v.hasOwnProperty("hash");})) {
    throw new TypeError('maleformed object, expected "hash" key in every cmd: ' + JSON.stringify(execMsg));
  } else {
    rks = unique(cmds.map(function(v){return v.hash;}));
  }
  return {"requestKey": rks[0]};
};

var mkExp = function(pgmName) {
  if (typeof pgmName !== 'string') {
    throw new TypeError('pgmName must be a string: ' + JSON.stringify(pgmName));
  }
  return '(' + pgmName + ' ' + Array.prototype.slice.call(arguments, 1).map(JSON.stringify).join(' ') + ')';
};

var mkMeta = function (sender, chainId, gasPrice, gasLimit) {
  if (typeof sender !== 'string' ) {
    throw new TypeError('sender must be a string: ' + JSON.stringify(sender));
  }
  if (typeof chainId !== 'string') {
    throw new TypeError('chainId must be a string: ' + JSON.stringify(chainId));
  }
  if (typeof gasPrice !== 'number' ) {
    throw new TypeError('gasPrice must be a number: ' + JSON.stringify(gasPrice));
  }
  if (typeof gasLimit !== 'number' ) {
    throw new TypeError('gasLimit must be a number: ' + JSON.stringify(gasLimit));
  }
  return {"gasLimit":gasLimit, "chainId":chainId, "gasPrice":gasPrice, "sender":sender}
}

module.exports = {
  crypto: {
    binToHex: binToHex,
    hexToBin: hexToBin,
    hash: hash,
    genKeyPair: genKeyPair,
    sign: sign,
    toTweetNaclSecretKey: toTweetNaclSecretKey
  },
  api: {
    mkSingleCmd: mkSingleCmd,
    mkPublicSend: mkPublicSend
  },
  lang: {
    mkExp: mkExp,
    mkMeta: mkMeta
  },
  simple: {
    exec: {
      createCommand: simpleExecCommand,
      createPollRequest: simplePollRequestFromExec,
      createListenRequest: simpleListenRequestFromExec
    }
  }
};
