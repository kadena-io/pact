#!/usr/bin/env node
var fs = require('fs');
var nacl = require('tweetnacl');
var pact = require('./pact-lang-api.js');

var args = {};
var key = undefined;
process.argv.slice(2).forEach(function (v,i,a) {
    if (key) {
        args[key] = v; key = undefined;
    } else {
        if (v[0]='-') { key = v.slice(1); }
    }
});
var usage = "\nload.js: create JSON for loading Pact code \n\n\
Arguments: [-cf codefile] [-c code] -n nonce -s sk -p pk [-df datafile] [-d data] [-t to -f from]\n\n\
  codefile    filepath containing pact code to load \n\
  code        pact code to execute \n\
  datafile    filepath containing JSON data to accompany pact code load \n\
  data        JSON string of data to accompany pact code load \n\
  nonce       nonce value for data payload \n\
  sk          secret key \n\
  pk          public key \n\
  to          Private message sender entity \n\
  from        Private message recipient JSON list \n\
";

function die(msg) {
    console.error("ERROR: " + msg);
    process.exit(1);
}
function dieu(msg) { die(msg + "\n\n" + usage); }

function read(fp,cb) {
    fs.readFile(fp,'utf8',function(err,data) {
        if (err) { die(err); }
        cb(data);
    });
}
function go(code,data) {
    var kp = { publicKey: args.p, secretKey: args.s};
    var addy;
    if (args.t && args.f) {
        addy = { "from": args.f, "to": JSON.parse(args.t) };
    }
    var msg = pact.simple.exec.createCommand(kp,args.n,code,JSON.parse(data),addy);
    console.log(JSON.stringify(msg));
}

if (!args.cf && !args.c) { dieu("Missing code or codefile argument"); }
if (!args.n) { dieu("Missing nonce argument"); }
if (!args.s) { dieu("Missing sk argument"); }
if (!args.p) { dieu("Missing pk argument"); }
if ((args.t && !args.f) || (!args.t && args.f)) { dieu("Must provide both TO and FROM when formatting private payload"); }

function doData(code) {
    if (args.df) {
        read(args.df,function(data) {
            go(code,data);
        });
    } else {
        if (args.d) {
            go(code,args.d);
        } else {
            go(code,"{}");
        }
    }
}

if (args.cf) {
    read(args.cf,doData);
} else {
    doData(args.c);
}
