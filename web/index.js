

var editor = CodeMirror.fromTextArea(document.getElementById("code-pact"), {
  lineNumbers: true,
  mode: "pact",
  matchBrackets: true,
  extraKeys: { "Ctrl-Enter": function(cm) {
    var v = evalCmd(true,cm.getValue());
    var o = v.success ? v.success : "ERROR: " + v.failure;
    document.getElementById("repl-output").innerHTML = o;
  } }

});
