ace.define("ace/mode/folding/pact", ["require", "exports", "module", "ace/lib/oop", "ace/range", "ace/mode/folding/fold_mode"], function(e, t, n) {
    "use strict";
    var r = e("../../lib/oop"),
        i = e("../../range").Range,
        s = e("./fold_mode").FoldMode,
        o = t.FoldMode = function(e) {
            e && (this.foldingStartMarker = new RegExp(this.foldingStartMarker.source.replace(/\|[^|]*?$/, "|" + e.start))
                  , this.foldingStopMarker = new RegExp(this.foldingStopMarker.source.replace(/\|[^|]*?$/, "|" + e.end)))
        };
    r.inherits(o, s),
    function() {
        this.foldingStartMarker = /(?!)/;
        this.foldingStopMarker = /(?!)/;
        var singleLineCommentRe = /^\s*;.*$/;
        var txRegionRe = /^\s*\(begin-tx\b/;
        this._getFoldWidgetBase = this.getFoldWidget;
        this.getFoldWidget = function(e, t, n) {
                var r = e.getLine(n);
                if (singleLineCommentRe.test(r) && !txRegionRe.test(r)) return "";
                var i = this._getFoldWidgetBase(e, t, n);
                return !i && txRegionRe.test(r) ? "start" : i
        };
        this.getFoldWidgetRange = function(e, t, n, r) {
                var i = e.getLine(n);
                if (txRegionRe.test(i)) return this.getTxBlock(e, i, n);
        };
        this.getTxBlock = function(e, t, n) {
                var r = t.search(/\s*$/),
                    s = e.getLength(),
                    o = n,
                    u = /^\s*\((commit|rollback)-tx\)/,
                    a = 1;
                while (++n < s) {
                    t = e.getLine(n);
                    var f = u.exec(t);
                    if (!f) continue;
                    a--;
                    if (!a) break
                }
                var l = n;
                if (l > o) return new i(o, r, l, 0);
            }
        }.call(o.prototype)
}), ace.define("ace/mode/pact", ["require", "exports", "module", "ace/lib/oop", "ace/mode/text", "ace/tokenizer", "ace/mode/pact_highlight_rules", "ace/mode/matching_parens_outdent", "ace/range"], function(e, t, n) {
    var r = e("../lib/oop"),
        i = e("./text").Mode,
        s = e("../tokenizer").Tokenizer,
        o = e("./pact_highlight_rules").PactHighlightRules,
        u = e("./matching_parens_outdent").MatchingParensOutdent,
        a = e("../range").Range,
        l = e("./folding/pact").FoldMode,
        f = function() {
            this.$tokenizer = new s((new o).getRules()), this.$outdent = new u, this.foldingRules = new l
        };
    r.inherits(f, i),
        function() {
            this.lineCommentStart = ";", this.getNextLineIndent = function(e, t, n) {
                var r = this.$getIndent(t),
                    i = this.$tokenizer.getLineTokens(t, e),
                    s = i.tokens;
                if (s.length && s[s.length - 1].type == "comment") return r;
                if (e == "start") {
                    var o = t.match(/[\(\[]/);
                    o && (r += "  "), o = t.match(/[\)]/), o && (r = "")
                }
                return r
            }, this.checkOutdent = function(e, t, n) {
                return this.$outdent.checkOutdent(t, n)
            }, this.autoOutdent = function(e, t, n) {
                this.$outdent.autoOutdent(t, n)
            }
        }.call(f.prototype), t.Mode = f
}), ace.define("ace/mode/pact_highlight_rules", ["require", "exports", "module", "ace/lib/oop", "ace/mode/text_highlight_rules"], function(e, t, n) {
    var r = e("../lib/oop"),
        i = e("./text_highlight_rules").TextHighlightRules,
        s = function() {
            var e = '> >= < <= = != or and not - + * / ' +
                    'time parse-time add-time diff-time minutes hours days ' +
                    'create-table with-read with-default-read read keys ' +
                    'txids write insert update txlog ' +
                    'describe-table describe-keyset describe-module ' +
                    'read-keyset define-keyset with-keyset keys-all keys-any keys-2 ' +
                    'format enforce env-data env-keys',
                t = 'use module defun defpact step step-with-rollback let let* defconst ' +
                    'load env-keys env-data env-step env-entity begin-tx commit-tx rollback-tx expect expect-failure ' ,
                n = "true false",
                r = this.createKeywordMapper({
                    keyword: t,
                    "constant.language": n,
                    "support.function": e
                }, "identifier", !1, " ");
            this.$rules = {
                start : [
                    {
                        token : "comment",
                        regex : ";.*$"
                    }, {
                        token : "keyword", //parens
                        regex : "[\\(|\\)]"
                    }, {
                        token : "keyword", //lists
                        regex : "[\\'\\(]"
                    }, {
                        token : "keyword", //sets and maps
                        regex : "[\\{\\:=\\}|\\{\\:\\}]"
                    }, {
                            token : "keyword", // ampersands
                            regex : '[\\&]'
                    }, {
                            token : "keyword", // metadata
                            regex : '[\\#\\^\\{]'
                    }, {
                            token : "keyword", // anonymous fn syntactic sugar
                            regex : '[\\%]'
                    }, {
                            token : "keyword", // deref reader macro
                            regex : '[@]'
                    }, {
                        token : "constant.numeric", // hex
                        regex : "0[xX][0-9a-fA-F]+\\b"
                    }, {
                        token : "constant.numeric", // float
                        regex : "[+-]?\\d+(?:(?:\\.\\d*)?(?:[eE][+-]?\\d+)?)?\\b"
                    }, {
                        token : "constant.language",
                        regex : '[!|\\$|%|&|\\*|\\-\\-|\\-|\\+\\+|\\+||=|!=|<=|>=|<>|<|>|!|&&]'
                    }, {
                        token : r,
                        regex : "[a-zA-Z_$][a-zA-Z0-9_$\\-]*\\b"
                    }, {
                        token : "string", // single line
                        regex : '"',
                        next: "string"
                    }, {
                        token : "constant", // symbol
                        regex : /:[^()\[\]{}'"\^%`,;\s]+/
                    }, {
                        token : "string.regexp", //Regular Expressions
                        regex : '/#"(?:\\.|(?:\\")|[^""\n])*"/g'
                    }

                ],
                string: [{
                    token: "constant.language.escape",
                    regex: "\\\\.|\\\\$"
                }, {
                    token: "string",
                    regex: '[^"\\\\]+'
                }, {
                    token: "string",
                    regex: '"',
                    next: "start"
                }]
            }
        };
    r.inherits(s, i), t.PactHighlightRules = s
}), ace.define("ace/mode/matching_parens_outdent", ["require", "exports", "module", "ace/range"], function(e, t, n) {
    var r = e("../range").Range,
        i = function() {};
    (function() {
        this.checkOutdent = function(e, t) {
            return /^\s+$/.test(e) ? /^\s*\)/.test(t) : !1
        }, this.autoOutdent = function(e, t) {
            var n = e.getLine(t),
                i = n.match(/^(\s*\))/);
            if (!i) return 0;
            var s = i[1].length,
                o = e.findMatchingBracket({
                    row: t,
                    column: s
                });
            if (!o || o.row == t) return 0;
            var u = this.$getIndent(e.getLine(o.row));
            e.replace(new r(t, 0, t, s - 1), u)
        }, this.$getIndent = function(e) {
            var t = e.match(/^(\s+)/);
            return t ? t[1] : ""
        }
    }).call(i.prototype), t.MatchingParensOutdent = i
})
