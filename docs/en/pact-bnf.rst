|image0|

The Pact BNF Grammar
====================

Here ye lies the pact bnf, based on [the current spec](https://pact-language.readthedocs.io/en/stable/pact-reference.html).

Note: happy is more efficient in parsing left-recursive s-expressions, so the grammar is being stated that way
(see: https://www.haskell.org/happy/doc/html/sec-sequences.html).

.. code:: ebnf

(* A program is a collection of modules and expressions *)
program        ::= top_level_list
top_level_list ::=    module top_level_list |
                        expr top_level_list |
                   interface top_level_list | ε

module ::= '(' 'module' ident module_guard doc_or_meta decl_list ')'
interface ::= '(' 'interface' ident doc_or_meta sig_list ')'

(* top level declarations *)
sig_list ::= defconst | defun_sig
decl_list ::= imports decls
imports ::= '(' use ')' { '(' use ')' }
decls ::=  '(' decl ')' { '(' decl ')' }
decl      ::= defun | defschema | defconst | defcap | defpact |
              deftable | bless

module_guard ::= ident | kset

defun_sig ::= 'defun' '(' fun_args ')' doc_or_meta
defun ::= 'defun' '(' fun_args ')' doc_or_meta expr
defconst ::= 'defconst' ident expr
defcap ::= 'defcap' ident '(' fun_args ')' doc_or_meta expr
bless ::= 'bless' hash
defpact ::= 'defpact' ident '(' fun_args ')' doc_or_meta { step }
defschema ::= 'defschema' ident doc_or_meta fields
deftable ::= 'deftable' ident [':' ident] doc_or_meta
use ::= 'use' ident
implements ::= 'implements' ident
step ::= '(' 'step' [ string_lit ] expr ')'
    | '(' 'step-with-rollback' [ string_lit ] expr expr ')'

fields ::= { field }
field ::= ident ':' ident

fun_args ::= arg arg_rest
arg_rest ::= { arg }
arg ::= ident [':' ident]

(* documentation for defuns and modules *)
doc_or_meta ::= doc meta | meta | ε
doc         ::= '@doc' string
meta        ::= '@model' '[' { models } ']'
models      ::=  property | invariant
property    ::= '(' 'property' expr ')'
invariant   ::= '(' 'invariant' expr ')'

(* Expressions. note: expr lists are nonempty, as () in lisp is nil literal *)
expr         ::= '(' ne_expr_list ')' | atom
ne_expr_list ::= expr { expr }

atom     ::= type_ann | let_bind | let_star_bind | atom2
type_ann ::= '(' expr ':' ident ')'
let_bind ::= '(' 'let' (single_bind | multi_bind) expr ')'
let_star_bind ::= '(' 'let*' (single_bind | multi_bind) expr ')'
single_bind ::= '(' ident expr ')'
multi_bind ::= '(' single_bind { single_bind } ')'
atom2    ::= ident | lit | op

ident      ::= letter { ('-' | letter | number) }

(* All literals *)
lit ::= string_lit | symbol | integer | decimal | bool
        bool | list_lit | object_lit | table | kset

string_lit           ::= '\"' string '\"'
symbol               ::= '\'' ident
bool                 ::= 'true' | 'false'
integer              ::= [ '-' ] numbers
decimal              ::= [ '-' ] numbers '.' numbers
list_lit             ::= '[' { expr } ']' | '[' expr { ',' expr } ']'
comma_delimited_list ::= expr comma_delimited_rest | ε
comma_delimited_rest ::= ',' expr | ε
object_lit           ::= '{' kv { ',' kv } '}'
kv                   ::= '\"' ident '\"' ':' expr
kset                 ::= hashes | '{' 'keys' ':' hashes '}' | {' 'keys' ':' hashes ',' 'pred' ':' pred '}'
pred                 ::= 'keys-all' | 'keys-1' | 'keys-2'

numbers ::= number { number }
letter ::= { (uc_letter | lc_letter | '_' ) }
alphanumeric ::= uc_letter | lc_letter | number
uc_letter ::= 'A' | ... | 'Z'
lc_letter ::= 'a' | ... | 'z'
number    ::= '0' | ... | '9'
op ::= (* Printable operators *)
string ::= (* All printable characters *)
hash ::= { alphanumeric } (* yes, this is b64-encoded *)
hashes ::= '[' { hash } ']'

.. |image0| image:: img/kadena-logo-210px.png
