.. _builtins:

Built-in Functions
==================

.. _General:

General
-------

at
~~

*idx* ``integer`` *list* ``[<l>]`` *→* ``<a>``

*idx* ``string`` *object* ``object:<{o}>`` *→* ``<a>``

Index LIST at IDX, or get value with key IDX from OBJECT.

.. code:: lisp

   pact> (at 1 [1 2 3])
   2
   pact> (at "bar" { "foo": 1, "bar": 2 })
   2

bind
~~~~

*src* ``object:<{row}>`` *binding* ``binding:<{row}>`` *→* ``<a>``

Special form evaluates SRC to an object which is bound to with BINDINGS
over subsequent body statements.

.. code:: lisp

   pact> (bind { "a": 1, "b": 2 } { "a" := a-value } a-value)
   1

compose
~~~~~~~

*x* ``(x:<a> -> <b>)`` *y* ``(x:<b> -> <c>)`` *value* ``<a>``
*→* ``<c>``

Compose X and Y, such that X operates on VALUE, and Y on the results of
X.

.. code:: lisp

   pact> (filter (compose (length) (< 2)) ["my" "dog" "has" "fleas"])
   ["dog" "has" "fleas"]

constantly
~~~~~~~~~~

*value* ``<a>`` *ignore1* ``<b>`` *→* ``<a>``

*value* ``<a>`` *ignore1* ``<b>`` *ignore2* ``<c>`` *→* ``<a>``

*value* ``<a>`` *ignore1* ``<b>`` *ignore2* ``<c>`` *ignore3* ``<d>``
*→* ``<a>``

Lazily ignore arguments IGNORE\* and return VALUE.

.. code:: lisp

   pact> (filter (constantly true) [1 2 3])
   [1 2 3]

contains
~~~~~~~~

*value* ``<a>`` *list* ``[<a>]`` *→* ``bool``

*key* ``<a>`` *object* ``object:<{o}>`` *→* ``bool``

*value* ``string`` *string* ``string`` *→* ``bool``

Test that LIST or STRING contains VALUE, or that OBJECT has KEY entry.

.. code:: lisp

   pact> (contains 2 [1 2 3])
   true
   pact> (contains 'name { 'name: "Ted", 'age: 72 })
   true
   pact> (contains "foo" "foobar")
   true

drop
~~~~

*count* ``integer`` *list* ``<a[[<l>],string]>``
*→* ``<a[[<l>],string]>``

*keys* ``[string]`` *object* ``object:<{o}>`` *→* ``object:<{o}>``

Drop COUNT values from LIST (or string), or entries having keys in KEYS
from OBJECT. If COUNT is negative, drop from end.

.. code:: lisp

   pact> (drop 2 "vwxyz")
   "xyz"
   pact> (drop (- 2) [1 2 3 4 5])
   [1 2 3]
   pact> (drop ['name] { 'name: "Vlad", 'active: false})
   {"active": false}

enforce
~~~~~~~

*test* ``bool`` *msg* ``string`` *→* ``bool``

Fail transaction with MSG if pure expression TEST is false. Otherwise,
returns true.

.. code:: lisp

   pact> (enforce (!= (+ 2 2) 4) "Chaos reigns")
   <interactive>:0:0: Chaos reigns

enforce-one
~~~~~~~~~~~

*msg* ``string`` *tests* ``[bool]`` *→* ``bool``

Run TESTS in order (in pure context, plus keyset enforces). If all fail,
fail transaction. Short-circuits on first success.

.. code:: lisp

   pact> (enforce-one "Should succeed on second test" [(enforce false "Skip me") (enforce (= (+ 2 2) 4) "Chaos reigns")])
   true

enforce-pact-version
~~~~~~~~~~~~~~~~~~~~

*min-version* ``string`` *→* ``bool``

*min-version* ``string`` *max-version* ``string`` *→* ``bool``

Enforce runtime pact version as greater than or equal MIN-VERSION, and
less than or equal MAX-VERSION. Version values are matched numerically
from the left, such that ‘2’, ‘2.2’, and ‘2.2.3’ would all allow
‘2.2.3’.

.. code:: lisp

   pact> (enforce-pact-version "2.3")
   true

Top level only: this function will fail if used in module code.

filter
~~~~~~

*app* ``(x:<a> -> bool)`` *list* ``[<a>]`` *→* ``[<a>]``

Filter LIST by applying APP to each element. For each true result, the
original value is kept.

.. code:: lisp

   pact> (filter (compose (length) (< 2)) ["my" "dog" "has" "fleas"])
   ["dog" "has" "fleas"]

fold
~~~~

*app* ``(x:<a> y:<b> -> <a>)`` *init* ``<a>`` *list* ``[<b>]``
*→* ``<a>``

Iteratively reduce LIST by applying APP to last result and element,
starting with INIT.

.. code:: lisp

   pact> (fold (+) 0 [100 10 5])
   115

format
~~~~~~

*template* ``string`` *vars* ``list`` *→* ``string``

Interpolate VARS into TEMPLATE using {}.

.. code:: lisp

   pact> (format "My {} has {}" ["dog" "fleas"])
   "My dog has fleas"

hash
~~~~

*value* ``<a>`` *→* ``string``

Compute BLAKE2b 512-bit hash of VALUE. Strings are converted directly
while other values are converted using their JSON representation.

.. code:: lisp

   pact> (hash "hello")
   "e4cfa39a3d37be31c59609e807970799caa68a19bfaa15135f165085e01d41a65ba1e1b146aeb6bd0092b49eac214c103ccfa3a365954bbbe52f74a2b3620c94"
   pact> (hash { 'foo: 1 })
   "61d3c8775e151b4582ca7f9a885a9b2195d5aa6acc58ddca61a504e9986bb8c06eeb37af722ad848f9009053b6379677bf111e25a680ab41a209c4d56ff1e183"

identity
~~~~~~~~

*value* ``<a>`` *→* ``<a>``

Return provided value.

.. code:: lisp

   pact> (map (identity) [1 2 3])
   [1 2 3]

if
~~

*cond* ``bool`` *then* ``<a>`` *else* ``<a>`` *→* ``<a>``

Test COND. If true, evaluate THEN. Otherwise, evaluate ELSE.

.. code:: lisp

   pact> (if (= (+ 2 2) 4) "Sanity prevails" "Chaos reigns")
   "Sanity prevails"

length
~~~~~~

*x* ``<a[[<l>],string,object:<{o}>]>`` *→* ``integer``

Compute length of X, which can be a list, a string, or an object.

.. code:: lisp

   pact> (length [1 2 3])
   3
   pact> (length "abcdefgh")
   8
   pact> (length { "a": 1, "b": 2 })
   2

list
~~~~

*elems* ``*`` *→* ``list``

Create list from ELEMS. Deprecated in Pact 2.1.1 with literal list
support.

.. code:: lisp

   pact> (list 1 2 3)
   [1 2 3]

list-modules
~~~~~~~~~~~~

*→* ``[string]``

List modules available for loading.

Top level only: this function will fail if used in module code.

make-list
~~~~~~~~~

*length* ``integer`` *value* ``<a>`` *→* ``[<a>]``

Create list by repeating VALUE LENGTH times.

.. code:: lisp

   pact> (make-list 5 true)
   [true true true true true]

map
~~~

*app* ``(x:<b> -> <a>)`` *list* ``[<b>]`` *→* ``[<a>]``

Apply APP to each element in LIST, returning a new list of results.

.. code:: lisp

   pact> (map (+ 1) [1 2 3])
   [2 3 4]

pact-id
~~~~~~~

*→* ``integer``

Return ID if called during current pact execution, failing if not.

pact-version
~~~~~~~~~~~~

*→* ``string``

Obtain current pact build version.

.. code:: lisp

   pact> (pact-version)
   "2.5.0"

Top level only: this function will fail if used in module code.

read-decimal
~~~~~~~~~~~~

*key* ``string`` *→* ``decimal``

Parse KEY string or number value from top level of message data body as
decimal.

.. code:: lisp

   (defun exec ()
      (transfer (read-msg "from") (read-msg "to") (read-decimal "amount")))

read-integer
~~~~~~~~~~~~

*key* ``string`` *→* ``integer``

Parse KEY string or number value from top level of message data body as
integer.

.. code:: lisp

   (read-integer "age")

read-msg
~~~~~~~~

*→* ``<a>``

*key* ``string`` *→* ``<a>``

Read KEY from top level of message data body, or data body itself if not
provided. Coerces value to their corresponding pact type: String ->
string, Number -> integer, Boolean -> bool, List -> list, Object ->
object. However, top-level values are provided as a ‘value’ JSON type.

.. code:: lisp

   (defun exec ()
      (transfer (read-msg "from") (read-msg "to") (read-decimal "amount")))

remove
~~~~~~

*key* ``string`` *object* ``object:<{o}>`` *→* ``object:<{o}>``

Remove entry for KEY from OBJECT.

.. code:: lisp

   pact> (remove "bar" { "foo": 1, "bar": 2 })
   {"foo": 1}

resume
~~~~~~

*binding* ``binding:<{y}>`` *body* ``*`` *→* ``<a>``

Special form binds to a yielded object value from the prior step
execution in a pact.

reverse
~~~~~~~

*list* ``[<a>]`` *→* ``[<a>]``

Reverse LIST.

.. code:: lisp

   pact> (reverse [1 2 3])
   [3 2 1]

sort
~~~~

*values* ``[<a>]`` *→* ``[<a>]``

*fields* ``[string]`` *values* ``[object:<{o}>]`` *→* ``[object:<{o}>]``

Sort a homogeneous list of primitive VALUES, or objects using supplied
FIELDS list.

.. code:: lisp

   pact> (sort [3 1 2])
   [1 2 3]
   pact> (sort ['age] [{'name: "Lin",'age: 30} {'name: "Val",'age: 25}])
   [{"name": "Val", "age": 25} {"name": "Lin", "age": 30}]

str-to-int
~~~~~~~~~~

*str-val* ``string`` *→* ``integer``

*base* ``integer`` *str-val* ``string`` *→* ``integer``

Compute the integer value of STR-VAL in base 10, or in BASE if
specified. STR-VAL must be <= 128 chars in length and BASE must be
between 2 and 16.

.. code:: lisp

   pact> (str-to-int 16 "123456")
   1193046
   pact> (str-to-int "abcdef123456")
   1123455123456

take
~~~~

*count* ``integer`` *list* ``<a[[<l>],string]>``
*→* ``<a[[<l>],string]>``

*keys* ``[string]`` *object* ``object:<{o}>`` *→* ``object:<{o}>``

Take COUNT values from LIST (or string), or entries having keys in KEYS
from OBJECT. If COUNT is negative, take from end.

.. code:: lisp

   pact> (take 2 "abcd")
   "ab"
   pact> (take (- 3) [1 2 3 4 5])
   [3 4 5]
   pact> (take ['name] { 'name: "Vlad", 'active: false})
   {"name": "Vlad"}

tx-hash
~~~~~~~

*→* ``string``

Obtain hash of current transaction as a string.

.. code:: lisp

   pact> (tx-hash)
   "786a02f742015903c6c6fd852552d272912f4740e15847618a86e217f71f5419d25e1031afee585313896444934eb04b903a685b1448b755d56f701afe9be2ce"

typeof
~~~~~~

*x* ``<a>`` *→* ``string``

Returns type of X as string.

.. code:: lisp

   pact> (typeof "hello")
   "string"

where
~~~~~

*field* ``string`` *app* ``(x:<a> -> bool)`` *value* ``object:<{row}>``
*→* ``bool``

Utility for use in ‘filter’ and ‘select’ applying APP to FIELD in VALUE.

.. code:: lisp

   pact> (filter (where 'age (> 20)) [{'name: "Mary",'age: 30} {'name: "Juan",'age: 15}])
   [{"name": "Juan", "age": 15}]

yield
~~~~~

*OBJECT* ``object:<{y}>`` *→* ``object:<{y}>``

Yield OBJECT for use with ‘resume’ in following pact step. The object is
similar to database row objects, in that only the top level can be bound
to in ‘resume’; nested objects are converted to opaque JSON values.

.. code:: lisp

   (yield { "amount": 100.0 })

.. _Database:

Database
--------

create-table
~~~~~~~~~~~~

*table* ``table:<{row}>`` *→* ``string``

Create table TABLE.

.. code:: lisp

   (create-table accounts)

Top level only: this function will fail if used in module code.

describe-keyset
~~~~~~~~~~~~~~~

*keyset* ``string`` *→* ``value``

Get metadata for KEYSET.

Top level only: this function will fail if used in module code.

describe-module
~~~~~~~~~~~~~~~

*module* ``string`` *→* ``value``

Get metadata for MODULE. Returns an object with ‘name’, ‘hash’,
‘blessed’, ‘code’, and ‘keyset’ fields.

.. code:: lisp

   (describe-module 'my-module)

Top level only: this function will fail if used in module code.

describe-table
~~~~~~~~~~~~~~

*table* ``table:<{row}>`` *→* ``value``

Get metadata for TABLE. Returns an object with ‘name’, ‘hash’,
‘blessed’, ‘code’, and ‘keyset’ fields.

.. code:: lisp

   (describe-table accounts)

Top level only: this function will fail if used in module code.

insert
~~~~~~

*table* ``table:<{row}>`` *key* ``string`` *object* ``object:<{row}>``
*→* ``string``

Write entry in TABLE for KEY of OBJECT column data, failing if data
already exists for KEY.

.. code:: lisp

   (insert accounts { "balance": 0.0, "note": "Created account." })

keylog
~~~~~~

*table* ``table:<{row}>`` *key* ``string`` *txid* ``integer``
*→* ``[object]``

Return updates to TABLE for a KEY in transactions at or after TXID, in a
list of objects indexed by txid.

.. code:: lisp

   (keylog accounts "Alice" 123485945)

keys
~~~~

*table* ``table:<{row}>`` *→* ``[string]``

Return all keys in TABLE.

.. code:: lisp

   (keys accounts)

read
~~~~

*table* ``table:<{row}>`` *key* ``string`` *→* ``object:<{row}>``

*table* ``table:<{row}>`` *key* ``string`` *columns* ``[string]``
*→* ``object:<{row}>``

Read row from TABLE for KEY, returning database record object, or just
COLUMNS if specified.

.. code:: lisp

   (read accounts id ['balance 'ccy])

select
~~~~~~

*table* ``table:<{row}>`` *where* ``(row:object:<{row}> -> bool)``
*→* ``[object:<{row}>]``

*table* ``table:<{row}>`` *columns* ``[string]``
*where* ``(row:object:<{row}> -> bool)`` *→* ``[object:<{row}>]``

Select full rows or COLUMNS from table by applying WHERE to each row to
get a boolean determining inclusion.

.. code:: lisp

   (select people ['firstName,'lastName] (where 'name (= "Fatima")))
   (select people (where 'age (> 30)))

txids
~~~~~

*table* ``table:<{row}>`` *txid* ``integer`` *→* ``[integer]``

Return all txid values greater than or equal to TXID in TABLE.

.. code:: lisp

   (txids accounts 123849535)

txlog
~~~~~

*table* ``table:<{row}>`` *txid* ``integer`` *→* ``[value]``

Return all updates to TABLE performed in transaction TXID.

.. code:: lisp

   (txlog accounts 123485945)

update
~~~~~~

*table* ``table:<{row}>`` *key* ``string`` *object* ``object:<{row}>``
*→* ``string``

Write entry in TABLE for KEY of OBJECT column data, failing if data does
not exist for KEY.

.. code:: lisp

   (update accounts { "balance": (+ bal amount), "change": amount, "note": "credit" })

with-default-read
~~~~~~~~~~~~~~~~~

*table* ``table:<{row}>`` *key* ``string`` *defaults* ``object:<{row}>``
*bindings* ``binding:<{row}>`` *→* ``<a>``

Special form to read row from TABLE for KEY and bind columns per
BINDINGS over subsequent body statements. If row not found, read columns
from DEFAULTS, an object with matching key names.

.. code:: lisp

   (with-default-read accounts id { "balance": 0, "ccy": "USD" } { "balance":= bal, "ccy":= ccy }
      (format "Balance for {} is {} {}" [id bal ccy]))

with-read
~~~~~~~~~

*table* ``table:<{row}>`` *key* ``string``
*bindings* ``binding:<{row}>`` *→* ``<a>``

Special form to read row from TABLE for KEY and bind columns per
BINDINGS over subsequent body statements.

.. code:: lisp

   (with-read accounts id { "balance":= bal, "ccy":= ccy }
      (format "Balance for {} is {} {}" [id bal ccy]))

write
~~~~~

*table* ``table:<{row}>`` *key* ``string`` *object* ``object:<{row}>``
*→* ``string``

Write entry in TABLE for KEY of OBJECT column data.

.. code:: lisp

   (write accounts { "balance": 100.0 })

.. _Time:

Time
----

add-time
~~~~~~~~

*time* ``time`` *seconds* ``decimal`` *→* ``time``

*time* ``time`` *seconds* ``integer`` *→* ``time``

Add SECONDS to TIME; SECONDS can be integer or decimal.

.. code:: lisp

   pact> (add-time (time "2016-07-22T12:00:00Z") 15)
   "2016-07-22T12:00:15Z"

days
~~~~

*n* ``decimal`` *→* ``decimal``

*n* ``integer`` *→* ``decimal``

N days, for use with ‘add-time’

.. code:: lisp

   pact> (add-time (time "2016-07-22T12:00:00Z") (days 1))
   "2016-07-23T12:00:00Z"

diff-time
~~~~~~~~~

*time1* ``time`` *time2* ``time`` *→* ``decimal``

Compute difference between TIME1 and TIME2 in seconds.

.. code:: lisp

   pact> (diff-time (parse-time "%T" "16:00:00") (parse-time "%T" "09:30:00"))
   23400

format-time
~~~~~~~~~~~

*format* ``string`` *time* ``time`` *→* ``string``

Format TIME using FORMAT. See `“Time Formats”
docs <pact-reference.html#time-formats>`__ for supported formats.

.. code:: lisp

   pact> (format-time "%F" (time "2016-07-22T12:00:00Z"))
   "2016-07-22"

hours
~~~~~

*n* ``decimal`` *→* ``decimal``

*n* ``integer`` *→* ``decimal``

N hours, for use with ‘add-time’

.. code:: lisp

   pact> (add-time (time "2016-07-22T12:00:00Z") (hours 1))
   "2016-07-22T13:00:00Z"

minutes
~~~~~~~

*n* ``decimal`` *→* ``decimal``

*n* ``integer`` *→* ``decimal``

N minutes, for use with ‘add-time’.

.. code:: lisp

   pact> (add-time (time "2016-07-22T12:00:00Z") (minutes 1))
   "2016-07-22T12:01:00Z"

parse-time
~~~~~~~~~~

*format* ``string`` *utcval* ``string`` *→* ``time``

Construct time from UTCVAL using FORMAT. See `“Time Formats”
docs <pact-reference.html#time-formats>`__ for supported formats.

.. code:: lisp

   pact> (parse-time "%F" "2016-09-12")
   "2016-09-12T00:00:00Z"

time
~~~~

*utcval* ``string`` *→* ``time``

Construct time from UTCVAL using ISO8601 format (%Y-%m-%dT%H:%M:%SZ).

.. code:: lisp

   pact> (time "2016-07-22T11:26:35Z")
   "2016-07-22T11:26:35Z"

.. _Operators:

Operators
---------

.. _bangeq:

!=
~~

*x* ``<a[integer,string,time,decimal,bool,[<l>],object:<{o}>,keyset]>``
*y* ``<a[integer,string,time,decimal,bool,[<l>],object:<{o}>,keyset]>``
*→* ``bool``

True if X does not equal Y.

.. code:: lisp

   pact> (!= "hello" "goodbye")
   true

.. _star:

\*
~~

*x* ``<a[integer,decimal]>`` *y* ``<a[integer,decimal]>``
*→* ``<a[integer,decimal]>``

*x* ``<a[integer,decimal]>`` *y* ``<b[integer,decimal]>``
*→* ``decimal``

Multiply X by Y.

.. code:: lisp

   pact> (* 0.5 10.0)
   5
   pact> (* 3 5)
   15

.. _plus:

\+
~~

*x* ``<a[integer,decimal]>`` *y* ``<a[integer,decimal]>``
*→* ``<a[integer,decimal]>``

*x* ``<a[integer,decimal]>`` *y* ``<b[integer,decimal]>``
*→* ``decimal``

*x* ``<a[string,[<l>],object:<{o}>]>``
*y* ``<a[string,[<l>],object:<{o}>]>``
*→* ``<a[string,[<l>],object:<{o}>]>``

Add numbers, concatenate strings/lists, or merge objects.

.. code:: lisp

   pact> (+ 1 2)
   3
   pact> (+ 5.0 0.5)
   5.5
   pact> (+ "every" "body")
   "everybody"
   pact> (+ [1 2] [3 4])
   [1 2 3 4]
   pact> (+ { "foo": 100 } { "foo": 1, "bar": 2 })
   {"bar": 2, "foo": 100}

.. _minus:

\-
~~

*x* ``<a[integer,decimal]>`` *y* ``<a[integer,decimal]>``
*→* ``<a[integer,decimal]>``

*x* ``<a[integer,decimal]>`` *y* ``<b[integer,decimal]>``
*→* ``decimal``

*x* ``<a[integer,decimal]>`` *→* ``<a[integer,decimal]>``

Negate X, or subtract Y from X.

.. code:: lisp

   pact> (- 1.0)
   -1.0
   pact> (- 3 2)
   1

.. _slash:

/
~

*x* ``<a[integer,decimal]>`` *y* ``<a[integer,decimal]>``
*→* ``<a[integer,decimal]>``

*x* ``<a[integer,decimal]>`` *y* ``<b[integer,decimal]>``
*→* ``decimal``

Divide X by Y.

.. code:: lisp

   pact> (/ 10.0 2.0)
   5
   pact> (/ 8 3)
   2

.. _lt:

<
~

*x* ``<a[integer,decimal,string,time]>``
*y* ``<a[integer,decimal,string,time]>`` *→* ``bool``

True if X < Y.

.. code:: lisp

   pact> (< 1 3)
   true
   pact> (< 5.24 2.52)
   false
   pact> (< "abc" "def")
   true

.. _lteq:

<=
~~

*x* ``<a[integer,decimal,string,time]>``
*y* ``<a[integer,decimal,string,time]>`` *→* ``bool``

True if X <= Y.

.. code:: lisp

   pact> (<= 1 3)
   true
   pact> (<= 5.24 2.52)
   false
   pact> (<= "abc" "def")
   true

.. _eq:

=
~

*x* ``<a[integer,string,time,decimal,bool,[<l>],object:<{o}>,keyset]>``
*y* ``<a[integer,string,time,decimal,bool,[<l>],object:<{o}>,keyset]>``
*→* ``bool``

True if X equals Y.

.. code:: lisp

   pact> (= [1 2 3] [1 2 3])
   true
   pact> (= 'foo "foo")
   true
   pact> (= { 1: 2 } { 1: 2})
   true

.. _gt:

>
~

*x* ``<a[integer,decimal,string,time]>``
*y* ``<a[integer,decimal,string,time]>`` *→* ``bool``

True if X > Y.

.. code:: lisp

   pact> (> 1 3)
   false
   pact> (> 5.24 2.52)
   true
   pact> (> "abc" "def")
   false

.. _gteq:

>=
~~

*x* ``<a[integer,decimal,string,time]>``
*y* ``<a[integer,decimal,string,time]>`` *→* ``bool``

True if X >= Y.

.. code:: lisp

   pact> (>= 1 3)
   false
   pact> (>= 5.24 2.52)
   true
   pact> (>= "abc" "def")
   false

.. _hat:

^
~

*x* ``<a[integer,decimal]>`` *y* ``<a[integer,decimal]>``
*→* ``<a[integer,decimal]>``

*x* ``<a[integer,decimal]>`` *y* ``<b[integer,decimal]>``
*→* ``decimal``

Raise X to Y power.

.. code:: lisp

   pact> (^ 2 3)
   8

abs
~~~

*x* ``decimal`` *→* ``decimal``

*x* ``integer`` *→* ``integer``

Absolute value of X.

.. code:: lisp

   pact> (abs (- 10 23))
   13

and
~~~

*x* ``bool`` *y* ``bool`` *→* ``bool``

Boolean logic with short-circuit.

.. code:: lisp

   pact> (and true false)
   false

and? {#and?}
~~~~~~~~~~~~

*a* ``(x:<r> -> bool)`` *b* ``(x:<r> -> bool)`` *value* ``<r>``
*→* ``bool``

Apply logical ‘and’ to the results of applying VALUE to A and B, with
short-circuit.

.. code:: lisp

   pact> (and? (> 20) (> 10) 15)
   false

ceiling
~~~~~~~

*x* ``decimal`` *prec* ``integer`` *→* ``decimal``

*x* ``decimal`` *→* ``integer``

Rounds up value of decimal X as integer, or to PREC precision as
decimal.

.. code:: lisp

   pact> (ceiling 3.5)
   4
   pact> (ceiling 100.15234 2)
   100.16

exp
~~~

*x* ``<a[integer,decimal]>`` *→* ``<a[integer,decimal]>``

Exp of X.

.. code:: lisp

   pact> (round (exp 3) 6)
   20.085537

floor
~~~~~

*x* ``decimal`` *prec* ``integer`` *→* ``decimal``

*x* ``decimal`` *→* ``integer``

Rounds down value of decimal X as integer, or to PREC precision as
decimal.

.. code:: lisp

   pact> (floor 3.5)
   3
   pact> (floor 100.15234 2)
   100.15

ln
~~

*x* ``<a[integer,decimal]>`` *→* ``<a[integer,decimal]>``

Natural log of X.

.. code:: lisp

   pact> (round (ln 60) 6)
   4.094345

log
~~~

*x* ``<a[integer,decimal]>`` *y* ``<a[integer,decimal]>``
*→* ``<a[integer,decimal]>``

*x* ``<a[integer,decimal]>`` *y* ``<b[integer,decimal]>``
*→* ``decimal``

Log of Y base X.

.. code:: lisp

   pact> (log 2 256)
   8

mod
~~~

*x* ``integer`` *y* ``integer`` *→* ``integer``

X modulo Y.

.. code:: lisp

   pact> (mod 13 8)
   5

not
~~~

*x* ``bool`` *→* ``bool``

Boolean logic.

.. code:: lisp

   pact> (not (> 1 2))
   true

not? {#not?}
~~~~~~~~~~~~

*app* ``(x:<r> -> bool)`` *value* ``<r>`` *→* ``bool``

Apply logical ‘not’ to the results of applying VALUE to APP.

.. code:: lisp

   pact> (not? (> 20) 15)
   false

or
~~

*x* ``bool`` *y* ``bool`` *→* ``bool``

Boolean logic with short-circuit.

.. code:: lisp

   pact> (or true false)
   true

or? {#or?}
~~~~~~~~~~

*a* ``(x:<r> -> bool)`` *b* ``(x:<r> -> bool)`` *value* ``<r>``
*→* ``bool``

Apply logical ‘or’ to the results of applying VALUE to A and B, with
short-circuit.

.. code:: lisp

   pact> (or? (> 20) (> 10) 15)
   true

round
~~~~~

*x* ``decimal`` *prec* ``integer`` *→* ``decimal``

*x* ``decimal`` *→* ``integer``

Performs Banker’s rounding value of decimal X as integer, or to PREC
precision as decimal.

.. code:: lisp

   pact> (round 3.5)
   4
   pact> (round 100.15234 2)
   100.15

sqrt
~~~~

*x* ``<a[integer,decimal]>`` *→* ``<a[integer,decimal]>``

Square root of X.

.. code:: lisp

   pact> (sqrt 25)
   5

.. _Keysets:

Keysets
-------

define-keyset
~~~~~~~~~~~~~

*name* ``string`` *keyset* ``string`` *→* ``string``

Define keyset as NAME with KEYSET. If keyset NAME already exists, keyset
will be enforced before updating to new value.

.. code:: lisp

   (define-keyset 'admin-keyset (read-keyset "keyset"))

Top level only: this function will fail if used in module code.

enforce-keyset
~~~~~~~~~~~~~~

*keyset-or-name* ``<k[string,keyset]>`` *→* ``bool``

Special form to enforce KEYSET-OR-NAME against message keys before
running BODY. KEYSET-OR-NAME can be a symbol of a keyset name or a
keyset object.

.. code:: lisp

   (with-keyset 'admin-keyset ...)
   (with-keyset (read-keyset "keyset") ...)

keys-2
~~~~~~

*count* ``integer`` *matched* ``integer`` *→* ``bool``

Keyset predicate function to match at least 2 keys in keyset.

.. code:: lisp

   pact> (keys-2 3 1)
   false

keys-all
~~~~~~~~

*count* ``integer`` *matched* ``integer`` *→* ``bool``

Keyset predicate function to match all keys in keyset.

.. code:: lisp

   pact> (keys-all 3 3)
   true

keys-any
~~~~~~~~

*count* ``integer`` *matched* ``integer`` *→* ``bool``

Keyset predicate function to match any (at least 1) key in keyset.

.. code:: lisp

   pact> (keys-any 10 1)
   true

read-keyset
~~~~~~~~~~~

*key* ``string`` *→* ``keyset``

Read KEY from message data body as keyset ({ “keys”: KEYLIST, “pred”:
PREDFUN }). PREDFUN should resolve to a keys predicate.

.. code:: lisp

   (read-keyset "admin-keyset")

.. _repl-lib:

REPL-only functions
-------------------

The following functions are loaded automatically into the interactive
REPL, or within script files with a ``.repl`` extension. They are not
available for blockchain-based execution.

begin-tx
~~~~~~~~

*→* ``string``

*name* ``string`` *→* ``string``

Begin transaction with optional NAME.

.. code:: lisp

   (begin-tx "load module")

bench
~~~~~

*exprs* ``*`` *→* ``string``

Benchmark execution of EXPRS.

.. code:: lisp

   (bench (+ 1 2))

commit-tx
~~~~~~~~~

*→* ``string``

Commit transaction.

.. code:: lisp

   (commit-tx)

env-data
~~~~~~~~

*json* ``<a[integer,string,time,decimal,bool,[<l>],object:<{o}>,keyset,value]>``
*→* ``string``

Set transaction JSON data, either as encoded string, or as pact types
coerced to JSON.

.. code:: lisp

   pact> (env-data { "keyset": { "keys": ["my-key" "admin-key"], "pred": "keys-any" } })
   "Setting transaction data"

env-entity
~~~~~~~~~~

*→* ``string``

*entity* ``string`` *→* ``string``

Set environment confidential ENTITY id, or unset with no argument.
Clears any previous pact execution state.

.. code:: lisp

   (env-entity "my-org")
   (env-entity)

env-gas
~~~~~~~

*→* ``integer``

*gas* ``integer`` *→* ``string``

Query gas state, or set it to GAS.

env-gaslimit
~~~~~~~~~~~~

*limit* ``integer`` *→* ``string``

Set environment gas limit to LIMIT.

env-gasprice
~~~~~~~~~~~~

*price* ``decimal`` *→* ``string``

Set environment gas price to PRICE.

env-gasrate
~~~~~~~~~~~

*rate* ``integer`` *→* ``string``

Update gas model to charge constant RATE.

env-hash
~~~~~~~~

*hash* ``string`` *→* ``string``

Set current transaction hash. HASH must be a valid BLAKE2b 512-bit hash.

.. code:: lisp

   pact> (env-hash (hash "hello"))
   "Set tx hash to e4cfa39a3d37be31c59609e807970799caa68a19bfaa15135f165085e01d41a65ba1e1b146aeb6bd0092b49eac214c103ccfa3a365954bbbe52f74a2b3620c94"

env-keys
~~~~~~~~

*keys* ``[string]`` *→* ``string``

Set transaction signature KEYS.

.. code:: lisp

   pact> (env-keys ["my-key" "admin-key"])
   "Setting transaction keys"

env-step
~~~~~~~~

*→* ``string``

*step-idx* ``integer`` *→* ``string``

*step-idx* ``integer`` *rollback* ``bool`` *→* ``string``

*step-idx* ``integer`` *rollback* ``bool`` *resume* ``object:<{y}>``
*→* ``string``

Set pact step state. With no arguments, unset step. With STEP-IDX, set
step index to execute. ROLLBACK instructs to execute rollback
expression, if any. RESUME sets a value to be read via ‘resume’.Clears
any previous pact execution state.

.. code:: lisp

   (env-step 1)
   (env-step 0 true)

expect
~~~~~~

*doc* ``string`` *expected* ``<a>`` *actual* ``<a>`` *→* ``string``

Evaluate ACTUAL and verify that it equals EXPECTED.

.. code:: lisp

   pact> (expect "Sanity prevails." 4 (+ 2 2))
   "Expect: success: Sanity prevails."

expect-failure
~~~~~~~~~~~~~~

*doc* ``string`` *exp* ``<a>`` *→* ``string``

Evaluate EXP and succeed only if it throws an error.

.. code:: lisp

   pact> (expect-failure "Enforce fails on false" (enforce false "Expected error"))
   "Expect failure: success: Enforce fails on false"

json
~~~~

*exp* ``<a>`` *→* ``value``

Encode pact expression EXP as a JSON value. This is only needed for
tests, as Pact values are automatically represented as JSON in API
output.

.. code:: lisp

   pact> (json [{ "name": "joe", "age": 10 } {"name": "mary", "age": 25 }])
   [{"age":10,"name":"joe"},{"age":25,"name":"mary"}]

load
~~~~

*file* ``string`` *→* ``string``

*file* ``string`` *reset* ``bool`` *→* ``string``

Load and evaluate FILE, resetting repl state beforehand if optional
RESET is true.

.. code:: lisp

   (load "accounts.repl")

pact-state
~~~~~~~~~~

*→* ``object``

Inspect state from previous pact execution. Returns object with fields
‘yield’: yield result or ‘false’ if none; ‘step’: executed step;
‘executed’: indicates if step was skipped because entity did not match.

.. code:: lisp

   (pact-state)

print
~~~~~

*value* ``<a>`` *→* ``string``

Output VALUE to terminal as unquoted, unescaped text.

rollback-tx
~~~~~~~~~~~

*→* ``string``

Rollback transaction.

.. code:: lisp

   (rollback-tx)

sig-keyset
~~~~~~~~~~

*→* ``keyset``

Convenience function to build a keyset from keys present in message
signatures, using ‘keys-all’ as the predicate.

typecheck
~~~~~~~~~

*module* ``string`` *→* ``string``

*module* ``string`` *debug* ``bool`` *→* ``string``

Typecheck MODULE, optionally enabling DEBUG output.

verify
~~~~~~

*module* ``string`` *→* ``string``

Verify MODULE, checking that all properties hold.
