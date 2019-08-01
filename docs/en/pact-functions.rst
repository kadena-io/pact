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

chain-data
~~~~~~~~~~

*→* ``object:{public-chain-data}``

Get transaction public metadata. Returns an object with ‘chain-id’,
‘block-height’, ‘block-time’, ‘sender’, ‘gas-limit’, ‘gas-price’, and
‘gas-fee’ fields.

.. code:: lisp

   pact> (chain-data)
   {"block-height": 0,"block-time": "1970-01-01T00:00:00Z","chain-id": "","gas-limit": 0,"gas-price": 0,"sender": ""}

compose
~~~~~~~

*x* ``x:<a> -> <b>`` *y* ``x:<b> -> <c>`` *value* ``<a>`` *→* ``<c>``

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

define-namespace
~~~~~~~~~~~~~~~~

*namespace* ``string`` *guard* ``guard`` *→* ``string``

Create a namespace called NAMESPACE where ownership and use of the
namespace is controlled by GUARD. If NAMESPACE is already defined, then
the guard previously defined in NAMESPACE will be enforced, and GUARD
will be rotated in its place.

.. code:: lisp

   (define-namespace 'my-namespace (read-keyset 'my-keyset))

Top level only: this function will fail if used in module code.

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

*app* ``x:<a> -> bool`` *list* ``[<a>]`` *→* ``[<a>]``

Filter LIST by applying APP to each element. For each true result, the
original value is kept.

.. code:: lisp

   pact> (filter (compose (length) (< 2)) ["my" "dog" "has" "fleas"])
   ["dog" "has" "fleas"]

fold
~~~~

*app* ``x:<a> y:<b> -> <a>`` *init* ``<a>`` *list* ``[<b>]`` *→* ``<a>``

Iteratively reduce LIST by applying APP to last result and element,
starting with INIT.

.. code:: lisp

   pact> (fold (+) 0 [100 10 5])
   115

format
~~~~~~

*template* ``string`` *vars* ``[*]`` *→* ``string``

Interpolate VARS into TEMPLATE using {}.

.. code:: lisp

   pact> (format "My {} has {}" ["dog" "fleas"])
   "My dog has fleas"

hash
~~~~

*value* ``<a>`` *→* ``string``

Compute BLAKE2b 256-bit hash of VALUE represented in unpadded
base64-url. Strings are converted directly while other values are
converted using their JSON representation. Non-value-level arguments are
not allowed.

.. code:: lisp

   pact> (hash "hello")
   "Mk3PAn3UowqTLEQfNlol6GsXPe-kuOWJSCU0cbgbcs8"
   pact> (hash { 'foo: 1 })
   "h9BZgylRf_M4HxcBXr15IcSXXXSz74ZC2IAViGle_z4"

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

int-to-str
~~~~~~~~~~

*base* ``integer`` *val* ``integer`` *→* ``string``

Represent integer VAL as a string in BASE. BASE can be 2-16, or 64 for
unpadded base64URL. Only positive values are allowed for base64URL
conversion.

.. code:: lisp

   pact> (int-to-str 16 65535)
   "ffff"
   pact> (int-to-str 64 43981)
   "q80"

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

*elems* ``*`` *→* ``[*]``

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

*app* ``x:<b> -> <a>`` *list* ``[<b>]`` *→* ``[<a>]``

Apply APP to each element in LIST, returning a new list of results.

.. code:: lisp

   pact> (map (+ 1) [1 2 3])
   [2 3 4]

namespace
~~~~~~~~~

*namespace* ``string`` *→* ``string``

Set the current namespace to NAMESPACE. All expressions that occur in a
current transaction will be contained in NAMESPACE, and once committed,
may be accessed via their fully qualified name, which will include the
namespace. Subsequent namespace calls in the same tx will set a new
namespace for all declarations until either the next namespace
declaration, or the end of the tx.

.. code:: lisp

   (namespace 'my-namespace)

Top level only: this function will fail if used in module code.

pact-id
~~~~~~~

*→* ``string``

Return ID if called during current pact execution, failing if not.

pact-version
~~~~~~~~~~~~

*→* ``string``

Obtain current pact build version.

.. code:: lisp

   pact> (pact-version)
   "3.0.1"

Top level only: this function will fail if used in module code.

public-chain-data
~~~~~~~~~~~~~~~~~

Schema type for data returned from ‘chain-data’.

Fields:   ``chain-id:string``   ``block-height:integer``
  ``block-time:time``   ``sender:string``   ``gas-limit:integer``
  ``gas-price:decimal``

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
object.

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

*binding* ``binding:<{r}>`` *→* ``<a>``

Special form binds to a yielded object value from the prior step
execution in a pact. If yield step was executed on a foreign chain,
enforce endorsement via SPV.

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
   [{"name": "Val","age": 25} {"name": "Lin","age": 30}]

str-to-int
~~~~~~~~~~

*str-val* ``string`` *→* ``integer``

*base* ``integer`` *str-val* ``string`` *→* ``integer``

Compute the integer value of STR-VAL in base 10, or in BASE if
specified. STR-VAL can be up to 512 chars in length. BASE must be
between 2 and 16, or 64 to perform unpadded base64url conversion. Each
digit must be in the correct range for the base.

.. code:: lisp

   pact> (str-to-int 16 "abcdef123456")
   188900967593046
   pact> (str-to-int "123456")
   123456
   pact> (str-to-int 64 "q80")
   43981

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
   "DldRwCblQ7Loqy6wYJnaodHl30d3j3eH-qtFzfEv46g"

typeof
~~~~~~

*x* ``<a>`` *→* ``string``

Returns type of X as string.

.. code:: lisp

   pact> (typeof "hello")
   "string"

where
~~~~~

*field* ``string`` *app* ``x:<a> -> bool`` *value* ``object:<{row}>``
*→* ``bool``

Utility for use in ‘filter’ and ‘select’ applying APP to FIELD in VALUE.

.. code:: lisp

   pact> (filter (where 'age (> 20)) [{'name: "Mary",'age: 30} {'name: "Juan",'age: 15}])
   [{"name": "Juan","age": 15}]

yield
~~~~~

*object* ``object:<{y}>`` *→* ``object:<{y}>``

*object* ``object:<{y}>`` *target-chain* ``string`` *→* ``object:<{y}>``

Yield OBJECT for use with ‘resume’ in following pact step. With optional
argument TARGET-CHAIN, target subsequent step to execute on targeted
chain using automated SPV endorsement-based dispatch.

.. code:: lisp

   (yield { "amount": 100.0 })
   (yield { "amount": 100.0 } "some-chain-id")

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

*keyset* ``string`` *→* ``object:*``

Get metadata for KEYSET.

Top level only: this function will fail if used in module code.

describe-module
~~~~~~~~~~~~~~~

*module* ``string`` *→* ``object:*``

Get metadata for MODULE. Returns an object with ‘name’, ‘hash’,
‘blessed’, ‘code’, and ‘keyset’ fields.

.. code:: lisp

   (describe-module 'my-module)

Top level only: this function will fail if used in module code.

describe-table
~~~~~~~~~~~~~~

*table* ``table:<{row}>`` *→* ``object:*``

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

   (insert accounts id { "balance": 0.0, "note": "Created account." })

keylog
~~~~~~

*table* ``table:<{row}>`` *key* ``string`` *txid* ``integer``
*→* ``[object:*]``

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

*table* ``table:<{row}>`` *where* ``row:object:<{row}> -> bool``
*→* ``[object:<{row}>]``

*table* ``table:<{row}>`` *columns* ``[string]``
*where* ``row:object:<{row}> -> bool`` *→* ``[object:<{row}>]``

Select full rows or COLUMNS from table by applying WHERE to each row to
get a boolean determining inclusion.

.. code:: lisp

   (select people ['firstName,'lastName] (where 'name (= "Fatima")))
   (select people (where 'age (> 30)))?

txids
~~~~~

*table* ``table:<{row}>`` *txid* ``integer`` *→* ``[integer]``

Return all txid values greater than or equal to TXID in TABLE.

.. code:: lisp

   (txids accounts 123849535)

txlog
~~~~~

*table* ``table:<{row}>`` *txid* ``integer`` *→* ``[object:*]``

Return all updates to TABLE performed in transaction TXID.

.. code:: lisp

   (txlog accounts 123485945)

update
~~~~~~

*table* ``table:<{row}>`` *key* ``string`` *object* ``object:~<{row}>``
*→* ``string``

Write entry in TABLE for KEY of OBJECT column data, failing if data does
not exist for KEY.

.. code:: lisp

   (update accounts id { "balance": (+ bal amount), "change": amount, "note": "credit" })

with-default-read
~~~~~~~~~~~~~~~~~

*table* ``table:<{row}>`` *key* ``string``
*defaults* ``object:~<{row}>`` *bindings* ``binding:~<{row}>``
*→* ``<a>``

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

   (write accounts id { "balance": 100.0 })

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

& {#&}
~~~~~~

*x* ``integer`` *y* ``integer`` *→* ``integer``

Compute bitwise X and Y.

.. code:: lisp

   pact> (& 2 3)
   2
   pact> (& 5 -7)
   1

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
   {"bar": 2,"foo": 100}

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

Compare alike terms for equality, returning TRUE if X is equal to Y.
Equality comparisons will fail immediately on type mismatch, or if types
are not value types.

.. code:: lisp

   pact> (= [1 2 3] [1 2 3])
   true
   pact> (= 'foo "foo")
   true
   pact> (= { 'a: 2 } { 'a: 2})
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

*a* ``x:<r> -> bool`` *b* ``x:<r> -> bool`` *value* ``<r>`` *→* ``bool``

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

Boolean not.

.. code:: lisp

   pact> (not (> 1 2))
   true

not? {#not?}
~~~~~~~~~~~~

*app* ``x:<r> -> bool`` *value* ``<r>`` *→* ``bool``

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

*a* ``x:<r> -> bool`` *b* ``x:<r> -> bool`` *value* ``<r>`` *→* ``bool``

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

shift
~~~~~

*x* ``integer`` *y* ``integer`` *→* ``integer``

Shift X Y bits left if Y is positive, or right by -Y bits otherwise.
Right shifts perform sign extension on signed number types; i.e. they
fill the top bits with 1 if the x is negative and with 0 otherwise.

.. code:: lisp

   pact> (shift 255 8)
   65280
   pact> (shift 255 -1)
   127
   pact> (shift -255 8)
   -65280
   pact> (shift -255 -1)
   -128

sqrt
~~~~

*x* ``<a[integer,decimal]>`` *→* ``<a[integer,decimal]>``

Square root of X.

.. code:: lisp

   pact> (sqrt 25)
   5

xor
~~~

*x* ``integer`` *y* ``integer`` *→* ``integer``

Compute bitwise X xor Y.

.. code:: lisp

   pact> (xor 127 64)
   63
   pact> (xor 5 -7)
   -4

.. _section-1:

\| {#|}
~~~~~~~

*x* ``integer`` *y* ``integer`` *→* ``integer``

Compute bitwise X or Y.

.. code:: lisp

   pact> (| 2 3)
   3
   pact> (| 5 -7)
   -3

.. _section-2:

~ {#~}
~~~~~~

*x* ``integer`` *→* ``integer``

Reverse all bits in X.

.. code:: lisp

   pact> (~ 15)
   -16

.. _Keysets:

Keysets
-------

define-keyset
~~~~~~~~~~~~~

*name* ``string`` *keyset* ``string`` *→* ``string``

*name* ``string`` *→* ``string``

Define keyset as NAME with KEYSET, or if unspecified, read NAME from
message payload as keyset, similarly to ‘read-keyset’. If keyset NAME
already exists, keyset will be enforced before updating to new value.

.. code:: lisp

   (define-keyset 'admin-keyset (read-keyset "keyset"))

Top level only: this function will fail if used in module code.

enforce-keyset
~~~~~~~~~~~~~~

*guard* ``guard`` *→* ``bool``

*keysetname* ``string`` *→* ``bool``

Execute GUARD, or defined keyset KEYSETNAME, to enforce desired
predicate logic.

.. code:: lisp

   (enforce-keyset 'admin-keyset)
   (enforce-keyset row-guard)

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

.. _Capabilities:

Capabilities
------------

compose-capability
~~~~~~~~~~~~~~~~~~

*capability* ``-> bool`` *→* ``bool``

Specifies and requests grant of CAPABILITY which is an application of a
‘defcap’ production, only valid within a (distinct) ‘defcap’ body, as a
way to compose CAPABILITY with the outer capability such that the scope
of the containing ‘with-capability’ call will “import” this capability.
Thus, a call to ‘(with-capability (OUTER-CAP) OUTER-BODY)’, where the
OUTER-CAP defcap calls ‘(compose-capability (INNER-CAP))’, will result
in INNER-CAP being granted in the scope of OUTER-BODY.

.. code:: lisp

   (compose-capability (TRANSFER src dest))

create-module-guard
~~~~~~~~~~~~~~~~~~~

*name* ``string`` *→* ``guard``

Defines a guard by NAME that enforces the current module admin
predicate.

create-pact-guard
~~~~~~~~~~~~~~~~~

*name* ``string`` *→* ``guard``

Defines a guard predicate by NAME that captures the results of
‘pact-id’. At enforcement time, the success condition is that at that
time ‘pact-id’ must return the same value. In effect this ensures that
the guard will only succeed within the multi-transaction identified by
the pact id.

create-user-guard
~~~~~~~~~~~~~~~~~

*closure* ``-> bool`` *→* ``guard``

Defines a custom guard CLOSURE whose arguments are strictly evaluated at
definition time, to be supplied to indicated function at enforcement
time.

enforce-guard
~~~~~~~~~~~~~

*guard* ``guard`` *→* ``bool``

*keysetname* ``string`` *→* ``bool``

Execute GUARD, or defined keyset KEYSETNAME, to enforce desired
predicate logic.

.. code:: lisp

   (enforce-guard 'admin-keyset)
   (enforce-guard row-guard)

keyset-ref-guard
~~~~~~~~~~~~~~~~

*keyset-ref* ``string`` *→* ``guard``

Creates a guard for the keyset registered as KEYSET-REF with
‘define-keyset’. Concrete keysets are themselves guard types; this
function is specifically to store references alongside other guards in
the database, etc.

require-capability
~~~~~~~~~~~~~~~~~~

*capability* ``-> bool`` *→* ``bool``

Specifies and tests for existing grant of CAPABILITY, failing if not
found in environment.

.. code:: lisp

   (require-capability (TRANSFER src dest))

with-capability
~~~~~~~~~~~~~~~

*capability* ``-> bool`` *body* ``[*]`` *→* ``<a>``

Specifies and requests grant of CAPABILITY which is an application of a
‘defcap’ production. Given the unique token specified by this
application, ensure that the token is granted in the environment during
execution of BODY. ‘with-capability’ can only be called in the same
module that declares the corresponding ‘defcap’, otherwise module-admin
rights are required. If token is not present, the CAPABILITY is
evaluated, with successful completion resulting in the
installation/granting of the token, which will then be revoked upon
completion of BODY. Nested ‘with-capability’ calls for the same token
will detect the presence of the token, and will not re-apply CAPABILITY,
but simply execute BODY. ‘with-capability’ cannot be called from within
an evaluating defcap.

.. code:: lisp

   (with-capability (UPDATE-USERS id) (update users id { salary: new-salary }))

.. _SPV:

SPV
---

verify-spv
~~~~~~~~~~

*type* ``string`` *payload* ``object:<in>`` *→* ``object:<out>``

Performs a platform-specific spv proof of type TYPE on PAYLOAD. The
format of the PAYLOAD object depends on TYPE, as does the format of the
return object. Platforms such as Chainweb will document the specific
payload types and return values.

.. code:: lisp

   (verify-spv "TXOUT" (read-msg "proof"))

.. _Commitments:

Commitments
-----------

decrypt-cc20p1305
~~~~~~~~~~~~~~~~~

*ciphertext* ``string`` *nonce* ``string`` *aad* ``string``
*mac* ``string`` *public-key* ``string`` *secret-key* ``string``
*→* ``string``

Perform decryption of CIPHERTEXT using the CHACHA20-POLY1305
Authenticated Encryption with Associated Data (AEAD) construction
described in IETF RFC 7539. CIPHERTEXT is an unpadded base64url string.
NONCE is a 12-byte base16 string. AAD is base16 additional
authentication data of any length. MAC is the “detached” base16 tag
value for validating POLY1305 authentication. PUBLIC-KEY and SECRET-KEY
are base-16 Curve25519 values to form the DH symmetric key.Result is
unpadded base64URL.

.. code:: lisp

   (decrypt-cc20p1305 ciphertext nonce aad mac pubkey privkey)

validate-keypair
~~~~~~~~~~~~~~~~

*public* ``string`` *secret* ``string`` *→* ``bool``

Enforce that the Curve25519 keypair of (PUBLIC,SECRET) match. Key values
are base-16 strings of length 32.

.. code:: lisp

   (validate-keypair pubkey privkey)

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

continue-pact
~~~~~~~~~~~~~

*step* ``integer`` *→* ``string``

*step* ``integer`` *rollback* ``bool`` *→* ``string``

*step* ``integer`` *rollback* ``bool`` *pact-id* ``string``
*→* ``string``

*step* ``integer`` *rollback* ``bool`` *pact-id* ``string``
*yielded* ``object:<{y}>`` *→* ``string``

Continue previously-initiated pact identified STEP, optionally
specifying ROLLBACK (default is false), PACT-ID of the pact to be
continued (defaults to the pact initiated in the current transaction, if
one is present), and YIELDED value to be read with ‘resume’ (if not
specified, uses yield in most recent pact exec, if any).

.. code:: lisp

   (continue-pact 1)
   (continue-pact 1 true)
   (continue-pact 1 false "[pact-id-hash]"))
   (continue-pact 2 1 false "[pact-id-hash]" { "rate": 0.9 })

env-chain-data
~~~~~~~~~~~~~~

*new-data* ``object:~{public-chain-data}`` *→* ``string``

Update existing entries of ‘chain-data’ with NEW-DATA, replacing those
items only.

.. code:: lisp

   pact> (env-chain-data { "chain-id": "TestNet00/2", "block-height": 20 })
   "Updated public metadata"

env-data
~~~~~~~~

*json* ``<a[integer,string,time,decimal,bool,[<l>],object:<{o}>,keyset]>``
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

env-gasmodel
~~~~~~~~~~~~

*model* ``string`` *→* ``string``

Update gas model to the model named MODEL.

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

Set current transaction hash. HASH must be an unpadded base64-url
encoded BLAKE2b 256-bit hash.

.. code:: lisp

   pact> (env-hash (hash "hello"))
   "Set tx hash to Mk3PAn3UowqTLEQfNlol6GsXPe-kuOWJSCU0cbgbcs8"

env-keys
~~~~~~~~

*keys* ``[string]`` *→* ``string``

Set transaction signature KEYS.

.. code:: lisp

   pact> (env-keys ["my-key" "admin-key"])
   "Setting transaction keys"

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

format-address
~~~~~~~~~~~~~~

*scheme* ``string`` *public-key* ``string`` *→* ``string``

Transform PUBLIC-KEY into an address (i.e. a Pact Runtime Public Key)
depending on its SCHEME.

load
~~~~

*file* ``string`` *→* ``string``

*file* ``string`` *reset* ``bool`` *→* ``string``

Load and evaluate FILE, resetting repl state beforehand if optional
RESET is true.

.. code:: lisp

   (load "accounts.repl")

mock-spv
~~~~~~~~

*type* ``string`` *payload* ``object:*`` *output* ``object:*``
*→* ``string``

Mock a successful call to ‘spv-verify’ with TYPE and PAYLOAD to return
OUTPUT.

.. code:: lisp

   (mock-spv "TXOUT" { 'proof: "a54f54de54c54d89e7f" } { 'amount: 10.0, 'account: "Dave", 'chainId: "1" })

pact-state
~~~~~~~~~~

*→* ``object:*``

*clear* ``bool`` *→* ``object:*``

Inspect state from most recent pact execution. Returns object with
fields ‘pactId’: pact ID; ‘yield’: yield result or ‘false’ if none;
‘step’: executed step; ‘executed’: indicates if step was skipped because
entity did not match. With CLEAR argument, erases pact from repl state.

.. code:: lisp

   (pact-state)
   (pact-state true)

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

test-capability
~~~~~~~~~~~~~~~

*capability* ``-> bool`` *→* ``string``

Specify and request grant of CAPABILITY. Once granted, CAPABILITY and
any composed capabilities are in scope for the rest of the transaction.
Allows direct invocation of capabilities, which is not available in the
blockchain environment.

.. code:: lisp

   (test-capability (MY-CAP))

typecheck
~~~~~~~~~

*module* ``string`` *→* ``string``

*module* ``string`` *debug* ``bool`` *→* ``string``

Typecheck MODULE, optionally enabling DEBUG output.

verify
~~~~~~

*module* ``string`` *→* ``string``

Verify MODULE, checking that all properties hold.
