.. properties-and-invariants:

Property and Invariant Functions
================================

.. Numerical:

Numerical operators
-------------------

.. FAddition:

\+
~~

.. code:: lisp

    (+ x y)

-  takes ``x`` of type *a*
-  takes ``y`` of type *a*
-  produces type *a*
-  where *a* is of type ``integer`` or ``decimal``

Addition of integers and decimals.

Supported in either invariants or properties.

.. FSubtraction:

\-
~~

.. code:: lisp

    (- x y)

-  takes ``x`` of type *a*
-  takes ``y`` of type *a*
-  produces type *a*
-  where *a* is of type ``integer`` or ``decimal``

Subtraction of integers and decimals.

Supported in either invariants or properties.

.. FMultiplication:

\*
~~

.. code:: lisp

    (* x y)

-  takes ``x`` of type *a*
-  takes ``y`` of type *a*
-  produces type *a*
-  where *a* is of type ``integer`` or ``decimal``

Multiplication of integers and decimals.

Supported in either invariants or properties.

.. FDivision:

/
~

.. code:: lisp

    (/ x y)

-  takes ``x`` of type *a*
-  takes ``y`` of type *a*
-  produces type *a*
-  where *a* is of type ``integer`` or ``decimal``

Division of integers and decimals.

Supported in either invariants or properties.

.. FExponentiation:

^
~

.. code:: lisp

    (^ x y)

-  takes ``x`` of type *a*
-  takes ``y`` of type *a*
-  produces type *a*
-  where *a* is of type ``integer`` or ``decimal``

Exponentiation of integers and decimals.

Supported in either invariants or properties.

.. FLogarithm:

log
~~~

.. code:: lisp

    (log b x)

-  takes ``b`` of type *a*
-  takes ``x`` of type *a*
-  produces type *a*
-  where *a* is of type ``integer`` or ``decimal``

Logarithm of ``x`` base ``b``.

Supported in either invariants or properties.

.. FNumericNegation:

\-
~~

.. code:: lisp

    (- x)

-  takes ``x`` of type *a*
-  produces type *a*
-  where *a* is of type ``integer`` or ``decimal``

Negation of integers and decimals.

Supported in either invariants or properties.

.. FSquareRoot:

sqrt
~~~~

.. code:: lisp

    (sqrt x)

-  takes ``x`` of type *a*
-  produces type *a*
-  where *a* is of type ``integer`` or ``decimal``

Square root of integers and decimals.

Supported in either invariants or properties.

.. FNaturalLogarithm:

ln
~~

.. code:: lisp

    (ln x)

-  takes ``x`` of type *a*
-  produces type *a*
-  where *a* is of type ``integer`` or ``decimal``

Logarithm of integers and decimals base e.

Supported in either invariants or properties.

.. FExponential:

exp
~~~

.. code:: lisp

    (exp x)

-  takes ``x`` of type *a*
-  produces type *a*
-  where *a* is of type ``integer`` or ``decimal``

Exponential of integers and decimals. e raised to the integer or decimal
``x``.

Supported in either invariants or properties.

.. FAbsoluteValue:

abs
~~~

.. code:: lisp

    (abs x)

-  takes ``x`` of type *a*
-  produces type *a*
-  where *a* is of type ``integer`` or ``decimal``

Absolute value of integers and decimals.

Supported in either invariants or properties.

.. FBankersRound:

round
~~~~~

.. code:: lisp

    (round x)

-  takes ``x`` of type ``decimal``
-  produces type ``integer``

.. code:: lisp

    (round x prec)

-  takes ``x`` of type ``decimal``
-  takes ``prec`` of type ``integer``
-  produces type ``integer``

Banker’s rounding value of decimal ``x`` as integer, or to ``prec``
precision as decimal.

Supported in either invariants or properties.

.. FCeilingRound:

ceiling
~~~~~~~

.. code:: lisp

    (ceiling x)

-  takes ``x`` of type ``decimal``
-  produces type ``integer``

.. code:: lisp

    (ceiling x prec)

-  takes ``x`` of type ``decimal``
-  takes ``prec`` of type ``integer``
-  produces type ``integer``

Rounds the decimal ``x`` up to the next integer, or to ``prec``
precision as decimal.

Supported in either invariants or properties.

.. FFloorRound:

floor
~~~~~

.. code:: lisp

    (floor x)

-  takes ``x`` of type ``decimal``
-  produces type ``integer``

.. code:: lisp

    (floor x prec)

-  takes ``x`` of type ``decimal``
-  takes ``prec`` of type ``integer``
-  produces type ``integer``

Rounds the decimal ``x`` down to the previous integer, or to ``prec``
precision as decimal.

Supported in either invariants or properties.

.. FModulus:

mod
~~~

.. code:: lisp

    (mod x y)

-  takes ``x`` of type ``integer``
-  takes ``y`` of type ``integer``
-  produces type ``integer``

Integer modulus

Supported in either invariants or properties.

.. Logical:

Logical operators
-----------------

.. FGreaterThan:

>
~

.. code:: lisp

    (> x y)

-  takes ``x`` of type *a*
-  takes ``y`` of type *a*
-  produces type ``bool``
-  where *a* is of type ``integer`` or ``decimal``

True if ``x`` > ``y``

Supported in either invariants or properties.

.. FLessThan:

<
~

.. code:: lisp

    (< x y)

-  takes ``x`` of type *a*
-  takes ``y`` of type *a*
-  produces type ``bool``
-  where *a* is of type ``integer`` or ``decimal``

True if ``x`` < ``y``

Supported in either invariants or properties.

.. FGreaterThanOrEqual:

>=
~~

.. code:: lisp

    (>= x y)

-  takes ``x`` of type *a*
-  takes ``y`` of type *a*
-  produces type ``bool``
-  where *a* is of type ``integer`` or ``decimal``

True if ``x`` >= ``y``

Supported in either invariants or properties.

.. FLessThanOrEqual:

<=
~~

.. code:: lisp

    (<= x y)

-  takes ``x`` of type *a*
-  takes ``y`` of type *a*
-  produces type ``bool``
-  where *a* is of type ``integer`` or ``decimal``

True if ``x`` <= ``y``

Supported in either invariants or properties.

.. FEquality:

=
~

.. code:: lisp

    (= x y)

-  takes ``x`` of type *a*
-  takes ``y`` of type *a*
-  produces type ``bool``
-  where *a* is of type ``integer``, ``decimal``, ``string``, ``time``,
   ``bool``, ``object``, or ``keyset``

True if ``x`` = ``y``

Supported in either invariants or properties.

.. FInequality:

!=
~~

.. code:: lisp

    (!= x y)

-  takes ``x`` of type *a*
-  takes ``y`` of type *a*
-  produces type ``bool``
-  where *a* is of type ``integer``, ``decimal``, ``string``, ``time``,
   ``bool``, ``object``, or ``keyset``

True if ``x`` != ``y``

Supported in either invariants or properties.

.. FLogicalConjunction:

and
~~~

.. code:: lisp

    (and x y)

-  takes ``x`` of type ``bool``
-  takes ``y`` of type ``bool``
-  produces type ``bool``

Short-circuiting logical conjunction

Supported in either invariants or properties.

.. FLogicalDisjunction:

or
~~

.. code:: lisp

    (or x y)

-  takes ``x`` of type ``bool``
-  takes ``y`` of type ``bool``
-  produces type ``bool``

Short-circuiting logical disjunction

Supported in either invariants or properties.

.. FLogicalNegation:

not
~~~

.. code:: lisp

    (not x)

-  takes ``x`` of type ``bool``
-  produces type ``bool``

Logical negation

Supported in either invariants or properties.

.. FLogicalImplication:

when
~~~~

.. code:: lisp

    (when x y)

-  takes ``x`` of type ``bool``
-  takes ``y`` of type ``bool``
-  produces type ``bool``

Logical implication. Equivalent to ``(or (not x) y)``.

Supported in either invariants or properties.

.. Object:

Object operators
----------------

.. FObjectProjection:

at
~~

.. code:: lisp

    (at k o)

-  takes ``k`` of type ``string``
-  takes ``o`` of type ``object``
-  produces type ``bool``

Object projection

Supported in either invariants or properties.

.. FObjectMerge:

\+
~~

.. code:: lisp

    (+ x y)

-  takes ``x`` of type ``object``
-  takes ``y`` of type ``object``
-  produces type ``object``

Object merge

Supported in either invariants or properties.

.. String:

String operators
----------------

.. FStringLength:

length
~~~~~~

.. code:: lisp

    (length s)

-  takes ``s`` of type ``string``
-  produces type ``integer``

String length

Supported in either invariants or properties.

.. FStringConcatenation:

\+
~~

.. code:: lisp

    (+ s t)

-  takes ``s`` of type ``string``
-  takes ``t`` of type ``string``
-  produces type ``string``

String concatenation

Supported in either invariants or properties.

.. Temporal:

Temporal operators
------------------

.. FTemporalAddition:

add-time
~~~~~~~~

.. code:: lisp

    (add-time t s)

-  takes ``t`` of type ``time``
-  takes ``s`` of type *a*
-  produces type ``time``
-  where *a* is of type ``integer`` or ``decimal``

Add seconds to a time

Supported in either invariants or properties.

.. Quantification:

Quantification operators
------------------------

.. FUniversalQuantification:

forall
~~~~~~

.. code:: lisp

    (forall (x:string) y)

-  binds ``x`` of type *a*
-  takes ``y`` of type *r*
-  produces type *r*
-  where *a* is *any type*
-  where *r* is *any type*

Bind a universally-quantified variable

Supported in properties only.

.. FExistentialQuantification:

exists
~~~~~~

.. code:: lisp

    (exists (x:string) y)

-  binds ``x`` of type *a*
-  takes ``y`` of type *r*
-  produces type *r*
-  where *a* is *any type*
-  where *r* is *any type*

Bind an existentially-quantified variable

Supported in properties only.

.. Transactional:

Transactional operators
-----------------------

.. FTransactionAborts:

abort
~~~~~

.. code:: lisp

    abort

-  of type ``bool``

Whether the transaction aborts. This function is only useful when
expressing propositions that do not assume transaction success.
Propositions defined via ``property`` implicitly assume transaction
success. We will be adding a new mode in which to use this feature in
the future – please let us know if you need this functionality.

Supported in properties only.

.. FTransactionSucceeds:

success
~~~~~~~

.. code:: lisp

    success

-  of type ``bool``

Whether the transaction succeeds. This function is only useful when
expressing propositions that do not assume transaction success.
Propositions defined via ``property`` implicitly assume transaction
success. We will be adding a new mode in which to use this feature in
the future – please let us know if you need this functionality.

Supported in properties only.

.. FFunctionResult:

result
~~~~~~

.. code:: lisp

    result

-  of type *r*
-  where *r* is *any type*

The return value of the function under test

Supported in properties only.

.. Database:

Database operators
------------------

.. FTableWritten:

table-written
~~~~~~~~~~~~~

.. code:: lisp

    (table-written t)

-  takes ``t`` of type *a*
-  produces type ``bool``
-  where *a* is of type ``table`` or ``string``

Whether a table is written in the function under analysis

Supported in properties only.

.. FTableRead:

table-read
~~~~~~~~~~

.. code:: lisp

    (table-read t)

-  takes ``t`` of type *a*
-  produces type ``bool``
-  where *a* is of type ``table`` or ``string``

Whether a table is read in the function under analysis

Supported in properties only.

.. FCellDelta:

cell-delta
~~~~~~~~~~

.. code:: lisp

    (cell-delta t c r)

-  takes ``t`` of type *a*
-  takes ``c`` of type *b*
-  takes ``r`` of type ``string``
-  produces type *c*
-  where *a* is of type ``table`` or ``string``
-  where *b* is of type ``column`` or ``string``
-  where *c* is of type ``integer`` or ``decimal``

The difference in a cell’s value before and after the transaction

Supported in properties only.

.. FColumnDelta:

column-delta
~~~~~~~~~~~~

.. code:: lisp

    (column-delta t c)

-  takes ``t`` of type *a*
-  takes ``c`` of type *b*
-  produces type *c*
-  where *a* is of type ``table`` or ``string``
-  where *b* is of type ``column`` or ``string``
-  where *c* is of type ``integer`` or ``decimal``

The difference in a column’s total summed value before and after the
transaction

Supported in properties only.

.. FRowRead:

row-read
~~~~~~~~

.. code:: lisp

    (row-read t r)

-  takes ``t`` of type *a*
-  takes ``r`` of type ``string``
-  produces type ``bool``
-  where *a* is of type ``table`` or ``string``

Whether a row is read in the function under analysis

Supported in properties only.

.. FRowWritten:

row-written
~~~~~~~~~~~

.. code:: lisp

    (row-written t r)

-  takes ``t`` of type *a*
-  takes ``r`` of type ``string``
-  produces type ``bool``
-  where *a* is of type ``table`` or ``string``

Whether a row is written in the function under analysis

Supported in properties only.

.. FRowReadCount:

row-read-count
~~~~~~~~~~~~~~

.. code:: lisp

    (row-read-count t r)

-  takes ``t`` of type *a*
-  takes ``r`` of type ``string``
-  produces type ``integer``
-  where *a* is of type ``table`` or ``string``

The number of times a row is read during a transaction

Supported in properties only.

.. FRowWriteCount:

row-write-count
~~~~~~~~~~~~~~~

.. code:: lisp

    (row-write-count t r)

-  takes ``t`` of type *a*
-  takes ``r`` of type ``string``
-  produces type ``integer``
-  where *a* is of type ``table`` or ``string``

The number of times a row is written during a transaction

Supported in properties only.

.. Authorization:

Authorization operators
-----------------------

.. FAuthorizedBy:

authorized-by
~~~~~~~~~~~~~

.. code:: lisp

    (authorized-by k)

-  takes ``k`` of type ``string``
-  produces type ``bool``

Whether the named keyset is enforced by the function under analysis

Supported in properties only.

.. FRowEnforced:

row-enforced
~~~~~~~~~~~~

.. code:: lisp

    (row-enforced t c r)

-  takes ``t`` of type *a*
-  takes ``c`` of type *b*
-  takes ``r`` of type ``string``
-  produces type ``bool``
-  where *a* is of type ``table`` or ``string``
-  where *b* is of type ``column`` or ``string``

Whether the keyset in the row is enforced by the function under analysis

Supported in properties only.
