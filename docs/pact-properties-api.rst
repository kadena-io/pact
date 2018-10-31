.. _properties-and-invariants:

Property and Invariant Functions
================================

These are functions available in properties and invariants – not
necessarily in executable Pact code. All of these functions are
available in properties, but only a subset are available in invariants.
As a general rule, invariants have vocabulary for talking about the
shape of data, whereas properties also add vocabulary for talking about
function inputs and outputs, and database interactions. Each function
also explicitly says whether it’s available in just properties, or
invariants as well.

.. _Numerical:

Numerical operators
-------------------

.. _FAddition:

\+
~~

.. code:: lisp

   (+ x y)

-  takes ``x``: *a*
-  takes ``y``: *a*
-  produces *a*
-  where *a* is of type ``integer`` or ``decimal``

Addition of integers and decimals.

Supported in either invariants or properties.

.. _FSubtraction:

\-
~~

.. code:: lisp

   (- x y)

-  takes ``x``: *a*
-  takes ``y``: *a*
-  produces *a*
-  where *a* is of type ``integer`` or ``decimal``

Subtraction of integers and decimals.

Supported in either invariants or properties.

.. _FMultiplication:

\*
~~

.. code:: lisp

   (* x y)

-  takes ``x``: *a*
-  takes ``y``: *a*
-  produces *a*
-  where *a* is of type ``integer`` or ``decimal``

Multiplication of integers and decimals.

Supported in either invariants or properties.

.. _FDivision:

/
~

.. code:: lisp

   (/ x y)

-  takes ``x``: *a*
-  takes ``y``: *a*
-  produces *a*
-  where *a* is of type ``integer`` or ``decimal``

Division of integers and decimals.

Supported in either invariants or properties.

.. _FExponentiation:

^
~

.. code:: lisp

   (^ x y)

-  takes ``x``: *a*
-  takes ``y``: *a*
-  produces *a*
-  where *a* is of type ``integer`` or ``decimal``

Exponentiation of integers and decimals.

Supported in either invariants or properties.

.. _FLogarithm:

log
~~~

.. code:: lisp

   (log b x)

-  takes ``b``: *a*
-  takes ``x``: *a*
-  produces *a*
-  where *a* is of type ``integer`` or ``decimal``

Logarithm of ``x`` base ``b``.

Supported in either invariants or properties.

.. _FNumericNegation:

\-
~~

.. code:: lisp

   (- x)

-  takes ``x``: *a*
-  produces *a*
-  where *a* is of type ``integer`` or ``decimal``

Negation of integers and decimals.

Supported in either invariants or properties.

.. _FSquareRoot:

sqrt
~~~~

.. code:: lisp

   (sqrt x)

-  takes ``x``: *a*
-  produces *a*
-  where *a* is of type ``integer`` or ``decimal``

Square root of integers and decimals.

Supported in either invariants or properties.

.. _FNaturalLogarithm:

ln
~~

.. code:: lisp

   (ln x)

-  takes ``x``: *a*
-  produces *a*
-  where *a* is of type ``integer`` or ``decimal``

Logarithm of integers and decimals base e.

Supported in either invariants or properties.

.. _FExponential:

exp
~~~

.. code:: lisp

   (exp x)

-  takes ``x``: *a*
-  produces *a*
-  where *a* is of type ``integer`` or ``decimal``

Exponential of integers and decimals. e raised to the integer or decimal
``x``.

Supported in either invariants or properties.

.. _FAbsoluteValue:

abs
~~~

.. code:: lisp

   (abs x)

-  takes ``x``: *a*
-  produces *a*
-  where *a* is of type ``integer`` or ``decimal``

Absolute value of integers and decimals.

Supported in either invariants or properties.

.. _FBankersRound:

round
~~~~~

.. code:: lisp

   (round x)

-  takes ``x``: ``decimal``
-  produces ``integer``

.. code:: lisp

   (round x prec)

-  takes ``x``: ``decimal``
-  takes ``prec``: ``integer``
-  produces ``integer``

Banker’s rounding value of decimal ``x`` as integer, or to ``prec``
precision as decimal.

Supported in either invariants or properties.

.. _FCeilingRound:

ceiling
~~~~~~~

.. code:: lisp

   (ceiling x)

-  takes ``x``: ``decimal``
-  produces ``integer``

.. code:: lisp

   (ceiling x prec)

-  takes ``x``: ``decimal``
-  takes ``prec``: ``integer``
-  produces ``integer``

Rounds the decimal ``x`` up to the next integer, or to ``prec``
precision as decimal.

Supported in either invariants or properties.

.. _FFloorRound:

floor
~~~~~

.. code:: lisp

   (floor x)

-  takes ``x``: ``decimal``
-  produces ``integer``

.. code:: lisp

   (floor x prec)

-  takes ``x``: ``decimal``
-  takes ``prec``: ``integer``
-  produces ``integer``

Rounds the decimal ``x`` down to the previous integer, or to ``prec``
precision as decimal.

Supported in either invariants or properties.

.. _FModulus:

mod
~~~

.. code:: lisp

   (mod x y)

-  takes ``x``: ``integer``
-  takes ``y``: ``integer``
-  produces ``integer``

Integer modulus

Supported in either invariants or properties.

.. _Logical:

Logical operators
-----------------

.. _FGreaterThan:

>
~

.. code:: lisp

   (> x y)

-  takes ``x``: *a*
-  takes ``y``: *a*
-  produces ``bool``
-  where *a* is of type ``integer`` or ``decimal``

True if ``x`` > ``y``

Supported in either invariants or properties.

.. _FLessThan:

<
~

.. code:: lisp

   (< x y)

-  takes ``x``: *a*
-  takes ``y``: *a*
-  produces ``bool``
-  where *a* is of type ``integer`` or ``decimal``

True if ``x`` < ``y``

Supported in either invariants or properties.

.. _FGreaterThanOrEqual:

>=
~~

.. code:: lisp

   (>= x y)

-  takes ``x``: *a*
-  takes ``y``: *a*
-  produces ``bool``
-  where *a* is of type ``integer`` or ``decimal``

True if ``x`` >= ``y``

Supported in either invariants or properties.

.. _FLessThanOrEqual:

<=
~~

.. code:: lisp

   (<= x y)

-  takes ``x``: *a*
-  takes ``y``: *a*
-  produces ``bool``
-  where *a* is of type ``integer`` or ``decimal``

True if ``x`` <= ``y``

Supported in either invariants or properties.

.. _FEquality:

=
~

.. code:: lisp

   (= x y)

-  takes ``x``: *a*
-  takes ``y``: *a*
-  produces ``bool``
-  where *a* is of type ``integer``, ``decimal``, ``string``, ``time``,
   ``bool``, ``object``, or ``keyset``

True if ``x`` = ``y``

Supported in either invariants or properties.

.. _FInequality:

!=
~~

.. code:: lisp

   (!= x y)

-  takes ``x``: *a*
-  takes ``y``: *a*
-  produces ``bool``
-  where *a* is of type ``integer``, ``decimal``, ``string``, ``time``,
   ``bool``, ``object``, or ``keyset``

True if ``x`` != ``y``

Supported in either invariants or properties.

.. _FLogicalConjunction:

and
~~~

.. code:: lisp

   (and x y)

-  takes ``x``: ``bool``
-  takes ``y``: ``bool``
-  produces ``bool``

Short-circuiting logical conjunction

Supported in either invariants or properties.

.. _FLogicalDisjunction:

or
~~

.. code:: lisp

   (or x y)

-  takes ``x``: ``bool``
-  takes ``y``: ``bool``
-  produces ``bool``

Short-circuiting logical disjunction

Supported in either invariants or properties.

.. _FLogicalNegation:

not
~~~

.. code:: lisp

   (not x)

-  takes ``x``: ``bool``
-  produces ``bool``

Logical negation

Supported in either invariants or properties.

.. _FLogicalImplication:

when
~~~~

.. code:: lisp

   (when x y)

-  takes ``x``: ``bool``
-  takes ``y``: ``bool``
-  produces ``bool``

Logical implication. Equivalent to ``(or (not x) y)``.

Supported in either invariants or properties.

.. _Object:

Object operators
----------------

.. _FObjectProjection:

at
~~

.. code:: lisp

   (at k o)

-  takes ``k``: ``string``
-  takes ``o``: ``object``
-  produces ``bool``

Object projection

Supported in either invariants or properties.

.. _FObjectMerge:

\+
~~

.. code:: lisp

   (+ x y)

-  takes ``x``: ``object``
-  takes ``y``: ``object``
-  produces ``object``

Object merge

Supported in either invariants or properties.

.. _String:

String operators
----------------

.. _FStringLength:

length
~~~~~~

.. code:: lisp

   (length s)

-  takes ``s``: ``string``
-  produces ``integer``

String length

Supported in either invariants or properties.

.. _FStringConcatenation:

\+
~~

.. code:: lisp

   (+ s t)

-  takes ``s``: ``string``
-  takes ``t``: ``string``
-  produces ``string``

String concatenation

Supported in either invariants or properties.

.. _Temporal:

Temporal operators
------------------

.. _FTemporalAddition:

add-time
~~~~~~~~

.. code:: lisp

   (add-time t s)

-  takes ``t``: ``time``
-  takes ``s``: *a*
-  produces ``time``
-  where *a* is of type ``integer`` or ``decimal``

Add seconds to a time

Supported in either invariants or properties.

.. _Quantification:

Quantification operators
------------------------

.. _FUniversalQuantification:

forall
~~~~~~

.. code:: lisp

   (forall (x:string) y)

-  binds ``x``: *a*
-  takes ``y``: *r*
-  produces *r*
-  where *a* is *any type*
-  where *r* is *any type*

Bind a universally-quantified variable

Supported in properties only.

.. _FExistentialQuantification:

exists
~~~~~~

.. code:: lisp

   (exists (x:string) y)

-  binds ``x``: *a*
-  takes ``y``: *r*
-  produces *r*
-  where *a* is *any type*
-  where *r* is *any type*

Bind an existentially-quantified variable

Supported in properties only.

.. _Transactional:

Transactional operators
-----------------------

.. _FTransactionAborts:

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

.. _FTransactionSucceeds:

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

.. _FFunctionResult:

result
~~~~~~

.. code:: lisp

   result

-  of type *r*
-  where *r* is *any type*

The return value of the function under test

Supported in properties only.

.. _Database:

Database operators
------------------

.. _FTableWritten:

table-written
~~~~~~~~~~~~~

.. code:: lisp

   (table-written t)

-  takes ``t``: *a*
-  produces ``bool``
-  where *a* is of type ``table`` or ``string``

Whether a table is written in the function under analysis

Supported in properties only.

.. _FTableRead:

table-read
~~~~~~~~~~

.. code:: lisp

   (table-read t)

-  takes ``t``: *a*
-  produces ``bool``
-  where *a* is of type ``table`` or ``string``

Whether a table is read in the function under analysis

Supported in properties only.

.. _FCellDelta:

cell-delta
~~~~~~~~~~

.. code:: lisp

   (cell-delta t c r)

-  takes ``t``: *a*
-  takes ``c``: *b*
-  takes ``r``: ``string``
-  produces *c*
-  where *a* is of type ``table`` or ``string``
-  where *b* is of type ``column`` or ``string``
-  where *c* is of type ``integer`` or ``decimal``

The difference in a cell’s value before and after the transaction

Supported in properties only.

.. _FColumnDelta:

column-delta
~~~~~~~~~~~~

.. code:: lisp

   (column-delta t c)

-  takes ``t``: *a*
-  takes ``c``: *b*
-  produces *c*
-  where *a* is of type ``table`` or ``string``
-  where *b* is of type ``column`` or ``string``
-  where *c* is of type ``integer`` or ``decimal``

The difference in a column’s total summed value before and after the
transaction

Supported in properties only.

.. _FColumnWritten:

column-written
~~~~~~~~~~~~~~

.. code:: lisp

   (column-written t c)

-  takes ``t``: *a*
-  takes ``c``: *b*
-  produces ``bool``
-  where *a* is of type ``table`` or ``string``
-  where *b* is of type ``column`` or ``string``

Whether a column is written to in a transaction

Supported in properties only.

.. _FColumnRead:

column-read
~~~~~~~~~~~

.. code:: lisp

   (column-read t c)

-  takes ``t``: *a*
-  takes ``c``: *b*
-  produces ``bool``
-  where *a* is of type ``table`` or ``string``
-  where *b* is of type ``column`` or ``string``

Whether a column is read from in a transaction

Supported in properties only.

.. _FRowRead:

row-read
~~~~~~~~

.. code:: lisp

   (row-read t r)

-  takes ``t``: *a*
-  takes ``r``: ``string``
-  produces ``bool``
-  where *a* is of type ``table`` or ``string``

Whether a row is read in the function under analysis

Supported in properties only.

.. _FRowWritten:

row-written
~~~~~~~~~~~

.. code:: lisp

   (row-written t r)

-  takes ``t``: *a*
-  takes ``r``: ``string``
-  produces ``bool``
-  where *a* is of type ``table`` or ``string``

Whether a row is written in the function under analysis

Supported in properties only.

.. _FRowReadCount:

row-read-count
~~~~~~~~~~~~~~

.. code:: lisp

   (row-read-count t r)

-  takes ``t``: *a*
-  takes ``r``: ``string``
-  produces ``integer``
-  where *a* is of type ``table`` or ``string``

The number of times a row is read during a transaction

Supported in properties only.

.. _FRowWriteCount:

row-write-count
~~~~~~~~~~~~~~~

.. code:: lisp

   (row-write-count t r)

-  takes ``t``: *a*
-  takes ``r``: ``string``
-  produces ``integer``
-  where *a* is of type ``table`` or ``string``

The number of times a row is written during a transaction

Supported in properties only.

.. _FRowExists:

row-exists
~~~~~~~~~~

.. code:: lisp

   (row-exists t r time)

-  takes ``t``: *a*
-  takes ``r``: ``string``
-  takes ``time``: one of {“before”, “after”}
-  produces ``bool``
-  where *a* is of type ``table`` or ``string``

Whether a row exists before or after a transaction

Supported in properties only.

.. _FPropRead:

read
~~~~

.. code:: lisp

   (read t r)

-  takes ``t``: *a*
-  takes ``r``: ``string``
-  takes ``time``: one of {“before”, “after”}
-  produces ``object``
-  where *a* is of type ``table`` or ``string``

The value of a read before or after a transaction

Supported in properties only.

.. _Authorization:

Authorization operators
-----------------------

.. _FAuthorizedBy:

authorized-by
~~~~~~~~~~~~~

.. code:: lisp

   (authorized-by k)

-  takes ``k``: ``string``
-  produces ``bool``

Whether the named keyset is enforced by the function under analysis

Supported in properties only.

.. _FRowEnforced:

row-enforced
~~~~~~~~~~~~

.. code:: lisp

   (row-enforced t c r)

-  takes ``t``: *a*
-  takes ``c``: *b*
-  takes ``r``: ``string``
-  produces ``bool``
-  where *a* is of type ``table`` or ``string``
-  where *b* is of type ``column`` or ``string``

Whether the keyset in the row is enforced by the function under analysis

Supported in properties only.
