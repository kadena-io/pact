Pact "Pacts" design doc. Pact version at time of writing is 2.2.3.

Introduction
===

The Pact smart contract language features "pacts", a form of _coroutine_ that allows for
orchestrating a sequence of transactions, as might be found in an escrow process.
In this document we discuss the two motivating scenarios under which pacts find their ideal
use-case: for private, confidential transactions it enables single-actor process serialization
with automatic rollbacks; for public, "trustless" two-party escrow with rules preventing early cancel
by the debtor.

Motivation: Sequenced Private Transactions
===

For private transactions, pacts provide the ability to sequence transactions such that they may
be modeled as a compound operation, designed to only allow a single "entity" operate at a time.
This comes from Kadena's "encrypted messaging" approach to smart contracts with entities representing
cryptographic identities (Bob, Alice) communicating with symmetric encryption in a DH scheme.

Entities
---

An entity is an identity as a keypair for symmetric DH communications. Entities are intended
to be highly-available, which means that a keypair must be replicated across local nodes, along with an
entropy source. Messaging thus always "self-sends" to ensure replication.

Entities are intended mainly for private use, but there is an application for encrypted messaging
on a public blockchain for execution in a separate, local runtime that has read-access to the global
blockchain.

Considerations regarding disjointedness
---

An encrypted transaction on a blockchain is ignored by all who cannot read the plaintext,
and will only impact the application state of those who can read it. Insofar as the blockchain
itself must have identical contents, this means that an encrypted message is a no-op, as any
record of results, even decryption success, leaks information and is necessarily inconsistent.
We see then that at the blockchain-application level, an encrypted message neither succeeds or
fails, which probably means that it always "succeeds" in performing the no-op.

Thus, once an encrypted message is decrypted and executed, its transaction log is completely
distinct from the blockchain log, although it can be linked to it by transaction ID. The log
of private transactions is thus _sparse_ relative to the blockchain log, and is
necessarily disjoint: one participant's private log records at different transaction IDs
than another's. Participants in a given transaction will record _something_ in their confidential
log at the same ID, representing a modicum of consistency at the transaction ID/participants level.

If the private transaction log is disjoint, clearly the application state DB is disjoint with
respect to these private applications. Thus the "private log" is consistent only in that "something
is recorded at the same TID", but _what_ is recorded is different. There is clearly an affinity
with database keys (here "key" indicates the table too), as sanity would dictate: if two entities write different values
to the same key at the same TID, logical consistency cannot be enforced. Instead we expect
that participants will either all write the same "global" value at a key (leading to consistency with
the private log), or only one of them write a "local" value at a key (narrowing consistency to a
single database). Formal verification of these interactions is attractive, implying also that
the former "global" case must be "stateless", ie argument-driven not db-driven, while DB-driven updates
must only happen for a single participant at a key "owned" by them. Without this guarantee, in fact it is
safer to not allow global cases, as these could unexpectedly fail disjointly.

Public/Private data hygeine
---

Clearly a "chinese wall" must divide public blockchain data from anything in private, most likely
resulting in private transactions having read-only access to public data. This concept could be
further extended as indicated above, with some tables only permitting "global" updates and others
only permitting "local" updates.

Public code can provide a higher level of trust for executing pacts, as the public store guarantees
all parties cannot run disjoint code.

Transaction success consistency
---

With single-actor disjoint execution, the active actor succeeds or fails, while the
inactive actors are no-ops, and in fact have no knowledge whether the tx succeeded or not. The need
arises to initiate another transaction simply to record the local success or failure. It can be
argued that a single-step private transaction does not need any ack; but any system worth its salt
will want it as it presumably represents the minimal "message received" so status can be given
to the other participants.

Thus we conclude that "ack transactions" are useful if not required for the most minimal private transaction with two or
more parties (one party being "self encryption", which succeeds or fails as a unit across replicating entity nodes, but
even here an ack transaction is harmless, however wasteful). Automation becomes attractive to correctly
service the ack transaction, and even "guarantee" acking (within some SLA).

Multi-step execution
---

Once in place, ack automation directly leads to the automation/orchestration of multi-step transactions.
This serves to address the awkwardness of single-actor disjoint execution, where all activity must be _serialized_ with
respect to keys owned by different participants. The ack transaction now kicks off the next step
upon success.

Failure automatically causes the runtime to "reverse direction" to serialize rollback execution in reverse
order; failure messages cascade back as "nacks" until the first step is reached.

Final success results in a true "ack transaction" which can report success to participants and provide
output. [NOTE: output for steps in Pact are currently ignored].

Multi-step leads directly to considerations of how to move information across the steps. Pacts offer
yield and resume to allow messaging of an output value to the next step. (It is TBD if "reverse
yielding" is necessary for rollbacks). Note however that this is forgeable, as it necessarily must be
messaged from the sole entity executing a step.

As will be seen, automatic rollback is private-only; currently it is hardcoded into pact syntax.


Motivation: Public Oracle and Escrow transactions
====

In public, we no longer need automated servicing of the pact flow: ack transactions become pure RESUME messages
sent from outside, as are CANCEL messages.

In public, automatic failure-initiated rollbacks are not valid, as it creates an attack vector of deliberately causing
failure to get around CANCEL restrictions. Failure must always be a no-op, leaving the pact state unchanged.

The idea of a specialized CANCEL execution mode is unnecessary, beyond providing stylistic support.
Since all failure is a no-op, a CANCEL is an intentional "success-oriented" operation, handled
by control flow in whatever step is active. Features like timeouts become trivial to implement
(assuming the system provides a time oracle) and normal keyset-based auth is useful.

Public requires the ability to retaining capabilities from previous steps. For example in the CANCEL
case, the second actor successfully cancels, but effecting the cancel requires some update to the
first actor's state, requiring that capability; a success case could see oracle data coming in that
the initiator wants to update their own tables.

An original idea was to simply cascade capabilities, but this is too blunt an instrument. Better
to explicitly scope the capability import, ie `(import-capabilities step action)`. This would
allow the CANCEL authorization scheme to enforce against the current signatures, before importing
capabilities to effect the cancel.

Conditional enforcement
---

A CANCEL with different rules for initiator (like a timeout) vs respondent (just keyset) requires
us to deviate from the "enforce failure kills the transaction" model Pact currently uses, and allow
the testing of a sequence of enforcement conditions. To do this, enforce needs to use a "pure"
execution model (like that used by keyset predicates) in order to "swallow" the failed enforces
on the way to finding the first one that succeeds. We call this `enforce-one`:

```
(enforce-one
  [(enforce-keyset 'responder-keyset)
   (and (enforce-timeout) (enforce-keyset 'initiator-keyset))])
```

Here the initiator has a timeout before a cancel, OR the respondent can cancel at any time.


Yield/Resume and application style
---

In Public, yield  provides for unforgeable messaging between steps for both public and private.
However, public has the additional need to inject more data in as part of a RESUME, in such a way
as to suggest normal function application.

A pact id as part of a RESUME basically identifies the message as belonging to the active pact.
A message can state step and other internal state but this can only be a form of over-validation; the
internal step counter determines what state the pact is in, so the request must match it.

Thus, attempts are problematic to resemble normal function application at each step. First, using a function name
that has been "blessed" to represent the current step will still have to check that blessing according
to the incremental counter, putting a weird requirement on application, or conversely requiring
functions to specify pact interactions "intrusively" (ie, `(defun next-step () (set-step 2) ...)`),
an inversion of control from the current design. The problem here
is that these functions will be available to run as stand-alone functions, too, but presumably insofar
as they need pact magic they will simply fail in direct application.

Without function names (ie just RESUME [pact-id] [step]), using arguments becomes impractical,
requiring a loosey-goosey notion of application
(assuming that the arguments should match the step function being executed). Instead, RESUME should
simply avail itself of the JSON payload only.

Pact-local and contract-local capabilities
---

Pact-local capabilities must be unforgeable, like yield/resume and the step counter.
Direct reification in the database is problematic, as these must not be reifiable from outside the system,
creating a datatype that cannot be exported. A pact capability becomes a transient (memory-only)
datatype, at which point an `(enforce-pact persisted-pactid)` is acceptable: the pact id is merely
a pointer to the internal data accessed in `enforce-pact`; knowing the id alone grants no capabilities.
The only issue here is if a single table wants to have a hybrid capability storage; but given the
move to also referencing keysets via pointer (to make rotation tractable) instead of directly, this
raises the simple notion of a CAP-TYPE and CAP-VALUE tuple. Likewise, contract-local capabilities
can simply `(enforce-module persisted-modulename)` where again the name alone does not grant capabilities.
The CAP-TYPE field could simply accept `(keyset|pact|module)`. Enum support would be nice for this.


Runtime Model
===

The unforgeable aspects of pact execution require an in-memory model (or at least distinct from the
user application db) of:

- step counter

- pact ID

- Public last-yielded values

- overall activity status (to prevent dupe execution etc: `(NEW|ACTIVE|DONE)`)

- Public previous step capability lookup

- Failure cascading and private yield values are messaging-borne and therefore forgeable.
  Entity signatures should be enforced to at least ensure the right entity sent the failure.
  Failure status should be tracked and validated.

- In Pact environment, entity name as a Maybe, which doubles as a "is-public".

Reconciling public and private
---

Currently, rollback and entity selection are in syntax. Public needs no
special meta-programming, which might indicate a private-oriented syntax, with
entity and rollback as special forms.

Public therefore could be as simple as:

```
(defpact do-public-pact (name date oracle-ks initiator-ks)
  (initiate-to-oracle oracle-ks name date)
  (respond-from-oracle date))
```

... with private needing the `step`, `step-with-rollback`. In the above, `respond-from-oracle` would handle canceling
with bespoke code.
However, CANCEL is common enough to merit usage that can match the syntax of `step-with-rollback`. Now we
avoid state-flag control flow, and can focus on authorizing and implementing the cancel as a single action.

It emerges that the _entity selector argument_ can sufficiently distinguish between private and public:

```
;; public step
(step (initiate-to-oracle oracle-ks name date))

;; public step with cancel
(step-with-rollback (initiate-to-oracle oracle-ks name date)
  (cancel-me initiator-ks oracle-ks name date))

;; private step with entity argument
(step 'me (do-stuff foo bar))

;; private step with rollback, with entity argument
(step-with-rollback 'me (do-stuff foo bar) (aw-shux baz))
```

The open question is to declare the whole pact public or private. Prefer to simply
validate via syntax that steps in a given pact cannot mix public and private
constructions.


Considerations of using private messaging over a public blockchain
---
This would require the chinese-wall to be implemented, which is hygeinic for private but clearly
essential for public: the execution environment must be entirely distinct, and even off-thread/pipelined as the
public ledger has already signed off on this payload as an ENCRYPTED no-op.

It's possible if not necessarily desirable that the system can infer which tables are private through code
analysis: any table that is touched under a pact must be private. Instead probably better to use a
`create-private-table` to indicate the use of a given table in a private context, such that public tables
will not be writable within a private pact execution.


Considerations of using public-style pacts in a private blockchain
---
Generally, this seems unnecessary as private trust contexts don't require magic assurance of e.g. an escrow flow.
However there seems to be no reason not to allow it. Private would need to add a system time oracle for timeouts,
or just blow up when attempting to get system time.
