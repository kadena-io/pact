Pact "pacts" design doc. Pact version at time of writing is 2.2.3

Motivation: Private Transactions
===

For private transactions, pacts provide the ability to sequence transactions such that they may
be modeled as a compound operation, designed to only allow a single "entity" operate at a time.
This comes from Kadena's "encrypted messaging" approach to smart contracts with entities representing
cryptographic actors (Bob, Alice).

Entities
---

An entity is currently an identity as a keypair for symmetric DH communications. Entities are intended
to be highly-available, which means that a keypair must be replicated across local nodes, along with an
entropy source. Messaging thus always "self-sends" to ensure replication. Note that for public entities must
be made optional, as an effective indicator of public execution.

Entities are currently hard-coded into `defpact` syntax.

Considerations regarding disjointedness
---

An encrypted transaction on a blockchain is ignored by all who cannot read the plaintext,
and will only impact the application state of those who can read it. Insofar as the blockchain
itself must have identical contents, this means that an encrypted message is a no-op, as any
record of results, even decryption success, leaks information and is necessarily inconsistent.
We see then that at the blockchain-application level, an encrypted message neither succeeds or
fails, which probably means that it always "succeeds" in the no-op.

Thus, once an encrypted message is decrypted and executed, its transaction log is completely
distinct from the blockchain log, although it must link to it by transaction ID. The log
of private transactions is thus "sparse" relative to the blockchain log, and is
necessarily disjoint: one participant's private log records at different transaction IDs
than another's. Per transaction however, all participants will record _something_ in the confidential
log at the same ID, representing a modicum of consistency at the transaction ID level.

If the private transaction log is disjoint, clearly the application state DB is disjoint with
respect to these private applications. Thus the "private log" is consistent only in that "something
is recorded at the same TID", but _what_ is recorded is different. There is clearly an affinity
with database keys (here "key" indicates the table too), as sanity would dictate: if two participants write different values
to the same key at the same TID, logical consistency cannot be enforced. Instead we expect
that participants will either all write the same "global" value at a key (leading to consistency with
the private log), or only one of them write a "local" value at a key (narrowing consistency to a
single database). Formal verification of these interactions is attractive, implying also that
the former "global" case must be "stateless", ie argument-driven not db-driven, while DB-driven updates
must only happen for a single participant at a key "owned" by them. Without this in fact it is
safer to not allow global cases, as these could unexpectedly fail disjointly.

Public/Private data hygeine
---

Clearly a "chinese wall" must divide public blockchain data from anything in private, most likely
resulting in private transactions having read-only access to public data. This concept could be
further extended as indicated above, with some tables only permitting "global" updates and others
only permitting "local" updates.

This provides for a higher level of trust for pacts. If the pact code is public, this guarantees
all parties cannot execute disjoint code.

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
upon success. Failure causes the runtime to "reverse direction" to serialize rollback execution in reverse
order; failure messages cascade back as "nacks" until the first step is reached.
Final success results in a true "ack transaction" which can report success to participants and provide
output. [NOTE: output for steps in Pact are currently ignored].

Multi-step leads directly to considerations of how to move information across the steps. Pacts offer
yield and resume to allow unforgeable messaging of an output value to the next step. (It is TBD if "reverse
yielding" is necessary for rollbacks).

As will be seen, rollback is private-only; currently it is hardcoded into pact syntax.


Motivation: Public Oracle and Escrow transactions
====

In public, we no longer need automated messaging. Ack transactions become pure RESUME messages
sent from outside.

Public rollbacks are not valid. The model of executing
specified rollback code from previous steps as some form of automatic failure handling
raises the attack vector of deliberately causing failure, undermining the transactional model.
Failure must always be a no-op.

The idea of a specialized CANCEL execution mode is unnecessary, beyond providing stylistic support.
Since all failure is a no-op, a CANCEL is an intentional "success-oriented" operation, handled
by control flow in whatever step is active. Features like timeouts become trivial to implement
(assuming the system provides a time oracle) and normal keyset-based auth is useful.

Public does require the idea of retaining capabilities from previous steps, for example in the CANCEL
case if the second actor successfully cancels which requires some update needing the first actor's
capabilities, or more positively, in the case of wanting to consume an oracle response by writing
to the initiator's tables.
However this needs to be carefully distinguished from the capabilities of the current
step. It might be better to explicitly model this with a scope, ie `(import-capabilities step action)`
with the idea that something like CANCEL authorization would have to examine only the current transaction's
capabilities, before importing previous capabilities.

Conditional enforcement
---

The only special thing about CANCEL is the idea of enforcing a keyset as an `OR` operation. Currently
keysets are all-or-nothing, failing the transaction. I suppose this can be programmed as

```
(enforce-one
  [(enforce-keyset 'responder-keyset)
   (and (enforce-timeout) (enforce-keyset 'initiator-keyset))])
```

... meaning we at least need something like the `enforce-one` form here. What sucks about this
is the loss of executional simplicity -- what if the database was written to? Suggests that the
yet-to-be-implemented pure-keyset-enforce-mode could be extended to enforcements in general: as long
as they're pure, you can fail fail fail succeed with no drawbacks. Otherwise some kind of freaky
keyset-based DSL must come into play, to avoid spaghetti of
`(enforce (and (check-keyset k) (check-timeout)) "did timeout or keyset fail??")`.


Yield/Resume and application style
---

Yield essentially provides for unforgeable messaging between steps for both public and private.
However, public has the additional need to inject more data in as part of a RESUME, in such a way
as to suggest normal function application.

A pact id as part of a RESUME basically authenticates the message as belonging to the active pact.
A message can state step and other internal state but this can only be a form of verification; the
internal step counter determines what state the pact is in.

Thus, attempts to resemble normal function application are problematic. First, using a function name
that has been "blessed" to represent the current step will still have to check that blessing according
to the incremental counter mode, putting a weird requirement on application, or conversely requiring
functions to specify pact interactions "intrusively", an inversion of control. The problem here
is that these functions will be available to run as stand-alone functions, too, but presumably insofar
as they need pact magic they will simply fail in direct application.

Without function names, using arguments becomes freaky too, requiring a loosey-goosey notion of application
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

- last-yielded values

- overall activity status (to prevent dupe execution etc: `(NEW|ACTIVE|DONE)`)

- Previous-step-capability access requires retreival of previously-validated signature public keys by step

- Failure cascading is messaging-initiated and therefore forgeable. Entity signatures could be enforced to
  at least ensure the right entity sent the failure. Some internal state must be maintained, ie a `inFailure`
  flag, which is ignored in public application. Unsure if yield should function in reverse.

- entity name as a Maybe, which doubles as a "is-private"

Reconciling public and private
---

Rollback and entity selection are unfortunately in syntax which will have to go. Public needs no
special meta-programming or other magic, which might indicate a private-oriented syntax, with
entity and rollback as special forms.

Public therefore could be as simple as:

```
(defpact do-public-pact (name date oracle-ks initiator-ks)
  (initiate-to-oracle oracle-ks name date)
  (respond-from-oracle date))
```

... with private needing the `step`, `step-with-rollback`. In the above, `respond-from-oracle` would handle canceling
with bespoke code. Alternately, `step` could accept multiple syntaxes, and we could add `step-with-cancel` to stylize
handling a cancel. Probably the most straightforward is to have distinct syntax for each:

```
;; public step
(step (initiate-to-oracle oracle-ks name date))

;; public step with cancel, entities not allowed
(step-with-cancel (initiate-to-oracle oracle-ks name date)
  (cancel-me initiator-ks oracle-ks name date))

;; private step
(step 'me (do-stuff foo bar))

;; private step with rollback, entity required
(step-with-rollback 'me (do-stuff foo bar) (aw-shux baz))
```

Private automated acks vs. public RESUME
---
