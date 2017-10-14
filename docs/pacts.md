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
entropy source. Messaging thus always "self-sends" to ensure replication.

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
yield and resume to allow messaging of an output value to the next step. (It is TBD if reverse-messaging
is necessary for rollbacks).

Automation here serves to guarantee ack servicingkind of SLA on responsiveness. S

It is assumed that the machine handling the
automation has access to the cryptographic identity of the entity
Note that automation is not required here except as a kind of SLA on responsiveness.

Runtime model
---

How is the state across transactions maintained? Automation implies that certain elements of


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
capabilities. However this needs to be carefully distinguished from the capabilities of the current
step. It might be better to explicitly model this with a scope, ie `(import-capabities step action)`
with the idea that something like CANCEL authorization would have to examine only the current transaction's
capabilities, before importing previous caps to handle a rollback-like operation. Ideally, however,
solutions should not require the capability promotion.

Yield/Resume and application in public
---

Yield essentially provides for unforgeable messaging between steps.


Pact-local capabilities
---

The pact itself requires some kind of unforgeable capability. This can be seen as part of the
internal state of the pact that necessarily cannot be specified by messaging, along with program
counter.






So, in public, failure is a noop, meaning that steps can use normal keyset-based auth for RESUME.

A "cancel" can be seen as a control-flow governed by a capability, or a straight FAILURE message.
FAILURE handling must still be governed by a keyset anyway, so it's inappropriate for a CANCEL
as we don't want control-flow in FAILURE. Not really sure CANCEL should be promoted to syntax-level,
as it moves so much logic into magic. The control flow itself could be stylized however. The main
thing CANCEL needs for escrow is a system-time.





A CANCEL represents a valid path for logic, and as explicitly handled in control flow, becomes
easy to assure correctness via capability checking, timeouts etc.


from previous transactions can be executed
as part of failure handling, although this alters Pact's never-catch-errors model, and requires
that capabilities for the rollbacks have been provisioned in the previous step.
This raises issues with control flow, as an attacker can intentionally send in a bad tx to trigger a malicious rollback.

Scenario: escrow completes hopeful for success but an error occurs: retry obviously, or cancel. No automatic
update should ensue.

A CANCEL represents a valid path for logic, and as explicitly handled in control flow, becomes
easy to assure correctness via capability checking, timeouts etc. A failure must always result in a noop,
to respect transactionality.












Obviously in either case this
results in another "ack transaction", either to increment the counter once again for success, or
rewind for failure.


before "nacking" back to the previous step.

The question however is how cross-transaction state is to be represented. Pacts use the coroutine
model of "suspended application", with the runtime managing environmental data, a step counter and
success/failure status; steps inter-message data via yield/resume which is necessarily part of
the ack transaction, along with success/failure.


yield/resume and memoizing environmental data providing the stack-snapshotting
and counter storage.
providing maintaining a "call stack" and counter

The desire to automate a multi-tran The desire to
automate this response drives the design of pacts, to automate the message-handling.  Failure modes are really only
required once we go multi-step: rollbacks.

Multi-step execution
---
and orchestrate rollbacks correctly once we move into multi-step execution.

Without system-level guarantees of "global" consistency, any "global" write can fail in a disjoint fashion.


This is
the main driver for single-actor updates, so that other participants can be messaged with the outcome.
Most likely, this kind of ack is alway
Note that this is not necessary for a "one-way" tx
but only where two or more local, disjoint writes must occur; however once we've sequenced anything,
rollbacks become necessary, indicating that failure _must_ result in an additional outcome-signalling
transaction in order to initiate rollbacks. An open question is whether success-outcome signals are necessary in the final step.

Establishing the need to signal failure to initiate rollbacks suggests making this automatic.

Cancellation
---

Cancellation was not considered initially, only arising with the public oracle/escrow cases. However once
oracle functionality is provided, it opens the door to







Atomicity
---





Immediately we face disjointed datasets based strictly on the visibility regime. Indeed, at the
blockchain level, an encrypted message has no status or results as anything else leaks information.
However
this also implies that actors within an encrypted transaction will face disjointed datasets
amongst themselves. Attempts to "mirror" a counterparties' state might require leaking more
information than desired, and would involve constant "checkpointing" between the counterparty's
source dataset and the mirror copy. In the end, we face a model where opaque operations happen
on a counterparty's database that differ from what any actor might enact or see in their own
database.

Nonetheless, we maintain that _Transaction success vs failure must be consistent across participants_.
The only truly consistent record across the blockchain is simply to note that the message is ENCRYPTED,
as recording any other state leaks information.
The true record of any encrypted transaction on the blockchain is ENCRYPTED if we want to reflect
a
We've already screwed up the execution model
enough such that for all non-participants the result can only be "IGNORE"; for participants, we
posit that success vs failure


The question becomes how to deal with non-uniform operations under the semantics of ACID or similar:

1) Atomicity. In a simplified transfer, the debitor ensures sufficient funds, debits, and the creditor credits
the amount. Clearly the credit "happens after" the debit, as the sufficient-funds requirement is the only
hard rule. In a disjoint scheme, only the creditor will be able to perform the sufficient-funds check.
