Pact "pacts" design doc. Pact version at time of writing is 2.2.3

Motivation: Confidentiality
===

For confidentiality, pacts provide the ability to sequence transactions such that they may
be modeled as a compound operation.

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
must only happen for a single participant at a key "owned" by them.

Clearly a "chinese wall" must divide public blockchain data from anything in private, most likely
resulting in private transactions having read-only access to public data. This concept could be
further extended as indicated above, with some tables only permitting "global" updates and others
only permitting "local" updates.

Transaction success consistency
---

If any disjoint execution is allowed, then clearly "success" or "failure" is only consistent
in the "global" model above. In the "local" model, the single actor succeeds or fails, while the
other actors are no-ops, and in fact have no knowledge whether the tx succeeded or not. The need
arises to initiate another transaction simply to record the local success or failure.

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
