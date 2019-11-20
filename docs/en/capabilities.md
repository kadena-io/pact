
# Introduction: Securing Smart Contracts with Capabilities
The Pact programming language was designed for implementing smart contracts to run on a blockchain, an execution environment that is characterized by the following:

1. Transactional & atomic execution. There are no long-running or "daemon" processes on a blockchain. Failures result in atomic rollback.

2. Focus on database maintenance. The goal of a transaction is to enforce invariants on inputs, compute the neessary state change, and persist it to the database. It must do this in a deterministic way to guarantee perfect replication.

3. Focus on safety. While not all Pact contracts run in public, those that do will be subject to inspection and attack. Even in a non-public context, the value of a blockchain lies in a "perfectly maintained record" that cannot be tampered with. 

4. Public-key signature verification. This is the core authorization primitive, without requiring cryptographic expertise on the part of the developer, and ensuring that multi-signature methods are as easy as single-signature ones.

5. Multi-tenant environment. The presence of disparate smart contracts running on the same system implies a multiplicity of trust environments. A single transaction can interact with more than one smart contract.

## Pact Authority Contexts
Pact's core features seek to offer a simple and easy way to write code that can operate correctly in this environment. Here we focus on how Pact decomposes _authority_ into different contexts, so that a "smart contract" can be "owned" by the module that implements it, individual rights can be goverened by public-key signatures, and multi-step processes can be controlled by "pacts".

#### Modules
A Pact module can be seen as a "smart contract", and indeed many simple blockchain applications are well-expressed in a single module. Modules support _governance_ which allows code upgrade and direct access to internals. Otherwise, the internals of a module are inaccessible except through the operations that are declared and form the module API. 

The module is thus a core "autonomous" authority context, where the code of the module is uniquely empowered. It alone can directly perform CRUD operations on rows of tables declared therein, for instance. 

#### Keysets and transaction signatures
A Pact _keyset_ is a primitive datatype that contains one or more public keys, paired with a _predicate_ that is used to test the set of keys against the runtime environment. 

Pact transactions are signed with one or more signatures, and the runtime enforces that all signatures are valid before any user code is executed. Once executing, the signatures are represented as a set of public keys, against which a keyset may be enforced using the `enforce-keyset` built-in. 

Signatures thus represent a central "trustless" authority context, with keysets as the main mechanism for authorization.

#### Pacts
Insofar as a real-world operation might require execution of multiple transactions in sequence, Pact offers a simple sequencing primitive called a "pact" in which a developer can specify multiple "steps" that must execute in forward sequence. 

Pacts can run on a single chain, but also have support to resume on a different chain, assuming the proper platform support (in the case of Chainweb, this is powered by SPV proofs of the execution of the prior step).

As the runtime guarantees invariants about pacts, such as their stepwise execution, and their uniqueness, pacts constitute another autonomous authority context. For instance, in a two-step escrow operation, the pact execution itself (as identified by its "pact ID") would "own" the funds and be the sole determinant of who receives them in the second step.

#### Guards as a unified representation of authority contexts

The three contexts, module, keyset, and pact, can all be seen as authority spheres that can "unseal" data. In order to "seal" the data or operation, we provide a family of predicates called _guards_ that correspond to each sphere. As a family of polymorphic types, Pact allows these to interoperate with each other. For example, a single table could have a "guard" column guarding access to changing data in that row, such that:

1. A module guard implies that only the module code can initiate the operation to effect the change. The presence or absence of signatures is irrelevant. 
2. A pact guard implies that only the current execution of a given pact can effect a change. Two different executions of the same pact cannot access each other's resources.
3. A keyset guard implies that matching signatures must be present in order to effect the change.

A fourth case is a "user guard", which simply captures a function call with its arguments as a closure at "sealing" time. This allows a developer to code arbitrary logic in the function to test the environment or database for proper state to "unseal" the operation.

#### Protecting code by "checking before"
The purpose of the authority contexts and guard system is to allow developers to enforce invariants to prevent unauthorized execution of code. In its simplest form, this is achieved by simply testing the invariant before executing the sensitive code.

As Pact is both Turing-incomplete (ie always terminating, and preventing recursion), and does not allow for interception of failures, this pattern of "checking-before" is more secure than it would be in a Turing-complete or exception-catching environment. As code always "moves forward" in Pact, "checking before" is a surprisingly effective way to protect code.

## Providing Abstraction of "Rights"
The aforementioned techniques comprise the "classic" Pact offering for securing code, but the need for a more comprehensive solution is evidenced by the following unmet needs:

1. Declare and express "rights" in code. A guard is a predicate only, and cannot express _for what purpose_ the guard is enforced. For instance, a keyset on a row might be tested to allow modifying a balance quantity, but it might elsewhere be tested to allow rotation of the key itself. Instead of a repeated check with a code comment, it is better to abstract the right as a distinct code element, to improve readability and maintenance. 
2. Check if a right has already been granted. The "checking before" mechanism creates an _implicit_ context that is impossible to query, forcing code to re-perform the low level check "to be sure". This happens for instance in delegate functions that are called in multiple contexts, where some might have already enforced the relevant guard: without an abstraction, the utility function must re-perform the check.
3. Avoid expensive duplicate checks. As an extension of #2, being able to test for a right being granted avoids potentially expensive duplicate checks, e.g. that might involve reading a guard from the database.
4. Declare and enforce an explicit _scope_ during which the right is granted. Especially in multi-tenant use cases, it is beneficial to evacuate the right from the execution context once the sensitive operation is complete.
5. Safely initiate stateful interactions with the right. In multi-tenant use cases it is often necessary to impose additional restrictions on the granting of the right, for instance that it can only be granted 1 time, or up to some limit. 
6. Constrain signatures to only apply to specified rights. While multi-signature is a great feature of Pact, it also means that signatures are not tied to a particular right. In multi-tenant contexts this creates a privilege leak: while the user may have provided a signature with some other intention, calling third-party code might try to exploit the presence of that signature (note there is no way in Pact to test for the presence of a given signature, so this would be difficult -- but not impossible).
7. Prevent rights from escaping their intended context. A "right" once granted is a dangerous thing that must not escape its confinement. Foreign modules must not be able to acquire rights directly, and rights must not be able to be stored in the database. Rights are in play only in the execution of the transaction that requested them, as managed by the module that declares them.  

### Modeling Rights as Capabilities
To address the above needs, Pact takes an approach inspired by the literature on _capabilities_. 

In its simplest form, a capability is simply some concrete token in a system whose presence identifies the grant of a specific authority. The classic presentation contrasts a UNIX-style system where "access control lists" (ACLs) list users who are allowed various rights, to a capability system where said right is explictly granted to a user in the form of a concrete reference or object. With ACLs, the environment is tested on demand to grant a relatively opaque right for an indeterminate scope, whereas a capability system actively manages this grant via the concrete object, actively designates who can be granted it, can withdraw or attenuate it at any time, and can even delegate a subset of the granting authority.

From here however the literature varies widely, to the point that capabilities do not comprise a unified discpline or practice but instead a body of research into both applications and operating systems. The elements that inform Pact's design directly are:

#### "No Ambient Authority"
One of the evils capabilities seek to prevent is "ambient authority", where some right is not sufficiently contained or scoped. For instance, allowing a user application to write to the filesystem simply because the user running it is in the appropriate ACL represents the "ambient" presence of that right, without any meaningful restrictions on when, how or why it can write. A capability system would instead provide that the application can be granted a specific right, say to make a particular change to a particular file, which could be revoked once the operation is complete.


#### "No Authority Without Designation"
Similarly to the ambient authority prohibition, an authority must be _designated_ to the particular identity or process that demands it. Thus, in the example above, the token representing the application's ability to write to a particular file in a particular way is unique to that application or process; if the same right was given to another application, that would be a distinct token with its own scope etc.

#### Unforgeability
The tokens representing a capability must be impossible to create outside of the capability governance system. In the file example, the token would not simply be the file path string, as anybody can put that together with knowledge of the file system, but some opaque object that the recipient system cannot create itself, which might resemble a "file handle" but with additional properties.

#### Blockchain and Functional Benefits
We note that the patterns of execution found in blockchains already provide some of these benefits. For instance, it is common to attach a public key to a quantity, so right away we have a concrete representation of a designated authority to spend the funds. Likewise, the traditions of functional programming surrounding immutability and functional purity prevent a fair amount of object-oriented shenanigans that object capabilities seek to address with clever designs. 






Capabilities in Pact represent a fresh approach to secure computing that is informed by research
into capability systems, such as _object capabilities_ by Miller et al, and as seen in recent efforts by Google with the Fuscia operating system, or BSD capabilities.

However, Pact has had to forge its own path for a number of reasons:

### Smart contracts are not general computation

Pact can be seen as a language whose whole job is to do a minimal amount of compute between the submission of a user-signed transaction and the persistence of a state change on an underlying database. There are no long-lived processes in Pact.  

### Not object-oriented

Unlike Solidity and other object-oriented smart contract languages, Pact is very deliberately _not_ object-oriented. Instead, Pact is functional, immutable, total, Turing-incomplete, strongly-typed, and boasts a "module-oriented" design. Code and data are distinct. 

For this reason, many notions of capabilities as e.g. _stateful_ objects (i.e. that can be "revoked" or attenuated behind the scenes, without changing the reference in question) does not apply to Pact.

### Focus on inter-transactional behavior

Pact is hyper-focused on the tiny slices of compute that happen "on the way to the database", and the capabilities model reflects this. Simply put, a capability cannot be stored on disk, as it instantly becomes a liability outside of a managed system. Nonetheless, the context of smart contracts processing signed transactions on a blockchain creates a number of challenging security problems all by itself.

### Stylized Runtime

The Pact runtime is heavily involved in Pact execution, so that e.g. keyset verification cannot leak keys into code, invalid signatures are never admitted, and the user is reduced to set membership as the safe primitive. With respect to capabilities, especially as they are subject to dynamic scoping over the course of a transaction, this means that a capability "object" is never directly manipulated by the code but is instead "installed" or "acquired" in the environment, with tight, automatic scoping.


### Lack of interest in capability theory in Pact's background

One challenge of applying capability theory is its unfamiliarity in the influential domains on Pact, namely Haskell and Bitcoin script. 

The latter, Bitcoin script, is an immutable predicate bytecode which has no ability to support an abstraction like capabilities. Its stylized execution model, where a script is directly attached to the sole unique value it can authorize, eliminates most of the security concerns a capability model would seek to address.

The former, Haskell, is the language that Pact is authored in. Here, ubiquitous immutability and referential transparency again rule out a number of shenanigans that might bedevil a C or Java codebase. Haskell favors _lawful_ functional programming which simply ejects the programmer from a whole host of bugs.


### Final Apologia
Pact's presentation of capabilities invite the accusation that they are better named something else that more closely resembles recent literature on capabilities. However, the impulse is the same, which is to "tame" stateful code that is responsible for granting sensitive rights, with reification as the main power tool: a "right" implemented as an ambient imperative implied state is hopelessly dangerous, whereas that same right, represented as a piece of data that has a well-defined scope both in time as well as in reachability, has already gained so many of the advertised benefits of a capability system that the remaining implementation details are arguably irrelevant.



# Definitions

## Capability

A capability *c* is specified by its domain _d_ and parameters _P_, which are sufficient to identify the specific invariant that governs access to some authority. 

The parameters can be decomposed into "identifying" parameters _P<sub>I</sub>_ and a "managed" parameter _q_. The domain plus identifying parameters compose a unique identity in the runtime system. 

The managed parameter is variable for that identity, and is mutated and validated by a manager function _m_ presumably to some linear constraint.

The "unsealing" of a capability is validated by the predicate function _f_, which operates on _P_ in order to authorize the capability, testing the environment and state and failing atomically. 

Finally, a capability can import or compose other capabilities _C<sub>+</sub>_.

<div align="center">
<b>c :: d = (P<sub>I</sub>, q, f, m, C<sub>+</sub>)</b>
</div>

### Declaration
A capability is declared with the `defcap` form, such that

```
(defcap PAY (sender receiver amount) 
  @managed pay-mgr amount
  (enforce-keyset 'sender)
  (compose-capability (DEBIT sender))
)
```

defines the capability

<div align="center">
<b>c :: d:PAY = (P<sub>I</sub>:[sender, receiver], q:amount, f:PAY lambda, m:pay-mgr, C<sub>+</sub>:{(DEBIT sender)})</b>
</div>

### Lexical representation

The PAY capability is represented, for sender "alice", receiver "bob", and amount 10.0, as

```
(PAY "alice" "bob" 10.0)
```

Note that this does _not_ indicate function application, but instead "borrows" application syntax to parameterize the `PAY` domain for the specific capability.

## Acquisition

_Acquisition_ is the process by which a capability is granted and kept in scope; once scope is left, the capability is no longer granted or acquired. "Acquired" status is represented implicitly in the runtime and has no user data representation. The two primitives of acquisition are:

### `with-capability`
The `with-capability c body` special form acquires _c_ and keeps it in scope during execution of _body_. If the capability has already been acquired in some outer scope, no action occurs. 



### "Standard" capability
Definition of standard capability *sc* of domain *d*, which is the name of the `defcap` whose parameters are *P* and defines the predicate/"guard" function *g*, which can compose/"import" other capabilities *C<sub>I</sub>*.

<div align="center">
<b>sc :: d = (P, g, C<sub>I</sub>)</b>
</div>

### "Managed" capability
For _managed_ capability *mc*, it has all the features of *c*, plus it can decompose *P* into _free_ or "identifying" variables that further inform its type, and _bound_ or "quantifying" variables that are computed in the manager function *m*:

<div align="center">
<b>mc :: (d, P<sub>F</sub>) = (P<sub>B</sub>, g, m, C<sub>I</sub>)</b>
</div>

### Module admin capability

"Module admin" describes a privilege escalation that is specified by the module declaration itself, either as **keyset-based** or **governance-based**, the latter requiring specification of a `defcap` to guard module upgrade.

Module admin capability _mm_ has a special type _a_, is parameterized by the module name _n_, cannot be managed, allows composition, and specifies either guard function g or keyset k.

<div align="center">
<b>mm :: a = (n, Either g k, C<sub>I</sub>)</b>
</div>

### Unified capability definition

In practice, managed capabilities are uniform with standard capabilities with only the presence of *m* to differentiate them, with the operational suggestion that *m* should _match_ on
*P<sub>F</sub>* and _compute_ on *P<sub>B</sub>*. Module admin remains its own subtype.

<div align="center">
<b>c :: d = (P, g, Maybe m, C<sub>I</sub>)</b><br>
<b>c :: a = (n, g, C<sub>I</sub>)</b>
</div><p>

If _m_ is populated in _c d_, the capability is considered "managed".

### `defcap` as constructor and predicate

The `defcap` special form plays two roles: it resembles a standard `defun` with a name, arguments and a body, which serves to implement the guard function *g*, but it also specifies a "capability reference" that uniquely identifies some inhabitant of *d* parameterized by *P*.

When a `defcap` is used for module governance, it specifies *g* only, as the module name _n_ is inferred.

#### Capability reference constructor

The `defcap` construction operates as a constructor of a capability reference *c\* d*:

<div align="center">
<b>P -> Maybe m -> c* d</b>
</div>

#### Definition of *g*

`defcap` also defines the lambda function *g* operating on environment E. In addition to validating whatever is directly required to acquire the capability, it can "compose" or import other capabilities *C<sub>I</sub>*. Lastly it is _pure_ in that it cannot update database state.

<div align="center">
<b>g :: P -> E -> Either Error C<sub>I</sub></b>
</div><p>

As such, *g* has the same shape as Pact's operation `enforce-guard :: guard -> E -> Either Error ()`. We remark that *g* is therefore "both an enforcement and definintion of a guard".

### Capability slots

Capabilities are collected in _slots_, which are _scoped_ to indicate their operational semantics. The slot *s* stores the reference *c\* d*, scope *o*, and any composed capability slots *S<sub>I</sub>*.

<div align="center">
<b>s = (o &isin; {Managed|Stack|Composed}, c* d, S<sub>I</sub>)</b>
</div>

#### _Managed_ scope

Managed capabilities are said to be "installed" as they can only be constructed and evaluated once. They are mutated by *m* when code requests a capability of matching domain *d*, described later.

#### _Stack_ scope

Stack capabilities are said to be "acquired" by user code, and are scoped/managed by the call-stack at acquisition time.

#### _Composed_ scope

A composed capability inherits the scope of the composing/parent capability and its slot inhabits parent _C<sub>I</sub>_.

### Manager function _m_

For a given domain _d_, the manager function takes the parameters of a managed capability *P<sub>M</sub>* and those of a requested stack capability *P<sub>S</sub>*, which on success returns updated params for the managed capability *P'<sub>M</sub>*. The function is a pure function that is not intended to consider the environment, although this is not forbidden operationally.

<div align="center">
<b>m :: P<sub>M</sub> -> P<sub>S</sub> -> Either Error P'<sub>M</sub></b>
</div><p>

NB the ideal presentation is _P<sub>M</sub><sup>B</sup> -> P<sub>S</sub><sup>B</sup> -> Either Error P'<sub>M</sub><sup>B</sup>_, where match has occurred on the free parameters.


### Signer/witness capability map

Signers/witnesses are associated with capability references as a map _W_ from the witness _w_ to the associated capability references _C*<sub>w</sub>_.

<div align="center">
<b>W = w -> C*<sub>w</sub>
</div>


### Runtime state *R*

The runtime *R* stores two sets of capability slots, Managed (_S<sub>M</sub>_) and Acquired (_S<sub>A</sub>_),  the map of signer witnesses _W_, and a collection to track "seen" managed capabilities _C*<sub>M</sub>.

<div align="center">
<b>R = (S<sub>M</sub>, S<sub>A</sub>, W, C*<sub>M</sub>)</b>
</div>

### Acquired Capability Stack _S<sub>A</sub>_

_S<sub>A</sub>_ operates as a stack in keeping with the callstack-scope of `with-capability`.

Operationally, it also accumulates _pending_ capabilities as a stack, to properly handle nested composition, and secure in Pact's terminating design that a failure means the stack being slightly inaccurate during capability acquisition is of no consequence.

Post-validation, the stack is pruned as needed:

- composed are popped and installed into parent _S<sub>I</sub>_;
- managed are popped and inserted into _S<sub>M</sub>_;
- acquired are left on the stack. After body is executed, the stack is blindly popped.

# Operational semantics

## Installation of managed capabilities: `install-capability`

Managed capabilities are said to be _installed_ by one of two mechanisms: `install-capability` or automatically when specified in *W*. Only managed capabilities can be installed, so non-managed capabilities in _W_ are ignored.

### Construct reference *c\*<sub>M</sub> d*.

Reference *c\*<sub>M</sub> d* is either specified in the `install-capability` call or already constructed in *W*.

### Test for already installed -> no action

Managed capabilities are tracked in runtime _C*<sub>M</sub>_ with their initially-installed parameterizations to prevent duplicate installs. If this is found, no action is taken.


### Not installed -> track, run *g*.

*c\*<sub>M</sub> d* is added to _C*<sub>M</sub>_ for dupe install tracking.

Newly-installed references execute *g* to validate install. Composed capabilities are added to S<sub>I</sub> as normal, but are not "manageable" themselves.

### Successful install.
A new slot containing *c\*<sub>M</sub> d* is added to _S<sub>M</sub>_.

### Lifecycle of Managed capability
Managed capability stays in _S<sub>M</sub>_ for the entire transaction, although it mutates per the manager function. The tracked reference in _C*<sub>M</sub>_ is never mutated.

## Acquisition of stack-scoped capabilities: `with-capability`

The `with-capability` special form attempts to acquire a stack-scoped or _acquired_ capability, which upon success executes
a contained body form.

### Construct reference.

The first argument of `with-capability` applies the `defcap` as a constructor to create reference *c<sub>A</sub>\* d*.

### Test for already acquired -> no action

If a slot with *c\*<sub>A</sub> d* is already in _S<sub>A</sub>_, control returns to the body form with no action.

### Not acquired -> test managed

_S<sub>M</sub>_ is scanned for any managed slots for references of the same domain *d*. For each, the associated manager function *m* is executed. The first to succeed wins. Order of execution is the natural order of the capability references.

NB: Ideally, it should be tested for an exact match on *(d, P<sub>F</sub>)* as only one match should be possible, and that one *m* function executed.

### Execution of manager function *m*

For the installed capability *c<sub>M</sub>\* d*, and the requested stack capability to acquire *c<sub>A</sub>\* d*, the manager function is invoked, which upon success produces *c'<sub>M</sub>\* d*.

### Success: update of slot *c<sub>M</sub>\* d*

The slot for *c<sub>M</sub>\* d* is replaced with the *c'<sub>M</sub>\* d* result from the manager function. Composed capabilities (_S<sub>I</sub>_) in *c<sub>M</sub>\* d* are "copied" into _S<sub>I</sub>_ in the requested capability slot.

### Failure: acquire fails

In a situation where a capability slot of matching domain *d* was found but none succeeded, the acquire operation fails.

### No match on managed: acquire fails

If *d* is managed but nothing was installed for *d*, acquire fails.

### No match on non-managed: run *g*

If *d* is not managed, *g* is executed to validate acquisition.

### Successful acquire.
If *m* or *g* succeeds, a new slot containing *c\*<sub>A</sub> d* is added to S<sub>A</sub>, and the body is executed.

### Lifecycle of Acquired capability
Once the body of `with-capability` terminates, the slot for *c\*<sub>A</sub> d* is popped off of _S<sub>A</sub>_ and terminates, along with any composed capabilities in _S<sub>I</sub>_.

## Import of composed capabilities: `compose-capability`

Import, via `compose-capability` can only occur in *g*, so there is always a "parent" capability slot ready to receive the capability in _C<sub>I</sub>_.

### Construct reference.

The first argument of `compose-capability` applies the `defcap` as a constructor to create reference *c<sub>I</sub>\* d*.

### Run identically as for an acquired capability

The procedure from here on out is identical to a stack/acquired capability, with the exception that on success no body form is executed, and failure causes the parent to fail as well.

### Successful compose
Install *c<sub>I</sub>\* d* parent _C<sub>I</sub>_.

### Lifecycle of composed capabilities.

Composed capabilities go out of scope/terminate with their parent capability.

## Testing capabilities: `require-capability`

`require-capability` looks for specified reference _c<sub>R</sub>* d_ in _S<sub>A</sub>_, including any composed capabilities. As such it does not modify _R_ so is safe to use anywhere. Also, it does _not_ check _S<sub>M</sub>_, which means that acquired capabilities are the only actual "enforcement" mechanism, as possibly managed by caps in _S<sub>M</sub>_.

## Signature/Witness capabilities

### Filtering keys from `enforce-keyset`
When `enforce-keyset` is invoked, matching signer witnesses are included if and only if a capability in _C*<sub>W</sub>_ is "in scope", which is anywhere in _S<sub>M</sub>_ or _S<sub>A</sub>_ including all composed capabilities, as well as any "pending" capabilities "coming into scope".

Note that since managed capabilities can only be installed once, those installed by dint of being in _W_ are only validated against specified witnesses in _W_ once. Thus, specifying a managed capability for _w_ ensures that _w_ will only be "used once".

## Module admin capability

A module admin capability  _m a_ is an **automatically acquired and managed** capability such that acquisition results in automatic install and the capability effectively remains in scope for the rest of the transaction.

### Automatic acquisition and management

Module admin is automatic in the following scenarios:

- Module upgrade.
- Direct access to module tables.
- Invocation of a module guard outside of module code.

### Reference construction, installation

Reference _c* a<sub>n</sub>_ is constructed. If a slot is already installed, acquisition is trivially successful (ie, a trivial _m_ function). If not, a new slot for _c* a<sub>n</sub>_ is constructed, and guard is tested.

### Guard execution

For keysets, this is simply enforcing the keyset. For governance function _g_, the function is evaluated.

On success, slot for _c* a<sub>n</sub>_ is installed (added to _S<sub>M</sub>_) **and** acquired (pushed onto _S<sub>A</sub>_). The capability reference is also added to "seen" collection _C*<sub>M</sub>_.

### Manual acquistion/require/reference not currently possible

Currently there is no way to manually acquire module admin. Whether the mechanism is keyset or cap-based governance, while it is possible to **replicate** governance in a separate capability, it is currently impossible to **directly reference** module admin in code.

TODO without this, there is no way to bind a signer to module admin, so a key with some other capability won't match this test. It is possible that the syntax for caps in _W_ should admit a special syntax for indicating _c* a<sub>n</sub>_, namely the bare module name. This could also suffice for other uses needing a module admin reference.

### Automatic management

Management of module admin simply allows the capability to come into scope anytime after first acquisition.

### Rationale for "sticky" acquisition

Given the extreme power of module admin, it appears alarming that its scope is so lenient. However the rationale is to give module administrators the most power to perform tasks like data migration and threat mitigation, namely by directly accessing database tables outside of module code. Also, module governance is potentially expensive to grant, as in decentralized cases involving tallying votes in the database, so not making this grant sticky can result in prohibitive performance or gas usage by re-acquiring the capability.

Any attempt to allow extra-module code to have a mechanism to acquire module admin manually makes it possible for third-party code to do so as well. Conversely, requiring all administrator code to reside in the module burdens the module author with properly securing those functions with module admin, which as noted above is impossible currently, requiring duplicate methods.

We note that third-party exploits of module admin are very hard to pull off, and indeed can only happen if the admin transaction calls into third-party code which is ready to enact the exploit (which would fail in any normal transaction and draw attention to itself, as there is no way to detect that module admin is acquired).

Finally, this firmly sounds the alarm that putting module-admin credentials in a transaction is a highly sensitive operation which should not call 3rd party code if it can help it, and instead wait for a safe transaction in which to do so.

# Runtime Safety

## Termination

Pact code is guaranteed to terminate by
- not allowing anonymous lambdas -- all lambdas must be declared in module code
- resolving all references at module load, failing on any cycle

However, there are two cases where a module can execute "dynamic" code that has not been previously resolved at runtime:

1. **Keyset predicate evaluation**. Custom keyset predicates are stored as a bare string and evaluated in `enforce-keyset` as an application of the indicated function.
2. **User guard evaluation**. User guards can be read from the database or constructed externally, and are evaluated by-name as the application of the indicated function.

In both cases, they are run in a special context that tests the call stack for a repeated occurrence of the application function name, and fails if found.

Thus Pact enforces termination in all cases.

## Safety in evaluation of _g_

The guard function _g_, when evaluated, runs as a "normal" application of _d_ as a function with _P_ for arguments. The invariants for safe execution of this function are:

### Termination

Generally this is implied by being a "normal" application in a module, thus as standard Pact module code.

### Acquire and "manual" install (`install-capability`) in-module only.

The ability to bring a capability into scope, either managed or call-stack, is restricted to in-module code only.

For acquisition, this guarantees that any state-mutating code guarded by `with-capability` cannot escape the confines of the declaring module. Otherwise, the authority granted by the capability is unknowable.

For installation, this ensures that the scenarios in which a managed capability is installed are only:

1. Module code controlled by the author
2. Installation via inclusion in _W_.

#2 is essential as it allows the end-user who might be affected by the capability to control how and if it is installed. Obviously, #2 will fail if for some reason this install cannot be achieved.

As "unscoped" capabilities, the goal here is to prevent a capability from being installed by untrusted code. Having said that, examples of safe usages of #1 are not known at this time, but the in-module restriction means that if one emerges, it's possible, and otherwise module authors can simply avoid it. (It is also useful for testing to install capabilities by acquiring module admin).

### Prevention of acquire or install.

Acquire or install of _c* d_ both change the state of _R_:

- acquire pushes a slot for _c* d_ onto _S<sub>A</sub>.
- install adds a slot for _c* d_ to _S<sub>M</sub>_ and adds _c* d_ to "seen" _C*<sub>M</sub>_.

If during this operation, a nested acquire or install were to happen:

1. A nested install within an outer acquire would **violate the scope implied by the outer acquire**, as the nested install would persist after the outer acquire goes out of scope.
2. A nested install within an outer install has no obvious defects.
3. A nested acquire in an outer acquire would allow protected state-mutating activity scoped by the nested acquire to occur, while we were trying to see if we can acquire the outer capability, presumably guarding some other state-mutating activity. Reasoning about state in this scenario is unsatisfying at a minimum and possibly incoherent. It is if nothing else inelegant, although the stack scope would probably be unharmed.
4. A nested acquire within an outer install has the same concerns as #2 about executing some state-modifying activity in the body of the install.

Since some issue above applies in either nested install (#1) or acquire (#3, #4), install and acquire are prevented while executing _g_. This is accomplished by testing for a `defcap` in the call stack when either `install-capability` or `require-capability` are invoked.

Obviously, this is what `compose-capability` is for, importing some other capability into the scope of the outer capability. `compose-capability` has the reverse invariant that it can only be invoked within a `defcap` execution, using the same call-stack-checking mechanism.

### Safety of `require-capability`

`require-capability` does not affect _R_, as it simply fails if it does not find the indicated capability in _S<sub>A</sub>_. It also does not mutate state.

It is therefore safe in any context, acquire or install, as requiring within _g_ does not produce any unwanted effects. It is more logical to compose instead of require in _g_, but it is otherwise harmless, and there are perhaps scenarios where testing without acquiring is correct. Plus as we will see, require is useful in user guards, which may be tested in the context of _g_.

This is a happy conclusion, as requiring capabilities in user guards are useful for certain scenarios, like allowing modules to pay gas for some external user: the external user can indicate some account that is controlled by a user guard that tests for a capability in the foreign module, which has been installed by the external user in _W_.

## Safety in dynamic code

As noted above, there are two places where code is dynamically evaluated. While user guards are the more relevant case, anything said about user guards can easily apply to keyset predicates, as they execute in identical contexts:

- The evaluation context is **pure**, which means the code cannot mutate state.
- As noted above, termination is operationally guaranteed.

### Acquire/install unsafe in dynamic code

As acquire and install cause execution of _g_, they are unsafe in dynamic code:

1. **Unsafe failures due to purity violations.** Dynamic code is run in a pure context, whereas _g_ is not thus restricted. The outer context of dynamic execution will cause impure code in _g_ to fail.

2. **Unsafe failures if evaluated in _g_**. If _g_ tests a user guard or keyset predicate that attempts to acquire or install, the dynamic code will fail due to the restrictions above on acquire or install in _g_.

TODO: this is not enforced currently. Dynamic code should be noted as such in the call stack, so that it can be detected and always fail upon embedded acquire or install.

#### "Semantic safety"
Note that here, "unsafe" refers to a slightly different notion of safety which we will call "semantic safety": an unexpected failure in dynamic code renders the intention of the dynamic code inoperable, in a way that would otherwise be undetectable during module load and potentially testing. In other words, the real safety here is **totality**, where the cases of the dynamic code failing are feasible to enumerate.

This note is here because "safety" when talking about capabilities often refers to avoiding exploits like privilege escalation. In this context it is more about having total semantics for a user guard or keyset predicate.

### Require safe in dynamic code

For the same reasons that require is safe in _g_, it is safe in dynamic code:

- Purity is ensured, as require is pure and without any effects on the capability state.
- Termination and "semantic safety" are ensured, as _g_ is never invoked when requiring.

## Notes on runtime vs compile-time safety

None of the enforcements in this section are currently detected at compile time, as there is no type-level representation of capabilities to guide it; while Pact's compilation could detect immediate violations, it cannot detect violations in linked code, even if it is in the same module. Therefore it is not currently possible to do this.

Instead, runtime guarantees ensure that simple coverage of impacted code will immediately cause failure if these invariants are violated.
