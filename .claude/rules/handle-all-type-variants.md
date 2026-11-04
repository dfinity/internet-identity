# Handle every variant a type can carry — never assume the backend won't use one

When code consumes a value of a union/variant type (a candid `variant`, a TS
discriminated union, a Rust `enum`), handle **every** case the type permits.
Never narrow the handling to "the cases the producer happens to return today."

**Why:** the type is the contract. The set of values that can cross the
boundary is defined by the type, not by the current behaviour of the code on
the other side. Relying on "the canister only ever returns `Ok(Succeeded)`
here, so I don't need to handle `Ok(Failed)`" couples the consumer to a runtime
detail invisible at the call site and free to change. When it changes — a new
backend release, a refactor, a different deploy — the consumer silently
mishandles the new value with no compiler error and no failing test. Branches
that match the type are cheap; a silent gap is a latent bug.

**How to apply:**

- Enumerate variants from the type definition, not from your model of what the
  other side returns.
- Write the "can't happen today" branch anyway: handle it, or fall through with
  an explicit comment that it is non-terminal / ignored on purpose.
- Prefer exhaustive constructs that fail loudly when a case is added (Rust
  `match` without a catch-all `_`; TS narrowing a reviewer can see is total).
- Don't delete a "redundant" branch because the current producer can't trigger
  it — if it matches the type, it's part of the contract.
- Cuts both ways: a frontend handles every variant the backend's return type
  allows; a backend handles every variant of the input type.

**Smell:** "this branch is unreachable because the other side never returns it"
— if the type says it can, it's reachable as far as the contract is concerned.
Handle it.
