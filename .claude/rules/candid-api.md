---
paths:
  - "**/*.did"
  - "src/internet_identity_interface/**"
  - "src/internet_identity/src/main.rs"
  - "src/internet_identity_frontend/src/main.rs"
  - "src/frontend/src/lib/generated/**"
---

# Candid API

The backend `.did` is a public API with outside callers (wallets, other identity apps, every integrating dapp). Nothing in CI blocks an incompatible change (the interface-compatibility job is disabled), so review it by hand.

## Evolving the interface

- Never change a shipped signature incompatibly: no new non-`opt` record fields, no changed `Ok` types, no renamed or removed fields, methods or variants without an explicit decision. Types still inside an unmerged PR or stack are not shipped and may change freely.
- Add new record fields and new trailing arguments as `opt`, so callers built against the old `.did` still encode and decode. Stored records are minicbor, not candid: see `stable-memory.md` for new fields there. A new optional argument defaults to the old behaviour; changing an existing default is a breaking change.
- Mark superseded methods `// Deprecated:` in the `.did`. Extend an existing method rather than adding a sibling that does nearly the same thing; add a narrow new method only when widening an existing one would expose data to callers that don't need it.
- Leave room to grow: return a `record` (`record { is_granted : bool }`), not a bare `bool`.
- Install and upgrade args: `opt T` means "leave the stored value unchanged". Use `opt opt T` when a field must also be clearable (`null` keeps, `opt null` clears, `opt opt v` sets).
- Don't add fields to the backend-to-frontend config (`/.config.did.bin`) or to frontend stores unless the frontend uses them.

## Shape

- One `*Request` record argument per method once it takes more than a trivial input. Give opaque `text`/`blob` arguments a named alias (`type RegistrationId = text`).
- Method names start with a verb. Methods apps call are prefixed `app_`. A boolean query is named for its answer.
- Updates and authenticated calls return `variant { Ok : T; Err : XError }`. A public, unauthenticated lookup may return `opt T`. Never return `Ok : false` as a failure.
- Match field names to existing response shapes (`signed_delegation`). Use the same type name on both sides of the frontend/backend boundary.
- Don't make the caller send what `caller()` already proves.

## Errors

- One error type per method, with variants named after the condition the caller sees (`AccountLimitReached`, `InvalidRegistrationId`).
- Add a variant only when the caller's next step differs: something the user can act on or the frontend renders differently. Anything the frontend can only show as a generic failure (input it already validated, a disabled feature, unreachable edges, storage failures) is `InternalCanisterError : text`.
- Pick the return shape from what the method is. A mutation with a clear success or failure returns `variant { Ok : ...; Err : ... }`. A read with several distinct states (resolved, pending, failed) returns a status variant with the error detail inside its failure state; never wrap such a variant in `Ok`/`Err`.
- Before adding a variant, grep the `.did` for an existing one with the same meaning and reuse it with the same payload (`Unauthorized : principal`, `InternalCanisterError : text`). Don't stretch a variant to cover a different failure: `Unauthorized` means the caller isn't an authorized access method of the identity, so a failed browser-key check is `InvalidBrowserKey`. Don't give the same variant name a different payload or meaning in another error type.
- Never swallow an error: don't fold it into a default or success value, or into a variant that means something else. Collapsing failures the caller can't act on into `InternalCanisterError` is fine (see above), as long as its text keeps the specific reason.
- Revoking or removing something that is already absent is a no-op `Ok`, not `NotFound`.
- Keep a refusal to one variant when distinguishing cases would leak information (whether an anchor, session or binding exists). Never put another identity's data in an error.
- The frontend never parses error strings; if it must branch, the backend returns a typed variant.

## Bindings and docs

- After any `.did` change, run `npm run generate` (with the `didc` version CI pins) and commit the result in the same PR. Never hand-edit files under `src/frontend/src/lib/generated/`. Keep `query` annotations and Rust signatures in step with the `.did`; the `service_equal` test checks this.
- `.did` comments say only what a caller needs: behaviour, units, authorization. No implementation detail or design reasoning.
