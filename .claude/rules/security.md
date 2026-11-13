---
paths:
  - "src/**"
---

# Security

Internet Identity is an authentication service: a bug here hands someone else's identity to an attacker. When in doubt, fail closed.

## Bounded resources

- Bound everything a caller can grow, by count and by bytes: vector lengths before allocating, stored entries per anchor, caches, metrics label sets, outcall `max_response_bytes`. Never ship growth without the limit that bounds it.
- Bound every loop over stored data per call (`MAX_TO_PRUNE`-style), counting items examined, not only items produced. Updates are capped at about 40B instructions, per heartbeat or timer.
- Reject cheaply before doing expensive work on an untrusted call. Give every outcall path a concurrency limit (`ConcurrencyLimiter`).
- II runs on a system subnet: cycles are not the constraint, and per-endpoint rate limits or per-identity caps don't help (identities are free). Keep state bounded instead.
- HTTP outcall transforms deserialize into the accepted shape and re-emit only that, with a fixed status and no headers, so replicas agree.

## Authorization and trust

- Authorize at the top of every authenticated endpoint through `authz_utils`, before any other work. `canister_inspect_message` is not authentication.
- Keep trust decisions (allowlists, gating) in the canister, not in frontend code the user can bypass.
- Security defaults fail closed: trust inputs are required, a malformed config counts as unset, `Default` is the disabled state.
- Revocation (sign-out, consent withdrawal, removal) closes every path that could still mint or read for that state, including derived queues and caches.
- Reset the session store whenever the identity changes or the current access method is removed, so session keys are never reused across identities.

## Origins, URLs and hostnames

- Parse URLs with `new URL()` / the `url` crate and compare exact origins (`new URL(x).origin === x`). Never concatenate URL parts or use substring checks.
- Match a hostname against an allowlist exactly or on a dot boundary (`host === domain || host.endsWith("." + domain)`), never a bare `endsWith(domain)`.
- Every origin used as an outcall target or derivation input goes through the canonicalizing validator. Outcall targets require `https`, with plain `http` allowed only for loopback hosts behind an explicit dev flag. Derivation origins are validated per transport: `OriginSchema` also accepts `chrome-extension://`.
- Validate the derivation origin in every channel/RPC handler before using it, with `OriginSchema` rather than `z.url()`.
- Sanitize navigation targets (`next`, redirects) to same-origin relative paths as early as possible (in `load`). No open redirects.
- Match an OpenID provider on `(iss, aud)`, never `iss` alone, and include `aud` in seed derivation.

## Delegations and keys

- Length-prefix every variable-length field when hashing a seed.
- Keep delegation seeds minimal and justify every input. Set `targets` on delegations whose contract names them (the session delegation is scoped to II), and keep them when rebuilding a chain. App delegations have none, because apps call arbitrary canisters.
- Allow for replica clock drift in delegation lifetimes, but never beyond the caller's `maxTimeToLive` or another cap such as the MCP session grant.
- Browser keys are generated with `extractable: false`. Use the browser's built-in crypto functions, never hand-rolled crypto.
- Every frontend agent, including ones created in service workers, carries the deployment's `shouldFetchRootKey`.

## Leaks

- Don't leak whether something exists: a missing anchor gets the same answer as an unauthorized caller, and a caller-supplied id must not trigger a trapping read.
- Keep PII and linkable data (emails, `sub`, mailbox provider, nonces) out of logs, the archive, metrics and errors. Don't tell relying parties which authentication method was used.
- `#[query(hidden = true)]` is not access control. Never ship debug endpoints in production candid.
- One production wasm per canister: dev behaviour (`fetch_root_key`, dummy auth, captcha) is set through init args, never compile-time features.

## Browser

- Size caps stop the download (`fetchCapped`), not check the size after buffering. Sensitive fetches use `redirect: "error"` and `credentials: "omit"`.
- Validate every untrusted message and stored value with zod. Every `postMessage` listener checks the origin.
- Show app-supplied metadata next to the origin it came from. Never inline SVG from an app; re-encode images and use `blob:` URLs.
- Read globals through `globalThis` (`globalThis.location.origin`), and strip the URL fragment right after reading secrets from it.
- Keep the CSP exactly as wide as what the code accepts, and scope relaxations per route.
- Cross-origin `target="_blank"` links get `rel="noopener noreferrer"`.
- Generate random ids with `secureRandomId` (rejection sampling), never `random % n`.
