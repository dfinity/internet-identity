---
paths:
  - "src/**"
---

# Security

Internet Identity is an authentication service: a bug here hands someone else's identity to an attacker. When in doubt, fail closed.

## Bounded resources

- `sec/bound-growth` Bound everything a caller can grow, by count and by bytes: vector lengths before allocating, stored entries per anchor, caches, metrics label sets, outcall `max_response_bytes`. Never ship growth without the limit that bounds it.
- `sec/bound-loops` Bound every loop over stored data per call (`MAX_TO_PRUNE`-style), counting items examined, not only items produced. Updates are capped at about 40B instructions, per heartbeat or timer.
- `sec/reject-cheaply` Reject cheaply before doing expensive work on an untrusted call.
- `sec/outcall-concurrency-limit` Give every outcall path a concurrency limit (`ConcurrencyLimiter`).
- `sec/bound-state-not-rate` II runs on a system subnet: cycles are not the constraint, and per-endpoint rate limits or per-identity caps don't help (identities are free). Keep state bounded instead.
- `sec/outcall-transform-shape` HTTP outcall transforms deserialize into the accepted shape and re-emit only that, with a fixed status and no headers, so replicas agree.

## Authorization and trust

- `sec/authorize-first` Authorize at the top of every authenticated endpoint through `authz_utils`, before any other work. `canister_inspect_message` is not authentication.
- `sec/trust-in-canister` Keep trust decisions (allowlists, gating) in the canister, not in frontend code the user can bypass.
- `sec/fail-closed` Security defaults fail closed: trust inputs are required, a malformed config counts as unset, `Default` is the disabled state.
- `sec/complete-revocation` Revocation (sign-out, consent withdrawal, removal) closes every path that could still mint or read for that state, including derived queues and caches.
- `sec/reset-session-on-identity-change` Reset the session store whenever the identity changes or the current access method is removed, so session keys are never reused across identities.

## Origins, URLs and hostnames

- `sec/parse-urls` Parse URLs with `new URL()` / the `url` crate and compare exact origins (`new URL(x).origin === x`). Never concatenate URL parts or use substring checks.
- `sec/hostname-match` Match a hostname against an allowlist exactly or on a dot boundary (`host === domain || host.endsWith("." + domain)`), never a bare `endsWith(domain)`.
- `sec/canonical-origins` Every origin used as an outcall target or derivation input goes through the canonicalizing validator. Outcall targets require `https`, with plain `http` allowed only for loopback hosts behind an explicit dev flag. Derivation origins are validated per transport: `OriginSchema` also accepts `chrome-extension://`.
- `sec/validate-derivation-origin` Validate the derivation origin in every channel/RPC handler before using it, with `OriginSchema` rather than `z.url()`.
- `sec/no-open-redirects` Sanitize navigation targets (`next`, redirects) to same-origin relative paths as early as possible (in `load`). No open redirects.
- `sec/openid-iss-aud` Match an OpenID provider on `(iss, aud)`, never `iss` alone, and include `aud` in seed derivation.

## Delegations and keys

- `sec/length-prefix-seeds` Length-prefix every variable-length field when hashing a seed.
- `sec/minimal-seeds` Keep delegation seeds minimal and justify every input.
- `sec/delegation-targets` Set `targets` on delegations whose contract names them (the session delegation is scoped to II), and keep them when rebuilding a chain. App delegations have none, because apps call arbitrary canisters.
- `sec/delegation-lifetime` Allow for replica clock drift in delegation lifetimes, but never beyond the caller's `maxTimeToLive` or another cap such as the MCP session grant.
- `sec/non-extractable-keys` Browser keys are generated with `extractable: false`.
- `sec/no-hand-rolled-crypto` Use the browser's built-in crypto functions, never hand-rolled crypto.
- `sec/root-key-everywhere` Every frontend agent, including ones created in service workers, carries the deployment's `shouldFetchRootKey`.

## Leaks

- `sec/no-existence-leaks` Don't leak whether something exists: a missing anchor gets the same answer as an unauthorized caller, and a caller-supplied id must not trigger a trapping read.
- `sec/no-pii-in-logs` Keep PII and linkable data (emails, `sub`, mailbox provider, nonces) out of logs, the archive, metrics and errors.
- `sec/hide-auth-method` Don't tell relying parties which authentication method was used.
- `sec/no-secrets-in-urls` Never put secrets, tokens, keys or personal data in a URL: not in the path, the query or the fragment. URLs end up in analytics, browser history, server logs, referrers and bookmarks. Pass such values through `postMessage`, a request body or storage instead. Where a URL has to carry one (the ICP Alerts install link, because iOS keeps only the URL of a Home Screen app), put it in the fragment and make sure analytics never reports it: `reportedUrl` in `lib/utils/analytics/analytics.ts` strips the fragment, so a new path that carries one must be covered there, with a test.
- `sec/no-debug-endpoints` `#[query(hidden = true)]` is not access control. Never ship debug endpoints in production candid.
- `sec/one-production-wasm` One production wasm per canister: dev behaviour (`fetch_root_key`, dummy auth, captcha) is set through init args, never compile-time features.

## Browser

- `sec/capped-fetch` Size caps stop the download (`fetchCapped`), not check the size after buffering.
- `sec/sensitive-fetch-options` Sensitive fetches use `redirect: "error"` and `credentials: "omit"`.
- `sec/validate-messages` Validate every untrusted message and stored value with zod. Every `postMessage` listener checks the origin.
- `sec/app-metadata` Show app-supplied metadata next to the origin it came from. Never inline SVG from an app; re-encode images and use `blob:` URLs.
- `sec/globals-and-fragments` Read globals through `globalThis` (`globalThis.location.origin`), and strip the URL fragment right after reading secrets from it.
- `sec/tight-csp` Keep the CSP exactly as wide as what the code accepts, and scope relaxations per route.
- `sec/noopener` Cross-origin `target="_blank"` links get `rel="noopener noreferrer"`.
- `sec/secure-random-ids` Generate random ids with `secureRandomId` (rejection sampling), never `random % n`.
