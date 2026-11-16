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
- `sec/bound-state-not-rate` II runs on a system subnet: cycles are not the constraint, and per-endpoint rate limits don't help. A per-identity cap bounds that identity's own state (`MAX_SESSIONS_PER_ANCHOR`), but never the total, since identities are free. Bound shared state and resources directly.
- `sec/outcall-transform-shape` HTTP outcall transforms deserialize into the accepted shape and re-emit only that, without headers, so replicas agree. Keep the status where the caller branches on it (`notifications/senders.rs` treats 404 as an empty list and retries other failures).

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
- `sec/validate-derivation-origin` Validate the derivation origin in every channel/RPC handler before using it: its shape with `OriginSchema` rather than `z.url()`, and that the requesting app may use it with `validateDerivationOrigin` against the channel origin.
- `sec/no-open-redirects` Sanitize navigation targets (`next`, redirects) to same-origin relative paths as early as possible (in `load`). No open redirects. Protocol callbacks are the exception when validated against what the requester declared: the `/mcp` server's allow-list, the `/cli` loopback callback.
- `sec/openid-iss-aud` Match an OpenID provider on `(iss, aud)`, never `iss` alone, and include `aud` in seed derivation.

## Delegations and keys

- `sec/length-prefix-seeds` Length-prefix every variable-length field when hashing a seed.
- `sec/minimal-seeds` Keep delegation seeds minimal and justify every input.
- `sec/delegation-targets` Set `targets` on delegations whose contract names them (the session delegation is scoped to II), and keep them when rebuilding a chain. App delegations have none, because apps call arbitrary canisters.
- `sec/delegation-lifetime` Allow for replica clock drift in delegation lifetimes, but never beyond the caller's `maxTimeToLive` or another cap such as the MCP session grant.
- `sec/non-extractable-keys` Browser keys are generated with `extractable: false`. The one exception is the one-shot ICP Alerts link key (`lib/utils/notifications/linkKey.ts`), whose private half travels in the install link; keys that are kept stay non-extractable.
- `sec/no-hand-rolled-crypto` Use the browser's built-in crypto functions, never hand-rolled crypto.
- `sec/root-key-everywhere` Every frontend agent, including ones created in service workers, carries the deployment's `shouldFetchRootKey`.

## Leaks

- `sec/no-existence-leaks` Don't leak whether something exists: a missing anchor gets the same answer as an unauthorized caller, and a caller-supplied id must not trigger a trapping read.
- `sec/no-pii-in-logs` Keep PII and linkable data (emails, `sub`, mailbox provider, nonces) out of logs, the archive, metrics and errors.
- `sec/hide-auth-method` Don't tell relying parties which authentication method was used.
- `sec/no-secrets-in-urls` Never put secrets, tokens, keys or personal data in a URL: not in the path, the query or the fragment. URLs end up in analytics, browser history, server logs, referrers and bookmarks. Pass such values through `postMessage`, a request body or storage instead. Where a URL has to carry one, put it in the fragment: the ICP Alerts install link (iOS keeps only the URL of a Home Screen app), and the requests a client opens II with (`/cli`, `/mcp`). Analytics must never report it, in either Plausible mode: `reportedUrl` in `lib/utils/analytics/analytics.ts` keeps the fragment in hash mode, so every path that carries a secret must be stripped there in both modes, with a test.
- `sec/no-debug-endpoints` `#[query(hidden = true)]` is not access control. Never ship debug endpoints in production candid.
- `sec/one-production-wasm` One production wasm per canister: dev behaviour (`fetch_root_key`, dummy auth, captcha) is set through init args, never compile-time features.

## Browser

- `sec/capped-fetch` Size caps stop the download (`fetchCapped`), not check the size after buffering.
- `sec/sensitive-fetch-options` Sensitive fetches use `redirect: "error"` and `credentials: "omit"`.
- `sec/validate-messages` Validate every untrusted message and stored value with zod. Every `postMessage` listener checks the origin.
- `sec/app-metadata` Show app-supplied metadata next to the origin it came from. Never inline SVG from an app; re-encode images and use `blob:` URLs.
- `sec/globals-and-fragments` Read globals through `globalThis` (`globalThis.location.origin`), and strip a secret-bearing URL fragment as soon as it is no longer needed. The ICP Alerts install page keeps it until iOS has saved the URL, and rereads it on retry.
- `sec/tight-csp` Keep the CSP exactly as wide as what the code accepts, and scope relaxations per route.
- `sec/noopener` Cross-origin `target="_blank"` links get `rel="noopener noreferrer"`.
- `sec/secure-random-ids` Generate random ids with `secureRandomId` (rejection sampling), never `random % n`.
