# Email-based identity recovery for Internet Identity

**Status:** Draft — RFC for review. Implementation in flight; see the
implementation status table below for which sections are shipped vs.
still planned.
**Last updated:** 2026-05-05
**Tracking PoC:** [#3760](https://github.com/dfinity/internet-identity/pull/3760) (DKIM postbox, will not be merged)
**Targets:** A new production-grade PR series, not a follow-up to #3760.

**Implementation status (as of 2026-05-06).**

| Component                                                                                             | Status    | Where                                                              |
| ----------------------------------------------------------------------------------------------------- | --------- | ------------------------------------------------------------------ |
| DNSSEC verifier (§7)                                                                                  | In review | PR [#3838](https://github.com/dfinity/internet-identity/pull/3838) |
| DKIM verifier (§5)                                                                                    | In review | PR [#3839](https://github.com/dfinity/internet-identity/pull/3839) |
| DMARC alignment (§6)                                                                                  | In review | PR [#3840](https://github.com/dfinity/internet-identity/pull/3840) |
| DoH fallback (§7.6)                                                                                   | In review | PR [#3841](https://github.com/dfinity/internet-identity/pull/3841) |
| Setup flow (§8.4): `prepare_add` → `smtp_request` → `status` → `credential_remove`                    | In review | PR [#3842](https://github.com/dfinity/internet-identity/pull/3842) |
| Recovery flow (§8.5): `prepare_delegation`, delegation issuance, `get_delegation`                     | In review | PR [#3843](https://github.com/dfinity/internet-identity/pull/3843) |
| Frontend wizards (§8.6, §8.10)                                                                        | In review | PR [#3844](https://github.com/dfinity/internet-identity/pull/3844) |
| Deploy/upgrade scripts (§7.5) — IANA root anchor fetcher, `dnssec_config` + `doh_config` install args | In review | PR [#3855](https://github.com/dfinity/internet-identity/pull/3855) |
| Frontend UX overhaul (§8.6)                                                                           | In review | PR [#3857](https://github.com/dfinity/internet-identity/pull/3857) |

Nothing in this stack is merged to `main` yet; all PRs are open and stacked on each other.

---

## Glossary

Conventions and protocol-specific tags used throughout this document.

**Email authentication.**

| Term                 | Meaning                                                                                                                                                                                               |
| -------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| **DKIM** (RFC 6376)  | DomainKeys Identified Mail. The sender's domain signs the email's headers + body hash with a per-domain private key; the public key is published as a TXT record at `<selector>._domainkey.<domain>`. |
| **DMARC** (RFC 7489) | Domain-based Message Authentication, Reporting, and Conformance. Policy record at `_dmarc.<domain>` declaring how receivers should handle mail that fails DKIM/SPF alignment.                         |
| **SPF** (RFC 7208)   | Sender Policy Framework. Authorizes sending IPs; not consulted in this design (see §6.5).                                                                                                             |
| **Selector**         | The DKIM `s=` value chosen by the email provider. The provider's currently-active public key is at `<selector>._domainkey.<domain>`.                                                                  |

**DKIM-Signature header tags** (RFC 6376 §3.5):

| Tag   | Meaning                                                                                                                                                                                                                         |
| ----- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `v=`  | DKIM version. Must be `1`.                                                                                                                                                                                                      |
| `a=`  | Signing algorithm: `rsa-sha256` or `ed25519-sha256`.                                                                                                                                                                            |
| `d=`  | Signing domain (the zone whose DNSKEY signed this message).                                                                                                                                                                     |
| `s=`  | Selector (sub-label under `_domainkey.<d>` where the public key is published).                                                                                                                                                  |
| `c=`  | Canonicalization mode `<header-algo>/<body-algo>`, each `simple` or `relaxed` (RFC 6376 §3.4). The canister accepts only `relaxed/*` on the header side — see §5.2.                                                             |
| `h=`  | Colon-separated list of header names the signature covers. Must include `From` (RFC 6376 §5.4) and, for this feature, `Subject` (§5.4).                                                                                         |
| `b=`  | Base64 of the signature bytes over the canonicalized hash input.                                                                                                                                                                |
| `bh=` | Base64 of the SHA-256 of the canonicalized body.                                                                                                                                                                                |
| `t=`  | Signature timestamp (Unix seconds).                                                                                                                                                                                             |
| `x=`  | Signature expiration (Unix seconds).                                                                                                                                                                                            |
| `l=`  | Body length, in canonicalized bytes, that the signature covers. Anything past byte `l` is unsigned (§5.3).                                                                                                                      |
| `i=`  | _Agent or User Identifier_ (RFC 6376 §3.5). Typically `<localpart>@<d>` or `@<d>`. The verifier requires this to align with `d=` — exact `<localpart>@<d>` if the DNS-side `t=s` flag is set, otherwise any subdomain of `<d>`. |

**DKIM TXT-record tags** (RFC 6376 §3.6.2.2):

| Tag       | Meaning                                                                                                                                                                     |
| --------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `v=DKIM1` | Record version marker.                                                                                                                                                      |
| `k=`      | Key type: `rsa` (default) or `ed25519`.                                                                                                                                     |
| `p=`      | Base64 of the public key, encoded as X.509 SubjectPublicKeyInfo (RFC 5280 §4.1). Empty `p=` means the key is revoked.                                                       |
| `t=`      | Flags. `t=y` marks the record as testing-only (we treat as Unverified, reason `TestingMode`). `t=s` ("strict") requires `i=` to be exactly `<localpart>@<d>`, no subdomain. |
| `s=`      | Service type. Unenforced.                                                                                                                                                   |
| `h=`      | Hash algorithms acceptable for this key. Unenforced.                                                                                                                        |

**DNSSEC.**

| Term                            | Meaning                                                                                                                                 |
| ------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------- |
| **DNSSEC** (RFC 4033/4034/4035) | Cryptographic chain from the IANA root KSK down to a leaf RRset, proving the records are what the authoritative DNS published.          |
| **KSK**                         | Key-Signing Key. Signs the zone's DNSKEY RRset; pinned by the parent zone's DS digest.                                                  |
| **ZSK**                         | Zone-Signing Key. Signs everything else in the zone. Both KSK and ZSK live in the same DNSKEY RRset.                                    |
| **DS** (RFC 4034 §5)            | Delegation Signer record. The parent zone's hash of the child's KSK; forms the cross-zone delegation chain.                             |
| **DNSKEY** (RFC 4034 §2)        | A public key as a DNS record.                                                                                                           |
| **RRSIG** (RFC 4034 §3)         | Signature record over an RRset, including inception and expiration timestamps.                                                          |
| **RRset**                       | All DNS records sharing the same `(owner_name, type, class)`. The unit DNSSEC signs over.                                               |
| **Trust anchor**                | Canister-side digest of the IANA root KSK; deploy-arg, see §7.5.                                                                        |
| **DoH**                         | DNS-over-HTTPS. Used both for caller-side bundle assembly (§7.4) and as a fallback path for domains that don't sign their zones (§7.6). |

**II-specific.**

| Term                  | Meaning                                                                                                                                                                                                                                                                                                                                                                                                                                                     |
| --------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| **Anchor**            | The user's identity number. Maps to passkeys, OpenID credentials, and (via this design) email-recovery credentials.                                                                                                                                                                                                                                                                                                                                         |
| **Nonce**             | The canister-issued one-time token (`II-Recovery-…`) embedded in the recovery email's `Subject:`. It self-identifies the pending challenge — see §3.2 / §8.4 — so there is no separate `challenge_id` on the wire.                                                                                                                                                                                                                                          |
| **Pending challenge** | An ephemeral heap entry keyed by nonce, holding the claimed address, a `(zone_name → DNSKEY RRset)` map populated from the skeleton chain (and grown at `submit_dkim_leaf` time when the DKIM CNAME chain crosses into a new zone), optional cached DMARC TXT bytes, optionally a partial-verification record (set when the email has arrived but the DKIM leaf hasn't been submitted yet), and (for recovery) the FE-supplied `session_pk`. 30-minute TTL. |
| **SMTP gateway**      | The off-chain service that accepts inbound mail at `register@<host>` / `recover@<host>` (for any `<host>` listed in the `related_origins` deploy arg — e.g. `id.ai` on prod, `beta.id.ai` on beta) and forwards each message to the canister via `smtp_request`. Untrusted by the canister beyond best-effort delivery — see §4.1.                                                                                                                          |

---

## 1. Background

Internet Identity currently offers one recovery channel when a user can no longer authenticate with any of their registered passkeys:

- **Recovery phrase** — a BIP-39-style seed phrase the user is responsible for storing offline.

(Earlier versions exposed a "Recovery device" flow as well; that surface is no longer offered to end users.)

Recovery phrases require the user to _prepare_ a recovery method before losing access, and to retain something — paper, password manager, hardware — outside the device that is now unusable. We hear from users that this falls through in practice: phrases get lost, password managers get locked out, paper backups end up next to the primary device and disappear together.

Email is the recovery channel almost every user already has and almost every user can reach from any browser. The PoC PR [#3760](https://github.com/dfinity/internet-identity/pull/3760) added enough plumbing to _receive_ DKIM-signed emails inside the canister and view them in a "Postbox" tab; that postbox surface is **out of scope** for this design. We borrow the DKIM verification primitive from the PoC, reshape it to verify a single email _in flight_ without persisting the message, and use it as the building block for a recovery-only feature.

This doc proposes the production design that supersedes the PoC. The PoC PR will be closed; the work below should land as a fresh PR series against `main`.

### What the PoC got right

- A first-pass DKIM verifier with a `DkimCheck` step-by-step result so the UI can show _why_ a signature did or didn't verify.
- The shape of `DkimVerificationStatus { Verified | Unverified | Pending }` decoupled from storage.
- Recipient-format and body/header bounds checks usable as input validation.

(The Postbox storage layout, the `smtp_postbox` stable map, per-anchor email pruning, and the `smtp_request` Candid surface for _delivering_ mail to the canister are not carried forward — see §2 non-goals.)

### What the PoC explicitly deferred

From the PR review thread (sea-snake's comments and aterga's replies) the following were left for a follow-up:

| Area                                      | Status in PoC                                | Spec gap                                                                                                  |
| ----------------------------------------- | -------------------------------------------- | --------------------------------------------------------------------------------------------------------- |
| Trusted body retention (`l=`)             | Stores full body, hashes only signed prefix  | Storage may include bytes not covered by the signature                                                    |
| DKIM-Signature parser                     | Naive split on `;` and `=`                   | RFC 6376 §3.5 allows folding, arbitrary whitespace, multiple `b=`-like substrings inside other tag values |
| DNS TXT record parser                     | Tolerant of `P=` casing only                 | RFC 6376 §3.6.2.2 allows folding across multiple TXT chunks, arbitrary whitespace                         |
| Header canonicalization                   | "simple" rebuilt from parsed `(name, value)` | Constrain verifier to `c=relaxed/*`, reject `c=simple/*` (see §5.2)                                       |
| Policy tags `i=`, `k=`, future-dated `t=` | Not enforced                                 | RFC 6376 §3.5 / §3.6.1                                                                                    |
| DMARC alignment                           | Not implemented                              | RFC 7489 §3                                                                                               |
| Service worker `postMessage` origin check | Missing                                      | CodeQL alert #127                                                                                         |

This document covers all of the above plus two architectural changes the PoC did not attempt:

- replacing DoH HTTP outcalls with **client-supplied DNSSEC-verified DNS records**;
- adding **email recovery** as a first-class authn method.

---

## 2. Goals & non-goals

**Goals**

- A user can register an email address as a recovery method. Anyone able to prove control of that address — by sending a DKIM-signed email containing the canister-issued challenge nonce — can sign in to the bound anchor. Recovery is the typical motivation (the user lost every passkey), but the flow is open to any anonymous caller that can complete the challenge; it doesn't gate on "you've lost everything else."
- DKIM verification is RFC-6376-compliant against the corpus of mainstream senders (Gmail, iCloud, Outlook, Fastmail, Proton, ProtonMail, Tutanota, AWS SES, SendGrid, Postmark, Mailgun).
- DMARC alignment is checked and enforced according to the sender's published policy.
- The canister's DKIM/DMARC verification is **fully deterministic** and does not depend on HTTPS outcalls during the recovery flow.
- A registered email holder can prove control of that address with a single signed email; the canister does not have to trust the SMTP gateway for _anything other than message delivery_.
- **The canister never persists incoming email contents.** The signed email and its DNSSEC bundle are passed in as a single call argument, verified, and acted on synchronously; only the small persistent state described in §8 is written to stable memory.

**Non-goals**

- The PoC's "Postbox" mailbox feature (storing inbound email per anchor for later viewing). The PoC's storage layout, push-notification path, and inbound-mail UI are not part of this design and will not be carried forward as part of email recovery.
- Sending email _from_ the canister. Outbound (e.g., notifications, recovery codes) is delivered by an off-chain service that need not be in the trust path.
- Replacing the existing recovery option. Email recovery is an _additional_ `AuthnMethodPurpose::Recovery` method; users can still register a recovery phrase.
- Protecting against a fully compromised mailbox provider. If Google's DKIM signing key is exfiltrated, every Gmail-recovery anchor is at risk; we accept that and document it.
- Verifying _encrypted_ (S/MIME, PGP) email contents. We verify DKIM-signed envelopes only.

---

## 3. Threat model

**Trusted parties**

- The user's mailbox provider (Gmail, iCloud, …): trusted to keep the DKIM private key secret and to reject spoofed inbound mail destined for the user.
- The DNS authoritative servers for the sender's domain: trusted to publish honest DKIM/DMARC records, _and_ trusted to sign them with DNSSEC.
- IANA / ICANN root KSK: trusted as the DNSSEC trust anchor.

**Untrusted parties**

- The SMTP-receiving gateway. Can drop, delay, reorder, or fabricate inbound messages. Cannot fabricate a DKIM signature without the sender's private key. Cannot fabricate a DNSSEC chain without the sender's signing keys.
- The DNSSEC resolver client (could be the SMTP gateway itself or the user's browser). Can lie about _which_ records exist; cannot fabricate a valid DNSSEC chain.
- Boundary nodes / DoH providers. Same — used only as transports if used at all.

**Attacker capabilities we defend against**

1. _Spoofed `From:` header_ — defended by DMARC alignment with verified DKIM `d=`.
2. _DKIM signature replay_ — defended by `x=` expiration plus an ingest-time freshness window plus a single-use challenge nonce embedded in the email's `Subject:` and burned on first acceptance.
3. _DNS poisoning_ — defended by DNSSEC validation against the IANA root KSK trust anchor (delivered as a deploy/upgrade arg, see §7.5).
4. _Length-extension via `l=` tag_ — defended by ignoring any unsigned tail (see §5.3).
5. _Mass enumeration of email→anchor mappings_ — gated by DKIM verification: an attacker cannot probe the index without a DKIM-valid email from the address being probed (see §3.1).
6. _Recovery phishing — attacker prepares a challenge, then tricks the victim into emailing the resulting nonce._ See §3.2 for the analysis. Defended by (a) the rotating random nonce, which kills async/mass phishing because by the time a phishing message reaches the victim the nonce has expired; (b) the token living in the email's `Subject:` rather than the body, so the user sees the recovery intent (`II-Recovery-…`) at compose time; (c) the legitimate token only ever appears on the real II recovery page in the user's own browser — a phisher who can place a token in front of the user has already phished them onto a fake II domain, which is the standard website-phishing problem we don't try to solve in this layer.

### 3.1 Why the email→anchor index does not need salted hashing

The index that maps a verified sender address to an anchor number is a public-shape concern (an attacker could in principle iterate addresses to discover whether a friend has an II account). In practice the lookup is gated by DKIM: the canister never accepts an `email_recovery_*` call without a DKIM-valid email signed by the queried domain on behalf of the queried address. An attacker who controls `mallory@gmail.com` can probe whether `mallory@gmail.com` is registered, but cannot probe `alice@gmail.com` without first compromising Alice's mailbox or Gmail's DKIM key — at which point they already have full mailbox control.

We therefore key the lookup index by `lowercase(local-part) + "@" + lowercase(domain)` directly. No per-anchor salt is needed; we do _not_ try to make the index itself unenumerable.

### 3.2 Recovery phishing: the prepare-then-trick attack

`email_recovery_prepare_delegation` is anonymous (it has to be — the user has lost every authn method by the time they reach for it). That means an attacker can call it for _anyone's_ email, get a fresh nonce, and bind it to an attacker-controlled `session_pk`. If the attacker can then convince the victim to send an email containing that nonce from the victim's own address, the canister will mark the challenge `Succeeded`, stamp a delegation for the attacker's `session_pk`, and the attacker signs in as the victim.

This is structurally the same attack as classic email-recovery phishing ("send your verification code to support@…"). We do not have outbound email in scope (§2 non-goals), so we cannot ship a confirmation-reply round trip that lives inside the victim's inbox. The defenses we _do_ layer:

- **The nonce rotates.** It exists only inside one pending entry with a 30-minute TTL. Phishing scripts blasted out at scale are useless: by the time a generic "send this to recover your account" message reaches a victim, the embedded nonce has likely expired, and the attacker would need to be running real-time interactive phishing on each target individually to keep nonces fresh.
- **The nonce lives in `Subject:`, not the body.** The user sees `Subject: II-Recovery-1a2b3c4d5e6f7081` at compose time, which is harder to misinterpret as "support help". Keeping the prefix verbose (`II-Recovery-…`) is deliberate — every legitimate UI surface says exactly the same prefix, so a token without it is an immediate tell.
- **The legitimate token only ever appears on the real II recovery page** in the user's own browser session. An attacker who can place a token in front of the user has already convinced them to visit a phishing site impersonating II — at which point the email step is the _least_ of the user's worries. We treat website phishing as a separate layer and do not try to make the email recovery flow robust against a successfully-impersonated frontend.
- **UX explicitness in step 2.** The wizard's send-the-magic-email screen (§8.6) names the action plainly: "_This will sign you in to your existing Internet Identity_", with a warning callout: "_Only continue if you started this recovery yourself, just now, on this page._" This is the canister-stack equivalent of the "is this you?" sticker on a hardware wallet — it doesn't stop a determined social engineer, but it is the place a careful user notices something is wrong.

What we explicitly _don't_ try to do is gate the prepare endpoint behind a captcha or per-address rate limit (see §8.9 for why per-address rate limiting is itself a denial-of-recovery vector). The nonce-rotation + UX-clarity stack is the practical ceiling for an inbound-only, no-stored-state-per-user flow. The natural upgrade path, when outbound email becomes available, is to add an inbox-side confirmation step — see §11 ("Open questions").

**Attacker capabilities we do _not_ defend against**

- A user who voluntarily forwards their own DKIM-signed challenge email to an attacker. Standard phishing concern; mitigated by UX (challenge email's body says "do not forward").
- A SIM-swap-equivalent at the email provider (attacker controls the inbox). Out of scope.
- A registrar or TLD compromise that lets an attacker rotate DNSKEYs. The DNSSEC chain still validates, but for a malicious key. This is the same trust assumption every DNSSEC consumer makes.

---

## 4. High-level architecture

```mermaid
sequenceDiagram
    participant User as User browser
    participant DNS as Public DoH resolver
    participant II as II canister
    participant Mail as User's mailbox<br/>(e.g. Gmail)
    participant GW as SMTP gateway<br/>(off-chain)

    Note over User,DNS: 1 — FE assembles a "skeleton" DNSSEC chain<br/>(everything <em>except</em> the DKIM leaf)
    User->>DNS: fetch DMARC TXT,<br/>walk DNSSEC chain to root
    DNS-->>User: signed RRsets + RRSIGs<br/>(no DKIM leaf yet — selector unknown)

    User->>II: 2. prepare { addr, dns_proof_skeleton, session_pk }
    II-->>User: 3. challenge { nonce, expires_at }

    User->>Mail: 4. compose and send email to<br/>register@[host] or recover@[host]<br/>(host = window.location.hostname),<br/>with the nonce in Subject:
    Mail->>GW: 5. SMTP DATA
    GW->>II: 6. smtp_request(SmtpRequest)

    Note over II: parse DKIM-Signature → s= (selector),<br/>verify body hash, cache headers digest + signature

    loop while waiting
        User->>II: 7. email_recovery_status(nonce)
        II-->>User: Pending → NeedDkimLeaf { selector } → …
    end

    Note over User,DNS: 8 — FE fetches the DKIM leaf for the *real* selector
    User->>DNS: fetch DKIM TXT at <s=>._domainkey.<domain><br/>+ RRSIGs anchored in the cached chain
    DNS-->>User: signed RRset

    User->>II: 9. submit_dkim_leaf(nonce, signed_rrset)
    II-->>User: Pending → Succeeded / RecoveryReady

    II-->>User: 10. SignedDelegation (recovery only)
```

The architecture is built around three ideas:

- **DNSSEC validation happens in two phases, separated by the email arrival.** Pre-email, the FE assembles a "skeleton" `DnsProofBundle`: the DNSSEC chain rooted at the IANA anchor down through the registered domain's zone DNSKEY, plus the DMARC TXT leaf at the well-known name `_dmarc.<domain>` if the zone publishes one. The DKIM leaf is _not_ included — its name is `<selector>._domainkey.<domain>`, and the active selector is published only inside the eventual email's `DKIM-Signature` header, so the FE can't fetch it yet. The canister validates the skeleton chain on `prepare`, caches the resulting `(zone_name → DNSKEY)` map, and returns the nonce. Post-email, the canister extracts `s=` from the DKIM-Signature header, sets the polled status to `NeedDkimLeaf { selector: s }`, and waits for the FE to walk the leaf. The FE submits the signed RRset(s) for the resolution chain `<s>._domainkey.<domain>` → … → final TXT via a follow-up call — supplying any additional zone delegation chains needed when the resolution crosses into a new signed zone (Proton-style cross-zone CNAME, §7.2); the canister merges those new chains into the cached map, validates each hop under the zone its RRSIG names, and finalises verification. **No selector probing on the FE**: the selector comes from the email itself, authoritatively.
- **The user actually emails the nonce.** They send a fresh email from the address they typed, with the nonce in the `Subject:` (e.g. `Subject: II-Recovery-1a2b3c4d5e6f7081`), to `recover@<host>` (or `register@<host>` for setup), where `<host>` is one of the `related_origins` deploy-arg entries — the FE renders the user-facing label by pairing the user-part with `window.location.hostname`, so each tab automatically shows the alias matching the origin the user is on (`register@id.ai` on prod, `register@beta.id.ai` on beta). The recipient is a single static mailbox per kind — **the nonce is the only identifier** in the protocol: it's the lookup key the canister uses to match the inbound email to a pending challenge, the value the FE polls under, and the human-typeable token in `Subject:`. Each nonce is drawn from a 64-bit PRNG (§8.9) into exactly one pending entry, so it self-identifies — knowing the nonce is necessary and sufficient to reference the challenge. We deliberately don't carry a separate `challenge_id`. Keeping the token in `Subject:` rather than the body is the other deliberate choice: the user sees the recovery intent (`II-Recovery-…`) at compose time, which is the cheapest defence against the prepare-then-phish attack discussed in §3.2.
- **The SMTP gateway forwards the email to the canister via `smtp_request`** — exactly the PoC's surface, no shape change. On arrival the canister parses the DKIM-Signature header (without yet having the public key), canonicalises the body and checks `bh=`, computes the SHA-256 of the would-be DKIM-signature input, and stashes a small partial-verification record (~500 B: headers digest, signature blob, selector, signing domain, From-domain, claimed address) on the pending challenge. The body itself is dropped — once `bh=` is verified the body's bytes don't matter for the rest of the pipeline. The status flips to `NeedDkimLeaf { selector }` so the FE can walk DNSSEC for that specific selector and call `email_recovery_submit_dkim_leaf(nonce, signed_rrset)`. The canister validates the leaf against the already-anchored chain, completes the DKIM signature check (`RSA.verify(pk, headers_digest, signature)`), confirms DMARC alignment + `From:` match, then either binds the address (setup) or prepares a delegation tied to the FE's session public key (recovery). The FE polls `email_recovery_status(nonce)` throughout — the spinner just stays up a bit longer between "email arrived" and "verified".

The SMTP gateway is _partially trusted_: it can drop, delay, or fabricate calls to `smtp_request`. It cannot fake a DKIM signature for a domain it doesn't control, so the worst it can do is withhold delivery (a DoS that's acceptable for a recovery channel users only hit when locked out).

This buys us:

- **Determinism without consensus tricks.** No HTTP transform, no `max_response_bytes`, no boundary-node trust on the DNS path.
- **No cycles spent on outcalls** during recovery, which is the latency-sensitive path.
- **No persistent inbox state on chain.** The only stable-memory state added is the registered email→anchor index and a small TTL'd map of pending challenge entries.
- **No raw email upload from the user's browser.** The FE never has to fetch and re-upload a multi-KB email — the gateway delivers the bytes once, directly to the canister. The FE only sees the verification _outcome_ via polling.

We pay for it in:

- **Caller complexity.** The browser has to assemble the DNSSEC chain by walking the delegation from root down — twice: once for the skeleton (DMARC + chain) at prepare, once for the DKIM leaf after the email arrives. Both walks are straightforward TypeScript on top of DoH (see §7.4), no separate library or WASM module needed. The chain reuses validated DNSKEYs from the skeleton so the second walk only needs the leaf RRset and its RRSIG — typically two DoH queries.
- **No DNSSEC, no email recovery.** Domains that don't sign their zones cannot be used. As of 2026-05, this includes a non-trivial slice of mainstream consumer mailbox domains. We surface this clearly in the UI at registration time and let the user pick a different address or fall back to a recovery phrase.
- **Trust the gateway to deliver.** A malicious or down gateway can stall recovery, but cannot fabricate or alter outcomes (every cryptographic check is on the canister side).

### 4.1 SMTP gateway: untrusted-public-relay assumptions

The SMTP gateway sits between the public internet and the canister. It is operated as an off-chain helper — the same shape as the postbox PoC ([#3760](https://github.com/dfinity/internet-identity/pull/3760)) — but the canister treats it as **untrusted infrastructure**, indistinguishable from any other publicly-reachable principal that can call `smtp_request`.

**What the canister assumes about the gateway:**

- **Public, anonymously-callable.** `smtp_request` is an open `update` call and `smtp_request_validate` is an open `query`: anyone holding the canister principal can submit an `SmtpRequest`. The gateway is the _expected_ caller of both — the validate query at SMTP `RCPT TO` time to decide whether to accept the connection (Ok for `register@<h>` / `recover@<h>` where `<h>` is in `related_origins`, 550 otherwise), and the update once it has the full message — but the caller identity is not authenticated and not trusted (we don't pin a principal to "the gateway"). A malicious actor sending an `SmtpRequest` directly is treated identically to one delivered via the gateway — both must produce a DKIM-valid email matching a known pending nonce, or the call is a no-op.
- **Best-effort delivery.** The gateway can drop, delay, reorder, duplicate, or fabricate calls. It can withhold mail entirely (DoS — acceptable for a recovery channel) or replay calls (idempotent — `smtp_request` flips a challenge to a terminal state once, further calls hit `NonceExpired` / `NonceUnknown`).
- **Cannot fabricate cryptographic state.** The gateway has no DKIM signing keys for the domains it relays mail from, no DNSSEC private keys, no II canister state. Every check the canister cares about — DKIM signature, DMARC alignment, `From` ↔ `d=` alignment, `Subject` in `h=` — is verified against material the gateway either passed through unchanged (the email bytes) or that the canister cached at prepare time (the validated DKIM TXT, the DMARC policy).
- **Header parsing contract.** Beyond delivery, the gateway is trusted for one narrow protocol-shape promise: it delivers headers as `(name: String, value: String)` pairs in receipt order, with values as raw bytes (no RFC 2047 decoding). This contract is exactly what PoC [#3760](https://github.com/dfinity/internet-identity/pull/3760)'s gateway already provides; §5.2 explains why this constrains the canister to `c=relaxed/*` on the header side.
- **No persistent storage.** The gateway holds the bytes of an inbound message only long enough to make one `smtp_request` call. It is not part of the canister's trust path, and it doesn't see the verification outcome (the FE polls the canister directly).

**Implications for deployment:**

- The gateway can run as a cycles-paying canister or an off-chain service; either way the canister code is the same.
- Multiple gateway instances can coexist (e.g. for redundancy) without coordination — `smtp_request` is idempotent on a per-nonce basis.
- A gateway compromise does not compromise existing or future recovery flows; the worst it can do is delay or drop them. There is no separate "trusted gateway" allowlist to maintain.

---

## 5. Component A — Production-grade DKIM verifier

The PoC's `src/internet_identity/src/dkim.rs` is replaced rather than incrementally fixed. A naïve manual parser is the wrong shape for the spec — folding, multi-chunk TXT records, and byte-exact canonicalization need careful, RFC-pinned code.

### 5.1 Library choice

**Decision: hand-rolled, in `src/internet_identity/src/dkim/`.** The split:

- `parse` — `DKIM-Signature` header tag-list parser (RFC 6376 §3.5).
- `dns_record` — DKIM TXT record parser (RFC 6376 §3.6.2.2).
- `canonicalize` — relaxed header (§3.4.2) and body (§3.4.4) forms; `simple/*` on the header side is rejected by construction (§5.2).
- `signature` — RSA-SHA256 (RFC 5702 / RFC 8301) and Ed25519-SHA256 (RFC 8463) on top of the workspace `rsa` and `ed25519-dalek` deps.
- `verify` — orchestration: multi-signature loop, tag enforcement, accept-on-first-pass.

**Rejected alternatives**

- [`mail-auth`](https://github.com/stalwartlabs/mail-auth) (Apache-2.0, used by Stalwart) — was the original recommendation. Drops out because its dependency tree pulls a non-optional `hickory-resolver` (DNS via tokio sockets) that doesn't compile to `wasm32-unknown-unknown`. Forking it to vendor a no-net resolver shim would be larger than rolling the verifier from the RFC.
- `cfdkim` — solid but tightly coupled to `tokio-trust-dns`, hard to unhook from network IO.
- Continue with the PoC's parser — every reviewer comment in the PoC PR is some shape of "you can't safely roll your own canonicalization parser." Agreed; the rewrite this section describes is the answer, just from-scratch rather than library-backed.

### 5.2 Header canonicalization with the existing gateway contract

DKIM "simple" header canonicalization signs the _exact original bytes_ of each signed header line. The existing SMTP gateway from PoC #3760 delivers headers as parsed `(name: String, value: String)` pairs (the PoC's `SmtpHeader`), which loses the original whitespace, folding, and the exact byte sequence after the colon — so the canister cannot verify simple-header signatures against this shape.

We accept the current gateway contract unchanged and constrain the canister-side verifier to `c=relaxed/*` for the _header_ canonicalization side. The relaxed algorithm (RFC 6376 §3.4.2) lowercases names, unfolds continuations, collapses runs of whitespace to a single SP, and strips WSP around the colon — every transformation is destructive of the same information the gateway's parser already discarded, so the relaxed-canonical form is reconstructible from `(name, value)` pairs by re-emitting them as `name + b": " + value + b"\r\n"` and feeding the result to the verifier.

In practice every mainstream sender we care about (Gmail, iCloud, Outlook, Fastmail, Proton, ProtonMail, AWS SES, SendGrid, Postmark, Mailgun) signs with `c=relaxed/relaxed` or `c=relaxed/simple`. Niche senders that sign with `c=simple/*` are rejected with reason `UnsupportedCanonicalization` and the user is asked to register a different address. We accept this surface loss in exchange for not blocking on a gateway-side change.

Body canonicalization is unaffected — the gateway delivers the body as a raw `blob`, so both `*/relaxed` and `*/simple` body modes verify byte-for-byte.

Two assumptions on the gateway-side parser, currently true of the PoC implementation, that this approach depends on:

- **Header values are passed as raw bytes**, not RFC 2047-decoded. Encoded-words like `=?utf-8?B?...?=` must reach the canister in their wire form so the relaxed canonicalization sees the same bytes the signer hashed.
- **Header order is preserved** in receipt order, with no deduplication. DKIM picks signed headers from the bottom up per `h=` tag (RFC 6376 §5.4), and signatures over duplicated headers fail if order is lost.

The canister extracts `From:`, `To:`, `Subject:`, etc. from the same parsed pairs for display and dispatch — there is no separate "raw" path.

### 5.3 Trusted-body handling (`l=`)

When a DKIM signature includes `l=N`, only the first N bytes of the canonicalized body are signed. Anything past byte N is unauthenticated and could have been appended by a forwarder or an attacker.

Email recovery does not store inbound email at all (see §2 non-goals), so this is a _verification-time_ concern rather than a storage-time concern:

- The DKIM verifier hashes only the first N bytes of the canonicalized body, exactly as RFC 6376 §3.4.5 requires.
- The challenge nonce lives in the `Subject:` header, which is signed (see §5.4), so `l=` does not affect the nonce search at all — the nonce is always covered by the cryptographic check regardless of body truncation.

The PoC's storage truncation (`truncate_at_char_boundary`) becomes irrelevant once the canister stops persisting the body, but we keep the same byte bound (`MAX_BODY_BYTES`) as an upper limit on the canister-call argument so a malformed caller cannot exhaust the message's argument budget.

### 5.4 Tag enforcement

Beyond the cryptographic check, the verifier rejects:

- `v != 1`.
- `a` outside the supported algorithm set: `rsa-sha256`, `ed25519-sha256`. (PoC supported `rsa-sha256` only.)
- `t > now + skew_window` — future-dated signatures (PoC parsed but did not enforce).
- `x < now` — expired signatures (the PoC's late round of fixes already enforces this; see [aterga's reply](https://github.com/dfinity/internet-identity/pull/3760#discussion_r3137585324) on the PoC review).
- `i=` (the optional _Agent or User Identifier_ tag from RFC 6376 §3.5; carries `<localpart>@<d>` or `@<d>` and identifies whose mailbox the signature is asserting) — its domain part must equal `d` (when the DNS-side `t=s` flag is set on the publishing key — strict alignment, no subdomains) or be a subdomain of `d` (the default — `i=user@mail.example.com` aligns with `d=example.com`). Mismatched `i=` is rejected with reason `IdentifierAlignment`.
- `c=` (the _canonicalization mode_ tag from RFC 6376 §3.4; declares how each side of the message — headers and body — was normalized before hashing, as `<header-algo>/<body-algo>` where each algo is `simple` or `relaxed`) — header side must be `relaxed`. Body side may be `simple` or `relaxed`. Signatures with `c=simple/*` are rejected with reason `UnsupportedCanonicalization` (see §5.2).
- `h=` — must include `From` (RFC 6376 §5.4 already requires this) **and** `Subject`. The `Subject` requirement is recovery-specific: the challenge nonce lives in `Subject:` (§8), so a signature that doesn't cover it would let a man-in-the-middle (a malicious gateway, a forwarder, a transparent proxy) rewrite the nonce on a legitimately-signed email and either DoS recovery or — worse — replace the nonce with one bound to an attacker's pending challenge. Every mainstream sender we care about already signs `Subject` by default; rejecting the rare niche signer that doesn't is the right tradeoff. Surfaces as reason `SubjectNotSigned`.
- DNS-side `k=` — defaults to `rsa`, must match the signature's algorithm.
- DNS-side `t=y` — testing flag; we treat the signature as Unverified with a `TestingMode` reason.

### 5.5 Multiple DKIM-Signature headers

PoC behaviour is correct: iterate over every `DKIM-Signature` header and accept on first verifying signature. Carry forward, emit per-signature `DkimCheck` arrays in the result so the UI can show why each one failed.

### 5.6 Public-key sanity

Already addressed in PoC: minimum 1024-bit RSA. Lift the floor to 2048 in a follow-up once telemetry shows no measurable rejection rate on the recovery surface — i.e., once we confirm none of the major senders we care about still sign at 1024.

---

## 6. Component B — DMARC alignment

DKIM proves "domain X signed this message." DMARC proves "the domain in the visible `From:` header authorized X to sign on its behalf." Without DMARC, an attacker who controls _any_ domain with valid DKIM can spoof `From: alice@gmail.com` and we'd accept it.

### 6.1 `From:` header parsing

The verifier needs the _header-`From:`_ domain (Y), not the SMTP envelope `MAIL FROM` the PoC consumes. RFC 5322 `From:` is an `address-list`; for DMARC, RFC 7489 §3.1.1 mandates that the message has _exactly one_ `From:` header containing _exactly one_ mailbox. We enforce both: reject (treat as Unverified, reason `MalformedFromHeader`) when there are zero, multiple, or list-style `From:` headers.

Implementation: small hand-written parser in `crate::dmarc::from_parser` covering both `addr-spec` (`alice@example.com`) and `name-addr` (`"Alice Example" <alice@example.com>`) forms. Quoted local-parts and obs-route forms are rejected as `MalformedFromHeader`.

### 6.2 DMARC record fetch

For sender domain Y, the canister needs the TXT record at `_dmarc.<Y>`. This is fetched the same way as DKIM keys — via the DNSSEC-validated arg bundle from §7. The verifier never makes its own DNS calls.

DMARC tags we honour:

| Tag                          | Meaning                                      | Default                         |
| ---------------------------- | -------------------------------------------- | ------------------------------- |
| `v=DMARC1`                   | Required                                     | —                               |
| `p=`                         | Policy: `none` / `quarantine` / `reject`     | required                        |
| `sp=`                        | Subdomain policy                             | inherits `p=`                   |
| `adkim=`                     | DKIM alignment mode: `s` strict, `r` relaxed | `r`                             |
| `aspf=`                      | SPF alignment                                | (we don't check SPF — see §6.4) |
| `pct=`                       | Percentage of failing mail to apply policy   | `100`                           |
| `fo=`, `rua=`, `ruf=`, `rf=` | Reporting                                    | ignored                         |

### 6.3 Alignment check

Each verified DKIM `d=` (call it X) is checked for alignment with the `From:` domain (Y), both ASCII-lowercased:

- **`adkim=s`** — X must equal Y.
- **`adkim=r`** — X must equal Y, _or_ X is a subdomain of Y (i.e. Y is a label-aligned suffix of X). Examples: `mail.example.com` aligns with `example.com` ✓; `evil.com` aligns with `example.com` ✗; `gmail.com` aligns with `googlemail.com` ✗.

This is _stricter_ than RFC-7489-compliant relaxed alignment, which uses the [Public Suffix List](https://publicsuffix.org/) (PSL) to compute "organizational domain" — under that algorithm, `gmail.com` and `googlemail.com` would align if they were listed as the same org, and `mail.example.com` aligns with `example.com` because both reduce to `example.com` regardless of subdomain depth. We accept the second case (covered by the subdomain rule) but reject the first. §6.4 explains why we deviate.

### 6.4 Why we don't use the Public Suffix List

The PSL is the de-facto reference for DMARC's "organizational domain" computation. We deliberately don't use it in the recovery flow.

**Trust expansion.** Email recovery currently trusts the user's mailbox provider (DKIM signing key), the DNSSEC chain (IANA root + delegations), and the user's own custody of their inbox. The PSL is community-maintained on GitHub (`publicsuffix/list`); anyone can submit a PR. Using it for alignment would add Mozilla and the broader reviewer set as a new trust point — thousands of entries, frequent merges, large surface area.

**Asymmetric failure mode.** A _missing_ entry fails closed: we'd reject mail that a spec-compliant verifier would accept (denial of recovery, not a compromise). An _added_ entry fails _open_: e.g. if `co.uk` were ever incorrectly removed from the list, `evil.co.uk` and `victim.co.uk` would alias under relaxed alignment, and an attacker controlling the former could sign mail aligning with `From: victim.co.uk`. That's an account-compromise vector with a wide blast radius. The probability is low but the consequences are bad enough to avoid by construction.

**Marginal benefit for this surface.** Every mainstream consumer mailbox we list in §2 (Gmail, iCloud, Outlook, Fastmail, Proton) signs `d=` exactly equal to the From-header domain — strict alignment, PSL never consulted. The cases where PSL helps:

- Multi-domain orgs (`gmail.com` ↔ `googlemail.com`): users register the address they actually send from, so the From and the registered address share a single domain. Not relevant.
- Subdomain sending (`d=mg.example.com` for `From: alice@example.com`): handled by the subdomain rule in §6.3 without the PSL.

The remaining gap (multi-domain orgs that _don't_ share a registrable suffix) fails closed under our policy. If the test corpus surfaces a real consumer mailbox provider that needs PSL-style alignment to verify, we revisit then. As of design time, none does.

**Net effect on the design.** No PSL bundle in the WASM, no weekly outcall, no transform function for PSL responses, no Mozilla/community-list trust dependency. The email-recovery stack has _zero_ HTTP outcalls — every per-verification check uses material the caller supplied (DNSSEC bundle) or material baked into the canister at deploy (DNSSEC root anchors).

### 6.5 SPF: not checked

DMARC permits a message to pass via either DKIM-aligned _or_ SPF-aligned. We deliberately do not check SPF, because:

- SPF needs the SMTP envelope's `MAIL FROM` (a.k.a. Return-Path) and the connecting IP. The IP is invisible to the canister; it would have to be passed by the gateway and is unauthenticated (the gateway can lie about it).
- SPF alone is not sufficient evidence of mailbox control — it only proves the connecting host was authorized, not that the mailbox holder originated the message.

For DKIM-aligned mail this never matters. For mail that _only_ passes SPF (no DKIM), we treat it as Unverified.

### 6.6 Policy enforcement and verification status

We extend `DkimVerificationStatus` to carry DMARC outcome:

```rust
pub enum VerificationStatus {
    Pending,
    Verified {
        dkim_checks: Vec<DkimCheck>,
        dkim_domain: String,         // d= of the signature that won
        from_domain: String,         // Y from From: header
        dmarc: DmarcOutcome,
    },
    Unverified {
        dkim_checks: Vec<DkimCheck>,
        reason: VerificationFailReason,
    },
}

pub enum DmarcOutcome {
    Aligned { policy: DmarcPolicy, alignment_mode: AlignmentMode },
    Misaligned { policy: DmarcPolicy }, // failed alignment; policy says what to do
    NoRecord,                            // no _dmarc TXT for the domain
}
```

For the recovery and registration flows, we only accept emails where `DmarcOutcome` is `Aligned` _or_ `NoRecord` with `dkim_domain == from_domain` (i.e., the DKIM domain matches the From: domain exactly even without an explicit DMARC record). Misaligned mail is rejected outright; there is no "spoofing suspected" middle state because the call has no value if it's not a usable proof.

### 6.7 Renaming

`DkimVerificationStatus` and `DkimCheck` are misnamed once DMARC enters; rename to `EmailVerificationStatus` and the wire-level `dkim_status` field to `verification_status`. The PoC types are not stable Candid; renaming during the rewrite is free.

---

## 7. Component C — DNSSEC arguments instead of HTTP outcalls

This is the largest architectural change versus the PoC and the foundation that makes the recovery flow practical.

### 7.1 Why move off HTTP outcalls

The PoC's `fetch_dkim_public_key` makes a `https://dns.google/resolve?...` outcall with a `transform_doh_response` function for replica consensus. It works, but:

- It costs ~30B cycles per verification.
- Consensus failure modes are subtle: every replica must see the same JSON, or the call traps.
- We trust dns.google (or whichever DoH provider) to honestly reflect the authoritative response.
- It does not work for the recovery hot path: ingress message size has a 2 MB ceiling and outcall latency is multi-second.
- For the _recovery_ call specifically, whose argument already includes a signed email, having the canister go fetch DNS adds another round trip after the user already had to ship something to it.

The email-recovery stack ends up with **zero HTTP outcalls** — every per-verification check uses caller-supplied material (DNSSEC bundle) or canister-baked-at-deploy material (DNSSEC root anchors, §7.5). See §6.4 for why we don't pull the PSL either.

### 7.2 The DNSSEC-arg pattern

For each DNS record the canister needs (DKIM TXT, DMARC TXT, in the future MX/SPF), the caller supplies the record bytes _plus_ a chain of DNSSEC RRSIGs and DNSKEYs that prove those bytes are what the authoritative DNS published.

A "DNS proof bundle" looks like:

```rust
pub struct DnsProofBundle {
    /// The signed root DNSKEY RRset (every chain in `chains` is verified
    /// up to here). Validated by checking that one of its KSK DNSKEYs
    /// hashes to a DS digest in the trust anchor stored on the canister.
    pub root_dnskey: SignedRRset,

    /// One delegation chain per signing zone touched by this bundle.
    /// Each chain anchors at the root and ends at a zone's DNSKEY
    /// RRset; the verifier validates each chain once and builds a
    /// `(zone_name → DNSKEY RRset)` lookup table reused for every hop.
    ///
    /// The single-zone case (gmail-style direct TXT under one zone)
    /// is just one chain. CNAME indirection across zones (Proton's
    /// `proton.me` → `proton.ch`, Tutanota's `tutanota.com` →
    /// `tutanota.de`, …) supplies one chain per zone touched — see
    /// "DKIM CNAME indirection" below.
    pub chains: Vec<DelegationChain>,

    /// The RRsets being proven, in CNAME-resolution order. Each hop's
    /// RRSIG `signer_name` identifies which zone signed it; the
    /// verifier looks that name up in the table built from `chains`
    /// and validates the RRset under that zone's DNSKEY.
    ///
    /// The two-phase flow uses this in two ways:
    /// - At `prepare_add`: zero or one hop — the DMARC TXT at
    ///   `_dmarc.<d>` if the zone publishes one, omitted otherwise
    ///   (the canister falls through to strict `d=` alignment).
    /// - At `submit_dkim_leaf`: one or more hops forming a coherent
    ///   resolution chain `<sel>._domainkey.<d>` → … → final TXT.
    ///   No CNAME indirection (Gmail-style) → a single TXT hop;
    ///   one CNAME hop (Proton-style) → CNAME + TXT.
    pub hops: Vec<SignedRRset>,
}

pub struct DelegationChain {
    /// Root → leaf. Each link is a parent-zone DS RRset plus the
    /// child-zone DNSKEY RRset; the last link's `child_dnskey` is
    /// the zone whose DNSKEY ends up in the lookup table.
    pub links: Vec<DelegationLink>,
}

pub struct SignedRRset {
    pub name: DnsName,
    pub rtype: u16,            // TXT, DNSKEY, DS, CNAME, ...
    pub rdata: Vec<Vec<u8>>,   // canonical RDATA per RFC 4034 §6
    pub ttl: u32,
    pub rrsig: Rrsig,          // RFC 4034 §3 — `signer_name` names the
                               //  zone whose DNSKEY validates this RRset
}

pub struct DelegationLink {
    pub child_ds: SignedRRset,         // DS RRset in parent
    pub child_dnskey: SignedRRset,     // DNSKEY RRset in child, signed by child's KSK
}
```

**DKIM CNAME indirection — why `hops` is a sequence, not a single leaf.** DKIM has no rule that a DKIM TXT must live in the same zone as the signing domain. Operationally, several mainstream providers implement DKIM by _delegating_ the TXT to a different zone via CNAME — which is a normal part of RFC 6376 resolution and is exactly how mass-mail platforms hand DKIM to their customers without managing per-customer DNS:

- **Same-zone direct TXT** — Gmail (`<sel>._domainkey.gmail.com IN TXT "v=DKIM1; ..."`), iCloud, Outlook (consumer mailboxes that don't use the Microsoft 365 platform), most ESPs configured the DIY way. One hop, one zone.
- **Cross-zone CNAME** — Proton (`<sel>._domainkey.proton.me IN CNAME <sel>._domainkey.<customer-id>.proton.ch`), Tutanota (`<sel>._domainkey.tutanota.com IN CNAME <sel>._domainkey.tuta.de`), most M365-on-custom-domain setups (`<sel>._domainkey.<customer>.com IN CNAME <sel>._domainkey.<customer>.onmicrosoft.com`). Two or more hops, two or more zones.

For DNSSEC verification each hop is _independently signed by its own zone_ — DNSSEC signatures don't span zone boundaries. So a CNAME chain that crosses zones must be presented as a sequence of RRsets, each carrying its zone-local RRSIG, and the verifier needs the DNSKEY chain for _every_ zone touched. That's why the bundle carries `chains: Vec<…>` and `hops: Vec<…>` rather than a single chain and a single leaf — one chain per signing zone, one hop per RRset in the resolution sequence.

The single-zone Gmail case is just `chains.len() == 1` and `hops.len() == 1` — the wire format isn't penalised for not needing CNAME indirection.

### 7.3 Verification algorithm

The trust anchor stored on the canister is a _DS-style digest_ of the IANA root KSK — exactly the same shape IANA publishes at `data.iana.org/root-anchors/root-anchors.xml` (an algorithm + digest-type + hex digest). It is **not** a DNSKEY itself; the root DNSKEY RRset is supplied by the caller and validated against the digest at verification time.

```
verify(bundle, requested_name, requested_type):
    # 1. Validate the root DNSKEY RRset against the bundled trust anchor.
    #    The trust anchor is a DS digest (algo, digest-type, digest-bytes).
    #    Pick the DNSKEY in `bundle.root_dnskey.rdata` whose KSK digest
    #    matches the trust anchor; this is the root KSK we trust this
    #    call. Then verify the root DNSKEY RRset's RRSIG using that KSK.
    root_ksk = pick_dnskey_matching_ds(bundle.root_dnskey.rdata, TRUST_ANCHOR_DS)
    verify_rrsig(bundle.root_dnskey.rrsig, bundle.root_dnskey.rdata, root_ksk)

    # 2. Walk every supplied delegation chain and build a
    #    (zone_name → DNSKEY RRset) lookup table.
    zone_keys = { ".": bundle.root_dnskey.rdata }
    for chain in bundle.chains:
        parent_keys = bundle.root_dnskey.rdata
        for link in chain.links:
            # Parent's DS RRset is signed by the parent's DNSKEY.
            verify_rrsig(link.child_ds.rrsig, link.child_ds.rdata, parent_keys)
            # Parent's DS digest matches one of the child's KSK DNSKEYs.
            # We don't keep `child_ksk` after this — it's enough to know
            # the DS pinned *some* KSK in the RRset; see below.
            require pick_dnskey_matching_ds(link.child_dnskey.rdata, link.child_ds).is_some()
            # Child's DNSKEY RRset is signed by *some* DNSKEY in the
            # RRset (typically the ZSK self-signing the KSK + its own
            # entry, but the standard also allows KSK-only signing —
            # operators differ). We accept any RRSIG whose `key_tag`
            # matches a DNSKEY in `link.child_dnskey.rdata`.
            verify_rrsig_under_dnskey_rrset(link.child_dnskey.rrsig, link.child_dnskey.rdata)
            parent_keys = link.child_dnskey.rdata
        # The last link's child_dnskey owner is the zone we just landed in.
        zone_name = chain.links.last().child_dnskey.name
        require zone_keys.insert(zone_name, parent_keys).is_none()  # reject duplicate zones

    # 3. Verify every hop under the zone its RRSIG names.
    for hop in bundle.hops:
        zone = hop.rrsig.signer_name
        require zone in zone_keys                      # bundle must include
                                                       #  the chain for this zone
        require hop.name.is_subdomain_of(zone)          # owner lies in that zone
        verify_rrsig(hop.rrsig, hop.rdata, zone_keys[zone])

    # 4. Validate the hop sequence forms a coherent CNAME → … → requested_type
    #    resolution. Reject loops, dangling chains, and oversize bundles.
    require bundle.hops.len() >= 1
    require bundle.hops.len() <= MAX_CNAME_HOPS         # cap, defaults to 4
    require bundle.hops[0].name == requested_name
    seen_names = { bundle.hops[0].name }
    for i in 0 .. bundle.hops.len() - 1:
        require bundle.hops[i].rtype == CNAME
        require bundle.hops[i].rdata.len() == 1         # CNAME RRset has exactly one target
        target = bundle.hops[i].rdata[0].canonical_target_name()
        require bundle.hops[i+1].name == target
        require seen_names.insert(target)               # no loops
    require bundle.hops.last().rtype == requested_type

    # 5. Freshness — every RRSIG's [inception, expiration] window must
    #    contain the verifier's clock (±clock_skew).
    now = ic_cdk::time()
    for rrsig in all_rrsigs(bundle):
        require rrsig.inception <= now + clock_skew
        require rrsig.expiration >= now - clock_skew
```

`verify_rrsig` uses RFC 4034 §3.1.8.1 canonical form (lowercase owner names, RDATA in canonical order) and the algorithm code from the RRSIG. We support algorithms 8 (RSA-SHA256), 13 (ECDSA-P256-SHA256), and 15 (Ed25519) — RFC 8624 "MUST" implementations. Anything older (RSA-SHA1, RSA-MD5) is rejected.

**Why "any DNSKEY in the RRset" is enough for the DNSKEY self-signature.** The chain of trust is still pinned by the DS step above — `pick_dnskey_matching_ds` proves that the parent's DS digest matches a KSK present in the child's DNSKEY RRset, so the RRset as a whole is authenticated by the parent. Once that's true, accepting any DNSKEY-set member as the validator of the DNSKEY RRset's RRSIG is equivalent to what production DNSSEC validators do: the standard permits the DNSKEY RRset to be signed by either the KSK alone, the ZSK alone, or both, and operator practice is split (Proton signs DNSKEY with the ZSK; Cloudflare signs with the KSK). Pinning to the KSK refused otherwise-valid Proton bundles for no security gain.

**Why per-hop signer-name validation matters.** The `RRSIG.signer_name` field is signed _as part of the RRSIG RDATA itself_ (RFC 4034 §3.1) — an attacker cannot rewrite it without invalidating the signature. Trusting `signer_name` to pick the validating DNSKEY is therefore safe and is exactly what every existing DNSSEC validator does. The canister additionally checks that the hop's owner name is in-zone of the named signer, defending against a curiosity case where an attacker who controls one zone tries to "claim" they signed an RRset from a different zone.

**Why the CNAME chain coherence checks.** Each hop is independently authenticated, but a buggy or malicious caller could still ship a _valid_ set of signatures whose owner names don't actually form a CNAME chain ending at the requested type — for example, three legitimately-signed CNAMEs that happen to all exist but don't link to each other. The owner-target equality check on consecutive hops, the no-loops invariant, and the final-hop type check together ensure that the bundle is exactly "what a recursive resolver would have followed if it asked for `requested_name`/`requested_type`".

**Why a hop cap.** `MAX_CNAME_HOPS = 4` is generous: real-world DKIM CNAME chains observed across providers max out at two (`<sel>._domainkey.<customer>.com → <sel>._domainkey.<customer>.onmicrosoft.com → <sel>._domainkey.outlook-something.com`). The cap prevents an oversized bundle from arriving at the canister.

### 7.4 Caller-side bundle assembly

The browser assembles `DnsProofBundle` directly in TypeScript. No separate library or WASM module is required.

**Pre-email — skeleton chain.** At `prepare` time the FE submits a `DnsProofBundle` covering everything _except_ the DKIM leaf:

- Issue DoH queries with `do=1` (request DNSSEC) and `cd=1` (don't validate, give us raw RRSIGs) to a public resolver. `dns.google` and `cloudflare-dns.com` both expose this; the FE can fall back between them. Browsers without OS-level DNS APIs cannot reach the authoritative servers directly, so DoH is the practical path.
- Walk the delegation chain for the registered domain by querying the DS RRset at each parent zone (root → TLD → registered domain). Stop at root, where the DNSKEY RRset is validated against the trust anchor stored on the canister rather than against another DS lookup. This populates `chains[0]`.
- Include the DMARC TXT leaf at `_dmarc.<domain>` (a well-known name) if the zone publishes one; omit otherwise (the canister falls through to strict DMARC alignment when no record is published).
- _Do not_ fetch any DKIM leaf — the active selector lives only in the eventual email's `DKIM-Signature` header.

**Post-email — DKIM leaf walk with CNAME following.** When polling sees `NeedDkimLeaf { selector }`, the FE walks `<selector>._domainkey.<domain>` and any CNAMEs in between:

```
walk_dkim(name):
    bundle.hops = []
    visited = {}
    while True:
        require name not in visited; visited.add(name)
        rrset = doh.fetch_signed(name, "ANY-DKIM-OR-CNAME")  # CNAME or TXT
        bundle.hops.push(rrset)
        if rrset.rtype == TXT: break
        if rrset.rtype == CNAME: name = rrset.target; continue
    # Ensure each zone touched by a hop has a chain in the bundle.
    needed_zones = { hop.rrsig.signer_name for hop in bundle.hops }
    for zone in needed_zones:
        if zone not in already_chained_at_prepare:
            bundle.chains.push(walk_delegation_to(zone))
```

Concretely:

- **Same-zone case (Gmail, iCloud, …).** One DoH query for the TXT, no extra chain — the apex zone walked at prepare already covers it. Bundle: 1 hop, 0 new chains.
- **Cross-zone CNAME (Proton, Tutanota, M365 custom domain, …).** Two DoH queries (CNAME then TXT) plus a delegation walk for the second zone. Bundle: 2 hops, 1 new chain.
- **CNAME landing in unsigned territory.** The TXT (or an intermediate CNAME) lives in a zone that doesn't publish DNSSEC — DoH returns the record but with no RRSIGs. The FE cannot complete the bundle; the canister therefore needs the DoH-allowlist fallback (§7.6) to handle this domain at all. We surface this clearly: the FE detects the missing-RRSIG case while walking, abandons the DNSSEC path, and submits the prepare without a `dns_proof` so the canister either takes the DoH path or rejects with `DomainNotSupported`.

The chain anchored at `prepare` time covers the apex zone; if the DKIM resolution stays in that zone (Gmail-style) the `submit_dkim_leaf` bundle just carries the leaf hop. If the resolution crosses into another zone (Proton-style) the FE adds that zone's delegation chain to `bundle.chains`. The canister merges those new chains into its cached `(zone_name → DNSKEY)` map (§8.2) before validating the hops.

The earlier draft mentioned "the OS resolver" as a fallback when DoH providers are unavailable; in practice browsers don't expose the OS resolver, so the practical fallback is between multiple DoH endpoints, not to the OS. Domains for which DoH refuses to return RRSIGs are treated the same as domains without DNSSEC (§7.6).

The TS module lives in `src/frontend/src/lib/utils/dnssec/` (~300 lines, no dependencies beyond `fetch`). There is no separate WASM module; the canister already has a Rust DNSSEC verifier and the FE only needs to _gather_ the records, not verify them.

**Why two phases instead of one.** A DKIM-signed email carries exactly one signature over exactly one selector — `s=` in the `DKIM-Signature` header. DKIM has no enumeration mechanism: there's no DNS record listing the active selectors, NSEC walking is defeated by NSEC3 on most modern zones, and probing a candidate list is a heuristic that misses esoteric custom selectors. The cleanest source of truth is the email itself — once it arrives, the canister parses `s=` and tells the FE which leaf to fetch. This eliminates selector probing entirely; the selector is authoritative, not guessed.

### 7.5 Root anchor management — deploy-arg, not bundled

The trust anchor (the DS digest of the IANA root KSK) lives in the canister's persistent state, set on every deploy via the `init`/`post_upgrade` arg:

```candid
type DnssecConfig = record {
    root_anchors : vec record {
        // Per `data.iana.org/root-anchors/root-anchors.xml`.
        key_tag      : nat16;
        algorithm    : nat8;       // 8 / 13 / 15
        digest_type  : nat8;       // 1 (SHA-1) or 2 (SHA-256). We only ship type 2.
        digest       : blob;       // 32 bytes for SHA-256
    };
};
```

The config sits at the top of `InternetIdentityInit` as `dnssec_config: Option<DnssecConfig>`; it isn't email-recovery-specific. Any future feature that needs DNSSEC-verified DNS (DANE, ACME, MX-pin, …) consumes the same trust anchors.

Multiple anchors are accepted simultaneously to make rollover trivial — during a key transition both the retiring and the incoming KSK digests live in `root_anchors`. This is the same shape IANA publishes when both old and new KSKs are valid.

II is deployed at least weekly, so refreshing the anchor list on every deploy is essentially free; we don't need a separate governance mechanism for it.

**Rollover frequency.** In practice the IANA root KSK rolls _very rarely_: once in DNSSEC's history, in October 2018 (the "KSK rollover from 2010 to 2017 KSK"). No further rollover has happened or is publicly scheduled at the time of writing. IANA publishes signed announcements months in advance when one is upcoming. We keep the deploy-arg shape so that when the next rollover does happen (announced anchor publication updates), it's a one-line config change in the next weekly deploy rather than a code change.

The currently configured anchor list is recoverable from the canister's last upgrade arg via the IC management canister; we do not expose a separate auditing endpoint for it.

### 7.6 Domains without DNSSEC: the DoH fallback

The major consumer mailbox providers we most care about — Gmail, Outlook, iCloud, Yahoo — do **not** publish DNSSEC end-to-end on their DKIM resolution paths. Rejecting them outright would leave the feature unusable for the majority of real users, so for an explicitly allowlisted set of domains the canister falls back to fetching DKIM/DMARC TXT records via DoH HTTP outcalls, with a multi-provider quorum standing in for the cryptographic provenance DNSSEC would otherwise give us. The DoH module is described in `crate::doh` (PR 4 of this stack); the relevant properties for this design are:

- **Allowlist-gated.** Outcalls fire only for domains in `DohConfig.allowed_domains` (deploy/upgrade arg). Non-allowlisted, non-DNSSEC domains still fail closed.
- **Multi-provider quorum.** Five DoH providers across four jurisdictions (Cloudflare 🇺🇸, Google 🇺🇸, Quad9 🇨🇭, CIRA Canadian Shield 🇨🇦, IIJ 🇯🇵), 3-of-5 strict-majority quorum on the TXT bytes, no single jurisdiction can reach quorum alone. We trust "at least three of these providers, run by independent operators in different legal regimes, all return the same bytes" as a workable substitute for "the IANA-rooted DNSSEC chain".
- **Heap cache + dedup.** Successful fetches stay in cache for the configured TTL (default 1 h, capped at 24 h); concurrent fetches for the same FQDN collapse to a single outcall fan-out via a hand-rolled Waker primitive.
- **No outcalls in the latency-sensitive path for repeat traffic.** The first email per provider per TTL window pays the outcall cost; subsequent emails are cache hits.

This means the email-recovery stack ends up with **two parallel verification paths** to the same `dkim::verify` + `dmarc::verify_email` core:

| Path          | Caller-supplied input (prepare)                                             | Caller-supplied input (post-email)                 | DKIM key sourced from                                                                             | When the outcall (if any) happens                                          |
| ------------- | --------------------------------------------------------------------------- | -------------------------------------------------- | ------------------------------------------------------------------------------------------------- | -------------------------------------------------------------------------- |
| DNSSEC        | Skeleton `DnsProofBundle` (DNSKEY chain to IANA root + optional DMARC leaf) | DKIM leaf RRset for the selector seen in the email | The FE-supplied DKIM TXT, validated against the cached chain                                      | None — verification is fully sync                                          |
| DoH allowlist | Just the address (no bundle)                                                | —                                                  | `doh::fetch_txt(selector._domainkey.<domain>, registered_domain)` once `s=` is read off the email | At verification time on the first uncached lookup; cache-served thereafter |

The canister picks the path per-call: if the FE supplies a `DnsProofBundle`, use the DNSSEC path; otherwise, if the registered domain is on the DoH allowlist, use the DoH path; otherwise reject with a clear error and the copy-able message:

> "Internet Identity cannot verify mail from `example.com` — that domain doesn't publish DNSSEC and isn't on our DoH allowlist. Try a different email address from a supported provider, or use a recovery phrase instead."

A small helper page (linked from the wizard's error state) lists the supported providers and their verification path.

**"DNSSEC at the apex" is not enough — what matters is end-to-end coverage of the DKIM resolution path.** A domain may publish a DS at its registered apex and still be unverifiable end-to-end if its DKIM TXT lives behind a CNAME that targets unsigned territory. The canonical example is **`live.com`**: the apex zone is signed (`dig DS live.com` returns a record), but DKIM at `<sel>._domainkey.live.com` resolves through CNAMEs into the unsigned `protection.outlook.com` zone, where the actual TXT lives. The DNSSEC chain breaks at the cross-zone CNAME — the FE walker sees a hop with no RRSIG and abandons the bundle. So `live.com` is on the DoH allowlist _despite_ having an apex DS, because that DS doesn't prove anything about the records the verifier actually needs.

The audit that decides whether a domain belongs on the allowlist is therefore:

1. Resolve `<some-active-selector>._domainkey.<d>` end-to-end via a DNSSEC-validating resolver.
2. Check the AD bit on the response — `true` means the resolver validated every hop in the CNAME chain.
3. If `AD=0` even though the apex publishes DS, the CNAME chain has crossed into unsigned territory; this domain belongs on the DoH allowlist.

Concretely, the categories observed across mainstream consumer mailbox providers (audit performed at the time of writing; revisited per release):

| Category                                              | Examples                                                                                                                                                | Verification path                            |
| ----------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------- | -------------------------------------------- |
| Apex unsigned, DKIM unsigned                          | `gmail.com`, `outlook.com`, `hotmail.com`, `icloud.com`, `me.com`, `yahoo.com`, `aol.com`, `yandex.com`, `mail.ru`, `qq.com`, `163.com`, `naver.com`, … | DoH allowlist                                |
| Apex signed but DKIM CNAMEs into unsigned territory   | `live.com` (CNAME → `protection.outlook.com`, unsigned), some M365-on-custom-domain setups                                                              | DoH allowlist                                |
| Apex signed, DKIM stays in same zone                  | `protonmail.com`, `pm.me`                                                                                                                               | DNSSEC, single chain, single hop             |
| Apex signed, DKIM CNAMEs to a _different signed_ zone | `proton.me` → `proton.ch`, `tutanota.com` → `tutanota.de`                                                                                               | DNSSEC, multi-zone bundle (2 chains, 2 hops) |

The FE walker does not need to know which category a domain falls into in advance — it tries the DNSSEC path, abandons on the first missing-RRSIG hop, and submits a no-bundle prepare. The canister then either takes the DoH path (if the domain is allowlisted) or rejects with `DomainNotSupported`. Operators audit allowlist additions using the procedure above; the resulting list is shipped via `default-doh-domains.bash` (sourced by `deploy-common.bash` and `make-upgrade-proposal`, single source of truth across deploy and upgrade-proposal flows).

### 7.7 Replay and freshness

DNSSEC RRSIGs have inception and expiration timestamps but the typical validity window is days to weeks — too coarse for our needs. We add two more layers:

- **DKIM `x=`** — mandatory ceiling enforced by §5.4.
- **Per-flow recovery nonce** — issued by the canister at the start of a registration or recovery attempt, included in the `Subject:` header of the challenge email, single-use, valid for 30 minutes. A replayed signed email contains a stale or already-burned nonce and is rejected.

---

## 8. Component D — Email recovery as an authn method

Email recovery shares the most code with the OpenID flow (`src/internet_identity/src/openid/`): both link an external identity to an anchor, both have a verification step, both produce a delegation. The natural shape is to add an `EmailRecoveryCredential` peer to `OpenIdCredential`.

### 8.1 The user-visible flow

The whole feature, both setup and recovery, fits in three steps the user sees:

1. **Enter your email.** The FE asks for the address. It tries to assemble a _skeleton_ `DnsProofBundle` (DNSSEC chain to root + the `_dmarc.<domain>` leaf if published) and submits it; if the domain doesn't publish DNSSEC, the bundle is omitted and the canister falls back to its DoH allowlist (§7.6) — that fallback is invisible to the FE. For recovery the FE also generates a fresh ECDSA keypair locally and sends the public key as part of the same call — that key is what the eventual delegation will be bound to.
2. **Send a confirmation email.** The canister issues a one-line token (e.g. `II-Recovery-1a2b3c4d5e6f7081`); the FE asks the user to send a fresh email with that token in `Subject:` to `recover@<host>` (or `register@<host>` for setup), where `<host>` is `window.location.hostname` — the alias matching the origin the user is already on. The canister accepts mail at any of the configured `related_origins` aliases; the FE just shows the one that matches the current tab.
3. **Walk the DKIM leaf.** Once the email arrives at the canister, the polled status flips to `NeedDkimLeaf { selector }` — the canister has read `s=` from the DKIM-Signature header. The FE walks DNSSEC for that selector and submits the leaf via `email_recovery_submit_dkim_leaf`. The canister completes verification and the polling spinner flips to either "all set" (setup) or "signed in" (recovery, with the delegation it can use immediately). The user sees only the spinner during this step — they don't have to do anything extra.

The shape is two canister calls (prepare + submit_dkim_leaf), with polling in between:

- `email_recovery_credential_prepare_add(anchor, dns_input)` / `email_recovery_prepare_delegation(dns_input, session_pk)` — the FE submits the flat `EmailRecoveryDnsInput` (`{ address, dns_proof: opt DnsProofBundle }`), plus, for recovery, the FE-generated session public key. The **canister** picks the path: if `dns_proof` is set, validate the (skeleton) chain synchronously and cache the validated zone DNSKEYs + the DMARC TXT bytes if included; otherwise check the registered domain against `DohConfig.allowed_domains` and defer the DKIM/DMARC fetch to `smtp_request` time. The FE doesn't need to know which domains are on the allowlist. Returns `{ nonce, expires_at }` — no mailbox label; the FE renders that locally from `window.location.hostname` (see §8.3).
- The gateway calls `smtp_request(SmtpRequest)` when the email arrives — exactly the PoC's surface, unchanged. The canister extracts the nonce from the DKIM-signed `Subject:`, looks up the pending challenge, parses the DKIM-Signature header (without yet having the public key), verifies `bh=` against the canonicalised body and drops the body, computes the SHA-256 of the canonical signed-headers input, and stashes the partial-verification record. On the **DNSSEC path** it then sets the status to `NeedDkimLeaf { selector }` and waits for the FE. On the **DoH path** it directly resolves the DKIM and DMARC TXTs (typically cache hits after the first email per provider per TTL window), completes verification, and either binds the address (setup) or stamps a delegation seed for the cached `session_pk` (recovery).
- `email_recovery_submit_dkim_leaf(nonce, hops, extra_chains)` — DNSSEC-path only. The FE supplies the DKIM resolution as a sequence of signed `hops` (CNAME → … → final TXT) for the selector seen in step 2's status, plus any `extra_chains` for new signed zones the resolution crosses into (see §7.2 — Gmail-style is one TXT hop and no extra chains; Proton/Tutanota-style is CNAME + TXT plus the second zone's chain). The canister merges new chains into the cached `(zone → DNSKEY)` map, verifies each hop under the zone its RRSIG names, runs `RSA.verify(pk, headers_digest, signature_blob)` on the final TXT's public key, completes DMARC alignment + `From:` match + `Subject` in `h=`, and finalises.
- The FE polls `email_recovery_status(nonce)` between calls. Terminal states are `RegistrationSucceeded` / `RecoveryReady` / `Failed` / `Expired`. Recovery flows then call `email_recovery_get_delegation(...)` (a query) for the SignedDelegation; setup flows just show "all set".

This shape gives us:

- Heavy work (DNSSEC chain validation, where applicable) happens once, up front, before the user has to do anything irreversible.
- Errors are localized: a domain whose DNSSEC chain doesn't validate, or whose DMARC policy is missing, or that isn't on the DoH allowlist, fails at step 1 with a clear message ("we can't accept email from `example.com` — see §7.6"). The user finds out _before_ sending an email.
- The browser never re-uploads a multi-KB email — the gateway hands the raw bytes directly to the canister.
- One nonce per challenge, one challenge per nonce. The nonce is the only identifier — both the human-typeable string the user types into the email's `Subject:`, and the lookup key the FE polls and the canister uses to match the inbound email. There is no separate `challenge_id`.

### 8.2 Storage model

```rust
#[derive(Clone, Debug, candid::CandidType, candid::Deserialize, minicbor::Encode, minicbor::Decode)]
pub struct EmailRecoveryCredential {
    /// Lowercased canonical form: `lowercase(local-part) + "@" + lowercase(domain)`.
    /// Stored verbatim (not hashed) so the user can see it back in the
    /// management UI; it's exactly what they typed at registration.
    /// Bounded by RFC 5321 §4.5.3.1: local-part ≤ 64 bytes, domain ≤
    /// 255 bytes, addr-spec ≤ 254 bytes. The same caps are enforced
    /// at every address-handling boundary (`prepare_add` validation,
    /// verified-`From:` extraction at `smtp_request` time).
    #[n(0)]
    pub address: String,

    /// Unix-seconds.
    #[n(1)]
    pub created_at: u64,
    #[n(2)]
    pub last_used: Option<u64>,
}
```

The credential lives **on the anchor struct itself**, not in a separate stable map:

```rust
// inside Anchor (storage::anchor::Anchor)
//
// In the storage form (StorableAnchor), wrapped in `Option<Vec<...>>`
// so anchors written under the previous schema decode cleanly as None
// — same pattern as the existing `passkey_credentials` and
// `recovery_keys` fields.
#[n(<next-free-index>)]
pub email_recovery: Option<Vec<StorableEmailRecoveryCredential>>,
```

Anchor storage already uses `minicbor-derive`, which is forward/backward compatible across optional-field additions out of the box — old anchors deserialize with the field absent. The exposed in-memory representation collapses the `Option` away into a plain `Vec` so callers iterate uniformly: empty vec = no credential bound, single element = one credential bound.

**Why a `Vec` if v1 only ever holds one entry.** The v1 API caps `email_recovery` at one entry per anchor (see "v1 API invariants" below). The data model is widened to a `Vec` anyway so a future iteration that lets a user bind multiple recovery emails can land without another schema migration. Today the `Vec` is structurally never longer than one; tomorrow we can lift the cap with a pure API-layer change.

Surface for the FE: `IdentityInfo` (returned by `identity_info`) carries a flat `email_recovery: opt EmailRecoveryCredential` field, populated as `anchor.email_recovery.first().cloned()`. The manage-page card renders directly off it without a second canister call. When the v1 cap is lifted, this field becomes a `vec` and the FE renders multiple cards.

One additional stable map exists alongside this:

- **Reverse address index**, memory ID 23 (next free): `SHA-256(lowercase(address)) → AnchorNumber`. Used at recovery time to resolve a verified `From:` to an anchor. Required because we can't efficiently scan all anchors to find one bound to a given address. The address itself already lives on the anchor (`Anchor.email_recovery.address`), so there is no reason to store it in the index too — the index just answers "which anchor does this address belong to?". A fixed-size hashed key keeps the index entry bounded by construction (32 bytes regardless of address length) and removes one variable-length-key surface from stable memory. Per §3.1, the lookup is gated by DKIM and is enumerable only to attackers who already control the queried mailbox, so the hash is unsalted.

**v1 API invariants** (enforced in canister code, not in storage):

- _One verified email per anchor._ A second `email_recovery_credential_prepare_add` for an anchor that already has a verified email is allowed; on `smtp_request` success the canister atomically drops the previously registered address and writes the new one. The replace happens at _verification time_, not at prepare time — so a user who starts a swap and abandons the wizard mid-flow keeps their existing recovery channel. No separate "remove first, then add" UX.
- _One anchor per address._ `smtp_request` rejects a register-flow email with `AddressAlreadyRegistered` if the verified `From:` is already bound to a different anchor. If bound to the caller's own anchor (re-confirm) the call is a no-op success.

A third, ephemeral map holds _pending challenges_ keyed by `nonce`. Each entry carries:

- `kind`: `Register { anchor }` or `Recover { session_pk }`,
- the claimed lowercased address,
- on the **DNSSEC path**: the validated `(zone_name → DNSKEY RRset)` map populated from the skeleton chain, and the verified DMARC TXT bytes when the FE included a DMARC leaf in the bundle. Stored as raw bytes; parsing happens at `submit_dkim_leaf` time. When the DMARC TXT is absent the canister falls back to strict `d=` ↔ `From:` domain alignment per §6.3. The map starts with one zone (the registered apex) for Gmail-style domains and grows at `submit_dkim_leaf` time when the DKIM resolution crosses into a different signed zone (Proton/Tutanota style — see §7.2). Bounded in size by `MAX_CHAINED_ZONES_PER_PENDING = 4`, which is more than any observed real-world DKIM CNAME chain needs.
- on the **DoH path**: nothing extra — both DKIM and DMARC TXTs are fetched via `doh::fetch_txt` at `smtp_request` time (cache-served after the first lookup per provider per TTL window),
- once `smtp_request` has run on a DNSSEC-path entry: a **partial-verification record** holding `{ selector, signing_domain, headers_digest (32 B), signature_blob, from_domain, claimed_address_lc }` — together ~500 B per entry. This is the minimum needed to complete DKIM signature verification once the FE submits the leaf with the public key. The body itself is dropped after `bh=` validates,
- a `status: Pending | NeedDkimLeaf { selector } | Succeeded { outcome } | Failed { error }`,
- a 30-minute expiry.

Entries flip from `Pending` to `NeedDkimLeaf` on `smtp_request` (DNSSEC path only), then to `Succeeded`/`Failed` on `email_recovery_submit_dkim_leaf`. The DoH path goes straight from `Pending` to `Succeeded`/`Failed` on `smtp_request` because the canister can fetch the DKIM TXT itself. They are dropped on poll-after-expiry, on terminal status read, or on TTL eviction. Held in a heap-only `thread_local!` `HashMap<nonce, PendingChallenge>` — _not_ in stable memory. The map is ephemeral by design: an upgrade in flight cancels every in-flight challenge, which is fine for a 30-minute-TTL flow that the user re-runs from scratch on retry. Bounded above by `MAX_PENDING_CHALLENGES` plus the TTL; oldest entries evict first when the cap is hit (§8.9).

We do **not** store any inbound email body, header bytes, or DKIM verification artefacts past the moment the challenge reaches a terminal state. The cached `session_pk` for recovery is the only piece that survives between prepare and the eventual delegation issuance.

**Archive operations.** Anchor-mutating operations land in II's existing audit log via two new `Operation` variants alongside the existing OpenID/passkey kinds: `AddEmailRecovery` (emitted from `email_recovery_submit_dkim_leaf` on a successful setup) and `RemoveEmailRecovery` (emitted from `email_recovery_credential_remove` and from the implicit replace path in `smtp_request`). Both are payload-free. We deliberately do _not_ carry the address — emitting the local-part, the domain, or even just the provider would let an archive consumer correlate anchors back to mailbox providers in aggregate, which is exactly the cross-issuer correlation §3.1 already pushes against. The standard `(anchor, timestamp, caller)` triple already in every audit entry is enough to answer "who changed their recovery email when?" without leaking the address itself.

### 8.3 Candid surface

```candid
// Returned by every prepare_* call. The nonce uniquely identifies the
// challenge — both as the human-typeable string the user types into the
// email's Subject:, and as the canister-side lookup key. We don't carry
// a separate challenge_id.
//
// The recipient mailbox is **not** carried on this struct. The FE
// renders the user-facing label by pairing the user-part
// (`register` / `recover`) with `window.location.hostname` — each
// tab automatically shows the alias matching the origin the user is
// already on. The canister accepts mail at `register@<h>` /
// `recover@<h>` for any host `<h>` listed in the `related_origins`
// deploy arg (see §7.5 / §8.4 for the dispatch side); they're equal
// aliases, so the canister never has to single one out as canonical.
type EmailRecoveryChallenge = record {
    nonce      : text;       // e.g. "II-Recovery-1a2b3c4d5e6f7081"; canister keys
                             //  pending challenges by this value, and the
                             //  user puts it verbatim in the Subject:
                             //  header of the email they send.
    expires_at : Timestamp;  // nanoseconds since the Unix epoch
                             //  (matches II's `Timestamp` convention);
                             //  30 minutes after issue.
};

// What the FE submits at prepare time. Single flat shape — the
// **canister** picks the verification path, not the FE:
//
//   - if `dns_proof` is supplied, the canister takes the DNSSEC path:
//     validates the (skeleton) chain synchronously and caches the
//     validated zone DNSKEYs on the pending challenge for the
//     post-email leaf submission to anchor against. The DMARC leaf,
//     if included, is also validated and its TXT bytes cached for
//     verification time.
//   - otherwise, the canister checks the registered domain against
//     `DohConfig.allowed_domains` (deploy-arg) and resolves the DKIM
//     and DMARC TXTs via `crate::doh::fetch_txt` at `smtp_request`
//     time (3-of-5 provider quorum + heap cache).
//   - if neither path applies, prepare returns `DomainNotSupported`.
//
// The FE deliberately **doesn't** need to know which domains are on
// the DoH allowlist — that's operator config, not user-visible state.
// The FE submits a skeleton bundle when DNSSEC is available; the
// DKIM leaf comes later via `email_recovery_submit_dkim_leaf`.
//
// `selector` is intentionally absent — it's read off the eventual
// email's `DKIM-Signature` header on arrival and surfaced back to the
// FE via the `NeedDkimLeaf` status variant.
type EmailRecoveryDnsInput = record {
    address    : text;                      // lowercase canonical form
    dns_proof  : opt DnsProofBundle;        // present iff the FE could
                                            //  walk the DNSSEC chain
                                            //  for the registered
                                            //  domain. Carries DNSKEYs
                                            //  to root + optional
                                            //  DMARC leaf, no DKIM
                                            //  leaf (selector unknown
                                            //  pre-email).
};

type EmailRecoveryError = variant {
    Unauthorized              : principal;
    NonceUnknown;                              // no pending challenge by that nonce
    NonceExpired;
    DomainNotAllowlisted      : text;          // DoH path requested for a non-allowlisted domain (§7.6)
    DohFetchFailed            : text;          // DoH path: doh::fetch_txt did not reach quorum
    DomainNotSupported        : text;          // covers "no DNSSEC and not on DoH allowlist"
    EmailVerificationFailed   : VerificationStatus;
    AddressMismatch;                           // From: did not match
    SubjectNotSigned;                          // h= didn't include Subject (§5.4)
    AddressAlreadyRegistered;                  // address already bound to a different anchor
    AddressNotRegistered;
    DkimLeafMismatch;                          // submitted leaf was for a different selector or
                                               //  failed validation against the cached chain
    InternalCanisterError     : text;
};

// Polling result.
//
// `NeedDkimLeaf` is the post-email-arrival state on the DNSSEC path:
// the canister has parsed `s=` from the DKIM-Signature header and is
// waiting for the FE to walk that selector's DKIM TXT and submit it
// via `email_recovery_submit_dkim_leaf`. The DoH-allowlist path skips
// this state (the canister fetches the DKIM TXT itself), going
// straight from `Pending` to `RegistrationSucceeded`/`RecoveryReady`.
type EmailRecoveryStatus = variant {
    Pending;
    NeedDkimLeaf : record { selector : text };                          // DNSSEC path, post-email
    RegistrationSucceeded;                                              // setup done
    RecoveryReady : record { user_key       : UserKey;                  // recovery ready
                             expiration     : Timestamp;
                             anchor_number  : IdentityNumber };         // anchor the verified
                                                                        //  From: resolved to,
                                                                        //  surfaced so the FE
                                                                        //  doesn't need a
                                                                        //  separate lookup hop
    Failed   : EmailRecoveryError;
    Expired;
};

service : {
    // ---------- Email-recovery feature methods ----------
    //
    // These follow the same naming convention as the OpenID surface
    // already in `main.rs` (`openid_credential_add`, `openid_prepare_delegation`,
    // `openid_get_delegation`, `openid_credential_remove`).

    // Setup: caller is the authenticated identity. Validate DNS, cache
    // verified key + policy, return a challenge bound to (anchor, address).
    email_recovery_credential_prepare_add :
        (IdentityNumber, EmailRecoveryDnsInput)
        -> (variant { Ok : EmailRecoveryChallenge; Err : EmailRecoveryError });

    // Recovery: anonymous. Same as setup-prepare plus session_pk that the
    // eventual delegation will be bound to. The anchor is resolved later
    // from the verified From: of the email.
    email_recovery_prepare_delegation :
        (EmailRecoveryDnsInput, SessionKey)
        -> (variant { Ok : EmailRecoveryChallenge; Err : EmailRecoveryError });

    // FE polls this while the user is sending the email. The argument is
    // the nonce returned at prepare time. Once Succeeded* the FE acts on it.
    //
    // On the DNSSEC path the FE will also see `NeedDkimLeaf { selector }`
    // — the cue to walk the DKIM TXT for that selector and call
    // `email_recovery_submit_dkim_leaf`.
    email_recovery_status :
        (text)
        -> (EmailRecoveryStatus) query;

    // DNSSEC path only — call after polling sees `NeedDkimLeaf`. The
    // FE supplies the DKIM resolution as a bundle of `hops` (one or
    // more SignedRRsets forming the CNAME chain `<sel>._domainkey.<d>`
    // → … → final TXT) plus any *additional* delegation `chains`
    // needed for zones that weren't already covered by the skeleton
    // chain anchored at prepare time — the cross-zone CNAME case
    // (Proton, Tutanota, M365 custom domains; see §7.2). For
    // Gmail-style same-zone resolution the FE supplies a single
    // TXT hop and an empty `chains` vec.
    //
    // The canister:
    // 1. validates each new chain against the cached root DNSKEY
    //    and merges the resulting (zone → DNSKEY) entries into the
    //    map cached at prepare time;
    // 2. verifies each hop under the zone its RRSIG names;
    // 3. checks the hop sequence is a coherent CNAME → … → TXT
    //    resolution starting at `<sel>._domainkey.<d>`;
    // 4. completes DKIM signature verification using the public
    //    key parsed from the final TXT against the headers digest
    //    cached at email-arrival.
    //
    // State transitions to RegistrationSucceeded / RecoveryReady on
    // success, Failed on validation/DKIM error.
    email_recovery_submit_dkim_leaf :
        (record {
            nonce      : text;
            hops       : vec SignedRRset;
            extra_chains : vec DelegationChain;       // empty vec for the
                                                      //  Gmail-style case
        })
        -> (variant { Ok; Err : EmailRecoveryError });

    // After RecoveryReady, the FE fetches the SignedDelegation. The
    // session_key + expiration must match what was stored at prepare time.
    email_recovery_get_delegation :
        (record { nonce : text; session_key : SessionKey; expiration : Timestamp })
        -> (variant { Ok : SignedDelegation; Err : EmailRecoveryError }) query;

    // Remove a registered recovery address.
    email_recovery_credential_remove :
        (IdentityNumber, text)
        -> (variant { Ok; Err : EmailRecoveryError });

    // ---------- SMTP gateway protocol ----------
    //
    // Carried forward from PoC #3760 unchanged: the SMTP gateway calls this
    // for every inbound message, supplying the envelope (To/From) and the
    // raw message. The canister inspects the recipient address to dispatch:
    //
    //   register@<h>  → email-recovery setup completion
    //   recover@<h>   → email-recovery delegation completion
    //
    // for any host `<h>` listed in the `related_origins` deploy arg (on prod
    // typically `id.ai` plus the `*.icp0.io` aliases; on beta `beta.id.ai`).
    // All entries are accepted as equal aliases — the same WASM works against
    // whichever zone the off-chain gateway routes mail for, and a multi-domain
    // prod deploy doesn't have to single one out as canonical. No id is
    // encoded in the address: the canister looks up the pending challenge by
    // the nonce found in the email's DKIM-signed Subject header, then
    // dispatches based on the kind stored under that nonce.
    //
    // The signature mirrors the PoC's gateway-protocol Candid (SmtpRequest /
    // SmtpResponse types defined in
    // `src/internet_identity_interface/src/internet_identity/types/smtp.rs`).
    // Open call: anyone can submit, but a request only causes state changes
    // if it carries a DKIM-valid email matching a known pending nonce.
    //
    // `smtp_request_validate` is the query-mode counterpart the gateway
    // calls at SMTP `RCPT TO` time — *before* it pulls the message body
    // from the sending MTA — to decide whether to accept the connection
    // at all. Returns `Ok` for the two recipients we handle (`register`
    // / `recover` paired with any `related_origins` host) and a 550
    // (mailbox unavailable) for everything else, so the gateway issues an
    // SMTP-level reject and the sender's MTA bounces. Without this query
    // the gateway has no way to know which recipients the canister
    // accepts and falls back to whatever default policy it was deployed
    // with (the postbox PoC's "user-part must be a numeric anchor
    // number" rule, which rejects `register@…` / `recover@…` outright).
    // The query is open (anyone can call it) but has no side effects and
    // leaks nothing beyond the `related_origins` deploy arg, which is
    // already public.

    smtp_request          : (SmtpRequest) -> (SmtpResponse);
    smtp_request_validate : (SmtpRequest) -> (SmtpResponse) query;
}
```

`email_recovery_get_delegation` mirrors the existing `openid_get_delegation` in `src/internet_identity/src/main.rs:1357`. The delegation seed produced at `smtp_request` time becomes the input to `prepare_jwt_delegation`-equivalent logic.

### 8.4 Setup flow

The SMTP gateway calls `smtp_request` for every inbound message — exactly the PoC's surface, no canister-side shape change. For email-recovery setups the recipient is `register@<host>` for any host listed in the `related_origins` deploy arg (one user-part per kind, no per-challenge id). The canister extracts the DKIM nonce from the signed `Subject:` header of the email, looks up the pending challenge by that nonce, and runs the setup-completion path. The gateway itself does not store the email beyond the brief in-memory hold needed for the canister call.

```mermaid
sequenceDiagram
    participant U as User (logged in)
    participant FE as II frontend
    participant DNS as Public DoH resolver
    participant II as II canister
    participant Mail as User's mailbox
    participant GW as SMTP gateway<br/>(off-chain)

    Note over U,FE: 1 — User enters email
    U->>FE: clicks "Activate" on the inactive email card, types alice@gmail.com

    Note over FE,II: 2 — FE assembles the DNSSEC skeleton chain and prepares the challenge
    FE->>DNS: DMARC TXT for _dmarc.gmail.com,<br/>full DNSSEC chain to root (DoH cd=1, do=1).<br/>DKIM leaf is NOT fetched — selector is not yet known.
    DNS->>FE: signed RRsets + RRSIGs + DS chain
    FE->>II: email_recovery_credential_prepare_add(anchor, EmailRecoveryDnsInput)
    II->>II: validate DNSSEC chain, cache validated zone DNSKEYs<br/>+ DMARC TXT bytes (if present), store pending challenge<br/>keyed by nonce (TTL 30 min)
    II->>FE: { nonce: "II-Recovery-1a2b3c4d5e6f7081", expires_at }

    Note over U,Mail: 3 — User emails the magic token
    FE->>U: "Send an email with Subject: II-Recovery-1a2b3c4d5e6f7081<br/>to register@[window.location.hostname]<br/>(e.g. register@id.ai on prod, register@beta.id.ai on beta)<br/>from alice@gmail.com"
    U->>Mail: composes & sends from alice@gmail.com
    Mail->>GW: SMTP DATA (DKIM-signed by gmail.com)

    Note over GW,II: 4 — Gateway forwards email — canister does pre-DKIM-key verification
    GW->>II: smtp_request(SmtpRequest)
    II->>II: extract nonce from signed Subject header,<br/>parse DKIM-Signature → s=, d=, b=,<br/>canonicalise body, verify bh=, drop body bytes,<br/>compute SHA-256 of signed-headers input,<br/>cache { headers_digest, signature_blob, selector,<br/>signing_domain, from_domain, claimed_address } (~500 B),<br/>set status = NeedDkimLeaf { selector: s }

    Note over FE,DNS: 5 — FE polls, sees the selector, walks the leaf
    loop until terminal or NeedDkimLeaf
        FE->>II: email_recovery_status(nonce)
        II->>FE: Pending → NeedDkimLeaf { selector: "20230601" }
    end
    FE->>DNS: DKIM TXT at 20230601._domainkey.gmail.com + RRSIG
    DNS->>FE: signed RRset
    FE->>II: email_recovery_submit_dkim_leaf(nonce, signed_rrset)
    II->>II: validate leaf against cached chain,<br/>RSA.verify(pk, headers_digest, signature_blob),<br/>check DMARC alignment vs cached policy,<br/>verify Subject is in the signed h= list,<br/>verify From matches the claimed address,<br/>refuse to bind if address is already on a different anchor,<br/>bind address → anchor and mark challenge Succeeded
    II->>FE: Ok

    Note over FE,II: 6 — FE polls one more time and shows result
    loop until terminal status
        FE->>II: email_recovery_status(nonce)
        II->>FE: Pending / RegistrationSucceeded / Failed / Expired
    end
    FE->>U: "alice@gmail.com is now a recovery method."
```

Notes:

- **Single selector per email.** A DKIM-signed email carries exactly one signature over exactly one selector — the value of the `s=` tag in the `DKIM-Signature` header. Verifying that one email therefore needs exactly one DKIM TXT record (the one for that selector), and one DNSSEC chain to prove it. The selector is published only inside the email itself, so the FE can't pre-fetch the DKIM leaf — that's why the bundle is built in two phases (skeleton at prepare, DKIM leaf after the email arrives).
- **No selector probing.** Earlier drafts of this design had the FE ship a candidate list (`selector1`, `s1`, `default`, `google`, date-style patterns for Google, …) and probe them all in parallel via DoH. That worked but was a heuristic — it missed esoteric custom selectors and burned ~20 unnecessary queries on every domain even when only one selector was ever active. The current design is authoritative: the canister reads `s=` directly from the DKIM-Signature header and tells the FE which leaf to fetch. No more guessing.
- **Cached partial-verification record (~500 B).** When the email arrives but the canister doesn't yet have the DKIM public key, we can't run the full DKIM verifier. We _can_: parse the signature header, canonicalise the body and check `bh=` (the body hash is signed, so once `bh=` validates the body's bytes don't matter further; we drop them), compute the SHA-256 over the canonical signed-headers input (RFC 6376 §3.7), and stash the digest plus the raw `b=` blob, the parsed `s=`/`d=`, the From-domain, and the claimed address. Total: under 500 B per pending challenge, vs. the 100 KB+ a full email payload would cost. When the FE submits the leaf in step 5, completing the signature check is a single `RSA.verify(pk, headers_digest, signature)` call.
- **What if the provider rotates between prepare and send.** Doesn't matter — the FE doesn't fetch a selector at prepare time, so there's no stale value to mismatch against. Whatever `s=` the email carries is what the FE walks at step 5.
- The nonce is searched as a case-insensitive substring inside the canonicalized `Subject:` header value, after confirming `Subject` is in the signature's `h=` list (§5.4). The body is not searched at all — moving the nonce out of the body removes the entire class of "is the nonce inside the `l=` window?" edge cases (§5.3).
- `smtp_request` is open: anyone can call it, but the only effect is to verify a DKIM-signed email against an already-cached challenge. A malicious gateway can withhold or duplicate calls, but cannot forge state changes.
- **Retries are concurrent, not overwriting.** A user who closes the wizard and starts again gets a fresh nonce; both pending entries co-exist in the map. Whichever nonce the user actually emails resolves; the other times out at TTL. There is no overwrite-by-anchor or overwrite-by-address — see §8.9 for why.
- Polling cadence: the FE backs off from 1 s to 5 s; after `expires_at` it stops polling and shows the timeout state.

### 8.5 Recovery flow

Same two-phase DNSSEC shape as setup, with three differences:

- `email_recovery_prepare_delegation` is anonymous and additionally takes `session_pk` (a fresh ECDSA public key the FE generated locally).
- On `email_recovery_submit_dkim_leaf`, the canister looks up the anchor from the verified `From:` address and stamps the delegation seed bound to that `session_pk`.
- After polling sees `RecoveryReady`, the FE makes a final query call to `email_recovery_get_delegation` for the actual `SignedDelegation`. `RecoveryReady` carries `anchor_number` so the FE can seed its auth store directly without a separate lookup hop.

```mermaid
sequenceDiagram
    participant U as User (lost passkey)
    participant FE as II frontend
    participant DNS as Public DoH resolver
    participant II as II canister
    participant Mail as User's mailbox
    participant GW as SMTP gateway<br/>(off-chain)

    Note over U,FE: 1 — User enters email
    U->>FE: "Recover with email", types alice@gmail.com
    FE->>FE: generate session ECDSA keypair (session_pk + session_sk)

    Note over FE,II: 2 — FE assembles the DNSSEC skeleton chain and prepares the challenge
    FE->>DNS: DMARC TXT for _dmarc.gmail.com,<br/>full DNSSEC chain to root.<br/>DKIM leaf is NOT fetched — selector is not yet known.
    DNS->>FE: signed RRsets + RRSIGs + DS chain
    FE->>II: email_recovery_prepare_delegation(EmailRecoveryDnsInput, session_pk)
    II->>II: validate DNSSEC chain, cache validated zone DNSKEYs<br/>+ DMARC TXT bytes (if present), store pending challenge<br/>by nonce (TTL 30 min). Anchor resolved later.
    II->>FE: { nonce, expires_at }

    Note over U,Mail: 3 — User emails the magic token
    FE->>U: "Send an email with Subject: II-Recovery-…<br/>to recover@[window.location.hostname]<br/>from alice@gmail.com"
    U->>Mail: send signed email
    Mail->>GW: SMTP DATA

    Note over GW,II: 4 — Gateway forwards email — canister parses signature and caches partial verification
    GW->>II: smtp_request(SmtpRequest)
    II->>II: extract nonce from signed Subject,<br/>parse DKIM-Signature → s=, d=, b=,<br/>verify body hash, drop body bytes,<br/>cache headers digest + signature blob (~500 B),<br/>set status = NeedDkimLeaf { selector: s }

    Note over FE,DNS: 5 — FE polls, sees the selector, walks the leaf
    loop until terminal or NeedDkimLeaf
        FE->>II: email_recovery_status(nonce)
        II->>FE: Pending → NeedDkimLeaf { selector }
    end
    FE->>DNS: DKIM TXT for that selector + RRSIG
    DNS->>FE: signed RRset
    FE->>II: email_recovery_submit_dkim_leaf(nonce, signed_rrset)
    II->>II: validate leaf against cached chain,<br/>RSA.verify(pk, headers_digest, signature_blob),<br/>check DMARC + From + h=Subject,<br/>look up anchor by lowercase(From address),<br/>stamp delegation seed bound to cached session_pk,<br/>mark challenge Succeeded
    II->>FE: Ok

    Note over FE,II: 6 — FE polls and retrieves the delegation
    loop until terminal status
        FE->>II: email_recovery_status(nonce)
        II->>FE: Pending / RecoveryReady { user_key, expiration, anchor_number } / Failed / Expired
    end
    FE->>II: email_recovery_get_delegation({ nonce, session_key, expiration })
    II->>FE: SignedDelegation
    FE->>U: signed in
```

Two design points worth pinning down:

- **No address pre-lookup.** The address typed by the user is sent to the canister at prepare time only as part of the DNS proof, not as a lookup key. The anchor isn't resolved until `email_recovery_submit_dkim_leaf` finalises the verification, when the verified `From:` of the email picks it. If the user typed the wrong address (or doesn't actually own it), the FE just times out polling — there's no leaky lookup-hint round trip.
- **One anchor per address, one address per anchor.** v1 API constraints (§8.2): the same address cannot be registered to two different anchors, because at recovery time the user's email proof would otherwise not uniquely identify which identity they meant; and each anchor holds at most one registered address. The underlying storage is structurally many-to-many (§8.2) so these are pure API checks — relaxing them later doesn't require a storage migration. `email_recovery_submit_dkim_leaf` (setup path) rejects with `AddressAlreadyRegistered` if the address is already bound to a _different_ anchor; if it's already bound to the caller's _own_ anchor (a re-confirm) the call is a no-op success. Swapping email A for email B on the same anchor is supported by submitting a new prepare for B and verifying — the swap commits atomically when the leaf submission succeeds.
- **Retries on recovery work the same as on setup.** A second `email_recovery_prepare_delegation` for the same address creates a _second_ pending entry under a fresh nonce; both co-exist. Whichever nonce the user emails resolves. We deliberately don't overwrite-by-address — see §8.9 for the threat-model reason.
- **Authorisation of the resulting delegation principal.** `check_authorization` (the gatekeeper for every authenticated method on the canister, e.g. `identity_info`) recognises three principal kinds: a passkey/recovery-phrase device on the anchor, an OpenID credential bound to the anchor, and — added with this feature — a delegation derived from the email-recovery seed. After recovery completes the FE's session keypair holds a delegation rooted in `H(salt || "email-recovery" || lowercase(address) || anchor)`; the canister re-derives the same principal at authz-check time, so any anchor with a bound recovery email is reachable via the new principal kind without needing to mint a separate device entry. `activity_bookkeeping` updates `last_used` on the matching credential, and the activity-stats counter has an `email_recovery_counter` next to the existing per-issuer OpenID counter.

### 8.6 UX screen mockups

Layout expressed as ASCII so the flow is reviewable inside this doc.
Headings, button labels, and structure are kept in step with what
PR [#3857](https://github.com/dfinity/internet-identity/pull/3857)
actually renders.

**Manage page — Recovery methods cards** (lives at `(access-and-recovery)/recovery/+page.svelte`)

The phrase and email cards sit in a two-up grid on `sm:` and up,
stacked on mobile. The grid is capped at `max-w-5xl` so the cards
land at roughly 500 px wide on desktop instead of stretching across
the full main column. Inactive cards add a second descriptive paragraph
in the slot the active card uses for "Last used", so the two states
match heights in the same row.

```
Recovery methods
Use these to regain access to your identity if you ever lose it.

┌──────────────────────────────────────┐  ┌──────────────────────────────────────┐
│ 🛡✓                       [ Reset ]   │  │ ✉                  [ Activate ]      │
│                                      │  │                                      │
│ Recovery phrase                      │  │ Recovery email                       │
│ Activated                            │  │ Not activated                        │
│ ──────────────────────────────────── │  │ ──────────────────────────────────── │
│ Last used                            │  │ Sign in by sending a signed email    │
│ 13 seconds ago                       │  │ from your inbox.                     │
│                                      │  │                                      │
│ A 24-word phrase you write down and  │  │ Each email carries a cryptographic   │
│ keep offline.                        │  │ signature that proves it really came │
│                                      │  │ from your inbox. The only party you  │
│                                      │  │ trust is your mail provider, with    │
│                                      │  │ nothing else in between.             │
└──────────────────────────────────────┘  └──────────────────────────────────────┘
```

The phrase card has three states in code (`InactiveRecoveryPhrase`,
`UnverifiedRecoveryPhrase`, `ActiveRecoveryPhrase`) with the same
overall layout — the icon colour, status text, and action button differ:

- `Inactive`: gray `ShieldOff` icon, status "Not activated", primary
  `Activate` button (top-right).
- `Unverified`: warning `Shield` icon, warning-tinted card background,
  status "Not verified", `More options` 3-dot menu with `Verify` and
  `Reset` items. (The Verify+Reset pair sit in a dropdown rather than
  inline because two equally-weighted top-right buttons looked
  cluttered against the primary `Activate`/`Reset` patterns of the
  other states.)
- `Active`: success `ShieldCheck` icon, status "Activated", direct
  `Reset` button (or "Unlock and reset" if the phrase is locked).
  When this method is the _current_ sign-in, a small green dot is
  overlaid on the icon and the "Last used" row reads "Right now".

The email card has two states (`InactiveEmailRecovery`,
`ActiveEmailRecovery`):

- `Inactive`: gray `Mail` icon, status "Not activated", primary
  `Activate` button.
- `Active`: success `MailCheck` icon, the registered address as
  subtitle in place of the activation status, `Last used` row, and a
  `More options` 3-dot menu with `Replace` and `Remove`.

`Replace` re-enters the setup wizard; on success the new address atomically replaces the current one (§8.2) and the user keeps the old one until verification completes. `Remove` opens the confirmation dialog (next mockup) and, on confirm, makes a single authenticated call to `email_recovery_credential_remove`.

Note this is a deliberate divergence from the recovery-phrase card, which has no `Remove`. A recovery phrase is a secret the user can effectively burn by resetting it and then forgetting the new value. An email address is a real-world identity that exists outside II — there is nothing to "burn" — so a user who decides they no longer want email recovery needs an explicit way to detach it.

**Manage page — remove confirmation dialog**

Modeled on `RemovePasskey.svelte` from the access-methods page so the
two destructive flows look the same. Stacked danger-then-cancel
button order matches the rest of the (new-styling) destructive
dialogs.

```
                       ⚠
            Remove recovery email?

   alice@gmail.com will no longer be able to sign
   in to your identity. Your passkeys and recovery
   phrase are unaffected.

   You can add a recovery email again at any time.

   ┌──────────────────────────────────────────┐
   │            Remove email                  │  (danger primary)
   └──────────────────────────────────────────┘
   ┌──────────────────────────────────────────┐
   │              Cancel                      │  (tertiary)
   └──────────────────────────────────────────┘
```

**Setup wizard — step 1: enter address**

There is no in-wizard `Cancel` button — the dialog's top-right `×`
is the only user-driven exit, matching the convention the rest of
the (new-styling) dialogs use. The wizard has no top-of-view step
indicator either; the user moves through three views (enter address →
send confirmation email → success toast) but the views are not
numbered in the UI.

```
Add a recovery email

Type the email address you want to use to recover this
Internet Identity. We'll ask you to send a signed email
from this inbox to confirm.

Email address
┌────────────────────────────────────────────┐
│ alice@gmail.com                            │
└────────────────────────────────────────────┘

┌────────────────────────────────────────────┐
│                Continue                    │  (primary, full width)
└────────────────────────────────────────────┘
```

If the canister returns `DomainNotAllowlisted` _and_ the FE wasn't
able to assemble a DNSSEC skeleton bundle for the domain, the wizard
routes to `UnsupportedDomain` (mockup further below) instead of
surfacing the error inline.

**Setup wizard — step 2: send the confirmation email** (FE shown after `email_recovery_credential_prepare_add` returns; the file is `SendConfirmationEmail.svelte`)

```
Verify your email

Send the email below to confirm.

┌─────────────────────────────────────────────┐
│ TO                                  [ copy ]│
│ register@beta.id.ai                         │
├─────────────────────────────────────────────┤
│ FROM                                        │
│ alice@gmail.com                       🛡✓   │
├─────────────────────────────────────────────┤
│ SUBJECT                             [ copy ]│
│ II-Recovery-1a2b3c4d5e6f7081                │
├─────────────────────────────────────────────┤
│ BODY                                        │
│ (anything, leave it blank)                  │
└─────────────────────────────────────────────┘

┌─────────────────────────────────────────────┐
│           ✉  Open in mail app               │  (mailto: link)
└─────────────────────────────────────────────┘

       ⟳  Waiting for your email to arrive…

           Expires in 29:42
```

The small shield-check chip on the right of the **From** row carries a
tooltip explaining the cryptographic-authenticity model (the wording
differs slightly between the DNSSEC and DoH paths, derived in the
wizard glue from whether `dnsProof` was supplied to `prepare_add`).
Putting the badge against the `From:` value visually anchors the
"this address is verified" claim to the address it's about. The
"Open in mail app" button is a `mailto:` link with `to`, `subject`,
and an empty `body` pre-filled, so the user can complete step 2 in
one click on platforms whose mail client honours `mailto:`. The To
and Subject rows each have a per-row copy button (top-right of the
row) that reveals a "Copied to clipboard" tooltip for ~700 ms after
success — same pattern as `ContinueOnNewDevice.svelte`.

There is no orange warning block, no `Cancel`, and no `Resend` —
closing and re-opening the dialog issues a fresh nonce naturally, so
those affordances would only have been visual noise.

**Setup wizard — step 3: success (no dedicated view)**

The terminal `RegistrationSucceeded` state fires the wizard's
`onSuccess(address)` callback, which closes the dialog and shows a
top-of-page toast ("alice@gmail.com is now a recovery method"). There
is no separate "Done" view — the toast plus the freshly-active card
on the manage page underneath is enough confirmation, and avoids a
modal step the user has to dismiss.

**Recovery sign-in — picker** (lives at `(new-styling)/recovery/+page.svelte`)

The picker uses the `ButtonCard` component with a hover-fade arrow
icon, and the phrase option uses the same `Shield` icon as the
manage-page cards for cross-page consistency.

```
↻ Recover your identity
Pick a recovery method below to sign back in.

┌────────────────────────────────────────────┐
│ 🛡  Recovery phrase                  →     │
│     Type your 24 recovery words.           │
└────────────────────────────────────────────┘
┌────────────────────────────────────────────┐
│ ✉  Recovery email                    →     │
│     Send an email from your inbox.         │
└────────────────────────────────────────────┘

       Cancel       (tertiary link → "/")
```

**Recovery sign-in — email branch** (the same three steps as setup,
with terminal step issuing a delegation and signing the user in
instead of toasting "all set"). Step 2 is identical to the setup-flow
mockup above except the recipient is `recover@<host>` instead of
`register@<host>`.

**Error states**

The unsupported-domain view (`UnsupportedDomain.svelte`) is reached
when the canister rejects the prepare call with `DomainNotAllowlisted`
and the FE couldn't assemble a DNSSEC skeleton bundle:

```
✉✗ Can't use this email

We can't accept email from <domain>.com yet.

▶  Why isn't this supported?           ← <details> collapsible

   To accept email from a domain we either need DNSSEC at
   its apex (so we can cryptographically validate the DKIM
   key your provider uses) or for the domain to be on our
   list of known mailbox providers.

   If you administer <domain>.com, enabling DNSSEC at your
   domain registrar usually takes a few clicks. Once it
   propagates, this address will work automatically. No
   further action from us needed.

┌────────────────────────────────────────────┐
│        Try a different address             │  (primary, full width)
└────────────────────────────────────────────┘
```

Other failure modes (DKIM/DMARC verification failure, address already
registered, expired nonce, etc.) surface via `FailedView.svelte`
inside the wizard — heading + short body + single full-width
`Try again` button that restarts from step 1 with a fresh nonce.

### 8.7 Timeouts and error reporting UX

The flow has two error surfaces: synchronous errors at `prepare_*` time (the user sees them immediately) and asynchronous errors discovered after `smtp_request` lands (surfaced through `email_recovery_status`). They have different UX shapes and different retry stories.

**Pending-challenge TTL.** Every pending entry expires 30 minutes after `prepare_*` returns. The TTL is the same for setup and recovery, and the same for the DNSSEC and DoH paths. The expiry timestamp is returned to the FE in `EmailRecoveryChallenge.expires_at` so the wizard can render a live countdown.

**Polling cadence.** The FE polls `email_recovery_status(nonce)` while the user is composing/sending the email:

- Initial poll: 1 s after the `prepare_*` reply.
- Backoff: doubles to 2 s, 4 s, capping at 5 s.
- Stops at `expires_at`. After that, the wizard transitions to the timeout state without further polls.
- Transient `call failed` errors during the poll are retried silently (one failure window doesn't surface to the user); a sustained outage (~30 s of consecutive failures) shows a non-blocking inline banner ("Trouble reaching Internet Identity — retrying…").

**Status states the FE renders:**

| `EmailRecoveryStatus`                    | UX                                                                                                                                                                                                                                         |
| ---------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| `Pending`                                | "Waiting for your email to arrive…" with a live countdown ("Expires in 29:42") below. No `Cancel` link inside the wizard — the dialog `×` is the cancel affordance.                                                                        |
| `RegistrationSucceeded`                  | (Setup only) Wizard closes; the host page fires a top-of-page toast ("alice@example.com is now a recovery method"). No dedicated "Done" view.                                                                                              |
| `RecoveryReady { user_key, expiration }` | (Recovery only) FE silently calls `email_recovery_get_delegation`, signs the user in, and redirects to `/manage/access`.                                                                                                                   |
| `Failed(reason)`                         | The wizard switches to `FailedView`: "We couldn't verify your email." plus a body mapped from the `EmailRecoveryError` variant (see table below). Single full-width `Try again` button restarts the wizard from step 1 with a fresh nonce. |
| `Expired`                                | Same `FailedView` shape as `Failed(NonceExpired)` ("This recovery link timed out. Try again.").                                                                                                                                            |

**Sync `prepare_*` errors** — surfaced inline before the user sends anything, so they don't waste an email composing on a flow that's already doomed:

| Error variant                                                                         | User-facing copy                                                                                                                                                                      |
| ------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `EmailVerificationFailed("DNSSEC: …")` (sync path — the DNSSEC chain didn't validate) | "We couldn't verify the DNS records for `<domain>`. This usually means the domain doesn't sign its DNS, or our records didn't match. Try a different email or use a recovery phrase." |
| `DomainNotAllowlisted(domain)`                                                        | "Internet Identity can't verify mail from `<domain>` yet. Try a different email or use a recovery phrase." (Plus link to the supported-providers help page from §7.6.)                |
| `DohFetchFailed(detail)`                                                              | "We're having trouble reaching DNS. Please try again in a moment." (Retriable; auto-retry once after 2 s.)                                                                            |
| `DomainNotSupported(domain)`                                                          | Same copy as `DomainNotAllowlisted`.                                                                                                                                                  |
| `Unauthorized(_)`                                                                     | (Setup only — never reached in recovery.) Generic "you need to be signed in" copy; not expected to fire in normal flow.                                                               |

**Async `smtp_request` errors** — only the FE's poll observer learns about them, so they show in the wizard's spinner step:

| Error variant (in `Failed(reason)`)           | User-facing copy                                                                                                                                                                                                                                                                                          |
| --------------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `EmailVerificationFailed(VerificationStatus)` | "Your email didn't verify (DKIM/DMARC failure). Make sure you sent it from `<address>` exactly, no forwarding, no aliases."                                                                                                                                                                               |
| `AddressMismatch`                             | "The email came from a different address than the one we have on file."                                                                                                                                                                                                                                   |
| `SubjectNotSigned`                            | "Your email provider didn't sign the Subject header. Try a different provider." (Edge case — every mainstream provider signs Subject by default.)                                                                                                                                                         |
| `DkimLeafMismatch`                            | "Couldn't verify the DKIM key for your provider. Please retry." (Means the leaf the FE walked didn't validate against the cached chain — typically because the provider rotated DKIM keys between prepare and submit, or a transient resolver hiccup. Both have the same retry-from-scratch remediation.) |
| `AddressAlreadyRegistered`                    | (Setup only.) "This email is already used to recover a different identity."                                                                                                                                                                                                                               |
| `AddressNotRegistered`                        | (Recovery only.) "We don't recognize this email. Did you mean to register it instead?"                                                                                                                                                                                                                    |
| `NonceExpired` / `NonceUnknown`               | Same UX as `Expired` — restart the wizard.                                                                                                                                                                                                                                                                |
| `InternalCanisterError(_)`                    | "Something went wrong on our end. Please try again." (Logged for observability.)                                                                                                                                                                                                                          |

**Cancellation.** Closing the wizard is the cancel action; there's no explicit `cancel_challenge` API. The pending entry expires after 30 minutes, and a fresh `prepare_*` call always issues a fresh nonce. If the user's mail arrives _after_ they cancelled but before TTL, `smtp_request` flips the abandoned challenge to `Succeeded` — for setup that's harmless (the credential is bound; the FE will pick it up on the next manage-page load), and for recovery the orphaned `RecoveryReady` entry just expires unread.

**Observability.** The canister exports per-failure-reason counters via the existing metrics endpoint (`/metrics`). The FE doesn't display these; they exist for operational debugging — e.g. to spot a sudden spike in `EmailVerificationFailed` from a specific provider after a DKIM-key rotation.

### 8.8 Delegation issuance

The delegation is produced via the same canister-signature path as OpenID. At `email_recovery_prepare_delegation` time the FE supplies a fresh ECDSA `session_pk` which the canister parks inside the pending-challenge entry. When `smtp_request` processes the recovery email successfully, the canister stamps the delegation seed using `(anchor || "email-recovery" || lowercase_address)` and the cached `session_pk`; both `user_key` and `expiration` are returned via `email_recovery_status` once the challenge is `RecoveryReady`. The FE then makes one query call to `email_recovery_get_delegation` to retrieve the SignedDelegation. Expiration is the standard 30 minutes (`OPENID_SESSION_DURATION_NS`).

### 8.9 Bounded state, not rate limits

We deliberately don't add per-anchor or per-address rate limits to the recovery flows. The keys we'd reach for don't survive review:

- **Per-anchor on setup.** `email_recovery_credential_prepare_add` is authenticated and the caller is their own anchor — spamming yourself has no upside.
- **Per-address on recovery.** `email_recovery_prepare_delegation` is anonymous and the canister has no IP visibility. Keying a limit by claimed address would create a denial-of-recovery vector: an attacker who knows Alice's email could keep submitting prepare calls for `alice@gmail.com` and lock Alice out of her own recovery channel for the limit window.
- **`smtp_request` per challenge.** This is idempotency, not a rate limit — terminal status flips once and further calls are no-ops.

What we _do_ keep are bounded-state caps, which actually protect the canister:

| Bound                                          | Mechanism                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
| ---------------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Pending-challenge map size                     | Fixed-capacity `StableBTreeMap` with 30-min TTL and oldest-first eviction. Sized generously (≥ 10 000 entries) so legitimate fill rates have plenty of headroom; eviction is the worst case for an attacker who fills it (see below).                                                                                                                                                                                                                                                                                                                                                                                                                                                               |
| Cached DMARC TXT bytes per pending entry       | `MAX_DMARC_TXT_BYTES = 1024` — ~2× the largest realistic DMARC record. The bytes come straight from the verified RDATA; `verify_dnssec_chain` authenticates them but doesn't bound their size, so an attacker who controls a signed zone could otherwise publish a multi-KB TXT and inflate every pending entry. Enforced after the chain validates and before the bytes land on the pending entry; oversize TXT returns `DnsTxtTooLarge`. The DKIM TXT is _not_ cached in v1 — the FE supplies it via `email_recovery_submit_dkim_leaf` post-arrival, where it's used immediately for verification and then dropped (see §8.4 and the `MAX_DKIM_TXT_BYTES = 4096` cap applied at submission time). |
| Cached `session_pk` per pending recovery entry | `MAX_SESSION_KEY_BYTES = 1024` — ~2× the largest realistic public-key encoding (Ed25519 ~44 B, ECDSA P-256 ~91 B, RSA-2048 ~294 B). The FE supplies this on `email_recovery_prepare_delegation` and an unbounded blob would otherwise let an anonymous caller inflate every challenge they prepare. Oversize keys return `InvalidSessionKey`.                                                                                                                                                                                                                                                                                                                                                       |
| Claimed-address bytes per pending entry        | RFC 5321 §4.5.3.1 caps: local-part ≤ 64, domain ≤ 255, addr-spec ≤ 254 (`MAX_LOCAL_PART`/`MAX_DOMAIN`/`MAX_ADDRESS`). The same caps are enforced on the on-anchor credential (§8.2), so the storage path can't widen to anything the prepare path rejects.                                                                                                                                                                                                                                                                                                                                                                                                                                          |
| Partial-verification record per pending entry  | ~500 B (headers digest 32 B + signature blob ≤ 256 B + selector + signing-domain + From-domain + claimed address). Set when the email arrives but the DKIM leaf hasn't been submitted yet; cleared when the leaf is submitted and the signature check completes. Hard caps on each variable-length field keep the record bounded by construction.                                                                                                                                                                                                                                                                                                                                                   |
| Registered addresses per anchor                | Hard cap of 1. A second verified email atomically replaces the prior one at `smtp_request` success time (§8.2).                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     |

**Why nonce-only keying matters.** Pending entries are keyed by the canister-issued `nonce` and _only_ by the nonce — not by the claimed address, not by the anchor. This gives email recovery the same untargetability property as II's existing "Continue from another device" QR flow: the random session ID there, like the random nonce here, is unguessable to anyone except the FE that just received it, so an attacker cannot evict a _specific_ user's pending entry. The only thing an attacker can do is fill the whole map past its capacity, and the eviction analysis below bounds that.

If we instead let a new prepare call overwrite an existing pending entry keyed by anchor or address — the attacker would only need to know the victim's email or anchor number (much more knowable than a fresh random nonce) to hold the legitimate entry permanently evicted. We deliberately don't do that. Concurrent retries co-exist; whichever nonce the user actually emails resolves, the others time out.

**Fill-rate analysis.** For DNSSEC-signed domains, every `prepare` call requires a full chain validation — a CPU-bound operation on the executing replica. The canister naturally rate-caps how fast an attacker can produce pending entries, and a 10 000-slot map at a 30-minute TTL is comfortably above any realistic abuse rate. For DoH-allowlisted domains the prepare call is much cheaper (an allowlist check, no chain to validate), so the same CPU throttle does _not_ apply — an attacker can spam `prepare` for `gmail.com` and fill the map relatively cheaply.

We accept that, because eviction is benign:

- **Untargeted.** Nonce-only keying (above) means an attacker cannot evict a _specific_ user's pending entry. They can only fill the whole map.
- **Self-draining.** Every entry expires in 30 minutes. The map cannot stay full longer than the attacker keeps spending bytes-per-second to refill it; once they stop, it returns to empty within one TTL window.
- **Recoverable.** A legitimate user whose entry was evicted polls and sees `Expired`, the FE shows "timed out, please try again", and the next prepare call gets a fresh slot (since attacker-evicted entries are themselves now closer to expiry too — eviction is FIFO).
- **No standing damage.** Nothing in the cache is sensitive. Filled-up cache entries hold a DKIM pubkey that is publicly resolvable, the address that was claimed, and at most an FE-supplied session_pk — losing them is identical to the user retrying.

If telemetry later shows the eviction churn making recovery noticeably flaky, the response is a frontend-side captcha on `email_recovery_prepare_delegation`, not a per-key rate limit (which would be a denial-of-recovery vector — see above). That's reactive, not a precondition for shipping.

**Nonce generation.** The challenge nonce is drawn from a `ChaCha20Rng` PRNG kept in the canister's heap, seeded once per canister lifetime via `raw_rand` (the IC management-canister API). Re-seeded on `post_upgrade`. Each `prepare` call costs a single PRNG draw (cheap) rather than an `ic-cdk` call to `raw_rand` (which is async and costs cycles). The draw is **8 random bytes**, rendered as 16 lowercase hex characters and appended to the `II-Recovery-` prefix — i.e. 64 bits of entropy. That's far below cryptographic-key territory but several orders of magnitude above what a remote attacker can probe inside the 30-minute TTL: at the 10 000-slot map cap, the per-attempt collision probability is ~2⁻⁵⁰.

### 8.10 Frontend changes

The current management surface lives at `src/frontend/src/routes/(new-styling)/manage/(authenticated)/(access-and-recovery)/recovery/+page.svelte` and today only renders the recovery-phrase card. The recovery (sign-in) flow lives at `src/frontend/src/routes/(new-styling)/recovery/+page.svelte` and uses `RecoverIdentityWizard`.

- **Manage page** — rename the page heading from "Recovery phrase" to **"Recovery methods"** (see §8.6 mockup) and the surrounding tab labels accordingly. The `(access-and-recovery)` layout splits into two cards: the existing `ActiveRecoveryPhrase` / `InactiveRecoveryPhrase` / `UnverifiedRecoveryPhrase` card (no functional change beyond moving Verify+Reset into a `More options` dropdown on the unverified state, see §8.6), and a new `ActiveEmailRecovery` / `InactiveEmailRecovery` pair. The inactive state's primary action is `Activate`, which opens the setup wizard. The active state has a `More options` dropdown with `Replace` / `Remove`. The `Remove` path goes through a confirmation dialog (see §8.6) and calls `email_recovery_credential_remove` once; on success the card flips back to inactive. Unlike the phrase card, removal is a first-class action — see the §8.6 note on why email recovery diverges from phrase UX. New svelte components live in the same `recovery/components/` directory next to the phrase ones.
- **Recovery sign-in page** — refactor `(new-styling)/recovery/+page.svelte` from a single "Get started" entry into a method picker with two `ButtonCard`s: "Recovery phrase" (gray `Shield` icon, opens `RecoverIdentityWizard` as before) and "Recovery email" (gated by the `EMAIL_RECOVERY` feature flag, opens the new `RecoverWithEmailWizard`). The page heading drops from a step-1 "Get started" CTA to a method-agnostic "Pick a recovery method below to sign back in." subtitle. The email-recovery wizard implements the three-step flow from §8.6: enter address → send-the-confirmation-email instructions screen with a live poll spinner → signed-in (terminal state issues the delegation and redirects to the manage page).

DNSSEC bundle assembly (see §7.4) lives in `src/frontend/src/lib/utils/dnssec/` and is reused by both wizards.

There is no "recover with device" flow in the current frontend; the older `FLOWS.mdx` references are stale and should be ignored. All routing and component additions go directly into the svelte routes above.

There is no `web_push.rs` / service-worker integration in scope here — push notifications were a Postbox feature and are out of scope (§2 non-goals).

---

## 9. Test corpus

A `test_vectors/email_recovery/` directory at the repo root carries plain `.eml`, `.json` (DNSSEC chains), and `.txt` (DKIM/DMARC TXT records) files. Both the canister-side unit tests in `internet_identity` and the PocketIC integration tests in `canister_tests` load them via `include_bytes!` (or `std::fs::read` for tests that aren't compiled into the wasm). No separate crate is needed — the data is just bytes shared between two consumers, not a library with behaviour. Categories carried:

- **DKIM happy path**: 100+ real signed messages from each major provider (Gmail, iCloud, Outlook, Fastmail, Proton, SES, SendGrid, Mailgun, Postmark, Tutanota), recorded once and committed as `.eml` + DNS bundle pairs.
- **DKIM tampering**: each happy-path vector mutated in 8+ ways (header bit flip, body byte flip, signature truncation, key swap, …) — every mutation must Unverify.
- **DKIM canonicalization rejection**: at least one captured signature each at `c=simple/simple` and `c=simple/relaxed` — both must Unverify with reason `UnsupportedCanonicalization` (see §5.2).
- **DMARC alignment**: matrix of `(d=, From:)` × `(adkim=s, adkim=r)` × `(equal / X-subdomain-of-Y / unrelated)`. Includes a "PSL would have aligned but we don't" vector (e.g. `d=googlemail.com`, `From: @gmail.com` with `adkim=r`) — must Unverify under our policy (§6.3).
- **DNSSEC**: 20 chains, including ECDSA-only and Ed25519-only; intentionally broken chains for negative tests.
- **Replay/expiry**: signatures with `x=` in the past, future-dated `t=`.

Vectors are committed alongside the expected verdict (`Verified` / `Unverified { reason }`); the canister's hand-rolled verifier asserts that verdict per-vector.

CI runs the corpus against the PocketIC-hosted canister to catch wasm-only regressions.

---

## 10. Migration & rollout

### Phase 0 — Land foundations (no user-visible change)

Split into a stack of focused PRs against `main`. None are merged yet; the stack is reviewed bottom-up.

1. **DNSSEC verifier scaffold** ([#3838](https://github.com/dfinity/internet-identity/pull/3838)). The chain-validation primitive in `src/internet_identity/src/dnssec/`, with the trust-anchor list wired through the canister init/upgrade arg (§7.5).
2. **DKIM verifier** ([#3839](https://github.com/dfinity/internet-identity/pull/3839)). Hand-rolled, in `src/internet_identity/src/dkim/` (see §5.1). Replaces the PoC's parser; takes raw header bytes + a DKIM TXT record string + the canister clock and emits an `EmailVerificationStatus`.
3. **DMARC verifier** ([#3840](https://github.com/dfinity/internet-identity/pull/3840)). Alignment check + combined DKIM+DMARC entry point in `crate::dmarc`.
4. **DoH fallback** ([#3841](https://github.com/dfinity/internet-identity/pull/3841)). 3-of-5 provider quorum + heap cache + dedup, gated by the deploy-arg allowlist (§7.6).
5. **Email-recovery setup flow** ([#3842](https://github.com/dfinity/internet-identity/pull/3842)). On-anchor `EmailRecoveryCredential`, `prepare_add` → `smtp_request` → `status` plumbing, both verification paths, PocketIC integration tests covering both happy paths.
6. **Recovery flow** (planned, not yet opened). `prepare_delegation` (anonymous), delegation-stamping at `smtp_request` time, `email_recovery_get_delegation` query.
7. **Test corpus** lands incrementally with the PRs above.

### Phase 1 — Beta email recovery

4. Add `EmailRecoveryCredential` storage + Candid surface (§8.2, §8.3).
5. Add registration and recovery flows behind a frontend feature flag — register an `EMAIL_RECOVERY` entry in `src/frontend/src/lib/state/featureFlags.ts` with the existing `createFeatureFlagStore(...)` helper (default `false`). Existing flags like `DISCOVERABLE_PASSKEY_FLOW` and `GUIDED_UPGRADE` use this same store; values persist in `localStorage` under `ii-localstorage-feature-flags__EMAIL_RECOVERY` and can be flipped from the browser console via `window.__featureFlags.EMAIL_RECOVERY.set(true)`. The setup and recovery wizard entry points subscribe to the store and render only when the flag is on.
6. Add the off-chain SMTP gateway forwarder service. Stateless w.r.t. canister; ~5-minute in-memory buffer per challenge.
7. Internal beta: developers and trusted testers turn the flag on locally to exercise both flows; nothing is exposed to general users.
8. Telemetry: success rates by mailbox provider, DNSSEC failure rates, time-to-completion percentiles.

### Phase 2 — Public beta

9. Change `EMAIL_RECOVERY`'s default value to `true` in `featureFlags.ts` so the flag ships on for all users; the inactive email card with its `Activate` button appears alongside the recovery-phrase card on `/manage/recovery`.
10. The "Recovery email" entry in the `(new-styling)/recovery/+page.svelte` picker is gated by the same flag and lights up at the same time.

### Phase 3 — GA

11. Remove the beta label; remove the feature flag.

We do **not** keep PoC PR #3760's WASM in any release. Its branch closes when Phase 0 lands.

---

## 11. Open questions

- **DNSSEC root anchor on rotation.** Settled — the trust anchor list is a deploy/upgrade arg (§7.5), refreshed alongside any IANA rollover in the next weekly deploy. KSK rollovers happen approximately once a decade; ZSK rolls don't affect the anchor.
- **Email aliases.** `alice+ii@gmail.com` and `alice@gmail.com` are kept as **distinct** addresses. Gmail treats them as the same mailbox, but other providers don't, and we do not want to bake provider-specific aliasing rules into the canister. The user registers exactly the address they typed, lowercased.
- **Lost mailbox.** If a user's email account is gone (provider closed, domain expired), recovery via this channel is unrecoverable. _Out of scope for this design._ Email recovery is one of several recovery surfaces; users with stronger guarantees should still keep a recovery phrase.
- **Privacy: enumeration.** Settled — addressed by §3.1. The lookup is gated by a DKIM-valid email signed for the queried address, so an attacker cannot probe addresses they do not already control. UX-level rate limiting on `email_recovery_prepare_delegation` keeps churn bounded.
- **Multi-anchor per address.** Not allowed — `smtp_request` returns `AddressAlreadyRegistered` (during a register-deliver) if any anchor already has the address bound. Recovery looks up by address, and users do not generally know their anchor number, so the mapping must be functional (§8.5).
- **Internationalised domains (IDN).** A-label vs U-label canonicalization matters for both the local-part and domain. ASCII-lowercase the A-label form everywhere and reject U-label inputs at the Candid boundary. Acceptable for v1; revisit if user reports surface IDN-mailbox cases we missed.
- **Outbound email confirmation.** Open. The natural upgrade path against the prepare-then-phish attack (§3.2) is to let the gateway send a confirmation email back to the bound address ("click this link to complete recovery") so the security-relevant action lives in the user's inbox, where a phisher can't reach it. The current design relies on the rotating-nonce + Subject-visibility stack (§3.2), which is the inbound-only ceiling. When outbound is built, we revisit recovery to add the confirmation step; the binding flow could move it earlier.

---

## 12. Future work

### 12.1 Hybrid DNSSEC + DoH path for custom domains

**Status: deferred. Open work item.**

The current design pins each flow to _one_ path: full end-to-end DNSSEC, or DoH against an allowlisted apex. A meaningful slice of real-world senders falls between the two:

- **Workspace custom domains with DNSSEC at the apex** (e.g. an organization on Google Workspace whose registrar publishes a DS for the apex): DKIM TXT lives in-zone. The DNSSEC path covers them, no allowlist entry needed. Already supported — the gap is operator-side (registrar-DS) not II-side.
- **M365 custom domains with DNSSEC at the apex**: DKIM resolves via a signed CNAME at the customer's apex into `*.onmicrosoft.com`, which is **unsigned**. End-to-end DNSSEC fails at the cross-zone hop; the DoH allowlist excludes the customer's apex; result: rejected.
- **`live.com` and other "apex signed but DKIM CNAMEs into unsigned" patterns**: today on the DoH allowlist out of necessity, even though most of the resolution chain is DNSSEC-signed.

The natural design that handles all three cases:

1. **FE walks DNSSEC as far as the chain holds** and submits whatever it got — signed apex, signed CNAME, stopping at the first cross-zone unsigned hop.
2. **BE verifies the signed prefix** with the existing DNSSEC verifier (§7.3).
3. **BE inspects the unsigned tail's owner zone**. If the zone is in `DohConfig.allowed_domains` (with label-anchored suffix matching), the BE issues a DoH outcall fan-out for the remaining hops and applies the existing 3-of-5 quorum (§7.6). Otherwise the bundle is rejected.

This reuses the existing allowlist for both pure-DoH and hybrid paths — one config, one matching rule (`name == entry || name.ends_with("." + entry)`). Adding `onmicrosoft.com` to the allowlist covers every M365 tenant globally because the suffix match captures `*.onmicrosoft.com`. The cryptographic warrant for _which_ unsigned name to query comes from the signed CNAME the FE walked, so an attacker can't redirect a DKIM lookup to a different tenant without forging a signature in the customer's signed apex.

**The precise rule:** a custom domain works iff its apex is DNSSEC-signed end-to-end AND either (a) DKIM resolves entirely in-zone, or (b) the unsigned tail terminates in an allowlisted operator zone. Apex signing is a one-time registrar action by the custom domain owner; everything else is automatic.

#### Cost trade-off

The hybrid's BE outcall is the only expensive piece — the FE-side DNSSEC walk is browser-DoH (free); only the unsigned-tail fetch costs cycles. Per-tenant cache fragmentation is real and inherent to per-organization DKIM keys (Workspace customers have the same property — gmail.com is a special case of shared infrastructure, not the norm). Bounding levers:

- The 1 h `max_cache_age_secs` default can be lifted; DKIM keys rotate on month timescales, so a 7-day cap brings the worst case to one cold fetch per tenant per week.
- §8.9's per-anchor outcall caps already prevent unbounded fan-out from an attacker registering throwaway domains.

vs. today's pure-DoH treatment of the same domains, the hybrid roughly halves outcall cost: one fan-out for the DKIM tail instead of two (today's pure DoH fetches both DKIM and DMARC; in the hybrid DMARC validates via DNSSEC at the apex or falls back to strict `d=` alignment per §6.3).

#### Implementation surface

- **FE**: emit a partial `DnsProofBundle` when the chain breaks at a CNAME, recording the unsigned-tail anchor name.
- **BE**: extend the §7.3 verifier to accept partial bundles; gate the unsigned tail on `allowed_domains` (suffix match); reuse `crate::doh::fetch_txt` for the tail fetch.
- **Config**: add `onmicrosoft.com` to `DEFAULT_DOH_ALLOWED_DOMAINS`. Other operator-backend zones can be added the same way as new managed-mail platforms emerge.
- **Matching helper**: lift the gate in `crate::doh::fetch_txt` from exact equality to label-anchored suffix match — same shape as the existing `name_within_domain` helper.

#### What this still doesn't cover

- **Custom domains with no DNSSEC at the apex** — for example a small organization self-hosting mail on a domain whose registrar/nameserver setup never enabled DNSSEC. There's no signed warrant we can anchor to, so the canister has no safe way to query their DKIM. The owner has to set up DNSSEC + DKIM + DMARC properly; until then their flow is rejected. This is a structural property, not something the hybrid can fix.
- **Sender domains with DKIM resolutions terminating in arbitrary unsigned zones** that we wouldn't curate. Same shape as M365 / `live.com`, but for a no-name backend operator we don't recognize. These would need a deploy-time allowlist addition like `onmicrosoft.com`.

---

## 13. References

- RFC 6376 — DomainKeys Identified Mail (DKIM) Signatures
- RFC 8301 — Cryptographic Algorithms and Key Usage for DKIM
- RFC 7489 — Domain-based Message Authentication, Reporting, and Conformance (DMARC)
- RFC 4033/4034/4035 — DNSSEC
- RFC 8624 — Algorithm Implementation Requirements and Usage Guidance for DNSSEC
- [Public Suffix List](https://publicsuffix.org/)
- [IANA DNSSEC root anchors](https://www.iana.org/dnssec/files)
- PoC PR [#3760](https://github.com/dfinity/internet-identity/pull/3760)
