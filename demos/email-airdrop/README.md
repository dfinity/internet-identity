# Email airdrop (Motoko demo)

A small demo canister that airdrops **1 ICP** to each person who emails
`airdrop@<your-domain>` with a valid request.

> ⚠️ This is a **demo**. It illustrates the architecture and the eligibility
> rules. It is **not** hardened for a real, value-bearing airdrop — see
> [Threat model & limitations](#threat-model--limitations).

## The rules of the game

An email is eligible when **all** of the following hold:

1. The **Subject** contains the sender's **ICP address** — a 64-hex-character
   [account identifier](https://internetcomputer.org/docs/current/references/ledger#_accounts).
   The checksum is verified, so only well-formed addresses are accepted.
2. There is **at least one `Cc`** holding a friend's email address (any valid
   address that is not the sender's).
3. The **body** contains, as **plain substrings**, both:
   - the app link (e.g. `https://<your-domain>`), and
   - a short participation phrase
     (`I want to participate in the ICP airdrop`).

   Substrings are used on purpose: mail clients append signatures, footers,
   tracking text, quoted replies, etc., so we never require an exact body.

Other rules:

- **One airdrop per email address, ever.**
- Rewards are paid **while the canister has ICP**. When it runs dry, an eligible
  sender is added to a **FIFO waitlist** and paid automatically once the
  canister is **topped up**.
- **Everyone who ever attempts** to participate is stored:
  `email_address → last_rewarded_at_timestamp` (plus attempt count, last
  outcome, and the address they used).

The landing page (`GET /`) displays these rules.

## The catch: canisters can't receive email

There is **no inbound SMTP on the Internet Computer**. A canister is only
reachable via the HTTP gateway, ingress calls, and outbound HTTPS — it cannot
host a mailbox. So `airdrop@<your-domain>` cannot be served by the canister
directly. The realistic wiring is an **inbound-email webhook**:

```
                 MX record for <your-domain>
   ┌─────────┐    points at the provider     ┌──────────────────────┐
   │  Sender ├──────────────────────────────►│  Inbound-email provider│
   │ (Alice) │   email to airdrop@<domain>   │ (SendGrid Inbound      │
   └─────────┘                               │  Parse / Mailgun Routes│
                                             │  / Cloudflare Email    │
                                             │  Worker)               │
                                             └──────────┬─────────────┘
                            HTTPS POST (raw message)    │
                            to the canister's custom    │
                            domain, with ?secret=...     ▼
                                          ┌──────────────────────────────┐
                                          │  airdrop canister              │
                                          │  http_request_update           │
                                          │   → parse → check rules         │
                                          │   → ICP ledger.transfer (1 ICP) │
                                          │   → or enqueue on the waitlist  │
                                          └──────────────────────────────┘
```

The provider also performs **SPF/DKIM/DMARC** verification and forwards the
result; the canister can require a DKIM `pass` before rewarding (see
`dkimPassed` in the code).

For local development you don't need any of that — call `submitEmail` directly,
or `POST` a raw message to the webhook (both go through the same logic).

## Backend design

One `persistent actor` (`main.mo`):

| Concern            | How it's handled |
| ------------------ | ---------------- |
| Participants       | `OrderedMap<Text, Participant>` keyed by lower-cased sender address. Stores `attempts`, `lastAttemptAt`, `lastRewardedAt`, `rewardedAddress`, `onWaitlist`, `lastOutcome`. Persists across upgrades (orthogonal persistence). |
| Waitlist           | `Deque<Pending>` (FIFO) + a length counter. |
| ICP address        | Scanned out of the subject as a 64-hex token and validated via its **CRC32 account-identifier checksum** before use. |
| Reward             | ICP ledger legacy `transfer` of `REWARD_E8S` to the 32-byte account identifier. |
| "While funds last" | If `transfer` returns `InsufficientFunds`, the sender is **waitlisted** instead. (The ledger is the source of truth, so this is race-safe even under concurrent requests.) |
| Top-ups            | A recurring `Timer` drains the waitlist (FIFO) whenever it can; `triggerWaitlist` lets an operator drain it immediately after topping up. |
| Dedup              | `lastRewardedAt != null` ⇒ already participated; `onWaitlist` ⇒ already queued (no double-enqueue). |
| Webhook auth       | A shared `WEBHOOK_SECRET` in the query string guards `http_request_update` and `submitEmail`. |

### Candid interface

```candid
service : {
  // Rules page + email webhook (POST /?secret=... is upgraded to update).
  http_request:        (HttpRequest)  -> (HttpResponse) query;
  http_request_update: (HttpRequest)  -> (HttpResponse);
  // Typed ingestion, same logic as the webhook (handy for tests).
  submitEmail:    (secret: text, email: Email) -> (Outcome);
  triggerWaitlist:(secret: text) -> ();
  getStats:       () -> (Stats);
  getParticipant: (email: text) -> (opt Participant) query;
  getRules:       () -> (text) query;
}
```

## Configuration

Edit the constants at the top of `main.mo` before deploying:

| Constant | Meaning |
| -------- | ------- |
| `LEDGER_ID` | ICP ledger canister id (mainnet default: `ryjl3-tyaaa-aaaaa-aaaba-cai`). |
| `AIRDROP_EMAIL` / `APP_URL` | Shown on the landing page; `APP_URL` must appear in the body. |
| `REQUIRED_BODY_PHRASE` | The phrase that must appear in the body. |
| `WEBHOOK_SECRET` | **Change this.** Guards the webhook and `submitEmail`. |
| `REWARD_E8S` / `FEE_E8S` | Reward (default 1 ICP) and ledger fee, in e8s. |
| `WAITLIST_INTERVAL_SECONDS` | How often the waitlist is retried. |

In production, prefer passing these as **init args** (or a controller-only
`configure` method) rather than baking them into the Wasm.

## Run it locally

Prerequisites: [`icp-cli`](https://github.com/dfinity/icp-cli/releases/latest).

```bash
cd demos/email-airdrop
icp network start -d --clean
icp deploy
```

`transfer` calls will fail locally unless you also deploy an ICP ledger and fund
the canister's default account — so locally you can expect a valid email to come
back as `transfer_failed` (the eligibility logic still runs). Wire up a local
ledger to exercise the reward and waitlist paths end to end.

### Simulate an email (typed)

```bash
SECRET="change-me-please"
ADDR="1b3ae1cd030a11181f262d343b424950575e656c737a81888f969da4abb2b9c0" # checksum-valid

icp canister call airdrop submitEmail "(
  \"$SECRET\",
  record {
    from = \"alice@example.com\";
    to = \"airdrop@CANISTER_DOMAIN\";
    cc = vec { \"bob@example.com\" };
    subject = \"$ADDR\";
    body = \"I want to participate in the ICP airdrop via https://CANISTER_DOMAIN\";
    dkimPassed = true;
  }
)"
```

### Simulate an email (raw webhook, as a provider would)

`sample-email.txt` is a ready-to-send raw message. Point the URL at your
deployed canister's HTTP endpoint:

```bash
curl -X POST \
  --data-binary @sample-email.txt \
  "https://<canister-id>.icp0.io/?secret=change-me-please"
```

Inspect state:

```bash
icp canister call airdrop getStats
icp canister call airdrop getParticipant '("alice@example.com")'
```

## Threat model & limitations

This demo deliberately keeps the rules simple. Before using anything like it for
real value, consider:

- **Sender spoofing.** Email `From` is trivially forged. Only reward
  **DKIM/DMARC-verified** senders — the provider supplies this; the code carries
  a `dkimPassed` flag but does not hard-require it by default.
- **Webhook authentication.** The shared secret lives in the URL (so it can end
  up in logs). Prefer an **HMAC signature over the body** (most providers sign
  their webhooks) and verify it in `http_request_update`.
- **`submitEmail` is a bypass.** It exists for testing and is only
  secret-gated. In production, remove it or restrict it to the relay's
  principal so the only real path is a verified inbound email.
- **"Cc a friend" is gameable.** A user can Cc a second address they control.
  Treat it as a fun social rule, not a Sybil defence.
- **No proof of address ownership.** The reward goes to whatever address is in
  the subject — fine here (you only pay the sender's own address), but combined
  with spoofing it enables grief/Sybil farming without DKIM.
- **Subject parsing** expects the account identifier as a delimited 64-hex
  token. ICRC-1 principal-style addresses are **not** supported (the demo uses
  the legacy account-identifier `transfer`).
- **Reentrancy.** Funds checks rely on the ledger rejecting overdrafts
  (`InsufficientFunds`), which is safe, rather than on an in-canister balance
  cache.

## Questions & suggestions

A few open design choices worth a decision before going further:

1. **Address format** — support ICRC-1 (`principal[.subaccount]`) addresses in
   addition to (or instead of) the legacy 64-hex account identifier?
2. **Repeat rewards** — the spec stores `last_rewarded_at_timestamp` (which
   hints at repeats), but also says "one participation per email". This demo
   enforces **once per address, ever**. Do you instead want a **cooldown**
   (reward again after N days)? That field is already in place for it.
3. **Friend incentive** — should the Cc'd friend also be enrolled / rewarded
   (a referral mechanic), or is Cc purely a participation gate?
4. **Authenticity** — is requiring **DKIM `pass`** acceptable UX, and which
   inbound-email provider do you want to standardise on?
5. **Top-up detection** — the recurring timer is simple but polls. Would you
   rather expose only `triggerWaitlist` (operator-driven, no idle cycles), or
   keep the automatic timer?
