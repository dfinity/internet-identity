# Session metrics

**Depends on:** `revocable-app-sessions.md` and `tracked-default-accounts.md`. This document decides which metrics ship with the feature and which need work first.

The session flow feeds no metrics at all. `prepare_account_session` calls no bookkeeping, so a sign-in that creates a session increments nothing and emits no event. Two existing dashboard panels therefore degrade as apps adopt sessions, and nothing reports on the feature itself.

Figures below are from the production endpoint on 2026-08-26.

## What the existing dashboard will get wrong

**Top 10 dapps by number of sign-ins.**  
Fed only by `prepare_account_delegation`. An app that moves to sessions will show its traffic falling to zero while its usage is unchanged.

**Top 10 dapps by cumulative session length.**  
This one is already wrong, before sessions exist. It renders `internet_identity_prepare_delegation_session_seconds`, which sums the lifetimes delegations were _issued for_, not time anyone spent signed in. The top app over 24 hours reads 12,960,000 seconds from 5 sign-ins: exactly 30 days each, because that is what the app requested. The panel is the sign-in count multiplied by a constant, which is why its axis reads in years.

**Logins per Hour.**  
Derived from `internet_identity_delegation_counter`, which the session flow does not touch either.

Two further properties of the endpoint, both worth knowing before adding anything.

All 53 metric families are encoded as gauges. There are no counters and no histograms, so `increase()` under-reports across every upgrade.

The `ii_origin` label does not mean what its name suggests, and this is worth reading carefully before anyone relies on it.

It comes from `check_authz_and_record_activity`, which derives the domain from the _authenticating device's registration origin_ rather than from the domain the request came from:

```rust
let maybe_domain = match &authorization_key {
    AuthorizationKey::DeviceKey(device_key) => anchor.device(device_key).unwrap().ii_domain(),
    _ => None,
};
```

`Device::ii_domain()` reads the origin stored when that passkey was created. So a passkey registered on `ic0.app` labels its events `ic0.app` for the rest of its life, whichever domain its owner uses today. And every authorization that is not a passkey, OpenID above all, has no origin to read and falls into the `None` bucket.

The per-app metrics then retrieve `Some(Ic0App)` only, so they publish 40 series on one value of a label that means "this passkey was registered on ic0.app". Both the id.ai-registered passkeys and the whole OpenID population are absent.

The scrape shows the size of that. Of 3,627 daily active identities, 1,893 are attributed to `id.ai`, 190 to `identity.ic0.app`, 20 to `internetcomputer.org` and 34 to both, leaving 1,490 attributed to no domain at all. Daily active OpenID identities on the same scrape total 1,485, which is that gap to within five. The per-app panels rest on roughly five percent of daily active identities, selected by where a passkey was once registered.

Dropping the filter is therefore not sufficient on its own. Summing across the label would total a quantity nobody means, so the label wants to carry the domain the request arrived on, or the per-app metrics want no domain label at all.

## Add now

Each is a constant, a counter on a write that already happens, or a count already stored. None needs a timer, a sweep, a scan at scrape time, or a new stable structure.

| Metric                                              | Type      | Source                                                       | Answers                                |
| --------------------------------------------------- | --------- | ------------------------------------------------------------ | -------------------------------------- |
| `internet_identity_sign_ins_total`                  | counter   | both sign-in paths, labels `flow` and `dapp`                 | session adoption; fixes the two panels |
| `internet_identity_app_delegation_requests_total`   | counter   | `app_prepare_delegation`, label `outcome`                    | the only error rate; traffic per user  |
| `internet_identity_sessions_ended_total`            | counter   | every removal path, label `reason`                           | sign out or drift; revocation working  |
| `internet_identity_session_age_seconds`             | histogram | `min(now, valid_till) - created_at` at removal, nine buckets | how long a sign-in really lasts        |
| `internet_identity_daily_active_sessions`           | gauge     | existing activity machinery, reading `last_refreshed`        | how many sign-ins are in use           |
| `internet_identity_identities_per_app`              | gauge     | `stored_account_references` per application, label `dapp`    | reach per app                          |
| `internet_identity_browsers_evicted_total`          | counter   | the browser list trimming at 20                              | forced sign-outs                       |
| `internet_identity_session_reclaim_passes_total`    | counter   | the reclaiming pass                                          | whether the caps bind                  |
| `internet_identity_session_max_lifetime_seconds`    | gauge     | `MAX_SESSION_TTL_NS`                                         | reference line                         |
| `internet_identity_app_delegation_lifetime_seconds` | gauge     | `APP_DELEGATION_TTL_NS`                                      | reference line                         |
| `internet_identity_sessions_per_identity_limit`     | gauge     | `MAX_SESSIONS_PER_ANCHOR`                                    | reference line                         |
| `internet_identity_browsers_per_identity_limit`     | gauge     | `MAX_SESSION_DEVICES`                                        | reference line                         |

Plus two fixes to what is already published. Make the per-app metrics cover everyone, which means deciding what `ii_origin` should mean rather than only removing the filter. And retire the cumulative-session-length panels once the age histogram exists, since the histogram is the number those panels were meant to show.

Four constraints.

New counters need encoding as counters and keeping in persistent state, which takes appended optional fields, so an upgrade does not reset them. Nothing on the endpoint does this today.

`outcome` can only say `expired_in_place` or `unknown`. All seven failure sites in the mint path return the same error, and revoking deletes the index entry, so a revoked sign-in is indistinguishable from one that never existed.

`app_get_delegation` and `check_session` are queries and cannot increment anything, so the error rate covers the update half of the traffic only.

Nine bucket edges on the age histogram, because `histogram_quantile` interpolates inside a bucket. Sign-ins that ran the full 30 days all land in the top finite bucket, and ones nobody returns to are removed late, so they are under-represented.

## Needs work first

| Question                                     | What it needs                                                                                                      |
| -------------------------------------------- | ------------------------------------------------------------------------------------------------------------------ |
| How many sign-ins are valid right now        | A timer sweep with a cursor stored across executions, or make expiry an event so a counter can follow it           |
| Apps and browsers per identity               | The same sweep, bucketed into `le` labels                                                                          |
| Which apps people are signed in to right now | A by-product of that sweep; it cannot be kept on writes, because expiry writes nothing                             |
| Seconds from revoking to access stopping     | A tombstone carrying the revocation time, kept about one credential lifetime, plus a histogram observed at refusal |
| Cost in cycles rather than in requests       | Instruction accounting per method, in the handlers                                                                 |
| Which release changed a number               | A version label at build time                                                                                      |
| How many people, not how many sign-ins       | A second instance of the activity machinery, keyed on an identity using a session                                  |

Settle the first row before the rest. Whether expiry becomes an event or stays lazy decides whether three of these are cheap or separate projects.

## Order of work

1. Publish the four constants, and decide what `ii_origin` means before touching the filter.
2. `sign_ins_total` on both paths, labelled `flow` and `dapp`. It makes the session flow visible and repairs the two per-app panels and Logins per Hour.
3. `app_delegation_requests_total`. The only alertable metric, and the denominator for traffic per signed-in user.
4. `sessions_ended_total`, `session_age_seconds`, `browsers_evicted_total`, `session_reclaim_passes_total`. Retire the cumulative-session-length panels here.
5. `daily_active_sessions`, then `identities_per_app`.
