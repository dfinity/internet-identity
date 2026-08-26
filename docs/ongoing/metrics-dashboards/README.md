# Internet Identity metrics

A proposed replacement for the single-page `internet-identity` Grafana dashboard, split into five pages by the question each answers. Click through them the way you would the real thing.

| Page                                      | Answers                                                    | Panels |
| ----------------------------------------- | ---------------------------------------------------------- | ------ |
| [Health](health.md)                       | Is anything broken right now, and would we be paged for it | 7      |
| [Usage](usage.md)                         | How much it is used, by how many people, on which apps     | 7      |
| [Staying signed in](staying-signed-in.md) | Whether people come back, how often, and how it ends       | 9      |
| [Access methods](access.md)               | Which methods people authenticate with, and how that moves | 2      |
| [Storage and capacity](storage.md)        | Where the memory goes and when anything runs out           | 6      |

Each panel shows the chart and what it answers. The panel it replaces, its data sources, its query and its caveats are folded underneath, so a page reads at a glance and the working is one click away.

Charts are sketches. Numbers described as live come from the production endpoint on 2026-08-26; the rest are shapes.

<details>
<summary><b>Why five pages instead of one</b></summary>

Today's dashboard has 24 panels on one page — 17 `timeseries`, 5 deprecated Angular `graph`, 2 `stat`. Seven of them are wrong, and one has been reporting a constant. A single page of 24 is scrolled past rather than read, and a panel nobody reads is a panel nobody notices is wrong.

The split also matches how they get used. Health is the page to open when something is reported. Usage and Staying signed in answer different halves of a planning question — how much, and whether it sticks. Access methods and Storage are opened when a specific question comes up, which is rarely.

Two concerns the current dashboard runs together are kept apart here. How much an app is used and how the people using it authenticated are separate questions, so no usage panel is labelled by access method and the mix gets its own page. A usage number should not move when the mix does.

Alerting is only on [Health](health.md), and only on two of its panels.

</details>

<details>
<summary><b>What belongs on these pages, and what does not</b></summary>

One rule decides most of it: **these pages are for what only the canister can know.**

Visitor behaviour, funnels, page views and drop-off are already tracked in Plausible, and tracking them twice means maintaining two definitions that drift apart. Where a panel here duplicates something Plausible has, it goes rather than getting fixed — which is what happens to Bounce Rate below.

The same rule excludes some session questions. Whether a silent re-issue rendered anything, and whether it had something to resume from, are decided in the frontend; the canister sees a delegation request like any other.

</details>

<details>
<summary><b>The endpoint as it stands: 53 families, all gauges</b></summary>

Every one of the 53 metric families is encoded as a gauge. There are no counters and no histograms anywhere, which has three consequences that recur across these pages.

A family named `_counter` is still a gauge, and several are documented as counting since the last upgrade. `increase()` over one loses whatever accumulated between the last scrape and the deploy.

Nothing can expose a distribution. Any question of the form "how long" or "what is the ninetieth percentile" is unanswerable today, whatever the data.

Every counter these pages propose therefore needs encoding as a counter and keeping in persistent state. Nothing on the endpoint does this yet, so it is the one piece of groundwork the rest rests on.

Separately, five panels still use Grafana's deprecated Angular `graph` type: Internet Identities, Registrations the last 24h, Logins per Hour, Identity Changes per Hour, Signature map size. The warning triangle on exactly those five is that deprecation, not an alert.

</details>

<details>
<summary><b><code>ii_origin</code> does not mean what it says, and comes off the usage metrics</b></summary>

The two per-app families are filtered to one value of a label read from the authenticating passkey's registration origin:

```rust
let maybe_domain = match &authorization_key {
    AuthorizationKey::DeviceKey(device_key) => anchor.device(device_key).unwrap().ii_domain(),
    _ => None,
};
```

A passkey created on `ic0.app` is labelled `ic0.app` for life whichever domain its owner uses, and anything that is not a passkey — OpenID above all — has no origin and falls into a bucket the endpoint never publishes. Live, of 3,627 daily active identities, 190 are attributed to `identity.ic0.app`, 1,893 to `id.ai`, and 1,490 to no domain at all, matching the 1,485 daily active OpenID identities to within five.

How somebody authenticated is a separate question from how much an app is used, and a usage number should not move when the answer changes. So the label comes off the per-app families rather than getting a better definition, and the same reasoning removes the by-domain series from the active-identity panels. Which domain a browser actually visited is measurable from the browser, so Plausible owns it.

</details>

<details>
<summary><b>What the canister can observe about a session</b></summary>

Three facts from the implementation on `feat/session-devices-settings` decide which session questions are answerable at all. Getting them wrong produces panels that look reasonable and are drawn from a biased sample.

**A sign-in always creates a session.** `prepare_account_session` calls `create_session` unconditionally; no path finds and reuses an existing one. One ceremony is one new session, a person signing in from three browsers creates three, and there is no returning sign-in to count. What the ceremony does distinguish is the browser: it computes `known_device` and records a `RegisterSessionDevice` operation only when the browser is new to the identity.

**Every use carries its own history.** `stamp_session_refresh` holds the session's `created_at`, its previous `last_refreshed` and the current time before overwriting anything, so the age of the relationship and the gap since its last use are both free at that point. `last_refreshed` is `None` until first use, making "was this ever used" an observable transition rather than an inference.

**An ending is mostly invisible.** Expiry writes nothing — the storage comment on the cap states that "a session can expire with no write anywhere, so the count drifts upwards". Only two things remove an expired session and neither is a sweep: `reclaim_sessions` runs once an identity holds 500 sessions, which is almost nobody, and `stamp_session_refresh` drops dead sessions from the row it was already rewriting, which only reaches apps somebody still uses.

So anything observed at removal is drawn from relationships still alive, and the abandoned ones — the population most worth knowing about — are the ones missing. Every session panel here is built from creation and use, which are complete. The one counter observed at removal counts deliberate endings only, and says so in its title.

</details>

<details>
<summary><b>Four panels that go away</b></summary>

**Bounce Rate on identity.ic0.app.** Not from this endpoint at all — it reads ClickHouse `http_access_important_canisters`, dividing visitors who took no authenticated action by all visitors, where an authenticated action is one of `('prepare_delegation', 'get_anchor_info', 'register')`. The current frontend signs in through `prepare_account_delegation` and registers through `identity_registration_start`, neither of which is in that list, so nearly every real sign-in counts as a bounce and the panel sits pinned at 1.0. Fixing the list would produce a correct number duplicating one Plausible already has, and it would drift again at the next flow change.

**Top 10 dapps by cumulative session length, 24h and 30d.** The family sums the lifetimes delegations were _issued for_, fixed at sign-in and capped at 30 days, so it measures nothing about time spent signed in. Live, the top app over 24 hours reads 12,960,000 seconds from 5 sign-ins — exactly 30 days each, because that is what the app requested. It is a sign-in count multiplied by a constant, which is why Grafana renders it in years and why its ranking disagrees with the sign-in panel beside it. Two panels, and nothing replaces them directly: the duration they appear to promise is not observable. [How much of the term gets used](staying-signed-in.md#how-much-of-the-term-gets-used) and [How long between visits](staying-signed-in.md#how-long-between-visits) answer what they were reaching for, from use rather than from removal.

Also note the word: "session" means two different things on today's dashboard, and this panel is why. Here it is a delegation's validity window, fixed at sign-in. Everywhere else it is a record letting one browser re-issue its own delegations for one app. Deleting this panel resolves the collision.

**Registration Rates / Captcha Threshold Rate.** Shows "No data", and has for long enough that nobody noticed. The canister emits `internet_identity_registrations_per_second` only when the rate tracker has data, and on the live canister it does not. If the captcha threshold is still a live mechanism the metric should be emitted unconditionally and the panel kept; if it is not, the panel is training people to ignore empty panels, which is expensive the first time an empty one matters.

</details>

<details>
<summary><b>New sources these pages need, and what each costs</b></summary>

Cost is what it takes the canister to produce the number. Sorting this into now, later and much later is a separate conversation.

| Source                                                 | Action | Type      | Cost                                             |
| ------------------------------------------------------ | ------ | --------- | ------------------------------------------------ |
| `internet_identity_sign_ins_total{flow,dapp,browser}`  | add    | counter   | one increment on two existing writes             |
| `internet_identity_app_delegation_requests_total`      | add    | counter   | one increment with a label on one update         |
| `internet_identity_session_uses_total{age}`            | add    | counter   | one increment where refresh already writes       |
| `internet_identity_session_first_uses_total`           | add    | counter   | one increment on the `None` to `Some` transition |
| `internet_identity_session_gap_seconds`                | add    | histogram | one subtraction where refresh already writes     |
| `internet_identity_sessions_revoked_total{reason}`     | add    | counter   | one increment at each of four revoke call sites  |
| `internet_identity_identities_per_app{dapp}`           | add    | gauge     | read and sort a count already stored             |
| `internet_identity_live_sessions`                      | add    | gauge     | needs a pass over the session rows               |
| `internet_identity_account_counter_discrepancy_count`  | keep   | gauge     | already published, never plotted                 |
| `internet_identity_anchor_operations_counter`          | change | counter   | move to persistent state so it survives upgrade  |
| `internet_identity_daily_active_anchors_by_domain`     | remove | gauge     | an access-method label on a count of people      |
| `internet_identity_prepare_delegation_count`           | remove | gauge     | superseded by `sign_ins_total`                   |
| `internet_identity_prepare_delegation_session_seconds` | remove | gauge     | measures requested lifetime, not time signed in  |
| `internet_identity_delegation_counter`                 | remove | gauge     | superseded by `sign_ins_total`                   |

Three panels — live sign-ins, apps per person, browsers per person — need something that walks the session rows, which nothing does today. They are on the pages because they are wanted, not because they are cheap.

</details>
