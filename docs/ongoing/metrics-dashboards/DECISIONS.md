# What to chart, and why

The panel list, settled. How to build each one is a separate question and is not answered here.

Three designers produced independent panel lists from the same verified findings; six auditors checked the previous draft against the code, the dashboard JSON and the live endpoint. Where the designers agreed, that is recorded as settled. Where they split, the call is stated with its reason.

## Pages

Five. Four permanent, one deleted when the migration ends.

| Page                  | Who opens it                | How often                                 |
| --------------------- | --------------------------- | ----------------------------------------- |
| **Rollout**           | Whoever is shipping a stage | Every deploy, daily while a stage is live |
| **Health**            | On call                     | On report, and after every deploy         |
| **Usage**             | PM, leadership              | Weekly                                    |
| **Staying signed in** | Identity team               | Monthly, and in any argument about a cap  |
| **Capacity**          | One engineer                | Quarterly                                 |

Abuse signals sit in a labelled band at the bottom of Health rather than on their own page — two designers of three, on the grounds that the same person reads them in the same moment. Every panel in that band rests at zero, so the band is readable in one glance.

Access methods is not a page. The rule that a usage number must not move when the auth mix moves is a rule about labels, not about page adjacency.

## Rollout — temporary, delete with the legacy path

| Panel                                                         | What it answers                                               |
| ------------------------------------------------------------- | ------------------------------------------------------------- |
| Sign-ins per hour: old path, new path, and the total          | Is the total flat while traffic moves between them            |
| Share of sign-ins that created a session                      | Can the old path be turned off yet                            |
| Principal index: entries written against entries owed         | The release gate — nothing resolves a principal until equal   |
| Index backfill throughput and time to finish                  | Is the sweep still moving, or stalled at 99%                  |
| Refusals in the 6h after a release against the 6h before      | Did the release just shipped break refresh                    |
| Apps still signing in on the old path                         | Who to call before turning it off                             |
| App records created per hour, and identities near the row cap | Sign-in became a write path — is growth on the modelled curve |
| The abandoned identity region against the live one            | Can the old memory and its read path be retired               |

The index gate cannot be watched today: the backfill's progress lives in a `thread_local` reachable only through a hidden query, so after any upgrade the canister cannot say whether the sweep it just restarted has finished. Publish the indexed count as a gauge and gate on it equalling the reference count — an equality between two published numbers survives an upgrade; a boolean does not.

## Health

| Panel                                                 | What it answers                                                       |
| ----------------------------------------------------- | --------------------------------------------------------------------- |
| Apps being refused a delegation                       | Are people being signed out against their will · **alerts**           |
| Sign-ins failing                                      | Can people get in at all, by outcome on the same counter · **alerts** |
| Sign-ins per hour against this hour last week         | The catch-all: everything upstream fails as absence · **alerts**      |
| Delegation mints per second                           | Load, undivided — a capacity number, not an engagement one            |
| Seconds since the archive last pulled                 | Is the archive alive · **alerts**                                     |
| Operations waiting for the archive, against its limit | Is it falling behind, with the limit drawn                            |
| Live delegation signatures held                       | Canary; its band must be re-derived once 5-minute mints land          |
| Time since each OpenID issuer's keys last refreshed   | Is Google, Microsoft or Apple key fetching broken · **alerts**        |
| OpenID verifications by issuer and outcome            | Which issuer is failing, and whether it is a missing key              |
| Running since                                         | Explains every counter reset; drives deploy annotations everywhere    |

Abuse band, all resting at zero:

| Panel                                          | What it answers                                     |
| ---------------------------------------------- | --------------------------------------------------- |
| Registration allowance consumed                | Is anyone registering in bulk                       |
| Browser takeover attempts                      | Is a copied or replayed browser key being announced |
| Sessions waking after a day or more idle       | Did many dormant sessions start minting at once     |
| Mints per session against the expected 12/hour | Is anything minting far above what a browser needs  |
| Identities pressed against the caps            | Is anyone at the session or browser cap             |
| Counts rebuilt, and DNS answers disagreeing    | Two invariants that should never move               |

The two OpenID panels cover the largest unmonitored dependency in the system. Roughly 1,485 of 3,627 daily active identities authenticate through Google, Microsoft or Apple; their signing keys are refetched on a fifteen-minute timer; and a failing fetch is reported only to the canister log. The comment on that line already describes the consequence — a provider rotating its key while fetches fail "shows up only as `Certificate not found for {kid}` on every sign-in, with no hint of the cause". Splitting that case out as its own verification outcome turns an unattributable sign-in failure into a named alert, and the freshness threshold derives from the refresh interval the way archive staleness derives from the polling interval.

The week-over-week comparison on sign-ins is the design, not decoration: the rate has hard daily and weekly seasonality, so a static threshold either never fires or fires every Sunday.

Cycles get a gauge but no alert. II runs on the NNS subnet, which does not charge for execution or storage, so the balance reads a constant — the gauge costs two lines and becomes the first alert on this page if II ever moves.

No memory panel here. Memory moves on a scale of months; putting it on the page read during an incident teaches people to scroll past the panels that matter.

## Usage

| Panel                                        | What it answers                                    |
| -------------------------------------------- | -------------------------------------------------- |
| Sign-ins per hour, by flow                   | Core volume, with the rollout visible in the split |
| Daily and monthly active identities          | How many people use II                             |
| Daily actives as a share of monthly          | Most days, or once a month                         |
| Total identities, and new identities per day | Population growth                                  |
| Sign-ins per app                             | Which apps carry the traffic                       |
| People reached per app                       | Breadth against volume — signed up and left        |
| How people sign in, daily and monthly        | Passkey against each OpenID issuer                 |
| Identity changes per hour, by kind           | What people change about their identity            |

`ii_origin` comes off every family here, and the by-domain series come off the endpoint entirely. The label is a passkey's registration origin, held for life, hardcoded to one domain in the encoder — it currently scopes the per-app panels to 190 of 3,627 daily actives, and OpenID credentials carry no origin at all.

## Staying signed in

| Panel                                                     | What it answers                                       |
| --------------------------------------------------------- | ----------------------------------------------------- |
| Do people come back                                       | The retention curve; read as a rate ratio, and say so |
| Sign-ins never used again                                 | How often a sign-in leads nowhere                     |
| How long between visits                                   | Return cadence                                        |
| How long a session was granted for, and what shortened it | Does anyone actually get less than the maximum        |
| When in its life a session gets used                      | The chart to bring to any argument about the term     |
| Sessions people ended themselves                          | Is revocation ever exercised                          |
| Sessions the canister ended for them                      | Involuntary sign-outs — the regret chart              |
| Sign-ins in use, daily and monthly                        | How many standing relationships are live              |
| Apps a person is signed in to                             | Identity layer, or login button                       |
| Browsers a person is signed in from                       | Is the browser cap anywhere near binding              |

Two corrections that the previous draft got wrong and that change what these mean.

**A refresh is a poll, not a visit.** The stamp fires from the delegation mint on a five-minute cadence, so an open tab emits about twelve an hour. Every behaviour panel above counts _returns_ — a refresh whose gap since the previous one exceeds thirty minutes — and polls appear only on the abuse band, where volume is the point.

**There is no single thirty-day term.** It is clamped between ten minutes and thirty days and narrowed by the consent picker and by an SSO organisation's ceiling, neither of which the canister re-enforces. So the granted term needs its own panel, and age has to be read as a fraction of each session's own term.

Involuntary endings get their own panel rather than a bar beside voluntary ones. Three mechanisms end live sessions with nobody asking — browser-cap eviction, session reclaim once an identity is at the cap, and a re-sign-in replacing whatever that browser held — and a spike in those demands the opposite response to a spike in people signing out.

## Capacity

| Panel                                    | What it answers                               |
| ---------------------------------------- | --------------------------------------------- |
| Where the stable memory goes             | Bytes by structure, ranked, named             |
| Stable memory against the cap that binds | Against the managed cap, not the subnet limit |
| Heap memory against its limit            | Heap exhaustion traps every call              |
| Years until identity numbers run out     | With the arithmetic corrected                 |
| What is stored                           | Apps, accounts, sessions, MCP grants          |
| Operations archived, total and per hour  | A monotone total hides a throughput change    |
| Archive stable memory                    | Pending an answer on the archive's own cap    |

The memory panel must report every memory. Four are missing from the list that feeds it, including the session index — the one structure whose growth is entirely new — and one series is published against the wrong structure.

## How these are reported

A dashboard viewer chooses the time range. The canister should therefore publish
quantities Prometheus can re-window, and bake in a window only where re-windowing is
mathematically impossible. Today's endpoint does neither consistently: the two per-app
families carry a `window="24h"` or `window="30d"` label, and not one of the 53 families is
a histogram, so nothing can be re-bucketed or quantiled at query time.

**A count of events is a counter, with no window label.** One counter, and
`increase(x[$__range])` answers every window anyone asks for. This is why the two per-app
panels collapse to one: the 24-hour and 30-day pair exist only because the canister computed
both windows itself, and the time picker replaces them. The `window` label goes with them.

**A distribution is a histogram, with `le` buckets.** Session age at use, the gap between
returns, the granted term: each is a property of an event, not a window over events, so a
counter cannot carry it. Published as `_bucket`/`_sum`/`_count`, quantiles work at query
time and the cumulative reading of `le` is standard rather than something a panel has to
define for itself. Bucket edges are fixed at publication and choosing them is a real
decision, but that is inherent to histograms and is not a window in the sense above.

**A count of unique things is the exception, and the window has to be the canister's.**
Uniqueness does not add up: seven daily unique-identity counts cannot be combined into a
weekly one, because Prometheus has no way to know how many identities appear in more than
one day. Nothing at query time can recover it, so the canister must decide the window before
it counts, and daily and monthly are two separate families rather than one re-windowable
series. This is why active identities and active sessions keep their fixed windows while
everything else loses them — the constraint is arithmetic, not habit.

## Settled questions

**Nothing charts sessions against the access method that created them, because a session is not bound to one.** A session record carries the browser it belongs to and nothing about the passkey or credential that authenticated the ceremony, so "the sessions this passkey created" is not a quantity the canister holds. Somebody whose access method is stolen removes it and signs out the browsers they do not recognise; those are two controls the settings screen already offers, and the second is the one that ends access.

**Per-app panels are bounded and floored.** At most twenty series per family, chosen by that family's own value in the window, and only for apps at or above one hundred; everything else sums into one `other` bucket. Any per-app value that can _decrease_ is additionally rounded down to a multiple of ten, because a scrape-to-scrape delta near the floor would otherwise isolate one person leaving.

This matters more than it looks: `/metrics` is served from a query with no caller check, it is public, and whoever scrapes it archives it permanently. Today's canister-side `take(10)` is doing this privacy work by accident; it should be doing it on purpose.

Scrape cost is not the reason. Almost every value on the endpoint is a stored counter read directly; the per-app families read rows that `event_aggregations` maintains as events arrive, and rank them at scrape rather than recomputing them. Two MCP gauges do scan their map per scrape and say so in their own help text, over 85 and 1 entries respectively. The floor is a privacy measure and stands on that alone.

**Four of today's panels die with nothing replacing them.** Bounce Rate, both cumulative-session-length panels, and Registration Rates. Three more die by merging into a panel that answers the same question better.

**No sweep.** REC-8 states that nothing may require a periodic sweep across identities, which rules out the obvious way to make expiry observable. Everything above is therefore built from creation, use, and the paths that already rewrite a row.

That constraint is also not unusual. Okta documents the same limitation in the same words — a session expiry does not appear in its logs unless the user signed out or the session was revoked — so building from creation and use is the mainstream answer rather than a concession.

**Refreshes do not count as activity.** Only the ceremony records identity activity; the mint path authenticates by session chain and never reaches that code. So the active-identity numbers will not inflate as sessions roll out, which is the trap a service counting token refreshes as activity walks into.

## What is deliberately not here

Anything measurable from the browser belongs to Plausible: bounce rate, funnels, page views, drop-off, and which II domain somebody actually visited. Two definitions of one number drift apart, and the drifted one is always the one on the dashboard.

Silent re-auth outcomes are the one genuinely desirable rollout number that is not canister-observable. The decision is made in the frontend and the canister sees an ordinary request.

Session lifetime observed at removal, because expiry writes nothing and nothing sweeps — any such histogram is drawn from sessions somebody came back to, and would report longer lives the more people abandoned.

Latency from inside the canister, which is not merely expensive but impossible: block time is constant for a whole message execution, so a duration measured there is identically zero. Instructions are what the canister can honestly report.

Latency itself is not out of reach, though, and this document previously implied it was. The boundary-node access log is already a data source on the same Grafana — it is what the deleted bounce-rate panel read — and it carries per-method rows. Availability and latency percentiles for the sign-in and session methods belong there, measured at the edge, which is where every comparable service measures them. Deleting the one panel that touched that source without repurposing it was the wrong instinct.
