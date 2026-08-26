# Adoption and usage

[Dashboards](README.md) · [Health](health.md) · **Adoption and usage** · [Apps](apps.md) · [Storage and capacity](storage.md)

Is it being used, by how many people, and for how long. The page to open before a planning conversation. Reasoning for each panel is in [metrics.md](../metrics.md).

## Sign-ins per hour · replaces Logins per Hour

`sum by (flow) (rate(internet_identity_sign_ins_total[5m])) * 3600`

A sign-in is one identity being granted access to one app, so this is larger than a count of people. Split by flow, the same panel shows the session rollout.

Today's panel reads `delegation_counter`, a gauge that returns to zero on upgrade and which the session flow does not touch at all, so it both notches at every deploy and goes quiet as apps migrate.

```mermaid
xychart-beta
  title "Sign-ins per hour, session flow"
  x-axis "hour" [h1, h2, h3, h4, h5, h6, h7, h8]
  y-axis "sign-ins per hour" 0 --> 500
  line [40, 60, 85, 120, 160, 200, 240, 270]
```

```mermaid
xychart-beta
  title "Sign-ins per hour, delegation flow"
  x-axis "hour" [h1, h2, h3, h4, h5, h6, h7, h8]
  y-axis "sign-ins per hour" 0 --> 500
  line [270, 270, 215, 175, 130, 120, 60, 40]
```

## Share of sign-ins that created a session · new

`sum(rate(internet_identity_sign_ins_total{flow="session"}[1d])) / sum(rate(internet_identity_sign_ins_total[1d]))`

The adoption question in one line, and the number to report when somebody asks whether the feature works. It needs one counter written on both sign-in paths: dividing by the existing `prepare_delegation_count` would compare two disjoint populations and exceed one as adoption grew.

```mermaid
xychart-beta
  title "Share of sign-ins that created a session"
  x-axis "week" [w1, w2, w3, w4, w5, w6, w7, w8]
  y-axis "% of sign-ins" 0 --> 100
  line [2, 9, 21, 38, 52, 63, 71, 76]
```

## How long sign-ins last · new · replaces two panels

`histogram_quantile(0.5, sum by (le) (rate(internet_identity_session_age_seconds_bucket[7d])))`

Observed when a sign-in ends, because that is the last moment anything knows how old it was. Replaces both cumulative-session-length panels, which sum the lifetimes delegations were _requested for_ and render them in years.

Sign-ins that ran the full 30 days all land in the top bucket, so quantiles above that share are meaningless. Sign-ins nobody returns to are removed late, so they are under-represented.

```mermaid
xychart-beta
  title "How long sign-ins lasted, last 30 days"
  x-axis "lifetime" ["0-5m", "5m-1h", "1-6h", "6-24h", "1-3d", "3-7d", "7-14d", "14-30d", "full 30d"]
  y-axis "sign-ins ended" 0 --> 30000
  bar [1800, 4200, 7600, 12000, 19000, 24000, 15000, 8000, 9500]
```

## How sign-ins ended · new

`sum by (reason) (increase(internet_identity_sessions_ended_total[30d])) / scalar(sum(increase(internet_identity_sessions_ended_total[30d])))`

A deliberate sign-out is somebody leaving; running the full term is somebody who stopped coming back. This is also the only measure of whether the settings screen is used: if almost nothing ends by a revocation from settings, the design's central promise is going unexercised.

```mermaid
xychart-beta
  title "How sign-ins ended, last 30 days"
  x-axis "ended by" ["signed out in app", "revoked in settings", "signed out of II", "ran full term", "dropped at a cap"]
  y-axis "% of sign-ins ended" 0 --> 60
  bar [46, 7, 3, 42, 2]
```

## Daily and monthly active identities · fix

`internet_identity_daily_active_anchors` with `..._by_domain`, plus the monthly pair

Identities that took an authenticated action in the window. Live: 3,627 daily, 38,079 monthly.

The fix is publishing the remainder. The per-domain parts sum to 2,137 against a total of 3,627, because a domain is recorded only when the authenticating device carries one and OpenID credentials do not. The 1,490 difference matches the 1,485 daily active OpenID identities.

```mermaid
xychart-beta
  title "Daily active identities by domain, with the remainder published"
  x-axis "domain" ["id.ai", "no domain", "identity.ic0.app", "both", "internetcomputer.org"]
  y-axis "identities" 0 --> 4000
  bar [1893, 1490, 190, 34, 20]
```

## Identities and registrations · keep

`internet_identity_user_count`, and `increase(internet_identity_user_count[1d])`

Total identities ever created, and the rolling daily delta. Live: 3,222,895 and about 1,100 a day. The second panel is titled "Registrations the last 24h" and is a rolling window across the whole range, so it should say so.

```mermaid
xychart-beta
  title "Registrations per day, rolling 24 hours"
  x-axis "day" [d1, d2, d3, d4, d5, d6, d7]
  y-axis "identities created per day" 0 --> 1400
  line [840, 700, 610, 640, 900, 1180, 1120]
```

## Traffic per signed-in user · new

`sum(rate(internet_identity_app_delegation_requests_total{outcome="served"}[1d])) * 86400 / internet_identity_daily_active_sessions`

Delegation requests per active sign-in per day. At a five-minute credential this is really a measure of how long apps stay open, and it is the figure to put beside any proposal to change those five minutes. Requests are a proxy for cost, not cost.

```mermaid
xychart-beta
  title "Delegation requests per active sign-in per day"
  x-axis "day" [mon, tue, wed, thu, fri, sat, sun]
  y-axis "requests per sign-in" 0 --> 40
  line [27, 28, 29, 28, 28, 23, 24]
```
