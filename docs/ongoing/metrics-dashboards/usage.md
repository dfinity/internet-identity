# Usage

[Dashboards](README.md) · [Health](health.md) · **Usage** · [Staying signed in](staying-signed-in.md) · [Access methods](access.md) · [Storage and capacity](storage.md)

How much it is used, by how many people, on which apps, and how far the session rollout has got. The page to open before a planning conversation.

Everything here is a volume. Whether people come back is a different question and has [its own page](staying-signed-in.md). Nothing here is labelled by how anyone authenticated either — that is [Access methods](access.md), and a usage number should not move when the mix does. Reasoning for each panel is in [metrics.md](../metrics.md).

## Sign-ins per hour

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

## Share of sign-ins that created a session

`sum(rate(internet_identity_sign_ins_total{flow="session"}[1d])) / sum(rate(internet_identity_sign_ins_total[1d]))`

The adoption question in one line, and the number to report when somebody asks whether the feature works. It needs one counter written on both sign-in paths: dividing by the existing `prepare_delegation_count` would compare two disjoint populations and exceed one as adoption grew.

```mermaid
xychart-beta
  title "Share of sign-ins that created a session"
  x-axis "week" [w1, w2, w3, w4, w5, w6, w7, w8]
  y-axis "% of sign-ins" 0 --> 100
  line [2, 9, 21, 38, 52, 63, 71, 76]
```

## Sign-ins per app

`topk(10, sum by (dapp) (increase(internet_identity_sign_ins_total[24h])))`

Which apps carry the traffic. A ranking rather than a time series, because a ranking is what people read off it, and one panel replaces the 24-hour and 30-day pair: once sign-ins come from a counter, Prometheus computes any window and the dashboard's time picker chooses it.

Today's panels are fed only by `prepare_account_delegation`, so a migrating app's line falls to zero while its usage is flat. They also filter on `ii_origin`, which scopes them to identities holding a passkey registered on one domain: 190 of 3,627. The replacement carries `dapp` and the flow, and nothing about how anyone authenticated.

```mermaid
xychart-beta
  title "Sign-ins per app, last 24 hours, both flows"
  x-axis "app" ["app-a", "app-b", "app-c", "app-d", "app-e", "app-f"]
  y-axis "sign-ins" 0 --> 600
  bar [510, 240, 180, 120, 90, 60]
```

## Identities per app

`topk(10, internet_identity_identities_per_app)`

Identities that have ever signed in to each app: reach rather than current traffic. Already stored as a count on each application record, so publishing it is a read and a sort.

Useful beside the panel above, since an app with many identities and little traffic is one people signed up for and left.

```mermaid
xychart-beta
  title "Identities that have signed in, ten largest apps"
  x-axis "app" ["app-a", "app-b", "app-c", "app-d", "app-e", "app-f"]
  y-axis "identities" 0 --> 60000
  bar [51000, 33000, 21000, 14000, 9000, 6500]
```

## Daily and monthly active identities

`internet_identity_daily_active_anchors` and `internet_identity_monthly_active_anchors`

Identities that took an authenticated action in the window. Live: 3,627 daily, 38,079 monthly.

The fix is removing the by-domain series plotted beside the total. They sum to 2,137 against 3,627, because a domain is recorded only from the authenticating passkey and OpenID credentials carry none — so they count passkeys by the domain they were registered on, not people by the domain they use. How somebody authenticated belongs on [Access methods](access.md); which domain a browser visited is measurable from the browser and belongs in Plausible.

```mermaid
xychart-beta
  title "Daily and monthly active identities, live values"
  x-axis "window" ["daily", "monthly"]
  y-axis "identities" 0 --> 40000
  bar [3627, 38079]
```

## Identities and registrations

`internet_identity_user_count`, and `increase(internet_identity_user_count[1d])`

Total identities ever created, and the rolling daily delta. Live: 3,222,895 and about 1,100 a day. The second panel is titled "Registrations the last 24h" and is a rolling window across the whole range, so it should say so.

```mermaid
xychart-beta
  title "Registrations per day, rolling 24 hours"
  x-axis "day" [d1, d2, d3, d4, d5, d6, d7]
  y-axis "identities created per day" 0 --> 1400
  line [840, 700, 610, 640, 900, 1180, 1120]
```

## Identity changes per hour

`rate(internet_identity_anchor_operations_counter[5m]) * 3600`

Mutation volume: adding a passkey, renaming a device, linking an OpenID credential. Live: 7,453 since the last upgrade.

The metric is a gauge that resets on upgrade, so `rate()` over it drops whatever accumulated before each deploy. Moving the counter into persistent state is the fix, and it is the same groundwork every new counter on this dashboard needs.

```mermaid
xychart-beta
  title "Identity changes per hour"
  x-axis "hour" [h1, h2, h3, h4, h5, h6, h7, h8]
  y-axis "operations per hour" 0 --> 400
  line [210, 240, 225, 260, 310, 280, 245, 220]
```
