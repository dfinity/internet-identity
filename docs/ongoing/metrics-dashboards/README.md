# Proposed dashboards

A walk-through of the dashboards proposed in [metrics.md](../metrics.md), split by the question each answers. Today all twenty-four panels sit on one page, which is why several have gone unread for long enough to break unnoticed.

Every chart here is a sketch. Numbers marked live come from the production endpoint on 2026-08-26; the rest are shapes.

| Dashboard                                 | Answers                                                    | Panels |
| ----------------------------------------- | ---------------------------------------------------------- | ------ |
| [Health](health.md)                       | Is anything broken right now, and would we be paged for it | 7      |
| [Usage](usage.md)                         | How much it is used, by how many people, on which apps     | 7      |
| [Staying signed in](staying-signed-in.md) | Whether people come back, how often, and how it ends       | 9      |
| [Access methods](access.md)               | Which methods people authenticate with, and how that moves | 2      |
| [Storage and capacity](storage.md)        | Where the memory goes and when anything runs out           | 6      |

This is the dashboard we want, not a plan for getting there. Some of it is a legend fix on a panel that already exists; some needs a counter the canister does not keep yet; three panels need something that walks the session rows, which nothing does today. Sorting that into now, later and much later is a separate conversation, and [metrics.md](../metrics.md) has what each one costs.

Every panel on [Staying signed in](staying-signed-in.md) was checked against the session implementation rather than the design docs, because the two differ in ways that matter: sign-in never reuses a session, and a session expiring writes nothing anywhere. That page opens with what the canister can actually observe, and every panel on it is built only from creation and use.

## Why split

Seven of the twenty-four panels are wrong today, and one of them has been reporting a constant. A single page of twenty-four panels is scrolled past rather than read, and a panel nobody reads is a panel nobody notices is wrong.

The split also matches how they would be used. Health is the page to open when something is reported. Usage and Staying signed in are the pages to open before a planning conversation, and they answer different halves of it: how much, and whether it sticks. Access methods and Storage are opened when a specific question comes up, which is rarely.

It also keeps two questions apart that the current dashboard runs together. How much an app is used and how the people using it authenticated are separate concerns, so no usage panel is labelled by access method, and the mix has a page of its own. That is why the per-app panels lose their `ii_origin` filter rather than getting a better one, and why the active-identity panels lose their by-domain series.

Alerting is only on [Health](health.md), and only on two of its panels.
