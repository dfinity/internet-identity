# Proposed dashboards

A walk-through of the dashboards proposed in [metrics.md](../metrics.md), split by the question each answers. Today all twenty-three panels sit on one page, which is why several have gone unread for long enough to break unnoticed.

Every chart here is a sketch. Numbers marked live come from the production endpoint on 2026-08-26; the rest are shapes.

| Dashboard                          | Answers                                                    | Panels |
| ---------------------------------- | ---------------------------------------------------------- | ------ |
| [Health](health.md)                | Is anything broken right now, and would we be paged for it | 6      |
| [Adoption and usage](usage.md)     | Is it being used, by how many people, and for how long     | 7      |
| [Apps](apps.md)                    | Which apps carry the traffic, and how people authenticate  | 3      |
| [Storage and capacity](storage.md) | Where the memory goes and when anything runs out           | 6      |

Twenty-two panels over four pages, against twenty-three on one page today. Five of today's are deleted and four are new; the count barely moves, and no page is longer than a screen.

## Why split

Three of the panels that were broken had been broken for months. A single page of twenty-three panels is scrolled past rather than read, and a panel nobody reads is a panel nobody notices is wrong.

The split also matches how they would be used. Health is the page to open when something is reported. Adoption is the page to open before a planning conversation. Apps and Storage are opened when a specific question comes up, which is rarely.

Alerting is only on [Health](health.md), and only on two of its panels.
