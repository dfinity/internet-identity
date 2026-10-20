# CLAUDE.md

Guidance for AI coding agents (e.g. Claude Code) working in this repository.
For the human-facing docs, see [CONTRIBUTING.md](CONTRIBUTING.md) and
[HACKING.md](HACKING.md).

## Rules

Conventions live in `.claude/rules/`, one file per topic. Most are scoped to the
paths they apply to via `paths:` frontmatter, so they load only when the files
they govern are in play:

| File | Applies to |
| --- | --- |
| `workflow.md` | all files — formatting, PR titles and descriptions, stacked PRs, CI and reviews |
| `handle-all-type-variants.md` | all files — exhaustive handling of candid variants, Rust enums, TS unions |
| `backend.md` | `src/internet_identity*/**`, `src/asset_util/**` — no-trap rule, clippy flags, PocketIC tests |
| `frontend.md` | `src/frontend/**` — design tokens, buttons, i18n, Svelte and TypeScript style |
| `e2e-testing.md` | `src/frontend/tests/**`, `scripts/dev-e2e*`, `playwright.config.ts` — the dev-e2e stack |

Add a new convention as a file in `.claude/rules/` rather than expanding this
one, and give it `paths:` frontmatter unless it genuinely applies everywhere.
