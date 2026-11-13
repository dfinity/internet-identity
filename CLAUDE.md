# CLAUDE.md

Guidance for AI coding agents (e.g. Claude Code) working in this repository.
For the human-facing docs, see [CONTRIBUTING.md](CONTRIBUTING.md) and
[HACKING.md](HACKING.md). `FLOWS.mdx` is out of date: treat the Svelte routes
as the source of truth for user flows.

## Rules

Conventions live in `.claude/rules/`, one file per topic. Most are scoped with
`paths:` frontmatter, so they load only when the files they govern are in play.

| File | Applies to | Covers |
| --- | --- | --- |
| `pull-requests.md` | everything | local checks, scope, titles, descriptions, stacks, CI, reviews |
| `code-style.md` | everything | naming, comments and doc comments, scope, reuse |
| `handle-all-type-variants.md` | everything | exhaustive handling of candid variants, Rust enums, TS unions |
| `security.md` | `src/**` | bounded resources, authorization, origins, delegations, leaks |
| `backend.md` | canister crates, `src/canister_tests/**` | no trapping, state and `await`, layering, Rust style, clippy, PocketIC |
| `candid-api.md` | `*.did`, interface crate, generated bindings | API evolution, shape, error design, bindings |
| `stable-memory.md` | storage, state, upgrade tests | record formats, memory indices, migrations, upgrades |
| `frontend.md` | `src/frontend/**` | design system, Svelte, async and errors, TypeScript, accessibility |
| `i18n-and-copy.md` | frontend sources, locales | translations, `<Trans>`/`$t`, UI copy |
| `testing.md` | tests, Rust sources | where coverage goes, assertions that can fail |
| `e2e-testing.md` | `src/frontend/tests/**`, `scripts/dev-e2e*`, `playwright.config.ts` | the dev-e2e stack, writing Playwright tests |
| `scripts-and-ci.md` | `scripts/**`, `.github/**`, manifests | shell scripts, workflows, dependencies, feature flags |

The rules are distilled from review discussions on this repository's pull
requests. Add a new convention to the file for its topic, or as a new file with
`paths:` frontmatter, rather than expanding this one.
