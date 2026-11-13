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
| `frontend.md` | `src/frontend/**` | design system, Svelte, async and errors, TypeScript, the `/authorize` state machine, accessibility |
| `i18n-and-copy.md` | frontend sources, locales | translations, `<Trans>`/`$t`, UI copy |
| `testing.md` | tests, Rust sources | where coverage goes, assertions that can fail |
| `e2e-testing.md` | `src/frontend/tests/**`, `scripts/dev-e2e*`, `playwright.config.ts` | the dev-e2e stack, writing Playwright tests |
| `scripts-and-ci.md` | `scripts/**`, `.github/**`, manifests | shell scripts, workflows, dependencies, feature flags |

The rules are distilled from review discussions on this repository's pull
requests. Add a new convention to the file for its topic, or as a new file with
`paths:` frontmatter, rather than expanding this one.

## Keep the rules current

These rules are how a correction reaches the next agent. Update `.claude/rules/` when:

- The user or a reviewer corrects how something should be done, and it's a convention rather than a one-off.
- A review comment states a general expectation ("we always…", "don't…", "ditto elsewhere") rather than fixing one line, so it would apply to the next PR too.
- Your change makes a rule wrong: a renamed symbol, a moved path, a replaced pattern.
- You notice a rule that contradicts the code or another rule.

Make the update in the same PR when it belongs to that change. Otherwise, propose it to the user before you finish. Say which rule you added or changed. Don't keep a repo convention only in your own memory: it never reaches a teammate's agent.
