# AGENTS.md

Guidance for AI coding agents working in this repository.
For the human-facing docs, see [CONTRIBUTING.md](CONTRIBUTING.md) and
[HACKING.md](HACKING.md). `FLOWS.mdx` is out of date: treat the Svelte routes
as the source of truth for user flows.

## Rules

Conventions live in `.claude/rules/`, one file per topic. Most are scoped with
`paths:` frontmatter: Claude Code loads them only when the files they govern are
in play. Other agents: before changing files, read every file in `.claude/rules/` whose
`paths:` frontmatter matches them; a file without `paths:` applies everywhere.
The table below is a summary, and the frontmatter is the scope definition.

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

## Rule IDs

Every rule starts with a stable ID, `<area>/<slug>` (`be/no-trap`,
`sec/no-secrets-in-urls`), so reviews and PR descriptions can cite it. The
area matches the file: `be`, `fe`, `candid`, `mem`, `sec`, `e2e`, `test`,
`style`, `i18n`, `ci`, `pr`, `variants`.

- One ID per rule. A bullet that grows a second requirement is split, and the new rule gets its own ID.
- A slug is kebab-case and names what the rule requires. IDs are not numbered, so adding a rule never shifts another.
- Never reuse a removed ID. A renamed rule keeps its old ID next to the new one: `` `be/new-slug` (was `be/old-slug`) ``.

## Keep the rules current

These rules are how a correction reaches the next agent. Update `.claude/rules/` when:

- The user or a reviewer corrects how something should be done, and it's a convention rather than a one-off.
- A review comment states a general expectation ("we always…", "don't…", "ditto elsewhere") rather than fixing one line, so it would apply to the next PR too.
- Your change makes a rule wrong: a renamed symbol, a moved path, a replaced pattern.
- You notice a rule that contradicts the code or another rule.

Make the update in the same PR when it belongs to that change. Otherwise, propose it to the user before you finish. List every rule you added, changed or removed in the PR description by its ID, in its own section, so reviewers see it. Don't keep a repo convention only in your own memory: it never reaches a teammate's agent.
