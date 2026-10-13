# Workflow

## Formatting and linting

Before committing:

- Rust: `cargo fmt`, `cargo clippy` (see `.claude/rules/backend.md` for the full clippy flags).
- JS/TS/Svelte: `npm run format`, `npm run lint -- --fix`.

Run these even if a pre-commit hook would normally do it — hooks aren't installed in fresh clones, containers, or CI.

## Code style

- Prefer early-return guard clauses over nested if/else. Flatten the happy path.
- Comments must describe the current state of the code — never reference removed code, previous implementations, or what's deliberately absent. History belongs in git.

## Full-stack mindset

- Don't limit fixes to frontend or backend only. When a behavior change is needed, consider whether the fix belongs in the Rust canister code, the frontend handler, the test app, or a combination. Fix the root cause in the right layer.

## PR titles

PR titles must follow conventional-commit format with a scope naming the affected side(s). The scope tracks which *product surface* changed (which changelog the commit lands in), **not** which language -- the frontend canister is Rust + candid but still counts as `fe`:

- `feat(be)` -- the backend II canister only: `src/internet_identity` (auth/storage logic) and its candid.
- `feat(fe)` -- the frontend: TS/JS/Svelte, **and** the frontend canister (`src/internet_identity_frontend`, including its `.did` and `main.rs`) plus its init-args type `InternetIdentityFrontendArgs` in `src/internet_identity_interface`.
- `feat(be,fe)` -- touches both surfaces.
- Same pattern for `fix(...)`, `chore(...)`, `refactor(...)`, `docs(...)`, `test(...)`.

`src/internet_identity_interface` is shared, so scope a change to it by which canister the changed type serves (e.g. `InternetIdentityFrontendArgs` -> `fe`, `InternetIdentityInit` -> `be`), not by the crate.

Release scripts split squash-merged commits across separate frontend and backend changelogs by parsing the scope. A PR without a `be`/`fe` scope breaks the split, and a frontend-canister change mistagged `be` lands in the wrong changelog. Commit titles inside the PR can use semantic scopes since they get squashed away.

## PR descriptions

- Write PR titles and descriptions from the user's perspective. Instead of "Reset continue screen state when switching identity", write "Fix multiple accounts toggle staying on after switching identity". Describe what the user sees going wrong and how it's fixed.
- When changes are pushed to an open PR, always update the PR description to reflect the current state. Don't wait to be asked. Use `gh pr edit` after pushing changes that affect scope.
- PR template has three sections: Motivation, Changes, Tests. Motivation should NOT have a heading -- just the text. Only include Tests if there are new or updated tests.

## Stacked PRs

When creating stacked PRs, add a footer section linking them together:

- First PR: `<div align="right">Next: #XXXX</div>`
- Middle PR: `<div align="left">Previous: #XXXX</div>` and `<div align="right">Next: #XXXX</div>`
- Last PR: `<div align="left">Previous: #XXXX</div>`

Update all PRs in the stack when a new one is added.

## CI and reviews

- After pushing changes to a PR, monitor the CI pipeline for failures and fix them proactively. Poll with a loop that emits one event per check as it resolves and exits when the run completes:
  ```bash
  prev=""
  while true; do
    s=$(gh pr checks <PR> --json name,bucket)
    cur=$(jq -r '.[] | select(.bucket!="pending") | "\(.name): \(.bucket)"' <<<"$s" | sort)
    comm -13 <(echo "$prev") <(echo "$cur")
    prev=$cur
    jq -e 'all(.bucket!="pending")' <<<"$s" >/dev/null && break
    sleep 30
  done
  ```
  Events stream in as each check lands; the loop exits when the run completes.
- For PRs targeting `main`, Copilot is auto-assigned as reviewer by repo rules. For stacked PRs, request Copilot via the REST API:
  ```bash
  gh api -X POST repos/OWNER/REPO/pulls/NUMBER/requested_reviewers --raw-field 'reviewers[]=Copilot'
  ```
  Wait for Copilot feedback and address valid suggestions before considering the PR ready.
- After pushing changes that address a reviewer's feedback, re-request a review from that reviewer — GitHub does not re-request automatically once a reviewer has left comments or requested changes, so without this the reviewer has to notice the update on their own and the PR can stall.

## Writing

- Bullet points that are complete sentences must end with punctuation.
- Always include `https://` in URLs so they render as clickable links.
