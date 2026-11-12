# Commits, pull requests and reviews

## Local checks

Run the checks CI runs yourself; hooks aren't installed in fresh clones, containers or CI. Don't hold a push back for the slow ones: push, run them locally while CI runs, and push a fix for anything either one catches.

- Before committing, since they are fast and a miss is a guaranteed red run: `cargo fmt` and `npm run format`; `npm run generate` after a `.did` change (see `candid-api.md`); revert any `src/frontend/src/lib/locales/*.po` diff (see `i18n-and-copy.md`).
- After pushing, in parallel with CI: `cargo clippy` with the flags in `backend.md`; `npm run lint:eslint -- --fix`, `npm run lint`, `npm run check`. CI stops at the first failure, so fix all three.

Never add "Generated with Claude" or similar attribution footers to commits, PR descriptions, review replies or code.

## Branches

Open PRs from a branch in `dfinity/internet-identity`, not from a fork, so maintainers can push to it. Keep the branch current with `main`: an unrelated file in the diff usually means it is stale.

## Scope and size

- One concern per PR. Refactors, renames, formatting, dependency bumps and CI changes go in separate PRs. Revert unrelated wording, default or behaviour changes; every line of the diff should be justifiable by the PR's purpose.
- Keep PRs small enough to review in one sitting (roughly under 500 lines of implementation). Split larger work into a stack (see Stacked PRs).
- Settle design questions in a design note before the implementation PR.
- When porting code from a library, link the exact upstream revision and list every change made.

## Titles

PR titles follow conventional commits with a scope naming the product surface that changed, which decides the changelog the squashed commit lands in. It is not about the language: the frontend canister is Rust and candid but still `fe`.

- `feat(be)`: the backend II canister only: `src/internet_identity` and its candid.
- `feat(fe)`: the frontend: TS/JS/Svelte, and the frontend canister (`src/internet_identity_frontend`, its `.did` and `main.rs`) plus its init-args type `InternetIdentityFrontendArgs` in `src/internet_identity_interface`.
- `feat(be,fe)`: both surfaces.
- The same for `fix`, `chore`, `refactor`, `docs`, `test`. The type matches the change (`feat`, not `test`, for a feature with tests).

`src/internet_identity_interface` is shared, so scope a change there by the canister the type serves (`InternetIdentityFrontendArgs` is `fe`, `InternetIdentityInit` is `be`). The upgrade-proposal generator (`scripts/make-upgrade-proposal`) routes squashed commits to the backend or frontend proposal by this scope, sending unscoped `feat` commits to both. Release notes are split by changed paths instead. The scope must match the diff: a `feat(be)` PR has no Svelte changes beyond generated bindings. Commit titles inside a PR can use any scope, since PRs are squash-merged.

## Descriptions

- Write the title and description from the user's perspective: "Fix multiple accounts toggle staying on after switching identity", not "Reset continue screen state when switching identity".
- Follow the template: Motivation (no heading, just the text), `# Changes`, `# Tests`. Include Tests only for new or updated tests, and state what was actually tested ("manually exercised" when nothing automated covers it).
- Explain why, not only what: the reason for a design choice or for any added, removed or changed dependency goes here, not in a code comment.
- Attach screenshots or a video for every UI change.
- Keep the description in sync with the code. After every push that changes scope or approach, update it with `gh pr edit` without being asked, and re-read it for claims that are no longer true.
- Call out any breaking change and any behaviour change outside the PR's stated purpose.

## Stacked PRs

- Put each change in the layer that introduces it. Don't add something in one PR and rename or remove it in a later one; fix it in the layer where it first appears.
- Every layer must build and pass CI on its own, so a symbol is defined in the same PR as its first use.
- Don't ship a change that breaks e2e ahead of the PR that consumes it.
- Create and maintain stacks with the `gh stack` CLI extension (`gh extension install github/gh-stack`) through the `gh-stack` skill: create, push, rebase, sync and submit with it, never with raw `git rebase` or `gh pr create --base`. `gh stack submit` links the PRs with its own footer, so don't add Previous/Next footers by hand.

## CI

After pushing, watch the PR's checks alongside the local checks and fix failures proactively. Read a failure's logs before rerunning it, and land a fix together with its regression test; never leave `main` red.

## Reviews

- Copilot is auto-assigned on PRs targeting `main`. For a PR in a stack, request it through GraphQL (the REST `requested_reviewers` endpoint returns 200 but adds nobody):
  ```bash
  pr=$(gh pr view <NUMBER> --json id --jq .id)
  gh api graphql -f query='mutation($pr:ID!,$bot:ID!){
    requestReviews(input:{pullRequestId:$pr, botIds:[$bot], union:true}){ clientMutationId } }' \
    -f pr="$pr" -f bot="BOT_kgDOCnlnWA"
  ```
- Treat bot findings (Copilot, CodeQL, ZeroPath) as claims to verify against the code and framework docs; they are often wrong here (Tailwind v4 syntax, Svelte 5 semantics, invented config). Fix real ones, and reply to false positives with the concrete reason (`@ZeroPath false-positive because ...`). Never present an unverified claim as fact.
- When a reviewer says "ditto elsewhere" or flags a pattern, sweep the whole PR for every instance, not only the commented line.
- Reply in every thread you address, naming the commit, then resolve it.
- After pushing changes that address a reviewer's feedback, re-request their review: GitHub doesn't do it once they've commented or requested changes, and the PR stalls.
- Don't merge while any review thread is open, even with approvals. Wait for an explicit approval from every reviewer who engaged.
- Test risky changes (storage formats, auth flows, verifier changes) on a staging deployment before merging.

## Writing

- Bullet points that are complete sentences end with punctuation.
- Always include `https://` in URLs so they render as links.
