You are running in CI, triggered by a reviewer comment or review on a pull request. Address the feedback on PR `#$PR_NUMBER` in the `dfinity/internet-identity` repository.

Follow the rules in `src/frontend/src/lib/locales/CLAUDE.md`. Read that file before applying any fixes.

You are committing as `gix-bot` — replies to comments and all git commits must be authored by that identity. "Already replied" checks must look for responses from `gix-bot`.

## Step 0: Determine PR kind

Fetch `gh pr view $PR_NUMBER --repo dfinity/internet-identity --json title,state,mergedAt,merged,headRefName` and decide:

- **Rule-proposal PR** — title starts with `chore(fe): propose translation rule`. Skip every `.po` / `npm run extract` / `npm run format` step below. Apply requested text edits directly to the proposed rule file under `src/frontend/src/lib/locales/rules/` on the PR branch, commit and push. If the reviewer is asking for the rule to be moved to a different file (e.g. rescoped from general to language-specific), do that and update the `@rules/...` import in `src/frontend/src/lib/locales/CLAUDE.md` accordingly. Still reply to every allowed-user comment.
- **Translation PR (open)** — title matches `chore(fe): update <Language> translations` and PR is open. Push fixes to the PR branch (Step 2a).
- **Translation PR (merged within 14 days)** — same title, PR is merged. Open a NEW translation PR on a fresh branch with the fixes (Step 2b), then reply on the merged PR with a link.
- **Context-audit PR (open)** — title starts with `chore(fe): add translation context` and PR is open. Refinements here are changes to the `context` values in `.svelte` files (e.g. a reviewer suggests a clearer context phrase, or flags a missed ambiguous string). Follow Step 2a, but note: edits go to `.svelte` sources, and you must re-run `npm run extract` and `npm run format` so the `.po` files stay in sync. Stage both the `.svelte` changes and the updated `.po` files. Do not modify translations in this flow.
- **Context-audit PR (merged within 14 days)** — rare; if a reviewer flags an issue on a merged context-audit PR, reply with a link to the `translations-context-audit` workflow and ask a maintainer to re-dispatch it so a fresh sweep can incorporate the feedback. Do not open a one-off follow-up PR.

If the title matches none of the above, stop and report "Not a translation-related PR".

## Step 1: Fetch feedback

Consider only comments from these users (ignore everyone else, including yourself):

`AntonioVentilii`, `aterga`, `sea-snake`, `marc0olo`, `mducroux`, `copilot-pull-request-reviewer`, `copilot-pull-request-reviewer[bot]`, `Copilot`.

Pull all three streams and treat each entry as a "comment":

- Inline review comments: `gh api repos/dfinity/internet-identity/pulls/$PR_NUMBER/comments`
- Issue-style PR comments: `gh api repos/dfinity/internet-identity/issues/$PR_NUMBER/comments`
- Review bodies: `gh api repos/dfinity/internet-identity/pulls/$PR_NUMBER/reviews` — each review's `body` field (when non-empty) is itself a comment to address. Reply to it with `gh api repos/dfinity/internet-identity/issues/$PR_NUMBER/comments -X POST -f body=...`, referencing the review id in the text.

A comment is **unaddressed** if there is no reply from `gix-bot` or a bot acknowledging the fix.

Every comment from the allowed users MUST get a reply — even a simple acknowledgement when no code change is needed. Always respond to feedback.

If none of the recent comments are unaddressed, stop here and report "No unaddressed feedback".

## Step 2a: Apply fixes to an open translation PR

For each unaddressed comment on an open translation PR:

1. Fetch the PR branch and check it out in a worktree:

   ```
   git fetch origin <headRefName>
   git worktree add ../feedback-<headRefName> <headRefName>
   cd ../feedback-<headRefName>
   ```

2. Apply the requested translation fix to the relevant `.po` file.
3. Run `npm run extract` to clean up, then `npm run format`.
4. Stage only the relevant `.po` file — do not stage other files that `npm run extract`/`npm run format` may have modified.
5. Commit and push to the PR branch.
6. Reply to the comment confirming the fix was applied.
7. If the comment is from `copilot-pull-request-reviewer`, `copilot-pull-request-reviewer[bot]`, or `Copilot`, resolve the review thread after replying (use `gh api repos/dfinity/internet-identity/pulls/comments/<comment_id>/resolve -X PUT` or the GraphQL `resolveReviewThread` mutation).

## Step 2b: Create a new PR for fixes to a merged translation PR

You cannot push to a merged branch. Instead, open a new translation PR:

1. From the latest `main`, create a branch `chore/translate-<lang>` (where `<lang>` is the language of the merged PR). If that branch already exists as an open PR, add the fix to that existing PR instead of opening another.
2. Apply the requested translation fix(es) to the `.po` file.
3. Run `npm run extract` then `npm run format`, stage only the relevant `.po` file, commit and push.
4. Open the PR with the body format from the check prompt, noting the source: `- Fixed translations based on reviewer feedback from #<merged PR number>`.
5. Add reviewers per the language mapping in the check prompt (always `aterga`, plus language-specific where applicable).
6. Reply to each original feedback comment on the merged PR with a link to the new PR (e.g. "Fixed in #1234").
7. Resolve Copilot threads as in Step 2a.

## Step 3: Classify each comment

- **Narrow**: a fix to a specific translation. Nothing further.
- **Broad**: feedback that implies a pattern or rule applicable to future translations (e.g. "always use formal 'you' in German", "prefer X terminology over Y", "don't translate brand names").

Skip this step when handling comments on a rule-proposal PR — broad/narrow distinction does not apply there.

## Step 4: Rule-proposal PR for broad feedback

For each broad comment, create one rule-proposal PR (branched from latest `main`, not from the feedback PR):

1. Create a branch `chore/translation-rule-<short-slug>`.
2. Rules live under `src/frontend/src/lib/locales/rules/` — one file per topic, imported by `src/frontend/src/lib/locales/CLAUDE.md`. Choose the target file:
   - **General rule** (applies to all languages) → append to `rules/general.md`.
   - **Language-specific rule** → append to `rules/<language>.md` (e.g. `rules/russian.md`, `rules/dutch.md`). If that file does not exist yet, create it and add a new `@rules/<language>.md` line under the "Language-specific" section of `CLAUDE.md`.
3. The rule text should be self-contained and prescriptive — do not include metadata like dates or source links; that lives in the PR description and git history.
4. Commit and push. When creating a new `rules/<language>.md`, stage both the new file and the `CLAUDE.md` import update in the same commit.
5. Open a PR with `gh pr create`:

   Title: `chore(fe): propose translation rule — <one-line summary>`

   Body:

   ```
   Rule proposed based on reviewer feedback from #<source PR number>.

   # Changes

   - Added translation rule to `src/frontend/src/lib/locales/CLAUDE.md`

   # Context

   Reviewer: @<username>
   Original comment: <quoted comment text>
   ```

6. Request review from `aterga` and `sea-snake` with `gh pr edit <number> --add-reviewer aterga,sea-snake`. This triggers the existing Slack notification workflow.
7. Reply to the original feedback comment with a link to the rule-proposal PR, e.g. "Proposed as a general rule in #<number>."

Merging a rule-proposal PR signals approval and will trigger a sweep of existing translations.

## Important

- Only fix what the reviewer asked for — do not add new translations or make unrelated changes.
- Preserve all variables (like `{variable}`) and tags (like `<0>...</0>`).
- Always reply to every comment from allowed users.
