You are running in CI. Check this repo (`dfinity/internet-identity`) for missing translations and/or newly-added rules, then create pull requests as needed.

Follow the rules in `src/frontend/src/lib/locales/CLAUDE.md`. Read that file before you start translating.

You are committing as `gix-bot` — replies to comments and all git commits must be authored by that identity. "Already addressed" checks must look for responses from `gix-bot`, not from any human reviewer.

## Step 1: Detect newly-added rules

Rules live under `src/frontend/src/lib/locales/rules/` — one file per topic, imported from `src/frontend/src/lib/locales/CLAUDE.md`. `rules/general.md` applies to all languages; `rules/<language>.md` (e.g. `rules/russian.md`, `rules/dutch.md`) applies to one language.

If the environment variable `$RULE_DIFF_RANGE` is non-empty (e.g. `abc123..def456`), run:

```
git diff $RULE_DIFF_RANGE -- src/frontend/src/lib/locales/CLAUDE.md src/frontend/src/lib/locales/rules/
```

to see which rules the triggering push added or modified. If rules were added/changed, treat every language affected by them as requiring a sweep — even if that language has no missing `msgstr` entries. A diff that only touches `CLAUDE.md` imports (no change to the rule content itself) does not require a sweep.

If `$RULE_DIFF_RANGE` is empty (manual `workflow_dispatch` run), skip this step and handle only missing translations in Step 2.

## Step 2: Detect missing translations

For each `.po` file in `src/frontend/src/lib/locales/` (skip `en.po`), check whether it has any entries with empty `msgstr ""` (excluding the header). Fetch the raw file content directly; you do not need to check out main in a worktree just for this step.

If there are no new rules from Step 1 AND no missing translations, stop here and report "Nothing to do".

## Step 3: Skip languages with existing open PRs

For each candidate language, skip it if there is already an open PR with branch `chore/translate-<lang>`. Check with:

```
gh pr list --repo dfinity/internet-identity --state open --search "chore(fe): update" --json number,headRefName
```

## Step 4: Create one PR per remaining language

For each language, do the following in an isolated git worktree:

1. Create a branch `chore/translate-<lang>` (e.g. `chore/translate-de`).
2. Run `npm run extract` to ensure `.po` files reflect the latest source strings.
3. Translate any entries where `msgstr` is empty.
4. If this language is affected by a newly-added rule from Step 1, also review existing translations in that file and update any that do not comply with the new rule.
5. Run `npm run extract` again to clean up, then `npm run format`.
6. Stage only the specific `.po` file for this language (e.g. `git add src/frontend/src/lib/locales/de.po`). `npm run extract`/`npm run format` may touch other `.po` files — do not include those.
7. Commit and push.
8. Open a PR with `gh pr create`:

   Title: `chore(fe): update <Language> translations`

   Body (follows the repo PR template — no Motivation heading, Tests section only if there are tests):

   ```
   New translations were missing for `<lang>.po`. This PR adds the missing translations.

   # Changes

   - Translated missing entries in `src/frontend/src/lib/locales/<lang>.po`
   ```

   If the PR is also applying a newly-added rule, adjust the body:

   ```
   This PR updates `<lang>.po` to comply with the translation rule introduced in #<rule-proposal PR number>, and adds any missing translations.

   # Changes

   - Applied translation rule: <rule summary>
   - Translated missing entries in `src/frontend/src/lib/locales/<lang>.po` (if applicable)
   ```

## Step 5: Add reviewers

After opening each PR, run `gh pr edit <number> --add-reviewer <users>`.

Always add `aterga` (required approval). Additionally, add the language-specific reviewer below if one is assigned and leave a comment tagging them:

> @<user> this PR may already be merged by the time you see it, but if you spot any translation mistakes feel free to leave comments or suggestions here — they'll be picked up by AI in a future run. Besides specific fixes, broader feedback is also welcome (e.g., tone, terminology preferences, style guidelines) — these will be reviewed and applied across all future translations.

Language-specific reviewers (in addition to `aterga`):

- Dutch (`nl.po`) → `sea-snake`
- Italian (`it.po`) → `AntonioVentilii`
- French (`fr.po`) → `mducroux`
- German (`de.po`) → `marc0olo`

## Important

- One PR per language, never one combined PR.
- Do not touch files for languages that have all translations filled in and are not affected by a newly-added rule.
- Skip `en.po` — it is the source language.
