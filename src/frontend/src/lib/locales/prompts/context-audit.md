You are running in CI to audit translatable strings in Svelte files. Your goal is to add translator-facing `context` annotations _only where ambiguity is real_, and to keep `.po` bloat minimal by reusing existing context phrases whenever they semantically match.

Follow the rules in `src/frontend/src/lib/locales/CLAUDE.md`. Read that file before starting.

You are committing as `gix-bot` — git commits must be authored by that identity.

## Step 1: Skip if a context-audit PR is already open

Check for an existing open context-audit PR:

```
gh pr list --repo dfinity/internet-identity --state open --head chore/translation-context --json number,title
```

If there is already an open PR on that branch, stop and report "A context-audit PR is already open." Reviewer feedback on that PR is handled by the separate feedback flow, not here.

## Step 2: Build a context inventory

Before touching anything, scan all `.svelte` files under `src/frontend/src/lib` and `src/frontend/src/routes` and build a map of:

- **Already-annotated entries**: every `$t({ message: "...", context: "..." })` occurrence → `{msgid, context, file:line, surrounding UI role}`.
- **Unannotated entries**: every `$t\`...\``and`$t({ message: "..." })`without`context`→`{msgid, file:line, surrounding UI role}`.

The inventory drives the rest of the sweep. Each `.po` file entry is keyed by `(msgid, msgctxt)` — reusing the _exact same_ context string across call sites collapses them into a single entry, which is what keeps translation files small.

## Step 3: Decide what needs a context

Ambiguity criteria, in priority order:

1. **Same msgid used in ≥2 places with divergent meanings** (e.g. `Cancel` as a modal dismiss vs. `Cancel` to abort reauthentication).
2. **Commonly ambiguous short words** (≤3 words): `Right`, `Left`, `Close`, `Open`, `Back`, `Save`, `Cancel`, `Continue`, `Skip`, `Done`, `New`, `Edit`, `View`, `Clear`, `Run`, `Set`, `Show`, `Hide`, `OK`, `Remove`, `Reset`, `Retry`, `Verify`, `Default`, `Upgrade`.
3. **Generic labels** that could refer to multiple kinds of things: `Name`, `Type`, `Status`.

Skip in all other cases. Long phrases (>5 words), file names, URLs, identifiers, and anything that's clearly self-disambiguating from the msgid alone are NOT candidates.

**When in doubt, do nothing.** A missing annotation is cheap to add in a later sweep; an unnecessary one permanently bloats `.po` files across every supported language.

## Step 4: Pick a context, preferring reuse

For each string you decided to annotate, pick a context phrase using this decision tree:

1. **Does an already-annotated call site of the same msgid have a context whose description matches the UI role at the new call site?** If yes, reuse the existing phrase _byte-for-byte_ (copy-paste it). Do not rephrase, punctuate, or "improve" it — any deviation creates a separate `.po` entry.
2. **Are there multiple unannotated call sites of the same msgid that share the same UI role?** Group them and assign them all the same new context phrase.
3. **Only invent a new context phrase when no existing one fits.** Use concise, translator-facing descriptions: `"button label: dismiss dialog"`, `"menu item: delete passkey"`, `"direction"`, `"heading"`. Prefer reusing words already present in the existing inventory over minting synonyms ("dismiss modal" vs. "dismiss dialog" generate two entries).

If during scanning you notice two existing annotations whose contexts are semantically equivalent but textually different (e.g. `"dismiss modal"` and `"close dialog"` both used for modal-dismiss), normalize them to the same phrase as part of this sweep. Note each normalization in the PR body.

## Step 5: Apply the conversions

For each call site chosen in Step 3 with its phrase from Step 4:

1. Convert `$t\`X\``to`$t({ message: "X", context: "<phrase>" })`. Preserve any variables — `$t\`Hello ${name}\`` becomes `$t({ message: \`Hello ${name}\`, context: "..." })`(template literal retained for`message` when variables are present).
2. For `$t({ message: "X" })` with no context, just add the `context` field.

Do not change anything else — no copy changes, no translation changes, no unrelated refactors.

### Budget

Apply **no more than 40 annotations per sweep**. If the inventory turns up more candidates than 40, pick the highest-impact ones (strings used in the most places, most-likely-mistranslated words from the list in Step 3). Remaining candidates will be picked up in a future sweep. If you're about to exceed 40, stop and note the carry-over in the PR body.

## Step 6: Extract and format

Run `npm run extract`, then `npm run format`. Existing translations for the now-context-annotated msgids will be moved/cleared by Lingui; new `(msgid, msgctxt)` pairs appear with empty `msgstr` and will be picked up by the `translations-check` workflow to produce per-language translation PRs once this PR merges.

## Step 7: Open the PR

1. Branch: `chore/translation-context` (branch from latest `main`).
2. Stage BOTH the `.svelte` source changes AND the `src/frontend/src/lib/locales/*.po` changes.
3. Commit and push.
4. Count `.po` entries before and after the sweep. Before: total entries on `main` (e.g. `grep -c '^msgid ' src/frontend/src/lib/locales/en.po`). After: same on your branch. Report both numbers in the PR body.
5. Open the PR with `gh pr create`:

   Title: `chore(fe): add translation context`

   Body (follows the repo PR template — no Motivation heading, Tests section only if there are tests):

   ```
   Adds translator-facing `context` annotations to ambiguous translatable strings so translators can diverge translations per UI role.

   # Changes

   - Converted <N> occurrences across <M> files to `$t({ message, context })`
   - Reused existing context phrases for <K> of them (no new `.po` entries)
   - Introduced <L> new context phrases
   - `.po` entries before → after: <X> → <Y> (+<Z>)
   <if any normalizations:>
   - Normalized <"old phrase" → "new phrase"> to collapse semantically-equivalent contexts
   <if budget hit:>
   - Budget reached at 40 annotations; <count> additional candidates deferred to next sweep

   # Examples

   - `<high-impact msgid>` → <N> distinct contexts (list them briefly)
   ```

6. Add reviewers with `gh pr edit <number> --add-reviewer aterga`. Do not add language-specific reviewers here — context phrasing is language-agnostic; per-language translation PRs will come later via the check workflow.

## Important

- Do not change translations or user-visible copy — only add `context` annotations to sources.
- Reuse existing context phrases byte-for-byte whenever the UI role matches; this is the primary lever for keeping `.po` files small.
- When in doubt, skip the annotation.
- One PR for the whole sweep.
