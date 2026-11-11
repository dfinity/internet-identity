---
paths:
  - "src/frontend/src/**/*.svelte"
  - "src/frontend/src/**/*.ts"
  - "src/frontend/src/lib/locales/**"
  - "lingui.config.ts"
  - "LOCALIZING.md"
---

# i18n and UI copy

## Translations

- Never commit `src/frontend/src/lib/locales/*.po` changes in a feature PR. Feature PRs add source strings only; a bot extracts and translates them in separate `chore(fe): update translations` PRs. Revert any `.po` diff before staging.
- Don't reword an existing source string outside the PR's scope: `lingui extract --clean` drops its translations in every locale.
- Never edit an existing `context` (msgctxt) string, for the same reason. Add a `context` when one English string carries two meanings.
- Translate every visible word, including "now", units and fallbacks.
- Format dates and durations with the `locale.store` helpers (`$formatRelative`, `$formatDuration`). Pass timestamps (or `Date`) into components as data and format them there.
- Translated arrays and option lists are `$derived`, so they follow locale changes.
- Use ICU plurals for counts.

## `<Trans>` and `$t`

- Use `<Trans>` for paragraph (`<p>`) text and `$t` tagged templates for short strings (labels, headings, buttons).
- Put `<Trans>` content on its own indented line:
  ```svelte
  <Trans>
    Content here.
  </Trans>
  ```
- Never wrap purely dynamic content in `<Trans>` or `$t` (it can crash at runtime); interpolate values inside a real message.

## Copy

- Write from the user's situation: say what happened and what to do next. No vague instructions, no hedging questions for absolute behaviour, no "Warning", no filler "please".
- Error copy has no technical terms (tokens, canisters, delegations). Errors that should never surface fall through to the generic unhandled message, and failure copy names the step that failed.
- Copy must not promise behaviour the code doesn't guarantee.
- Use one term per concept, matching the rest of the UI. "passkey" is lowercase.
- Highlight a UI label referenced in prose with `<b class="text-text-primary whitespace-nowrap">`, not quotes.
- Check how text wraps at the real component width: avoid orphaned words, and keep distinct ideas as separate sentences.
