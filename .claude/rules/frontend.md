---
paths:
  - "src/frontend/**"
---

# Frontend

## Design tokens and components

- Always use Tailwind design token classes instead of hardcoded values. Colors: `bg-bg-primary`, `text-text-tertiary`, `border-border-secondary` -- never raw hex like `bg-[#ecfdf3]`. Border radius: `rounded-lg`, `rounded-xl`, `rounded-full` -- never `rounded-[10px]`. Avoid arbitrary bracket values when a named token exists.
- Check `src/frontend/src/lib/components/ui/` for existing components before writing raw markup. The codebase has a design system; duplicating its markup inline leads to inconsistency.

## Template comments

- Don't write template comments that solely narrate Tailwind utility classes (e.g. "`-mx-4` bleeds outside the padding", "`@container` switches breakpoints to container query"). Utility classes are self-documenting; the only thing worth committing is the non-obvious WHY — the constraint, the bug avoided, the visual intent. If after stripping the class-paraphrase nothing remains, drop the comment entirely.
  - Good: `<!-- React to the manage pane's actual width, not the viewport — the sidebar can narrow it -->`.
  - Bad: `<!-- @container sets up a container query, @xl:grid-cols-3 switches to 3 columns at 36rem container width -->`.

## Buttons

- Never assign color classes (e.g. `text-fg-quaternary`) to icons inside `btn` elements. The button component handles icon colors; adding explicit color classes overrides built-in states (hover, focus, disabled). Only add sizing classes (e.g. `size-5`).
- Don't add `rounded-*` classes to `btn` elements unless explicitly asked or visible in a design. Exception: standalone `btn-icon` buttons (not in a row of icon buttons) should get `rounded-full`.
- `btn-danger` is a separate modifier class added alongside the variant. Correct: `btn btn-tertiary btn-danger`. Wrong: `btn btn-tertiary-danger`.

## i18n

- Content inside `<Trans>` must be on its own indented line, not inline with the tags:
  ```svelte
  <Trans>
    Content here.
  </Trans>
  ```
- Use `<Trans>` for paragraph (`<p>`) text content, not `$t` tagged template literals. `$t` is fine for short inline strings (button labels, headings).

## Svelte patterns

- When a derived value is only used within a single snippet, use `{@const}` inside the snippet rather than a script-level `$derived`. Reference the snippet's local parameter instead of component-level state.

## TypeScript

- Never use `as` type casts or `!` non-null assertions. Use control flow narrowing (guards, early returns) or utility functions that narrow the type. Restructure code so TypeScript can narrow through the pattern.
- Never use inline `import("module").Type` syntax. Always import the type at the top of the file with `import type { Foo } from "module"`.
- Use explicit checks like `!== undefined` instead of truthy checks for non-boolean values. `if (flag)` is fine for booleans; for everything else use `!== undefined`, `!== null`, `.length > 0`, etc.
- When a catch block doesn't use the error, write bare `catch { ... }` (no parameter) rather than `catch (_error) { ... }`.

## APIs and utilities

- Never use Node's `Buffer` class (e.g. `Buffer.from(str, "base64")`). Search for existing utilities in this order: local utils in the same directory, project-level shared utils in `$lib/utils/`, library helpers, browser-native APIs as last resort.
- Never concatenate URL parts. Use `new URL()` and pass the URL object directly to `goto()`, `fetch()`, etc. Never do `goto(url.pathname + url.search)`.
- Never use inline `import()` calls at runtime. Use static imports at the top of the file.
- Co-locate utility functions with the route or component that uses them. Only move to `$lib/utils/` if multiple unrelated routes need the same function.

## UX copy

- After writing UI text, check how it wraps visually in the browser at the actual component width. Avoid orphaned words (a single word on its own line). Adjust wording to get balanced line breaks. Keep distinct ideas as separate sentences -- don't merge them into one.
