---
paths:
  - "src/frontend/**"
---

# Frontend

The frontend ships inside the II frontend canister wasm, which has a hard size cap. Don't add large libraries, and check the cost of any new dependency. List every added, removed or changed dependency in the PR description with its reason (see `scripts-and-ci.md`).

## Design system and styling

- Use Tailwind design-token classes, never hardcoded values: `bg-bg-primary`, `text-text-tertiary`, `border-border-secondary`, not `bg-[#ecfdf3]`; `rounded-lg`/`rounded-xl`/`rounded-full`, not `rounded-[10px]`. Avoid arbitrary bracket values when a token exists. Use `rem`, not `px`.
- Tailwind is v4: the important modifier is a suffix (`size-3!`) and CSS variables are `max-h-(--x)`. Reject review suggestions in v3 syntax.
- Check `src/frontend/src/lib/components/ui/` for an existing component before writing markup, and fix a bug in the shared component, not at one call site. Don't push one-off variants into the design system: extend `btn` and friends with classes at the call site.
- Build class lists as arrays (`class={["...", isActive && "..."]}`) grouped by concern, never template strings.
- Use logical properties (`ms-*`, `pe-*`, `start-*`, `text-start`) instead of left/right: the UI supports RTL languages.
- A reusable component owns its own padding, radius, text size and color; it never relies on the parent's font size. Outer spacing is the parent's job (`gap`).
- Size icons with `size-*` classes.
- Use semantic HTML: `<p>` is text only (never a flex container), `<li>` sits directly in a `<ul>`, no interactive content inside `<button>`.
- Match the design exactly, and don't restyle parts the task didn't cover.

## Buttons

- Never put color classes (e.g. `text-fg-quaternary`) on icons inside `btn` elements; the button handles icon color across hover, focus and disabled states. Only add sizing (`size-5`).
- Don't add `rounded-*` to `btn` elements unless asked or in the design. Exception: a standalone `btn-icon` (not in a row of icon buttons) gets `rounded-full`.
- `btn-danger` is a modifier next to the variant: `btn btn-tertiary btn-danger`, not `btn btn-tertiary-danger`.

## Svelte

- Svelte 5 runes only (`$props`, `$state`, `$derived`, `$effect`); no `export let`. Use `SvelteMap`/`SvelteSet` for reactive collections. Assigning to a `$derived` for an optimistic update is valid.
- When a derived value is used only inside one snippet, use `{@const}` in the snippet rather than a script-level `$derived`, and reference the snippet's parameter.
- Use `$store`, not `get(store)`, in `.svelte` files.
- Keep business logic in the flow or store (`*.svelte.ts` flows, `lib/stores`); components render and forward events. Give child components presentational props (`isOpen`, `onRemove`), not the parent's state or whole service objects. Callbacks pass data, not functions.
- Extract non-trivial inline handlers into named `handle*` functions. Submit forms through one path: `<form onsubmit>`.
- Use unique `{#each}` keys (compound when needed).
- Template comments: only the non-obvious why (constraint, bug avoided, visual intent). Never paraphrase the utility classes or describe what is rendered.

## Lifecycle and async

- Clean up every timer, RAF, observer, listener, poll loop and in-flight request in the `$effect` cleanup or `onDestroy`, and on dialog close. Use `AbortController` and honour the signal in every await.
- Reset loading and submitting flags in `finally`. Disable a control while its write runs, ignore stale async results, and reconcile optimistic updates with the authoritative state.
- Reset per-request store state, and don't render until async-derived state is ready.
- Open popups and tabs synchronously inside the click handler; do the async work after.
- Wait for async cleanup to finish before navigating away.

## Errors

- Unwrap canister results with `.then(throwCanisterError)` and check with `isCanisterError`; never `"Err" in result` and never fabricate a canister error.
- Route every async failure to the user-visible path (`handleError` / the flow's `onError`), the way the delegation handler does. `console.error` alone is not handling.
- `void promise.catch(...)` that drops an error needs a comment saying why; otherwise let it propagate. When the error is unused, write bare `catch { ... }`.
- Retry only transient failures (network, 408, 429, 5xx).
- Disable an action the UI already knows will fail rather than erroring after the click.

## TypeScript

- Never use `as` casts or `!` non-null assertions; narrow with guards and early returns, or restructure. Type props so misuse can't compile (discriminated unions, `Pick<...>`).
- Import types at the top of the file (`import type { Foo } from "module"`); no inline `import("module").Type`. Use static imports; a runtime `import()` is only for intentional lazy loading (locale catalogs in `locale.store.ts`).
- Use the generated candid types for canister calls; no `any`, no `JSON.stringify(result.Err)`.
- Validate untrusted data (URL params, `postMessage`, stored values) with zod schemas and derive types with `z.infer`. Don't hand-write parsers zod covers.
- Use `Uint8Array` for bytes, never Node's `Buffer`.
- Never concatenate URL parts. Build a `new URL()` and pass the URL object to `goto()`, `fetch()` and friends; never `goto(url.pathname + url.search)`. Don't hardcode `id.ai`: use the current origin.
- No redundant guards: no null check before `?.`, no length check on a candid `[] | [T]` already narrowed.

## Code organization

- Co-locate helpers with the route or component that uses them. Move to `$lib/utils/` only when unrelated routes share it; a `$lib` module never lives under `routes/`.
- Import through `$lib/...`, not long `../` chains.
- Before writing a utility, weigh the existing options (local utils, `$lib/utils/`, `@icp-sdk` helpers, browser APIs) and recommend one in the PR description, as in `code-style.md`. Notable ones:
  - Canister calls and errors: `throwCanisterError` and `isCanisterError` (`$lib/utils/utils.ts`); `handleError` (`$lib/components/utils/error.ts`) for the user-visible error path; `anonymousActor` (`$lib/globals.ts`) for public data.
  - Encoding: `toBase64`/`fromBase64`, `toBase64URL`/`fromBase64URL`, `toHex`/`fromHex` (`$lib/utils/utils.ts`).
  - Async: `promiseQueue` (`$lib/utils/promiseQueue.ts`) to serialize work, `retryFor`, `waitFor`, `waitForStore` (`$lib/utils/utils.ts`).
  - Origins and URLs: `OriginSchema` (`$lib/utils/transport/utils.ts`), `validateDerivationOrigin` (`$lib/utils/validateDerivationOrigin.ts`), `isSameOrigin` and `originLabel` (`$lib/utils/urlUtils.ts`).
  - Fetching untrusted content: `fetchCapped` (`$lib/utils/appMetadata.ts`).
  - Ids and time: `secureRandomId` (`$lib/utils/utils.ts`), `nanosToMillis`/`millisToNanos` (`$lib/utils/time.ts`).
  - Stores and formatting: `writableStored` (`$lib/stores/writable.store.ts`), `$formatRelative` and `$formatDuration` (`$lib/stores/locale.store.ts`).
  - UI: the components in `$lib/components/ui/` (`Dialog`, `Popover`, `Tooltip`, `Alert`, `Input`, `Select`, `Toggle`, `Checkbox`, `CodeInput`, `HoldToConfirm`) before building a new one.
- Persist per-identity preferences with a `writableStored` store keyed by anchor, never raw global `localStorage`.
- Before using a new browser API, check support in older browsers such as older Safari. Core flows (such as sign-up, sign-in, authorize and recovery) must work there: feature-detect and fall back. Elsewhere, degraded styling or polish is acceptable as long as the page stays usable and navigable. Don't hold back the UX in current browsers to match older ones.

## Accessibility

Accessibility is a requirement, and e2e tests select through the accessibility tree.

- Icon-only buttons get an `aria-label`; decorative icons and mockups get `aria-hidden`.
- Dialogs have `aria-labelledby`; async status text has `role="status"`; popovers are not modals and close on Escape.
- Prefer native controls (radio inputs in a labelled `fieldset`) over ARIA role emulation. Use the real `disabled` attribute, not `pointer-events-none`. Keep visible `focus-visible` styles.
- Inputs are at least 16px font size (iOS zooms otherwise), and dialogs handle the on-screen keyboard (see `Dialog.svelte`).

## Analytics

- Fire funnel events from the flow, and check they still fire after a refactor. Never close a funnel on failure: an interrupted funnel is the signal.
