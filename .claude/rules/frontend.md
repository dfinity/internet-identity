---
paths:
  - "src/frontend/**"
---

# Frontend

`fe/bundle-size` The frontend ships inside the II frontend canister wasm, which has a hard size cap. Don't add large libraries, and check the cost of any new dependency. List every added, removed or changed dependency in the PR description with its reason (see `scripts-and-ci.md`).

## Design system and styling

- `fe/design-tokens` Use Tailwind design-token classes, never hardcoded values: `bg-bg-primary`, `text-text-tertiary`, `border-border-secondary`, not `bg-[#ecfdf3]`; `rounded-lg`/`rounded-xl`/`rounded-full`, not `rounded-[10px]`. Avoid arbitrary bracket values when a token exists. Use `rem`, not `px`.
- `fe/tailwind-v4` Tailwind is v4: the important modifier is a suffix (`size-3!`) and CSS variables are `max-h-(--x)`. Reject review suggestions in v3 syntax.
- `fe/reuse-ui-components` Check `src/frontend/src/lib/components/ui/` for an existing component before writing markup, and fix a bug in the shared component, not at one call site.
- `fe/no-one-off-variants` Don't push one-off variants into the design system: extend `btn` and friends with classes at the call site.
- `fe/class-arrays` Build class lists as arrays (`class={["...", isActive && "..."]}`) grouped by concern, never template strings.
- `fe/logical-properties` Use logical properties (`ms-*`, `pe-*`, `start-*`, `text-start`) instead of left/right: the UI supports RTL languages.
- `fe/component-owns-styling` A reusable component owns its own padding, radius, text size and color; it never relies on the parent's font size. Outer spacing is the parent's job (`gap`).
- `fe/icon-size-classes` Size icons with `size-*` classes.
- `fe/semantic-html` Use semantic HTML: `<p>` is text only (never a flex container), `<li>` sits directly in a `<ul>`, no interactive content inside `<button>`.
- `fe/match-design` Match the design exactly, and don't restyle parts the task didn't cover.

## Buttons

- `fe/no-icon-colors-in-buttons` Never put color classes (e.g. `text-fg-quaternary`) on icons inside `btn` elements; the button handles icon color across hover, focus and disabled states. Only add sizing (`size-5`).
- `fe/no-button-rounding` Don't add `rounded-*` to `btn` elements unless asked or in the design. Exception: a standalone `btn-icon` (not in a row of icon buttons) gets `rounded-full`.
- `fe/danger-modifier` `btn-danger` is a modifier next to the variant: `btn btn-tertiary btn-danger`, not `btn btn-tertiary-danger`.

## Svelte

- `fe/runes-only` Svelte 5 runes only (`$props`, `$state`, `$derived`, `$effect`); no `export let`. Use `SvelteMap`/`SvelteSet` for reactive collections.
- `fe/snippet-const` When a derived value is used only inside one snippet, use `{@const}` in the snippet rather than a script-level `$derived`, and reference the snippet's parameter.
- `fe/store-syntax` Use `$store`, not `get(store)`, in `.svelte` files.
- `fe/logic-in-flows` Keep business logic in the flow or store (`*.svelte.ts` flows, `lib/stores`); components render and forward events. Give child components presentational props (`isOpen`, `onRemove`), not the parent's state or whole service objects. Callbacks pass data, not functions.
- `fe/async-callback-promise` Type an action callback prop that does async work as returning a `Promise` (`onSubmit: (recoveryPhrase: string[]) => Promise<void>`), and keep `() => void` for one that completes at once. The component awaits the promise to show progress and block repeat actions: set an `is*` state, `await` the callback in `try`/`finally`, and disable the control and show a spinner meanwhile. The parent doesn't pass a separate loading prop. The parent handles the error itself (message, retry, routing). The component only learns that the promise rejected, not why, and may reflect that visually where it helps, for example by resetting the button or marking the field invalid.
- `fe/named-handlers` Extract non-trivial inline handlers into named `handle*` functions.
- `fe/form-onsubmit` Submit forms through one path: `<form onsubmit>`.
- `fe/unique-each-keys` Use unique `{#each}` keys (compound when needed).
- `fe/template-comments` Template comments: only the non-obvious why (constraint, bug avoided, visual intent). Never paraphrase the utility classes or describe what is rendered.

## Lifecycle and async

- `fe/clean-up-async` Clean up every timer, RAF, observer, listener, poll loop and in-flight request in the `$effect` cleanup or `onDestroy`, and on dialog close. Use `AbortController` and honour the signal in every await.
- `fe/reset-in-finally` Reset loading and submitting flags in `finally`. Disable a control while its write runs, and ignore stale async results through the `AbortController` signal of `fe/clean-up-async`, never a hand-rolled counter, epoch or token.
- `fe/reload-after-write` Page data comes from SvelteKit loaders, and a write is followed by `invalidate`/`invalidateAll()` to reload it. Choose per case between a progress indicator and an optimistic update:
  - Progress indicator: typical for dialog submissions. Disable the control and show a spinner until the write settles (see the async callback rule under Svelte).
  - Optimistic update: for something that should feel instant, like a toggle. Assign the next state to the `$derived` that reads the loader data, run the write, then invalidate. When the loader data comes back, the `$derived` recomputes from it: on success it matches, on failure it rolls back on its own. Don't write rollback code. Still show the error.

  Updating the UI only after the write resolves is not an optimistic update.
- `fe/reset-request-state` Reset per-request store state, and don't render until async-derived state is ready.
- `fe/open-popups-synchronously` Open popups and tabs synchronously inside the click handler; do the async work after.
- `fe/await-cleanup-before-navigation` Wait for async cleanup to finish before navigating away.

## Errors

- `fe/throw-canister-error` Unwrap canister results with `.then(throwCanisterError)` and check with `isCanisterError`; never `"Err" in result` and never fabricate a canister error. Legacy exception: the MCP methods (`get_mcp_registration_delegation` and the other `mcp_*` methods) return `Err : text`, which `throwCanisterError` can't wrap, so they unwrap with `throwTextCanisterError`. New methods return variant errors (`candid/result-variant`), so they never need it.
- `fe/route-async-errors` Route every async failure to the user-visible path (`handleError` / the flow's `onError`), the way the delegation handler does. `console.error` alone is not handling.
- `fe/no-silent-catch` `void promise.catch(...)` that drops an error needs a comment saying why; otherwise let it propagate. When the error is unused, write bare `catch { ... }`.
- `fe/retry-transient-only` Retry only transient failures (network, 408, 429, 5xx).
- `fe/disable-known-failures` Disable an action the UI already knows will fail rather than erroring after the click.

## TypeScript

- `fe/no-casts` Never use `as` casts or `!` non-null assertions; narrow with guards and early returns, or restructure. Type props so misuse can't compile (discriminated unions, `Pick<...>`).
- `fe/type-imports` Import types at the top of the file (`import type { Foo } from "module"`); no inline `import("module").Type`.
- `fe/static-imports` Use static imports; a runtime `import()` is only for intentional lazy loading (locale catalogs in `locale.store.ts`).
- `fe/generated-candid-types` Use the generated candid types for canister calls; no `any`, no `JSON.stringify(result.Err)`.
- `fe/zod-for-untrusted` Validate untrusted data (URL params, `postMessage`, stored values) with zod schemas and derive types with `z.infer`. Don't hand-write parsers zod covers.
- `fe/uint8array` Use `Uint8Array` for bytes, never Node's `Buffer`.
- `fe/build-urls` Never concatenate URL parts. Build a `new URL()` and pass the URL object to `goto()`, `fetch()` and friends; never `goto(url.pathname + url.search)`.
- `fe/current-origin` Don't hardcode `id.ai`: use the current origin.
- `fe/no-redundant-guards` No redundant guards: no null check before `?.`, no length check on a candid `[] | [T]` already narrowed.

## Code organization

- `fe/colocate-helpers` Co-locate helpers with the route or component that uses them. Move to `$lib/utils/` only when unrelated routes share it; a `$lib` module never lives under `routes/`.
- `fe/lib-imports` Import through `$lib/...`, not long `../` chains.
- `fe/weigh-utility-options` Before writing a utility, weigh the existing options (local utils, `$lib/utils/`, `@icp-sdk` helpers, browser APIs) and recommend one in the PR description, as in `code-style.md`. Notable ones:
  - Canister calls and errors: `throwCanisterError` and `isCanisterError` (`$lib/utils/utils.ts`); `handleError` (`$lib/components/utils/error.ts`) for the user-visible error path; `anonymousActor` (`$lib/globals.ts`) for public data.
  - Encoding: `toBase64`/`fromBase64`, `toBase64URL`/`fromBase64URL`, `toHex`/`fromHex` (`$lib/utils/utils.ts`).
  - Async: `promiseQueue` (`$lib/utils/promiseQueue.ts`) to serialize work, `retryFor`, `waitFor`, `waitForStore` (`$lib/utils/utils.ts`).
  - Origins and URLs: `OriginSchema` (`$lib/utils/transport/utils.ts`), `validateDerivationOrigin` (`$lib/utils/validateDerivationOrigin.ts`), `isSameOrigin` and `originLabel` (`$lib/utils/urlUtils.ts`).
  - Fetching untrusted content: `fetchCapped` (`$lib/utils/appMetadata.ts`).
  - Ids and time: `secureRandomId` (`$lib/utils/utils.ts`), `nanosToMillis`/`millisToNanos` (`$lib/utils/time.ts`).
  - Stores and formatting: `writableStored` (`$lib/stores/writable.store.ts`), `$formatRelative` and `$formatDuration` (`$lib/stores/locale.store.ts`).
  - UI: the components in `$lib/components/ui/` (`Dialog`, `Popover`, `Tooltip`, `Alert`, `Input`, `Select`, `Toggle`, `Checkbox`, `CodeInput`, `HoldToConfirm`) before building a new one.
- `fe/stored-preferences` Persist per-identity preferences with a `writableStored` store keyed by anchor, never raw global `localStorage`.
- `fe/older-browsers` Before using a new browser API, check its support in older browsers. Core flows must work there: feature-detect and fall back. Elsewhere, degraded styling or polish is acceptable as long as the page stays usable and navigable. Don't hold back the UX in current browsers to match older ones.

## The `/authorize` route

`/authorize` is a state machine driven by app messages, not a set of URL routes. Three layers, decoupled through stores:

- `fe/authorize-channel-handlers` Channel handlers (`lib/stores/channelHandlers/*`) receive app requests: validate the params (zod, `OriginSchema`), write the request into its store (`attributeConsentStore`, `notificationConsentStore`), wait for the user's answer from a store (`waitForStore`) and reply on the channel. They never import components or touch the DOM. Requests that drive authorization state run one at a time through `serializeAuthorizationRequest`. Attribute consent has its own queue, `serializeConsentRequest`: it waits for the sign-in flow, so on the authorization queue it would block the sign-in it waits for.
- `fe/authorize-stores` Stores hold the requests and the user's decisions, and are the only thing handlers and views share.
- `fe/authorize-views` Views (`routes/(new-styling)/authorize/views/*`) read stores and write the user's decisions back to them. They never touch the channel.
- `fe/authorize-view-precedence` `+page.svelte` picks the view with one `{#if}` chain over the stores, in precedence order: the first state that matches is what the user sees. Pending attribute consent comes before pending notification consent, both before the redirect animation, and then the sign-in screens. Add a step as a store state, a view and a handler, and place its condition in that chain. A request that needs time before it knows its screen holds the current one with `claimScreen()`, so the flow doesn't flash the redirect animation.
- `fe/authorize-no-navigation` Avoid navigating away from the page: open a dialog or a popup instead (for example `redirectInPopup` in `lib/utils/openID.ts` for OpenID), so the channel to the app stays open. The 1-click OpenID resume and SSO redirect flows are the exceptions.

## Accessibility

Accessibility is a requirement, and e2e tests select through the accessibility tree.

- `fe/a11y-labels` Icon-only buttons get an `aria-label`; decorative icons and mockups get `aria-hidden`.
- `fe/a11y-dialogs` Dialogs have `aria-labelledby`; async status text has `role="status"`; popovers are not modals and close on Escape.
- `fe/native-controls` Prefer native controls (radio inputs in a labelled `fieldset`) over ARIA role emulation. Use the real `disabled` attribute, not `pointer-events-none`. Keep visible `focus-visible` styles.
- `fe/input-font-size` Inputs are at least 16px font size (iOS zooms otherwise), and dialogs handle the on-screen keyboard (see `Dialog.svelte`).

## Analytics

- `fe/no-sensitive-analytics-urls` Analytics reports the page URL with every pageview and event, so nothing sensitive may be in it (see Leaks in `security.md`).
- `fe/funnel-events` Fire funnel events from the flow, and check they still fire after a refactor. Never close a funnel on failure: an interrupted funnel is the signal.
