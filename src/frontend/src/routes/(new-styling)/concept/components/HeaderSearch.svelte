<script lang="ts">
  /**
   * Finding an app from any page, so search isn't a part of Explore only.
   * On desktop it's a field in the canvas's top corner, always in view: a
   * focus opens what it finds under it, over the page, and the page stays
   * where it was. On a phone the header has room for its glyph only: a tap
   * opens the search over the whole screen, the field where the header was
   * and the keyboard up at once, as a phone's search does; Cancel puts it
   * away. Both list the same rows as the "Add app" popover (SearchResults).
   */
  import { flushSync } from "svelte";
  import { SearchIcon, XIcon } from "@lucide/svelte";
  import { cubicOut } from "svelte/easing";
  import { prefersReducedMotion } from "svelte/motion";
  import { scale } from "svelte/transition";
  import type { ClassValue } from "svelte/elements";
  import type { FeaturedApp } from "../mockData";
  import SearchResults from "./SearchResults.svelte";
  import SurfaceLevel from "./SurfaceLevel.svelte";

  interface Props {
    isAdded: (origin: string) => boolean;
    onAdd: (app: FeaturedApp) => void;
    onAddLink?: (host: string) => void;
    /** Offered while the field is empty. */
    suggestions?: FeaturedApp[];
    /** A phone's header: the glyph, which opens the search over the page. */
    compact?: boolean;
    /** Content runs under the header: the field lifts off it, as the
     *  header's tabs do. */
    lifted?: boolean;
    class?: ClassValue;
  }

  const {
    isAdded,
    onAdd,
    onAddLink,
    suggestions = [],
    compact = false,
    lifted = false,
    class: className,
  }: Props = $props();

  const LABEL = "Search apps or paste a link";
  const reduced = $derived(prefersReducedMotion.current);

  let query = $state("");
  let open = $state(false);
  let root = $state<HTMLElement>();
  let input = $state<HTMLInputElement>();
  let dialog = $state<HTMLDialogElement>();
  let results = $state<HTMLElement>();

  /** Desktop: something to show under the field, found or offered. */
  const listing = $derived(
    open && (query.trim() !== "" || suggestions.length > 0),
  );

  const clear = () => {
    query = "";
    input?.focus();
  };

  // --- Desktop: the field in the corner ---

  // Focus stays in the field while the list is open: a press on the list
  // doesn't take it (onmousedown below), and a Tab into the rows keeps it
  // inside. Focus going anywhere else closes the list.
  const onFocusOut = (event: FocusEvent) => {
    if (
      !(event.relatedTarget instanceof Node) ||
      root?.contains(event.relatedTarget) !== true
    ) {
      open = false;
    }
  };
  /** Escape closes the list first, then empties the field, then leaves it. */
  const onKeydown = (event: KeyboardEvent) => {
    if (event.key !== "Escape") return;
    event.preventDefault();
    if (listing) {
      open = false;
      input?.focus();
    } else if (query !== "") {
      query = "";
    } else {
      input?.blur();
    }
  };

  // --- Phone: the search over the screen ---

  // Opened within the tap itself, so iOS brings the keyboard up with it: a
  // focus from a timer or after a frame would leave it down.
  const openOver = () => {
    open = true;
    flushSync();
    dialog?.showModal();
    input?.focus();
    if (!reduced) {
      dialog?.animate([{ opacity: 0 }, { opacity: 1 }], {
        duration: 160,
        easing: "ease-out",
      });
      results?.animate(
        [{ transform: "translateY(8px)" }, { transform: "translateY(0)" }],
        { duration: 240, easing: "cubic-bezier(0.23, 1, 0.32, 1)" },
      );
    }
  };
  const closeOver = async () => {
    if (dialog === undefined || !dialog.open) return;
    input?.blur();
    if (!reduced) {
      await dialog.animate([{ opacity: 1 }, { opacity: 0 }], {
        duration: 120,
        easing: "ease-in",
        fill: "forwards",
      }).finished;
    }
    dialog.close();
    open = false;
  };
</script>

{#snippet field(big: boolean)}
  <SearchIcon
    class="text-fg-tertiary pointer-events-none absolute start-3.5 top-1/2 size-4 -translate-y-1/2"
    aria-hidden="true"
  />
  <!-- A pill in the shelf's grey, as the tabs' track is, so it shows on the
       canvas at rest; focus darkens a hairline instead of adding a ring. A
       phone's is a step larger, 16px, so iOS doesn't zoom in on it. -->
  <input
    bind:this={input}
    bind:value={query}
    onkeydown={compact ? undefined : onKeydown}
    type="search"
    placeholder={LABEL}
    aria-label={LABEL}
    autocomplete="off"
    autocapitalize="off"
    spellcheck="false"
    enterkeyhint="search"
    class={[
      "text-text-primary placeholder:text-text-placeholder focus:border-fg-quaternary h-10 w-full min-w-0 appearance-none rounded-full border border-transparent bg-(--ii-shelf) ps-10 pe-10 transition-[border-color,box-shadow] duration-200 ease-out outline-none focus:ring-0 motion-reduce:transition-none [&::-webkit-search-cancel-button]:appearance-none",
      big ? "text-base" : "text-sm",
      lifted && "ii-elevation-6",
    ]}
  />
  {#if query !== ""}
    <button
      type="button"
      onclick={clear}
      aria-label="Clear search"
      class="text-fg-tertiary hover:text-fg-secondary focus-visible:outline-focus-ring absolute end-1.5 top-1/2 grid size-7 -translate-y-1/2 place-items-center rounded-full transition-colors duration-150 ease-out outline-none hover:bg-(--ii-hover) focus-visible:outline-2 motion-reduce:transition-none"
    >
      <XIcon class="size-4" aria-hidden="true" />
    </button>
  {/if}
{/snippet}

{#if compact}
  <button
    type="button"
    onclick={openOver}
    aria-label="Search apps"
    aria-haspopup="dialog"
    class={[
      "text-fg-secondary focus-visible:outline-focus-ring grid size-8 shrink-0 place-items-center rounded-full transition-colors duration-150 hover:bg-(--ii-hover) focus-visible:outline-2 active:bg-(--ii-selected)",
      className,
    ]}
  >
    <SearchIcon class="size-4" aria-hidden="true" />
  </button>
  <!-- In the top layer, over the tab bar and everything else on the page. -->
  <dialog
    bind:this={dialog}
    aria-label="Search apps"
    oncancel={(event) => {
      event.preventDefault();
      void closeOver();
    }}
    class="text-text-primary fixed inset-0 m-0 h-dvh max-h-none w-full max-w-none flex-col bg-(--ii-surface-3) p-0 backdrop:bg-transparent open:flex"
  >
    {#if open}
      <div class="h-[env(safe-area-inset-top)] shrink-0"></div>
      <!-- Where the header's row was, as tall, on the same edge. -->
      <div class="flex shrink-0 items-center gap-1 px-(--ii-edge) py-3">
        <div class="relative min-w-0 flex-1">
          {@render field(true)}
        </div>
        <button
          type="button"
          onclick={() => void closeOver()}
          class="text-text-secondary hover:text-text-primary focus-visible:outline-focus-ring -me-2 h-10 shrink-0 rounded-full px-2 text-base font-medium transition-colors duration-150 focus-visible:outline-2"
        >
          Cancel
        </button>
      </div>
      <div
        bind:this={results}
        class="@container/search flex min-h-0 flex-1 flex-col gap-4 overflow-y-auto overscroll-contain px-(--ii-edge) pt-2 pb-[calc(1.5rem+env(safe-area-inset-bottom))]"
      >
        <SearchResults
          {query}
          {isAdded}
          {onAdd}
          {onAddLink}
          {suggestions}
          limit={Infinity}
          flush
        />
      </div>
    {/if}
  </dialog>
{:else}
  <!-- Escape from a row comes up here: it goes back to the field. -->
  <!-- svelte-ignore a11y_no_noninteractive_element_interactions -->
  <div
    bind:this={root}
    role="search"
    onfocusin={() => (open = true)}
    onfocusout={onFocusOut}
    onkeydown={(event) => {
      if (event.key === "Escape" && event.target !== input) {
        event.preventDefault();
        open = false;
        input?.focus();
      }
    }}
    class={["pointer-events-auto relative", className]}
  >
    {@render field(false)}
    {#if listing}
      <!-- Over the page, from the field's end: as wide as the "Add app"
           popover, painted as a popup is (two levels over the canvas). A
           press on it keeps the focus in the field, so the list stays. -->
      <div
        role="presentation"
        onmousedown={(event) => event.preventDefault()}
        transition:scale={{
          start: 0.97,
          duration: reduced ? 0 : 160,
          easing: cubicOut,
        }}
        class="absolute end-0 top-full z-10 mt-2 w-[min(26rem,calc(100cqw-2.5rem))] origin-top-right overflow-hidden rounded-3xl bg-(--ii-surface-5) shadow-(--ii-shadow-3) rtl:origin-top-left"
      >
        <SurfaceLevel level={5}>
          <div
            class="@container/search flex max-h-[min(32rem,calc(100dvh-7rem))] flex-col gap-3 overflow-y-auto overscroll-contain px-2 pt-3 pb-2"
          >
            <SearchResults
              {query}
              {isAdded}
              {onAdd}
              {onAddLink}
              {suggestions}
            />
          </div>
        </SurfaceLevel>
      </div>
    {/if}
  </div>
{/if}
