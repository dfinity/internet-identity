<script lang="ts">
  /**
   * Finding an app to add in the "Add app" popover: one field for a name, a
   * word about the app, or its link, and what it finds under it (the same
   * rows as the header's search, see SearchResults).
   */
  import { onMount } from "svelte";
  import { SearchIcon, XIcon } from "@lucide/svelte";
  import type { FeaturedApp } from "../mockData";
  import SearchResults from "./SearchResults.svelte";

  interface Props {
    isAdded: (origin: string) => boolean;
    onAdd: (app: FeaturedApp) => void;
    /** Without it a link is searched as words, and not offered to add. */
    onAddLink?: (host: string) => void;
    /** Offered while the field is empty. */
    suggestions?: FeaturedApp[];
    suggestionsTitle?: string;
    /** The most matches listed. */
    limit?: number;
    /** Focuses the field once it shows, as in a popover opened to search. */
    autofocus?: boolean;
    query?: string;
    /** The rows' titles and logos stand on the field's edge and their
     *  plates reach past it, as on Explore's shelves. In a popover they keep
     *  their inset, inside its padding. */
    flush?: boolean;
  }

  let {
    isAdded,
    onAdd,
    onAddLink,
    suggestions = [],
    suggestionsTitle = "Popular",
    limit = 6,
    autofocus = false,
    query = $bindable(""),
    flush = false,
  }: Props = $props();

  let input = $state<HTMLInputElement>();

  onMount(() => {
    if (autofocus) {
      // After whatever opened the search (a popover) has placed focus.
      const timer = setTimeout(() => input?.focus({ preventScroll: true }), 60);
      return () => clearTimeout(timer);
    }
  });

  const clear = () => {
    query = "";
    input?.focus();
  };
</script>

<div class="@container/search flex flex-col gap-4">
  <!-- A pill with a hairline, as Fluid Functionalism draws a field: focus
       darkens the hairline instead of adding a ring, and hovering fills it. -->
  <div class="relative">
    <SearchIcon
      class="text-fg-tertiary pointer-events-none absolute start-3.5 top-1/2 size-4.5 -translate-y-1/2"
      aria-hidden="true"
    />
    <input
      bind:this={input}
      bind:value={query}
      type="search"
      placeholder="Search apps or paste a link"
      aria-label="Search apps or paste a link"
      autocomplete="off"
      autocapitalize="off"
      spellcheck="false"
      enterkeyhint="search"
      class="text-text-primary placeholder:text-text-placeholder border-border-tertiary focus:border-fg-quaternary h-11 w-full appearance-none rounded-full border bg-transparent ps-10 pe-10 text-base transition-[background-color,border-color] duration-100 ease-out outline-none hover:bg-(--ii-hover) focus:ring-0 focus:hover:bg-transparent motion-reduce:transition-none [&::-webkit-search-cancel-button]:appearance-none"
    />
    {#if query !== ""}
      <button
        type="button"
        onclick={clear}
        aria-label="Clear search"
        class="text-fg-tertiary hover:text-fg-secondary focus-visible:outline-focus-ring absolute end-2 top-1/2 grid size-7 -translate-y-1/2 place-items-center rounded-full transition-colors duration-150 ease-out outline-none hover:bg-(--ii-hover) focus-visible:outline-2 motion-reduce:transition-none"
      >
        <XIcon class="size-4" aria-hidden="true" />
      </button>
    {/if}
  </div>

  <SearchResults
    {query}
    {isAdded}
    {onAdd}
    {onAddLink}
    {suggestions}
    {suggestionsTitle}
    {limit}
    {flush}
  />
</div>
