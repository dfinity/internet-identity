<script lang="ts">
  /**
   * Finding an app to add, the same at the top of Explore and in the "Add
   * app" popover: one field for a name, a word about the app, or its link.
   * Empty, it can offer a few apps; from the first letter it lists the
   * matches as rows. A link is offered as a row of its own, so pasting one
   * needs no separate mode, and a search that finds nothing says so.
   */
  import { onMount } from "svelte";
  import { SearchIcon, XIcon } from "@lucide/svelte";
  import { asLink, searchCatalog } from "../catalog";
  import type { FeaturedApp } from "../mockData";
  import AppRow from "./AppRow.svelte";

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

  const ROWS = "grid grid-cols-[minmax(0,1fr)] gap-x-4 @xl/search:grid-cols-2";

  let input = $state<HTMLInputElement>();

  const typed = $derived(query.trim());
  const matches = $derived(searchCatalog(typed));
  const listed = $derived(matches.slice(0, limit));
  // A link to a listed app finds the app itself, first, so the link isn't
  // offered on top of it.
  const link = $derived.by(() => {
    const host = onAddLink !== undefined ? asLink(typed) : undefined;
    return host === undefined ||
      matches.some(({ origin }) => origin === `https://${host}`)
      ? undefined
      : { host, origin: `https://${host}` };
  });
  const status = $derived(
    typed === ""
      ? ""
      : matches.length === 1
        ? "1 app found"
        : `${matches.length === 0 ? "No" : matches.length} apps found`,
  );

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

{#snippet rows(title: string, apps: FeaturedApp[])}
  <section class="flex flex-col gap-1">
    <h2 class={["text-text-tertiary text-sm font-medium", !flush && "px-2"]}>
      {title}
    </h2>
    <ul class={[ROWS, flush && "-mx-2"]}>
      {#each apps as app (app.origin)}
        <li>
          <AppRow {app} added={isAdded(app.origin)} onAdd={() => onAdd(app)} />
        </li>
      {/each}
    </ul>
  </section>
{/snippet}

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

  <p role="status" class="sr-only">{status}</p>

  {#if typed === ""}
    {#if suggestions.length > 0}
      {@render rows(suggestionsTitle, suggestions)}
    {/if}
  {:else}
    {#if link !== undefined}
      {@const added = isAdded(link.origin)}
      <ul class={[ROWS, flush && "-mx-2"]}>
        <li>
          <AppRow
            app={{
              origin: link.origin,
              name: link.host,
              url: link.origin,
              oneLiner: added ? "Added as a link" : "Add as a link",
            }}
            {added}
            onAdd={() => onAddLink?.(link.host)}
          />
        </li>
      </ul>
    {/if}
    {#if listed.length > 0}
      {@render rows(
        listed.length < matches.length
          ? "Top matches"
          : `${matches.length} ${matches.length === 1 ? "app" : "apps"}`,
        listed,
      )}
    {:else if link === undefined}
      <div class="flex flex-col items-center gap-1 px-4 py-8 text-center">
        <p
          class="text-text-primary text-sm font-medium [overflow-wrap:anywhere]"
        >
          No app called “{typed}”
        </p>
        <p class="text-text-tertiary text-sm text-pretty">
          {onAddLink !== undefined
            ? "Paste its link to add it anyway."
            : "Check the spelling, or try another word."}
        </p>
      </div>
    {/if}
  {/if}
</div>
