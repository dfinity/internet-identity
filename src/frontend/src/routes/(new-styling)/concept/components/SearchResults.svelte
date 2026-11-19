<script lang="ts">
  /**
   * What a search for an app finds, under whichever field asks: the "Add
   * app" popover's, and the header's on every page. Empty, it can offer a
   * few apps; from the first letter it lists the matches as rows. A link is
   * offered as a row of its own, so pasting one needs no separate mode, and
   * a search that finds nothing says so.
   */
  import { asLink, searchCatalog } from "../catalog";
  import type { FeaturedApp } from "../mockData";
  import AppRow from "./AppRow.svelte";

  interface Props {
    query: string;
    isAdded: (origin: string) => boolean;
    onAdd: (app: FeaturedApp) => void;
    /** Without it a link is searched as words, and not offered to add. */
    onAddLink?: (host: string) => void;
    /** Offered while the field is empty. */
    suggestions?: FeaturedApp[];
    suggestionsTitle?: string;
    /** The most matches listed. */
    limit?: number;
    /** The rows' titles and logos stand on the field's edge and their
     *  plates reach past it, as on Explore's shelves. In a popover they keep
     *  their inset, inside its padding. */
    flush?: boolean;
  }

  const {
    query,
    isAdded,
    onAdd,
    onAddLink,
    suggestions = [],
    suggestionsTitle = "Popular",
    limit = 6,
    flush = false,
  }: Props = $props();

  const ROWS = "grid grid-cols-[minmax(0,1fr)] gap-x-4 @xl/search:grid-cols-2";

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
      <p class="text-text-primary text-sm font-medium [overflow-wrap:anywhere]">
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
