<script lang="ts">
  /**
   * Explore: a search on top, and under it the feed, read like the App
   * Store: a banner, a shelf, another banner, more shelves. From the first
   * letter typed the feed steps aside for the matches, in the same rows the
   * shelves use, and the same search as "Add app" on My apps.
   */
  import { exploreFeed, type FeaturedApp } from "../mockData";
  import AppSearch from "./AppSearch.svelte";
  import ExploreBanner from "./ExploreBanner.svelte";
  import ExploreBuild from "./ExploreBuild.svelte";
  import ExploreShelf from "./ExploreShelf.svelte";

  interface Props {
    isAdded: (origin: string) => boolean;
    onAdd: (app: FeaturedApp) => void;
    /** Offers a pasted link to add as it is. */
    onAddLink?: (host: string) => void;
    /** The featured shelf's apps; it steps aside when there are none. */
    featured: FeaturedApp[];
  }

  const { isAdded, onAdd, onAddLink, featured }: Props = $props();

  let query = $state("");
  const searching = $derived(query.trim() !== "");
</script>

<!-- As wide as the logos on My apps reach (the page sets it). On a phone the
     page keeps 10px short of the edge, for the apps' plates; the content
     steps in by those 10px, so it stands on the edge too. -->
<div class="flex w-full flex-col gap-8 @max-lg/canvas:px-2.5">
  <AppSearch bind:query {isAdded} {onAdd} {onAddLink} limit={Infinity} flush />

  {#if !searching}
    <div class="flex flex-col gap-10">
      {#each exploreFeed as item, index (index)}
        {#if item.kind === "banner"}
          <ExploreBanner {item} />
        {:else if item.kind === "shelf"}
          <ExploreShelf title={item.title} apps={item.apps} {isAdded} {onAdd} />
        {:else if item.kind === "build"}
          <ExploreBuild app={item.app} />
        {:else if featured.length > 0}
          <ExploreShelf title={item.title} apps={featured} {isAdded} {onAdd} />
        {/if}
      {/each}
    </div>
  {/if}
</div>
