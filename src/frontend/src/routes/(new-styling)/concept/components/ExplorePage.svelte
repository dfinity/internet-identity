<script lang="ts">
  /**
   * Explore: the feed, read like the App Store: a banner, a shelf, another
   * banner, more shelves. Search isn't here: it's the header's, on every
   * page (HeaderSearch), so finding an app doesn't mean coming here first.
   */
  import { exploreFeed, type FeaturedApp } from "../mockData";
  import ExploreBanner from "./ExploreBanner.svelte";
  import ExploreBuild from "./ExploreBuild.svelte";
  import ExploreShelf from "./ExploreShelf.svelte";

  interface Props {
    isAdded: (origin: string) => boolean;
    onAdd: (app: FeaturedApp) => void;
    /** The featured shelf's apps; it steps aside when there are none. */
    featured: FeaturedApp[];
  }

  const { isAdded, onAdd, featured }: Props = $props();
</script>

<!-- As wide as the logos on My apps reach (the page sets it). On a phone the
     page keeps 10px short of the edge, for the apps' plates; the content
     steps in by those 10px, so it stands on the edge too. -->
<div class="flex w-full flex-col gap-10 @max-lg/canvas:px-2.5">
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
