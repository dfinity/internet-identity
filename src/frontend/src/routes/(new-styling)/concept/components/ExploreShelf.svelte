<script lang="ts">
  /**
   * A shelf of apps the way the App Store lists them: rows of icon, name,
   * one line about it, and Add, or Open once the app is in My apps. Rows
   * stay put rather than scroll sideways, so a horizontal swipe always means
   * the next page. Two columns where there's room keep each one-liner long
   * enough to read.
   */
  import type { FeaturedApp } from "../mockData";
  import AppRow from "./AppRow.svelte";

  interface Props {
    title: string;
    apps: FeaturedApp[];
    isAdded: (origin: string) => boolean;
    onAdd: (app: FeaturedApp) => void;
  }

  const { title, apps, isAdded, onAdd }: Props = $props();
</script>

<!-- The title and the rows' logos stand on the page's edge, the line the
     banners and the search start on, and a row's plate reaches 8px past it,
     as a tile's does on My apps. -->
<section class="flex flex-col gap-2">
  <h2 class="text-text-primary text-xl font-semibold tracking-tight">
    {title}
  </h2>
  <ul class="-mx-2 grid grid-cols-[minmax(0,1fr)] gap-x-4 @4xl:grid-cols-2">
    {#each apps as app (app.origin)}
      <li>
        <AppRow {app} added={isAdded(app.origin)} onAdd={() => onAdd(app)} />
      </li>
    {/each}
  </ul>
</section>
