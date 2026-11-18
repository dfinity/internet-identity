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

<!-- On a phone the title and the rows' logos stand on the canvas's edge, and
     a row's plate reaches 8px past it, as a tile's does on My apps. -->
<section class="flex flex-col gap-2">
  <h2
    class="text-text-primary px-2 text-xl font-semibold tracking-tight @max-lg/canvas:px-0"
  >
    {title}
  </h2>
  <ul
    class="grid grid-cols-[minmax(0,1fr)] gap-x-4 @max-lg/canvas:-mx-2 @2xl:grid-cols-2"
  >
    {#each apps as app (app.origin)}
      <li>
        <AppRow {app} added={isAdded(app.origin)} onAdd={() => onAdd(app)} />
      </li>
    {/each}
  </ul>
</section>
