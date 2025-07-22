<script lang="ts">
  import { type Snippet } from "svelte";

  type Props = {
    content: Snippet;
    header: Snippet;
    footer: Snippet;
  };

  const { content, header, footer }: Props = $props();
</script>

<div class="sidebar-layout">
  <div
    class="bg-bg-primary_alt col-start-1 col-end-4 row-start-1 row-end-6"
  ></div>
  <div class="col-start-2 col-end-3 row-start-3 row-end-4 p-4">
    {@render content?.()}
  </div>
  <header class="col-start-2 col-end-5 row-start-1 row-end-2 pt-2 pr-0 md:pr-6">
    {@render header?.()}
  </header>
  <footer class="col-start-1 col-end-5 row-start-5 row-end-6">
    {@render footer?.()}
  </footer>
</div>

<style>
  .sidebar-layout {
    display: grid;
    grid-template-columns: 1fr minmax(auto, 920px) 1fr;

    grid-template-rows: min-content 1fr max-content 1fr min-content;
    min-height: 100dvh;
    min-width: 100dvw;
  }

  @media (max-width: 768px) {
    .sidebar-layout {
      display: grid;
      grid-template-columns: 0fr 1fr 0fr;

      grid-template-rows: min-content 1fr max-content 1fr min-content;
      min-height: 100dvh;
      min-width: 100dvw;
    }
  }
</style>
