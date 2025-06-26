<script lang="ts">
  import { onMount } from "svelte";
  import { isDesktopViewport } from "$lib/utils/UI/deviceDetection";
  import SideBar from "$lib/components/ui/SideBar.svelte";
  import Tabs from "$lib/components/ui/Tabs.svelte";

  const { sidebarElements, tabElements, content, header, footer } = $props();

  const handleResize = () => {
    displayVersion = isDesktopViewport() ? "sidebar" : "tabs";
  };

  let displayVersion: "sidebar" | "tabs" = $state("sidebar");

  onMount(handleResize);
</script>

<svelte:window onresize={handleResize} />

{#if displayVersion === "sidebar"}
  <div class="sidebar-layout">
    <SideBar class="col-start-1 col-end-2 row-start-1 row-end-6">
      {@render sidebarElements?.()}
    </SideBar>
    <div
      class="bg-bg-primary_alt col-start-2 col-end-5 row-start-1 row-end-6"
    ></div>
    <div class="col-start-3 col-end-4 row-start-3 row-end-4 p-4">
      {@render content?.()}
    </div>
    <header class="col-start-2 col-end-5 row-start-1 row-end-2 pt-2 pr-6">
      {@render header?.()}
    </header>
    <footer class="col-start-2 col-end-5 row-start-5 row-end-6">
      {@render footer?.()}
    </footer>
  </div>
{:else}
  <div class="bg-bg-primary_alt flex min-h-screen flex-col">
    <header class="bg-bg-primary_alt absolute top-0 right-0 mt-1">
      {@render header?.()}
    </header>
    <Tabs>
      {@render tabElements?.()}
    </Tabs>
    <div class="bg-bg-primary_alt flex-1 px-4">
      {@render content?.()}
    </div>
    <footer class="bg-bg-primary_alt">
      {@render footer?.()}
    </footer>
  </div>
{/if}

<style>
  .sidebar-layout {
    display: grid;
    grid-template-columns:
      296px
      minmax(0, 1fr)
      minmax(min-content, 4fr)
      minmax(0, 1fr);

    grid-template-rows: min-content 1fr max-content 1fr min-content;
    min-height: 100dvh;
    min-width: 100dvw;
  }

  @media (max-width: 1155px) {
    .sidebar-layout {
      display: grid;
      grid-template-columns:
        296px
        1fr
        minmax(0, max-content)
        1fr;

      grid-template-rows: min-content 1fr max-content 1fr min-content;
      min-height: 100dvh;
      min-width: 100dvw;
    }
  }
</style>
