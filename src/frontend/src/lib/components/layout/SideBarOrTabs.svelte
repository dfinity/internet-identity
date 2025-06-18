<script lang="ts">
  import { onMount } from "svelte";
  import { isMobileDevice } from "$lib/utils/UI/deviceDetection";
  import SideBar from "./SideBar.svelte";
  import Tabs from "./Tabs.svelte";

  const { sidebarSnippet, tabsSnippet } = $props();

  const handleResize = () => {
    displayVersion = isMobileDevice() ? "tabs" : "sidebar";
  };

  let displayVersion: "sidebar" | "tabs" = $state("sidebar");

  onMount(handleResize);
</script>

<svelte:window onresize={handleResize} />

{#if displayVersion === "sidebar"}
  <SideBar>
    {@render sidebarSnippet?.()}
  </SideBar>
{:else}
  <Tabs>
    {@render tabsSnippet?.()}
  </Tabs>
{/if}
