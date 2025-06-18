<script lang="ts">
  import Header from "$lib/components/layout/Header.svelte";
  import SideBarElement from "$lib/components/layout/SideBarElement.svelte";
  import SideBarOrTabs from "$lib/components/layout/SideBarOrTabs.svelte";
  const { children } = $props();
</script>

<SideBarOrTabs>
  {#snippet sidebarSnippet()}
    <SideBarElement class="text-text-primary" href="/hub">
      <h1>Home</h1>
    </SideBarElement>
    <SideBarElement class="text-text-primary" href="/hub/security">
      <h1>Security</h1>
    </SideBarElement>
  {/snippet}

  {#snippet tabsSnippet()}
    <SideBarElement class="text-text-primary" href="/hub">
      <h1>Home</h1>
    </SideBarElement>
    <SideBarElement class="text-text-primary" href="/hub/security">
      <h1>Security</h1>
    </SideBarElement>
  {/snippet}
</SideBarOrTabs>

<div>
  {@render children?.()}
</div>
