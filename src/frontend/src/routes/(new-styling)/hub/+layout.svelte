<script lang="ts">
  import SideBarElement from "$lib/components/layout/SideBarElement.svelte";
  import SideBarOrTabs from "$lib/components/layout/SideBarOrTabs.svelte";
  import Avatar from "$lib/components/ui/Avatar.svelte";
  import ButtonOrAnchor from "$lib/components/utils/ButtonOrAnchor.svelte";
  import { UserIcon } from "@lucide/svelte";
  const { children } = $props();
</script>

<SideBarOrTabs>
  {#snippet sidebarElements()}
    <SideBarElement class="text-text-primary" href="/hub">
      <h1>Home</h1>
    </SideBarElement>
    <SideBarElement class="text-text-primary" href="/hub/security">
      <h1>Security</h1>
    </SideBarElement>
  {/snippet}

  {#snippet tabElements()}
    <SideBarElement class="text-text-primary" href="/hub">
      <h1>Home</h1>
    </SideBarElement>
    <SideBarElement class="text-text-primary" href="/hub/security">
      <h1>Security</h1>
    </SideBarElement>
  {/snippet}

  {#snippet content()}
    {@render children?.()}
  {/snippet}

  {#snippet header()}
    <div class="flex px-8 py-4">
      <div class="flex-1"></div>
      <ButtonOrAnchor>
        <Avatar size="sm">
          <UserIcon size="1.25rem" />
        </Avatar>
      </ButtonOrAnchor>
    </div>
  {/snippet}
  {#snippet footer()}
    <div class="flex px-8 py-4">
      <p class="text-text-primary">© Internet Identity</p>
      <div class="flex-1"></div>
      <ButtonOrAnchor class="text-text-primary" href="/support"
        >Support</ButtonOrAnchor
      >
    </div>
  {/snippet}
</SideBarOrTabs>
