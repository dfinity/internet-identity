<script lang="ts">
  import SideBarElement from "$lib/components/ui/SideBarElement.svelte";
  import SideBarOrTabs from "$lib/components/layout/SideBarOrTabs.svelte";
  import Avatar from "$lib/components/ui/Avatar.svelte";
  import ButtonOrAnchor from "$lib/components/utils/ButtonOrAnchor.svelte";
  import { LucideHome, Shield, UserIcon } from "@lucide/svelte";
  import SideBarElementGroup from "$lib/components/ui/SideBarElementGroup.svelte";
  import { fly, scale } from "svelte/transition";
  import { navigating, page } from "$app/state";
  import TabElement from "$lib/components/ui/TabElement.svelte";
  import TabElementGroup from "$lib/components/ui/TabElementGroup.svelte";
  const { children } = $props();

  let divRef = $state<HTMLDivElement>();
  const animationDuration = 200;
</script>

<SideBarOrTabs>
  {#snippet sidebarElements()}
    <SideBarElementGroup>
      <SideBarElement href="/hub">
        <LucideHome size="1rem" class="stroke-fg-quaternary" />
        <h1>Home</h1>
      </SideBarElement>
      <SideBarElement class="text-text-primary" href="/hub/security">
        <Shield size="1rem" class="stroke-fg-quaternary" />
        <h1>Security</h1>
      </SideBarElement>
    </SideBarElementGroup>
  {/snippet}

  {#snippet tabElements()}
    <TabElementGroup>
      <TabElement class="text-text-primary" href="/hub">
        <h1>Home</h1>
      </TabElement>
      <TabElement class="text-text-primary" href="/hub/security">
        <h1>Security</h1>
      </TabElement>
    </TabElementGroup>
  {/snippet}

  {#snippet content()}
    {#key page.url.pathname}
      <div
        bind:this={divRef}
        in:fly={{
          x: 200 * (navigating.delta ?? 1),
          duration: 200,
          delay: 200,
        }}
        out:fly={{ x: -160 * (navigating.delta ?? 1), duration: 160 }}
        onoutrostart={() => divRef?.setAttribute("aria-hidden", "true")}
        class="nth-2:hidden"
      >
        {@render children?.()}
      </div>
    {/key}
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
