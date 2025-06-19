<script lang="ts">
  import SideBarElement from "$lib/components/ui/SideBarElement.svelte";
  import SideBarOrTabs from "$lib/components/layout/SideBarOrTabs.svelte";
  import Avatar from "$lib/components/ui/Avatar.svelte";
  import ButtonOrAnchor from "$lib/components/utils/ButtonOrAnchor.svelte";
  import { LucideHome, Shield, UserIcon } from "@lucide/svelte";
  import SideBarElementGroup from "$lib/components/ui/SideBarElementGroup.svelte";
  import { fly } from "svelte/transition";
  import { navigating, page } from "$app/state";
  import TabElement from "$lib/components/ui/TabElement.svelte";
  import TabElementGroup from "$lib/components/ui/TabElementGroup.svelte";
  import { beforeNavigate } from "$app/navigation";
  import { isDesktopViewport } from "$lib/utils/UI/deviceDetection";

  const { children } = $props();

  let divRef = $state<HTMLDivElement>();

  let sideBarGroupRef = $state<HTMLDivElement>();
  let tabsGroupRef = $state<HTMLDivElement>();
  let animationDirection: "up" | "down" | "left" | "right" = $state("up");

  beforeNavigate((nav) => {
    const fromPathName = nav.from?.url.pathname;
    const toPathName = nav.to?.url.pathname;

    let anchors;
    if (isDesktopViewport()) {
      if (!sideBarGroupRef) return;
      anchors = Array.from(sideBarGroupRef.querySelectorAll("a"));
    } else {
      if (!tabsGroupRef) return;
      anchors = Array.from(tabsGroupRef.querySelectorAll("a"));
    }

    const fromElement = anchors.find(
      (a) => a.getAttribute("href") === fromPathName,
    );
    if (!fromElement) return;
    const fromPosition = anchors.indexOf(fromElement);

    const toElement = anchors.find(
      (a) => a.getAttribute("href") === toPathName,
    );
    if (!toElement) return;
    const toPosition = anchors.indexOf(toElement);

    if (fromPosition > toPosition) {
      if (isDesktopViewport()) {
        animationDirection = "up";
      } else {
        animationDirection = "left";
      }
    } else {
      if (isDesktopViewport()) {
        animationDirection = "down";
      } else {
        animationDirection = "right";
      }
    }
  });
</script>

<SideBarOrTabs>
  {#snippet sidebarElements()}
    <SideBarElementGroup bind:bindableGroupRef={sideBarGroupRef}>
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
    <TabElementGroup bind:bindableGroupRef={tabsGroupRef}>
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
          y:
            animationDirection === "up"
              ? -200
              : animationDirection === "down"
                ? 200
                : undefined,
          x:
            animationDirection === "left"
              ? -200
              : animationDirection === "right"
                ? 200
                : undefined,
          duration: 200,
          delay: 200,
        }}
        out:fly={{
          y:
            animationDirection === "up"
              ? 160
              : animationDirection === "down"
                ? -160
                : undefined,
          x:
            animationDirection === "left"
              ? 160
              : animationDirection === "right"
                ? -160
                : undefined,
          duration: 160,
        }}
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
      <ButtonOrAnchor class="transition-all hover:invert-100">
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
      <ButtonOrAnchor class="text-text-primary hover:underline" href="/support"
        >Support</ButtonOrAnchor
      >
    </div>
  {/snippet}
</SideBarOrTabs>
