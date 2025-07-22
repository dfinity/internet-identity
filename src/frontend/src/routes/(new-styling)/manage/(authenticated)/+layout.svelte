<script lang="ts">
  import ButtonOrAnchor from "$lib/components/utils/ButtonOrAnchor.svelte";
  import { ChevronDownIcon } from "@lucide/svelte";
  import { fly } from "svelte/transition";
  import { page } from "$app/state";
  import { beforeNavigate, goto } from "$app/navigation";
  import { isDesktopViewport } from "$lib/utils/UI/deviceDetection";
  import { expoIn, expoOut } from "svelte/easing";
  import IdentitySwitcher from "$lib/components/ui/IdentitySwitcher.svelte";
  import { authenticatedStore } from "$lib/stores/authentication.store";
  import { lastUsedIdentitiesStore } from "$lib/stores/last-used-identities.store";
  import Button from "$lib/components/ui/Button.svelte";
  import identityInfo from "$lib/stores/identity-info.state.svelte";
  import Popover from "$lib/components/ui/Popover.svelte";
  import { toaster } from "$lib/components/utils/toaster";
  import { AuthLastUsedFlow } from "$lib/flows/authLastUsedFlow.svelte";
  import MainContent from "$lib/components/layout/MainContent.svelte";
  import { handleError } from "$lib/components/utils/error";
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import AuthWizard from "$lib/components/wizards/auth/AuthWizard.svelte";
  import Footer from "$lib/components/layout/Footer.svelte";

  const { children } = $props();

  let divRef = $state<HTMLDivElement>();

  let sideBarGroupRef = $state<HTMLDivElement>();
  let tabsGroupRef = $state<HTMLDivElement>();
  let identityButtonRef = $state<HTMLElement>();

  let animationDirection = $state<"up" | "down" | "left" | "right">("up");

  let isIdentityPopoverOpen = $state(false);
  let isAuthDialogOpen = $state(false);
  let isAuthenticating = $state(false);

  const lastUsedIdentities = $derived(
    Object.values($lastUsedIdentitiesStore.identities)
      .sort((a, b) => b.lastUsedTimestampMillis - a.lastUsedTimestampMillis)
      .slice(0, 3),
  );

  const gotoManage = () => goto("/manage", { replaceState: true });
  const onSignIn = async (identityNumber: bigint) => {
    identityInfo.reset();
    lastUsedIdentitiesStore.selectIdentity(identityNumber);
    identityInfo.fetch();
    await gotoManage();
    isAuthDialogOpen = false;
  };
  const onSignUp = async (identityNumber: bigint) => {
    identityInfo.reset();
    toaster.success({
      title: "You're all set. Your identity has been created.",
      duration: 2000,
    });
    lastUsedIdentitiesStore.selectIdentity(identityNumber);
    identityInfo.fetch();
    await gotoManage();
    isAuthDialogOpen = false;
  };

  const handleSwitchIdentity = async (identityNumber: bigint) => {
    const authLastUsedFlow = new AuthLastUsedFlow();
    const chosenIdentity =
      $lastUsedIdentitiesStore.identities[Number(identityNumber)];
    await authLastUsedFlow.authenticate(chosenIdentity);
    identityInfo.reset();
    lastUsedIdentitiesStore.selectIdentity(identityNumber);
    identityInfo.fetch();
    await gotoManage();
    isIdentityPopoverOpen = false;
  };

  const flyInX = $derived(
    animationDirection === "left"
      ? -200
      : animationDirection === "right"
        ? 200
        : undefined,
  );

  const flyInY = $derived(
    animationDirection === "up"
      ? -200
      : animationDirection === "down"
        ? 200
        : undefined,
  );

  const flyOutX = $derived(
    animationDirection === "left"
      ? 160
      : animationDirection === "right"
        ? -160
        : undefined,
  );

  const flyOutY = $derived(
    animationDirection === "up"
      ? 160
      : animationDirection === "down"
        ? -160
        : undefined,
  );

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

<!-- TODO: Reenable with SideBarOrTabs once we support multiple dashboard pages -->
<!-- {#snippet sidebarElements()}
  <SideBarElementGroup bind:bindableGroupRef={sideBarGroupRef}>
    <SideBarElement href="/manage">
      <LucideHome size="1rem" class="stroke-fg-quaternary" />
      <h1>Home</h1>
    </SideBarElement>
    <SideBarElement class="text-text-primary" href="/manage/security">
      <Shield size="1rem" class="stroke-fg-quaternary" />
      <h1>Security</h1>
    </SideBarElement>
  </SideBarElementGroup>
{/snippet}

{#snippet tabElements()}
  <TabElementGroup bind:bindableGroupRef={tabsGroupRef}>
    <TabElement class="text-text-primary" href="/manage">
      <h1>Home</h1>
    </TabElement>
    <TabElement class="text-text-primary" href="/manage/security">
      <h1>Security</h1>
    </TabElement>
  </TabElementGroup>
{/snippet} -->

<MainContent>
  {#snippet content()}
    {#key page.url.pathname}
      <div
        bind:this={divRef}
        in:fly={{
          y: flyInY,
          x: flyInX,
          duration: 160,
          delay: 160,
          easing: expoOut,
        }}
        out:fly={{
          y: flyOutY,
          x: flyOutX,
          duration: 160,
          easing: expoIn,
        }}
        onoutrostart={() => divRef?.setAttribute("aria-hidden", "true")}
        class="nth-2:hidden"
      >
        {@render children?.()}
      </div>
    {/key}
  {/snippet}

  {#snippet header()}
    <div class="flex">
      <div class="flex-1"></div>
      <Button
        bind:element={identityButtonRef}
        onclick={() => (isIdentityPopoverOpen = true)}
        variant="tertiary"
        class="ml-auto gap-2.5 pr-3 md:-mr-3"
        aria-label="Switch identity"
      >
        <span>{identityInfo.name ?? $authenticatedStore.identityNumber}</span>
        <ChevronDownIcon size="1rem" />
      </Button>
      {#if isIdentityPopoverOpen}
        <Popover
          anchor={identityButtonRef}
          onClose={() => (isIdentityPopoverOpen = false)}
          direction="down"
          align="end"
          distance="0.75rem"
        >
          <IdentitySwitcher
            selected={$authenticatedStore.identityNumber}
            identities={lastUsedIdentities}
            switchIdentity={handleSwitchIdentity}
            useAnotherIdentity={() => {
              isIdentityPopoverOpen = false;
              isAuthDialogOpen = true;
            }}
            onClose={() => (isIdentityPopoverOpen = false)}
            onLogout={identityInfo.logout}
          />
        </Popover>
      {/if}
      {#if isAuthDialogOpen}
        <Dialog
          onClose={() => (isAuthDialogOpen = false)}
          showCloseButton={!isAuthenticating}
          closeOnOutsideClick={!isAuthenticating}
        >
          <AuthWizard
            bind:isAuthenticating
            {onSignIn}
            {onSignUp}
            onError={(error) => {
              isAuthDialogOpen = false;
              handleError(error);
            }}
            withinDialog
          >
            <h1 class="text-text-primary my-2 self-start text-2xl font-medium">
              Use another identity
            </h1>
            <p class="text-text-secondary mb-6 self-start text-sm">
              choose method
            </p>
          </AuthWizard>
        </Dialog>
      {/if}
    </div>
  {/snippet}
  {#snippet footer()}
    <Footer />
  {/snippet}
</MainContent>
