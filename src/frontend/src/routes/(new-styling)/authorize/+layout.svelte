<script lang="ts">
  import type { LayoutProps } from "./$types";
  import { onMount } from "svelte";
  import {
    authorizationStore,
    authorizationStatusStore,
  } from "$lib/stores/authorization.store";
  import AuthPanel from "$lib/components/layout/AuthPanel.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import { fly, scale } from "svelte/transition";
  import { nonNullish } from "@dfinity/utils";
  import { lastUsedIdentitiesStore } from "$lib/stores/last-used-identities.store";
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import {
    RotateCcwIcon,
    CircleAlertIcon,
    ChevronDownIcon,
  } from "@lucide/svelte";
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import Header from "$lib/components/layout/Header.svelte";
  import Footer from "$lib/components/layout/Footer.svelte";
  import { authenticationStore } from "$lib/stores/authentication.store";
  import { goto } from "$app/navigation";
  import { toaster } from "$lib/components/utils/toaster";
  import IdentitySwitcher from "$lib/components/ui/IdentitySwitcher.svelte";
  import Popover from "$lib/components/ui/Popover.svelte";
  import { handleError } from "$lib/components/utils/error";
  import { AuthWizard } from "$lib/components/wizards/auth";

  const { children }: LayoutProps = $props();

  const lastUsedIdentities = $derived(
    Object.values($lastUsedIdentitiesStore.identities)
      .sort((a, b) => b.lastUsedTimestampMillis - a.lastUsedTimestampMillis)
      .slice(0, 3),
  );
  const selectedIdentity = $derived($lastUsedIdentitiesStore.selected);
  const status = $derived($authorizationStatusStore);

  let animationWrapperRef = $state<HTMLElement>();
  let identityButtonRef = $state<HTMLElement>();
  let isIdentityPopoverOpen = $state(false);
  let isAuthDialogOpen = $state(false);
  let isAuthenticating = $state(false);

  const gotoAccounts = () =>
    goto("/authorize/account", {
      replaceState: true,
      invalidateAll: true,
      state: { disableNavigationAnimation: true },
    });
  const onSignIn = async (identityNumber: bigint) => {
    lastUsedIdentitiesStore.selectIdentity(identityNumber);
    await gotoAccounts();
    isAuthDialogOpen = false;
  };
  const onSignUp = async (identityNumber: bigint) => {
    toaster.success({
      title: "You're all set. Your identity has been created.",
      duration: 4000,
      closable: false,
    });
    lastUsedIdentitiesStore.selectIdentity(identityNumber);
    await gotoAccounts();
    isAuthDialogOpen = false;
  };

  onMount(() => {
    authorizationStore.init();
  });
</script>

<div class="flex min-h-[100dvh] flex-col" data-page="new-authorize-view">
  <div class="h-[env(safe-area-inset-top)]"></div>
  <Header>
    {#if nonNullish(selectedIdentity)}
      <Button
        bind:element={identityButtonRef}
        onclick={() => (isIdentityPopoverOpen = true)}
        variant="tertiary"
        class="ml-auto gap-2.5 pr-3 md:-mr-3"
        aria-label="Switch identity"
      >
        <span>{selectedIdentity.name ?? selectedIdentity.identityNumber}</span>
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
            selected={selectedIdentity.identityNumber}
            identities={lastUsedIdentities}
            switchIdentity={(identityNumber) => {
              authenticationStore.reset();
              lastUsedIdentitiesStore.selectIdentity(identityNumber);
              isIdentityPopoverOpen = false;
            }}
            useAnotherIdentity={() => {
              isIdentityPopoverOpen = false;
              isAuthDialogOpen = true;
            }}
            onClose={() => (isIdentityPopoverOpen = false)}
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
    {/if}
  </Header>
  <div class="flex flex-1 flex-col items-center justify-center">
    {#if status === "authenticating"}
      <div
        class="grid w-full flex-1 items-center max-sm:items-stretch sm:max-w-100"
      >
        {#if nonNullish(selectedIdentity)}
          {#key selectedIdentity.identityNumber}
            <div
              bind:this={animationWrapperRef}
              class="col-start-1 row-start-1 flex flex-col"
              in:fly={{ duration: 300, y: 60, delay: 200 }}
              out:scale={{ duration: 500, start: 0.9 }}
              onoutrostart={() =>
                animationWrapperRef?.setAttribute("aria-hidden", "true")}
            >
              <AuthPanel>
                {@render children()}
              </AuthPanel>
            </div>
          {/key}
        {:else}
          <div class="col-start-1 row-start-1 flex flex-col">
            <AuthPanel>
              {@render children()}
            </AuthPanel>
          </div>
        {/if}
      </div>
    {:else if status === "authorizing"}
      <!-- Spinner is not shown for other statuses to avoid flicker -->
      <div class="flex flex-col items-center justify-center gap-4">
        <ProgressRing class="text-fg-primary size-14" />
        <p class="text-text-secondary text-lg">Redirecting to the app</p>
      </div>
    {:else if status === "orphan" || status === "closed" || status === "invalid" || status === "failure" || status === "unverified-origin"}
      {@const title = {
        orphan: "Missing request",
        closed: "Connection closed",
        invalid: "Invalid request",
        failure: "Something went wrong",
        "unverified-origin": "Unverified origin",
      }[status]}
      {@const description = {
        orphan:
          "There was an issue connecting with the application. Try a different browser; if the issue persists, contact the developer.",
        closed:
          "It seems like the connection with the service could not be established. Try a different browser; if the issue persists, contact support.",
        invalid:
          "It seems like an invalid authentication request was received.",
        failure:
          "Something went wrong during authentication. Authenticating service was notified and you may close this page.",
        "unverified-origin":
          "There was an error verifying the origin of the request. Authenticating service was notified and you may close this page.",
      }[status]}
      <Dialog>
        <FeaturedIcon size="lg" variant="error" class="mb-4 self-start">
          <CircleAlertIcon size="1.5rem" />
        </FeaturedIcon>
        <h1 class="text-text-primary mb-3 text-2xl font-medium">{title}</h1>
        <p class="text-md text-text-tertiary mb-6 font-medium">{description}</p>
        <Button onclick={() => window.close()} variant="secondary">
          <RotateCcwIcon size="1rem" />
          Return to app
        </Button>
      </Dialog>
    {:else if status === "late-success"}
      <Dialog>
        <FeaturedIcon size="lg" class="mb-4 self-start">
          <CircleAlertIcon size="1.5rem" />
        </FeaturedIcon>
        <h1 class="text-text-primary mb-3 text-2xl font-medium">
          Authentication successful
        </h1>
        <p class="text-md text-text-tertiary mb-6 font-medium">
          You may close this page.
        </p>
        <Button onclick={() => window.close()} variant="secondary">
          <RotateCcwIcon size="1rem" />
          Return to app
        </Button>
      </Dialog>
    {/if}
  </div>
  <Footer />
  <div class="h-[env(safe-area-inset-bottom)]"></div>
</div>
