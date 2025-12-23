<script lang="ts">
  import type { LayoutProps } from "./$types";
  import { onMount } from "svelte";
  import {
    authorizationStore,
    authorizationStatusStore,
  } from "$lib/stores/authorization.store";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import { nonNullish } from "@dfinity/utils";
  import { lastUsedIdentitiesStore } from "$lib/stores/last-used-identities.store";
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import { ChevronDownIcon } from "@lucide/svelte";
  import Header from "$lib/components/layout/Header.svelte";
  import Footer from "$lib/components/layout/Footer.svelte";
  import { authenticationStore } from "$lib/stores/authentication.store";
  import { afterNavigate, goto, replaceState } from "$app/navigation";
  import { toaster } from "$lib/components/utils/toaster";
  import IdentitySwitcher from "$lib/components/ui/IdentitySwitcher.svelte";
  import Popover from "$lib/components/ui/Popover.svelte";
  import { handleError } from "$lib/components/utils/error";
  import { AuthWizard } from "$lib/components/wizards/auth";
  import { page } from "$app/state";
  import { sessionStore } from "$lib/stores/session.store";
  import AuthorizeError from "$lib/components/views/AuthorizeError.svelte";
  import { t } from "$lib/stores/locale.store";
  import { AuthLastUsedFlow } from "$lib/flows/authLastUsedFlow.svelte";

  const { children, data }: LayoutProps = $props();

  const authLastUsedFlow = new AuthLastUsedFlow();

  const lastUsedIdentities = $derived(
    Object.values($lastUsedIdentitiesStore.identities)
      .sort((a, b) => b.lastUsedTimestampMillis - a.lastUsedTimestampMillis)
      .slice(0, 3),
  );
  const selectedIdentity = $derived($lastUsedIdentitiesStore.selected);
  const status = $derived($authorizationStatusStore);

  let identityButtonRef = $state<HTMLElement>();
  let isIdentityPopoverOpen = $state(false);
  let isAuthDialogOpen = $state(false);
  let isAuthenticating = $state(false);

  const handleSignIn = async (identityNumber: bigint) => {
    isAuthenticating = true;
    if ($authenticationStore?.identityNumber !== identityNumber) {
      // Sign in if not authenticated with this identity yet
      await sessionStore.reset();
      await authLastUsedFlow.authenticate(
        $lastUsedIdentitiesStore.identities[`${identityNumber}`],
      );
    }
    lastUsedIdentitiesStore.selectIdentity(identityNumber);
    isIdentityPopoverOpen = false;
    isAuthDialogOpen = false;
    isAuthenticating = false;
  };
  const handleSignUp = async (identityNumber: bigint) => {
    await handleSignIn(identityNumber);
    toaster.success({
      title: $t`You're all set. Your identity has been created.`,
      duration: 4000,
    });
  };
  const handleUpgrade = async (identityNumber: bigint) => {
    await handleSignIn(identityNumber);
    await goto("/authorize/upgrade-success");
  };

  onMount(() => {
    authorizationStore.init({
      // Use either legacy PostMessage protocol or ICRC-29 PostMessage protocol
      legacyProtocol: data.legacyProtocol,
    });
  });

  // Pre-fetch passkey credential ids
  $effect(() =>
    authLastUsedFlow.init(
      lastUsedIdentities.map(({ identityNumber }) => identityNumber),
    ),
  );

  // Remove legacyProtocol param from URL bar after initializing
  afterNavigate(() => {
    if (page.url.searchParams.has("legacyProtocol")) {
      const next = new URL(page.url);
      next.searchParams.delete("legacyProtocol");
      replaceState(next, {});
    }
  });

  $effect(() => {
    if (status === "orphan") {
      goto("/unsupported", {
        replaceState: true,
        invalidateAll: true,
      });
    }
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
        <ChevronDownIcon class="size-4" />
      </Button>
      {#if isIdentityPopoverOpen}
        <Popover
          anchor={identityButtonRef}
          onClose={() => {
            if (isAuthenticating) {
              return;
            }
            isIdentityPopoverOpen = false;
          }}
          direction="down"
          align="end"
          distance="0.75rem"
        >
          <IdentitySwitcher
            selected={selectedIdentity.identityNumber}
            identities={lastUsedIdentities}
            onSwitchIdentity={handleSignIn}
            onUseAnotherIdentity={() => {
              isIdentityPopoverOpen = false;
              isAuthDialogOpen = true;
            }}
            onError={(error) => {
              isIdentityPopoverOpen = false;
              handleError(error);
            }}
            onClose={() => (isIdentityPopoverOpen = false)}
          />
        </Popover>
      {/if}
      {#if isAuthDialogOpen}
        <Dialog
          onClose={() => {
            if (isAuthenticating) {
              return;
            }
            isAuthDialogOpen = false;
          }}
        >
          <AuthWizard
            onSignIn={handleSignIn}
            onSignUp={handleSignUp}
            onUpgrade={handleUpgrade}
            onError={(error) => {
              isAuthDialogOpen = false;
              handleError(error);
            }}
            withinDialog
          >
            <h1 class="text-text-primary my-2 self-start text-2xl font-medium">
              {$t`Sign in`}
            </h1>
            <p class="text-text-secondary mb-6 self-start text-sm">
              {$t`choose method to continue`}
            </p>
          </AuthWizard>
        </Dialog>
      {/if}
    {/if}
  </Header>
  <div class="flex flex-1 flex-col items-center justify-center">
    {#if status === "authenticating"}
      {@render children()}
    {:else if status === "authorizing"}
      <!-- Spinner is not shown for other statuses to avoid flicker -->
      <div class="flex flex-col items-center justify-center gap-4">
        <ProgressRing class="text-fg-primary size-14" />
        <p class="text-text-secondary text-lg">{$t`Redirecting to the app`}</p>
      </div>
    {/if}
  </div>
  <Footer />
  <div class="h-[env(safe-area-inset-bottom)]"></div>
</div>

<!-- Renders any error status or late success status dialog when needed -->
<AuthorizeError {status} />
