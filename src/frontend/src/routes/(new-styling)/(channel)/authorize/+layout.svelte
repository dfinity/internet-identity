<script lang="ts">
  import type { LayoutProps } from "./$types";
  import {
    authorizationContextStore,
    authorizationStore,
    authorizedStore,
  } from "$lib/stores/authorization.store";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import { lastUsedIdentitiesStore } from "$lib/stores/last-used-identities.store";
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import { ChevronDownIcon, UserIcon } from "@lucide/svelte";
  import Header from "$lib/components/layout/Header.svelte";
  import Footer from "$lib/components/layout/Footer.svelte";
  import { authenticationStore } from "$lib/stores/authentication.store";
  import { goto } from "$app/navigation";
  import { toaster } from "$lib/components/utils/toaster";
  import IdentitySwitcher from "$lib/components/ui/IdentitySwitcher.svelte";
  import ManageIdentities from "$lib/components/ui/ManageIdentities.svelte";
  import Popover from "$lib/components/ui/Popover.svelte";
  import { handleError } from "$lib/components/utils/error";
  import { AuthWizard } from "$lib/components/wizards/auth";
  import { sessionStore } from "$lib/stores/session.store";
  import { t } from "$lib/stores/locale.store";
  import { AuthLastUsedFlow } from "$lib/flows/authLastUsedFlow.svelte";
  import { throwCanisterError, waitFor } from "$lib/utils/utils";
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import { CircleAlertIcon, RotateCcwIcon } from "@lucide/svelte";
  import { onMount } from "svelte";
  import { analytics } from "$lib/utils/analytics/analytics";
  import Avatar from "$lib/components/ui/Avatar.svelte";
  import { triggerDropWaveAnimation } from "$lib/utils/animation-dispatcher";

  const { children }: LayoutProps = $props();

  const authLastUsedFlow = new AuthLastUsedFlow();

  const lastUsedIdentities = $derived(
    Object.values($lastUsedIdentitiesStore.identities).sort(
      (a, b) => b.lastUsedTimestampMillis - a.lastUsedTimestampMillis,
    ),
  );
  const selectedIdentity = $derived($lastUsedIdentitiesStore.selected);

  let identityButtonRef = $state<HTMLElement>();
  let isIdentityPopoverOpen = $state(false);
  let isAuthDialogOpen = $state(false);
  let isAuthenticating = $state(false);
  let isManageIdentitiesDialogOpen = $state(false);

  const handleSignIn = async (identityNumber: bigint) => {
    try {
      isAuthenticating = true;
      if ($authenticationStore?.identityNumber !== identityNumber) {
        // Sign in if not authenticated with this identity yet
        sessionStore.reset();
        await authLastUsedFlow.authenticate(
          $lastUsedIdentitiesStore.identities[`${identityNumber}`],
        );
      }
      lastUsedIdentitiesStore.selectIdentity(identityNumber);
    } finally {
      isIdentityPopoverOpen = false;
      isAuthDialogOpen = false;
      isAuthenticating = false;
    }
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
  const handleRemoveIdentity = (identityNumber: bigint) => {
    // If the removed identity is currently selected,
    // switch to the next available one in the list.
    //
    // The last identity cannot be removed, so there will
    // always be at least one next identity available.
    const isCurrent = selectedIdentity?.identityNumber === identityNumber;
    if (isCurrent) {
      const nextIdentity = lastUsedIdentities.find(
        (identity) => identity.identityNumber !== identityNumber,
      );
      if (nextIdentity !== undefined) {
        lastUsedIdentitiesStore.selectIdentity(nextIdentity.identityNumber);
      }
    }

    // Now we can remove it safely
    const removedIdentity =
      $lastUsedIdentitiesStore.identities[`${identityNumber}`];
    lastUsedIdentitiesStore.removeIdentity(identityNumber);

    // Hide manage identities dialog and show toaster with undo option
    isManageIdentitiesDialogOpen = false;
    if (removedIdentity !== undefined) {
      const identityName =
        removedIdentity.name ?? `${removedIdentity.identityNumber}`;
      toaster.create({
        title: $t`Identity removed`,
        description: $t`${identityName} has been removed from this device.`,
        closable: true,
        duration: 5000,
        action: {
          label: $t`Undo`,
          onClick: () => {
            // Restore removed identity
            lastUsedIdentitiesStore.restoreIdentity(removedIdentity);
            // Switch to it if it was current
            if (isCurrent) {
              lastUsedIdentitiesStore.selectIdentity(
                removedIdentity.identityNumber,
              );
            }
          },
        },
      });
    }
  };
  const authorizeDefault = async () => {
    try {
      const { identityNumber, actor } = $authenticationStore!;
      const origin = $authorizationContextStore.effectiveOrigin;
      const accountNumber = await actor
        .get_default_account(identityNumber, origin)
        .then(throwCanisterError)
        .then((account) => account.account_number[0]);
      void triggerDropWaveAnimation();
      authorizationStore.authorize(accountNumber);
    } catch (error) {
      handleError(error);
    }
  };

  // Pre-fetch passkey credential ids
  $effect(() =>
    authLastUsedFlow.init(
      lastUsedIdentities.map(({ identityNumber }) => identityNumber),
    ),
  );

  // Track page view for authorization flow
  onMount(() => {
    analytics.pageView();
  });
</script>

<div class="flex min-h-[100dvh] flex-col" data-page="new-authorize-view">
  <div class="h-[env(safe-area-inset-top)]"></div>
  <Header>
    {#if selectedIdentity !== undefined}
      <Button
        bind:element={identityButtonRef}
        onclick={() => (isIdentityPopoverOpen = true)}
        variant="tertiary"
        class="ms-auto gap-2.5 pe-3 md:-me-3"
        aria-label="Switch identity"
      >
        <Avatar size="xs">
          <UserIcon class="size-4" />
        </Avatar>
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
          class="!bg-bg-primary"
        >
          <IdentitySwitcher
            selected={selectedIdentity.identityNumber}
            identities={lastUsedIdentities}
            onSwitchIdentity={async (identityNumber) => {
              await handleSignIn(identityNumber);
              await authorizeDefault();
            }}
            onUseAnotherIdentity={() => {
              isIdentityPopoverOpen = false;
              isAuthDialogOpen = true;
            }}
            onManageIdentity={async () => {
              isIdentityPopoverOpen = false;
              window.open("/manage", "_blank");
            }}
            onManageIdentities={() => {
              isIdentityPopoverOpen = false;
              isManageIdentitiesDialogOpen = true;
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
              {$t`Choose method to continue`}
            </p>
          </AuthWizard>
        </Dialog>
      {/if}
    {/if}
  </Header>
  <div class="flex flex-1 flex-col items-center justify-center">
    {#if $authorizedStore !== undefined}
      {#await waitFor(10000)}
        <div class="flex flex-col items-center justify-center gap-4">
          <ProgressRing class="text-fg-primary size-14" />
          <p class="text-text-secondary text-lg">
            {$t`Redirecting to the app`}
          </p>
        </div>
      {:then _}
        <Dialog>
          <FeaturedIcon size="lg" class="mb-4 self-start">
            <CircleAlertIcon class="size-6" />
          </FeaturedIcon>
          <h1 class="text-text-primary mb-3 text-2xl font-medium">
            {$t`Authentication successful`}
          </h1>
          <p class="text-text-tertiary mb-6 text-base font-medium">
            {$t`You may close this page.`}
          </p>
          <Button onclick={() => window.close()} variant="secondary">
            <RotateCcwIcon class="size-4" />
            <span>{$t`Return to app`}</span>
          </Button>
        </Dialog>
      {/await}
    {:else}
      {@render children()}
    {/if}
  </div>
  <Footer />
  <div class="h-[env(safe-area-inset-bottom)]"></div>
</div>

{#if isManageIdentitiesDialogOpen}
  <Dialog onClose={() => (isManageIdentitiesDialogOpen = false)}>
    <ManageIdentities
      identities={lastUsedIdentities}
      onRemoveIdentity={handleRemoveIdentity}
    />
  </Dialog>
{/if}
