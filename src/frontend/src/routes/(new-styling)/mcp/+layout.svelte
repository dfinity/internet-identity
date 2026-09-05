<script lang="ts">
  import type { LayoutProps } from "./$types";
  import { ChevronDownIcon, UserIcon } from "@lucide/svelte";
  import { lastUsedIdentitiesStore } from "$lib/stores/last-used-identities.store";
  import { forgetIdentity } from "$lib/stores/session-delegation.store";
  import { t } from "$lib/stores/locale.store";
  import { AuthWizard } from "$lib/components/wizards/auth";
  import Header from "$lib/components/layout/Header.svelte";
  import Footer from "$lib/components/layout/Footer.svelte";
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import Popover from "$lib/components/ui/Popover.svelte";
  import IdentitySwitcher from "$lib/components/ui/IdentitySwitcher.svelte";
  import ManageIdentities from "$lib/components/ui/ManageIdentities.svelte";
  import ManageHandoff from "$lib/components/ui/ManageHandoff.svelte";
  import Avatar from "$lib/components/ui/Avatar.svelte";
  import { handleError } from "$lib/components/utils/error";
  import { toaster } from "$lib/components/utils/toaster";
  import { ManageHandoffFlow } from "$lib/flows/manageHandoffFlow.svelte";
  import {
    showIdentitySwitcher,
    identitySwitcherTrigger,
    connectScreenActive,
  } from "./mcp-switcher.store";

  const { children }: LayoutProps = $props();

  // --- Identity switcher state ---
  const lastUsedIdentities = $derived(
    Object.values($lastUsedIdentitiesStore.identities).sort(
      (a, b) => b.lastUsedTimestampMillis - a.lastUsedTimestampMillis,
    ),
  );
  const selectedIdentity = $derived($lastUsedIdentitiesStore.selected);

  let identityButtonRef = $state<HTMLElement>();
  let isAuthDialogOpen = $state(false);
  let isManageIdentitiesDialogOpen = $state(false);

  const closeIdentitySwitcher = (): void =>
    identitySwitcherTrigger.set(undefined);

  $effect(() => {
    if (!$showIdentitySwitcher) {
      closeIdentitySwitcher();
    }
  });

  // Authenticate the selected identity here, then open /manage in a new tab
  // carrying the session — so the user doesn't have to sign in again there.
  const manageHandoff = new ManageHandoffFlow();
  const handleManageIdentity = async (): Promise<void> => {
    closeIdentitySwitcher();
    if (selectedIdentity === undefined) {
      return;
    }
    try {
      await manageHandoff.start("/manage", selectedIdentity);
    } catch (error) {
      handleError(error);
    }
  };

  // Switching identity only *selects* here — unlike the authorize route, it
  // doesn't sign in. The page's Allow access button performs the authentication
  // for the selected identity, so the user always makes that explicit choice.
  const handleSelectIdentity = (identityNumber: bigint): Promise<void> => {
    lastUsedIdentitiesStore.selectIdentity(identityNumber);
    closeIdentitySwitcher();
    isAuthDialogOpen = false;
    return Promise.resolve();
  };
  // The "use another identity" dialog signs in through its own AuthWizard;
  // these run once it has authenticated, so they only select and notify.
  const handleSignUp = async (identityNumber: bigint): Promise<void> => {
    await handleSelectIdentity(identityNumber);
    toaster.success({
      title: $t`You're all set. Your identity has been created.`,
      duration: 4000,
    });
  };
  const handleRemoveIdentity = (identityNumber: bigint) => {
    const isCurrent = selectedIdentity?.identityNumber === identityNumber;
    if (isCurrent) {
      const nextIdentity = lastUsedIdentities.find(
        (identity) => identity.identityNumber !== identityNumber,
      );
      if (nextIdentity !== undefined) {
        lastUsedIdentitiesStore.selectIdentity(nextIdentity.identityNumber);
      }
    }

    const removedIdentity =
      $lastUsedIdentitiesStore.identities[`${identityNumber}`];
    lastUsedIdentitiesStore.removeIdentity(identityNumber);
    void forgetIdentity(identityNumber);

    isManageIdentitiesDialogOpen = false;
    if (removedIdentity !== undefined) {
      const identityName =
        removedIdentity.name ?? `${removedIdentity.identityNumber}`;
      toaster.create({
        title: $t`Identity removed`,
        description: $t`${identityName} has been removed from this device. Apps you were signed into here have been signed out.`,
        closable: true,
        duration: 5000,
        action: {
          label: $t`Undo`,
          onClick: () => {
            lastUsedIdentitiesStore.restoreIdentity(removedIdentity);
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
</script>

<div class="flex min-h-[100dvh] flex-col" data-page="mcp-authorize-view">
  <div class="h-[env(safe-area-inset-top)]"></div>
  {#if !$connectScreenActive}
    <Header>
      {#if selectedIdentity !== undefined && $showIdentitySwitcher}
        <button
          bind:this={identityButtonRef}
          class="btn btn-tertiary ms-auto gap-2.5 pe-3 md:-me-3"
          onclick={() => identitySwitcherTrigger.set("header")}
          aria-label="Switch identity"
        >
          <Avatar size="xs">
            <UserIcon class="size-4" />
          </Avatar>
          <span>{selectedIdentity.name ?? selectedIdentity.identityNumber}</span
          >
          <ChevronDownIcon class="size-4" />
        </button>
      {/if}
    </Header>
  {/if}

  <div class="flex flex-1 flex-col items-center justify-center">
    {@render children()}
  </div>

  <Footer />
  <div class="h-[env(safe-area-inset-bottom)]"></div>
</div>

{#if selectedIdentity !== undefined && $showIdentitySwitcher}
  {#snippet switcher()}
    <IdentitySwitcher
      selected={selectedIdentity.identityNumber}
      identities={lastUsedIdentities}
      onSwitchIdentity={(identityNumber) =>
        handleSelectIdentity(identityNumber)}
      onUseAnotherIdentity={() => {
        closeIdentitySwitcher();
        isAuthDialogOpen = true;
      }}
      onManageIdentity={handleManageIdentity}
      onManageIdentities={() => {
        closeIdentitySwitcher();
        isManageIdentitiesDialogOpen = true;
      }}
      onError={(error) => {
        closeIdentitySwitcher();
        handleError(error);
      }}
      onClose={closeIdentitySwitcher}
    />
  {/snippet}
  {#if $identitySwitcherTrigger === "row"}
    <Dialog
      onClose={closeIdentitySwitcher}
      showCloseButton={false}
      contentClass="!p-0"
      class="!bg-bg-primary"
    >
      {@render switcher()}
    </Dialog>
  {:else if $identitySwitcherTrigger === "header"}
    <Popover
      anchor={identityButtonRef}
      onClose={closeIdentitySwitcher}
      direction="down"
      align="end"
      distance="0.75rem"
      class="!bg-bg-primary"
    >
      {@render switcher()}
    </Popover>
  {/if}
  {#if isAuthDialogOpen}
    <Dialog onClose={() => (isAuthDialogOpen = false)}>
      <AuthWizard
        mode="signin"
        onSignIn={handleSelectIdentity}
        onSignUp={handleSignUp}
        onError={(error) => {
          isAuthDialogOpen = false;
          handleError(error);
        }}
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

{#if isManageIdentitiesDialogOpen}
  <Dialog onClose={() => (isManageIdentitiesDialogOpen = false)}>
    <ManageIdentities
      identities={lastUsedIdentities}
      onRemoveIdentity={handleRemoveIdentity}
    />
  </Dialog>
{/if}

<ManageHandoff flow={manageHandoff} />
