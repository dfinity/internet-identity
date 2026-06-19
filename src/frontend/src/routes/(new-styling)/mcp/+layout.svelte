<script lang="ts">
  import type { LayoutProps } from "./$types";
  import { ChevronDownIcon, UserIcon } from "@lucide/svelte";
  import { lastUsedIdentitiesStore } from "$lib/stores/last-used-identities.store";
  import { t } from "$lib/stores/locale.store";
  import { AuthWizard } from "$lib/components/wizards/auth";
  import Header from "$lib/components/layout/Header.svelte";
  import Footer from "$lib/components/layout/Footer.svelte";
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import Popover from "$lib/components/ui/Popover.svelte";
  import IdentitySwitcher from "$lib/components/ui/IdentitySwitcher.svelte";
  import ManageIdentities from "$lib/components/ui/ManageIdentities.svelte";
  import Avatar from "$lib/components/ui/Avatar.svelte";
  import { handleError } from "$lib/components/utils/error";
  import { toaster } from "$lib/components/utils/toaster";
  import { showIdentitySwitcher } from "./mcp-switcher.store";

  const { children }: LayoutProps = $props();

  // --- Identity switcher state ---
  const lastUsedIdentities = $derived(
    Object.values($lastUsedIdentitiesStore.identities).sort(
      (a, b) => b.lastUsedTimestampMillis - a.lastUsedTimestampMillis,
    ),
  );
  const selectedIdentity = $derived($lastUsedIdentitiesStore.selected);

  let identityButtonRef = $state<HTMLElement>();
  let isIdentityPopoverOpen = $state(false);
  let isAuthDialogOpen = $state(false);
  let isManageIdentitiesDialogOpen = $state(false);

  // Switching identity only *selects* here — unlike the authorize route, it
  // doesn't sign in. The page's Allow access button performs the authentication
  // for the selected identity, so the user always makes that explicit choice.
  const handleSelectIdentity = (identityNumber: bigint): Promise<void> => {
    lastUsedIdentitiesStore.selectIdentity(identityNumber);
    isIdentityPopoverOpen = false;
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
  <Header>
    {#if selectedIdentity !== undefined && $showIdentitySwitcher}
      <button
        bind:this={identityButtonRef}
        class="btn btn-tertiary ms-auto gap-2.5 pe-3 md:-me-3"
        onclick={() => (isIdentityPopoverOpen = true)}
        aria-label="Switch identity"
      >
        <Avatar size="xs">
          <UserIcon class="size-4" />
        </Avatar>
        <span>{selectedIdentity.name ?? selectedIdentity.identityNumber}</span>
        <ChevronDownIcon class="size-4" />
      </button>
      {#if isIdentityPopoverOpen}
        <Popover
          anchor={identityButtonRef}
          onClose={() => (isIdentityPopoverOpen = false)}
          direction="down"
          align="end"
          distance="0.75rem"
          class="!bg-bg-primary"
        >
          <IdentitySwitcher
            selected={selectedIdentity.identityNumber}
            identities={lastUsedIdentities}
            onSwitchIdentity={(identityNumber) =>
              handleSelectIdentity(identityNumber)}
            onUseAnotherIdentity={() => {
              isIdentityPopoverOpen = false;
              isAuthDialogOpen = true;
            }}
            onManageIdentity={(): Promise<void> => {
              isIdentityPopoverOpen = false;
              window.open("/manage", "_blank");
              return Promise.resolve();
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
        <Dialog onClose={() => (isAuthDialogOpen = false)}>
          <AuthWizard
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
  </Header>

  <div class="flex flex-1 flex-col items-center justify-center">
    {@render children()}
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
