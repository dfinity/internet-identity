<script lang="ts">
  import type { LayoutProps } from "./$types";
  import { ChevronDownIcon, UserIcon } from "@lucide/svelte";
  import { lastUsedIdentitiesStore } from "$lib/stores/last-used-identities.store";
  import { authenticationStore } from "$lib/stores/authentication.store";
  import { sessionStore } from "$lib/stores/session.store";
  import { t } from "$lib/stores/locale.store";
  import { AuthLastUsedFlow } from "$lib/flows/authLastUsedFlow.svelte";
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

  const { children }: LayoutProps = $props();

  // --- Identity switcher state (mirrors /authorize layout chrome) ---
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

  $effect(() =>
    authLastUsedFlow.init(
      lastUsedIdentities.map(({ identityNumber }) => identityNumber),
    ),
  );
</script>

<div class="flex min-h-[100dvh] flex-col" data-page="cli-authorize-view">
  <div class="h-[env(safe-area-inset-top)]"></div>
  <Header>
    {#if selectedIdentity !== undefined}
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
            onSwitchIdentity={(identityNumber) => handleSignIn(identityNumber)}
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
