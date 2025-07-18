<script lang="ts">
  import { nonNullish } from "@dfinity/utils";
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import { handleError } from "$lib/components/utils/error";
  import {
    lastUsedIdentitiesStore,
    type LastUsedIdentity,
  } from "$lib/stores/last-used-identities.store";
  import { toaster } from "$lib/components/utils/toaster";
  import AuthPanel from "$lib/components/layout/AuthPanel.svelte";
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import { PlusIcon, UserIcon } from "@lucide/svelte";
  import ButtonCard from "$lib/components/ui/ButtonCard.svelte";
  import Avatar from "$lib/components/ui/Avatar.svelte";
  import { AuthLastUsedFlow } from "$lib/flows/authLastUsedFlow.svelte";
  import Header from "$lib/components/layout/Header.svelte";
  import Footer from "$lib/components/layout/Footer.svelte";
  import { goto } from "$app/navigation";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import { AuthWizard } from "$lib/components/wizards/auth";

  const gotoManage = () => goto("/manage", { replaceState: true });
  const onSignIn = async (identityNumber: bigint) => {
    lastUsedIdentitiesStore.selectIdentity(identityNumber);
    await gotoManage();
    isAuthDialogOpen = false;
  };
  const onSignUp = async (identityNumber: bigint) => {
    toaster.success({
      title: "You're all set. Your identity has been created.",
      duration: 2000,
    });
    lastUsedIdentitiesStore.selectIdentity(identityNumber);
    await gotoManage();
    isAuthDialogOpen = false;
  };
  const authLastUsedFlow = new AuthLastUsedFlow();

  const lastUsedIdentities = $derived(
    Object.values($lastUsedIdentitiesStore.identities)
      .sort((a, b) => b.lastUsedTimestampMillis - a.lastUsedTimestampMillis)
      .slice(0, 3),
  );

  let isAuthDialogOpen = $state(false);
  let isAuthenticating = $state(false);

  const handleContinueAs = async (identity: LastUsedIdentity) => {
    await authLastUsedFlow.authenticate(identity).catch(handleError);
    await onSignIn(identity.identityNumber);
  };
</script>

<div class="flex min-h-[100dvh] flex-col">
  <div class="h-[env(safe-area-inset-top)]"></div>
  <Header />
  <div class="flex flex-1 flex-col items-center justify-center">
    <AuthPanel class="sm:max-w-100">
      <div class="flex-1"></div>
      {#if lastUsedIdentities.length > 0}
        <h1 class="text-text-primary my-2 self-start text-2xl font-medium">
          Manage your Internet&nbsp;Identity
        </h1>
        <p class="text-text-secondary mb-6 self-start text-sm">
          choose identity to continue
        </p>
        <div class="flex flex-col gap-1.5">
          <ul class="contents">
            {#each lastUsedIdentities as identity}
              <li class="contents">
                <ButtonCard
                  onclick={() => handleContinueAs(identity)}
                  disabled={nonNullish(authLastUsedFlow.authenticatingIdentity)}
                >
                  <Avatar size="sm">
                    {#if identity.identityNumber === authLastUsedFlow.authenticatingIdentity}
                      <ProgressRing />
                    {:else}
                      <UserIcon size="1.25rem" />
                    {/if}
                  </Avatar>
                  <div class="flex flex-col text-left text-sm">
                    <div class="font-semibold">
                      {identity.name ?? identity.identityNumber}
                    </div>
                    <div class="text-text-tertiary" aria-hidden="true">
                      {"passkey" in identity.authMethod ? "Passkey" : "Google"}
                    </div>
                  </div>
                </ButtonCard>
              </li>
            {/each}
          </ul>
          <ButtonCard
            onclick={() => (isAuthDialogOpen = true)}
            disabled={nonNullish(authLastUsedFlow.authenticatingIdentity)}
          >
            <FeaturedIcon size="sm">
              <PlusIcon size="1.25rem" />
            </FeaturedIcon>
            <span>Use another identity</span>
          </ButtonCard>
        </div>
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
              <h1
                class="text-text-primary my-2 self-start text-2xl font-medium"
              >
                Use another identity
              </h1>
              <p class="text-text-secondary mb-6 self-start text-sm">
                choose method
              </p>
            </AuthWizard>
          </Dialog>
        {/if}
      {:else}
        <AuthWizard {onSignIn} {onSignUp} onError={handleError}>
          <h1 class="text-text-primary my-2 self-start text-2xl font-medium">
            Manage your Internet&nbsp;Identity
          </h1>
          <p class="text-text-secondary mb-6 self-start text-sm">
            sign in to continue
          </p>
        </AuthWizard>
      {/if}
    </AuthPanel>
  </div>
  <Footer />
  <div class="h-[env(safe-area-inset-bottom)]"></div>
</div>
