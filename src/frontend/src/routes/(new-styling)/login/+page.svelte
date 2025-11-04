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
  import { goto, preloadData } from "$app/navigation";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import { AuthWizard } from "$lib/components/wizards/auth";
  import type { PageProps } from "./$types";
  import { onMount } from "svelte";
  import {
    AuthenticationV2Events,
    authenticationV2Funnel,
  } from "$lib/utils/analytics/authenticationV2Funnel";
  import { lastUsedIdentityTypeName } from "$lib/utils/lastUsedIdentity";
  import { LARGE_GOOGLE_BUTTON } from "$lib/state/featureFlags";
  import { t } from "$lib/stores/locale.store";

  const { data }: PageProps = $props();

  let redirectingIdentity = $state<bigint>();

  // Show loading indicator until load function of next page has been preloaded
  const gotoNext = async (identityNumber: bigint) => {
    redirectingIdentity = identityNumber;
    const next = data.next ?? "/manage";
    await preloadData(next);
    await goto(next, { replaceState: true });
  };
  const onMigration = async (identityNumber: bigint) => {
    lastUsedIdentitiesStore.selectIdentity(identityNumber);
    toaster.success({
      title: $t`Migration completed successfully`,
      duration: 4000,
    });
    isAuthDialogOpen = false;
    await gotoNext(identityNumber);
  };
  const onSignIn = async (identityNumber: bigint) => {
    lastUsedIdentitiesStore.selectIdentity(identityNumber);
    isAuthDialogOpen = false;
    authenticationV2Funnel.trigger(AuthenticationV2Events.GoToDashboard);
    authenticationV2Funnel.close();
    await gotoNext(identityNumber);
  };
  const onSignUp = async (identityNumber: bigint) => {
    toaster.success({
      title: $t`You're all set. Your identity has been created.`,
      duration: 2000,
    });
    lastUsedIdentitiesStore.selectIdentity(identityNumber);
    isAuthDialogOpen = false;
    authenticationV2Funnel.trigger(AuthenticationV2Events.GoToDashboard);
    authenticationV2Funnel.close();
    await gotoNext(identityNumber);
  };

  const lastUsedIdentities = $derived(
    Object.values($lastUsedIdentitiesStore.identities)
      .sort((a, b) => b.lastUsedTimestampMillis - a.lastUsedTimestampMillis)
      .slice(0, 3),
  );
  const authLastUsedFlow = new AuthLastUsedFlow();
  // Initialize the flow every time the last used identities change
  $effect(() =>
    authLastUsedFlow.init(
      lastUsedIdentities.map(({ identityNumber }) => identityNumber),
    ),
  );

  let isAuthDialogOpen = $state(false);
  let isAuthenticating = $state(false);

  const handleContinueAs = async (identity: LastUsedIdentity) => {
    await authLastUsedFlow.authenticate(identity).catch(handleError);
    await gotoNext(identity.identityNumber);
  };

  onMount(() => {
    authenticationV2Funnel.init({
      origin: window.location.origin,
      abTestGroup: $LARGE_GOOGLE_BUTTON
        ? "largeGoogleButton"
        : "smallGoogleButton",
    });
  });
</script>

<div class="flex min-h-[100dvh] flex-col">
  <div class="h-[env(safe-area-inset-top)]"></div>

  <Header />
  <div class="flex flex-1 flex-col items-center justify-center">
    <AuthPanel class="sm:max-w-100">
      <div class="flex-1"></div>
      {#if lastUsedIdentities.length > 0}
        <h1 class="text-text-primary my-2 self-start text-2xl font-medium">
          {$t`Manage your Internet Identity`}
        </h1>
        <p class="text-text-secondary mb-6 self-start text-sm">
          {$t`choose identity to continue`}
        </p>
        <div class="flex flex-col gap-1.5">
          <ul class="contents">
            {#each lastUsedIdentities as identity}
              <li class="contents">
                <ButtonCard
                  onclick={() => handleContinueAs(identity)}
                  disabled={nonNullish(
                    authLastUsedFlow.authenticatingIdentity,
                  ) || nonNullish(redirectingIdentity)}
                >
                  <Avatar size="sm">
                    {#if identity.identityNumber === authLastUsedFlow.authenticatingIdentity || identity.identityNumber === redirectingIdentity}
                      <ProgressRing />
                    {:else}
                      <UserIcon class="size-5" />
                    {/if}
                  </Avatar>
                  <div class="flex flex-col text-left text-sm">
                    <div class="font-semibold">
                      {identity.name ?? identity.identityNumber}
                    </div>
                    <div class="text-text-tertiary" aria-hidden="true">
                      {lastUsedIdentityTypeName(identity)}
                    </div>
                  </div>
                </ButtonCard>
              </li>
            {/each}
          </ul>
          <ButtonCard
            onclick={() => (isAuthDialogOpen = true)}
            disabled={nonNullish(authLastUsedFlow.authenticatingIdentity) ||
              nonNullish(redirectingIdentity)}
          >
            <FeaturedIcon size="sm">
              <PlusIcon class="size-5" />
            </FeaturedIcon>
            <span>{$t`Use another identity`}</span>
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
              {onMigration}
              onError={(error) => {
                isAuthDialogOpen = false;
                handleError(error);
              }}
              withinDialog
            >
              <h1
                class="text-text-primary my-2 self-start text-2xl font-medium"
              >
                {$t`Use another identity`}
              </h1>
              <p class="text-text-secondary mb-6 self-start text-sm">
                {$t`choose method`}
              </p>
            </AuthWizard>
          </Dialog>
        {/if}
      {:else}
        <AuthWizard {onSignIn} {onSignUp} {onMigration} onError={handleError}>
          <h1 class="text-text-primary my-2 self-start text-2xl font-medium">
            {$t`Manage your Internet Identity`}
          </h1>
          <p class="text-text-secondary mb-6 self-start text-sm">
            {$t`sign in to continue`}
          </p>
        </AuthWizard>
      {/if}
    </AuthPanel>
  </div>
  <Footer />
  <div class="h-[env(safe-area-inset-bottom)]"></div>
</div>
