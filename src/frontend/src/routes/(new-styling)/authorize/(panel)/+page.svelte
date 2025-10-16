<script lang="ts">
  import { nonNullish } from "@dfinity/utils";
  import { lastUsedIdentitiesStore } from "$lib/stores/last-used-identities.store";
  import { goto, preloadCode, preloadData } from "$app/navigation";
  import {
    authorizationStore,
    authorizationContextStore,
  } from "$lib/stores/authorization.store";
  import { toaster } from "$lib/components/utils/toaster";
  import AuthorizeHeader from "$lib/components/ui/AuthorizeHeader.svelte";
  import { getDapps } from "$lib/legacy/flows/dappsExplorer/dapps";
  import { handleError } from "$lib/components/utils/error";
  import { AuthWizard } from "$lib/components/wizards/auth";
  import { triggerDropWaveAnimation } from "$lib/utils/animation-dispatcher";

  const dapps = getDapps();
  const dapp = $derived(
    dapps.find((dapp) =>
      dapp.hasOrigin($authorizationContextStore.requestOrigin),
    ),
  );

  const onSignIn = async (identityNumber: bigint) => {
    lastUsedIdentitiesStore.selectIdentity(identityNumber);
    setTimeout(async () => {
      await goto("/authorize/account");
    }, 700);
  };
  const onSignUp = async (identityNumber: bigint) => {
    toaster.success({
      title: "You're all set. Your identity has been created.",
      duration: 4000,
      closable: false,
    });
    lastUsedIdentitiesStore.selectIdentity(identityNumber);
    lastUsedIdentitiesStore.addLastUsedAccount({
      origin: $authorizationContextStore.effectiveOrigin,
      identityNumber,
      accountNumber: undefined,
    });
    triggerDropWaveAnimation();
    await authorizationStore.authorize(undefined, 4000);
  };
  const onMigration = async () => {
    await goto("/authorize/upgrade-success");
  };
</script>

<AuthWizard {onSignIn} {onSignUp} {onMigration} onError={handleError}>
  <AuthorizeHeader origin={$authorizationContextStore.requestOrigin} />
  <h1 class="text-text-primary mb-2 self-start text-2xl font-medium">
    Choose method
  </h1>
  <p class="text-text-secondary mb-6 self-start text-sm">
    <span>to continue with</span>
    {#if nonNullish(dapp?.name)}
      <span><b>{dapp.name}</b></span>
    {:else}
      <span>this app</span>
    {/if}
  </p>
</AuthWizard>
