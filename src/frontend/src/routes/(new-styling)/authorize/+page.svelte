<script lang="ts">
  import { nonNullish } from "@dfinity/utils";
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import { handleError } from "$lib/components/utils/error";
  import PickAuthenticationMethod from "$lib/components/views/PickAuthenticationMethod.svelte";
  import { lastUsedIdentitiesStore } from "$lib/stores/last-used-identities.store";
  import { goto } from "$app/navigation";
  import {
    authorizationStore,
    authorizationContextStore,
  } from "$lib/stores/authorization.store";
  import { toaster } from "$lib/components/utils/toaster";
  import AuthorizeHeader from "$lib/components/ui/AuthorizeHeader.svelte";
  import SolveCaptcha from "$lib/components/views/SolveCaptcha.svelte";
  import SetupOrUseExistingPasskey from "$lib/components/views/SetupOrUseExistingPasskey.svelte";
  import CreatePasskey from "$lib/components/views/CreatePasskey.svelte";
  import SystemOverlayBackdrop from "$lib/components/utils/SystemOverlayBackdrop.svelte";
  import { getDapps } from "$lib/legacy/flows/dappsExplorer/dapps";
  import { AuthFlow } from "$lib/flows/authFlow.svelte";

  const authFlow = new AuthFlow({
    onSignIn: async (identityNumber) => {
      lastUsedIdentitiesStore.selectIdentity(identityNumber);
      await goto("/authorize/account");
    },
    onSignUp: async (identityNumber) => {
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
      await authorizationStore.authorize(undefined, 4000);
    },
  });

  const dapps = getDapps();
  const dapp = $derived(
    dapps.find((dapp) =>
      dapp.hasOrigin($authorizationContextStore.requestOrigin),
    ),
  );
</script>

{#if nonNullish(authFlow.captcha)}
  <SolveCaptcha {...authFlow.captcha} />
{:else}
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
  <PickAuthenticationMethod
    setupOrUseExistingPasskey={authFlow.setupOrUseExistingPasskey}
    continueWithGoogle={authFlow.continueWithGoogle}
  />
  {#if authFlow.view !== "chooseMethod"}
    <Dialog onClose={() => (authFlow.view = "chooseMethod")}>
      {#if authFlow.view === "setupOrUseExistingPasskey"}
        <SetupOrUseExistingPasskey
          setupNew={authFlow.setupNewPasskey}
          useExisting={authFlow.continueWithExistingPasskey}
        />
      {:else if authFlow.view === "setupNewPasskey"}
        <CreatePasskey create={authFlow.createPasskey} />
      {/if}
    </Dialog>
  {/if}
{/if}
{#if authFlow.systemOverlay}
  <SystemOverlayBackdrop />
{/if}
