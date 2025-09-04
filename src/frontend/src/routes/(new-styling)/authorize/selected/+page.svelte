<script lang="ts">
  import { onMount } from "svelte";
  import { goto } from "$app/navigation";
  import { AuthFlow } from "$lib/flows/authFlow.svelte";
  import {
    authorizationStore,
    authorizationContextStore,
  } from "$lib/stores/authorization.store";
  import { lastUsedIdentitiesStore } from "$lib/stores/last-used-identities.store";
  import { canisterConfig } from "$lib/globals";
  import { nonNullish } from "@dfinity/utils";
  import { toaster } from "$lib/components/utils/toaster";
  import { handleError } from "$lib/components/utils/error";
  import { triggerDropWaveAnimation } from "$lib/utils/animation-dispatcher";
  import type { OpenIdConfig } from "$lib/generated/internet_identity_types";
  import type { PageData } from "./$types";

  interface Props {
    data: PageData;
  }

  let { data }: Props = $props();
  const { authMethod } = data;

  let error = $state<string | undefined>();

  const authFlow = new AuthFlow();

  const handlePasskeyAuth = async () => {
    try {
      const identityNumber = await authFlow.continueWithExistingPasskey();
      await handleSignIn(identityNumber);
    } catch (err) {
      handleError(err);
      error = "Failed to authenticate with passkey";
    }
  };

  const handleOpenIdAuth = async (config: OpenIdConfig) => {
    try {
      const result = await authFlow.continueWithOpenId(config);
      if (result.type === "signIn") {
        await handleSignIn(result.identityNumber);
      } else if (nonNullish(result.name)) {
        // If name is present, complete registration
        const identityNumber = await authFlow.completeOpenIdRegistration(
          result.name,
        );
        await handleSignUp(identityNumber);
      } else {
        // Name is required for OpenID registration but wasn't provided
        error = "OpenID provider did not provide a name for registration";
        setTimeout(() => goto("/authorize"), 3000);
      }
    } catch (err) {
      handleError(err);
      error = "Failed to authenticate with OpenID provider";
    }
  };

  const handleSignIn = async (identityNumber: bigint) => {
    lastUsedIdentitiesStore.selectIdentity(identityNumber);
    await authorizationStore.authorize(undefined, 4000);
  };

  const handleSignUp = async (identityNumber: bigint) => {
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

  onMount(async () => {
    // Trigger authentication immediately based on authMethod
    if (authMethod === "passkey") {
      await handlePasskeyAuth();
    } else {
      // Try to find the OpenID config by name
      const openIdConfigs = canisterConfig.openid_configs?.[0] ?? [];
      const config = openIdConfigs.find(
        (cfg) => cfg.name.toLowerCase() === authMethod.toLowerCase(),
      );

      if (config) {
        await handleOpenIdAuth(config);
      } else {
        error = `Unknown authentication method: ${authMethod}`;
        // Redirect back to main authorize page after showing error
        setTimeout(() => goto("/authorize"), 3000);
      }
    }
  });
</script>

<div class="flex min-h-screen items-center justify-center">
  <div class="text-center">
    {#if error}
      <div class="mb-4 text-red-500">
        {error}
      </div>
      <p class="text-sm text-gray-600">Redirecting...</p>
    {:else}
      <div
        class="border-primary inline-block h-8 w-8 animate-spin rounded-full border-4 border-solid border-r-transparent"
      ></div>
      <p class="mt-4 text-sm text-gray-600">
        {#if authMethod === "passkey"}
          Authenticating with passkey...
        {:else}
          Authenticating with {authMethod}...
        {/if}
      </p>
    {/if}
  </div>
</div>
