<script lang="ts">
  import { canisterConfig } from "$lib/globals";
  import { goto } from "$app/navigation";
  import { AuthFlow } from "$lib/flows/authFlow.svelte";
  import { lastUsedIdentitiesStore } from "$lib/stores/last-used-identities.store";
  import {
    AuthenticationV2Events,
    authenticationV2Funnel,
  } from "$lib/utils/analytics/authenticationV2Funnel";
  import type { PageProps } from "./$types";
  import { nonNullish } from "@dfinity/utils";
  import { toaster } from "$lib/components/utils/toaster";
  import { isOpenIdCancelError } from "$lib/utils/openID";
  import { handleError } from "$lib/components/utils/error";
  import type { OpenIdConfig } from "$lib/generated/internet_identity_types";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";

  const { data }: PageProps = $props();

  const openIdProviders = canisterConfig.openid_configs?.[0] ?? [];

  const url = new URL(window.location.href);
  const authMethod = url.searchParams.get("authMethod");
  const provider = openIdProviders.find(
    (p) => p.name.toLowerCase() === authMethod?.toLowerCase(),
  );

  const gotoNext = () => goto(data.next ?? "/manage", { replaceState: true });

  const authFlow = new AuthFlow();

  const onSignIn = async (identityNumber: bigint) => {
    lastUsedIdentitiesStore.selectIdentity(identityNumber);
    authenticationV2Funnel.trigger(AuthenticationV2Events.GoToDashboard);
    authenticationV2Funnel.close();
    await gotoNext();
  };

  const onSignUp = async (identityNumber: bigint) => {
    toaster.success({
      title: "You're all set. Your identity has been created.",
      duration: 2000,
    });
    lastUsedIdentitiesStore.selectIdentity(identityNumber);
    authenticationV2Funnel.trigger(AuthenticationV2Events.GoToDashboard);
    authenticationV2Funnel.close();
    await gotoNext();
  };

  const handleContinueWithOpenId = async (
    config: OpenIdConfig,
  ): Promise<void | "cancelled"> => {
    try {
      const result = await authFlow.continueWithOpenId(config, true);
      if (result.type === "signIn") {
        onSignIn(result.identityNumber);
      } else if (nonNullish(result.name)) {
        onSignUp(await authFlow.completeOpenIdRegistration(result.name));
      }
    } catch (error) {
      if (isOpenIdCancelError(error)) {
        return "cancelled";
      }
      handleError(error);
    }
  };

  if (provider) {
    handleContinueWithOpenId(provider);
  } else {
    // if an incorrect provider is provided redirect the user to the login page
    goto(`/login${data.next ? `next=${data.next}` : ""}`);
  }
</script>

<div class="flex min-h-[100dvh] flex-col">
  <div class="h-[env(safe-area-inset-top)]"></div>
  <div class="flex h-dvh w-full flex-col items-center justify-center gap-2">
    {#if provider}
      <p class="text-text-primary">Redirecting you to {provider?.name}</p>
    {/if}
    <ProgressRing class="text-fg-brand-primary" />
  </div>
</div>
