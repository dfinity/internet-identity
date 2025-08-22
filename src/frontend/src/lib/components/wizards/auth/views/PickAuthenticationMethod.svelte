<script lang="ts">
  import { nonNullish } from "@dfinity/utils";
  import Button from "$lib/components/ui/Button.svelte";
  import GoogleIcon from "$lib/components/icons/GoogleIcon.svelte";
  import PasskeyIcon from "$lib/components/icons/PasskeyIcon.svelte";
  import Alert from "$lib/components/ui/Alert.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import { canisterConfig } from "$lib/globals";
  import {
    ENABLE_GENERIC_OPEN_ID,
    ENABLE_MIGRATE_FLOW,
  } from "$lib/state/featureFlags";
  import { waitFor } from "$lib/utils/utils";
  import Tooltip from "$lib/components/ui/Tooltip.svelte";
  import type { OpenIdConfig } from "$lib/generated/internet_identity_types";

  interface Props {
    setupOrUseExistingPasskey: () => void;
    continueWithGoogle: () => Promise<void | "cancelled">;
    continueWithOpenId: (config: OpenIdConfig) => Promise<void | "cancelled">;
    migrate: () => void;
  }

  const {
    setupOrUseExistingPasskey,
    continueWithGoogle,
    continueWithOpenId,
    migrate,
  }: Props = $props();

  let isAuthenticating = $state(false);
  let isCancelled = $state(false);
  let cancelledProviderId = $state<string | null>(null);

  const handleContinueWithGoogle = async () => {
    isAuthenticating = true;
    const result = await continueWithGoogle();
    isAuthenticating = false;

    if (result === "cancelled") {
      isCancelled = true;
      await waitFor(4000);
      isCancelled = false;
    }
  };

  const handleContinueWithOpenId = async (config: OpenIdConfig) => {
    isAuthenticating = true;
    const result = await continueWithOpenId(config);
    isAuthenticating = false;

    if (result === "cancelled") {
      cancelledProviderId = config.client_id;
      await waitFor(4000);
      cancelledProviderId = null;
    }
  };

  const supportsPasskeys = nonNullish(window.PublicKeyCredential);
  const showGoogleButton =
    canisterConfig.openid_google?.[0]?.[0] && !$ENABLE_GENERIC_OPEN_ID;
  const openIdProviders = canisterConfig.openid_configs?.[0] ?? [];
</script>

<div class="flex flex-col items-stretch gap-6">
  {#if !supportsPasskeys}
    <Alert
      title="Passkeys not available here"
      description="Passkeys are unavailable on this device or browser. Please choose
        another sign-in method to continue."
    />
  {/if}
  <div class="flex flex-col items-stretch gap-3">
    {#if $ENABLE_GENERIC_OPEN_ID}
      <div class="flex flex-row flex-nowrap justify-stretch gap-3">
        {#each openIdProviders as provider}
          <Tooltip
            label="Interaction canceled. Please try again."
            hidden={cancelledProviderId !== provider.client_id}
            manual
          >
            <Button
              onclick={() => handleContinueWithOpenId(provider)}
              variant="secondary"
              disabled={isAuthenticating}
              size="xl"
              class="flex-1"
            >
              {#if isAuthenticating}
                <ProgressRing />
              {:else if provider.logo}
                {@html provider.logo}
              {/if}
            </Button>
          </Tooltip>
        {/each}
      </div>
    {/if}
    <Button
      onclick={setupOrUseExistingPasskey}
      disabled={!supportsPasskeys || isAuthenticating}
      size="xl"
      variant={$ENABLE_GENERIC_OPEN_ID ? "secondary" : "primary"}
    >
      <PasskeyIcon />
      Continue with Passkey
    </Button>
    {#if showGoogleButton}
      <Tooltip
        label="Interaction canceled. Please try again."
        hidden={!isCancelled}
        manual
      >
        <Button
          onclick={handleContinueWithGoogle}
          variant="secondary"
          disabled={isAuthenticating}
          size="xl"
        >
          {#if isAuthenticating}
            <ProgressRing />
            <span>Authenticating with Google...</span>
          {:else}
            <GoogleIcon />
            <span>Continue with Google</span>
          {/if}
        </Button>
      </Tooltip>
    {/if}
    {#if $ENABLE_MIGRATE_FLOW}
      <Button onclick={migrate} variant="tertiary" size="xl">
        Upgrade from legacy identity
      </Button>
    {/if}
  </div>
</div>
