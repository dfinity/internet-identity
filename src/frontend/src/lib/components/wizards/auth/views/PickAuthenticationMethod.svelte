<script lang="ts">
  import { nonNullish } from "@dfinity/utils";
  import Button from "$lib/components/ui/Button.svelte";
  import GoogleIcon from "$lib/components/icons/GoogleIcon.svelte";
  import PasskeyIcon from "$lib/components/icons/PasskeyIcon.svelte";
  import Alert from "$lib/components/ui/Alert.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import { canisterConfig } from "$lib/globals";
  import {
    AUTH_FLOW_UPDATES,
    ENABLE_GENERIC_OPEN_ID,
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

  let authenticatingProviderId = $state<string | null>(null);
  let isGoogleAuthenticating = $state(false);
  let isCancelled = $state(false);
  let cancelledProviderId = $state<string | null>(null);

  const handleContinueWithGoogle = async () => {
    isGoogleAuthenticating = true;
    const result = await continueWithGoogle();
    isGoogleAuthenticating = false;

    if (result === "cancelled") {
      isCancelled = true;
      await waitFor(4000);
      isCancelled = false;
    }
  };

  const handleContinueWithOpenId = async (config: OpenIdConfig) => {
    authenticatingProviderId = config.client_id;
    const result = await continueWithOpenId(config);
    authenticatingProviderId = null;

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
              disabled={nonNullish(authenticatingProviderId)}
              size="xl"
              class="flex-1"
              aria-label={`Continue with ${provider.name}`}
            >
              {#if authenticatingProviderId === provider.client_id}
                <ProgressRing />
              {:else if provider.logo}
                <div class="size-6">
                  {@html provider.logo}
                </div>
              {/if}
            </Button>
          </Tooltip>
        {/each}
      </div>
    {/if}
    <Button
      onclick={setupOrUseExistingPasskey}
      disabled={!supportsPasskeys ||
        nonNullish(authenticatingProviderId) ||
        isGoogleAuthenticating}
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
          disabled={isGoogleAuthenticating}
          size="xl"
        >
          {#if isGoogleAuthenticating}
            <ProgressRing />
            <span>Authenticating with Google...</span>
          {:else}
            <GoogleIcon />
            <span>Continue with Google</span>
          {/if}
        </Button>
      </Tooltip>
    {/if}
    <div class="border-border-tertiary border-t"></div>
    <div class="flex flex-row items-center justify-between gap-4">
      <p class="text-text-secondary text-sm">Still have an identity number?</p>
      <button
        onclick={migrate}
        class="text-text-primary text-sm font-semibold outline-0 hover:underline focus-visible:underline"
      >
        Upgrade
      </button>
    </div>
  </div>
</div>
