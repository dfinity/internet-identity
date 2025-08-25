<script lang="ts">
  import ShieldIllustration from "$lib/components/illustrations/ShieldIllustration.svelte";
  import PasskeyIcon from "$lib/components/icons/PasskeyIcon.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import Alert from "$lib/components/ui/Alert.svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import GoogleIcon from "$lib/components/icons/GoogleIcon.svelte";
  import { nonNullish } from "@dfinity/utils";
  import { canisterConfig } from "$lib/globals";
  import { ENABLE_GENERIC_OPEN_ID } from "$lib/state/featureFlags";
  import type { OpenIdConfig } from "$lib/generated/internet_identity_types";

  interface Props {
    linkGoogleAccount: () => Promise<void>;
    linkOpenIdAccount: (config: OpenIdConfig) => Promise<void>;
    continueWithPasskey: () => void;
  }

  const { linkGoogleAccount, linkOpenIdAccount, continueWithPasskey }: Props =
    $props();

  let authenticatingGoogle = $state(false);
  let authenticatingProviderId = $state<Record<string, boolean>>({});
  let authenticating = $derived(
    Object.values(authenticatingProviderId).some(Boolean) ||
      authenticatingGoogle,
  );

  const isPasskeySupported = nonNullish(window.PublicKeyCredential);

  const handleContinueWithGoogle = async () => {
    authenticatingGoogle = true;
    try {
      await linkGoogleAccount();
    } finally {
      authenticatingGoogle = false;
    }
  };

  const handleContinueWithOpenId = async (config: OpenIdConfig) => {
    authenticatingProviderId[config.client_id] = true;
    try {
      await linkOpenIdAccount(config);
    } finally {
      authenticatingProviderId[config.client_id] = false;
    }
  };

  const showGoogleButton =
    canisterConfig.openid_google?.[0]?.[0] && !$ENABLE_GENERIC_OPEN_ID;
  const openIdProviders = canisterConfig.openid_configs?.[0] ?? [];
</script>

<div class="mt-4 mb-6 flex flex-col">
  <div class={["self-center", !isPasskeySupported && "illustration"]}>
    <ShieldIllustration class="text-text-primary mb-8 h-24" />
  </div>
  <h1 class="text-text-primary mb-3 text-2xl font-medium sm:text-center">
    Add access method
  </h1>
  <p class="text-md text-text-tertiary font-medium text-balance sm:text-center">
    Add another way to sign in with a passkey or third-party account for secure
    access.
  </p>
</div>
<div class="flex flex-col items-stretch gap-6">
  {#if !isPasskeySupported}
    <Alert
      title="Passkeys not available here"
      description="Passkeys are unavailable on this device or browser. Please choose
        another access method to continue."
      direction="horizontal"
    />
  {/if}
  <div class="flex flex-col items-stretch gap-3">
    <Button
      onclick={continueWithPasskey}
      disabled={!isPasskeySupported || authenticating}
      size="xl"
    >
      <PasskeyIcon />
      Continue with Passkey
    </Button>
    {#if $ENABLE_GENERIC_OPEN_ID}
      <div class="flex flex-row flex-nowrap justify-stretch gap-3">
        {#each openIdProviders as provider}
          <Button
            onclick={() => handleContinueWithOpenId(provider)}
            variant="secondary"
            disabled={authenticating}
            size="xl"
            class="flex-1"
          >
            {#if authenticatingProviderId[provider.client_id]}
              <ProgressRing />
            {:else if provider.logo}
              <div class="size-6">
                {@html provider.logo}
              </div>
            {/if}
          </Button>
        {/each}
      </div>
    {:else if showGoogleButton}
      <Button
        onclick={handleContinueWithGoogle}
        variant="secondary"
        disabled={authenticating}
        size="xl"
      >
        {#if authenticatingGoogle}
          <ProgressRing />
          <span>Authenticating with Google...</span>
        {:else}
          <GoogleIcon />
          <span>Continue with Google</span>
        {/if}
      </Button>
    {/if}
  </div>
</div>

<style>
  @media (max-height: 700px) {
    /*noinspection CssUnusedSymbol*/
    .illustration {
      display: none !important;
    }
  }
</style>
