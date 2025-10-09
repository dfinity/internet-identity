<script lang="ts">
  import ShieldIllustration from "$lib/components/illustrations/ShieldIllustration.svelte";
  import PasskeyIcon from "$lib/components/icons/PasskeyIcon.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import Alert from "$lib/components/ui/Alert.svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import Tooltip from "$lib/components/ui/Tooltip.svelte";
  import { issuerMatches } from "$lib/utils/openID";
  import { nonNullish } from "@dfinity/utils";
  import { canisterConfig } from "$lib/globals";
  import type {
    OpenIdConfig,
    OpenIdCredential,
  } from "$lib/generated/internet_identity_types";

  interface Props {
    linkOpenIdAccount: (config: OpenIdConfig) => Promise<void>;
    continueWithPasskey: () => void;
    openIdCredentials?: OpenIdCredential[];
    maxPasskeysReached?: boolean;
  }

  const {
    linkOpenIdAccount,
    continueWithPasskey,
    openIdCredentials = [],
    maxPasskeysReached,
  }: Props = $props();

  let authenticatingGoogle = $state(false);
  let authenticatingProviderId = $state<string | null>();
  let authenticating = $derived(
    nonNullish(authenticatingProviderId) || authenticatingGoogle,
  );

  const isPasskeySupported = nonNullish(window.PublicKeyCredential);

  const handleContinueWithOpenId = async (config: OpenIdConfig) => {
    authenticatingProviderId = config.client_id;
    try {
      await linkOpenIdAccount(config);
    } finally {
      authenticatingProviderId = null;
    }
  };

  const openIdProviders = canisterConfig.openid_configs?.[0] ?? [];

  const hasCredential = (configIssuer: string): boolean =>
    openIdCredentials.some((cred) =>
      issuerMatches(configIssuer, cred.iss, cred.metadata),
    );
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
    <Tooltip
      label="You have reached the maximum number of passkeys."
      hidden={!maxPasskeysReached}
    >
      <Button
        onclick={continueWithPasskey}
        disabled={!isPasskeySupported || authenticating || maxPasskeysReached}
        size="xl"
      >
        <PasskeyIcon />
        Continue with Passkey
      </Button>
    </Tooltip>
    <div class="flex flex-row flex-nowrap justify-stretch gap-3">
      {#each openIdProviders as provider}
        <Tooltip
          label={`You already have a ${provider.name} account linked`}
          hidden={!hasCredential(provider.issuer)}
        >
          <Button
            onclick={() => handleContinueWithOpenId(provider)}
            variant="secondary"
            disabled={authenticating || hasCredential(provider.issuer)}
            size="xl"
            class="flex-1"
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
