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
  import { waitFor } from "$lib/utils/utils";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";

  interface Props {
    linkOpenIdAccount: (config: OpenIdConfig) => Promise<"cancelled" | void>;
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

  let authenticatingProviderId = $state<string | null>();
  let cancelledProviderId = $state<string | null>(null);

  const isPasskeySupported = nonNullish(window.PublicKeyCredential);

  const handleContinueWithOpenId = async (config: OpenIdConfig) => {
    authenticatingProviderId = config.client_id;
    const result = await linkOpenIdAccount(config);
    authenticatingProviderId = null;

    if (result === "cancelled") {
      cancelledProviderId = config.client_id;
      await waitFor(4000);
      cancelledProviderId = null;
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
    {$t`Add access method`}
  </h1>
  <p
    class="text-text-tertiary text-base font-medium text-balance sm:text-center"
  >
    <Trans>
      Add another way to sign in with a passkey or third-party account for
      secure access.
    </Trans>
  </p>
</div>
<div class="flex flex-col items-stretch gap-6">
  {#if !isPasskeySupported}
    <Alert
      title={$t`Passkeys not available here`}
      description={$t`Passkeys are unavailable on this device or browser. Please choose another access method to continue.`}
      direction="horizontal"
    />
  {/if}
  <div class="flex flex-col items-stretch gap-3">
    <div class="flex flex-row flex-nowrap justify-stretch gap-3">
      {#each openIdProviders as provider}
        {@const name = provider.name}
        <Tooltip
          label={$t`Interaction canceled. Please try again.`}
          hidden={cancelledProviderId !== provider.client_id}
          manual
        >
          <Tooltip
            label={$t`You already have a ${name} account linked`}
            hidden={!hasCredential(provider.issuer)}
          >
            <Button
              onclick={() => handleContinueWithOpenId(provider)}
              variant="secondary"
              disabled={nonNullish(authenticatingProviderId) ||
                hasCredential(provider.issuer)}
              size="xl"
              class="flex-1"
              aria-label={$t`Continue with ${name}`}
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
        </Tooltip>
      {/each}
    </div>
    <Tooltip
      label={$t`You have reached the maximum number of passkeys`}
      hidden={!maxPasskeysReached}
    >
      <Button
        onclick={continueWithPasskey}
        variant="secondary"
        disabled={!isPasskeySupported ||
          nonNullish(authenticatingProviderId) ||
          maxPasskeysReached}
        size="xl"
      >
        <PasskeyIcon />
        <span>{$t`Continue with passkey`}</span>
      </Button>
    </Tooltip>
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
