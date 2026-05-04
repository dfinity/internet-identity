<script lang="ts">
  import PasskeyIcon from "$lib/components/icons/PasskeyIcon.svelte";
  import SsoIcon from "$lib/components/icons/SsoIcon.svelte";
  import Alert from "$lib/components/ui/Alert.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import { backendCanisterConfig } from "$lib/globals";
  import { waitFor } from "$lib/utils/utils";
  import Tooltip from "$lib/components/ui/Tooltip.svelte";
  import type { OpenIdConfig } from "$lib/generated/internet_identity_types";
  import { t } from "$lib/stores/locale.store";

  interface Props {
    setupOrUseExistingPasskey: () => void;
    continueWithOpenId: (config: OpenIdConfig) => Promise<void | "cancelled">;
    signInWithSso: () => void;
  }

  const {
    setupOrUseExistingPasskey,
    continueWithOpenId,
    signInWithSso,
  }: Props = $props();

  let authenticatingProviderId = $state<string>();
  let cancelledProviderId = $state<string>();

  const handleContinueWithOpenId = async (config: OpenIdConfig) => {
    authenticatingProviderId = config.client_id;
    const result = await continueWithOpenId(config);
    authenticatingProviderId = undefined;

    if (result === "cancelled") {
      cancelledProviderId = config.client_id;
      await waitFor(4000);
      cancelledProviderId = undefined;
    }
  };

  const supportsPasskeys = window.PublicKeyCredential !== undefined;
  const openIdProviders = backendCanisterConfig.openid_configs?.[0] ?? [];
</script>

<div class="flex flex-col items-stretch gap-5">
  {#if !supportsPasskeys}
    <Alert
      title={$t`Passkeys not available here`}
      description={$t`Passkeys are unavailable on this device or browser. Please choose another sign-in method to continue.`}
    />
  {/if}
  <div class="flex flex-col items-stretch gap-3">
    <button
      class="btn btn-secondary btn-xl"
      onclick={setupOrUseExistingPasskey}
      disabled={!supportsPasskeys || authenticatingProviderId !== undefined}
    >
      <PasskeyIcon />
      {$t`Continue with passkey`}
    </button>
    <div class="flex flex-row flex-nowrap justify-stretch gap-3">
      {#each openIdProviders as provider (provider.issuer)}
        {@const name = provider.name}
        <Tooltip
          label={$t`Interaction canceled. Please try again.`}
          hidden={cancelledProviderId !== provider.client_id}
          manual
        >
          <button
            class="btn btn-secondary btn-xl flex-1"
            onclick={() => handleContinueWithOpenId(provider)}
            disabled={authenticatingProviderId !== undefined}
            aria-label={$t`Continue with ${name}`}
          >
            {#if authenticatingProviderId === provider.client_id}
              <ProgressRing />
            {:else}
              <div class="size-6">
                <!-- eslint-disable-next-line svelte/no-at-html-tags -- provider.logo is a trusted SVG string sourced from the backend canister's openid_configs -->
                {@html provider.logo}
              </div>
            {/if}
          </button>
        </Tooltip>
      {/each}
      <!--
        SSO entry is always rendered. Registration is enforced on the
        backend by the `sso_discoverable_domains` allowlist (init arg,
        falling back to the `is_production`-keyed defaults in
        `allowed_discovery_domains`) checked inside
        `add_discoverable_oidc_config`. Unregistered domains surface as
        an error inside the SignInWithSso screen rather than being gated
        here — we keep this option visible so users know the mechanism
        exists.
      -->
      <button
        class="btn btn-secondary btn-xl flex-1"
        onclick={signInWithSso}
        disabled={authenticatingProviderId !== undefined}
        aria-label={$t`Continue with SSO`}
      >
        <SsoIcon class="size-6" />
      </button>
    </div>
  </div>
  <div class="border-border-tertiary border-t"></div>
  <div class="flex flex-row items-center justify-between gap-4">
    <p class="text-text-secondary text-sm">
      {$t`Lost access to your identity?`}
    </p>
    <a
      href="/recovery"
      target="_blank"
      class="text-text-primary text-sm font-semibold outline-0 hover:underline focus-visible:underline"
    >
      {$t`Recover`}
    </a>
  </div>
</div>
