<script lang="ts">
  import Button from "$lib/components/ui/Button.svelte";
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
    <Button
      onclick={setupOrUseExistingPasskey}
      disabled={!supportsPasskeys || authenticatingProviderId !== undefined}
      size="xl"
      variant={"secondary"}
    >
      <PasskeyIcon />
      {$t`Continue with passkey`}
    </Button>
    <div class="flex flex-row flex-nowrap justify-stretch gap-3">
      {#each openIdProviders as provider}
        {@const name = provider.name}
        <Tooltip
          label={$t`Interaction canceled. Please try again.`}
          hidden={cancelledProviderId !== provider.client_id}
          manual
        >
          <Button
            onclick={() => handleContinueWithOpenId(provider)}
            variant="secondary"
            disabled={authenticatingProviderId !== undefined}
            size="xl"
            class="flex-1"
            aria-label={$t`Continue with ${name}`}
          >
            {#if authenticatingProviderId === provider.client_id}
              <ProgressRing />
            {:else}
              <div class="size-6">
                {@html provider.logo}
              </div>
            {/if}
          </Button>
        </Tooltip>
      {/each}
      <!--
        SSO entry is always rendered. Registration is enforced on the
        backend (via the `ALLOWED_DISCOVERY_DOMAINS` canary allowlist on
        `add_discoverable_oidc_config`), so unregistered domains surface as
        an error inside the SignInWithSso screen rather than being gated
        here — we keep this option visible so users know the mechanism
        exists.
      -->
      <Button
        onclick={signInWithSso}
        variant="secondary"
        disabled={authenticatingProviderId !== undefined}
        size="xl"
        class="flex-1"
        aria-label={$t`Sign in with SSO`}
      >
        <SsoIcon class="size-6" />
      </Button>
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
