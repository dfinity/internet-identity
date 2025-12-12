<script lang="ts">
  import { nonNullish } from "@dfinity/utils";
  import Button from "$lib/components/ui/Button.svelte";
  import PasskeyIcon from "$lib/components/icons/PasskeyIcon.svelte";
  import Alert from "$lib/components/ui/Alert.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import { canisterConfig } from "$lib/globals";
  import { waitFor } from "$lib/utils/utils";
  import Tooltip from "$lib/components/ui/Tooltip.svelte";
  import type { OpenIdConfig } from "$lib/generated/internet_identity_types";
  import { LARGE_GOOGLE_BUTTON } from "$lib/state/featureFlags";
  import { t } from "$lib/stores/locale.store";

  interface Props {
    setupOrUseExistingPasskey: () => void;
    continueWithOpenId: (config: OpenIdConfig) => Promise<void | "cancelled">;
  }

  const { setupOrUseExistingPasskey, continueWithOpenId }: Props = $props();

  let authenticatingProviderId = $state<string | null>(null);
  let cancelledProviderId = $state<string | null>(null);

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
  const openIdProviders = canisterConfig.openid_configs?.[0] ?? [];
</script>

<div class="flex flex-col items-stretch gap-5">
  {#if !supportsPasskeys}
    <Alert
      title={$t`Passkeys not available here`}
      description={$t`Passkeys are unavailable on this device or browser. Please choose another sign-in method to continue.`}
    />
  {/if}
  {#if $LARGE_GOOGLE_BUTTON}
    <div class="mb-3 flex flex-col items-stretch gap-3">
      {#each openIdProviders.slice(0, 1) as provider}
        {@const name = provider.name}
        <Tooltip
          label={$t`Interaction canceled. Please try again.`}
          hidden={cancelledProviderId !== provider.client_id}
          manual
        >
          <Button
            onclick={() => handleContinueWithOpenId(provider)}
            variant="secondary"
            disabled={nonNullish(authenticatingProviderId)}
            size="xl"
          >
            {#if authenticatingProviderId === provider.client_id}
              <ProgressRing />
              <span>{$t`Authenticating...`}</span>
            {:else}
              <div class="size-6">
                {@html provider.logo}
              </div>
              <span>{$t`Continue with ${name}`}</span>
            {/if}
          </Button>
        </Tooltip>
      {/each}
      <div class="flex flex-row flex-nowrap justify-stretch gap-3">
        {#each openIdProviders.slice(1) as provider}
          {@const name = provider.name}
          <Tooltip
            label={$t`Interaction canceled. Please try again.`}
            hidden={cancelledProviderId !== provider.client_id}
            manual
          >
            <Button
              onclick={() => handleContinueWithOpenId(provider)}
              variant="secondary"
              disabled={nonNullish(authenticatingProviderId)}
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
      </div>
      <div class="flex flex-row items-center gap-2" aria-hidden="true">
        <div
          class="border-border-tertiary flex-1 translate-y-[100%] border-t"
        ></div>
        <div class="text-text-secondary text-sm">{$t`or`}</div>
        <div
          class="border-border-tertiary flex-1 translate-y-[100%] border-t"
        ></div>
      </div>
      <Button
        onclick={setupOrUseExistingPasskey}
        disabled={!supportsPasskeys || nonNullish(authenticatingProviderId)}
        size="xl"
        variant={"secondary"}
      >
        <PasskeyIcon />
        {$t`Continue with passkey`}
      </Button>
    </div>
  {:else}
    <div class="flex flex-col items-stretch gap-3">
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
              disabled={nonNullish(authenticatingProviderId)}
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
      </div>
      <Button
        onclick={setupOrUseExistingPasskey}
        disabled={!supportsPasskeys || nonNullish(authenticatingProviderId)}
        size="xl"
        variant={"secondary"}
      >
        <PasskeyIcon />
        {$t`Continue with passkey`}
      </Button>
    </div>
    <div class="border-border-tertiary border-t"></div>
  {/if}
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
