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
  import { ArrowRightIcon } from "@lucide/svelte";

  interface Props {
    setupOrUseExistingPasskey: () => void;
    continueWithOpenId: (config: OpenIdConfig) => Promise<void | "cancelled">;
    signInWithSso: () => void;
    mode?: "signin" | "signup" | "both";
    onSwitchMode?: () => void;
    withinDialog?: boolean;
  }

  const {
    setupOrUseExistingPasskey,
    continueWithOpenId,
    signInWithSso,
    mode = "both",
    onSwitchMode,
    withinDialog = false,
  }: Props = $props();

  const showLostAccess = $derived(mode !== "signup");
  const showSwitchMode = $derived(
    onSwitchMode !== undefined && (mode === "signin" || mode === "signup"),
  );

  const passkeyLabel = $derived(
    mode === "signin"
      ? $t`Sign in with passkey`
      : mode === "signup"
        ? $t`Sign up with passkey`
        : $t`Continue with passkey`,
  );
  const providerLabel = (name: string) =>
    mode === "signin"
      ? $t`Sign in with ${name}`
      : mode === "signup"
        ? $t`Sign up with ${name}`
        : $t`Continue with ${name}`;
  const ssoLabel = $derived(
    mode === "signin"
      ? $t`Sign in with SSO`
      : mode === "signup"
        ? $t`Sign up with SSO`
        : $t`Continue with SSO`,
  );

  let authenticatingProviderId = $state<string>();
  let cancelledProviderId = $state<string>();

  const handleContinueWithOpenId = async (config: OpenIdConfig) => {
    authenticatingProviderId = config.issuer;
    const result = await continueWithOpenId(config);
    authenticatingProviderId = undefined;

    if (result === "cancelled") {
      cancelledProviderId = config.issuer;
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
      class="btn btn-primary btn-xl h-14"
      onclick={setupOrUseExistingPasskey}
      disabled={!supportsPasskeys || authenticatingProviderId !== undefined}
    >
      <PasskeyIcon />
      {passkeyLabel}
    </button>
    <div class="flex flex-row flex-nowrap justify-stretch gap-3">
      {#each openIdProviders as provider (provider.issuer)}
        {@const name = provider.name}
        <Tooltip
          label={$t`Interaction canceled. Please try again.`}
          hidden={cancelledProviderId !== provider.issuer}
          manual
        >
          <button
            class="border-border-secondary text-fg-primary bg-bg-primary hover:not-disabled:bg-bg-primary_hover disabled:border-border-disabled disabled:text-fg-disabled focus-visible:ring-focus-ring focus-visible:ring-offset-bg-primary flex h-16 w-full flex-col items-center justify-center gap-1.5 rounded-md border text-xs font-semibold outline-none not-disabled:cursor-pointer focus-visible:ring-2 focus-visible:ring-offset-2"
            onclick={() => handleContinueWithOpenId(provider)}
            disabled={authenticatingProviderId !== undefined}
            aria-label={providerLabel(name)}
          >
            {#if authenticatingProviderId === provider.issuer}
              <ProgressRing />
            {:else}
              <div class="size-5">
                <!-- eslint-disable-next-line svelte/no-at-html-tags -- provider.logo is a trusted SVG string sourced from the backend canister's openid_configs -->
                {@html provider.logo}
              </div>
              <span>{name}</span>
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
        class="border-border-secondary text-fg-primary bg-bg-primary hover:not-disabled:bg-bg-primary_hover disabled:border-border-disabled disabled:text-fg-disabled focus-visible:ring-focus-ring focus-visible:ring-offset-bg-primary flex h-16 w-full flex-col items-center justify-center gap-1.5 rounded-md border text-xs font-semibold outline-none not-disabled:cursor-pointer focus-visible:ring-2 focus-visible:ring-offset-2"
        onclick={signInWithSso}
        disabled={authenticatingProviderId !== undefined}
        aria-label={ssoLabel}
      >
        <SsoIcon class="size-5" />
        <span>{$t`SSO`}</span>
      </button>
    </div>
  </div>
  {#if showLostAccess}
    <div
      class={[
        "flex flex-row items-center justify-between gap-4",
        !withinDialog && "border-border-secondary border-b pb-3",
      ]}
    >
      <p class="text-text-tertiary text-sm">
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
  {/if}
  {#if showSwitchMode}
    <div
      class={[
        "flex flex-row items-center gap-3",
        withinDialog
          ? "bg-bg-secondary dark:bg-bg-primary border-border-secondary -mx-4 -mb-4 border-t px-4 py-4 sm:-mx-6 sm:-mb-8 sm:px-6"
          : "py-2",
      ]}
    >
      <div class="min-w-0 flex-1">
        <div class="text-text-primary text-sm font-semibold">
          {mode === "signin"
            ? $t`New to Internet Identity?`
            : $t`Already have an identity?`}
        </div>
        <div class="text-text-tertiary mt-0.5 text-xs">
          {mode === "signin"
            ? $t`Create your private, passwordless identity.`
            : $t`Sign in with a passkey or familiar provider.`}
        </div>
      </div>
      <button
        onclick={onSwitchMode}
        disabled={authenticatingProviderId !== undefined}
        class="btn btn-secondary btn-sm shrink-0 gap-2"
      >
        {mode === "signin" ? $t`Sign up` : $t`Sign in`}
        <ArrowRightIcon class="size-4 rtl:-scale-x-100" />
      </button>
    </div>
  {/if}
</div>
