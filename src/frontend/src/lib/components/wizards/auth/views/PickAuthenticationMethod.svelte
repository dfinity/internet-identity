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
    // The signin variant in AuthWizard returns a promise that resolves
    // to `"cancelled"` on WebAuthn cancel; the signup / both variants
    // are synchronous view transitions. We don't await the result here
    // (Svelte onclick fires it as a handler), but the type has to
    // accommodate both shapes so callers don't trip the prop checker.
    setupOrUseExistingPasskey: () => void | Promise<void | "cancelled">;
    continueWithOpenId: (config: OpenIdConfig) => Promise<void | "cancelled">;
    signInWithSso: () => void;
    continueOnAnotherDevice?: () => void;
    mode?: "signin" | "signup" | "both";
    onSwitchMode?: () => void;
    withinDialog?: boolean;
    // Override the switch-mode CTA title (e.g. "Create a new identity"
    // instead of the default "Want a new identity?" pull).
    switchModeTitle?: string;
    // Override the switch-mode CTA button label (e.g. "Create" instead
    // of the default "Sign up" / "Sign in").
    switchModeAction?: string;
    // Override the primary passkey-button label. The add-identity
    // "Add existing identity" (signin) surfaces set this to "Select a
    // passkey" instead of the default mode-derived "Sign in with passkey".
    // Sign-up surfaces keep the default ("Create with passkey").
    passkeyLabel?: string;
  }

  const {
    setupOrUseExistingPasskey,
    continueWithOpenId,
    signInWithSso,
    continueOnAnotherDevice,
    mode = "both",
    onSwitchMode,
    withinDialog = false,
    switchModeTitle,
    switchModeAction,
    passkeyLabel: passkeyLabelOverride,
  }: Props = $props();

  const showLostAccess = $derived(mode !== "signup");
  const showSwitchMode = $derived(
    onSwitchMode !== undefined && (mode === "signin" || mode === "signup"),
  );

  const passkeyLabel = $derived(
    passkeyLabelOverride ??
      (mode === "signin"
        ? $t`Sign in with passkey`
        : mode === "signup"
          ? $t`Create with passkey`
          : $t`Continue with passkey`),
  );
  const providerLabel = (name: string) =>
    mode === "signin"
      ? $t`Sign in with ${name}`
      : mode === "signup"
        ? $t`Create with ${name}`
        : $t`Continue with ${name}`;
  const ssoLabel = $derived(
    mode === "signin"
      ? $t`Sign in with SSO`
      : mode === "signup"
        ? $t`Create with SSO`
        : $t`Continue with SSO`,
  );

  // Keyed by `${issuer}:${client_id}` — two configs can share an issuer with
  // different client_ids (e.g. client_id rotation, distinct audiences); using
  // the composite avoids conflating the loader/tooltip across them.
  const providerKey = (config: OpenIdConfig) =>
    `${config.issuer}:${config.client_id}`;

  let authenticatingProviderId = $state<string>();
  let cancelledProviderId = $state<string>();

  const handleContinueWithOpenId = async (config: OpenIdConfig) => {
    const key = providerKey(config);
    authenticatingProviderId = key;
    const result = await continueWithOpenId(config);
    authenticatingProviderId = undefined;

    if (result === "cancelled") {
      cancelledProviderId = key;
      await waitFor(4000);
      cancelledProviderId = undefined;
    }
  };

  const supportsPasskeys = window.PublicKeyCredential !== undefined;
  const openIdProviders = backendCanisterConfig.openid_configs?.[0] ?? [];
</script>

<div class="flex flex-col items-stretch gap-7">
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
      {#each openIdProviders as provider (providerKey(provider))}
        {@const name = provider.name}
        {@const key = providerKey(provider)}
        <Tooltip
          label={$t`Interaction canceled. Please try again.`}
          hidden={cancelledProviderId !== key}
          manual
        >
          <button
            class="btn btn-secondary h-16 w-full flex-col gap-1.5 text-xs whitespace-normal"
            onclick={() => handleContinueWithOpenId(provider)}
            disabled={authenticatingProviderId !== undefined}
            aria-label={providerLabel(name)}
          >
            {#if authenticatingProviderId === key}
              <ProgressRing />
            {:else}
              {#if provider.logo}
                <div class="size-5">
                  <!-- eslint-disable-next-line svelte/no-at-html-tags -- provider.logo is a trusted SVG string sourced from the backend canister's openid_configs -->
                  {@html provider.logo}
                </div>
              {/if}
              <span>{name}</span>
            {/if}
          </button>
        </Tooltip>
      {/each}
      <!--
        SSO entry is always rendered. The domain is validated on the backend
        (a bare-authority check inside `discover_sso`); a malformed domain
        surfaces as an error inside the SignInWithSso screen rather than
        being gated here — we keep this option visible so users know the
        mechanism exists.
      -->
      <button
        class="btn btn-secondary h-16 w-full flex-col gap-1.5 text-xs whitespace-normal"
        onclick={signInWithSso}
        disabled={authenticatingProviderId !== undefined}
        aria-label={ssoLabel}
      >
        <SsoIcon class="size-5" />
        <span>{$t`SSO`}</span>
      </button>
    </div>
  </div>
  {#if continueOnAnotherDevice !== undefined && showLostAccess}
    <div class="flex flex-row items-center justify-between gap-4">
      <p class="text-text-tertiary text-sm">
        {$t`Add identity from another device`}
      </p>
      <button
        onclick={continueOnAnotherDevice}
        disabled={authenticatingProviderId !== undefined}
        class="text-text-primary text-sm font-semibold outline-0 hover:underline focus-visible:underline"
      >
        {$t`URL | QR Code`}
      </button>
    </div>
  {/if}
  {#if showLostAccess}
    <div class="flex flex-row items-center justify-between gap-4">
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
  {#if showLostAccess && showSwitchMode && !withinDialog}
    <div class="border-border-secondary border-t" aria-hidden="true"></div>
  {/if}
  {#if showSwitchMode}
    <div
      class={[
        "flex flex-row items-center gap-3",
        withinDialog
          ? "bg-bg-secondary dark:bg-bg-primary border-border-secondary -mx-4 -mb-4 border-t p-4 sm:-mx-6 sm:-mb-8 sm:rounded-b-2xl sm:p-6"
          : "py-2",
      ]}
    >
      <div class="min-w-0 flex-1">
        <div class="text-text-primary text-sm font-semibold">
          {switchModeTitle ??
            (mode === "signin"
              ? $t`Create new identity`
              : $t`Already have an identity?`)}
        </div>
        <div class="text-text-tertiary mt-1 text-xs">
          {$t`Use a passkey or familiar provider.`}
        </div>
      </div>
      <button
        onclick={onSwitchMode}
        disabled={authenticatingProviderId !== undefined}
        class="btn btn-secondary btn-sm shrink-0 gap-2"
      >
        {switchModeAction ?? (mode === "signin" ? $t`Create` : $t`Sign in`)}
        <ArrowRightIcon class="size-4 rtl:-scale-x-100" />
      </button>
    </div>
  {/if}
</div>
