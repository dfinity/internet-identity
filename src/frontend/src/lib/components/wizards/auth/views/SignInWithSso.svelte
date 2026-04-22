<script lang="ts">
  import { onMount } from "svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import Input from "$lib/components/ui/Input.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import SsoIcon from "$lib/components/icons/SsoIcon.svelte";
  import { anonymousActor } from "$lib/globals";
  import {
    validateDomain,
    discoverSsoConfig,
    DomainNotConfiguredError,
  } from "$lib/utils/ssoDiscovery";
  import type { SsoDiscoveryResult } from "$lib/utils/ssoDiscovery";
  import { t } from "$lib/stores/locale.store";

  interface Props {
    continueWithSso: (
      result: SsoDiscoveryResult,
    ) => Promise<void | "cancelled">;
    goBack: () => void;
  }

  const { continueWithSso, goBack }: Props = $props();

  let inputRef = $state<HTMLInputElement>();
  let domain = $state("");
  let error = $state<string>();
  let isSubmitting = $state(false);

  /**
   * Map a backend/discovery error to a user-visible message. Tries to give
   * the most actionable phrasing available for each distinct failure mode:
   *
   * - **Canary-allowlist trap** from `add_discoverable_oidc_config`: the
   *   domain is well-formed but not yet approved by II admins.
   * - **Hop 1 unreachable / HTTP error / malformed response** → wrapped in
   *   {@link DomainNotConfiguredError}; we surface the specific reason
   *   (HTTP status or parser detail) so a domain owner can diagnose.
   * - **Hop 2 failures** (issuer / auth_endpoint mismatch, HTTPS, zod
   *   parse) → pattern-matched to short human messages; falls back to the
   *   raw error text so nothing gets silently swallowed.
   */
  const mapSubmitError = (e: unknown, domainInput: string): string => {
    if (e instanceof DomainNotConfiguredError) {
      if (e.reason === "http-error" && e.httpStatus !== undefined) {
        return $t`${domainInput} didn't serve /.well-known/ii-openid-configuration (HTTP ${String(e.httpStatus)}). The domain owner needs to publish it for II to sign you in.`;
      }
      if (e.reason === "network") {
        return $t`Couldn't reach ${domainInput}. Check the spelling and your network, then try again.`;
      }
      // invalid-response: response decoded but didn't match the expected
      // shape. The detail (zod error or "Provider URL must use HTTPS: …")
      // is much more actionable than "invalid response".
      if (e.detail !== undefined && e.detail.length > 0) {
        return $t`${domainInput}'s /.well-known/ii-openid-configuration is malformed: ${e.detail}`;
      }
      return $t`${domainInput}'s /.well-known/ii-openid-configuration is malformed.`;
    }
    if (e instanceof Error) {
      const msg = e.message;
      if (msg.toLowerCase().includes("canary allowlist")) {
        return $t`SSO is not available for "${domainInput}" yet. Ask an II admin to register this domain.`;
      }
      // Hop-2 verification failures — keep the detail but lead with a
      // short human summary.
      if (msg.includes("Provider issuer hostname mismatch")) {
        return $t`SSO provider misconfigured: issuer doesn't match the configured hostname. (${msg})`;
      }
      if (msg.includes("Provider authorization endpoint hostname mismatch")) {
        return $t`SSO provider misconfigured: authorization endpoint points to a different host than the issuer. (${msg})`;
      }
      if (msg.includes("Provider issuer must use HTTPS")) {
        return $t`SSO provider misconfigured: issuer URL is not HTTPS. (${msg})`;
      }
      if (msg.includes("Provider authorization endpoint must use HTTPS")) {
        return $t`SSO provider misconfigured: authorization endpoint is not HTTPS. (${msg})`;
      }
      if (msg.startsWith("Provider discovery:")) {
        return $t`SSO provider's discovery document is malformed: ${msg}`;
      }
      if (msg.startsWith("Rate limited:")) {
        return $t`Too many recent attempts for ${domainInput}. Wait a few minutes and try again.`;
      }
      if (msg === "Too many concurrent SSO discovery requests") {
        return $t`Several SSO sign-ins are in flight already. Wait a moment and try again.`;
      }
      return msg;
    }
    return $t`SSO sign-in failed. Please try again.`;
  };

  const handleSubmit = async () => {
    error = undefined;
    const trimmed = domain.trim().toLowerCase();
    if (trimmed.length === 0) {
      return;
    }

    try {
      validateDomain(trimmed);
    } catch (e) {
      error = e instanceof Error ? e.message : $t`Invalid domain`;
      return;
    }

    isSubmitting = true;
    try {
      // Register the domain with the backend. This is the gate: the backend
      // traps if the domain isn't on the canary allowlist. If it's already
      // registered, the call is idempotent and returns OK.
      await anonymousActor.add_discoverable_oidc_config({
        discovery_domain: trimmed,
      });

      // Run our own two-hop discovery and redirect. The backend will also
      // run its own discovery asynchronously (for JWT verification later);
      // the two paths are intentionally independent.
      const result = await discoverSsoConfig(trimmed);
      await continueWithSso(result);
    } catch (e) {
      error = mapSubmitError(e, trimmed);
    } finally {
      isSubmitting = false;
    }
  };

  onMount(() => {
    inputRef?.focus();
  });
</script>

<div class="flex flex-1 flex-col">
  <div class="text-text-primary mb-8 flex w-full flex-col gap-5">
    <FeaturedIcon size="lg" variant="info">
      <SsoIcon class="size-5" />
    </FeaturedIcon>
    <div class="flex flex-col gap-3">
      <h1 class="text-2xl font-medium">{$t`Sign In With SSO`}</h1>
      <p class="text-text-tertiary text-base font-medium">
        {$t`Enter your company domain`}
      </p>
    </div>
  </div>
  <form
    class="flex flex-col items-stretch gap-6"
    onsubmit={(e) => {
      e.preventDefault();
      handleSubmit();
    }}
  >
    <Input
      bind:element={inputRef}
      bind:value={domain}
      oninput={() => {
        error = undefined;
      }}
      inputmode="url"
      placeholder={$t`company.domain.com`}
      type="text"
      size="md"
      autocomplete="off"
      autocorrect="off"
      autocapitalize="off"
      spellcheck="false"
      disabled={isSubmitting}
      {error}
      aria-label={$t`Company domain`}
    />
    <Button
      onclick={handleSubmit}
      variant="primary"
      size="lg"
      type="submit"
      disabled={domain.trim().length === 0 || isSubmitting}
    >
      {#if isSubmitting}
        <ProgressRing />
        <span>{$t`Signing in...`}</span>
      {:else}
        <span>{$t`Continue`}</span>
      {/if}
    </Button>
    <button
      type="button"
      onclick={goBack}
      class="text-text-secondary self-center text-sm font-semibold outline-0 hover:underline focus-visible:underline"
    >
      {$t`Back to sign-in options`}
    </button>
  </form>
</div>
