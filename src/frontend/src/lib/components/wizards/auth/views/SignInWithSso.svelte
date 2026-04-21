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
   * Map a backend/discovery error to a user-visible message. Three cases
   * we explicitly translate:
   *
   * 1. Canary-allowlist trap from the backend's `add_discoverable_oidc_config`
   *    → the domain is well-formed but not yet approved by II admins.
   * 2. {@link DomainNotConfiguredError} from hop-1 discovery → the domain
   *    owner hasn't set up `/.well-known/ii-openid-configuration` (or it's
   *    broken). We include the HTTP status when the failure was an HTTP
   *    error, otherwise a generic reason.
   * 3. Anything else → surface the raw message, which should only happen
   *    for unexpected infrastructure failures.
   */
  const mapSubmitError = (e: unknown, domainInput: string): string => {
    if (e instanceof DomainNotConfiguredError) {
      // Reported errors the user can act on:
      // - HTTP error: e.g. 404 when the path doesn't exist (most common).
      // - invalid-response: 2xx but HTML / non-JSON body (SPA fallback).
      // - network: DNS/TLS/timeout.
      const detail =
        e.reason === "http-error" && e.httpStatus !== undefined
          ? $t`HTTP ${String(e.httpStatus)}`
          : e.reason === "invalid-response"
            ? $t`invalid response`
            : $t`network error`;
      return $t`This domain isn't correctly configured for Internet Identity (${detail}).`;
    }
    if (e instanceof Error) {
      if (e.message.toLowerCase().includes("canary allowlist")) {
        return $t`SSO is not available for "${domainInput}" yet.`;
      }
      return e.message;
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
