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
  import { OAuthProviderError } from "$lib/utils/openID";
  import { t } from "$lib/stores/locale.store";

  interface Props {
    continueWithSso: (
      result: SsoDiscoveryResult,
    ) => Promise<void | "cancelled">;
    goBack: () => void;
  }

  const { continueWithSso, goBack }: Props = $props();

  /**
   * Debounce delay before kicking off the (network-heavy) two-hop lookup.
   * Short enough that the button typically enables by the time the user
   * finishes typing; long enough to avoid firing requests on every keystroke.
   */
  const LOOKUP_DEBOUNCE_MS = 200;

  let inputRef = $state<HTMLInputElement>();
  let domain = $state("");
  let error = $state<string>();
  let isLookingUp = $state(false);
  let isSubmitting = $state(false);
  /**
   * Result of the most recent successful discovery for the currently-typed
   * `domain`. Non-undefined means the Continue button can invoke
   * `continueWithSso` directly from the click handler — critical for
   * Safari, which blocks `window.open` calls that follow an `await`.
   */
  let preparedResult = $state<SsoDiscoveryResult>();

  let debounceTimer: ReturnType<typeof setTimeout> | undefined;

  const mapSubmitError = (e: unknown, domainInput: string): string => {
    if (e instanceof DomainNotConfiguredError) {
      if (e.reason === "http-error" && e.httpStatus !== undefined) {
        return $t`${domainInput} didn't serve /.well-known/ii-openid-configuration (HTTP ${String(e.httpStatus)}). The domain owner needs to publish it for II to sign you in.`;
      }
      if (e.reason === "network") {
        return $t`Couldn't reach ${domainInput}. Check the spelling and your network, then try again.`;
      }
      if (e.detail !== undefined && e.detail.length > 0) {
        return $t`${domainInput}'s /.well-known/ii-openid-configuration is malformed: ${e.detail}`;
      }
      return $t`${domainInput}'s /.well-known/ii-openid-configuration is malformed.`;
    }
    if (e instanceof OAuthProviderError) {
      // `unsupported_response_type` is the signature of an SSO app that
      // only allows the plain authorization-code flow. II needs the
      // hybrid flow (id_token + code) because it verifies JWTs canister-
      // side with no token-endpoint exchange. Spell out the fix so the
      // SSO admin can act on it directly.
      if (e.error === "unsupported_response_type") {
        return $t`${domainInput}'s SSO app doesn't allow the hybrid OAuth flow II requires. Ask the SSO admin to enable response_type "id_token code" (e.g. in Okta, set the app to Single-Page Application with "Implicit (hybrid)" grant enabled).`;
      }
      if (e.error === "access_denied") {
        return $t`${domainInput}'s SSO denied the sign-in. Try again, and check with your SSO admin if the problem persists.`;
      }
      if (e.errorDescription !== undefined && e.errorDescription.length > 0) {
        return $t`${domainInput}'s SSO returned "${e.error}": ${e.errorDescription}`;
      }
      return $t`${domainInput}'s SSO returned error "${e.error}".`;
    }
    if (e instanceof Error) {
      const msg = e.message;
      if (msg.toLowerCase().includes("canary allowlist")) {
        return $t`SSO is not available for "${domainInput}" yet. Ask an II admin to register this domain.`;
      }
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

  const invalidatePrepared = () => {
    preparedResult = undefined;
    error = undefined;
    if (debounceTimer !== undefined) {
      clearTimeout(debounceTimer);
      debounceTimer = undefined;
    }
    isLookingUp = false;
  };

  /**
   * Debounced lookup: runs the backend registration + two-hop discovery
   * shortly after the user stops typing, and stashes the result so the
   * Continue button can hand off to `continueWithSso` synchronously.
   *
   * Drives the button's enabled state: the button lights up only once
   * `preparedResult` is populated for the current `domain`.
   */
  const handleInput = () => {
    invalidatePrepared();
    const trimmed = domain.trim().toLowerCase();
    if (trimmed.length === 0) return;

    // Immediate format validation so bad input gets feedback without a
    // backend round-trip. Only surface the error if the input looks like
    // a complete domain (contains a dot); otherwise the user is still
    // mid-typing.
    try {
      validateDomain(trimmed);
    } catch (e) {
      if (trimmed.includes(".")) {
        error = e instanceof Error ? e.message : $t`Invalid domain`;
      }
      return;
    }

    debounceTimer = setTimeout(async () => {
      debounceTimer = undefined;
      isLookingUp = true;
      // The input may change again while these awaits are in flight; we
      // only apply / error-out when our `trimmed` is still the current
      // domain, so a stale response can't clobber a fresher one.
      const matchesCurrent = () => trimmed === domain.trim().toLowerCase();
      try {
        await anonymousActor.add_discoverable_oidc_config({
          discovery_domain: trimmed,
        });
        const result = await discoverSsoConfig(trimmed);
        if (matchesCurrent()) {
          preparedResult = result;
        }
      } catch (e) {
        if (matchesCurrent()) {
          error = mapSubmitError(e, trimmed);
        }
      } finally {
        if (matchesCurrent()) {
          isLookingUp = false;
        }
      }
    }, LOOKUP_DEBOUNCE_MS);
  };

  const handleSubmit = async () => {
    if (preparedResult === undefined) {
      // Button should be disabled in this state; defensive no-op.
      return;
    }
    isSubmitting = true;
    try {
      // IMPORTANT: no `await` before `continueWithSso`. The popup is
      // opened synchronously inside `continueWithSso → requestJWT →
      // requestWithPopup → redirectInPopup → window.open`. Any await
      // between the click event and `window.open` causes Safari to
      // block the popup. All the network work
      // (add_discoverable_oidc_config + two-hop discovery) is already
      // done — stashed in `preparedResult` by the debounced input
      // handler.
      await continueWithSso(preparedResult);
    } catch (e) {
      error = mapSubmitError(e, domain.trim().toLowerCase());
    } finally {
      isSubmitting = false;
    }
  };

  onMount(() => {
    inputRef?.focus();
    return () => {
      if (debounceTimer !== undefined) clearTimeout(debounceTimer);
    };
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
      oninput={handleInput}
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
      variant="primary"
      size="lg"
      type="submit"
      disabled={preparedResult === undefined || isSubmitting || isLookingUp}
    >
      {#if isSubmitting}
        <ProgressRing />
        <span>{$t`Signing in...`}</span>
      {:else if isLookingUp}
        <ProgressRing />
        <span>{$t`Checking...`}</span>
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
