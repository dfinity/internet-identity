<script lang="ts">
  import { onMount } from "svelte";
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import Input from "$lib/components/ui/Input.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import SsoIcon from "$lib/components/icons/SsoIcon.svelte";
  import {
    validateDomain,
    discoverSsoConfig,
    DomainNotConfiguredError,
  } from "$lib/utils/ssoDiscovery";
  import type { SsoDiscoveryResult } from "$lib/utils/ssoDiscovery";
  import { OAuthProviderError } from "$lib/utils/openID";
  import type { OpenIdCredential } from "$lib/generated/internet_identity_types";
  import { t } from "$lib/stores/locale.store";

  interface Props {
    continueWithSso: (
      result: SsoDiscoveryResult,
    ) => Promise<void | "cancelled">;
    goBack: () => void;
    /**
     * Credentials already linked to the signed-in identity. When present,
     * the Continue button is disabled and an inline hint is shown if the
     * discovered SSO's `(iss, aud)` matches an existing credential —
     * mirroring how the add-access-method UI disables already-linked
     * direct providers (Google / Apple / Microsoft) in
     * `AddAccessMethod.svelte`. Leave this `undefined` in the sign-in
     * flow, where reusing an already-linked credential is the point.
     */
    openIdCredentials?: OpenIdCredential[];
    /**
     * The target dapp origin, set only in the authorize (dapp sign-in) flow;
     * threaded into SSO discovery to resolve the per-app client for the origin.
     */
    origin?: string;
  }

  const { continueWithSso, goBack, openIdCredentials, origin }: Props =
    $props();

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

  /**
   * True when `preparedResult`'s `(issuer, client_id)` already matches a
   * credential in `openIdCredentials` — i.e. this SSO domain is already
   * linked to the signed-in identity. Keeps the add-access-method Continue
   * button disabled with an inline hint, matching the direct-provider
   * behaviour in `AddAccessMethod.svelte`. Always `false` in the sign-in
   * flow (where `openIdCredentials` is left undefined).
   */
  const isAlreadyLinked = $derived.by(() => {
    if (preparedResult === undefined || openIdCredentials === undefined) {
      return false;
    }
    const { discovery, clientId } = preparedResult;
    return openIdCredentials.some(
      (c) => c.iss === discovery.issuer && c.aud === clientId,
    );
  });

  let debounceTimer: ReturnType<typeof setTimeout> | undefined;
  /**
   * Aborts the in-flight `discoverSsoConfig` from a previous keystroke so a
   * new lookup doesn't pile up behind it. Replaced on every fresh lookup
   * and aborted on unmount.
   */
  let lookupController: AbortController | undefined;

  /**
   * Map caught errors to user-actionable copy, or `undefined` for
   * "nothing useful to tell the user" — the caller will fall back to a
   * generic message. Raw errors are always logged to the console so
   * engineers have the full stack while end users see concise copy.
   *
   * Only branches that the user (or their SSO admin) can act on live
   * here; provider-misconfiguration details (hostname mismatch, HTTPS,
   * malformed discovery document) are dropped to the generic path —
   * they're dev-facing and the console log is the right audience.
   */
  const mapSubmitError = (
    e: unknown,
    domainInput: string,
  ): string | undefined => {
    if (e instanceof DomainNotConfiguredError) {
      if (e.reason === "origin-denied") {
        // The org gated this dapp off, so no client can serve this origin.
        return $t`Your organization hasn't granted this app access via ${domainInput}.`;
      }
      // `timeout`: discovery never resolved — a wrong domain, or an unreachable
      // or failed discovery fetch (the canister keeps reporting `Pending` until
      // we time out) — so point the SSO admin at the discovery endpoint.
      return $t`Couldn't load SSO settings from ${domainInput}. Ask your SSO admin to check that /.well-known/ii-openid-configuration is reachable.`;
    }
    if (e instanceof OAuthProviderError) {
      // `unsupported_response_type` = the SSO app is code-only; II needs
      // the hybrid flow because it verifies JWTs canister-side with no
      // token-endpoint exchange. Spell out the fix so the SSO admin can
      // act on it directly.
      if (e.error === "unsupported_response_type") {
        return $t`${domainInput}'s SSO app doesn't allow the hybrid OAuth flow II requires. Ask the SSO admin to enable response_type "id_token code".`;
      }
      if (e.error === "access_denied") {
        return $t`${domainInput}'s SSO denied the sign-in.`;
      }
      // Everything else: show the provider's own words if any, else the
      // error code. Useful because these bubble straight from the IdP.
      if (e.errorDescription !== undefined && e.errorDescription.length > 0) {
        return $t`${domainInput}'s SSO returned "${e.error}": ${e.errorDescription}`;
      }
      return $t`${domainInput}'s SSO returned error "${e.error}".`;
    }
    return undefined;
  };

  /**
   * Set `error` from a thrown exception: always `console.error` the raw
   * error (so engineers can inspect the stack / non-Error values), then
   * surface a user-actionable message if we have one, or a generic
   * fallback.
   */
  const setErrorFrom = (e: unknown, domainInput: string) => {
    console.error("SSO sign-in failed", e);
    error =
      mapSubmitError(e, domainInput) ??
      $t`SSO sign-in for ${domainInput} failed. Please try again.`;
  };

  const invalidatePrepared = () => {
    preparedResult = undefined;
    error = undefined;
    if (debounceTimer !== undefined) {
      clearTimeout(debounceTimer);
      debounceTimer = undefined;
    }
    lookupController?.abort();
    lookupController = undefined;
    isLookingUp = false;
  };

  /**
   * Debounced lookup: resolves the SSO config via the canister shortly after
   * the user stops typing, and stashes the result so the Continue button can
   * hand off to `continueWithSso` synchronously.
   *
   * Drives the button's enabled state: the button lights up only once
   * `preparedResult` is populated for the current `domain`.
   */
  const handleInput = () => {
    invalidatePrepared();
    const trimmed = domain.trim().toLowerCase();
    if (trimmed.length === 0) return;

    // Immediate format validation so bad input gets feedback without a
    // backend round-trip. Only surface the error once the input looks
    // like a complete domain (contains a dot); otherwise the user is
    // still mid-typing.
    try {
      validateDomain(trimmed);
    } catch (e) {
      if (trimmed.includes(".")) {
        error = e instanceof Error ? e.message : $t`Invalid domain`;
      }
      return;
    }

    const controller = new AbortController();
    lookupController = controller;
    debounceTimer = setTimeout(async () => {
      debounceTimer = undefined;
      isLookingUp = true;
      // The input may change again while this await is in flight; we
      // only apply / error-out when our `trimmed` is still the current
      // domain, so a stale response can't clobber a fresher one. The
      // matching `lookupController` is also aborted by `invalidatePrepared`,
      // so a superseded lookup stops polling — ignored below since
      // cancellation isn't a user error.
      const matchesCurrent = () => trimmed === domain.trim().toLowerCase();
      try {
        const result = await discoverSsoConfig(
          trimmed,
          controller.signal,
          origin,
        );
        if (matchesCurrent()) {
          preparedResult = result;
        }
      } catch (e) {
        // Cancelled by a fresher keystroke — silently drop, keyed on
        // `controller.signal.aborted` rather than the error so a genuine
        // discovery timeout still surfaces.
        if (controller.signal.aborted) return;
        if (matchesCurrent()) {
          setErrorFrom(e, trimmed);
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
    error = undefined;
    try {
      // IMPORTANT: no `await` before `continueWithSso`. The popup is
      // opened synchronously inside `continueWithSso → requestJWT →
      // requestWithPopup → redirectInPopup → window.open`. Any await
      // between the click event and `window.open` causes Safari to
      // block the popup. The discovery is already resolved — stashed in
      // `preparedResult` by the debounced input handler.
      await continueWithSso(preparedResult);
    } catch (e) {
      setErrorFrom(e, domain.trim().toLowerCase());
    } finally {
      isSubmitting = false;
    }
  };

  onMount(() => {
    inputRef?.focus();
    return () => {
      if (debounceTimer !== undefined) clearTimeout(debounceTimer);
      lookupController?.abort();
    };
  });
</script>

<div class="flex flex-1 flex-col">
  <div class="text-text-primary mb-8 flex w-full flex-col gap-5">
    <FeaturedIcon size="lg" variant="info">
      <SsoIcon class="size-5" />
    </FeaturedIcon>
    <div class="flex flex-col gap-3">
      <h1 class="text-2xl font-medium">{$t`Continue with SSO`}</h1>
      <p class="text-text-tertiary text-base font-medium">
        {$t`Enter your company domain`}
      </p>
    </div>
  </div>
  <form
    class="flex flex-col items-stretch gap-6"
    onsubmit={(e) => {
      e.preventDefault();
      void handleSubmit();
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
    {#if isAlreadyLinked}
      <p class="text-text-tertiary text-sm">
        {$t`This SSO is already linked to your identity.`}
      </p>
    {/if}
    <button
      class="btn btn-primary btn-lg"
      type="submit"
      disabled={preparedResult === undefined ||
        isSubmitting ||
        isLookingUp ||
        isAlreadyLinked}
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
    </button>
    <button
      type="button"
      onclick={goBack}
      class="text-text-secondary self-center text-sm font-semibold outline-0 hover:underline focus-visible:underline"
    >
      {$t`Back to sign-in options`}
    </button>
  </form>
</div>
