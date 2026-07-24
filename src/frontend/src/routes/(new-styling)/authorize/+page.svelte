<script lang="ts">
  import type { PageProps } from "./$types";
  import type { Snippet } from "svelte";
  import type { OpenIdConfig } from "$lib/globals";
  import { lastUsedIdentitiesStore } from "$lib/stores/last-used-identities.store";
  import { authorizedStore } from "$lib/stores/authorization.store";
  import { isAuthenticatedStore } from "$lib/stores/authentication.store";
  import {
    channelStore,
    establishedChannelStore,
  } from "$lib/stores/channelStore";
  import { getDapps } from "$lib/legacy/flows/dappsExplorer/dapps";
  import { handleError } from "$lib/components/utils/error";
  import { toaster } from "$lib/components/utils/toaster";
  import { t } from "$lib/stores/locale.store";
  import { onMount } from "svelte";
  import { get } from "svelte/store";
  import { GUIDED_UPGRADE, MIN_GUIDED_UPGRADE } from "$lib/state/featureFlags";
  import { MigrationWizard } from "$lib/components/wizards/migration";
  import { XIcon } from "@lucide/svelte";
  import { Trans } from "$lib/components/locale";
  import MigrationIllustration from "$lib/components/illustrations/MigrationIllustration.svelte";
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import AuthPanel from "$lib/components/ui/AuthPanel.svelte";

  import RedirectAnimationView from "./views/RedirectAnimationView.svelte";
  import UpgradeSuccessView from "./views/UpgradeSuccessView.svelte";
  import ContinueView from "./views/ContinueView.svelte";
  import type { AccessLevel } from "$lib/utils/accessLevel";
  import AuthWizardView from "./views/AuthWizardView.svelte";
  import AttributeConsentView from "./views/AttributeConsentView.svelte";
  import {
    type AttributeConsent,
    attributeConsentStore,
    attributeConsentResultStore,
  } from "$lib/stores/attributeConsent.store";

  // --- OpenID resume imports ---
  import {
    authorizationContextStore,
    authorizationStore,
  } from "$lib/stores/authorization.store";
  import {
    decodeJWT,
    findConfig,
    isOpenIdCancelError,
    selectAuthScopes,
  } from "$lib/utils/openID";
  import { AuthFlow } from "$lib/flows/authFlow.svelte";
  import {
    DirectOpenIdEvents,
    directOpenIdFunnel,
  } from "$lib/utils/analytics/DirectOpenIdFunnel";
  import {
    createRedirectURL,
    extractIdTokenFromCallback,
  } from "$lib/utils/openID";
  import { sessionStore } from "$lib/stores/session.store";
  import {
    discoverSsoConfig,
    type SsoDiscoveryResult,
  } from "$lib/utils/ssoDiscovery";
  import { SsoNormalLoginRequiredError } from "$lib/utils/authentication/jwt";
  import SsoNormalLoginRequired from "$lib/components/wizards/auth/views/SsoNormalLoginRequired.svelte";
  import { waitForStore } from "$lib/utils/utils";
  import { remapToLegacyDomain } from "$lib/utils/iiConnection";

  const { data }: PageProps = $props();

  // --- Local state ---
  let upgradeSuccess = $state(false);
  let openIdResumeProcessing = $state(false);
  // Set when a 1-click SSO redemption hits the normal-login-required fail-safe.
  // Holds everything the dialog needs to run one normal (primary-client)
  // sign-in, then replay the stashed gated JWT and authorize.
  let ssoNormalLogin = $state<{
    name: string;
    discovery: SsoDiscoveryResult;
    jwt: string;
    config: OpenIdConfig;
    domain: string;
    origin: string;
  }>();
  let ssoNormalLoginBusy = $state(false);

  // --- Upgrade panel state ---
  let isUpgradeCollapsed = $state(
    localStorage.getItem("ii-guided-upgrade-collapsed") === "true" ||
      $MIN_GUIDED_UPGRADE,
  );
  let upgradePanelHeight = $state<number>(224);
  let isUpgrading = $state(false);

  $effect(() => {
    localStorage.setItem(
      "ii-guided-upgrade-collapsed",
      `${isUpgradeCollapsed}`,
    );
  });

  const dapps = getDapps();
  const dapp = $derived(
    dapps.find((dapp) => dapp.hasOrigin($establishedChannelStore.origin)),
  );

  // --- View selection ---
  const selectedIdentity = $derived($lastUsedIdentitiesStore.selected);

  // --- Handlers ---
  const handleAuthWizardSignIn = (identityNumber: bigint): Promise<void> => {
    lastUsedIdentitiesStore.selectIdentity(identityNumber);
    return Promise.resolve();
  };
  const handleAuthWizardSignUp = (identityNumber: bigint): Promise<void> => {
    toaster.success({
      title: $t`You're all set. Your identity has been created.`,
      duration: 4000,
    });
    lastUsedIdentitiesStore.selectIdentity(identityNumber);
    return Promise.resolve();
  };

  // Dialog primary action for the 1-click normal-login fail-safe: run one normal
  // (primary-client) sign-in to bridge the identity, replay the stashed gated JWT
  // (no fresh ceremony), then authorize and redirect to the app.
  const handleSsoNormalLoginContinue = async () => {
    if (ssoNormalLogin === undefined) {
      return;
    }
    const { discovery, jwt, config, domain, origin } = ssoNormalLogin;
    ssoNormalLoginBusy = true;
    try {
      const authFlow = new AuthFlow();
      const primaryResult: SsoDiscoveryResult = {
        ...discovery,
        resolvedClientId: discovery.clientId,
      };
      const normal = await authFlow.continueWithSso(primaryResult, "both");
      if (normal?.type === "signUp") {
        await authFlow.completeSsoRegistration(
          normal.name ??
            normal.email?.split("@")[0] ??
            discovery.name ??
            domain,
        );
      }
      const replay = await authFlow.continueWithOpenId(
        config,
        jwt,
        "signin",
        domain,
        { origin },
      );
      if (replay?.type !== "signIn") {
        throw new Error("Gated SSO sign-in did not resolve after normal login");
      }
      authorizationStore.setFlow({ type: "1-click-sso", domain });
      authorizationStore.authorize(Promise.resolve(undefined), "full-access");
    } catch (e) {
      ssoNormalLoginBusy = false;
      if (isOpenIdCancelError(e)) {
        return;
      }
      ssoNormalLogin = undefined;
      handleError(e);
    }
  };

  const handleSsoNormalLoginCancel = () => {
    ssoNormalLogin = undefined;
  };
  const handleAuthorize = (
    accountNumber: Promise<bigint | undefined>,
    accessLevel: AccessLevel,
  ) => {
    authorizationStore.authorize(accountNumber, accessLevel);
  };

  const handleAttributeConsent = (consent: AttributeConsent) => {
    attributeConsentStore.setConsent(consent);
  };

  const onUpgradeWizardSuccess = (identityNumber: bigint) => {
    lastUsedIdentitiesStore.selectIdentity(identityNumber);
    isUpgrading = false;
    isUpgradeCollapsed = true;
    upgradeSuccess = true;
  };

  /** Redirect to the OpenID provider. */
  const initiateOpenId = (config: OpenIdConfig) => {
    directOpenIdFunnel.trigger(DirectOpenIdEvents.RedirectToOpenId, {
      openid_issuer: config.issuer,
    });
    const next = createRedirectURL(
      {
        clientId: config.client_id,
        authURL: config.auth_uri,
        authScope: config.auth_scope.join(" "),
      },
      {
        nonce: $sessionStore.nonce,
        mediation: "required",
      },
    );
    sessionStorage.setItem(
      "ii-pending-channel-origin",
      $establishedChannelStore.origin,
    );
    sessionStorage.setItem(
      "ii-openid-authorize-state",
      next.searchParams.get("state")!,
    );
    window.location.assign(next);
  };

  /**
   * Resolve once the dapp's effective origin is known. Pending `?sso=`/`?openid=`
   * flows don't have it until the dapp's authorize request arrives; times out so
   * a non-conforming dapp can't hang the flow.
   */
  const waitForEffectiveOrigin = (
    timeoutMs = 15_000,
  ): Promise<string | undefined> =>
    new Promise((resolve) => {
      const current = get(authorizationStore)?.effectiveOrigin;
      if (current !== undefined) {
        resolve(current);
        return;
      }
      let settled = false;
      const finish = (value: string | undefined) => {
        if (settled) return;
        settled = true;
        clearTimeout(timer);
        unsubscribe();
        resolve(value);
      };
      const timer = setTimeout(() => finish(undefined), timeoutMs);
      const unsubscribe = authorizationStore.subscribe((context) => {
        if (context?.effectiveOrigin !== undefined) {
          finish(context.effectiveOrigin);
        }
      });
    });

  /**
   * 1-click SSO equivalent of {@link initiateOpenId}: resolve the discovery
   * domain via the canister, then redirect through the same OpenID-redirect
   * machinery as the direct flow. Distinct from the wizard `SignInWithSso`
   * path only in that it has nothing to debounce or validate UI-side — the URL
   * has already committed to a domain that's on the allowlist (gated in
   * `+page.ts`).
   */
  const initiateSso = async (domain: string, derivationOrigin?: string) => {
    // `appOrigin` only selects which client to run against — the server
    // re-validates in the gate, so a wrong value can deny but never bypass.
    // Remap to canonical (as the delegation path does) so it keys the origin the
    // gate will.
    const appOrigin = remapToLegacyDomain(
      derivationOrigin ??
        (await waitForStore(channelStore, (ch) => ch?.origin)),
    );
    const result = await discoverSsoConfig(domain, undefined, appOrigin);
    // Stash the SSO discovery domain so `resumeOpenId` knows the returning JWT
    // belongs to a 1-click SSO flow rather than a 1-click OpenID one. The flow
    // type drives which auto-approve allowlist the attribute consent handler
    // bypasses.
    sessionStorage.setItem("ii-sso-1-click-domain", domain);
    const syntheticConfig: OpenIdConfig = {
      auth_uri: result.discovery.authorization_endpoint,
      jwks_uri: "",
      logo: "",
      name: "SSO",
      fedcm_uri: [],
      email_verification: [],
      issuer: result.discovery.issuer,
      auth_scope: selectAuthScopes(result.discovery.scopes_supported),
      // The per-app client for a gated dapp, else the primary.
      client_id: result.resolvedClientId,
      seed_jwks: [],
    };
    initiateOpenId(syntheticConfig);
  };

  /** Process the OpenID callback and authorize. */
  const resumeOpenId = async () => {
    // The canister's POST /callback landing page stashes the payload here
    // before navigating to `/authorize?flow=openid-resume` in the same-tab
    // flow. Single-use: remove it before anything else can throw.
    const storedPayload = sessionStorage.getItem("ii-openid-callback-data");
    sessionStorage.removeItem("ii-openid-callback-data");
    window.history.replaceState(
      undefined,
      "",
      window.location.origin + "/authorize",
    );
    const openIdAuthorizeState = sessionStorage.getItem(
      "ii-openid-authorize-state",
    );
    // The callback landing page routes on this marker (present = resume
    // in-app, absent = deliver to the opener); popups inherit a copy of
    // sessionStorage in Chrome, so a stale marker would misroute a later
    // popup sign-in from this tab.
    sessionStorage.removeItem("ii-openid-authorize-state");
    if (storedPayload === null || openIdAuthorizeState === null) {
      return;
    }
    let jwt: string;
    try {
      jwt = extractIdTokenFromCallback(
        JSON.parse(storedPayload),
        openIdAuthorizeState,
      );
    } catch {
      // A state mismatch, an IdP error report or a missing token is not
      // recoverable here; fall back to the regular flow so the user can
      // start sign-in again.
      return;
    }
    // Track the last-used identity: a 1-click sign-up (or a sign-in on a
    // device with no local entry yet) must land in localStorage so the
    // identity shows up when the user later visits II directly.
    const authFlow = new AuthFlow();
    const { iss, aud, ...metadata } = decodeJWT(jwt);
    // The marker is set by `initiateSso` for the `?sso=<domain>` path.
    // If present, treat the returning JWT as a 1-click SSO flow so the
    // attribute consent handler auto-approves `sso:<domain>:<key>` keys
    // instead of `openid:<issuer>:<key>` ones. Clear it so a stale
    // marker can't leak into a subsequent direct-OpenID round-trip.
    const ssoDomain = sessionStorage.getItem("ii-sso-1-click-domain");
    sessionStorage.removeItem("ii-sso-1-click-domain");
    // Show the redirect animation now so the wait below doesn't flash the wizard.
    openIdResumeProcessing = true;
    // Redeem through the origin-bound gate path so the session certifies
    // `sso:<domain>` attributes.
    let sso: { origin: string } | undefined;
    if (ssoDomain !== null) {
      const dappOrigin = await waitForEffectiveOrigin();
      if (dappOrigin !== undefined) {
        sso = { origin: dappOrigin };
      }
    }
    let config: OpenIdConfig | undefined;
    if (ssoDomain !== null) {
      // SSO sign-in: there's no matching `openid_configs` entry, so build a
      // synthetic config from the JWT itself. `continueWithOpenId` only needs
      // `name` (for analytics) and `client_id` / `issuer` here since the JWT is
      // already in hand; `ssoDomain` is passed through so the canister verifies
      // it against the SSO discovery for that domain.
      config = {
        auth_uri: "",
        jwks_uri: "",
        logo: "",
        name: ssoDomain,
        fedcm_uri: [],
        email_verification: [],
        issuer: iss,
        auth_scope: [],
        client_id: aud ?? "",
        seed_jwks: [],
      };
      authorizationStore.setFlow({ type: "1-click-sso", domain: ssoDomain });
    } else {
      config = findConfig(
        iss,
        aud,
        Object.entries(metadata).map(([key, value]) => [
          key,
          { String: value! },
        ]),
      );
      if (config === undefined) {
        return;
      }
      authorizationStore.setFlow({
        type: "1-click-openid",
        issuer: config.issuer,
      });
    }

    directOpenIdFunnel.addProperties({ openid_issuer: config.issuer });
    directOpenIdFunnel.trigger(DirectOpenIdEvents.CallbackFromOpenId);
    const authFlowResult = await authFlow.continueWithOpenId(
      config,
      jwt,
      undefined,
      ssoDomain ?? undefined,
      sso,
    );
    const { name, email } = decodeJWT(jwt);
    if (authFlowResult?.type === "signUp") {
      // AuthFlow owns last-used persistence (via `trackLastUsed`): sign-up is
      // recorded by `completeOpenIdRegistration` here, and the sign-in case is
      // already recorded inside `continueWithOpenId` (the JWT was supplied, so
      // there's no interactive disambiguation to defer the write for).
      try {
        await authFlow.completeOpenIdRegistration(
          name ?? email?.split("@")[0] ?? $t`${config.name} user`,
        );
      } catch (e) {
        // A non-`sub` gated org can't register from the per-app token: the
        // identity must sign in normally (primary client) first to bridge its
        // stable id. Hand off to the wizard's "sign in normally first" CTA,
        // seeded with the resolved gated result. `sub` orgs never hit this —
        // they register in one trip.
        if (
          e instanceof SsoNormalLoginRequiredError &&
          ssoDomain !== null &&
          sso !== undefined
        ) {
          const dappOrigin = await waitForEffectiveOrigin();
          const appOrigin =
            dappOrigin !== undefined
              ? remapToLegacyDomain(dappOrigin)
              : undefined;
          // Re-resolve to get the org's display name + primary client for the
          // dialog. The gated JWT (`jwt`), its synthetic `config`, and the exact
          // `origin` used at redemption are stashed for the silent replay.
          const discovery = await discoverSsoConfig(
            ssoDomain,
            undefined,
            appOrigin,
          );
          ssoNormalLogin = {
            name: discovery.name ?? ssoDomain,
            discovery,
            jwt,
            config,
            domain: ssoDomain,
            origin: sso.origin,
          };
          openIdResumeProcessing = false;
          return;
        }
        throw e;
      }
    }
    // 1-click OpenID flow: no access-level toggle, always full access.
    authorizationStore.authorize(Promise.resolve(undefined), "full-access");
    directOpenIdFunnel.trigger(DirectOpenIdEvents.RedirectToApp);
  };

  onMount(() => {
    if (data.flow === "openid-init") {
      initiateOpenId(data.config);
    } else if (data.flow === "sso-init") {
      // Discovery + redirect runs async; the page renders nothing while
      // it's in flight (mirrors the openid-init render branch).
      initiateSso(data.domain, data.derivationOrigin).catch(handleError);
    } else if (data.flow === "openid-resume") {
      // resumeOpenId sets the flow once the JWT (and thus the issuer)
      // has been decoded.
      resumeOpenId().catch(handleError);
    } else {
      authorizationStore.setFlow({ type: "regular" });
    }
  });
</script>

{#snippet upgradePanel()}
  <div
    class={[
      "relative overflow-hidden rounded-xl border-1 transition-all duration-200 max-sm:mx-4",
      "border-[#d3d8e6] bg-[#e8e9f2]",
      "dark:border-[#34384b] dark:bg-[#1f212d]",
      isUpgradeCollapsed && "sm:-mb-[calc(var(--upgrade-card-height)-22px)]",
    ]}
    style="--upgrade-card-height: {upgradePanelHeight}px"
  >
    <div
      bind:clientHeight={upgradePanelHeight}
      inert={isUpgradeCollapsed}
      class={[
        "relative flex flex-col items-stretch p-6 transition-all transition-discrete duration-200",
        isUpgradeCollapsed
          ? "opacity-0 max-sm:-mb-[calc(var(--upgrade-card-height)-44px)]"
          : "z-1 delay-100",
      ]}
    >
      <MigrationIllustration
        class={[
          "mb-3 self-center text-black dark:text-white",
          "!gap-3 !py-1.5 [&_svg:first-child]:!h-3 [&_svg:last-child]:!h-3 [&_svg:nth-child(2)]:!size-3",
        ]}
      />
      <h2
        class="mx-6 mb-3 text-center text-lg font-medium text-balance text-black dark:text-white"
      >
        {#if dapp?.name !== undefined}
          {@const application = dapp.name}
          {$t`${application} has moved to the new Internet Identity`}
        {:else}
          {$t`This app has moved to the new Internet Identity`}
        {/if}
      </h2>
      <p class="mb-3 text-center text-sm text-black/70 dark:text-white/70">
        <Trans>Still using an identity number?</Trans>
      </p>
      <button
        onclick={() => (isUpgrading = true)}
        class="btn !bg-[#6752cc] !text-white hover:!bg-[#6d57cf]"
      >
        {$t`Upgrade your identity`}
      </button>
      <button
        onclick={() => (isUpgradeCollapsed = true)}
        class="btn btn-tertiary btn-lg btn-icon absolute end-2 top-2 !rounded-full hover:!bg-black/5 dark:hover:!bg-white/3"
        aria-label={$t`Close`}
      >
        <XIcon class="size-5" aria-hidden="true" />
      </button>
    </div>
    <div
      inert={!isUpgradeCollapsed}
      class={[
        "absolute inset-x-0 top-0 flex h-11 flex-row items-stretch gap-3 transition-all transition-discrete duration-200",
        !isUpgradeCollapsed ? "opacity-0" : "z-1 delay-200",
      ]}
    >
      <button
        onclick={() => (isUpgradeCollapsed = false)}
        class="flex-1 border-none ps-6 text-start text-sm text-black/70 outline-none dark:text-white/70"
      >
        <Trans>Still using an identity number?</Trans>
      </button>
      <button
        onclick={() => (isUpgrading = true)}
        class="me-6 border-none text-sm font-semibold text-black/70 outline-none hover:underline focus-visible:underline dark:text-white/70"
      >
        {$t`Upgrade`}
      </button>
    </div>
  </div>
  {#if isUpgrading}
    <Dialog onClose={() => (isUpgrading = false)}>
      <MigrationWizard
        onSuccess={onUpgradeWizardSuccess}
        onError={(error) => {
          handleError(error);
          isUpgrading = false;
        }}
      />
    </Dialog>
  {/if}
{/snippet}

{#snippet panelWrapper(content: Snippet)}
  <div
    class="grid w-full flex-1 items-center max-sm:items-stretch sm:w-100 sm:max-w-100"
  >
    <div class="relative col-start-1 row-start-1 flex min-w-0 flex-col gap-5">
      {#if $GUIDED_UPGRADE || $MIN_GUIDED_UPGRADE}
        {@render upgradePanel()}
      {/if}
      <div
        class={[
          "z-1 grid w-full flex-col overflow-hidden rounded-xl px-4 pt-5 pb-8",
          "max-sm:flex-1",
          "sm:bg-bg-secondary sm:border-border-secondary sm:border sm:px-6",
          ($GUIDED_UPGRADE || $MIN_GUIDED_UPGRADE) &&
            isUpgradeCollapsed &&
            "rounded-t-none",
        ]}
      >
        <AuthPanel>
          {@render content()}
        </AuthPanel>
      </div>
    </div>
  </div>
{/snippet}

{#if data.flow === "openid-init" || data.flow === "sso-init"}
  <!-- OpenID/SSO init — nothing to render, onMount redirects to provider. -->
{:else if $attributeConsentStore !== undefined && $attributeConsentResultStore === undefined && ($authorizedStore !== undefined || data.flow === "openid-resume")}
  <!-- Consent needed (or loading) — consent view handles its own loading state. -->
  {@render panelWrapper(attributeConsentContent)}
{:else if $authorizedStore !== undefined || (data.flow === "openid-resume" && openIdResumeProcessing)}
  <!-- Authorized or OpenID callback processing — show redirect animation. -->
  <RedirectAnimationView />
{:else if upgradeSuccess && $isAuthenticatedStore}
  <!-- Migration wizard completed — show success countdown before authorizing. -->
  {@render panelWrapper(upgradeSuccessContent)}
{:else if ssoNormalLogin !== undefined}
  <!-- 1-click SSO hit the normal-login-required fail-safe — show the one-step
       "First sign-in with X" dialog instead of proceeding. -->
  {@render panelWrapper(ssoNormalLoginContent)}
{:else if selectedIdentity !== undefined}
  <!-- Returning user with a selected identity — show account selection. -->
  {@render panelWrapper(continueContent)}
{:else}
  <!-- New user or no identity selected — show authentication methods. -->
  {@render panelWrapper(authWizardContent)}
{/if}

{#snippet attributeConsentContent()}
  {#if $attributeConsentStore !== undefined}
    {#key $attributeConsentStore}
      <AttributeConsentView
        context={$attributeConsentStore}
        variant={data.flow === "openid-resume" ? "openid" : "normal"}
        onConsent={handleAttributeConsent}
      />
    {/key}
  {/if}
{/snippet}

{#snippet upgradeSuccessContent()}
  <UpgradeSuccessView onAuthorize={handleAuthorize} />
{/snippet}

{#snippet continueContent()}
  <ContinueView
    effectiveOrigin={$authorizationContextStore.effectiveOrigin}
    displayOrigin={$establishedChannelStore.origin}
    onAuthorize={handleAuthorize}
  />
{/snippet}

{#snippet authWizardContent()}
  <AuthWizardView
    onSignIn={handleAuthWizardSignIn}
    onSignUp={handleAuthWizardSignUp}
    onError={handleError}
    mode="signin"
  />
{/snippet}

{#snippet ssoNormalLoginContent()}
  {#if ssoNormalLogin !== undefined}
    <SsoNormalLoginRequired
      name={ssoNormalLogin.name}
      onContinue={handleSsoNormalLoginContinue}
      onCancel={handleSsoNormalLoginCancel}
      loading={ssoNormalLoginBusy}
    />
  {/if}
{/snippet}
