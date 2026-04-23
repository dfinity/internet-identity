<script lang="ts">
  import type { PageProps } from "./$types";
  import type { Snippet } from "svelte";
  import type { OpenIdConfig } from "$lib/globals";
  import { lastUsedIdentitiesStore } from "$lib/stores/last-used-identities.store";
  import { authorizedStore } from "$lib/stores/authorization.store";
  import {
    isAuthenticatedStore,
    pendingOpenIdIssuerStore,
  } from "$lib/stores/authentication.store";
  import { establishedChannelStore } from "$lib/stores/channelStore";
  import { getDapps } from "$lib/legacy/flows/dappsExplorer/dapps";
  import { handleError } from "$lib/components/utils/error";
  import { toaster } from "$lib/components/utils/toaster";
  import { t } from "$lib/stores/locale.store";
  import { onMount } from "svelte";
  import { GUIDED_UPGRADE, MIN_GUIDED_UPGRADE } from "$lib/state/featureFlags";
  import { MigrationWizard } from "$lib/components/wizards/migration";
  import { XIcon } from "@lucide/svelte";
  import { Trans } from "$lib/components/locale";
  import MigrationIllustration from "$lib/components/illustrations/MigrationIllustration.svelte";
  import Dialog from "$lib/components/ui/Dialog.svelte";

  import RedirectAnimationView from "./views/RedirectAnimationView.svelte";
  import UpgradeSuccessView from "./views/UpgradeSuccessView.svelte";
  import ContinueView from "./views/ContinueView.svelte";
  import AuthWizardView from "./views/AuthWizardView.svelte";

  // --- OpenID resume imports ---
  import {
    authorizationContextStore,
    authorizationStore,
  } from "$lib/stores/authorization.store";
  import { decodeJWT, findConfig } from "$lib/utils/openID";
  import { AuthFlow } from "$lib/flows/authFlow.svelte";
  import {
    DirectOpenIdEvents,
    directOpenIdFunnel,
  } from "$lib/utils/analytics/DirectOpenIdFunnel";
  import { triggerDropWaveAnimation } from "$lib/utils/animation-dispatcher";
  import { createRedirectURL } from "$lib/utils/openID";
  import { sessionStore } from "$lib/stores/session.store";

  const { data }: PageProps = $props();

  // --- Local state ---
  let upgradeSuccess = $state(false);
  let openIdResumeProcessing = $state(false);

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
  const handleAuthWizardSignIn = async (identityNumber: bigint) => {
    lastUsedIdentitiesStore.selectIdentity(identityNumber);
  };
  const handleAuthWizardSignUp = async (identityNumber: bigint) => {
    toaster.success({
      title: $t`You're all set. Your identity has been created.`,
      duration: 4000,
    });
    lastUsedIdentitiesStore.selectIdentity(identityNumber);
  };
  const handleAuthWizardUpgrade = async () => {
    upgradeSuccess = true;
  };

  const handleAuthorize = (accountNumber: Promise<bigint | undefined>) => {
    authorizationStore.authorize(accountNumber);
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

  /** Process the OpenID callback and authorize. */
  const resumeOpenId = async () => {
    const searchParams = new URLSearchParams(window.location.hash.slice(1));
    window.history.replaceState(
      undefined,
      "",
      window.location.origin + "/authorize",
    );
    const redirectState = searchParams.get("state");
    const jwt = searchParams.get("id_token");
    const openIdAuthorizeState = sessionStorage.getItem(
      "ii-openid-authorize-state",
    );
    if (
      openIdAuthorizeState === null ||
      redirectState !== openIdAuthorizeState ||
      jwt === null
    ) {
      return;
    }
    const authFlow = new AuthFlow({ trackLastUsed: false });
    const { iss, aud, ...metadata } = decodeJWT(jwt);
    const config = findConfig(
      iss,
      aud,
      Object.entries(metadata).map(([key, value]) => [key, { String: value! }]),
    );
    if (config === undefined) {
      return;
    }
    pendingOpenIdIssuerStore.set(config.issuer);
    openIdResumeProcessing = true;
    triggerDropWaveAnimation();

    directOpenIdFunnel.addProperties({ openid_issuer: config.issuer });
    directOpenIdFunnel.trigger(DirectOpenIdEvents.CallbackFromOpenId);
    const authFlowResult = await authFlow.continueWithOpenId(config, jwt);
    const { name, email } = decodeJWT(jwt);
    if (authFlowResult.type === "signUp") {
      await authFlow.completeOpenIdRegistration(
        name ?? email?.split("@")[0] ?? $t`${config.name} user`,
      );
    }
    authorizationStore.authorize(Promise.resolve(undefined));
    directOpenIdFunnel.trigger(DirectOpenIdEvents.RedirectToApp);
  };

  onMount(() => {
    if (data.flow === "openid-init") {
      initiateOpenId(data.config);
    } else if (data.flow === "openid-resume") {
      resumeOpenId();
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
    class="grid w-full flex-1 items-center max-sm:items-stretch sm:max-w-100"
  >
    <div class="relative col-start-1 row-start-1 flex flex-col gap-5">
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
        {@render content()}
      </div>
    </div>
  </div>
{/snippet}

{#if data.flow === "openid-init"}
  <!-- OpenID init — nothing to render, onMount redirects to provider. -->
{:else if $authorizedStore !== undefined}
  <!-- User has been authorized — show redirect animation while delegation completes. -->
  <RedirectAnimationView />
{:else if data.flow === "openid-resume" && openIdResumeProcessing}
  <!-- OpenID callback is being processed — show animation while auth resolves. -->
  <RedirectAnimationView />
{:else if upgradeSuccess && $isAuthenticatedStore}
  <!-- Migration wizard completed — show success countdown before authorizing. -->
  {@render panelWrapper(upgradeSuccessContent)}
{:else if selectedIdentity !== undefined}
  <!-- Returning user with a selected identity — show account selection. -->
  {@render panelWrapper(continueContent)}
{:else}
  <!-- New user or no identity selected — show authentication methods. -->
  {@render panelWrapper(authWizardContent)}
{/if}

{#snippet upgradeSuccessContent()}
  <UpgradeSuccessView onAuthorize={handleAuthorize} />
{/snippet}

{#snippet continueContent()}
  <ContinueView
    effectiveOrigin={$authorizationContextStore.effectiveOrigin}
    onAuthorize={handleAuthorize}
  />
{/snippet}

{#snippet authWizardContent()}
  <AuthWizardView
    onSignIn={handleAuthWizardSignIn}
    onSignUp={handleAuthWizardSignUp}
    onUpgrade={handleAuthWizardUpgrade}
    onError={handleError}
  />
{/snippet}
