<script lang="ts">
  import { browser } from "$app/environment";
  import Footer from "$lib/components/layout/Footer.svelte";
  import { manuallyReroute } from "$lib/utils/reroute";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";
  import { ArrowRightIcon, PlusIcon, PencilIcon } from "@lucide/svelte";
  import FlairCanvas from "$lib/components/backgrounds/FlairCanvas.svelte";
  import Logo from "$lib/components/ui/Logo.svelte";
  import { handleError } from "$lib/components/utils/error";
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import {
    AuthWizard,
    IdentityAlreadyLinked,
    IdentityNotConnected,
    SignUpHero,
    SwitchAccessMethod,
  } from "$lib/components/wizards/auth";
  import {
    afterNavigate,
    beforeNavigate,
    goto,
    preloadData,
  } from "$app/navigation";
  import {
    lastUsedIdentitiesStore,
    type LastUsedIdentity,
  } from "$lib/stores/last-used-identities.store";
  import { backendCanisterConfig } from "$lib/globals";
  import type { AccessMethod } from "$lib/components/wizards/auth/views/SwitchAccessMethod.svelte";
  import { toaster } from "$lib/components/utils/toaster";
  import {
    AuthenticationV2Events,
    authenticationV2Funnel,
  } from "$lib/utils/analytics/authenticationV2Funnel";
  import { page } from "$app/state";
  import { authenticationStore } from "$lib/stores/authentication.store";
  import { sessionStore } from "$lib/stores/session.store";
  import ManageIdentities from "$lib/components/ui/ManageIdentities.svelte";
  import IdentityAvatar from "$lib/components/ui/IdentityAvatar.svelte";
  import IdentityListItem from "$lib/components/ui/IdentityListItem.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import { AuthLastUsedFlow } from "$lib/flows/authLastUsedFlow.svelte";
  import { onMount } from "svelte";
  import { analytics } from "$lib/utils/analytics/analytics";
  import { getMetadataString } from "$lib/utils/openID";

  const authLastUsedFlow = new AuthLastUsedFlow();

  let next = $state("/manage");
  let isAuthDialogOpen = $state(false);
  let isCreateIdentityDialogOpen = $state(false);
  let isManageIdentitiesDialogOpen = $state(false);
  let isAuthenticating = $state(false);
  let switchingToIdentity = $state<bigint>();

  let notConnectedPayload = $state<{
    providerName: string;
    providerLogo?: string;
    userName?: string;
    userEmail?: string;
    resume: () => Promise<void>;
    cancel: () => void;
  }>();

  let alreadyLinkedPayload = $state<{
    providerName: string;
    providerLogo?: string;
    userName?: string;
    userEmail?: string;
    signIn: () => Promise<void>;
    cancel: () => void;
  }>();

  let methodSwitchPayload = $state<{
    previous: LastUsedIdentity;
    newProvider: AccessMethod;
    proceed: () => Promise<void>;
  }>();

  // True when the sign-up dialog was opened by toggling from the sign-in
  // modal — in that case "Sign in" inside the sign-up dialog should return
  // to the sign-in modal, not just close down to the landing page.
  let signUpOpenedFromSignInModal = $state(false);

  const authMethodToAccessMethod = (
    m: LastUsedIdentity["authMethod"],
  ): AccessMethod => {
    if ("passkey" in m) return { type: "passkey" };
    if ("openid" in m) {
      const config = backendCanisterConfig.openid_configs[0]?.find(
        (c) => c.issuer === m.openid.iss,
      );
      return {
        type: "openid",
        logo: config?.logo ?? "",
        name: config?.name ?? m.openid.iss,
      };
    }
    return { type: "sso", name: m.sso.name ?? m.sso.domain };
  };

  const lastUsedIdentities = $derived(
    Object.values($lastUsedIdentitiesStore.identities).sort(
      (a, b) => b.lastUsedTimestampMillis - a.lastUsedTimestampMillis,
    ),
  );
  const selectedIdentity = $derived($lastUsedIdentitiesStore.selected);
  const otherIdentities = $derived(
    lastUsedIdentities.filter(
      (identity) =>
        identity.identityNumber !== selectedIdentity?.identityNumber,
    ),
  );

  const handleSignIn = async (identityNumber: bigint) => {
    try {
      isAuthenticating = true;
      switchingToIdentity = identityNumber;
      if ($authenticationStore?.identityNumber !== identityNumber) {
        // Switch sign in if not authenticated with this identity yet
        sessionStore.reset();
        await authLastUsedFlow.authenticate(
          $lastUsedIdentitiesStore.identities[`${identityNumber}`],
        );
      }
      lastUsedIdentitiesStore.selectIdentity(identityNumber);
      await preloadData(next);
      await goto(next, { replaceState: true });
    } finally {
      isAuthDialogOpen = false;
      isCreateIdentityDialogOpen = false;
      signUpOpenedFromSignInModal = false;
      isAuthenticating = false;
      switchingToIdentity = undefined;
    }
  };
  // Direct invocation path used by the welcome-back state. The AuthWizard
  // path catches errors itself; here we surface them ourselves.
  const handleSwitchIdentity = async (identityNumber: bigint) => {
    try {
      await handleSignIn(identityNumber);
    } catch (error) {
      handleError(error);
    }
  };
  const handleUpgrade = async (identityNumber: bigint) => {
    await handleSignIn(identityNumber);
    toaster.success({
      title: $t`Upgrade completed successfully`,
      duration: 4000,
    });
  };
  const handleSignUp = async (identityNumber: bigint) => {
    await handleSignIn(identityNumber);
    toaster.success({
      title: $t`You're all set. Your identity has been created.`,
      duration: 2000,
    });
  };
  const handleRemoveIdentity = (identityNumber: bigint) => {
    // If the removed identity is currently selected,
    // switch to the next available one in the list.
    //
    // The last identity cannot be removed, so there will
    // always be at least one next identity available.
    const isCurrent = selectedIdentity?.identityNumber === identityNumber;
    if (isCurrent) {
      const nextIdentity = lastUsedIdentities.find(
        (identity) => identity.identityNumber !== identityNumber,
      );
      if (nextIdentity !== undefined) {
        lastUsedIdentitiesStore.selectIdentity(nextIdentity.identityNumber);
      }
    }

    const removedIdentity =
      $lastUsedIdentitiesStore.identities[`${identityNumber}`];
    lastUsedIdentitiesStore.removeIdentity(identityNumber);

    isManageIdentitiesDialogOpen = false;
    if (removedIdentity !== undefined) {
      const identityName =
        removedIdentity.name ?? `${removedIdentity.identityNumber}`;
      toaster.create({
        title: $t`Identity removed`,
        description: $t`${identityName} has been removed from this device.`,
        closable: true,
        duration: 5000,
        action: {
          label: $t`Undo`,
          onClick: () => {
            lastUsedIdentitiesStore.restoreIdentity(removedIdentity);
            if (isCurrent) {
              lastUsedIdentitiesStore.selectIdentity(
                removedIdentity.identityNumber,
              );
            }
          },
        },
      });
    }
  };

  // When another page (e.g. `/login`) redirects here with a `next`
  // target in `page.state.login`, capture it so post-auth navigation
  // returns the user where they were headed.
  afterNavigate(() => {
    if ("login" in page.state && typeof page.state.login === "string") {
      next = page.state.login;
    }
  });

  // Pre-fetch passkey credential ids
  $effect(() =>
    authLastUsedFlow.init(
      lastUsedIdentities.map(({ identityNumber }) => identityNumber),
    ),
  );

  // Open authentication funnel once started
  $effect(() => {
    if (!isAuthDialogOpen) {
      return;
    }
    authenticationV2Funnel.init();
  });

  // Close authentication funnel once completed
  beforeNavigate((navigation) => {
    if (navigation.to?.url.pathname.startsWith("/manage") !== true) {
      return;
    }
    authenticationV2Funnel.trigger(AuthenticationV2Events.GoToDashboard);
    authenticationV2Funnel.close();
  });

  onMount(async () => {
    // Add rerouting back on this SSG route
    const rerouted = await manuallyReroute();
    // Track page view for landing page unless we have rerouted,
    // in which case it will be tracked on the destination page.
    if (!rerouted) {
      analytics.pageView();
    }
  });
</script>

<div class="flex min-h-dvh flex-col">
  <div class="h-[env(safe-area-inset-top)]"></div>
  <header
    class="relative z-10 flex h-16 flex-row items-center px-4 md:px-6 lg:px-8"
  >
    <a href="/" dir="ltr" aria-label={$t`Internet Identity`}>
      <Logo class="text-fg-primary h-5.5" />
    </a>
  </header>

  {#if browser}
    <div class="fade-in absolute inset-0 -z-1 hidden opacity-0 md:block">
      <div
        class={[
          "absolute inset-e-0 top-0 h-screen w-[50vw] opacity-50",
          // Fade dots from the left so they lead in from the content
          // edge; built-in `vignette` is radial and fades the corners
          // too, which we don't want here.
          "mask-[linear-gradient(to_right,transparent_0%,black_80%),linear-gradient(to_bottom,transparent_0%,black_30%),linear-gradient(to_top,transparent_0%,black_30%)] mask-intersect",
          "rtl:mask-[linear-gradient(to_left,transparent_0%,black_80%),linear-gradient(to_bottom,transparent_0%,black_30%),linear-gradient(to_top,transparent_0%,black_30%)]",
        ]}
      >
        <FlairCanvas
          spacing="small"
          aspect="wide"
          dotSize="medium"
          vignette="none"
          visibility="always"
          enableRandomPointSize
          enableRandomOpacity={false}
          pointSizeNoiseScale="medium"
          pointSizeNoiseMultiplier="medium"
          springOrTween={{
            type: "spring",
            stiffness: "high",
            dampening: "medium",
          }}
        />
      </div>
    </div>
    <main class="mx-auto flex w-full max-w-7xl flex-1 flex-col md:flex-row">
      <div
        class="fade-in flex flex-1 items-end px-4 pb-12 opacity-0 md:flex-3 md:items-start md:px-12 md:pb-0 lg:px-16"
      >
        <div class="w-full max-w-120">
          {#if selectedIdentity !== undefined}
            <!-- Welcome-back state -->
            <div class="md:pt-[max(0px,calc(50dvh-16rem))]">
              <h1
                class="text-text-primary mb-4 text-4xl font-medium tracking-tight text-balance md:text-[40px] md:leading-[1.1]"
              >
                {$t`Manage your Identity`}
              </h1>
              <p
                class="text-text-tertiary mb-8 max-w-md text-base text-balance md:text-lg"
              >
                <Trans>Keep your access and recovery methods up to date.</Trans>
              </p>

              <div
                class="border-border-secondary flex flex-row flex-wrap items-center gap-4 rounded-md border p-4"
              >
                <IdentityAvatar identity={selectedIdentity} size="lg" />
                <div class="flex min-w-0 flex-1 flex-col overflow-hidden">
                  <span
                    class="text-text-primary overflow-hidden text-sm font-semibold text-ellipsis whitespace-nowrap"
                  >
                    {selectedIdentity.name ?? selectedIdentity.identityNumber}
                  </span>
                  <span
                    class="text-text-tertiary overflow-hidden text-sm text-ellipsis whitespace-nowrap"
                  >
                    {#if "openid" in selectedIdentity.authMethod && selectedIdentity.authMethod.openid.metadata !== undefined}
                      {getMetadataString(
                        selectedIdentity.authMethod.openid.metadata,
                        "email",
                      ) ?? $t`Hidden email`}
                    {:else if "sso" in selectedIdentity.authMethod}
                      {@const sso = selectedIdentity.authMethod.sso}
                      {sso.email ?? sso.name ?? sso.domain}
                    {:else}
                      {$t`Passkey`}
                    {/if}
                  </span>
                </div>
                <button
                  onclick={() =>
                    handleSwitchIdentity(selectedIdentity.identityNumber)}
                  disabled={isAuthenticating}
                  class="btn btn-primary order-last w-full gap-2 md:order-0 md:ms-auto md:w-auto md:shrink-0"
                >
                  {$t`Continue`}
                  {#if switchingToIdentity === selectedIdentity.identityNumber}
                    <ProgressRing class="size-4" />
                  {:else}
                    <ArrowRightIcon class="size-4 rtl:-scale-x-100" />
                  {/if}
                </button>
              </div>
            </div>

            {#if otherIdentities.length > 0}
              <div class="mt-10 flex flex-col">
                <div class="mb-2 flex h-9 flex-row items-center ps-1">
                  <h2 class="text-text-tertiary text-sm font-semibold">
                    {$t`Or sign in with another identity`}
                  </h2>
                  <button
                    onclick={() => (isManageIdentitiesDialogOpen = true)}
                    disabled={isAuthenticating}
                    class="btn btn-tertiary btn-icon btn-sm ms-auto rounded-full"
                  >
                    <PencilIcon class="size-4" />
                    <span>{$t`Edit identities`}</span>
                  </button>
                </div>
                <ul class="flex flex-col gap-2">
                  {#each otherIdentities as identity (identity.identityNumber)}
                    <li>
                      <button
                        onclick={() =>
                          handleSwitchIdentity(identity.identityNumber)}
                        disabled={isAuthenticating}
                        class={[
                          "group flex w-full flex-row items-center gap-3 p-3 text-start",
                          "border-border-secondary rounded-md border outline-none",
                          "enabled:hover:bg-bg-primary_hover enabled:focus-visible:bg-bg-primary_hover",
                          "disabled:border-border-disabled",
                        ]}
                      >
                        <IdentityListItem {identity} />
                        {#if switchingToIdentity === identity.identityNumber}
                          <ProgressRing
                            class="text-fg-disabled ms-auto size-5"
                          />
                        {:else}
                          <ArrowRightIcon
                            class={[
                              "text-fg-tertiary ms-auto me-3 size-5 transform opacity-0 transition-all duration-200 rtl:-scale-x-100",
                              "group-enabled:group-hover:me-2 group-enabled:group-hover:opacity-100",
                              "group-enabled:group-focus-visible:me-0 group-enabled:group-focus-visible:opacity-100",
                            ]}
                          />
                        {/if}
                      </button>
                    </li>
                  {/each}
                </ul>
              </div>
            {/if}

            <button
              onclick={() => (isAuthDialogOpen = true)}
              disabled={isAuthenticating}
              class="btn btn-tertiary mt-6 gap-2"
            >
              <PlusIcon class="size-4" />
              {$t`Add identity`}
            </button>
          {:else}
            <!-- Sign-up state -->
            <div class="md:pt-[max(0px,calc(50dvh-16rem))]">
              <h1
                class="text-text-primary mb-4 text-4xl font-medium tracking-tight text-balance md:text-[40px] md:leading-[1.1]"
              >
                {$t`Manage your Identity`}
              </h1>
              <p
                class="text-text-tertiary mb-8 max-w-md text-base text-balance md:text-lg"
              >
                <Trans>Keep your access and recovery methods up to date.</Trans>
              </p>
              <AuthWizard
                onSignIn={handleSignIn}
                onSignUp={handleSignUp}
                onUpgrade={handleUpgrade}
                onError={(error) => {
                  isAuthenticating = false;
                  handleError(error);
                }}
                onOpenIdNotConnected={(args) => (notConnectedPayload = args)}
                onMethodSwitch={(args) => (methodSwitchPayload = args)}
                onSwitchMode={() => {
                  signUpOpenedFromSignInModal = false;
                  isCreateIdentityDialogOpen = true;
                }}
                withinDialog={false}
                mode="signin"
              />
            </div>
          {/if}
        </div>
      </div>
    </main>
  {/if}
  <Footer class="mt-auto" />
  <div class="h-[env(safe-area-inset-bottom)]"></div>
</div>

{#if isAuthDialogOpen}
  <Dialog
    onClose={() => {
      if (isAuthenticating) {
        return;
      }
      isAuthDialogOpen = false;
    }}
  >
    <AuthWizard
      onSignIn={handleSignIn}
      onSignUp={handleSignUp}
      onUpgrade={handleUpgrade}
      onError={(error) => {
        isAuthDialogOpen = false;
        isAuthenticating = false;
        handleError(error);
      }}
      onOpenIdNotConnected={(args) => (notConnectedPayload = args)}
      onMethodSwitch={(args) => (methodSwitchPayload = args)}
      onSwitchMode={() => {
        signUpOpenedFromSignInModal = true;
        isAuthDialogOpen = false;
        isCreateIdentityDialogOpen = true;
      }}
      withinDialog={true}
      mode="signin"
    >
      <h1 class="text-text-primary my-2 self-start text-2xl font-medium">
        {$t`Sign in`}
      </h1>
      <p class="text-text-secondary mb-6 self-start text-sm">
        {$t`Choose method to continue`}
      </p>
    </AuthWizard>
  </Dialog>
{/if}

{#if isCreateIdentityDialogOpen}
  <Dialog
    onClose={() => {
      if (isAuthenticating) {
        return;
      }
      isCreateIdentityDialogOpen = false;
      signUpOpenedFromSignInModal = false;
    }}
  >
    <AuthWizard
      onSignIn={handleSignIn}
      onSignUp={handleSignUp}
      onUpgrade={handleUpgrade}
      onError={(error) => {
        isCreateIdentityDialogOpen = false;
        isAuthenticating = false;
        handleError(error);
      }}
      onMethodSwitch={(args) => (methodSwitchPayload = args)}
      onOpenIdAlreadyLinked={(args) => (alreadyLinkedPayload = args)}
      onSwitchMode={lastUsedIdentities.length > 0
        ? () => {
            isCreateIdentityDialogOpen = false;
            if (signUpOpenedFromSignInModal) {
              signUpOpenedFromSignInModal = false;
              isAuthDialogOpen = true;
            }
          }
        : undefined}
      withinDialog={true}
      mode="signup"
    >
      <SignUpHero />
    </AuthWizard>
  </Dialog>
{/if}

{#if isManageIdentitiesDialogOpen}
  <Dialog onClose={() => (isManageIdentitiesDialogOpen = false)}>
    <ManageIdentities
      identities={lastUsedIdentities}
      onRemoveIdentity={handleRemoveIdentity}
    />
  </Dialog>
{/if}

{#if notConnectedPayload !== undefined}
  {@const payload = notConnectedPayload}
  <Dialog
    onClose={() => {
      if (isAuthenticating) {
        return;
      }
      const cancel = payload.cancel;
      notConnectedPayload = undefined;
      cancel();
    }}
  >
    <IdentityNotConnected
      providerName={payload.providerName}
      providerLogo={payload.providerLogo}
      userName={payload.userName ?? payload.userEmail ?? payload.providerName}
      userEmail={payload.userName !== undefined ? payload.userEmail : undefined}
      onSignUp={() => {
        const resume = payload.resume;
        notConnectedPayload = undefined;
        void resume();
      }}
      onRecover={() => {
        const cancel = payload.cancel;
        notConnectedPayload = undefined;
        cancel();
        void goto("/recovery");
      }}
    />
  </Dialog>
{/if}

{#if alreadyLinkedPayload !== undefined}
  {@const payload = alreadyLinkedPayload}
  <Dialog
    onClose={() => {
      if (isAuthenticating) {
        return;
      }
      const cancel = payload.cancel;
      alreadyLinkedPayload = undefined;
      cancel();
    }}
  >
    <IdentityAlreadyLinked
      providerName={payload.providerName}
      providerLogo={payload.providerLogo}
      userName={payload.userName ?? payload.userEmail ?? payload.providerName}
      userEmail={payload.userName !== undefined ? payload.userEmail : undefined}
      onSignIn={() => {
        const signIn = payload.signIn;
        alreadyLinkedPayload = undefined;
        void signIn();
      }}
    />
  </Dialog>
{/if}

{#if methodSwitchPayload !== undefined}
  {@const payload = methodSwitchPayload}
  {@const previous = payload.previous}
  {@const previousEmail =
    "openid" in previous.authMethod &&
    previous.authMethod.openid.metadata !== undefined
      ? getMetadataString(previous.authMethod.openid.metadata, "email")
      : "sso" in previous.authMethod
        ? previous.authMethod.sso.email
        : undefined}
  <Dialog onClose={() => (methodSwitchPayload = undefined)}>
    <SwitchAccessMethod
      userName={previous.name ?? previousEmail ?? `${previous.identityNumber}`}
      userEmail={previous.name !== undefined ? previousEmail : undefined}
      fromMethod={authMethodToAccessMethod(previous.authMethod)}
      toMethod={payload.newProvider}
      onSwitch={() => {
        const proceed = payload.proceed;
        methodSwitchPayload = undefined;
        void proceed();
      }}
    />
  </Dialog>
{/if}

<style>
  .fade-in {
    animation: fadeIn 0.6s ease-in 0.2s forwards;
  }

  @keyframes fadeIn {
    from {
      opacity: 0;
    }
    to {
      opacity: 1;
    }
  }
</style>
