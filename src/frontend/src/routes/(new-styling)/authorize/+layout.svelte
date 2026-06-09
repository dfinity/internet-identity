<script lang="ts">
  import type { LayoutProps } from "./$types";
  import { channelErrorStore, channelStore } from "$lib/stores/channelStore";
  import {
    authorizationContextStore,
    authorizationStore,
    authorizedStore,
  } from "$lib/stores/authorization.store";
  import { lastUsedIdentitiesStore } from "$lib/stores/last-used-identities.store";
  import {
    authenticationStore,
    type Authenticated,
  } from "$lib/stores/authentication.store";
  import { goto } from "$app/navigation";
  import { toaster } from "$lib/components/utils/toaster";
  import { handleError } from "$lib/components/utils/error";
  import { sessionStore } from "$lib/stores/session.store";
  import { t } from "$lib/stores/locale.store";
  import { onMount } from "svelte";
  import { analytics } from "$lib/utils/analytics/analytics";
  import { throwCanisterError } from "$lib/utils/utils";
  import { get } from "svelte/store";
  import { AuthLastUsedFlow } from "$lib/flows/authLastUsedFlow.svelte";
  import {
    AuthWizard,
    SignUpHero,
    IdentityNotConnected,
    IdentityAlreadyLinked,
    SwitchAccessMethod,
  } from "$lib/components/wizards/auth";
  import type { AccessMethod } from "$lib/components/wizards/auth/views/SwitchAccessMethod.svelte";
  import type { LastUsedIdentity } from "$lib/stores/last-used-identities.store";
  import { backendCanisterConfig } from "$lib/globals";
  import { getMetadataString } from "$lib/utils/openID";
  import {
    HANDOFF_HASH_KEY,
    generateHandoffNonce,
    sendAuthToOpenedTab,
  } from "$lib/utils/auth-handoff";
  import ChannelError from "$lib/components/ui/ChannelError.svelte";
  import Header from "$lib/components/layout/Header.svelte";
  import Footer from "$lib/components/layout/Footer.svelte";
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import Popover from "$lib/components/ui/Popover.svelte";
  import IdentitySwitcher from "$lib/components/ui/IdentitySwitcher.svelte";
  import ManageIdentities from "$lib/components/ui/ManageIdentities.svelte";
  import Avatar from "$lib/components/ui/Avatar.svelte";
  import { ChevronDownIcon, ExternalLinkIcon, UserIcon } from "@lucide/svelte";

  const { children }: LayoutProps = $props();

  // --- Flow detection (captured once, not reactive to URL changes) ---
  const flow = (() => {
    const url = new URL(window.location.href);
    const hasOpenid = url.searchParams.has("openid");
    const hasSso = url.searchParams.has("sso");
    if (hasOpenid && hasSso) {
      // `?openid=` and `?sso=` pick mutually exclusive 1-click entry
      // points; combining them is a misconfigured sign-in URL. Render
      // the channel-error view directly without trying to establish a
      // channel — the dapp side hasn't picked an entry point either.
      return "error" as const;
    }
    if (hasOpenid) {
      return "openid-init" as const;
    }
    if (hasSso) {
      return "sso-init" as const;
    }
    if (url.searchParams.get("flow") === "openid-resume") {
      return "openid-resume" as const;
    }
    return "normal" as const;
  })();

  // --- Channel establishment ---
  $effect.pre(() => {
    if (flow === "error") {
      channelErrorStore.set("invalid-request");
      return;
    }
    if (flow === "openid-init" || flow === "sso-init") {
      // Same lifecycle as direct OpenID 1-click: the page is going to
      // redirect away to the IdP and come back, so the channel is
      // established in `pending` mode and resumed via
      // `ii-pending-channel-origin` after the JWT round-trip.
      channelStore.establish({ pending: true });
    } else if (flow === "openid-resume") {
      const pendingOrigin = sessionStorage.getItem("ii-pending-channel-origin");
      if (pendingOrigin !== null) {
        channelStore.establish({ allowedOrigin: pendingOrigin });
      } else {
        channelErrorStore.set("connection-closed");
      }
    } else {
      channelStore.establish();
    }
  });

  // --- Unsupported browser redirect ---
  $effect(() => {
    if ($channelErrorStore === "unsupported-browser") {
      void goto("/unsupported");
    }
  });

  // --- Rendering gates ---
  const hasError = $derived(
    $channelErrorStore !== undefined &&
      $channelErrorStore !== "unsupported-browser",
  );
  const isReady = $derived.by(() => {
    if (hasError) {
      return false;
    }
    if (flow === "normal") {
      return $authorizationStore !== undefined;
    }
    // OpenID/SSO 1-click flows gate on channel establishment
    return $channelStore !== undefined;
  });
  const showHeaderFooter = $derived(
    isReady &&
      $authorizedStore === undefined &&
      flow !== "openid-init" &&
      flow !== "sso-init" &&
      flow !== "openid-resume",
  );

  // --- Identity switcher state ---
  const authLastUsedFlow = new AuthLastUsedFlow();

  const lastUsedIdentities = $derived(
    Object.values($lastUsedIdentitiesStore.identities).sort(
      (a, b) => b.lastUsedTimestampMillis - a.lastUsedTimestampMillis,
    ),
  );
  const selectedIdentity = $derived($lastUsedIdentitiesStore.selected);

  let identityButtonRef = $state<HTMLElement>();
  let isIdentityPopoverOpen = $state(false);
  let isAuthDialogOpen = $state(false);
  let isCreateIdentityDialogOpen = $state(false);
  let isAuthenticating = $state(false);
  let isManageIdentitiesDialogOpen = $state(false);
  let pendingHandoff: { cancel: () => void } | undefined;
  // Set when the user clicked manage but had to authenticate first — the
  // passkey/IdP prompt consumes the click's transient activation on Safari
  // (and strict Firefox), so a follow-up window.open() would be silently
  // blocked. The confirmation dialog's own button click provides fresh
  // activation and drives window.open from there.
  let pendingManageOpen = $state<{
    nonce: string;
    auth: Omit<Authenticated, "agent" | "actor" | "salt" | "nonce">;
  }>();
  let signUpOpenedFromSignInModal = $state(false);

  let notConnectedPayload = $state<{
    providerName: string;
    providerLogo?: string;
    userName?: string;
    userEmail?: string;
    resume: () => Promise<void>;
    cancel: () => void;
  }>();
  let isResumingRegistration = $state(false);
  let alreadyLinkedPayload = $state<{
    providerName: string;
    providerLogo?: string;
    userName?: string;
    userEmail?: string;
    signIn: () => Promise<void>;
    cancel: () => void;
  }>();
  let isSigningInAlreadyLinked = $state(false);
  let methodSwitchPayload = $state<{
    previous: LastUsedIdentity;
    newProvider: AccessMethod;
    proceed: () => Promise<void>;
  }>();

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

  const handleSignIn = async (identityNumber: bigint) => {
    try {
      isAuthenticating = true;
      if ($authenticationStore?.identityNumber !== identityNumber) {
        sessionStore.reset();
        await authLastUsedFlow.authenticate(
          $lastUsedIdentitiesStore.identities[`${identityNumber}`],
        );
      }
      lastUsedIdentitiesStore.selectIdentity(identityNumber);
    } finally {
      isIdentityPopoverOpen = false;
      isAuthDialogOpen = false;
      isCreateIdentityDialogOpen = false;
      signUpOpenedFromSignInModal = false;
      isAuthenticating = false;
    }
  };
  const handleSignUp = async (identityNumber: bigint) => {
    await handleSignIn(identityNumber);
    toaster.success({
      title: $t`You're all set. Your identity has been created.`,
      duration: 4000,
    });
  };
  const handleUpgrade = async (identityNumber: bigint) => {
    await handleSignIn(identityNumber);
  };
  const handleRemoveIdentity = (identityNumber: bigint) => {
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
  const authorizeDefault = async () => {
    try {
      const { identityNumber, actor } = $authenticationStore!;
      const origin = $authorizationContextStore.effectiveOrigin;
      const accountNumber = await actor
        .get_default_account(identityNumber, origin)
        .then(throwCanisterError)
        .then((account) => account.account_number[0]);
      authorizationStore.authorize(Promise.resolve(accountNumber));
    } catch (error) {
      handleError(error);
    }
  };

  // Pre-fetch passkey credential ids
  $effect(() =>
    authLastUsedFlow.init(
      lastUsedIdentities.map(({ identityNumber }) => identityNumber),
    ),
  );

  // Track page view for authorization flow
  onMount(() => {
    analytics.pageView();
  });
</script>

{#if hasError}
  <ChannelError error={$channelErrorStore!} />
{:else if isReady}
  <div class="flex min-h-[100dvh] flex-col" data-page="new-authorize-view">
    {#if showHeaderFooter}
      <div class="h-[env(safe-area-inset-top)]"></div>
      <Header>
        {#if selectedIdentity !== undefined}
          <button
            bind:this={identityButtonRef}
            class="btn btn-tertiary ms-auto gap-2.5 pe-3 md:-me-3"
            onclick={() => (isIdentityPopoverOpen = true)}
            aria-label="Switch identity"
          >
            <Avatar size="xs">
              <UserIcon class="size-4" />
            </Avatar>
            <span
              >{selectedIdentity.name ?? selectedIdentity.identityNumber}</span
            >
            <ChevronDownIcon class="size-4" />
          </button>
          {#if isIdentityPopoverOpen}
            <Popover
              anchor={identityButtonRef}
              onClose={() => {
                if (isAuthenticating) {
                  return;
                }
                isIdentityPopoverOpen = false;
              }}
              direction="down"
              align="end"
              distance="0.75rem"
              class="!bg-bg-primary"
            >
              <IdentitySwitcher
                selected={selectedIdentity.identityNumber}
                identities={lastUsedIdentities}
                onSwitchIdentity={async (identityNumber) => {
                  await handleSignIn(identityNumber);
                  await authorizeDefault();
                }}
                onUseAnotherIdentity={() => {
                  isIdentityPopoverOpen = false;
                  isAuthDialogOpen = true;
                }}
                onManageIdentity={async (): Promise<void> => {
                  isIdentityPopoverOpen = false;
                  if (selectedIdentity === undefined) return;
                  try {
                    isAuthenticating = true;
                    const needsAuth =
                      $authenticationStore?.identityNumber !==
                      selectedIdentity.identityNumber;
                    if (needsAuth) {
                      sessionStore.reset();
                      await authLastUsedFlow.authenticate(
                        $lastUsedIdentitiesStore.identities[
                          `${selectedIdentity.identityNumber}`
                        ],
                      );
                    }
                    const auth = get(authenticationStore);
                    if (auth === undefined) {
                      await goto("/manage");
                      return;
                    }
                    const nonce = generateHandoffNonce();
                    if (needsAuth) {
                      // The just-completed passkey/IdP prompt consumed the
                      // click's transient activation on Safari/strict
                      // Firefox, so window.open() here would be silently
                      // blocked. Surface the confirmation dialog and let
                      // its own button click drive window.open with fresh
                      // activation. When the user was already signed in we
                      // never awaited anything, so the popup goes straight
                      // through.
                      pendingManageOpen = { nonce, auth };
                      return;
                    }
                    const w = window.open(
                      `/manage#${HANDOFF_HASH_KEY}=${encodeURIComponent(nonce)}`,
                      "_blank",
                    );
                    if (w === null) {
                      await goto("/manage");
                      return;
                    }
                    pendingHandoff?.cancel();
                    pendingHandoff = sendAuthToOpenedTab(w, auth, nonce);
                  } catch (error) {
                    handleError(error);
                  } finally {
                    isAuthenticating = false;
                  }
                }}
                onManageIdentities={() => {
                  isIdentityPopoverOpen = false;
                  isManageIdentitiesDialogOpen = true;
                }}
                onError={(error) => {
                  isIdentityPopoverOpen = false;
                  handleError(error);
                }}
                onClose={() => (isIdentityPopoverOpen = false)}
              />
            </Popover>
          {/if}
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
                withinDialog
                mode="signin"
              >
                <h1
                  class="text-text-primary my-2 self-start text-2xl font-medium"
                >
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
                onSwitchMode={() => {
                  isCreateIdentityDialogOpen = false;
                  if (signUpOpenedFromSignInModal) {
                    signUpOpenedFromSignInModal = false;
                    isAuthDialogOpen = true;
                  }
                }}
                withinDialog
                mode="signup"
              >
                <SignUpHero />
              </AuthWizard>
            </Dialog>
          {/if}
        {/if}
      </Header>
    {/if}
    <div class="flex flex-1 flex-col items-center justify-center">
      {@render children()}
    </div>
    {#if showHeaderFooter}
      <Footer />
      <div class="h-[env(safe-area-inset-bottom)]"></div>
    {/if}
  </div>

  {#if isManageIdentitiesDialogOpen}
    <Dialog onClose={() => (isManageIdentitiesDialogOpen = false)}>
      <ManageIdentities
        identities={lastUsedIdentities}
        onRemoveIdentity={handleRemoveIdentity}
      />
    </Dialog>
  {/if}

  {#if pendingManageOpen !== undefined}
    {@const pending = pendingManageOpen}
    <Dialog onClose={() => (pendingManageOpen = undefined)}>
      <div class="flex flex-col">
        <h2 class="text-text-primary my-2 self-start text-2xl font-medium">
          {$t`You're signed in`}
        </h2>
        <p class="text-text-secondary mb-6 self-start text-sm">
          {$t`Open Internet Identity in a new tab to manage your access methods and recovery options.`}
        </p>
        <button
          class="btn btn-primary btn-lg w-full gap-2"
          onclick={() => {
            // {@const pending = pendingManageOpen} is reactive — once we
            // assign pendingManageOpen = undefined, `pending.auth` would
            // resolve through the live state proxy and throw. Snapshot
            // to locals before mutating.
            const nonce = pending.nonce;
            const auth = pending.auth;
            const w = window.open(
              `/manage#${HANDOFF_HASH_KEY}=${encodeURIComponent(nonce)}`,
              "_blank",
            );
            if (w === null) {
              pendingManageOpen = undefined;
              void goto("/manage");
              return;
            }
            pendingHandoff?.cancel();
            pendingHandoff = sendAuthToOpenedTab(w, auth, nonce);
            pendingManageOpen = undefined;
          }}
        >
          <ExternalLinkIcon class="size-4" aria-hidden="true" />
          {$t`Open manage`}
        </button>
      </div>
    </Dialog>
  {/if}

  {#if notConnectedPayload !== undefined}
    {@const payload = notConnectedPayload}
    <Dialog
      onClose={() => {
        if (isAuthenticating || isResumingRegistration) {
          return;
        }
        payload.cancel();
        notConnectedPayload = undefined;
      }}
    >
      <IdentityNotConnected
        providerName={payload.providerName}
        providerLogo={payload.providerLogo}
        userName={payload.userName ?? payload.userEmail ?? payload.providerName}
        userEmail={payload.userName !== undefined
          ? payload.userEmail
          : undefined}
        loading={isResumingRegistration}
        onSignUp={async () => {
          isResumingRegistration = true;
          try {
            await payload.resume();
          } finally {
            isResumingRegistration = false;
            notConnectedPayload = undefined;
          }
        }}
        onRecover={() => {
          payload.cancel();
          notConnectedPayload = undefined;
          void goto("/recovery");
        }}
      />
    </Dialog>
  {/if}

  {#if alreadyLinkedPayload !== undefined}
    {@const payload = alreadyLinkedPayload}
    <Dialog
      onClose={() => {
        if (isAuthenticating || isSigningInAlreadyLinked) {
          return;
        }
        payload.cancel();
        alreadyLinkedPayload = undefined;
      }}
    >
      <IdentityAlreadyLinked
        providerName={payload.providerName}
        providerLogo={payload.providerLogo}
        userName={payload.userName ?? payload.userEmail ?? payload.providerName}
        userEmail={payload.userName !== undefined
          ? payload.userEmail
          : undefined}
        loading={isSigningInAlreadyLinked}
        onSignIn={async () => {
          isSigningInAlreadyLinked = true;
          try {
            await payload.signIn();
          } finally {
            isSigningInAlreadyLinked = false;
            alreadyLinkedPayload = undefined;
          }
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
    <Dialog
      onClose={() => {
        const proceed = payload.proceed;
        methodSwitchPayload = undefined;
        void proceed();
      }}
    >
      <SwitchAccessMethod
        userName={previous.name ??
          previousEmail ??
          `${previous.identityNumber}`}
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
{/if}
