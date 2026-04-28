<script lang="ts">
  import type { LayoutProps } from "./$types";
  import { channelErrorStore, channelStore } from "$lib/stores/channelStore";
  import {
    authorizationContextStore,
    authorizationStore,
    authorizedStore,
  } from "$lib/stores/authorization.store";
  import { lastUsedIdentitiesStore } from "$lib/stores/last-used-identities.store";
  import { authenticationStore } from "$lib/stores/authentication.store";
  import { goto } from "$app/navigation";
  import { toaster } from "$lib/components/utils/toaster";
  import { handleError } from "$lib/components/utils/error";
  import { sessionStore } from "$lib/stores/session.store";
  import { t } from "$lib/stores/locale.store";
  import { onMount } from "svelte";
  import { analytics } from "$lib/utils/analytics/analytics";
  import { throwCanisterError } from "$lib/utils/utils";
  import { AuthLastUsedFlow } from "$lib/flows/authLastUsedFlow.svelte";
  import { AuthWizard } from "$lib/components/wizards/auth";
  import ChannelError from "$lib/components/ui/ChannelError.svelte";
  import Header from "$lib/components/layout/Header.svelte";
  import Footer from "$lib/components/layout/Footer.svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import Popover from "$lib/components/ui/Popover.svelte";
  import IdentitySwitcher from "$lib/components/ui/IdentitySwitcher.svelte";
  import ManageIdentities from "$lib/components/ui/ManageIdentities.svelte";
  import Avatar from "$lib/components/ui/Avatar.svelte";
  import { ChevronDownIcon, UserIcon } from "@lucide/svelte";

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
  let isAuthenticating = $state(false);
  let isManageIdentitiesDialogOpen = $state(false);

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
          <Button
            bind:element={identityButtonRef}
            onclick={() => (isIdentityPopoverOpen = true)}
            variant="tertiary"
            class="ms-auto gap-2.5 pe-3 md:-me-3"
            aria-label="Switch identity"
          >
            <Avatar size="xs">
              <UserIcon class="size-4" />
            </Avatar>
            <span
              >{selectedIdentity.name ?? selectedIdentity.identityNumber}</span
            >
            <ChevronDownIcon class="size-4" />
          </Button>
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
                onManageIdentity={(): Promise<void> => {
                  isIdentityPopoverOpen = false;
                  window.open("/manage", "_blank");
                  return Promise.resolve();
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
                  handleError(error);
                }}
                withinDialog
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
{/if}
