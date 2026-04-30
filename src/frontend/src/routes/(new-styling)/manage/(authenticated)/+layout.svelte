<script lang="ts">
  import {
    ChevronDownIcon,
    HouseIcon,
    KeyRoundIcon,
    MenuIcon,
    XIcon,
    LifeBuoyIcon,
    CodeIcon,
    LanguagesIcon,
    InfoIcon,
    UserIcon,
  } from "@lucide/svelte";
  import { page } from "$app/state";
  import { afterNavigate, goto } from "$app/navigation";
  import { onMount } from "svelte";
  import { analytics } from "$lib/utils/analytics/analytics";
  import {
    authenticatedStore,
    authenticationStore,
  } from "$lib/stores/authentication.store";
  import { lastUsedIdentitiesStore } from "$lib/stores/last-used-identities.store";
  import { sessionStore } from "$lib/stores/session.store";
  import { locales, localeStore, t } from "$lib/stores/locale.store";
  import { AuthLastUsedFlow } from "$lib/flows/authLastUsedFlow.svelte";
  import { handleError } from "$lib/components/utils/error";
  import { toaster } from "$lib/components/utils/toaster";
  import { getMetadataString } from "$lib/utils/openID";
  import { SOURCE_CODE_URL, SUPPORT_URL } from "$lib/config";
  import { Trans } from "$lib/components/locale";
  import type { LayoutProps } from "./$types";
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import Popover from "$lib/components/ui/Popover.svelte";
  import Logo from "$lib/components/ui/Logo.svelte";
  import NavItem from "$lib/components/ui/NavItem.svelte";
  import Avatar from "$lib/components/ui/Avatar.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import IdentitySwitcher from "$lib/components/ui/IdentitySwitcher.svelte";
  import ManageIdentities from "$lib/components/ui/ManageIdentities.svelte";
  import SignOutConfirmation from "$lib/components/ui/SignOutConfirmation.svelte";
  import ReauthPrompt from "$lib/components/ui/ReauthPrompt.svelte";
  import AuthWizard from "$lib/components/wizards/auth/AuthWizard.svelte";
  import ChooseLanguage from "$lib/components/views/ChooseLanguage.svelte";

  // --- Props & state ---

  const { children, data }: LayoutProps = $props();
  const authLastUsedFlow = new AuthLastUsedFlow();

  let identityButtonRef = $state<HTMLElement>();
  let isMobileSidebarOpen = $state(false);
  let isIdentityPopoverOpen = $state(false);
  let isAuthDialogOpen = $state(false);
  let isAuthenticating = $state(false);
  let isManageIdentitiesDialogOpen = $state(false);
  let isSignOutDialogOpen = $state(false);
  let isLanguageDialogOpen = $state(false);
  let isRecoveryPhraseSetUpDismissed = $state(false);
  let isReauthDialogOpen = $state(false);

  // --- Derived ---

  const lastUsedIdentities = $derived(
    Object.values($lastUsedIdentitiesStore.identities).sort(
      (a, b) => b.lastUsedTimestampMillis - a.lastUsedTimestampMillis,
    ),
  );

  let recoveryPhraseStatus: "missing" | "unverified" | "verified" = $derived.by(
    () => {
      const value = data.identityInfo.authn_methods.find(
        (m) =>
          "Recovery" in m.security_settings.purpose &&
          getMetadataString(m.metadata, "usage") === "recovery_phrase",
      );
      return value === undefined
        ? "missing"
        : value.last_authentication[0] === undefined
          ? "unverified"
          : "verified";
    },
  );

  // --- Sign in / sign up / upgrade ---

  const handleSignIn = async (identityNumber: bigint) => {
    try {
      isAuthenticating = true;
      if ($authenticationStore?.identityNumber !== identityNumber) {
        // Sign in if not authenticated with this identity yet
        sessionStore.reset();
        await authLastUsedFlow.authenticate(
          $lastUsedIdentitiesStore.identities[`${identityNumber}`],
        );
      }
      lastUsedIdentitiesStore.selectIdentity(identityNumber);
      await goto("/manage", { replaceState: true, invalidateAll: true });
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
      duration: 2000,
    });
  };

  const handleUpgrade = async (identityNumber: bigint) => {
    await handleSignIn(identityNumber);
    toaster.success({
      title: $t`Upgrade completed successfully`,
      duration: 4000,
    });
  };

  // --- Sign out ---

  const handleSignOut = (): Promise<void> => {
    isIdentityPopoverOpen = false;
    isSignOutDialogOpen = true;
    return Promise.resolve();
  };

  const handleConfirmSignOut = () => {
    window.location.replace("/");
  };

  const handleConfirmSignOutAndRemove = () => {
    const identity = $lastUsedIdentitiesStore.selected;
    if (identity !== undefined) {
      lastUsedIdentitiesStore.removeIdentity(identity.identityNumber);
    }
    window.location.replace("/");
  };

  // --- Manage identities ---

  const handleRemoveIdentity = (identityNumber: bigint) => {
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
          onClick: () =>
            lastUsedIdentitiesStore.restoreIdentity(removedIdentity),
        },
      });
    }
  };

  // --- Session re-authentication ---
  // Shows the re-auth dialog 5 min before delegation expiry.
  // If another dialog is open, waits for it to close — but force-shows at expiry.

  const REAUTH_BUFFER_MS = 5 * 60 * 1000;

  let reauthCleanup: (() => void) | undefined;

  const showReauthDialog = (expiryMs: number) => {
    if (document.querySelector("dialog[open]") === null) {
      isReauthDialogOpen = true;
      return;
    }
    const forceTimer = setTimeout(
      () => {
        observer.disconnect();
        reauthCleanup = undefined;
        isReauthDialogOpen = true;
      },
      Math.max(0, expiryMs - Date.now()),
    );
    const observer = new MutationObserver(() => {
      if (document.querySelector("dialog[open]") === null) {
        clearTimeout(forceTimer);
        observer.disconnect();
        reauthCleanup = undefined;
        isReauthDialogOpen = true;
      }
    });
    observer.observe(document.body, {
      childList: true,
      subtree: true,
      attributes: true,
      attributeFilter: ["open"],
    });
    reauthCleanup = () => {
      clearTimeout(forceTimer);
      observer.disconnect();
    };
  };

  const handleReauthenticate = async () => {
    const { identityNumber } = $authenticatedStore;
    const lastUsedIdentity =
      $lastUsedIdentitiesStore.identities[`${identityNumber}`];
    if (lastUsedIdentity === undefined) {
      handleConfirmSignOut();
      return;
    }
    sessionStore.reset();
    await authLastUsedFlow.authenticate(lastUsedIdentity);
    isReauthDialogOpen = false;
  };

  // --- Effects & lifecycle ---

  onMount(() => {
    analytics.pageView();
  });

  afterNavigate(() => {
    isMobileSidebarOpen = false;
  });

  // Pre-fetch passkey credential ids
  $effect(() =>
    authLastUsedFlow.init(
      lastUsedIdentities.map(({ identityNumber }) => identityNumber),
    ),
  );

  // Re-authentication timer
  $effect(() => {
    const authenticated = $authenticationStore;
    if (authenticated === undefined) {
      return;
    }
    let earliest = Infinity;
    for (const { delegation } of authenticated.identity.getDelegation()
      .delegations) {
      const expiryMs = Number(delegation.expiration / BigInt(1_000_000));
      if (expiryMs < earliest) {
        earliest = expiryMs;
      }
    }
    const delay = Math.max(0, earliest - REAUTH_BUFFER_MS - Date.now());
    const timer = setTimeout(() => showReauthDialog(earliest), delay);
    return () => {
      clearTimeout(timer);
      reauthCleanup?.();
      reauthCleanup = undefined;
    };
  });
</script>

{#snippet recoveryPhraseSetUp()}
  {#if recoveryPhraseStatus === "missing"}
    <div class="mb-4 grid size-16">
      <!-- Progress ring is actually only 85% of the way to
           make it more clear there's set-up work remaining -->
      <ProgressRing
        value={85}
        strokeWidth={5}
        class="col-start-1 row-start-1 size-16 text-blue-700 dark:text-blue-300"
      />
      <span
        class="text-text-primary col-start-1 row-start-1 m-auto text-sm font-semibold"
      >
        90%
      </span>
    </div>
    <h3 class="text-text-primary mb-1 text-sm font-semibold">
      {$t`Complete set-up`}
    </h3>
    <p class="text-text-secondary mb-4 text-sm">
      <Trans>
        Activate your recovery phrase so that you can recover your identity at
        any point.
      </Trans>
    </p>
    <div class="flex flex-row gap-3">
      <button
        onclick={() => (isRecoveryPhraseSetUpDismissed = true)}
        class="text-text-primary border-none text-sm font-semibold outline-none hover:underline focus-visible:underline"
      >
        {$t`Dismiss`}
      </button>
      <button
        onclick={() => goto("/manage/recovery", { state: { activate: true } })}
        class="text-text-primary border-none text-sm font-semibold outline-none hover:underline focus-visible:underline"
      >
        {$t`Activate`}
      </button>
    </div>
  {:else if recoveryPhraseStatus === "unverified"}
    <InfoIcon class="text-fg-secondary mb-3 size-5" />
    <h3 class="text-text-primary mb-1 text-sm font-semibold">
      {$t`Verify your recovery phrase`}
    </h3>
    <p class="text-text-secondary mb-4 text-sm">
      <Trans>
        Your recovery phrase is active, verify you saved it correctly.
      </Trans>
    </p>
    <div class="flex flex-row gap-3">
      <button
        onclick={() => (isRecoveryPhraseSetUpDismissed = true)}
        class="text-text-primary border-none text-sm font-semibold outline-none hover:underline focus-visible:underline"
      >
        {$t`Dismiss`}
      </button>
      <button
        onclick={() => goto("/manage/recovery", { state: { verify: true } })}
        class="text-text-primary border-none text-sm font-semibold outline-none hover:underline focus-visible:underline"
      >
        {$t`Verify`}
      </button>
    </div>
  {/if}
{/snippet}

<!-- Layout -->
<div class="bg-bg-primary_alt flex min-h-[100dvh] flex-row">
  <!-- Sidebar and backdrop on mobile -->
  <div
    onclick={() => (isMobileSidebarOpen = false)}
    class={[
      "bg-bg-overlay absolute inset-0 z-1 transition-opacity duration-200 ease-out sm:hidden",
      isMobileSidebarOpen ? "opacity-80" : "pointer-events-none opacity-0 ",
    ]}
    aria-hidden="true"
  ></div>
  <aside
    class={[
      "bg-bg-primary border-border-secondary flex w-74 flex-col sm:border-r sm:max-md:w-19",
      "max-sm:absolute max-sm:start-0 max-sm:end-20 max-sm:top-0 max-sm:bottom-0 max-sm:z-1 max-sm:w-auto max-sm:overflow-y-auto max-sm:transition-transform max-sm:duration-200 max-sm:ease-out",
      isMobileSidebarOpen
        ? "max-sm:translate-x-0"
        : "max-sm:pointer-events-none max-sm:translate-x-[-100%]",
    ]}
  >
    <div class="h-[env(safe-area-inset-top)]"></div>
    <!-- Mobile logo and close button -->
    <div
      class="mb-3 flex flex-row items-center justify-start px-4 py-3 sm:hidden"
    >
      <div dir="ltr" class="flex items-center gap-4">
        <Logo class="text-fg-primary h-4" />
        <div class="text-text-primary text-base font-semibold">
          Internet Identity
        </div>
      </div>
      <button
        onclick={() => (isMobileSidebarOpen = false)}
        class="btn btn-tertiary btn-icon ms-auto -me-1.5 sm:hidden"
        aria-label={$t`Close menu`}
      >
        <XIcon class="size-5" />
      </button>
    </div>
    <!-- Desktop logo -->
    <div
      class={[
        "mt-5 mb-6 flex h-6 flex-row items-center gap-4 max-md:justify-center md:px-7",
        "max-sm:hidden",
      ]}
    >
      <div dir="ltr" class="flex items-center gap-4">
        <Logo class="text-fg-primary h-4" />
        <div class="text-text-primary text-base font-semibold max-md:hidden">
          Internet Identity
        </div>
      </div>
    </div>
    <!-- Navigation -->
    <nav class="flex flex-col gap-0.5 px-4">
      <ul class="contents">
        <li class="contents">
          <NavItem href="/manage" current={page.url.pathname === "/manage"}>
            <HouseIcon class="size-5 sm:max-md:mx-auto" />
            <span class="sm:max-md:hidden">{$t`Home`}</span>
          </NavItem>
        </li>
        <li class="contents">
          <NavItem
            href="/manage/access"
            current={page.url.pathname === "/manage/access" ||
              page.url.pathname === "/manage/recovery"}
          >
            <KeyRoundIcon class="size-5 sm:max-md:mx-auto" />
            <span class="sm:max-md:hidden">{$t`Access and recovery`}</span>
          </NavItem>
        </li>
      </ul>
    </nav>
    <!-- Empty space between top and bottom content-->
    <div class="flex-1"></div>
    <!-- Recovery phrase set-up guidance -->
    <div
      class={[
        "mx-4 mt-24 mb-6",
        "bg-bg-secondary rounded-xl p-4",
        "sm:transition-all sm:transition-discrete sm:starting:scale-95 sm:starting:opacity-0",
        "sm:max-md:hidden",
        (recoveryPhraseStatus === "verified" ||
          isRecoveryPhraseSetUpDismissed) &&
          "hidden scale-90 opacity-0",
      ]}
    >
      {@render recoveryPhraseSetUp()}
    </div>
    <!-- Footer navigation -->
    <div class="mb-5 flex flex-col gap-0.5 px-4">
      <ul class="contents">
        <li class="contents">
          <NavItem onclick={() => (isLanguageDialogOpen = true)}>
            <LanguagesIcon class="size-5 sm:max-md:mx-auto" />
            <span class="uppercase sm:max-md:hidden">{$localeStore}</span>
          </NavItem>
        </li>
        <li class="contents">
          <NavItem href={SUPPORT_URL} target="_blank" rel="noopener">
            <LifeBuoyIcon class="size-5 sm:max-md:mx-auto" />
            <span class="sm:max-md:hidden">{$t`Support`}</span>
          </NavItem>
        </li>
        <li class="contents">
          <NavItem href={SOURCE_CODE_URL} target="_blank" rel="noopener">
            <CodeIcon class="size-5 sm:max-md:mx-auto" />
            <span class="sm:max-md:hidden">{$t`Source code`}</span>
          </NavItem>
        </li>
      </ul>
    </div>
    <div class="h-[env(safe-area-inset-bottom)]"></div>
  </aside>
  <!-- Main content -->
  <div class="relative flex flex-1 flex-col">
    <div class="h-[env(safe-area-inset-top)]"></div>
    <!-- Header -->
    <header class="flex h-16 flex-row items-center px-4 sm:px-8 md:px-12">
      <!-- Mobile menu button -->
      <div class="relative -ms-1.5 sm:hidden">
        <button
          onclick={() => (isMobileSidebarOpen = true)}
          class="btn btn-tertiary btn-icon"
          aria-label={$t`Open menu`}
        >
          <MenuIcon class="size-5" />
        </button>
        <!-- Indicator that there's a message in the mobile menu
             e.g. recovery phrase has not been set-up yet. -->
        <div
          class={[
            "border-bg-primary_alt absolute end-2 top-2 size-2 rounded-full border-2 bg-blue-700 dark:bg-blue-300",
            (recoveryPhraseStatus === "verified" ||
              isRecoveryPhraseSetUpDismissed) &&
              "hidden",
          ]}
        ></div>
      </div>
      <!-- Empty space between left and right content -->
      <div class="flex-1"></div>
      <!-- Identity button-->
      <button
        bind:this={identityButtonRef}
        onclick={() => (isIdentityPopoverOpen = true)}
        class="btn btn-tertiary gap-2.5 pe-3 max-sm:-me-2 sm:-me-3"
        aria-label={$t`Switch identity`}
      >
        <Avatar size="xs">
          <UserIcon class="size-4" />
        </Avatar>
        <span>
          {data.identityInfo.name[0] ?? data.identityNumber.toString()}
        </span>
        <ChevronDownIcon size="1rem" />
      </button>
    </header>
    <!-- Page content -->
    <main class="flex flex-col px-4 py-5 sm:px-8 sm:py-3 md:px-12">
      {@render children()}
    </main>
    <div class="h-[env(safe-area-inset-bottom)]"></div>
  </div>
</div>

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
      selected={$authenticatedStore.identityNumber}
      identities={lastUsedIdentities}
      onSwitchIdentity={handleSignIn}
      onUseAnotherIdentity={() => {
        isIdentityPopoverOpen = false;
        isAuthDialogOpen = true;
      }}
      onError={(error) => {
        isIdentityPopoverOpen = false;
        isAuthenticating = false;
        handleError(error);
      }}
      onManageIdentities={() => {
        isIdentityPopoverOpen = false;
        isManageIdentitiesDialogOpen = true;
      }}
      onClose={() => (isIdentityPopoverOpen = false)}
      onSignOut={handleSignOut}
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
      withinDialog
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

{#if isLanguageDialogOpen}
  <Dialog onClose={() => (isLanguageDialogOpen = false)}>
    <ChooseLanguage
      locales={$locales}
      value={$localeStore}
      onChange={(value) => {
        isLanguageDialogOpen = false;
        void localeStore.setOrReset(value);
      }}
    />
  </Dialog>
{/if}

{#if isManageIdentitiesDialogOpen}
  <Dialog onClose={() => (isManageIdentitiesDialogOpen = false)}>
    <ManageIdentities
      selected={$authenticatedStore.identityNumber}
      identities={lastUsedIdentities}
      onRemoveIdentity={handleRemoveIdentity}
    />
  </Dialog>
{/if}

{#if isSignOutDialogOpen && $lastUsedIdentitiesStore.selected !== undefined}
  <Dialog onClose={() => (isSignOutDialogOpen = false)}>
    <SignOutConfirmation
      identity={$lastUsedIdentitiesStore.selected}
      onSignOut={handleConfirmSignOut}
      onSignOutAndRemove={handleConfirmSignOutAndRemove}
    />
  </Dialog>
{/if}

{#if isReauthDialogOpen}
  <Dialog>
    <ReauthPrompt
      onReauthenticate={handleReauthenticate}
      onSignOut={handleConfirmSignOut}
    />
  </Dialog>
{/if}
