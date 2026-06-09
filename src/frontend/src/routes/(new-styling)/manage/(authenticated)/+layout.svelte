<script lang="ts">
  import {
    BriefcaseMedicalIcon,
    ChevronDownIcon,
    HouseIcon,
    KeyRoundIcon,
    MenuIcon,
    XIcon,
    LifeBuoyIcon,
    CodeIcon,
    LanguagesIcon,
    SettingsIcon,
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
  import { SOURCE_CODE_URL, SUPPORT_URL } from "$lib/config";
  import type { LayoutProps } from "./$types";
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import Popover from "$lib/components/ui/Popover.svelte";
  import Logo from "$lib/components/ui/Logo.svelte";
  import NavItem from "$lib/components/ui/NavItem.svelte";
  import Avatar from "$lib/components/ui/Avatar.svelte";
  import IdentitySwitcher from "$lib/components/ui/IdentitySwitcher.svelte";
  import ManageIdentities from "$lib/components/ui/ManageIdentities.svelte";
  import SignOutConfirmation from "$lib/components/ui/SignOutConfirmation.svelte";
  import ReauthPrompt from "$lib/components/ui/ReauthPrompt.svelte";
  import {
    AuthWizard,
    IdentityNotConnected,
    IdentityAlreadyLinked,
    SwitchAccessMethod,
  } from "$lib/components/wizards/auth";
  import type { AccessMethod } from "$lib/components/wizards/auth/views/SwitchAccessMethod.svelte";
  import type { LastUsedIdentity } from "$lib/stores/last-used-identities.store";
  import { backendCanisterConfig } from "$lib/globals";
  import { getMetadataString } from "$lib/utils/openID";
  import ChooseLanguage from "$lib/components/views/ChooseLanguage.svelte";

  // --- Props & state ---

  const { children, data }: LayoutProps = $props();
  const authLastUsedFlow = new AuthLastUsedFlow();

  let identityButtonRef = $state<HTMLElement>();
  let isMobileSidebarOpen = $state(false);
  let isIdentityPopoverOpen = $state(false);
  let isAuthDialogOpen = $state(false);
  let isCreateIdentityDialogOpen = $state(false);
  let isAuthenticating = $state(false);
  let isManageIdentitiesDialogOpen = $state(false);
  let isSignOutDialogOpen = $state(false);
  let isLanguageDialogOpen = $state(false);
  let isReauthDialogOpen = $state(false);
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

  // --- Derived ---

  const lastUsedIdentities = $derived(
    Object.values($lastUsedIdentitiesStore.identities).sort(
      (a, b) => b.lastUsedTimestampMillis - a.lastUsedTimestampMillis,
    ),
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
      isCreateIdentityDialogOpen = false;
      signUpOpenedFromSignInModal = false;
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
    lastUsedIdentitiesStore.removeIdentity($authenticatedStore.identityNumber);
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
            current={page.url.pathname === "/manage/access"}
          >
            <KeyRoundIcon class="size-5 sm:max-md:mx-auto" />
            <span class="sm:max-md:hidden">{$t`Access`}</span>
          </NavItem>
        </li>
        <li class="contents">
          <NavItem
            href="/manage/recovery"
            current={page.url.pathname === "/manage/recovery"}
          >
            <BriefcaseMedicalIcon class="size-5 sm:max-md:mx-auto" />
            <span class="sm:max-md:hidden">{$t`Recovery`}</span>
          </NavItem>
        </li>
        <li class="contents">
          <NavItem
            href="/manage/settings"
            current={page.url.pathname === "/manage/settings"}
          >
            <SettingsIcon class="size-5 sm:max-md:mx-auto" />
            <span class="sm:max-md:hidden">{$t`Settings`}</span>
          </NavItem>
        </li>
      </ul>
    </nav>
    <!-- Empty space between top and bottom content-->
    <div class="flex-1"></div>
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
      switchModeTitle={$t`Want to create a new identity?`}
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
      <h1 class="text-text-primary my-2 self-start text-2xl font-medium">
        {$t`Create another identity`}
      </h1>
      <p class="text-text-secondary mb-6 self-start text-sm">
        {$t`Set up a new identity.`}
      </p>
    </AuthWizard>
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
      userEmail={payload.userName !== undefined ? payload.userEmail : undefined}
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
      userEmail={payload.userName !== undefined ? payload.userEmail : undefined}
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

{#if isSignOutDialogOpen}
  {@const currentIdentity =
    $lastUsedIdentitiesStore.identities[
      $authenticatedStore.identityNumber.toString()
    ]}
  {#if currentIdentity !== undefined}
    <Dialog onClose={() => (isSignOutDialogOpen = false)}>
      <SignOutConfirmation
        identity={currentIdentity}
        onSignOut={handleConfirmSignOut}
        onSignOutAndRemove={handleConfirmSignOutAndRemove}
      />
    </Dialog>
  {/if}
{/if}

{#if isReauthDialogOpen}
  <Dialog>
    <ReauthPrompt
      onReauthenticate={handleReauthenticate}
      onSignOut={handleConfirmSignOut}
    />
  </Dialog>
{/if}
