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
  import IdentitySwitcher from "$lib/components/ui/IdentitySwitcher.svelte";
  import {
    authenticatedStore,
    authenticationStore,
  } from "$lib/stores/authentication.store";
  import { lastUsedIdentitiesStore } from "$lib/stores/last-used-identities.store";
  import Popover from "$lib/components/ui/Popover.svelte";
  import { toaster } from "$lib/components/utils/toaster";
  import { AuthLastUsedFlow } from "$lib/flows/authLastUsedFlow.svelte";
  import { handleError } from "$lib/components/utils/error";
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import AuthWizard from "$lib/components/wizards/auth/AuthWizard.svelte";
  import { sessionStore } from "$lib/stores/session.store";
  import {
    formatDate,
    locales,
    localeStore,
    t,
  } from "$lib/stores/locale.store";
  import Logo from "$lib/components/ui/Logo.svelte";
  import NavItem from "$lib/components/ui/NavItem.svelte";
  import { SOURCE_CODE_URL, SUPPORT_URL } from "$lib/config";
  import type { LayoutProps } from "./$types";
  import ChooseLanguage from "$lib/components/views/ChooseLanguage.svelte";
  import { nanosToMillis } from "$lib/utils/time";
  import { lastUsedIdentityTypeName } from "$lib/utils/lastUsedIdentity";
  import ButtonCard from "$lib/components/ui/ButtonCard.svelte";
  import Avatar from "$lib/components/ui/Avatar.svelte";
  import { Trans } from "$lib/components/locale";
  import { getMetadataString } from "$lib/utils/openID";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";

  const { children, data }: LayoutProps = $props();

  const authLastUsedFlow = new AuthLastUsedFlow();

  let identityButtonRef = $state<HTMLElement>();
  let isMobileSidebarOpen = $state(false);
  let isIdentityPopoverOpen = $state(false);
  let isAuthDialogOpen = $state(false);
  let isAuthenticating = $state(false);
  let isLanguageDialogOpen = $state(false);
  let isRecoveryPhraseSetUpDismissed = $state(false);

  const lastUsedIdentities = $derived(
    Object.values($lastUsedIdentitiesStore.identities)
      .sort((a, b) => b.lastUsedTimestampMillis - a.lastUsedTimestampMillis)
      .slice(0, 3),
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

  const handleSignIn = async (identityNumber: bigint) => {
    isAuthenticating = true;
    if ($authenticationStore?.identityNumber !== identityNumber) {
      // Sign in if not authenticated with this identity yet
      await sessionStore.reset();
      await authLastUsedFlow.authenticate(
        $lastUsedIdentitiesStore.identities[`${identityNumber}`],
      );
    }
    lastUsedIdentitiesStore.selectIdentity(identityNumber);
    await goto("/manage", { replaceState: true, invalidateAll: true });
    isIdentityPopoverOpen = false;
    isAuthDialogOpen = false;
    isAuthenticating = false;
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
  const handleSignOut = async () => {
    window.location.replace("/");
  };

  // Pre-fetch passkey credential ids
  $effect(() =>
    authLastUsedFlow.init(
      lastUsedIdentities.map(({ identityNumber }) => identityNumber),
    ),
  );

  // Hide mobile sidebar on navigation
  afterNavigate(() => {
    isMobileSidebarOpen = false;
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
  <!-- Sidebar -->
  <aside
    class={[
      "bg-bg-primary border-border-secondary flex w-74 flex-col sm:border-r sm:max-md:w-19",
      "max-sm:invisible max-sm:absolute max-sm:inset-0 max-sm:z-1 max-sm:w-full max-sm:overflow-y-auto",
      isMobileSidebarOpen && "max-sm:visible",
    ]}
  >
    <div class="h-[env(safe-area-inset-top)]"></div>
    <!-- Mobile logo and close button -->
    <div
      class="mb-3 flex flex-row items-center justify-end px-4 py-3 sm:hidden"
    >
      <Logo class="text-fg-primary h-4" />
      <div class="text-text-primary ms-4 text-base font-semibold">
        Internet Identity
      </div>
      <button
        onclick={() => (isMobileSidebarOpen = false)}
        class="btn btn-tertiary btn-icon ms-auto sm:hidden"
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
      <Logo class="text-fg-primary h-4" />
      <div class="text-text-primary text-base font-semibold max-md:hidden">
        Internet Identity
      </div>
    </div>
    <!-- Mobile identity button-->
    {#if $lastUsedIdentitiesStore.selected !== undefined}
      {@const name = lastUsedIdentityTypeName(
        $lastUsedIdentitiesStore.selected,
      )}
      <ButtonCard
        onclick={() => (isIdentityPopoverOpen = true)}
        class="mx-4 mb-6 sm:hidden"
        aria-label={$t`Switch identity`}
      >
        <Avatar size="sm">
          <UserIcon class="size-5" />
        </Avatar>
        <div class="flex flex-col text-left text-sm">
          <div class="text-text-primary font-semibold">
            {data.identityInfo.name[0] ?? data.identityNumber.toString()}
          </div>
          <div class="text-text-tertiary font-normal" aria-hidden="true">
            {#if data.identityInfo.created_at[0] !== undefined}
              {@const date = $formatDate(
                new Date(nanosToMillis(data.identityInfo.created_at[0])),
                {
                  dateStyle: "short",
                },
              )}
              <span>{$t`${name} | Created ${date}`}</span>
            {:else}
              <span>{name}</span>
            {/if}
          </div>
        </div>
        <ChevronDownIcon class="ms-auto me-1 size-5" />
      </ButtonCard>
    {/if}
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
      <!-- Mobile logo -->
      <Logo class="text-fg-primary h-4 sm:hidden" />
      <!-- Empty space between left and right content -->
      <div class="flex-1"></div>
      <!-- Identity button-->
      <button
        bind:this={identityButtonRef}
        onclick={() => (isIdentityPopoverOpen = true)}
        class="btn btn-tertiary gap-2.5 pr-3 max-sm:hidden sm:-mr-3"
        aria-label={$t`Switch identity`}
      >
        <span>
          {data.identityInfo.name[0] ?? data.identityNumber.toString()}
        </span>
        <ChevronDownIcon size="1rem" />
      </button>
      <!-- Mobile menu button -->
      <div class="relative ms-2 sm:hidden">
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
        {$t`choose method to continue`}
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
        localeStore.setOrReset(value);
      }}
    />
  </Dialog>
{/if}
