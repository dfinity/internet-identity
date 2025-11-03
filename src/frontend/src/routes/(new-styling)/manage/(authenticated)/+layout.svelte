<script lang="ts">
  import {
    ChevronDownIcon,
    HomeIcon,
    KeyRoundIcon,
    MenuIcon,
    XIcon,
    LifeBuoyIcon,
    CodeIcon,
  } from "@lucide/svelte";
  import type { LayoutProps } from "./$types";
  import { page } from "$app/state";
  import {
    afterNavigate,
    goto,
    invalidateAll,
    replaceState,
  } from "$app/navigation";
  import IdentitySwitcher from "$lib/components/ui/IdentitySwitcher.svelte";
  import { authenticatedStore } from "$lib/stores/authentication.store";
  import { lastUsedIdentitiesStore } from "$lib/stores/last-used-identities.store";
  import Button from "$lib/components/ui/Button.svelte";
  import Popover from "$lib/components/ui/Popover.svelte";
  import { toaster } from "$lib/components/utils/toaster";
  import { AuthLastUsedFlow } from "$lib/flows/authLastUsedFlow.svelte";
  import { handleError } from "$lib/components/utils/error";
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import AuthWizard from "$lib/components/wizards/auth/AuthWizard.svelte";
  import { sessionStore } from "$lib/stores/session.store";
  import { t } from "$lib/stores/locale.store";
  import Logo from "$lib/components/ui/Logo.svelte";
  import NavItem from "$lib/components/ui/NavItem.svelte";
  import { SOURCE_CODE_URL, SUPPORT_URL } from "$lib/config";

  const { children, data }: LayoutProps = $props();

  const authLastUsedFlow = new AuthLastUsedFlow();

  let pendingRegistrationId = $state(data.pendingRegistrationId);
  let identityButtonRef = $state<HTMLElement>();
  let isMobileSidebarOpen = $state(false);
  let isIdentityPopoverOpen = $state(false);
  let isAuthDialogOpen = $state(false);
  let isAuthenticating = $state(false);

  const lastUsedIdentities = $derived(
    Object.values($lastUsedIdentitiesStore.identities)
      .sort((a, b) => b.lastUsedTimestampMillis - a.lastUsedTimestampMillis)
      .slice(0, 3),
  );

  const gotoManage = () => goto("/manage", { replaceState: true });
  const handleSignIn = async (identityNumber: bigint) => {
    isAuthDialogOpen = false;
    lastUsedIdentitiesStore.selectIdentity(identityNumber);
    await invalidateAll();
  };
  const handleSignUp = async (identityNumber: bigint) => {
    isAuthDialogOpen = false;
    lastUsedIdentitiesStore.selectIdentity(identityNumber);
    toaster.success({
      title: "You're all set. Your identity has been created.",
      duration: 2000,
    });
    await invalidateAll();
  };
  const handleMigration = async (identityNumber: bigint) => {
    isAuthDialogOpen = false;
    lastUsedIdentitiesStore.selectIdentity(identityNumber);
    toaster.success({
      title: "Migration completed successfully",
      duration: 4000,
    });
    await invalidateAll();
  };
  const handleSwitchIdentity = async (identityNumber: bigint) => {
    await sessionStore.reset();
    const chosenIdentity =
      $lastUsedIdentitiesStore.identities[Number(identityNumber)];
    await authLastUsedFlow.authenticate(chosenIdentity);
    isIdentityPopoverOpen = false;
    lastUsedIdentitiesStore.selectIdentity(identityNumber);
    await invalidateAll();
  };

  // Initialize the flow every time the identity changes
  $effect(() => {
    authLastUsedFlow.init([$authenticatedStore.identityNumber]);
  });

  // Hide mobile sidebar on navigation
  $effect(() => {
    page.route.id;
    isMobileSidebarOpen = false;
  });

  // Remove registration id from URL bar after assigning it to state
  afterNavigate(() => {
    if (page.url.searchParams.has("activate")) {
      replaceState("/manage", {});
    }
  });
</script>

<!-- Layout -->
<div class="bg-bg-primary_alt flex min-h-[100dvh] flex-row">
  <!-- Sidebar -->
  <aside
    class={[
      "bg-bg-primary border-border-secondary flex w-74 flex-col sm:border-r sm:max-md:w-19",
      "max-sm:invisible max-sm:absolute max-sm:inset-0 max-sm:z-1 max-sm:w-full",
      isMobileSidebarOpen && "max-sm:visible",
    ]}
  >
    <div class="h-[env(safe-area-inset-top)]"></div>
    <!-- Mobile close button -->
    <div
      class="mb-3 flex flex-row items-center justify-end px-4 py-3 sm:hidden"
    >
      <Button
        onclick={() => (isMobileSidebarOpen = false)}
        variant="tertiary"
        iconOnly
        class="ml-2 sm:hidden"
        aria-label={$t`Close menu`}
      >
        <XIcon class="size-5" />
      </Button>
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
        Internet Identity Hub
      </div>
    </div>
    <!-- Navigation -->
    <nav class="flex flex-col gap-0.5 px-4">
      <ul class="contents">
        <li class="contents">
          <NavItem href="/manage" current={page.url.pathname === "/manage"}>
            <HomeIcon class="size-5 sm:max-md:mx-auto" />
            <span class="sm:max-md:hidden">{$t`Home`}</span>
          </NavItem>
        </li>
        <li class="contents">
          <NavItem
            href="/manage/access"
            current={page.url.pathname === "/manage/access"}
          >
            <KeyRoundIcon class="size-5 sm:max-md:mx-auto" />
            <span class="sm:max-md:hidden">{$t`Access methods`}</span>
          </NavItem>
        </li>
      </ul>
    </nav>
    <!-- Footer navigation -->
    <div class="mt-auto mb-5 flex flex-col gap-0.5 px-4">
      <ul class="contents">
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
  <div class="flex flex-1 flex-col">
    <div class="h-[env(safe-area-inset-top)]"></div>
    <!-- Header -->
    <header class="flex h-16 flex-row items-center px-4 sm:px-8 md:px-12">
      <!-- Mobile logo -->
      <Logo class="text-fg-primary h-4 sm:hidden" />
      <!-- Identity button-->
      <Button
        bind:element={identityButtonRef}
        onclick={() => (isIdentityPopoverOpen = true)}
        variant="tertiary"
        class="ml-auto gap-2.5 pr-3 sm:-mr-3"
        aria-label={$t`Switch identity`}
      >
        <span>
          {data.identityInfo.name ?? $authenticatedStore.identityNumber}
        </span>
        <ChevronDownIcon size="1rem" />
      </Button>
      <!-- Mobile menu button -->
      <Button
        onclick={() => (isMobileSidebarOpen = true)}
        variant="tertiary"
        iconOnly
        class="ml-2 sm:hidden"
        aria-label={$t`Open menu`}
      >
        <MenuIcon class="size-5" />
      </Button>
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
    onClose={() => (isIdentityPopoverOpen = false)}
    direction="down"
    align="end"
    distance="0.75rem"
  >
    <IdentitySwitcher
      selected={$authenticatedStore.identityNumber}
      identities={lastUsedIdentities}
      switchIdentity={handleSwitchIdentity}
      useAnotherIdentity={() => {
        isIdentityPopoverOpen = false;
        isAuthDialogOpen = true;
      }}
      onClose={() => (isIdentityPopoverOpen = false)}
      onLogout={() => location.replace("/login")}
    />
  </Popover>
{/if}

{#if isAuthDialogOpen}
  <Dialog
    onClose={() => (isAuthDialogOpen = false)}
    showCloseButton={!isAuthenticating}
    closeOnOutsideClick={!isAuthenticating}
  >
    <AuthWizard
      bind:isAuthenticating
      onSignIn={handleSignIn}
      onSignUp={handleSignUp}
      onMigration={handleMigration}
      onError={(error) => {
        isAuthDialogOpen = false;
        handleError(error);
      }}
      withinDialog
    >
      <h1 class="text-text-primary my-2 self-start text-2xl font-medium">
        {$t`Use another identity`}
      </h1>
      <p class="text-text-secondary mb-6 self-start text-sm">
        {$t`choose method`}
      </p>
    </AuthWizard>
  </Dialog>
{/if}
