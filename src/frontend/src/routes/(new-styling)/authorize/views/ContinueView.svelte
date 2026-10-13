<script lang="ts">
  import { untrack, type Snippet } from "svelte";
  import { lastUsedIdentitiesStore } from "$lib/stores/last-used-identities.store";
  import {
    CheckIcon,
    HelpCircleIcon,
    PlusIcon,
    PencilIcon,
  } from "@lucide/svelte";
  import { handleError } from "$lib/components/utils/error";
  import AuthorizeHeader from "$lib/components/ui/AuthorizeHeader.svelte";
  import SystemOverlayBackdrop from "$lib/components/utils/SystemOverlayBackdrop.svelte";
  import {
    AuthenticationV2Events,
    authenticationV2Funnel,
  } from "$lib/utils/analytics/authenticationV2Funnel";
  import { getAppMetadataStore } from "$lib/stores/app-metadata.store";
  import { AuthLastUsedFlow } from "$lib/flows/authLastUsedFlow.svelte";
  import { plural, t } from "$lib/stores/locale.store";
  import Toggle from "$lib/components/ui/Toggle.svelte";
  import Tooltip from "$lib/components/ui/Tooltip.svelte";
  import {
    authenticationStore,
    isAuthenticatedStore,
  } from "$lib/stores/authentication.store";
  import {
    actorForIdentity,
    purgeSession,
  } from "$lib/stores/session-delegation.store";
  import { throwCanisterError, isCanisterError } from "$lib/utils/utils";
  import type { ActorSubclass } from "@icp-sdk/core/agent";
  import type {
    _SERVICE,
    SessionDelegationError,
    AccountInfo,
    AccountNumber,
  } from "$lib/generated/internet_identity_types";
  import Badge from "$lib/components/ui/Badge.svelte";
  import { slide, fade } from "svelte/transition";
  import AccessLevelSelector from "$lib/components/ui/AccessLevelSelector.svelte";
  import type { AccessLevel } from "$lib/utils/accessLevel";
  import { accessLevelStore } from "$lib/stores/access-level.store";
  import { READ_ONLY_MODE } from "$lib/state/featureFlags";
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import EditAccount from "$lib/components/views/EditAccount.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import SessionDurationSelect from "$lib/components/ui/SessionDurationSelect.svelte";
  import {
    SESSION_DURATION_PRESETS,
    cappedSessionDurations,
    sessionDurationCeilingSeconds,
    sessionDurationToNanos,
  } from "$lib/utils/sessionDuration";

  interface Props {
    effectiveOrigin: string;
    /** Origin shown to the user — drives the app-name lookup and the default
     *  header. Kept separate from `effectiveOrigin` (which may be remapped for
     *  canister calls) and from the postMessage channel, so non-authorize flows
     *  (e.g. /mcp) can reuse this picker by passing the origin they connect to. */
    displayOrigin: string;
    /** Called when the user confirms with the default account or selects a
     *  specific account. `accessLevel` reflects the access-level choice
     *  ("Questions only" vs "Actions & questions"): "read-only" restricts the
     *  session delegation to query calls, so the app can read on the user's
     *  behalf but cannot change state (the Internet Computer rejects update
     *  calls authenticated through it). `maxTimeToLive` is the session duration
     *  the user picked (nanoseconds), at most the app's requested value. */
    onAuthorize: (
      accountNumber: Promise<bigint | undefined>,
      accessLevel: AccessLevel,
      maxTimeToLive: bigint,
    ) => void;
    /** The session duration the app requested (`maxTimeToLive`, nanoseconds), or
     *  `undefined` when it didn't specify one. It's the ceiling the user picks
     *  under; when absent the picker allows up to the 30-day maximum (the
     *  backend's default and cap). */
    requestedMaxTimeToLive?: bigint;
    /** Replaces the default authorize header (app tile + "Continue to <app>"),
     *  letting /mcp render its own connect consent above the same picker. */
    header?: Snippet;
    /** Label for the default-account confirm button. Defaults to "Continue". */
    continueLabel?: string;
  }

  const {
    effectiveOrigin,
    displayOrigin,
    onAuthorize,
    requestedMaxTimeToLive,
    header,
    continueLabel,
  }: Props = $props();

  const id = $props.id();

  type PRIMARY_ACCOUNT_NUMBER = undefined;
  const MAX_ACCOUNTS = 5;

  // The largest duration the picker offers: the app's requested value, capped
  // at the 30-day maximum, or the maximum itself when the app didn't request
  // one. A plain "Continue" grants exactly this; with all options shown the
  // picker starts on the duration last chosen for this app on this browser.
  const ceilingSeconds = sessionDurationCeilingSeconds(requestedMaxTimeToLive);
  let selectedTtlSeconds = $state(ceilingSeconds);
  const selectedMaxTimeToLive = $derived(
    sessionDurationToNanos(selectedTtlSeconds),
  );

  // Browser-local, per-anchor persistence for the multi-accounts toggle.
  // Per-anchor (not per-dapp) because the toggle is a mental-mode switch:
  // a user who self-identifies as a multi-accounts user wants the
  // affordance everywhere, not separately for each dapp.
  const TOGGLE_STORAGE_PREFIX = "ii:multi-accounts:";
  const readToggle = (anchor: bigint): boolean => {
    if (typeof localStorage === "undefined") return false;
    return (
      localStorage.getItem(`${TOGGLE_STORAGE_PREFIX}${anchor.toString()}`) ===
      "1"
    );
  };
  const writeToggle = (anchor: bigint, enabled: boolean): void => {
    if (typeof localStorage === "undefined") return;
    const key = `${TOGGLE_STORAGE_PREFIX}${anchor.toString()}`;
    if (enabled) {
      localStorage.setItem(key, "1");
    } else {
      localStorage.removeItem(key);
    }
  };

  let defaultAccountNumber = $state<
    AccountNumber | PRIMARY_ACCOUNT_NUMBER | null
  >(null);
  let accounts = $state<AccountInfo[]>();
  let isAuthenticatingDefault = $state(false);
  // The access-level selector (shown only when READ_ONLY_MODE is on) starts on
  // this anchor's last choice for this flow, or unselected on a first-time sign
  // in so they pick explicitly. Per-anchor (the browser may be shared); the
  // effect below re-hydrates it when the selected identity changes. While the
  // flag is off (the current default), the selector is hidden and
  // `effectiveAccessLevel` forces full access, so this state is never surfaced
  // (a queries-only delegation would fail closed in every current agent — see
  // the READ_ONLY_MODE flag).
  let accessLevel: AccessLevel | undefined = $state(
    accessLevelStore.getPreference(
      "continue",
      $lastUsedIdentitiesStore.selected!.identityNumber,
    ),
  );
  // With the flag on, the selector gates Continue until a choice is made, so
  // `accessLevel` is always defined here; the fallback only satisfies the type.
  const effectiveAccessLevel: AccessLevel = $derived(
    $READ_ONLY_MODE ? (accessLevel ?? "full-access") : "full-access",
  );
  // First-time sign in must choose before continuing (flag on only). Gates both
  // the default Continue button and the per-account list items.
  const mustChooseAccess = $derived(
    $READ_ONLY_MODE && accessLevel === undefined,
  );
  // Persist the chosen level for this anchor when the selector was actually
  // shown and used, so it pre-fills next time. No-op while the flag is off
  // (nothing was chosen).
  const rememberAccessLevel = (): void => {
    if ($READ_ONLY_MODE && accessLevel !== undefined) {
      accessLevelStore.setPreference(
        "continue",
        selectedIdentityNumber,
        accessLevel,
      );
    }
  };
  let isMultipleAccountsEnabled = $state(
    readToggle($lastUsedIdentitiesStore.selected!.identityNumber),
  );
  // Clear old accounts data when user toggles switch off
  $effect(() => {
    if (!isMultipleAccountsEnabled) {
      accounts = undefined;
      selectedAccountNumber = null;
    }
  });

  // The account and duration last chosen for this app with all options shown,
  // on this browser.
  const rememberedOptions = $derived(
    $lastUsedIdentitiesStore.selected?.authorizeOptions?.[effectiveOrigin],
  );
  const rememberedTtlSeconds = (): number => {
    const remembered = rememberedOptions?.sessionDurationSeconds;
    return remembered !== undefined &&
      cappedSessionDurations(ceilingSeconds, SESSION_DURATION_PRESETS).includes(
        remembered,
      )
      ? remembered
      : ceilingSeconds;
  };

  // The account picked in the list, or `null` until the user picks one, in
  // which case Continue signs in with the remembered account if it still
  // exists, or else the default account.
  let selectedAccountNumber = $state<
    AccountNumber | PRIMARY_ACCOUNT_NUMBER | null
  >(null);
  const chosenAccountNumber = $derived.by(() => {
    if (selectedAccountNumber !== null) {
      return selectedAccountNumber;
    }
    if (
      rememberedOptions !== undefined &&
      accounts?.some(
        (account) =>
          account.account_number[0] === rememberedOptions.accountNumber,
      ) === true
    ) {
      return rememberedOptions.accountNumber;
    }
    return defaultAccountNumber ?? undefined;
  });

  let isCreateAccountDialogVisible = $state(false);
  let isEditAccountDialogVisibleForNumber = $state<
    AccountNumber | PRIMARY_ACCOUNT_NUMBER | null
  >(null);
  // The fallback label the edit dialog opened with. The live value can change
  // while the dialog is open (the app's metadata resolves asynchronously),
  // which would shift an unnamed account's baseline under an untouched form
  // and persist the stale fallback as an explicit name on save.
  let editDialogFallbackName = $state<string>();

  const isEditAccountDialogVisibleFor = $derived(
    accounts?.find(
      (account) =>
        account.account_number[0] === isEditAccountDialogVisibleForNumber,
    ),
  );
  // Continue looks unchanged while the account list loads, but does nothing
  // until it has.
  const isLoadingAccounts = $derived(
    isMultipleAccountsEnabled && accounts === undefined,
  );
  const isAccountLimitReached = $derived(
    accounts !== undefined && accounts.length >= 5,
  );
  // The app's metadata is published on the origin its identity is derived for
  // (`effectiveOrigin`), which is also where its accounts live — so the account
  // labels below stay stable no matter which of its origins the user signs in
  // from. `displayOrigin` remains what the header shows.
  const metadataStore = $derived(
    getAppMetadataStore(effectiveOrigin, displayOrigin),
  );
  const application = $derived($metadataStore.name);
  const dappName = $derived(application ?? new URL(displayOrigin).hostname);
  // Account names are capped at 32 characters in `EditAccount`; a longer
  // (e.g. app-provided) name would make the fallback label unsaveable there,
  // so it falls back to the generic label instead.
  const MAX_ACCOUNT_NAME_LENGTH = 32;
  const primaryAccountName = $derived.by(() => {
    if (application !== undefined) {
      const label = $t`My ${application} account`;
      if (label.length <= MAX_ACCOUNT_NAME_LENGTH) {
        return label;
      }
    }
    return $t`My account`;
  });
  const existingNames = $derived(
    accounts?.map((account) => account.name[0] ?? primaryAccountName) ?? [],
  );
  const authLastUsedFlow = new AuthLastUsedFlow();
  const selectedIdentityNumber = $derived(
    $lastUsedIdentitiesStore.selected!.identityNumber,
  );
  // Re-initialize the flow and re-hydrate per-identity state when the
  // identity changes. Read the persisted values into locals so the
  // effect's only reactive dependency is `selectedIdentityNumber` -- if
  // we read `isMultipleAccountsEnabled` (or `accessLevel`) directly, the
  // user's own toggle/selector click would re-trigger this effect and
  // overwrite the new value back from stale localStorage.
  $effect(() => {
    authLastUsedFlow.init([selectedIdentityNumber]);
    const hydrated = readToggle(selectedIdentityNumber);
    isMultipleAccountsEnabled = hydrated;
    accessLevel = accessLevelStore.getPreference(
      "continue",
      selectedIdentityNumber,
    );
    defaultAccountNumber = null;
    selectedAccountNumber = null;
    if (hydrated) {
      void handleEnableMultipleAccounts();
    }
  });

  // Persist the toggle whenever it (or the identity) changes.
  $effect(() => {
    writeToggle(selectedIdentityNumber, isMultipleAccountsEnabled);
  });

  const handleContinueDefault = async () => {
    rememberAccessLevel();
    isAuthenticatingDefault = true;
    try {
      if (defaultAccountNumber === null) {
        const sessionActor = await actorForIdentity(selectedIdentityNumber);
        if (sessionActor !== undefined) {
          try {
            const account = await sessionActor
              .get_default_account(selectedIdentityNumber, effectiveOrigin)
              .then(throwCanisterError);
            defaultAccountNumber = account.account_number[0];
          } catch (err) {
            if (
              isCanisterError<SessionDelegationError>(err) &&
              err.type === "Unauthorized"
            ) {
              void purgeSession(selectedIdentityNumber);
            } else {
              throw err;
            }
          }
        }
      }

      if (!$isAuthenticatedStore) {
        await authLastUsedFlow.authenticate(
          $lastUsedIdentitiesStore.selected!,
          effectiveOrigin,
        );
      }
      const { identityNumber, actor } = $authenticationStore!;
      const accountNumberPromise =
        defaultAccountNumber === null
          ? actor
              .get_default_account(identityNumber, effectiveOrigin)
              .then(throwCanisterError)
              .then((account) => account.account_number[0])
          : Promise.resolve(defaultAccountNumber);
      onAuthorize(
        accountNumberPromise,
        effectiveAccessLevel,
        sessionDurationToNanos(ceilingSeconds),
      );
    } catch (error) {
      // Left busy on the way out: this view stays up while a request that still owes
      // the flow a screen works out what to show, and the button the user pressed is
      // where that wait belongs. Cleared only here, where they stay and can retry.
      isAuthenticatingDefault = false;
      handleError(error);
    }
  };
  const handleContinueAs = async (
    accountNumber: AccountNumber | PRIMARY_ACCOUNT_NUMBER,
  ) => {
    rememberAccessLevel();
    isAuthenticatingDefault = true;
    try {
      if (!$isAuthenticatedStore) {
        await authLastUsedFlow.authenticate(
          $lastUsedIdentitiesStore.selected!,
          effectiveOrigin,
        );
      }
      lastUsedIdentitiesStore.setAuthorizeOptions(
        selectedIdentityNumber,
        effectiveOrigin,
        { accountNumber, sessionDurationSeconds: selectedTtlSeconds },
      );
      onAuthorize(
        Promise.resolve(accountNumber),
        effectiveAccessLevel,
        selectedMaxTimeToLive,
      );
    } catch (error) {
      // See `handleContinueDefault`: the busy state outlives a successful authorize.
      isAuthenticatingDefault = false;
      handleError(error);
    }
  };
  const loadAccountsViaActor = async (
    actor: ActorSubclass<_SERVICE>,
    identityNumber: bigint,
  ) => {
    const values = await Promise.all([
      actor
        .get_accounts(identityNumber, effectiveOrigin)
        .then(throwCanisterError),
      actor
        .get_default_account(identityNumber, effectiveOrigin)
        .then(throwCanisterError),
    ]);
    accounts = values[0].sort((a, b) => {
      const aVal = a.last_used[0] ?? BigInt(-1);
      const bVal = b.last_used[0] ?? BigInt(-1);
      return bVal > aVal ? 1 : bVal < aVal ? -1 : 0;
    });
    defaultAccountNumber = values[1].account_number[0];
  };

  const handleEnableMultipleAccounts = async () => {
    // Untracked: this runs inside the identity effect, which must not rerun
    // when Continue saves the options.
    selectedTtlSeconds = untrack(rememberedTtlSeconds);
    try {
      const sessionActor = await actorForIdentity(selectedIdentityNumber);
      if (sessionActor !== undefined) {
        try {
          await loadAccountsViaActor(sessionActor, selectedIdentityNumber);
          return;
        } catch (err) {
          if (
            isCanisterError<SessionDelegationError>(err) &&
            err.type === "Unauthorized"
          ) {
            void purgeSession(selectedIdentityNumber);
          } else {
            throw err;
          }
        }
      }

      if (!$isAuthenticatedStore) {
        await authLastUsedFlow.authenticate(
          $lastUsedIdentitiesStore.selected!,
          effectiveOrigin,
        );
      }
      const { identityNumber, actor } = $authenticationStore!;
      await loadAccountsViaActor(actor, identityNumber);
    } catch (error) {
      isMultipleAccountsEnabled = false;
      handleError(error);
    }
  };
  const handleCreateAccount = async (account: {
    name: string;
    isDefaultSignIn: boolean;
  }) => {
    try {
      // create_account is not in the session-delegation scope, so it needs a
      // full-auth identity even when the screen was loaded ceremony-free.
      if (!$isAuthenticatedStore) {
        await authLastUsedFlow.authenticate(
          $lastUsedIdentitiesStore.selected!,
          effectiveOrigin,
        );
      }
      const authenticated = $authenticationStore;
      if (authenticated === undefined) {
        return;
      }
      const { identityNumber, actor } = authenticated;
      const createdAccount = await actor
        .create_account(identityNumber, effectiveOrigin, account.name)
        .then(throwCanisterError);
      if (account.isDefaultSignIn) {
        defaultAccountNumber = (
          await actor
            .set_default_account(
              identityNumber,
              effectiveOrigin,
              createdAccount.account_number,
            )
            .then(throwCanisterError)
        ).account_number[0];
      }
      accounts = [...(accounts ?? []), createdAccount];
    } catch (error) {
      handleError(error);
    } finally {
      isCreateAccountDialogVisible = false;
    }
  };
  const openEditAccountDialog = (
    accountNumber: AccountNumber | PRIMARY_ACCOUNT_NUMBER,
  ): void => {
    editDialogFallbackName = primaryAccountName;
    isEditAccountDialogVisibleForNumber = accountNumber;
  };
  const handleEditAccount = async (account: {
    name: string;
    isDefaultSignIn: boolean;
  }) => {
    if (accounts === undefined) {
      return;
    }
    try {
      const index = accounts.findIndex(
        (acc) => acc.account_number[0] === isEditAccountDialogVisibleForNumber,
      );
      if (index === -1) {
        return;
      }
      const nameChanged =
        account.name !==
        (accounts[index].name[0] ??
          editDialogFallbackName ??
          primaryAccountName);
      const defaultChanged =
        account.isDefaultSignIn &&
        defaultAccountNumber !== accounts[index].account_number[0];

      if (nameChanged) {
        if (!$isAuthenticatedStore) {
          await authLastUsedFlow.authenticate(
            $lastUsedIdentitiesStore.selected!,
            effectiveOrigin,
          );
        }
        const authenticated = $authenticationStore;
        if (authenticated === undefined) {
          return;
        }
        accounts[index] = await authenticated.actor
          .update_account(
            authenticated.identityNumber,
            effectiveOrigin,
            accounts[index].account_number,
            { name: [account.name] },
          )
          .then(throwCanisterError);

        if (isEditAccountDialogVisibleForNumber === defaultAccountNumber) {
          defaultAccountNumber = accounts[index].account_number[0];
        }
      }

      if (defaultChanged) {
        const sessionActor = await actorForIdentity(selectedIdentityNumber);
        if (sessionActor !== undefined) {
          try {
            accounts[index] = await sessionActor
              .set_default_account(
                selectedIdentityNumber,
                effectiveOrigin,
                accounts[index].account_number,
              )
              .then(throwCanisterError);
            defaultAccountNumber = accounts[index].account_number[0];
            return;
          } catch (err) {
            if (
              isCanisterError<SessionDelegationError>(err) &&
              err.type === "Unauthorized"
            ) {
              void purgeSession(selectedIdentityNumber);
            } else {
              throw err;
            }
          }
        }

        if (!$isAuthenticatedStore) {
          await authLastUsedFlow.authenticate(
            $lastUsedIdentitiesStore.selected!,
            effectiveOrigin,
          );
        }
        const authenticated = $authenticationStore;
        if (authenticated === undefined) {
          return;
        }
        accounts[index] = await authenticated.actor
          .set_default_account(
            authenticated.identityNumber,
            effectiveOrigin,
            accounts[index].account_number,
          )
          .then(throwCanisterError);
        defaultAccountNumber = accounts[index].account_number[0];
      }
    } catch (error) {
      handleError(error);
    } finally {
      isEditAccountDialogVisibleForNumber = null;
    }
  };

  $effect(() => {
    if (accounts === undefined) {
      return;
    }
    lastUsedIdentitiesStore.syncLastUsedAccounts(
      selectedIdentityNumber,
      effectiveOrigin,
      accounts,
    );
  });

  $effect(() => {
    authenticationV2Funnel.trigger(AuthenticationV2Events.ContinueAsScreen);
  });
</script>

{#snippet accountRow(account: AccountInfo)}
  {@const name = account.name[0] ?? primaryAccountName}
  {@const isSelected = account.account_number[0] === chosenAccountNumber}
  <div
    class={[
      "border-border-tertiary flex h-13 flex-row items-center border-b pe-1.5",
      isSelected ? "bg-bg-quaternary" : "hover:bg-bg-tertiary",
    ]}
  >
    <label
      class="relative flex min-w-0 flex-1 flex-row items-center gap-3 self-stretch ps-4 pe-3 not-has-disabled:cursor-pointer"
    >
      <input
        type="radio"
        name="{id}-account"
        checked={isSelected}
        onchange={() => (selectedAccountNumber = account.account_number[0])}
        class="absolute inset-0 appearance-none opacity-0"
      />
      <CheckIcon
        class={["size-4.5 shrink-0", isSelected ? "opacity-100" : "opacity-0"]}
        aria-hidden="true"
      />
      <span class="text-text-primary flex-1 truncate text-sm font-semibold">
        {name}
      </span>
      {#if account.account_number[0] === defaultAccountNumber}
        <Badge size="sm" class="shrink-0 !bg-transparent">{$t`Default`}</Badge>
      {/if}
    </label>
    <button
      class={[
        "btn btn-tertiary btn-sm btn-icon shrink-0",
        isSelected ? "hover:!bg-bg-tertiary" : "hover:!bg-bg-quaternary",
      ]}
      onclick={() => openEditAccountDialog(account.account_number[0])}
      aria-label={$t`Edit ${name}`}
    >
      <PencilIcon class="size-5" />
    </button>
  </div>
{/snippet}

{#snippet allOptions(accounts: AccountInfo[])}
  <div
    class="col-start-1 row-start-1"
    transition:slide={{ axis: "y", duration: 300 }}
  >
    <div
      class="flex flex-col pb-6"
      in:fade={{ duration: 300 }}
      out:fade={{ duration: 100 }}
    >
      <span
        id="{id}-account-label"
        class="text-text-tertiary mb-2 text-sm font-medium"
      >
        {$t`Account`}
      </span>
      <div
        class={[
          "border-border-secondary bg-bg-primary mb-5 flex flex-col overflow-hidden rounded-xl border",
          "has-[input:focus-visible]:outline-focus-ring has-[input:focus-visible]:outline-2 has-[input:focus-visible]:outline-offset-2",
        ]}
      >
        <!-- Native radios: arrow keys, a single tab stop and the radio
               announcement come from the browser. -->
        <fieldset
          class="flex min-w-0 flex-col"
          aria-labelledby="{id}-account-label"
          disabled={isAuthenticatingDefault}
        >
          {#each accounts as account (account.account_number[0])}
            {@render accountRow(account)}
          {/each}
        </fieldset>
        <Tooltip
          label={$t`Limit reached`}
          description={$plural(MAX_ACCOUNTS, {
            one: `You have reached the maximum of # account for a single app.`,
            other: `You have reached the maximum of # accounts for a single app.`,
          })}
          direction="up"
          align="center"
          hidden={!isAccountLimitReached}
        >
          <button
            class={[
              "text-fg-primary hover:not-disabled:bg-bg-tertiary disabled:text-fg-disabled flex h-13 w-full cursor-pointer flex-row items-center gap-3 px-4 text-start text-sm font-semibold disabled:cursor-default",
              "focus-visible:outline-focus-ring rounded-b-[calc(var(--radius-xl)-1px)] focus-visible:outline-2 focus-visible:-outline-offset-2",
            ]}
            onclick={() => (isCreateAccountDialogVisible = true)}
            disabled={isAccountLimitReached}
          >
            <PlusIcon class="size-4.5 shrink-0" />
            {$t`Add another account`}
          </button>
        </Tooltip>
      </div>
      <span class="text-text-tertiary mb-2 text-sm font-medium">
        {$t`Session`}
      </span>
      <!-- How long the sign-in lasts before the user must sign in again,
             capped at the duration the app requested (see `ceilingSeconds`). -->
      <div
        class="border-border-secondary bg-bg-primary flex h-13 flex-row items-center gap-3 rounded-xl border ps-4 pe-1.5"
      >
        <span class="text-text-secondary flex-1 text-sm">
          {$t`Stay signed in for`}
        </span>
        <SessionDurationSelect
          maxSeconds={ceilingSeconds}
          bind:value={selectedTtlSeconds}
          disabled={isAuthenticatingDefault}
          variant="tertiary"
        />
      </div>
    </div>
  </div>
{/snippet}

<div class="flex flex-1 flex-col">
  {#if header}
    {@render header()}
  {:else}
    <AuthorizeHeader origin={displayOrigin} metadataOrigin={effectiveOrigin} />
    <h1 class="text-text-primary mb-2 self-start text-2xl font-medium">
      {$t`Continue to ${dappName}`}
    </h1>
    <p class="text-text-secondary mb-6 self-start text-sm">
      {$t`with your Internet Identity`}
    </p>
  {/if}
  <div class="grid grid-cols-1">
    {#if isMultipleAccountsEnabled && accounts !== undefined}
      {@render allOptions(accounts)}
    {/if}
  </div>
  <div class="pb-6">
    <button
      class="btn btn-primary btn-xl w-full"
      onclick={() => {
        if (!isMultipleAccountsEnabled) {
          void handleContinueDefault();
          return;
        }
        if (isLoadingAccounts) {
          return;
        }
        void handleContinueAs(chosenAccountNumber);
      }}
      disabled={isAuthenticatingDefault || mustChooseAccess}
      aria-disabled={isLoadingAccounts}
    >
      {#if isAuthenticatingDefault}
        <ProgressRing />
        <span>{$t`Authenticating...`}</span>
      {:else}
        <span>{continueLabel ?? $t`Continue`}</span>
      {/if}
    </button>
  </div>
  <div class="flex flex-row items-center">
    <!-- Intentionally we use onclick here instead of onchange to make sure it's a user gesture-->
    <Toggle
      bind:checked={isMultipleAccountsEnabled}
      onclick={isMultipleAccountsEnabled
        ? undefined
        : handleEnableMultipleAccounts}
      label={$t`Show all options`}
      size="sm"
      disabled={isAuthenticatingDefault}
    />
    <Tooltip
      label={$t`All options`}
      description={$t`By showing all options, you can choose how long your session lasts before signing in again and create more than one account for a single app (e.g. work, personal, or demo).`}
      direction="up"
      align="end"
      offset="0rem"
      class="max-w-80"
    >
      <button
        class="btn btn-tertiary btn-sm btn-icon ms-auto !cursor-default !rounded-full"
        aria-label={$t`More information about all options`}
      >
        <HelpCircleIcon class="size-5" />
      </button>
    </Tooltip>
  </div>
  {#if $READ_ONLY_MODE}
    <div class="border-border-tertiary mt-4 border-t pt-4">
      <AccessLevelSelector
        bind:accessLevel
        disabled={isAuthenticatingDefault}
      />
    </div>
  {/if}
</div>

{#if authLastUsedFlow.systemOverlay}
  <SystemOverlayBackdrop />
{/if}

{#if isCreateAccountDialogVisible}
  <Dialog onClose={() => (isCreateAccountDialogVisible = false)}>
    <EditAccount {existingNames} save={handleCreateAccount} />
  </Dialog>
{/if}

{#if isEditAccountDialogVisibleFor !== undefined}
  {@const account = {
    name:
      isEditAccountDialogVisibleFor.name[0] ??
      editDialogFallbackName ??
      primaryAccountName,
    isDefaultSignIn:
      defaultAccountNumber === isEditAccountDialogVisibleForNumber,
  }}
  <Dialog onClose={() => (isEditAccountDialogVisibleForNumber = null)}>
    <EditAccount
      {account}
      existingNames={existingNames.filter((name) => name !== account.name)}
      save={handleEditAccount}
    />
  </Dialog>
{/if}
