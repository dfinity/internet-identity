<script lang="ts">
  import type { Snippet } from "svelte";
  import { lastUsedIdentitiesStore } from "$lib/stores/last-used-identities.store";
  import { HelpCircleIcon, PlusIcon, PencilIcon } from "@lucide/svelte";
  import { handleError } from "$lib/components/utils/error";
  import AuthorizeHeader from "$lib/components/ui/AuthorizeHeader.svelte";
  import SystemOverlayBackdrop from "$lib/components/utils/SystemOverlayBackdrop.svelte";
  import {
    AuthenticationV2Events,
    authenticationV2Funnel,
  } from "$lib/utils/analytics/authenticationV2Funnel";
  import { getDapps } from "$lib/legacy/flows/dappsExplorer/dapps";
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
  import { slide, fade, scale } from "svelte/transition";
  import AccessLevelSelector from "$lib/components/ui/AccessLevelSelector.svelte";
  import type { AccessLevel } from "$lib/utils/accessLevel";
  import { accessLevelStore } from "$lib/stores/access-level.store";
  import { READ_ONLY_MODE } from "$lib/state/featureFlags";
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import EditAccount from "$lib/components/views/EditAccount.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import SessionDurationSelect from "$lib/components/ui/SessionDurationSelect.svelte";
  import {
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

  type PRIMARY_ACCOUNT_NUMBER = undefined;
  const MAX_ACCOUNTS = 5;

  // The largest duration the picker offers: the app's requested value, capped
  // at the 30-day maximum, or the maximum itself when the app didn't request
  // one. Defaulting the selection to it means a plain "Continue" grants exactly
  // what the app asked for (as before this picker existed); the user can shorten
  // it first.
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
    }
  });

  let isCreateAccountDialogVisible = $state(false);
  let isEditAccountDialogVisibleForNumber = $state<
    AccountNumber | PRIMARY_ACCOUNT_NUMBER | null
  >(null);

  const isEditAccountDialogVisibleFor = $derived(
    accounts?.find(
      (account) =>
        account.account_number[0] === isEditAccountDialogVisibleForNumber,
    ),
  );
  const isAccountLimitReached = $derived(
    accounts !== undefined && accounts.length >= 5,
  );
  const dapps = getDapps();
  const application = $derived(
    dapps.find((dapp) => dapp.hasOrigin(displayOrigin))?.name,
  );
  const dappName = $derived(application ?? new URL(displayOrigin).hostname);
  const primaryAccountName = $derived(
    application !== undefined ? $t`My ${application} account` : $t`My account`,
  );
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
        selectedMaxTimeToLive,
      );
    } catch (error) {
      handleError(error);
    } finally {
      isAuthenticatingDefault = false;
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
      onAuthorize(
        Promise.resolve(accountNumber),
        effectiveAccessLevel,
        selectedMaxTimeToLive,
      );
    } catch (error) {
      handleError(error);
    } finally {
      isAuthenticatingDefault = false;
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
        account.name !== (accounts[index].name[0] ?? primaryAccountName);
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

{#snippet accountListItem(account: AccountInfo)}
  {@const name = account.name[0] ?? primaryAccountName}
  <div in:slide={{ duration: 300, delay: 300, axis: "y" }}>
    <div
      in:scale={{ duration: 300, delay: 450, start: 0.95 }}
      class={[
        // Layout
        "relative flex flex-row items-stretch gap-3",
        // Styling
        "border-border-secondary bg-bg-primary rounded-sm border shadow-xs",
        // Animate scale and shadow
        "transition-all duration-100 ease-out",
        // While an access level still needs choosing, the card can't authorize
        // yet: drop the interactive affordances and dim it.
        mustChooseAccess
          ? "opacity-60"
          : [
              // Apply scale effect on hover
              "hover:z-1 hover:scale-102 hover:shadow-md hover:shadow-black/5",
              // Also apply scale effect on keyboard focus besides hover
              "has-focus-visible:z-1 has-focus-visible:scale-102 has-focus-visible:shadow-md has-focus-visible:shadow-black/5",
              // When cursor is between two items, we still want an item
              // to be scaled and the cursor to be a pointer nonetheless.
              "cursor-pointer after:absolute after:-inset-2 after:-z-1",
            ],
      ]}
    >
      <button
        onclick={() => handleContinueAs(account.account_number[0])}
        disabled={mustChooseAccess}
        class="flex flex-1 flex-row items-center text-start outline-0"
        aria-label={$t`Continue with ${name}`}
      >
        <span class="text-text-primary flex-1 py-3 ps-5 text-sm font-semibold">
          {name}
        </span>
        {#if account.account_number[0] === defaultAccountNumber}
          <Badge size="sm">{$t`Default`}</Badge>
        {/if}
      </button>
      <button
        class="btn btn-tertiary btn-sm btn-icon my-3 me-3 shrink-0"
        onclick={() =>
          (isEditAccountDialogVisibleForNumber = account.account_number[0])}
        aria-label={$t`Edit ${name}`}
      >
        <PencilIcon class="size-5" />
      </button>
    </div>
  </div>
{/snippet}

{#snippet accountList(accounts: AccountInfo[])}
  <div class="col-start-1 row-start-1" out:fade={{ duration: 100 }}>
    <div class="!min-h-18" out:slide={{ axis: "y", duration: 300 }}>
      <div class="!min-h-18" in:slide={{ axis: "y", duration: 300 }}>
        <div class="flex flex-col gap-2 pb-6" in:fade={{ duration: 300 }}>
          <ul class="contents" aria-label={$t`Choose an account`}>
            {#each accounts as account (account.account_number[0])}
              <li class="contents">
                {@render accountListItem(account)}
              </li>
            {/each}
          </ul>
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
            <div class="mt-3 shrink-0">
              <button
                class="btn btn-tertiary w-full"
                onclick={() => (isCreateAccountDialogVisible = true)}
                disabled={isAccountLimitReached}
              >
                <PlusIcon class="size-5" />
                {$t`Add another account`}
              </button>
            </div>
          </Tooltip>
        </div>
      </div>
    </div>
  </div>
{/snippet}

{#snippet continueDefault()}
  <div
    class="col-start-1 row-start-1 pb-6"
    in:fade={{ duration: 200, delay: 100 }}
  >
    <button
      class="btn btn-primary btn-xl w-full"
      onclick={handleContinueDefault}
      disabled={isAuthenticatingDefault || mustChooseAccess}
    >
      {#if isAuthenticatingDefault}
        <ProgressRing />
        <span>{$t`Authenticating...`}</span>
      {:else}
        <span>{continueLabel ?? $t`Continue`}</span>
      {/if}
    </button>
  </div>
{/snippet}

<div class="flex flex-1 flex-col">
  {#if header}
    {@render header()}
  {:else}
    <AuthorizeHeader origin={displayOrigin} />
    <h1 class="text-text-primary mb-2 self-start text-2xl font-medium">
      {$t`Continue to ${dappName}`}
    </h1>
    <p class="text-text-secondary mb-6 self-start text-sm">
      {$t`with your Internet Identity`}
    </p>
  {/if}
  {#if isMultipleAccountsEnabled}
    <!-- Session: how long the sign-in lasts before the user must sign in
         again, capped at the duration the app requested (see `ceilingSeconds`).
         Revealed alongside the account choices when "all options" is on. -->
    <div class="border-border-tertiary mb-6 flex flex-col border-t pt-4">
      <span class="text-text-primary mb-0.5 text-base font-medium">
        {$t`Session duration`}
      </span>
      <div class="flex flex-row items-center justify-between gap-2">
        <span class="text-text-tertiary text-base">
          {$t`until you have to sign in again`}
        </span>
        <SessionDurationSelect
          maxSeconds={ceilingSeconds}
          bind:value={selectedTtlSeconds}
          disabled={isAuthenticatingDefault}
        />
      </div>
    </div>
    <span class="text-text-primary mb-3 self-start text-base font-medium">
      {$t`Available accounts`}
    </span>
  {/if}
  <div class="grid">
    <!-- Nested if/else conditions breaks transitions, so they've been flattened here-->
    {#if isMultipleAccountsEnabled && accounts !== undefined}
      {@render accountList(accounts)}
    {:else if isMultipleAccountsEnabled}
      <div
        class="col-start-1 row-start-1 flex min-h-18 items-center justify-center pb-6"
        in:fade={{ duration: 100, delay: 300 }}
        out:fade={{ duration: 100 }}
      >
        <ProgressRing class="text-fg-tertiary size-8" />
      </div>
    {:else}
      {@render continueDefault()}
    {/if}
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
    name: isEditAccountDialogVisibleFor.name[0] ?? primaryAccountName,
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
