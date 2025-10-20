<script lang="ts">
  import { lastUsedIdentitiesStore } from "$lib/stores/last-used-identities.store";
  import {
    authorizationStore,
    authorizationContextStore,
  } from "$lib/stores/authorization.store";
  import { HelpCircleIcon, PlusIcon, PencilIcon } from "@lucide/svelte";
  import { handleError } from "$lib/components/utils/error";
  import AuthorizeHeader from "$lib/components/ui/AuthorizeHeader.svelte";
  import SystemOverlayBackdrop from "$lib/components/utils/SystemOverlayBackdrop.svelte";
  import {
    AuthenticationV2Events,
    authenticationV2Funnel,
  } from "$lib/utils/analytics/authenticationV2Funnel";
  import { nonNullish } from "@dfinity/utils";
  import { getDapps } from "$lib/legacy/flows/dappsExplorer/dapps";
  import { AuthLastUsedFlow } from "$lib/flows/authLastUsedFlow.svelte";
  import { plural, t } from "$lib/stores/locale.store";
  import Button from "$lib/components/ui/Button.svelte";
  import Toggle from "$lib/components/ui/Toggle.svelte";
  import Tooltip from "$lib/components/ui/Tooltip.svelte";
  import {
    authenticationStore,
    isAuthenticatedStore,
  } from "$lib/stores/authentication.store";
  import { throwCanisterError } from "$lib/utils/utils";
  import type {
    AccountInfo,
    AccountNumber,
  } from "$lib/generated/internet_identity_types";
  import Badge from "$lib/components/ui/Badge.svelte";
  import { slide, fade, scale } from "svelte/transition";
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import CreateAccount from "$lib/components/views/CreateAccount.svelte";
  import EditAccount from "$lib/components/views/EditAccount.svelte";

  const PRIMARY_ACCOUNT_NUMBER = undefined;
  const MAX_ACCOUNTS = 5;

  let defaultAccountNumber = $state<
    AccountNumber | typeof PRIMARY_ACCOUNT_NUMBER | null
  >(null);
  let accounts = $state<AccountInfo[]>();
  let isMultipleAccountsEnabled = $state(false);
  // Clear old accounts data when user toggles switch off
  $effect(() => {
    if (!isMultipleAccountsEnabled) {
      accounts = undefined;
    }
  });

  let isCreateAccountDialogVisible = $state(false);
  let isEditAccountDialogVisibleForNumber = $state<
    AccountNumber | typeof PRIMARY_ACCOUNT_NUMBER | null
  >(null);

  const isEditAccountDialogVisibleFor = $derived(
    accounts?.find(
      (account) =>
        account.account_number[0] === isEditAccountDialogVisibleForNumber,
    ),
  );
  const isAccountLimitReached = $derived(
    nonNullish(accounts) && accounts.length >= 5,
  );
  const dapps = getDapps();
  const application = $derived(
    dapps.find((dapp) =>
      dapp.hasOrigin($authorizationContextStore.requestOrigin),
    )?.name,
  );
  const primaryAccountName = $derived(
    nonNullish(application) ? $t`My ${application} account` : $t`My account`,
  );
  const authLastUsedFlow = new AuthLastUsedFlow();
  // Initialize the flow every time the identity changes
  $effect(() => {
    authLastUsedFlow.init([$lastUsedIdentitiesStore.selected!.identityNumber]);
  });

  const handleContinueDefault = async () => {
    try {
      if (!$isAuthenticatedStore) {
        await authLastUsedFlow.authenticate($lastUsedIdentitiesStore.selected!);
      }
      if (defaultAccountNumber === null) {
        const { identityNumber, actor } = $authenticationStore!;
        const { effectiveOrigin } = $authorizationContextStore;
        defaultAccountNumber = (
          await actor
            .get_default_account(identityNumber, effectiveOrigin)
            .then(throwCanisterError)
        ).account_number[0];
      }
      await authorizationStore.authorize(defaultAccountNumber);
    } catch (error) {
      handleError(error);
    }
  };
  const handleContinueAs = async (
    accountNumber: AccountNumber | typeof PRIMARY_ACCOUNT_NUMBER,
  ) => {
    try {
      await authorizationStore.authorize(accountNumber);
    } catch (error) {
      handleError(error);
    }
  };
  const handleEnableMultipleAccounts = async () => {
    try {
      if (!$isAuthenticatedStore) {
        await authLastUsedFlow.authenticate($lastUsedIdentitiesStore.selected!);
      }
      const { identityNumber, actor } = $authenticationStore!;
      const { effectiveOrigin } = $authorizationContextStore;
      const values = await Promise.all([
        actor
          .get_accounts(identityNumber, effectiveOrigin)
          .then(throwCanisterError),
        actor
          .get_default_account(identityNumber, effectiveOrigin)
          .then(throwCanisterError),
      ]);
      accounts = values[0].sort((a, b) => {
        // Undefined should come last (new/unused accounts at the bottom),
        // defined should sort descending (most recently used first).
        const aVal = a.last_used[0] ?? BigInt(-1);
        const bVal = b.last_used[0] ?? BigInt(-1);
        return bVal > aVal ? 1 : bVal < aVal ? -1 : 0;
      });
      defaultAccountNumber = values[1].account_number[0];
    } catch (error) {
      isMultipleAccountsEnabled = false;
      handleError(error);
    }
  };
  const handleCreateAccount = async (
    name: string,
    isDefaultSignIn: boolean,
  ) => {
    try {
      const { identityNumber, actor } = $authenticationStore!;
      const createdAccount = await actor
        .create_account(
          identityNumber,
          $authorizationContextStore.effectiveOrigin,
          name,
        )
        .then(throwCanisterError);
      if (isDefaultSignIn) {
        defaultAccountNumber = (
          await actor
            .set_default_account(
              identityNumber,
              $authorizationContextStore.effectiveOrigin,
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
  const handleEditAccount = async (changes: {
    name?: string;
    isDefaultSignIn?: boolean;
  }) => {
    try {
      const { identityNumber, actor } = $authenticationStore!;
      if (nonNullish(changes.name)) {
        const updatedAccount = await actor
          .update_account(
            identityNumber,
            $authorizationContextStore.effectiveOrigin,
            nonNullish(isEditAccountDialogVisibleForNumber)
              ? [isEditAccountDialogVisibleForNumber]
              : [],
            { name: [changes.name] },
          )
          .then(throwCanisterError);
        accounts = accounts?.map((account) =>
          account.account_number[0] === updatedAccount.account_number[0]
            ? updatedAccount
            : account,
        );
      }
      if (nonNullish(changes.isDefaultSignIn) && changes.isDefaultSignIn) {
        defaultAccountNumber = (
          await actor
            .set_default_account(
              identityNumber,
              $authorizationContextStore.effectiveOrigin,
              nonNullish(isEditAccountDialogVisibleForNumber)
                ? [isEditAccountDialogVisibleForNumber]
                : [],
            )
            .then(throwCanisterError)
        ).account_number[0];
      }
    } catch (error) {
      handleError(error);
    } finally {
      isEditAccountDialogVisibleForNumber = null;
    }
  };

  $effect(() => {
    authenticationV2Funnel.trigger(AuthenticationV2Events.ContinueAsScreen);
  });
</script>

{#snippet accountListItem(account: AccountInfo)}
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
        // Apply scale effect on hover
        "hover:z-1 hover:scale-102 hover:shadow-md hover:shadow-black/5",
        // Also apply scale effect on keyboard focus besides hover
        "has-focus-visible:z-1 has-focus-visible:scale-102 has-focus-visible:shadow-md has-focus-visible:shadow-black/5",
        // When cursor is between two items, we still want an item
        // to be scaled and the cursor to be a pointer nonetheless.
        "cursor-pointer after:absolute after:-inset-2 after:-z-1",
      ]}
    >
      <button
        onclick={() => handleContinueAs(account.account_number[0])}
        class="flex flex-1 flex-row items-center text-start outline-0"
      >
        <span class="text-text-primary flex-1 py-3 ps-5 text-sm font-semibold">
          {account.name[0] ?? primaryAccountName}
        </span>
        {#if account.account_number[0] === defaultAccountNumber}
          <Badge size="sm">{$t`Default`}</Badge>
        {/if}
      </button>
      <Button
        onclick={() =>
          (isEditAccountDialogVisibleForNumber = account.account_number[0])}
        variant="tertiary"
        size="sm"
        iconOnly
        class="my-3 me-3 shrink-0"
      >
        <PencilIcon class="size-5" />
      </Button>
    </div>
  </div>
{/snippet}

<div class="flex flex-1 flex-col">
  <AuthorizeHeader origin={$authorizationContextStore.requestOrigin} />
  <h1 class="text-text-primary mb-2 self-start text-2xl font-medium">
    {$t`Sign in`}
  </h1>
  <p class="text-text-secondary mb-6 self-start text-sm">
    {$t`with your Internet Identity`}
  </p>
  <div class="grid">
    {#if isMultipleAccountsEnabled}
      <div class="col-start-1 row-start-1" out:fade={{ duration: 100 }}>
        <div class="!min-h-18" out:slide={{ axis: "y", duration: 300 }}>
          {#if nonNullish(accounts)}
            <div class="!min-h-18" in:slide={{ axis: "y", duration: 300 }}>
              <div class="flex flex-col gap-2 pb-6" in:fade={{ duration: 300 }}>
                {#each accounts as account (account.account_number[0])}
                  {@render accountListItem(account)}
                {/each}
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
                    <Button
                      onclick={() => (isCreateAccountDialogVisible = true)}
                      variant="tertiary"
                      disabled={isAccountLimitReached}
                      class="w-full"
                    >
                      <PlusIcon class="size-5" />
                      {$t`Add another account`}
                    </Button>
                  </div>
                </Tooltip>
              </div>
            </div>
          {/if}
        </div>
      </div>
    {:else}
      <div
        class="col-start-1 row-start-1 pb-6"
        in:fade={{ duration: 200, delay: 100 }}
      >
        <Button onclick={handleContinueDefault} size="xl" class="w-full">
          {$t`Continue`}
        </Button>
      </div>
    {/if}
  </div>
  <div class="border-border-tertiary mb-6 border-t"></div>
  <div class="flex flex-row items-center">
    <Toggle
      bind:checked={isMultipleAccountsEnabled}
      onclick={isMultipleAccountsEnabled
        ? undefined
        : handleEnableMultipleAccounts}
      label={$t`Enable multiple accounts`}
      size="sm"
    />
    <Tooltip
      label={$t`Multiple accounts`}
      description={$t`By enabling this feature, you can create more than one account for a single app. Easily switch between accounts (e.g. work, personal, or demo).`}
      direction="up"
      align="end"
      offset="0rem"
      class="max-w-80"
    >
      <Button
        variant="tertiary"
        iconOnly
        size="sm"
        class="ms-auto !cursor-default !rounded-full"
        aria-label={$t`More information about multiple accounts`}
      >
        <HelpCircleIcon class="size-5" />
      </Button>
    </Tooltip>
  </div>
</div>

{#if authLastUsedFlow.systemOverlay}
  <SystemOverlayBackdrop />
{/if}

{#if isCreateAccountDialogVisible}
  <Dialog onClose={() => (isCreateAccountDialogVisible = false)}>
    <CreateAccount create={handleCreateAccount} />
  </Dialog>
{/if}

{#if nonNullish(isEditAccountDialogVisibleFor)}
  <Dialog onClose={() => (isEditAccountDialogVisibleForNumber = null)}>
    <EditAccount
      name={isEditAccountDialogVisibleFor.name[0] ?? primaryAccountName}
      isDefaultSignIn={defaultAccountNumber ===
        isEditAccountDialogVisibleForNumber}
      save={handleEditAccount}
    />
  </Dialog>
{/if}
