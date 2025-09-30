<script lang="ts">
  import type { PageProps } from "./$types";
  import { isCanisterError, throwCanisterError } from "$lib/utils/utils";
  import {
    authenticatedStore,
    isAuthenticatedStore,
  } from "$lib/stores/authentication.store";
  import {
    authorizationStore,
    authorizationContextStore,
  } from "$lib/stores/authorization.store";
  import { Edit2, HelpCircleIcon, PlusIcon } from "@lucide/svelte";
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import { handleError } from "$lib/components/utils/error";
  import { getDapps } from "$lib/legacy/flows/dappsExplorer/dapps";
  import { nonNullish } from "@dfinity/utils";
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import CreateAccount from "$lib/components/views/CreateAccount.svelte";
  import Avatar from "$lib/components/ui/Avatar.svelte";
  import ButtonCard from "$lib/components/ui/ButtonCard.svelte";
  import AuthorizeHeader from "$lib/components/ui/AuthorizeHeader.svelte";
  import { goto } from "$app/navigation";
  import { lastUsedIdentitiesStore } from "$lib/stores/last-used-identities.store";
  import type {
    AccountInfo,
    UpdateAccountError,
  } from "$lib/generated/internet_identity_types";
  import Tooltip from "$lib/components/ui/Tooltip.svelte";
  import { AUTH_FLOW_UPDATES } from "$lib/state/featureFlags";
  import Button from "$lib/components/ui/Button.svelte";
  import Toggle from "$lib/components/ui/Toggle.svelte";
  import Popover from "$lib/components/ui/Popover.svelte";
  import EditAccount from "$lib/components/views/EditAccount.svelte";

  const { data }: PageProps = $props();
  const { defaultAccount } = data;
  let accounts = $derived(
    data.accounts.sort((a, b) =>
      Number((b.last_used[0] ?? BigInt(0)) - (a.last_used[0] ?? BigInt(0))),
    ),
  );

  let toggled = $state<boolean>(true);
  let showPrivacyPopover = $state<boolean>(false);
  let popoverAnchorRef = $state<HTMLDivElement>();

  const isAccountLimitReached = $derived(accounts.length >= 5);
  const origin = $derived($authorizationContextStore.requestOrigin);
  const dapps = getDapps();
  const dapp = $derived(dapps.find((dapp) => dapp.hasOrigin(origin)));

  let isCreateAccountDialogVisible = $state(false);
  let isEditAccountDialogVisible = $state<boolean>(false);
  let isAuthorizing = $state(false);
  let tooltipAnchorRef = $state<HTMLElement>();
  let accountToEdit = $state<AccountInfo | undefined>(undefined);

  const createAccount = async (name: string, isDefault?: boolean) => {
    try {
      const account = await $authenticatedStore.actor
        .create_account(
          $authenticatedStore.identityNumber,
          $authorizationContextStore.effectiveOrigin,
          name.trim(),
        )
        .then(throwCanisterError);
      if (isDefault) {
        await $authenticatedStore.actor
          .set_default_account(
            $authenticatedStore.identityNumber,
            $authorizationContextStore.effectiveOrigin,
            account.account_number,
          )
          .then(throwCanisterError);
      }
      accounts = [...accounts, account];
    } catch (error) {
      handleError(error);
    } finally {
      isCreateAccountDialogVisible = false;
    }
  };

  const editAccount = async (
    account: AccountInfo,
    name: string,
    isDefault?: boolean,
  ) => {
    try {
      // TODO: fix account.account_number = []
      // currently when requesting to update_account against a primary account
      // as there is no account number is causes this to panic
      await $authenticatedStore.actor
        .update_account(
          $authenticatedStore.identityNumber,
          $authorizationContextStore.effectiveOrigin,
          account.account_number,
          { name: [name.trim()] },
        )
        .then(throwCanisterError);
      if (isDefault) {
        await $authenticatedStore.actor
          .set_default_account(
            $authenticatedStore.identityNumber,
            $authorizationContextStore.effectiveOrigin,
            account.account_number,
          )
          .then(throwCanisterError);
      }
      accounts = [...accounts, account];
      isEditAccountDialogVisible = false;
      accountToEdit = undefined;
    } catch (error) {
      handleError(error);
    }
  };

  const continueAs = async (account: AccountInfo) => {
    isAuthorizing = true;
    lastUsedIdentitiesStore.addLastUsedAccount({
      origin: $authorizationContextStore.effectiveOrigin,
      identityNumber: $authenticatedStore.identityNumber,
      accountNumber: account.account_number[0],
      name: account.name[0],
    });
    await authorizationStore.authorize(account.account_number[0]);
  };

  const handleDisableMultipleAccounts = async () => {
    toggled = false;
    await goto("/authorize/continue");
  };

  const handleEditAccount = async (account: AccountInfo) => {
    accountToEdit = account;
    isEditAccountDialogVisible = true;
  };

  $effect(() => {
    if (!$isAuthenticatedStore) {
      goto("/authorize/continue", {
        replaceState: true,
        state: { disableNavigationAnimation: true },
      });
    }
  });
</script>

<div class="flex flex-1 flex-col justify-end">
  <AuthorizeHeader origin={$authorizationContextStore.requestOrigin} />
  <div class="mb-6 flex flex-col gap-2">
    <h1 class="text-text-primary text-2xl font-medium">Choose account</h1>
    <p class="text-text-secondary self-start text-sm">
      <span>or create another for</span>
      {#if nonNullish(dapp?.name)}
        <b>{dapp.name}</b>
      {:else}
        <span>this app</span>
      {/if}
    </p>
  </div>
  <div class="flex flex-col items-stretch gap-1.5 self-stretch">
    <ul class="contents">
      {#each accounts as account}
        <li class="contents">
          <ButtonCard
            onclick={() => continueAs(account)}
            disabled={isAuthorizing}
            class="!justify-between  has-[.no-card-hover:hover]:hover:bg-transparent"
          >
            <div class="flex flex-row items-center gap-3">
              <Avatar size="sm">
                {(account.name[0] ?? "Primary account")
                  .slice(0, 1)
                  .toUpperCase()}
              </Avatar>
              <span class="overflow-hidden overflow-ellipsis whitespace-nowrap">
                {account.name[0] ?? "Primary account"}
              </span>
            </div>
            {#if $AUTH_FLOW_UPDATES}
              <div class="relative flex flex-row items-center gap-2 pr-12">
                {#if account.account_number[0] === defaultAccount.account_number[0]}
                  <div
                    class="border-border-tertiary bg-bg-primary grid place-items-center rounded-full border px-2 py-0.5"
                  >
                    <p class="text-text-secondary text-xs font-medium">
                      Default
                    </p>
                  </div>
                {/if}
                <div class="absolute right-0">
                  <Button
                    variant="tertiary"
                    class="no-card-hover"
                    onclick={(event) => {
                      event.stopPropagation();
                      handleEditAccount(account);
                    }}
                  >
                    <Edit2 size={20}></Edit2>
                  </Button>
                </div>
              </div>
            {/if}
          </ButtonCard>
        </li>
      {/each}
    </ul>
    <Tooltip
      label="Limit reached"
      description="You’ve reached the maximum of 5 accounts."
      direction="down"
      align="start"
      anchor={tooltipAnchorRef}
      hidden={!isAccountLimitReached}
    >
      {#if $AUTH_FLOW_UPDATES}
        <Button
          variant="tertiary"
          class="mt-2"
          onclick={() => (isCreateAccountDialogVisible = true)}
        >
          <PlusIcon size="1.25rem" />
          <span>Add another account</span>
        </Button>
      {:else}
        <ButtonCard
          onclick={() => (isCreateAccountDialogVisible = true)}
          disabled={isAuthorizing || isAccountLimitReached}
        >
          <FeaturedIcon
            bind:element={tooltipAnchorRef}
            size="sm"
            class={[isAccountLimitReached ? "opacity-50" : ""]}
          >
            <PlusIcon size="1.25rem" />
          </FeaturedIcon>
          <span>Create additional account</span>
        </ButtonCard>
      {/if}
    </Tooltip>
    {#if $AUTH_FLOW_UPDATES}
      <div class="mt-4.5 flex flex-col gap-6">
        <div class="border-border-tertiary border-t"></div>
        <div class="flex flex-row items-center justify-between gap-4">
          <Toggle
            {toggled}
            onClick={handleDisableMultipleAccounts}
            label="Enable multiple accounts"
          ></Toggle>
          <div bind:this={popoverAnchorRef}>
            <Button
              variant="tertiary"
              onclick={() => (showPrivacyPopover = !showPrivacyPopover)}
            >
              <HelpCircleIcon
                size="20"
                class="text-text-primary stroke-fg-tertiary"
              />
            </Button>
          </div>
          {#if showPrivacyPopover}
            <Popover
              anchor={popoverAnchorRef}
              direction="up"
              align="end"
              distance="10px"
              class="gap-0.5"
              onClose={() => (showPrivacyPopover = false)}
            >
              <p class="text-text-primary text-xs font-semibold">
                Enable Multiple accounts
              </p>
              <p class="text-text-secondary text-xs font-medium">
                By enabling this feature, you can create more than one account
                for a single app. Easily switch between accounts (e.g. work,
                personal, or demo).
              </p>
            </Popover>
          {/if}
        </div>
      </div>
    {/if}
  </div>
</div>
{#if isCreateAccountDialogVisible}
  <Dialog onClose={() => (isCreateAccountDialogVisible = false)}>
    <CreateAccount create={createAccount} />
  </Dialog>
{/if}

{#if isEditAccountDialogVisible && accountToEdit}
  <Dialog
    onClose={() => {
      accountToEdit = undefined;
      isEditAccountDialogVisible = false;
    }}
  >
    <EditAccount
      handleEdit={editAccount}
      account={accountToEdit}
      {defaultAccount}
    />
  </Dialog>
{/if}
