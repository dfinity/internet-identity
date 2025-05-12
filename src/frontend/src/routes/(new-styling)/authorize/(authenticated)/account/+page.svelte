<script lang="ts">
  import type { PageProps } from "./$types";
  import { throwCanisterError } from "$lib/utils/utils";
  import { authenticatedStore } from "$lib/stores/authentication.store";
  import { lastUsedIdentitiesStore } from "$lib/stores/last-used-identities.store";
  import {
    authorizationStore,
    authorizationContextStore,
  } from "$lib/stores/authorization.store";
  import RadioCard from "$lib/components/ui/RadioCard.svelte";
  import Avatar from "$lib/components/ui/Avatar.svelte";
  import { PlusIcon } from "@lucide/svelte";
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import { handleError } from "../../error";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import Ellipsis from "$lib/components/utils/Ellipsis.svelte";
  import DappLogo from "$lib/components/ui/DappLogo.svelte";
  import { getDapps } from "$lib/flows/dappsExplorer/dapps";
  import { nonNullish } from "@dfinity/utils";

  const { data }: PageProps = $props();
  const { accounts } = data;

  const dapps = getDapps();
  const img = $derived(
    dapps.find((dapp) =>
      dapp.hasOrigin($authorizationContextStore.requestOrigin),
    )?.logoSrc,
  );

  let selectedAccountNumber = $state(accounts[0].account_number[0]);
  let creatingAccount = $state(false);
  let createAccountInputRef = $state<HTMLInputElement>();
  let newAccountName = $state("");
  let loading = $state(false);

  const handleCreateAccountFocus = () => {
    if (loading) {
      return;
    }
    creatingAccount = true;
    createAccountInputRef?.focus();
  };
  const handleSelectAccount = (accountNumber: bigint | undefined) => {
    if (loading) {
      return;
    }
    selectedAccountNumber = accountNumber;
    newAccountName = "";
    creatingAccount = false;
  };
  const handleContinue = async () => {
    try {
      loading = true;
      const account = creatingAccount
        ? await $authenticatedStore.actor
            .create_account(
              $authenticatedStore.identityNumber,
              $authorizationContextStore.authRequest.derivationOrigin ??
                $authorizationContextStore.requestOrigin,
              newAccountName,
            )
            .then(throwCanisterError)
        : accounts.find(
            ({ account_number }) => account_number[0] === selectedAccountNumber,
          )!;
      lastUsedIdentitiesStore.addLastUsedAccount({
        origin:
          $authorizationContextStore.authRequest.derivationOrigin ??
          $authorizationContextStore.requestOrigin,
        identityNumber: $authenticatedStore.identityNumber,
        accountNumber: account.account_number[0],
        name: account.name[0],
      });
      await authorizationStore.authorize(account.account_number[0]);
    } catch (error) {
      loading = false;
      handleError(error);
    }
  };
</script>

<form class="flex flex-col" onsubmit={(e) => e.preventDefault()}>
  <div class="mb-6 flex flex-col gap-2">
    <h1 class="text-gray-light-900 dark:text-gray-dark-25 text-2xl font-medium">
      Select an account
    </h1>
    <p class="text-gray-light-700 dark:text-gray-dark-50 text-sm">
      you'd like to sign in with
    </p>
  </div>
  <div class="mb-3 flex h-8 items-center justify-between gap-8">
    <p
      class="text-gray-light-900 dark:text-gray-dark-25 w-0 flex-1 text-sm font-semibold"
    >
      <Ellipsis
        text={$authorizationContextStore.requestOrigin}
        position="middle"
      />
    </p>
    {#if nonNullish(img)}
      <img src={img} alt="" aria-hidden="true" class="h-8 rounded-xl" />
    {/if}
  </div>
  <div
    class="mb-6 flex flex-col items-stretch gap-1.5 self-stretch"
    role="radiogroup"
  >
    {#each accounts as account}
      <button
        onclick={() => handleSelectAccount(account.account_number[0])}
        type="button"
      >
        <RadioCard
          checked={account.account_number[0] === selectedAccountNumber &&
            !creatingAccount}
        >
          <Avatar size="sm">
            {account.name[0]?.slice(0, 1).toUpperCase() ?? "A"}
          </Avatar>
          <span class="overflow-hidden overflow-ellipsis whitespace-nowrap">
            {account.name[0] ?? "Primary account"}
          </span>
        </RadioCard>
      </button>
    {/each}
    <RadioCard
      checked={creatingAccount}
      class={[creatingAccount ? "cursor-text" : "cursor-pointer"]}
      onclick={handleCreateAccountFocus}
    >
      <FeaturedIcon size="sm">
        <PlusIcon size="1.25rem" />
      </FeaturedIcon>
      <input
        bind:this={createAccountInputRef}
        bind:value={newAccountName}
        placeholder={creatingAccount
          ? "Enter account name"
          : "Create additional account"}
        onfocus={handleCreateAccountFocus}
        minlength="1"
        autocomplete="off"
        spellCheck="false"
        disabled={creatingAccount && loading}
        class={[
          // Base/light/dark
          "h-8 w-full border-none bg-transparent p-0 text-sm font-medium ring-0 !outline-none",
          !creatingAccount && "cursor-pointer",
          "text-gray-light-900",
          creatingAccount
            ? "placeholder:text-gray-light-400"
            : "placeholder:text-gray-light-900",
          "dark:text-gray-dark-25",
          creatingAccount
            ? "dark:placeholder:text-gray-dark-100"
            : "dark:placeholder:text-gray-dark-25",
        ]}
      />
    </RadioCard>
  </div>
  <Button
    onclick={handleContinue}
    variant="primary"
    size="xl"
    type="submit"
    disabled={(newAccountName.length === 0 && creatingAccount) || loading}
    class="mb-3"
  >
    {#if loading}
      <ProgressRing />
      <span>{creatingAccount ? "Creating account..." : "Signing in..."}</span>
    {:else}
      <span>{creatingAccount ? "Create account" : "Sign in"}</span>
    {/if}
  </Button>
  <Button
    href="/authorize/continue"
    variant="tertiary"
    size="xl"
    type="submit"
    disabled={loading}
  >
    Return
  </Button>
</form>
