<script lang="ts">
  import type { PageProps } from "./$types";
  import { throwCanisterError } from "$lib/utils/utils";
  import {
    authenticatedStore,
    isAuthenticatedStore,
  } from "$lib/stores/authentication.store";
  import {
    authorizationStore,
    authorizationContextStore,
  } from "$lib/stores/authorization.store";
  import { PlusIcon } from "@lucide/svelte";
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import { handleError } from "$lib/components/utils/error";
  import { getDapps } from "$lib/flows/dappsExplorer/dapps";
  import { nonNullish } from "@dfinity/utils";
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import CreateAccount from "$lib/components/views/CreateAccount.svelte";
  import Avatar from "$lib/components/ui/Avatar.svelte";
  import ButtonCard from "$lib/components/ui/ButtonCard.svelte";
  import AuthorizeHeader from "$lib/components/ui/AuthorizeHeader.svelte";
  import { goto } from "$app/navigation";
  import { lastUsedIdentitiesStore } from "$lib/stores/last-used-identities.store";
  import type { AccountInfo } from "$lib/generated/internet_identity_types";

  const { data }: PageProps = $props();
  let accounts = $derived(
    data.accounts.sort((a, b) =>
      Number((b.last_used[0] ?? BigInt(0)) - (a.last_used[0] ?? BigInt(0))),
    ),
  );

  const origin = $derived($authorizationContextStore.requestOrigin);
  const dapps = getDapps();
  const dapp = $derived(dapps.find((dapp) => dapp.hasOrigin(origin)));

  let createAccountDialog = $state(false);
  let loading = $state(false);

  const createAccount = async (name: string) => {
    try {
      const account = await $authenticatedStore.actor
        .create_account(
          $authenticatedStore.identityNumber,
          $authorizationContextStore.effectiveOrigin,
          name.trim(),
        )
        .then(throwCanisterError);
      accounts = [...accounts, account];
    } catch (error) {
      handleError(error);
    } finally {
      createAccountDialog = false;
    }
  };

  const continueAs = async (account: AccountInfo) => {
    loading = true;
    lastUsedIdentitiesStore.addLastUsedAccount({
      origin: $authorizationContextStore.effectiveOrigin,
      identityNumber: $authenticatedStore.identityNumber,
      accountNumber: account.account_number[0],
      name: account.name[0],
    });
    await authorizationStore.authorize(account.account_number[0]);
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
          <ButtonCard onclick={() => continueAs(account)} disabled={loading}>
            <Avatar size="sm">
              {(account.name[0] ?? "Primary account").slice(0, 1).toUpperCase()}
            </Avatar>
            <span class="overflow-hidden overflow-ellipsis whitespace-nowrap">
              {account.name[0] ?? "Primary account"}
            </span>
          </ButtonCard>
        </li>
      {/each}
    </ul>
    <ButtonCard onclick={() => (createAccountDialog = true)} disabled={loading}>
      <FeaturedIcon size="sm">
        <PlusIcon size="1.25rem" />
      </FeaturedIcon>
      <span>Create additional account</span>
    </ButtonCard>
  </div>
</div>
{#if createAccountDialog}
  <Dialog onClose={() => (createAccountDialog = false)}>
    <CreateAccount create={createAccount} />
  </Dialog>
{/if}
