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
  import { PlusIcon } from "@lucide/svelte";
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import { handleError } from "$lib/components/utils/error";
  import Ellipsis from "$lib/components/utils/Ellipsis.svelte";
  import { getDapps } from "$lib/flows/dappsExplorer/dapps";
  import { nonNullish } from "@dfinity/utils";
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import CreateAccount from "$lib/components/views/CreateAccount.svelte";
  import { untrack } from "svelte";
  import Avatar from "$lib/components/ui/Avatar.svelte";

  const { data }: PageProps = $props();
  let accounts = $derived(data.accounts);

  const origin = $derived($authorizationContextStore.requestOrigin);
  const hostname = $derived(new URL(origin).hostname);
  const dapps = getDapps();
  const dapp = $derived(dapps.find((dapp) => dapp.hasOrigin(origin)));

  let selectedAccount = $state.raw(untrack(() => accounts[0]));
  let dialog = $state(false);

  const createAccount = async (name: string) => {
    try {
      const account = await $authenticatedStore.actor
        .create_account(
          $authenticatedStore.identityNumber,
          $authorizationContextStore.authRequest.derivationOrigin ??
            $authorizationContextStore.requestOrigin,
          name,
        )
        .then(throwCanisterError);
      accounts = [...accounts, account];
      selectedAccount = accounts.slice(-1)[0];
      dialog = false;
      // await invalidateAll(); TODO: uncomment this once backend is updated
    } catch (error) {
      handleError(error);
      dialog = false;
    }
  };

  const handleContinue = async () => {
    try {
      lastUsedIdentitiesStore.addLastUsedAccount({
        origin:
          $authorizationContextStore.authRequest.derivationOrigin ??
          $authorizationContextStore.requestOrigin,
        identityNumber: $authenticatedStore.identityNumber,
        accountNumber: selectedAccount.account_number[0],
        name: selectedAccount.name[0],
      });
      await authorizationStore.authorize(selectedAccount.account_number[0]);
    } catch (error) {
      handleError(error);
    }
  };
</script>

<div class="flex flex-1 flex-col justify-end">
  <div class="mb-6 flex flex-col gap-2">
    <h1 class="text-text-primary text-2xl font-medium">Select an account</h1>
    <p class="text-text-secondary text-sm">you'd like to sign in with</p>
  </div>
  <div class="mb-3 flex h-8 items-center justify-between gap-8">
    <Ellipsis
      text={hostname}
      position="middle"
      class="text-text-primary w-0 max-w-[75%] flex-1 text-sm font-semibold"
    />
    {#if nonNullish(dapp?.logoSrc)}
      <img
        src={dapp.logoSrc}
        alt=""
        aria-hidden="true"
        class="h-8 rounded-xl"
      />
    {/if}
  </div>
  <div
    class="mb-6 flex flex-col items-stretch gap-1.5 self-stretch"
    role="radiogroup"
  >
    {#each accounts as account}
      <RadioCard
        onclick={() => (selectedAccount = account)}
        checked={account === selectedAccount}
      >
        <Avatar size="sm">
          {account.name[0]?.slice(0, 1).toUpperCase() ?? "A"}
        </Avatar>
        <span class="overflow-hidden overflow-ellipsis whitespace-nowrap">
          {account.name[0] ?? "Primary account"}
        </span>
      </RadioCard>
    {/each}
    <RadioCard onclick={() => (dialog = true)}>
      <FeaturedIcon size="sm">
        <PlusIcon size="1.25rem" />
      </FeaturedIcon>
      <span>Create additional account</span>
    </RadioCard>
  </div>
  <Button
    onclick={handleContinue}
    variant="primary"
    size="xl"
    type="submit"
    class="mb-3"
  >
    Continue
  </Button>
</div>
{#if dialog}
  <Dialog onClose={() => (dialog = false)}>
    <CreateAccount create={createAccount} />
  </Dialog>
{/if}
