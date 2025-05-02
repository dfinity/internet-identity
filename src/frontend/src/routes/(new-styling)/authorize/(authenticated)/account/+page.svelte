<script lang="ts">
  import type { PageProps } from "./$types";
  import type { FocusEventHandler, MouseEventHandler } from "svelte/elements";
  import { nonNullish } from "@dfinity/utils";
  import { ProgressRing } from "@skeletonlabs/skeleton-svelte";
  import { formatLastUsage } from "$lib/utils/time";
  import { throwCanisterError } from "$lib/utils/utils";
  import { invalidate } from "$app/navigation";
  import { authenticationStore } from "$lib/stores/authentication.store";
  import { lastUsedIdentitiesStore } from "$lib/stores/last-used-identities.store";
  import { authorizationStore } from "$lib/stores/authorization.store";

  const { data }: PageProps = $props();
  const { accounts } = data;

  let selectedAccount = $state.raw(accounts[0]);
  let createAccountFocused = $state(false);
  let creatingAccount = $state(false);
  let newAccountName = $state("");
  const handleCreateFocus = () => {
    createAccountFocused = true;
  };
  const handleCreateBlur: FocusEventHandler<HTMLInputElement> = (event) => {
    if (
      creatingAccount ||
      (nonNullish(event.relatedTarget) &&
        event.currentTarget
          .closest("form")
          ?.contains(event.relatedTarget as HTMLElement))
    ) {
      return;
    }
    newAccountName = "";
    createAccountFocused = false;
  };
  const handleCreate: MouseEventHandler<HTMLButtonElement> = async (event) => {
    creatingAccount = true;
    const _account = await $authenticationStore.actor
      .create_account(
        $authenticationStore.identityNumber,
        $authorizationStore.requestOrigin,
        newAccountName,
      )
      .then(throwCanisterError);
    creatingAccount = false;
    newAccountName = "";
    await invalidate("/authorize/account");
  };

  const handleContinue = () => {
    lastUsedIdentitiesStore.addLastUsedAccount({
      origin:
        $authorizationStore.authRequest.derivationOrigin ??
        $authorizationStore.requestOrigin,
      identityNumber: $authenticationStore.identityNumber,
      accountNumber: selectedAccount.account_number[0],
      name: selectedAccount.name[0],
    });
    authorizationStore.authorize(selectedAccount.account_number[0]);
  };
</script>

<div class="flex flex-col items-start">
  <div
    class="mb-6 flex flex-col items-stretch gap-3 self-stretch"
    role="radiogroup"
  >
    {#each accounts as account}
      <button
        onclick={() => (selectedAccount = account)}
        class={[
          "btn box-border flex h-18 flex-col items-start justify-center gap-0 rounded-lg p-4 px-4 text-left transition-none",
          account === selectedAccount
            ? "bg-surface-200-800 border-surface-0 border-2 font-semibold"
            : "preset-outlined-surface-300-700",
        ]}
        role="radio"
        aria-checked={account === selectedAccount}
      >
        <span class="-mt-0.5">{account.name[0] ?? "Primary account"}</span>
        <span class="text-sm opacity-80"
          >Last used: {nonNullish(account.last_used[0])
            ? formatLastUsage(
                new Date(Number(account.last_used[0] / BigInt(1000000))),
              )
            : "never"}</span
        >
      </button>
    {/each}
    <form class="relative flex items-center justify-end">
      <input
        bind:value={newAccountName}
        placeholder={createAccountFocused
          ? "Enter account name"
          : "Create additional account"}
        onfocus={handleCreateFocus}
        onblur={handleCreateBlur}
        class={[
          "input not-focus:placeholder:text-surface-contrast-50-950 ring-surface-300-700 box-border h-15 justify-start rounded-lg py-4 ps-4 pe-16 text-left transition-none not-focus:cursor-pointer",
        ]}
      />
      {#if creatingAccount}
        <div class="absolute me-4.5">
          <ProgressRing
            value={null}
            size="size-4"
            meterStroke="stroke-surface-contrast-50-950"
            trackStroke="stroke-surface-contrast-50-950/0"
          />
        </div>
      {:else if createAccountFocused && newAccountName.length > 0}
        <button
          onclick={handleCreate}
          type="submit"
          class="btn hover:preset-tonal absolute me-3 size-9 p-0"
        >
          ✓
        </button>
      {/if}
    </form>
  </div>
  <button onclick={handleContinue} class="btn preset-filled self-stretch py-2"
    >Continue</button
  >
</div>
