<script lang="ts">
  import type { PageProps } from "./$types";
  import type { FocusEventHandler } from "svelte/elements";
  import { nonNullish } from "@dfinity/utils";
  import { formatLastUsage } from "$lib/utils/time";
  import { throwCanisterError } from "$lib/utils/utils";
  import { authenticationStore } from "$lib/stores/authentication.store";
  import { lastUsedIdentitiesStore } from "$lib/stores/last-used-identities.store";
  import { authorizationStore } from "$lib/stores/authorization.store";

  const { data }: PageProps = $props();

  const origin =
    $authorizationStore.authRequest.derivationOrigin ??
    $authorizationStore.requestOrigin;
  let accounts = $derived(data.accounts);
  let selectedAccountNumber = $state(accounts[0].account_number[0]);
  let accountRefs = $state<HTMLButtonElement[]>([]);
  let createAccountFocused = $state(false);
  let authorizing = $state(false);
  let newAccountName = $state("");

  const handleCreateFocus = () => {
    createAccountFocused = true;
  };
  const handleCreateBlur: FocusEventHandler<HTMLInputElement> = (event) => {
    if (
      authorizing ||
      (event.relatedTarget as HTMLElement)?.closest("button")
    ) {
      return;
    }
    newAccountName = "";
    createAccountFocused = false;
  };
  const handleSelectAccount = (accountNumber: bigint | undefined) => {
    selectedAccountNumber = accountNumber;
    newAccountName = "";
    createAccountFocused = false;
  };
  const handleContinue = async () => {
    authorizing = true;
    const account = newAccountName
      ? await $authenticationStore.actor
          .create_account(
            $authenticationStore.identityNumber,
            origin,
            newAccountName,
          )
          .then(throwCanisterError)
      : accounts.find(
          ({ account_number }) => account_number[0] === selectedAccountNumber,
        )!;
    lastUsedIdentitiesStore.addLastUsedAccount({
      origin,
      identityNumber: $authenticationStore.identityNumber,
      accountNumber: account.account_number[0],
      name: account.name[0],
    });
    await authorizationStore.authorize(account.account_number[0]);
  };
</script>

<form class="flex flex-col items-start">
  <div
    class="mb-6 flex flex-col items-stretch gap-3 self-stretch"
    role="radiogroup"
  >
    {#each accounts as account, index}
      {@const selected = account.account_number[0] === selectedAccountNumber}
      <button
        bind:this={accountRefs[index]}
        onclick={() => handleSelectAccount(account.account_number[0])}
        class={[
          "btn box-border flex h-18 flex-col items-start justify-center gap-0 rounded-lg p-4 px-4 text-left transition-none",
          selected && !createAccountFocused && newAccountName.length === 0
            ? "bg-surface-200-800 border-surface-contrast-50-950 border-2 font-semibold"
            : "preset-outlined-surface-300-700",
        ]}
        role="radio"
        aria-checked={selected}
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
    <input
      bind:value={newAccountName}
      placeholder={createAccountFocused
        ? "Enter account name"
        : "Create additional account"}
      onfocus={handleCreateFocus}
      onblur={handleCreateBlur}
      class={[
        "input not-focus:placeholder:text-surface-contrast-50-950 not-focus:ring-surface-300-700 box-border h-15 justify-start rounded-lg py-4 ps-4 pe-16 text-left transition-none not-focus:cursor-pointer",
        (createAccountFocused || newAccountName.length > 0) &&
          "!ring-surface-contrast-50-950 bg-surface-200-800 ring-2",
        newAccountName.length > 0 && "font-semibold",
      ]}
    />
  </div>
  <button
    onclick={handleContinue}
    type="submit"
    class="btn preset-filled self-stretch py-2">Continue</button
  >
</form>
