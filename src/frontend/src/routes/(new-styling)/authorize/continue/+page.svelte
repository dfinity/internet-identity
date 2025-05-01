<script lang="ts">
  import type { PageProps } from "./$types";
  import Dialog from "$lib/components/UI/Dialog.svelte";
  import { authenticateWithLastUsed } from "$lib/utils/authenticate/lastUsed";
  import { authenticationState } from "$lib/state/authentication";

  const { data }: PageProps = $props();
  const { canisterId, config, session, lastUsedIdentities } = data;

  let selectedIdentity = $state(lastUsedIdentities[0]);
  let continueWith = $state<"lastUsedAccount" | "anotherAccount">(
    "lastUsedAccount",
  );
  let identitySwitcherVisible = $state(false);

  const handleContinue = async () => {
    const { identity, anchorNumber } = await authenticateWithLastUsed({
      canisterId,
      config,
      session,
      lastUsed: selectedIdentity,
    });

    switch (continueWith) {
      case "lastUsedAccount":
        // TODO
        break;
      case "anotherAccount":
        authenticationState.authenticated = { identity, anchorNumber };
        break;
      default:
        void (continueWith satisfies never);
    }
  };

  // $effect(() => {
  //   if (currentIdentity) {
  //     selectedOption = "continueAs";
  //     showOtherIdentities = false;
  //   }
  // });
</script>

<div class="flex flex-col items-start">
  <button
    onclick={() => (identitySwitcherVisible = !identitySwitcherVisible)}
    class="btn mb-3 self-start px-0 py-1 font-medium"
  >
    <span
      >{selectedIdentity.identity.name ??
        selectedIdentity.identity.identityNumber}'s Internet Identity</span
    >
    <span class="-mt-2.5">⌄</span>
  </button>
  <div
    class="mb-6 flex flex-col items-stretch gap-3 self-stretch"
    role="radiogroup"
  >
    <button
      onclick={() => (continueWith = "lastUsedAccount")}
      class={[
        "btn box-border h-15 justify-start rounded-lg p-4 px-4 text-left transition-none",
        continueWith === "lastUsedAccount"
          ? "bg-surface-200-800 border-surface-0 border-2 font-semibold"
          : "preset-outlined-surface-300-700",
      ]}
      role="radio"
      aria-checked={continueWith === "lastUsedAccount"}
    >
      {selectedIdentity.account.name ?? "Primary account"}
    </button>
    <button
      onclick={() => (continueWith = "anotherAccount")}
      class={[
        "btn box-border h-15 justify-start rounded-lg p-4 px-4 text-left transition-none",
        continueWith === "anotherAccount"
          ? "bg-surface-200-800 border-surface-0 border-2 font-semibold"
          : "preset-outlined-surface-300-700",
      ]}
      role="radio"
      aria-checked={continueWith === "lastUsedAccount"}
    >
      Use another account
    </button>
  </div>
  <button onclick={handleContinue} class="btn preset-filled self-stretch py-2"
    >Continue</button
  >
</div>
{#if identitySwitcherVisible}
  <Dialog
    title={"Switch Internet Identity"}
    onClose={() => (identitySwitcherVisible = false)}
  >
    <div class="h-4"></div>
    {#each lastUsedIdentities as lastUsedIdentity}
      <button
        onclick={() => (selectedIdentity = lastUsedIdentity)}
        class="border-t-surface-100-900 text-surface-contrast-50-950/80 flex items-center border-t p-2 text-start"
      >
        <span class="flex-1"
          >{lastUsedIdentity.identity.name ??
            lastUsedIdentity.identity.identityNumber}'s Internet Identity</span
        >
        {#if lastUsedIdentity === selectedIdentity}
          <span
            class="preset-filled size-5 rounded-full text-center text-xs leading-5 font-bold"
            >✓</span
          >
        {/if}
      </button>
    {/each}
    <a
      href="/authorize"
      class="border-y-surface-100-900 text-surface-contrast-50-950/80 border-y p-2 text-start"
      >Use another Internet Identity</a
    >
  </Dialog>
{/if}
