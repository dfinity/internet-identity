<script lang="ts">
  import { type State } from "../state";
  import Dialog from "$lib/components/UI/Dialog.svelte";
  import { type LastUsedIdentity } from "$lib/stores/last-used-identities.store";

  type Props = Extract<State, { state: "continueAs" }>;

  const {
    currentIdentity,
    identities,
    continueAs,
    switchIdentity,
    useAnotherIdentity,
    useAnotherAccount,
  }: Props = $props();

  const options = ["continueAs", "useAnother"] as const;
  const optionLabels: Record<(typeof options)[number], string> = {
    continueAs: "Primary account", // TODO: account from current identity
    useAnother: "Use another account",
  };

  let selectedOption = $state<(typeof options)[number]>(options[0]);
  let showOtherIdentities = $state(false);

  const handleContinue = () => {
    switch (selectedOption) {
      case "continueAs":
        return continueAs(currentIdentity);
      case "useAnother":
        return useAnotherAccount(currentIdentity);
      default:
        void (selectedOption satisfies never);
    }
  };

  $effect(() => {
    if (currentIdentity) {
      selectedOption = "continueAs";
      showOtherIdentities = false;
    }
  });
</script>

<div class="flex flex-col items-start">
  <button
    onclick={() => (showOtherIdentities = !showOtherIdentities)}
    class="btn mb-3 self-start px-0 py-1 font-medium"
  >
    <span
      >{currentIdentity.name ?? currentIdentity.identityNumber}'s Internet
      Identity</span
    >
    <span class="-mt-2.5">⌄</span>
  </button>
  <div
    class="mb-6 flex flex-col items-stretch gap-3 self-stretch"
    role="radiogroup"
  >
    {#each options as option}
      <button
        onclick={() => (selectedOption = option)}
        class={[
          "btn box-border h-15 justify-start rounded-lg p-4 px-4 text-left transition-none",
          option === selectedOption
            ? "bg-surface-200-800 border-surface-0 border-2 font-semibold"
            : "preset-outlined-surface-300-700",
        ]}
        role="radio"
        aria-checked={option === selectedOption}
      >
        {optionLabels[option]}
      </button>
    {/each}
  </div>
  <button onclick={handleContinue} class="btn preset-filled self-stretch py-2"
    >Continue</button
  >
</div>
{#if showOtherIdentities}
  <Dialog
    title={"Switch Internet Identity"}
    onClose={() => (showOtherIdentities = false)}
  >
    <div class="h-4"></div>
    {#each identities as identity}
      <button
        onclick={() => switchIdentity(identity)}
        class="border-t-surface-100-900 text-surface-contrast-50-950/80 flex items-center border-t p-2 text-start"
      >
        <span class="flex-1"
          >{identity.name ?? identity.identityNumber}'s Internet Identity</span
        >
        {#if identity.identityNumber === currentIdentity.identityNumber}
          <span
            class="preset-filled size-5 rounded-full text-center text-xs leading-5 font-bold"
            >✓</span
          >
        {/if}
      </button>
    {/each}
    <button
      onclick={useAnotherIdentity}
      class="border-y-surface-100-900 text-surface-contrast-50-950/80 border-y p-2 text-start"
      >Use another Internet Identity</button
    >
  </Dialog>
{/if}
