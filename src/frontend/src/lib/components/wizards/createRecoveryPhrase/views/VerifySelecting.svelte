<script lang="ts">
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";
  import Steps from "$lib/components/wizards/createRecoveryPhrase/components/Steps.svelte";
  import StepsProgressBar from "$lib/components/wizards/createRecoveryPhrase/components/StepsProgressBar.svelte";

  interface Props {
    recoveryPhrase: string[];
    onCompleted: (recoveryPhrase: string[]) => Promise<void>;
  }

  const { recoveryPhrase, onCompleted }: Props = $props();

  let selectedIndexes = $state<number[]>([]);

  const shuffledIndexes = $derived(
    recoveryPhrase
      .map((_, index) => ({ index, sort: Math.random() }))
      .sort((a, b) => a.sort - b.sort)
      .map(({ index }) => index),
  );
  const selectedWords = $derived(
    selectedIndexes.map((index) => recoveryPhrase[index]),
  );
  const isCheckingOrder = $derived(
    selectedWords.length === recoveryPhrase.length,
  );

  const handleSelect = (index: number) => {
    selectedIndexes = [...selectedIndexes, index];
  };
  const handleUndo = () => {
    selectedIndexes = selectedIndexes.slice(0, -1);
  };
  const handleClear = () => {
    selectedIndexes = [];
  };

  // Auto-submit after the last word has been selected
  $effect(() => {
    if (!isCheckingOrder) {
      return;
    }
    onCompleted(selectedWords);
  });
</script>

<div class="my-10 max-sm:hidden">
  {#if isCheckingOrder}
    <StepsProgressBar total={3} />
  {:else}
    <Steps total={3} current={3} />
  {/if}
</div>
<h2 class="text-text-primary mb-3 text-2xl font-medium">
  {#if isCheckingOrder}
    {$t`Checking your order`}
  {:else}
    {$t`Verify your recovery phrase`}
  {/if}
</h2>
<p class="text-text-tertiary mb-8 text-base font-medium">
  {#if isCheckingOrder}
    <Trans>This may take a few seconds</Trans>
  {:else}
    <Trans>Select each word in the correct order:</Trans>
  {/if}
</p>
<ul class={["mb-8 grid grid-cols-3 gap-3"]}>
  {#each shuffledIndexes as index}
    {@const word = recoveryPhrase[index]}
    {@const selectedPosition = selectedIndexes.indexOf(index)}
    {@const isSelected = selectedPosition !== -1 && !isCheckingOrder}
    {@const isLastSelected = selectedIndexes.slice(-1)[0] === index}
    <li class="contents">
      <button
        onclick={isLastSelected ? handleUndo : () => handleSelect(index)}
        class={[
          "border-border-primary hover:not-disabled:bg-bg-primary_hover focus-visible:not-disabled:bg-bg-primary_hover flex h-7 flex-row items-center rounded-full border px-1.5 outline-none",
          isSelected && "!border-fg-primary",
          isCheckingOrder && "!bg-bg-disabled !border-border-disabled_subtle",
        ]}
        disabled={(isSelected && !isLastSelected) || isCheckingOrder}
        aria-pressed={isSelected ? "true" : "false"}
      >
        <span
          class={[
            "text-text-secondary w-4 text-center text-xs font-semibold tabular-nums select-none",
            isSelected ? "!text-text-primary" : "-translate-y-0.25",
            isCheckingOrder && "!text-text-disabled",
          ]}
          aria-hidden="true"
        >
          {isSelected || isCheckingOrder
            ? `${selectedPosition + 1}`.padStart(2, "0")
            : "＿"}
        </span>
        <span
          class={[
            "border-border-secondary mx-1 h-full border-r",
            isSelected && "!border-fg-primary",
            isCheckingOrder && "!border-border-disabled_subtle",
          ]}
        ></span>
        <span
          class={[
            "text-text-secondary -translate-y-0.25 text-base select-none",
            word.length > 7 && "tracking-tight",
            isSelected && "!text-text-primary font-semibold",
            isCheckingOrder && "!text-text-disabled",
          ]}
        >
          {word}
        </span>
      </button>
    </li>
  {/each}
</ul>
<button
  onclick={handleClear}
  disabled={selectedIndexes.length === 0 || isCheckingOrder}
  class="btn btn-secondary btn-lg"
>
  {$t`Clear all`}
</button>
