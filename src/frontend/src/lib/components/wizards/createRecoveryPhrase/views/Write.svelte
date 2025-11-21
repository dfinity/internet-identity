<script lang="ts">
  import { t } from "$lib/stores/locale.store";
  import Button from "$lib/components/ui/Button.svelte";
  import Steps from "$lib/components/wizards/createRecoveryPhrase/components/Steps.svelte";
  import { Trans } from "$lib/components/locale";
  import { EyeOffIcon } from "@lucide/svelte";

  interface Props {
    recoveryPhrase: string[];
    onWritten: () => void;
  }

  const { recoveryPhrase, onWritten }: Props = $props();

  let isRevealed = $state(false);
</script>

<div class="limited-height my-10">
  <Steps total={3} current={2} />
</div>
<h2 class="text-text-primary text-2xl font-medium">
  {$t`Save your recovery phrase`}
</h2>
<p class="text-text-tertiary limited-height mt-3 text-base font-medium">
  <Trans>
    Write down your recovery phrase in order and verify it afterwards. Store it
    safely and do not share it. Losing it means you will lose access.
  </Trans>
</p>
<div class="relative">
  <ol
    class={["my-8 grid grid-cols-3 gap-3", !isRevealed && "blur-md"]}
    aria-hidden={isRevealed ? "false" : "true"}
  >
    {#each recoveryPhrase as word, index}
      <li
        class="border-border-primary flex h-7 flex-row items-center rounded-full border px-1.5"
      >
        <div
          class="text-text-secondary pointer-events-none w-4 text-center text-xs font-semibold tabular-nums select-none"
          aria-hidden="true"
        >
          {`${index + 1}`.padStart(2, "0")}
        </div>
        <div class="border-border-secondary mx-1 h-full border-r"></div>
        <div
          class={[
            "text-text-secondary -translate-y-0.25 text-base",
            word.length > 7 && "tracking-tight",
          ]}
        >
          {word}
        </div>
      </li>
    {/each}
  </ol>
  <button
    onclick={() => (isRevealed = true)}
    class={[
      "group absolute inset-0 flex flex-col items-center justify-center outline-none",
      isRevealed && "hidden",
    ]}
  >
    <EyeOffIcon class="text-fg-primary mb-1.5 size-5" aria-hidden />
    <span
      class="text-text-primary mb-4 text-sm font-bold group-focus-visible:underline"
    >
      {$t`Click to reveal`}
    </span>
    <span class="text-text-primary text-sm text-balance" aria-hidden="true">
      <Trans>
        Make sure no one can see what's on your screen or what you are writing.
      </Trans>
    </span>
  </button>
</div>
<Button
  onclick={onWritten}
  disabled={!isRevealed}
  size="lg"
  aria-hidden={isRevealed ? "false" : "true"}
>
  {$t`I have written it down`}
</Button>

<style>
  @media (max-height: 700px) {
    .limited-height {
      display: none !important;
    }
  }
</style>
