<script lang="ts">
  import { CheckIcon, CopyIcon } from "@lucide/svelte";
  import { t } from "$lib/stores/locale.store";

  interface Props {
    command: string;
  }

  const { command }: Props = $props();

  let copied = $state(false);

  const onCopy = async () => {
    try {
      await navigator.clipboard.writeText(command);
    } catch {
      return;
    }
    copied = true;
    setTimeout(() => (copied = false), 1000);
  };
</script>

<button
  type="button"
  onclick={onCopy}
  aria-label={$t`Copy install command: ${command}`}
  class="flex w-full items-center gap-2 rounded-lg p-[18px] font-mono text-sm transition-colors"
  style="background-color: var(--cli-terminal-bg); border: 1px solid var(--cli-terminal-border); color: var(--cli-terminal-body);"
>
  <span style="color: var(--cli-terminal-prompt);" aria-hidden="true">$</span>
  <span style="color: var(--cli-terminal-em);" class="flex-1 text-left"
    >{command}</span
  >
  <span
    class="shrink-0"
    style={copied
      ? "color: var(--fg-success-primary);"
      : "color: var(--cli-terminal-prompt);"}
    aria-hidden="true"
  >
    {#if copied}
      <CheckIcon class="size-4" />
    {:else}
      <CopyIcon class="size-4" />
    {/if}
  </span>
</button>
