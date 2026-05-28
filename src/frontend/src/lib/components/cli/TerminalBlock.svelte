<script lang="ts">
  import { t } from "$lib/stores/locale.store";

  interface Props {
    /** Command the user ran in their terminal (e.g. `icp identity link ii`). */
    command: string;
    /** Progress line under the command. */
    progressLine?: string;
    /** Whether the progress line should show the trailing ellipsis. */
    busy?: boolean;
    /** Show the "Requested by" badge in the top-right corner. */
    showBadge?: boolean;
  }

  const {
    command,
    progressLine,
    busy = false,
    showBadge = true,
  }: Props = $props();
</script>

<!--
  Hardcoded terminal colors so the block reads as a real terminal in both
  themes (light mode uses a slightly softer near-black so it doesn't slam
  against a white page).
-->
<div
  class="relative overflow-hidden rounded-lg p-[18px] font-mono text-sm leading-6 text-[var(--cli-terminal-body)]"
  style="background-color: var(--cli-terminal-bg); border: 1px solid var(--cli-terminal-border);"
  aria-hidden="true"
>
  {#if showBadge}
    <span
      class="border-border-tertiary bg-bg-secondary text-text-secondary absolute top-3 right-3 rounded-full border px-3 py-1 font-sans text-xs font-medium"
    >
      {$t`Requested by`}
    </span>
  {/if}
  <div>
    <span class="text-[var(--cli-terminal-prompt)]">$ </span><span
      class="text-[var(--cli-terminal-em)]">{command}</span
    >
  </div>
  {#if progressLine !== undefined}
    <div>
      <span class="text-[var(--cli-terminal-ok)]">✓</span>
      <span>{$t`Browser opened`}</span>
    </div>
    <div>
      <span class="text-[var(--cli-terminal-prompt)]">·</span>
      <span>{progressLine}{busy ? "…" : ""}</span>
    </div>
  {/if}
</div>
