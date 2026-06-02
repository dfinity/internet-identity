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
  against a white page). No `overflow-hidden` so the badge can straddle the
  top edge; each prefix glyph gets a right margin so the following text keeps
  a consistent gap regardless of the glyph's width.
-->
<div
  class="relative rounded-lg p-[18px] font-mono text-[13px] leading-[1.55] text-[var(--cli-terminal-body)]"
  style="background-color: var(--cli-terminal-bg); border: 1px solid var(--cli-terminal-border);"
  aria-hidden="true"
>
  {#if showBadge}
    <span
      class="border-border-tertiary bg-bg-secondary text-text-secondary absolute top-0 right-4 -translate-y-1/2 rounded-full border px-3 py-1 font-sans text-xs font-medium"
    >
      {$t`Requested by`}
    </span>
  {/if}
  <div>
    <span class="mr-2 text-[var(--cli-terminal-prompt)] select-none">$</span
    ><span class="text-[var(--cli-terminal-em)]">{command}</span>
  </div>
  {#if progressLine !== undefined}
    <div>
      <span class="mr-1.5 text-[var(--cli-terminal-ok)] select-none">✓</span
      ><span>{$t`Browser opened`}</span>
    </div>
    <div>
      <span class="mr-1.5 text-[var(--cli-terminal-prompt)] select-none">·</span
      ><span>{progressLine}{busy ? "…" : ""}</span>
    </div>
  {/if}
</div>
