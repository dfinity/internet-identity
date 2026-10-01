<script lang="ts">
  import type { Snippet } from "svelte";

  interface Props {
    /** Its place in the list, which the user counts off as they follow it. */
    n: number;
    title: string;
    /** The instruction, with the thing to look for in bold. */
    instruction: Snippet;
    /** The browser's own chrome, as this step would show it. */
    mock: Snippet;
  }

  const { n, title, instruction, mock }: Props = $props();
</script>

<li
  class="bg-surface-light-200 dark:bg-surface-dark-700 rounded-xl px-3 pt-5 pb-4"
>
  <div class="mb-3 flex flex-row items-center gap-3">
    <div
      class="bg-fg-primary-inversed text-fg-secondary size-8 shrink-0 rounded-full text-center text-base leading-8"
      aria-hidden="true"
    >
      {n}
    </div>
    <h2 class="text-text-primary text-lg">{title}</h2>
  </div>
  <p class="text-text-primary mb-5 text-sm">{@render instruction()}</p>
  <div aria-hidden="true">{@render mock()}</div>
</li>
