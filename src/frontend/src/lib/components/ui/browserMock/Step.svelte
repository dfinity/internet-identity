<script lang="ts">
  import type { Snippet } from "svelte";

  const {
    n,
    title,
    instruction,
    children,
  }: {
    /** Its place in the list, shown in the badge. */
    n: number;
    title: string;
    /** What to do, with the words to look for in the mock marked up. */
    instruction: Snippet;
    /** The mock the instruction points at. */
    children: Snippet;
  } = $props();
</script>

<li
  class="bg-surface-light-200 dark:bg-surface-dark-700 rounded-xl px-3 pt-5 pb-4"
>
  <div class="mb-3 flex flex-row items-center gap-3">
    <div
      aria-hidden="true"
      class="bg-fg-primary-inversed text-fg-secondary h-8 w-8 shrink-0 rounded-full text-center text-base leading-8"
    >
      {n}
    </div>
    <h2 class="text-text-primary m-0 text-lg leading-7 font-normal">{title}</h2>
  </div>
  <p class="text-text-primary mb-5 text-sm leading-5">
    {@render instruction()}
  </p>
  {@render children()}
</li>
