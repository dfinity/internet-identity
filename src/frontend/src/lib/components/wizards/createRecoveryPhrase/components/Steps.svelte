<script lang="ts">
  import type { SvelteHTMLElements } from "svelte/elements";

  type Props = SvelteHTMLElements["div"] & {
    current: number;
    total: number;
  };

  const { current, total, class: className, ...props }: Props = $props();
</script>

<div {...props} class={["gap flex flex-row gap-4", className]}>
  {#each Array.from({ length: total }) as _, index}
    <div
      class={[
        "bg-border-tertiary h-1 flex-1",
        index < current && "bg-fg-tertiary",
      ]}
    ></div>
  {/each}
</div>
