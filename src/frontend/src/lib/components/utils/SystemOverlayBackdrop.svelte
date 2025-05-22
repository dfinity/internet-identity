<script lang="ts">
  import { fade } from "svelte/transition";
  import type { HTMLAttributes } from "svelte/elements";

  type Props = HTMLAttributes<HTMLElement>;

  const { class: className, ...props }: Props = $props();
</script>

<div
  class={[`bg-bg-overlay/80 fixed inset-0 z-999`, className]}
  transition:fade={{ duration: 100 }}
  {...props}
></div>
