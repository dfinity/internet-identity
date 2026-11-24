<!--
  The mark on the identity used most recently on this device: a quiet pill,
  a tint on whatever it lies on, so it reads in both themes and on an
  identity's own colours without a fill of its own.
-->
<script lang="ts">
  import type { ClassValue } from "svelte/elements";

  const { class: className }: { class?: ClassValue } = $props();
</script>

<span
  class={[
    "inline-flex h-5 shrink-0 items-center rounded-full bg-(--ii-hover) px-2 text-[0.6875rem] leading-none font-medium whitespace-nowrap",
    className ?? "text-text-tertiary",
  ]}
>
  Last used
</span>
