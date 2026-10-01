<script lang="ts">
  import type { SvelteHTMLElements } from "svelte/elements";

  const {
    children,
    class: className,
    ...props
  }: SvelteHTMLElements["div"] = $props();
</script>

<div {...props} class={["relative", className]}>
  {@render children?.()}
  <div
    class="absolute -inset-[5px] rounded-full border-[1px] border-blue-700 dark:border-blue-300"
  ></div>
  <div
    class="absolute -inset-[10px] rounded-full border-[0.75px] border-blue-500 dark:border-blue-500"
  ></div>
  <div
    class="absolute -inset-[14px] rounded-full border-[0.5px] border-blue-300 dark:border-blue-700"
  ></div>
  <div
    class="absolute -inset-[18px] rounded-full border-[0.25px] border-blue-200 dark:border-blue-800"
  ></div>
</div>
