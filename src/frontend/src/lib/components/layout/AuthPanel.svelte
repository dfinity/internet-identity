<script lang="ts">
  import type { HTMLAttributes } from "svelte/elements";

  type Props = HTMLAttributes<HTMLDivElement>;

  const { children, class: className, ...props }: Props = $props();
</script>

<div
  {...props}
  class={[
    "sm:bg-gray-light-50 sm:border-gray-light-300 sm:dark:bg-gray-light-900 sm:dark:border-gray-light-600 w-full rounded-xl px-4 pt-5 pb-6 sm:max-w-100 sm:border sm:px-6",
    className,
  ]}
>
  {@render children?.()}
</div>
