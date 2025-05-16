<script lang="ts">
  import type { HTMLAttributes } from "svelte/elements";

  type Props = HTMLAttributes<HTMLDivElement>;

  const { children, class: className, ...props }: Props = $props();
</script>

<div
  {...props}
  class={[
    "flex w-full flex-col rounded-xl px-4 pt-5 pb-6",
    "max-sm:flex-1",
    "sm:bg-bg-secondary sm:border-border-secondary sm:max-w-100 sm:border sm:px-6",
    className,
  ]}
>
  {@render children?.()}
</div>
