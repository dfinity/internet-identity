<script lang="ts">
  import ButtonOrAnchor from "$lib/components/utils/ButtonOrAnchor.svelte";
  import type {
    HTMLAnchorAttributes,
    HTMLButtonAttributes,
  } from "svelte/elements";

  type Props = HTMLButtonAttributes &
    HTMLAnchorAttributes & {
      current?: boolean;
    };

  const { children, class: className, current, ...props }: Props = $props();
</script>

<ButtonOrAnchor
  {...props}
  class={[
    "flex h-10 flex-row items-center gap-3 rounded-sm px-3 py-2 text-base font-medium",
    current
      ? "text-text-primary bg-bg-active ring-border-secondary hover:bg-bg-primary_hover ring-1 ring-inset"
      : "text-text-secondary hover:bg-bg-active",
    current
      ? "[&_svg]:text-fg-primary"
      : "[&_svg]:text-fg-quaternary dark:[&_svg]:text-fg-tertiary",
    className,
  ]}
>
  {@render children?.()}
</ButtonOrAnchor>
