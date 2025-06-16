<script lang="ts">
  import type {
    HTMLAnchorAttributes,
    HTMLButtonAttributes,
  } from "svelte/elements";
  import ButtonOrAnchor from "$lib/components/utils/ButtonOrAnchor.svelte";

  type Props = HTMLButtonAttributes &
    HTMLAnchorAttributes & {
      element?: HTMLElement;
    };

  let {
    children,
    element = $bindable(),
    class: className,
    ...props
  }: Props = $props();
</script>

<ButtonOrAnchor
  bind:element
  {...props}
  class={[
    "bg-bg-primary ring-border-secondary text-text-primary not-disabled:hover:bg-bg-primary_hover relative flex items-center justify-start gap-3 rounded-md p-3 text-sm font-semibold ring-1 outline-none ring-inset not-dark:shadow-xs",
    "disabled:ring-border-disabled disabled:text-text-disabled",
    className,
  ]}
>
  <div
    class="in-focus-visible:ring-focus-ring in-focus-visible:ring-offset-bg-primary pointer-events-none absolute inset-0 rounded-md in-focus-visible:ring-2 in-focus-visible:ring-offset-2"
  ></div>
  {@render children?.()}
</ButtonOrAnchor>
