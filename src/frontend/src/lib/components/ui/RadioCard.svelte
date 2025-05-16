<script lang="ts">
  import type {
    HTMLAnchorAttributes,
    HTMLButtonAttributes,
  } from "svelte/elements";
  import Checkbox from "$lib/components/ui/Checkbox.svelte";
  import ButtonOrAnchor from "$lib/components/utils/ButtonOrAnchor.svelte";

  type Props = HTMLButtonAttributes &
    HTMLAnchorAttributes & {
      element?: HTMLElement;
      checked?: boolean;
      checkIcon?: boolean;
    };

  let {
    children,
    element = $bindable(),
    class: className,
    checked,
    checkIcon,
    ...props
  }: Props = $props();
</script>

<ButtonOrAnchor
  bind:element
  role="radio"
  aria-checked={checked}
  {...props}
  class={[
    "bg-bg-primary ring-border-secondary text-text-primary hover:bg-bg-primary_hover relative flex items-center justify-start gap-3 rounded-md p-3 text-sm font-semibold ring-1 outline-none ring-inset not-dark:shadow-xs",
    checked && "!ring-border-brand ring-2",
    className,
  ]}
>
  <div
    class="in-focus-visible:ring-focus-ring in-focus-visible:ring-offset-bg-primary pointer-events-none absolute inset-0 rounded-md in-focus-visible:ring-2 in-focus-visible:ring-offset-2"
  ></div>
  {@render children?.()}
  {#if checked && checkIcon}
    <Checkbox
      checked
      size="md"
      class="pointer-events-none mr-1 !rounded-full"
      tabindex={-1}
      aria-hidden="true"
    />
  {/if}
</ButtonOrAnchor>
