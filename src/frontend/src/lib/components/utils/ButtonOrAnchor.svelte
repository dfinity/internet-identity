<script lang="ts">
  import type {
    HTMLButtonAttributes,
    HTMLAnchorAttributes,
  } from "svelte/elements";

  type Props = HTMLButtonAttributes &
    HTMLAnchorAttributes & {
      element?: HTMLElement;
    };

  let { element = $bindable(), ...props }: Props = $props();
  const buttonProps = $derived(!("href" in props) ? props : undefined);
  const anchorProps = $derived("href" in props ? props : undefined);
</script>

{#if buttonProps !== undefined}
  <button bind:this={element} {...buttonProps}>
    {@render buttonProps.children?.()}
  </button>
{:else if anchorProps !== undefined}
  <a bind:this={element} {...anchorProps}>
    {@render anchorProps.children?.()}
  </a>
{/if}
