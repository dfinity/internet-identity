<script lang="ts">
  import type {
    HTMLButtonAttributes,
    HTMLAnchorAttributes,
  } from "svelte/elements";
  import { nonNullish } from "@dfinity/utils";

  type Props = HTMLButtonAttributes &
    HTMLAnchorAttributes & {
      element?: HTMLElement;
    };

  let { element = $bindable(), ...props }: Props = $props();
  const buttonProps: HTMLButtonAttributes | undefined = !("href" in props)
    ? props
    : undefined;
  const anchorProps: HTMLAnchorAttributes | undefined =
    "href" in props ? props : undefined;
</script>

{#if nonNullish(buttonProps)}
  <button bind:this={element} {...buttonProps}>
    {@render buttonProps.children?.()}
  </button>
{:else if nonNullish(anchorProps)}
  <a bind:this={element} {...anchorProps}>
    {@render anchorProps.children?.()}
  </a>
{/if}
