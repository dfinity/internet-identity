<script lang="ts">
  import { slide, fade } from "svelte/transition";
  import { isMobile } from "$lib/state/UI/isMobile";

  const {
    children,
    class: classes,
    close,
    ...rest
  } = $props<{
    children: () => any;
    class?: string;
    close: () => void;
    [key: string]: any;
  }>();

  function handleKeydown(e: KeyboardEvent) {
    if (e.key === "Escape") {
      close();
    }
  }

  function handleClick(e: MouseEvent) {
    if (e.target === e.currentTarget) {
      close();
    }
  }
</script>

<div
  class={`fixed top-0 left-0 h-screen w-screen backdrop-blur-3xl backdrop-brightness-50 ${!$isMobile && "flex items-center justify-center"}`}
  onclick={handleClick}
  onkeydown={handleKeydown}
  role="button"
  tabindex="0"
  transition:fade={{
    duration: 150,
  }}
>
  {#if $isMobile}
    <div
      class={`preset-filled card bg-ii-background-primary-light dark:bg-ii-background-primary-dark text-ii-text-primary-dark dark:text-ii-text-primary-light fixed bottom-0 w-full rounded-t-2xl rounded-b-none pb-4 ${classes}`}
      {...rest}
      transition:slide={{ axis: "y", duration: 150 }}
    >
      {@render children()}
    </div>
  {:else}
    <div
      class={`preset-filled card bg-ii-background-primary-light dark:bg-ii-background-primary-dark text-ii-text-primary-dark dark:text-ii-text-primary-light min-h-md min-w-md overflow-hidden rounded-2xl pb-4 ${classes}`}
      {...rest}
      transition:slide={{ axis: "y", duration: 150 }}
    >
      {@render children()}
    </div>
  {/if}
</div>
