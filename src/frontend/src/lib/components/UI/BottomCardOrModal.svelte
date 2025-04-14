<script lang="ts">
  import { isMobile } from "$lib/state/UI/isMobile";
  import { onMount, type Snippet } from "svelte";
  import { scale, fly } from "svelte/transition";

  const {
    title,
    children,
    onclose,
    class: classes,
    ...rest
  } = $props<{
    title: string;
    children?: Snippet;
    onclose: () => void;
    class?: string;
    [key: string]: any;
  }>();

  let dialogRef: HTMLDialogElement;
  let visible = $state(false);
  let onCancel = (e: Event): void => {
    e.preventDefault();
    visible = false;
  };

  onMount(() => {
    dialogRef.showModal();
    visible = true;
  });
</script>

{#if $isMobile}
  <dialog
    bind:this={dialogRef}
    oncancel={onCancel}
    closedby="any"
    class={`top-auto mx-auto max-w-100 overflow-hidden bg-transparent backdrop:transition-opacity backdrop:duration-150 backdrop:opacity-${visible ? 100 : 0} backdrop:backdrop-brightness-75`}
    {...rest}
  >
    {#if visible}
      <div
        class={`bg-ii-background-primary-light dark:bg-ii-background-primary-dark text-ii-text-primary-dark dark:text-ii-text-primary-light flex max-h-screen flex-col overflow-hidden rounded-t-2xl p-6 ${classes}`}
        transition:fly|global={{ duration: 150, y: "100%" }}
        onoutroend={onclose}
      >
        <div class="flex">
          <h3 class="h3 -mt-1 mb-4 flex-1 items-center">{title}</h3>
          <button
            type="button"
            class="btn-icon preset-tonal rounded-full"
            onclick={onCancel}>✕</button
          >
        </div>
        <div
          class="bg-ii-background-primary-light dark:bg-ii-background-primary-dark overflow-y-auto"
        >
          {@render children()}
        </div>
      </div>
    {/if}
  </dialog>
{:else}
  <dialog
    bind:this={dialogRef}
    oncancel={onCancel}
    closedby="any"
    class={`m-auto max-h-screen max-w-100 bg-transparent backdrop:transition-opacity backdrop:duration-150 backdrop:opacity-${visible ? 100 : 0} backdrop:backdrop-brightness-75`}
    {...rest}
  >
    {#if visible}
      <div
        class={`bg-ii-background-primary-light dark:bg-ii-background-primary-dark text-ii-text-primary-dark dark:text-ii-text-primary-light flex max-h-screen flex-col overflow-hidden rounded-2xl p-6 ${classes}`}
        transition:scale|global={{ duration: 150, start: 0.9 }}
        onoutroend={onclose}
      >
        <div class="flex">
          <h3 class="h3 -mt-1 mb-4 flex-1 items-center">{title}</h3>
          <button
            type="button"
            class="btn-icon preset-tonal rounded-full"
            onclick={onCancel}>✕</button
          >
        </div>
        <div
          class="bg-ii-background-primary-light dark:bg-ii-background-primary-dark flex flex-1 flex-col overflow-y-auto"
        >
          {@render children()}
        </div>
      </div>
    {/if}
  </dialog>
{/if}
