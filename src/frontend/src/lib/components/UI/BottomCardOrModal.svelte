<script lang="ts">
  import { isMobile } from "$lib/state/UI/isMobile";
  import { onMount } from "svelte";
  import { scale, fly } from "svelte/transition";
  import { type HTMLAttributes } from "svelte/elements";

  type Props = HTMLAttributes<HTMLDialogElement> & {
    title: string;
    onClose: () => void;
  };

  const {
    title,
    children,
    onClose,
    class: className,
    ...props
  }: Props = $props();

  let dialogRef: HTMLDialogElement;
  let visible = $state(false);
  let onCancel = (e: Event): void => {
    e.preventDefault();
    visible = false;
  };

  const transitionFn = $derived(
    $isMobile
      ? (node: Element) => fly(node, { duration: 150, y: "100%" })
      : (node: Element) => scale(node, { duration: 150, start: 0.9 }),
  );

  onMount(() => {
    dialogRef.showModal();
    visible = true;
  });
</script>

<dialog
  bind:this={dialogRef}
  oncancel={onCancel}
  closedby="any"
  class={[
    "max-h-screen overflow-hidden bg-transparent backdrop:backdrop-brightness-75 backdrop:transition-opacity backdrop:duration-150",
    $isMobile ? "mt-auto min-w-full" : "m-auto w-100",
    !visible && "backdrop:opacity-0",
  ]}
  {...props}
>
  {#if visible}
    <div
      class={[
        "bg-ii-background-primary-light dark:bg-ii-background-primary-dark text-ii-text-primary-dark dark:text-ii-text-primary-light flex max-h-screen flex-col overflow-hidden p-6",
        $isMobile ? "rounded-t-2xl" : "rounded-2xl",
        className,
      ]}
      transition:transitionFn
      onoutroend={onClose}
    >
      <div class="flex">
        <h1 class="h1 -mt-1 mb-4 flex-1 items-center text-2xl">{title}</h1>
        <button
          type="button"
          class="btn-icon preset-tonal rounded-full"
          onclick={onCancel}>✕</button
        >
      </div>
      <div
        class="bg-ii-background-primary-light dark:bg-ii-background-primary-dark flex flex-1 flex-col overflow-y-auto"
      >
        {@render children?.()}
      </div>
    </div>
  {/if}
</dialog>
