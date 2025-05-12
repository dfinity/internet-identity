<script lang="ts">
  import { onMount } from "svelte";
  import { scale, fly } from "svelte/transition";
  import { type HTMLAttributes } from "svelte/elements";
  import { nonNullish } from "@dfinity/utils";
  import Button from "$lib/components/ui/Button.svelte";
  import { XIcon } from "@lucide/svelte";

  type Props = HTMLAttributes<HTMLDialogElement> & {
    onClose?: () => void;
    title?: string;
    closeOnOutsideClick?: boolean;
    showCloseButton?: boolean;
    backdrop?: boolean;
  };

  const {
    children,
    onClose,
    title,
    class: className,
    closeOnOutsideClick = true,
    showCloseButton = true,
    backdrop = true,
    ...props
  }: Props = $props();

  let dialogRef: HTMLDialogElement;
  let onCancel = (e: Event): void => {
    e.preventDefault();
    onClose?.();
  };

  const transitionFn = $derived(
    window.innerWidth < 480
      ? (node: Element) => fly(node, { duration: 200, y: "100%" })
      : (node: Element) => scale(node, { duration: 200, start: 0.9 }),
  );

  const fadeOutBackDrop = () => {
    dialogRef.removeAttribute("data-visible");
  };

  onMount(() => {
    dialogRef.showModal();
    dialogRef.setAttribute("data-visible", "true");
  });
</script>

<dialog
  bind:this={dialogRef}
  oncancel={onCancel}
  closedby={closeOnOutsideClick ? "any" : "none"}
  class={[
    // Layout base/dialog/bottomsheet
    "fixed flex max-h-screen max-w-full flex-col overflow-hidden bg-transparent",
    "sm:m-auto sm:w-100",
    "w-full max-sm:top-auto max-sm:bottom-0",
    // Backdrop base/visible
    "backdrop:bg-black backdrop:opacity-0 backdrop:transition-opacity backdrop:duration-200",
    backdrop && "[&[data-visible]]:backdrop:opacity-80",
  ]}
  transition:transitionFn
  onoutrostart={fadeOutBackDrop}
  {...props}
>
  <div
    class={[
      // Container base/dialog/bottomsheet/light/dark
      "relative flex max-h-screen flex-col overflow-hidden",
      "sm:w-100 sm:rounded-2xl sm:px-6 sm:pt-6 sm:pb-8",
      "w-full rounded-t-2xl px-4 pt-4 pb-6",
      "bg-gray-light-25",
      "dark:bg-gray-dark-800 dark:border-gray-light-700 sm:dark:border",
      className,
    ]}
  >
    <div
      class={[
        // Non-interactive element to render dark-mode bottom sheet border gradient
        "from-gray-light-700 pointer-events-none absolute top-0 right-0 left-0 z-0 hidden rounded-t-2xl bg-gradient-to-b to-transparent p-[1px] max-sm:dark:block",
      ]}
    >
      <div class="bg-gray-dark-800 h-24 rounded-t-2xl"></div>
    </div>
    {#if showCloseButton && nonNullish(onClose)}
      <Button
        variant="tertiary"
        size="lg"
        iconOnly
        type="button"
        class="absolute top-2 right-2 z-2 !rounded-full"
        onclick={onClose}
      >
        <XIcon size="1.25rem" />
      </Button>
    {/if}
    <div class="relative z-1 flex flex-1 flex-col">
      {@render children?.()}
    </div>
  </div>
</dialog>
