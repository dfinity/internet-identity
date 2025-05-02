<script lang="ts">
  import { isMobile } from "$lib/state/UI/isMobile";
  import { onMount } from "svelte";
  import { scale, fly } from "svelte/transition";
  import { type HTMLAttributes } from "svelte/elements";
  import { nonNullish } from "@dfinity/utils";

  type Props = HTMLAttributes<HTMLDialogElement> & {
    onClose?: () => void;
    title?: string;
    closeOnOutsideClick?: boolean;
    showCloseButton?: boolean;
    backdrop?: boolean;
    bottomSheet?: "mobile" | "always" | "never";
  };

  const {
    children,
    onClose,
    title,
    class: className,
    closeOnOutsideClick = true,
    showCloseButton = true,
    backdrop = true,
    bottomSheet = "mobile",
    ...props
  }: Props = $props();

  let dialogRef: HTMLDialogElement;
  let onCancel = (e: Event): void => {
    e.preventDefault();
    onClose?.();
  };

  const transitionFn = $derived(
    ($isMobile && bottomSheet === "mobile") || bottomSheet === "always"
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
    "backdrop:bg-surface-100 flex max-h-screen max-w-full flex-col overflow-hidden bg-transparent backdrop:opacity-0 backdrop:transition-opacity backdrop:duration-200 max-[460px]:min-w-full backdrop:dark:bg-[#040404]",
    backdrop && "[&[data-visible]]:backdrop:opacity-80",
    ($isMobile && bottomSheet === "mobile") || bottomSheet === "always"
      ? "fixed top-auto bottom-0 mx-auto"
      : "m-auto max-[460px]:m-0 max-[460px]:min-h-full",
  ]}
  transition:transitionFn
  onoutrostart={fadeOutBackDrop}
  {...props}
>
  <div
    class={[
      "preset-filled-surface-50-950 flex max-h-screen w-100 flex-col overflow-hidden p-6 max-[460px]:max-w-full max-[460px]:min-w-full",
      ($isMobile && bottomSheet === "mobile") || bottomSheet === "always"
        ? "max-w-full rounded-t-2xl"
        : "rounded-2xl max-[460px]:flex-1 max-[460px]:rounded-none",
      className,
    ]}
  >
    <div class="flex">
      {#if nonNullish(title)}
        <h1 class="h1 -mt-1 mb-4 flex-1 items-center text-xl">{title}</h1>
      {/if}
      {#if showCloseButton && nonNullish(onClose)}
        <button
          type="button"
          class="btn-icon preset-tonal rounded-full"
          onclick={onClose}>✕</button
        >
      {/if}
    </div>
    <div
      class="preset-filled-surface-50-950 flex flex-1 flex-col overflow-x-hidden overflow-y-auto"
    >
      {@render children?.()}
    </div>
  </div>
</dialog>
