<script lang="ts">
  import { onMount } from "svelte";
  import { scale, fly } from "svelte/transition";
  import type { HTMLAttributes } from "svelte/elements";
  import { nonNullish } from "@dfinity/utils";
  import Button from "$lib/components/ui/Button.svelte";
  import { XIcon } from "@lucide/svelte";

  type Props = HTMLAttributes<HTMLDialogElement> & {
    onClose?: () => void;
    closeOnOutsideClick?: boolean;
    showCloseButton?: boolean;
    backdrop?: boolean;
  };

  const {
    children,
    onClose,
    class: className,
    closeOnOutsideClick = nonNullish(onClose),
    showCloseButton = nonNullish(onClose),
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

    // Use the virtualKeyboard API to intentionally render the software keyboard
    // on top of the page, we manually adjust the dialog positioning for it.
    //
    // If the API is not supported (e.g. iOS) polyfill it with visualViewport.
    let visualViewportResizeTimeout: ReturnType<typeof setTimeout>;
    const updateKeyboardInset = () => {
      clearTimeout(visualViewportResizeTimeout);
      visualViewportResizeTimeout = setTimeout(() => {
        dialogRef.style.setProperty(
          "--keyboard-inset-height",
          `${Math.max(window.innerHeight - window.visualViewport!.height, 0)}px`,
        );
      }, 100);
    };
    if ("virtualKeyboard" in navigator) {
      (
        navigator.virtualKeyboard as { overlaysContent: boolean }
      ).overlaysContent = true;
    } else {
      window.visualViewport?.addEventListener("resize", updateKeyboardInset);
      window.visualViewport?.addEventListener("scroll", updateKeyboardInset);
    }
    return () => {
      if ("virtualKeyboard" in navigator) {
        (
          navigator.virtualKeyboard as { overlaysContent: boolean }
        ).overlaysContent = false;
      } else {
        window.visualViewport?.removeEventListener(
          "resize",
          updateKeyboardInset,
        );
        window.visualViewport?.removeEventListener(
          "scroll",
          updateKeyboardInset,
        );
      }
    };
  });
</script>

<dialog
  bind:this={dialogRef}
  oncancel={onCancel}
  closedby={closeOnOutsideClick ? "any" : "none"}
  class={[
    // Layout base/dialog/bottomsheet
    "fixed flex max-w-full flex-col overflow-hidden bg-transparent outline-none",
    "sm:m-auto sm:w-100",
    "w-full max-sm:top-auto max-sm:bottom-0",
    // Backdrop base/visible
    "backdrop:bg-bg-overlay backdrop:opacity-0 backdrop:transition-opacity backdrop:duration-200",
    backdrop && "[&[data-visible]]:backdrop:opacity-80",
  ]}
  style="--keyboard-inset-height: env(keyboard-inset-height);"
  transition:transitionFn|global
  onoutrostart={fadeOutBackDrop}
  {...props}
>
  <div
    class={[
      // Container base/dialog/bottomsheet
      "bg-bg-primary_alt border-border-secondary relative flex max-h-screen flex-col overflow-hidden dark:sm:border",
      "sm:m-auto sm:w-100 sm:rounded-2xl sm:px-6 sm:pt-6 sm:pb-8",
      "w-full rounded-t-2xl px-4 pt-4 pb-6",
      className,
    ]}
  >
    <!-- Non-interactive element to render dark-mode bottom sheet border gradient -->
    <div
      class="from-border-secondary pointer-events-none absolute top-0 right-0 left-0 z-0 hidden rounded-t-2xl bg-gradient-to-b to-transparent p-[1px] max-sm:dark:block"
    >
      <div class="bg-bg-primary_alt h-24 rounded-t-2xl"></div>
    </div>
    <div class="relative flex flex-1 flex-col">
      {@render children?.()}
      <!-- Element that pushes bottom sheet away from mobile keyboard or gesture navigation -->
      <div class="flex sm:hidden">
        <div class="h-[var(--keyboard-inset-height)]"></div>
        <div class="h-[env(safe-area-inset-bottom)]"></div>
      </div>
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
  </div>
  <!-- Element that pushes dialog away from mobile keyboard or gesture navigation -->
  <div class="flex max-sm:hidden">
    <div class="h-[var(--keyboard-inset-height)]"></div>
    <div class="h-[env(safe-area-inset-bottom)]"></div>
  </div>
</dialog>
