<script lang="ts">
  import { onMount } from "svelte";
  import { scale, fly } from "svelte/transition";
  import type { HTMLAttributes } from "svelte/elements";
  import { nonNullish } from "@dfinity/utils";
  import Button from "$lib/components/ui/Button.svelte";
  import { XIcon } from "@lucide/svelte";
  import { t } from "$lib/stores/locale.store";

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
  let contentRef: HTMLDivElement;
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
        dialogRef.style.setProperty(
          "--max-content-height",
          `${window.visualViewport!.height}px`,
        );
      }, 100);
    };
    // If the API is not supported (e.g. iOS) prevent manual scrolling of the page
    const preventScroll = (event: TouchEvent) => {
      event.preventDefault();
    };
    let lastY = 0;
    const touchScrollStart = (event: TouchEvent) => {
      lastY = event.touches[0].clientY;
    };
    const touchScrollMove = (event: TouchEvent) => {
      const y = event.touches[0].clientY;
      const dy = y - lastY;
      lastY = y;
      contentRef.scrollTo({
        top: contentRef.scrollTop - dy,
        behavior: "instant",
      });
    };
    if ("virtualKeyboard" in navigator) {
      (
        navigator.virtualKeyboard as { overlaysContent: boolean }
      ).overlaysContent = true;
    } else {
      window.visualViewport?.addEventListener("resize", updateKeyboardInset);
      window.visualViewport?.addEventListener("scroll", updateKeyboardInset);
      document.documentElement.addEventListener("touchmove", preventScroll, {
        passive: false,
      });
      contentRef.addEventListener("touchstart", touchScrollStart, {
        passive: false,
      });
      contentRef.addEventListener("touchmove", touchScrollMove, {
        passive: false,
      });
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
        document.documentElement.removeEventListener(
          "touchmove",
          preventScroll,
        );
        contentRef.removeEventListener("touchstart", touchScrollStart);
        contentRef.removeEventListener("touchmove", touchScrollMove);
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
    "fixed flex min-h-max max-w-full flex-col overflow-auto bg-transparent outline-none",
    "sm:m-auto sm:w-100",
    "max-sm:top-auto max-sm:bottom-0 max-sm:w-full",
    // Backdrop base/visible
    "backdrop:bg-bg-overlay backdrop:opacity-0 backdrop:transition-opacity backdrop:duration-200",
    backdrop && "[&[data-visible]]:backdrop:opacity-80",
  ]}
  style="--keyboard-inset-height: env(keyboard-inset-height);--max-content-height: calc(100dvh - var(--keyboard-inset-height))"
  transition:transitionFn|global
  onoutrostart={fadeOutBackDrop}
  {...props}
>
  <div
    class={[
      // Container base/dialog/bottomsheet
      "bg-bg-primary_alt border-border-secondary relative flex flex-col overflow-hidden dark:sm:border",
      "min-h-max sm:m-auto sm:w-100 sm:rounded-2xl",
      "w-full rounded-t-2xl",
      className,
    ]}
  >
    <!-- Non-interactive element to render dark-mode bottom sheet border gradient -->
    <div
      class="from-border-secondary pointer-events-none absolute top-0 right-0 left-0 z-0 hidden rounded-t-2xl bg-gradient-to-b to-transparent p-[1px] max-sm:dark:block"
    >
      <div class="bg-bg-primary_alt h-24 rounded-t-2xl"></div>
    </div>
    <div class="flex flex-1 flex-col">
      <div
        bind:this={contentRef}
        class="relative max-h-[var(--max-content-height)] overflow-y-auto"
      >
        <div
          class="flex flex-1 shrink-0 flex-col px-4 pt-4 pb-4 sm:px-6 sm:pt-6 sm:pb-8"
        >
          {@render children?.()}
          {#if showCloseButton && nonNullish(onClose)}
            <button
              class="btn btn-tertiary btn-lg btn-icon absolute end-2 top-2 z-2 !rounded-full"
              onclick={onClose}
              aria-label={$t`Close`}
            >
              <XIcon class="size-5" aria-hidden="true" />
            </button>
          {/if}
        </div>
      </div>
      <!-- Element that pushes bottom sheet away from mobile keyboard or gesture navigation -->
      <div class="flex sm:hidden">
        <div class="h-[var(--keyboard-inset-height)]"></div>
        <div class="h-[env(safe-area-inset-bottom)]"></div>
      </div>
    </div>
    <!-- Element that pushes dialog away from mobile keyboard or gesture navigation -->
    <div class="flex max-sm:hidden">
      <div class="h-[var(--keyboard-inset-height)]"></div>
      <div class="h-[env(safe-area-inset-bottom)]"></div>
    </div>
  </div>
</dialog>
