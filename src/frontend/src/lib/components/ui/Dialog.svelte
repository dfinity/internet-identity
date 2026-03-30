<script lang="ts">
  import { onMount } from "svelte";
  import { scale, fly } from "svelte/transition";
  import type { ClassValue, HTMLAttributes } from "svelte/elements";
  import { XIcon } from "@lucide/svelte";
  import { t } from "$lib/stores/locale.store";
  import { onNavigate } from "$app/navigation";

  type Props = HTMLAttributes<HTMLDialogElement> & {
    onClose?: () => void;
    closeOnOutsideClick?: boolean;
    showCloseButton?: boolean;
    backdrop?: boolean;
    contentClass?: ClassValue | null;
  };

  const {
    children,
    onClose,
    class: className,
    closeOnOutsideClick = onClose !== undefined,
    showCloseButton = onClose !== undefined,
    backdrop = true,
    contentClass,
    ...props
  }: Props = $props();

  let dialogRef = $state<HTMLDialogElement | null>();

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
    dialogRef?.removeAttribute("data-visible");
  };

  // Delay navigation until the dialog's outro has finished. Setting
  // isOpen to false triggers the {#if} block's |global outro, and
  // resolving the promise lets the navigation complete.
  let isOpen = $state(true);
  let resolveNavigation: (() => void) | undefined;

  onNavigate((navigation) => {
    if (navigation.to?.url.pathname === navigation.from?.url.pathname) return;
    isOpen = false;
    return new Promise<void>((resolve) => {
      resolveNavigation = resolve;
    });
  });

  const completeNavigation = () => {
    resolveNavigation?.();
  };

  const setupVirtualKeyboardApi = (): (() => void) => {
    const { virtualKeyboard } = navigator as Navigator & {
      virtualKeyboard: { overlaysContent: boolean };
    };
    virtualKeyboard.overlaysContent = true;
    return () => {
      virtualKeyboard.overlaysContent = false;
    };
  };

  const setupVisualViewportFallback = (): (() => void) => {
    let debounceTimeout: ReturnType<typeof setTimeout>;
    const updateKeyboardInset = () => {
      const keyboardHeight = Math.max(
        window.innerHeight - window.visualViewport!.height,
        0,
      );
      dialogRef?.style.setProperty(
        "--keyboard-inset-height",
        `${keyboardHeight}px`,
      );
      dialogRef?.style.setProperty(
        "--max-content-height",
        `${window.visualViewport!.height}px`,
      );
    };
    const updateDebounced = () => {
      clearTimeout(debounceTimeout);
      debounceTimeout = setTimeout(updateKeyboardInset, 100);
    };

    updateKeyboardInset();
    window.visualViewport?.addEventListener("resize", updateDebounced);
    window.visualViewport?.addEventListener("scroll", updateDebounced);

    return () => {
      window.visualViewport?.removeEventListener("resize", updateDebounced);
      window.visualViewport?.removeEventListener("scroll", updateDebounced);
    };
  };

  onMount(() => {
    dialogRef?.showModal();
    dialogRef?.setAttribute("data-visible", "true");

    // Keep the dialog above the software keyboard when it opens:
    // - Most browsers: the VirtualKeyboard API handles this natively.
    // - Safari/iOS: no API, so we measure the keyboard height ourselves
    //   and update the dialog's CSS variables to match.
    return "virtualKeyboard" in navigator
      ? setupVirtualKeyboardApi()
      : setupVisualViewportFallback();
  });
</script>

<!--
  Renders as a centered dialog on sm+ and a bottom sheet on mobile.
  On both layouts, spacer elements push the content above the software
  keyboard and safe-area insets.
-->
{#if isOpen}
  <dialog
    bind:this={dialogRef}
    oncancel={onCancel}
    closedby={closeOnOutsideClick ? "any" : "none"}
    class={[
      // Base: transparent overlay container, touch-none so only the
      // scrollable content area (touch-pan-y) responds to gestures.
      "fixed flex min-h-max max-w-full touch-none flex-col bg-transparent outline-none",
      // Dialog (sm+): centered with fixed width
      "sm:m-auto sm:w-100",
      // Bottom sheet (mobile): pinned to bottom, full width
      "max-sm:top-auto max-sm:bottom-0 max-sm:w-full",
      // Backdrop: fades in via data-visible attribute
      "backdrop:bg-bg-overlay backdrop:opacity-0 backdrop:transition-opacity backdrop:duration-200",
      backdrop && "data-visible:backdrop:opacity-80",
    ]}
    style="--keyboard-inset-height: env(keyboard-inset-height);--max-content-height: calc(100dvh - var(--keyboard-inset-height))"
    transition:transitionFn|global
    onoutrostart={fadeOutBackDrop}
    onoutroend={completeNavigation}
    {...props}
  >
    <div
      class={[
        // Base: card surface with clipped overflow
        "bg-bg-primary_alt border-border-secondary relative flex flex-col overflow-hidden",
        // Dialog (sm+): centered card with border and full rounding
        "min-h-max sm:m-auto sm:w-100 sm:rounded-2xl dark:sm:border",
        // Bottom sheet (mobile): full width, only top corners rounded
        "w-full rounded-t-2xl",
        className,
      ]}
    >
      <!-- Faux border gradient, visible only on dark-mode bottom sheet.
           Uses a mask to clip the gradient to just the border area. -->
      <div
        class={[
          "from-border-secondary pointer-events-none absolute top-0 right-0 left-0 z-1 hidden h-24 rounded-t-2xl bg-linear-to-b to-transparent p-px max-sm:dark:block",
          "mask-exclude! [mask:linear-gradient(#fff_0_0)_content-box,linear-gradient(#fff_0_0)]",
        ]}
      ></div>
      <div class="flex flex-1 flex-col">
        <!-- Scrollable content area. overscroll-contain prevents scroll
             chaining to the page; touch-pan-y re-enables native vertical
             scroll (the dialog itself is touch-none). -->
        <div
          class="relative touch-pan-y overflow-y-auto overscroll-contain max-sm:max-h-(--max-content-height) sm:max-h-[min(var(--max-content-height),48rem)]"
        >
          <div
            class={[
              "flex flex-1 shrink-0 flex-col px-4 pt-4 pb-4 sm:px-6 sm:pt-6 sm:pb-8",
              contentClass,
            ]}
          >
            {@render children?.()}
            {#if showCloseButton && onClose !== undefined}
              <button
                class="btn btn-tertiary btn-lg btn-icon absolute inset-e-2 top-2 z-2 rounded-full!"
                onclick={onClose}
                aria-label={$t`Close`}
              >
                <XIcon class="size-5" aria-hidden="true" />
              </button>
            {/if}
          </div>
        </div>
        <!-- Bottom sheet spacer: keyboard + safe area (hidden on sm+) -->
        <div class="flex sm:hidden">
          <div class="h-(--keyboard-inset-height)"></div>
          <div class="h-[env(safe-area-inset-bottom)]"></div>
        </div>
      </div>
      <!-- Dialog spacer: keyboard + safe area (hidden on mobile) -->
      <div class="flex max-sm:hidden">
        <div class="h-(--keyboard-inset-height)"></div>
        <div class="h-[env(safe-area-inset-bottom)]"></div>
      </div>
    </div>
  </dialog>
{/if}
