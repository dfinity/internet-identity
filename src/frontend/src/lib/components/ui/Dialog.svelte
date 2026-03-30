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
  let contentRef = $state<HTMLDivElement | null>();
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
  let resolveOutro: (() => void) | undefined;

  onNavigate((navigation) => {
    if (navigation.to?.url.pathname === navigation.from?.url.pathname) return;
    isOpen = false;
    return new Promise<void>((resolve) => {
      resolveOutro = resolve;
    });
  });

  const onOutroEnd = () => {
    resolveOutro?.();
  };

  onMount(() => {
    dialogRef?.showModal();
    dialogRef?.setAttribute("data-visible", "true");

    // Use the virtualKeyboard API to intentionally render the software keyboard
    // on top of the page, we manually adjust the dialog positioning for it.
    //
    // If the API is not supported (e.g. iOS) polyfill it with visualViewport.
    let visualViewportResizeTimeout: ReturnType<typeof setTimeout>;
    const updateKeyboardInset = () => {
      dialogRef?.style.setProperty(
        "--keyboard-inset-height",
        `${Math.max(window.innerHeight - window.visualViewport!.height, 0)}px`,
      );
      dialogRef?.style.setProperty(
        "--max-content-height",
        `${window.visualViewport!.height}px`,
      );
    };
    const updateKeyboardInsetDebounced = () => {
      clearTimeout(visualViewportResizeTimeout);
      visualViewportResizeTimeout = setTimeout(updateKeyboardInset, 100);
    };
    if ("virtualKeyboard" in navigator) {
      (
        navigator.virtualKeyboard as { overlaysContent: boolean }
      ).overlaysContent = true;
    } else {
      updateKeyboardInset();
      window.visualViewport?.addEventListener(
        "resize",
        updateKeyboardInsetDebounced,
      );
      window.visualViewport?.addEventListener(
        "scroll",
        updateKeyboardInsetDebounced,
      );
    }
    return () => {
      if ("virtualKeyboard" in navigator) {
        (
          navigator.virtualKeyboard as { overlaysContent: boolean }
        ).overlaysContent = false;
      } else {
        window.visualViewport?.removeEventListener(
          "resize",
          updateKeyboardInsetDebounced,
        );
        window.visualViewport?.removeEventListener(
          "scroll",
          updateKeyboardInsetDebounced,
        );
      }
    };
  });
</script>

{#if isOpen}
  <dialog
    bind:this={dialogRef}
    oncancel={onCancel}
    closedby={closeOnOutsideClick ? "any" : "none"}
    class={[
      // Layout base/dialog/bottomsheet
      "fixed flex min-h-max max-w-full touch-none flex-col bg-transparent outline-none",
      "sm:m-auto sm:w-100",
      "max-sm:top-auto max-sm:bottom-0 max-sm:w-full",
      // Backdrop base/visible
      "backdrop:bg-bg-overlay backdrop:opacity-0 backdrop:transition-opacity backdrop:duration-200",
      backdrop && "data-visible:backdrop:opacity-80",
    ]}
    style="--keyboard-inset-height: env(keyboard-inset-height);--max-content-height: calc(100dvh - var(--keyboard-inset-height))"
    transition:transitionFn|global
    onoutrostart={fadeOutBackDrop}
    onoutroend={onOutroEnd}
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
        class={[
          "from-border-secondary pointer-events-none absolute top-0 right-0 left-0 z-1 hidden h-24 rounded-t-2xl bg-linear-to-b to-transparent p-px max-sm:dark:block",
          // Use a mask to only show the gradient on the border area, and prevent it from overlapping with the dialog content.
          "mask-exclude! [mask:linear-gradient(#fff_0_0)_content-box,linear-gradient(#fff_0_0)]",
        ]}
      ></div>
      <div class="flex flex-1 flex-col">
        <div
          bind:this={contentRef}
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
        <!-- Element that pushes bottom sheet away from mobile keyboard or gesture navigation -->
        <div class="flex sm:hidden">
          <div class="h-(--keyboard-inset-height)"></div>
          <div class="h-[env(safe-area-inset-bottom)]"></div>
        </div>
      </div>
      <!-- Element that pushes dialog away from mobile keyboard or gesture navigation -->
      <div class="flex max-sm:hidden">
        <div class="h-(--keyboard-inset-height)"></div>
        <!-- <div class="h-[env(keyboard-inset-height)]"></div> -->
        <div class="h-[env(safe-area-inset-bottom)]"></div>
      </div>
    </div>
  </dialog>
{/if}
