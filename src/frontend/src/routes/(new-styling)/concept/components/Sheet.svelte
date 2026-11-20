<script lang="ts">
  import { onMount, type Snippet } from "svelte";
  import { XIcon } from "@lucide/svelte";
  import { cubicOut, quintOut } from "svelte/easing";
  import { prefersReducedMotion } from "svelte/motion";
  import { fade, fly } from "svelte/transition";
  import { FLOATING, provideSurface } from "./Elevated.svelte";

  interface Props {
    onClose: () => void;
    /** Close at the top end, for content without one of its own. */
    showCloseButton?: boolean;
    /** Whether a tap outside, Escape and a swipe down put it away. Off where
     *  that would lose a step half done; its own buttons close it then. */
    dismissable?: boolean;
    "aria-labelledby"?: string;
    /** A name, for a sheet with no heading of its own to point at. */
    "aria-label"?: string;
    children: Snippet;
  }

  const {
    onClose,
    showCloseButton = false,
    dismissable = true,
    "aria-labelledby": labelledBy,
    "aria-label": label,
    children,
  }: Props = $props();

  // A sheet floats over the page, as the island and search do, and what it
  // holds stands on it: its cards a step above, never below.
  provideSurface(FLOATING);

  let dialog = $state<HTMLDialogElement>();
  let panel = $state<HTMLDivElement>();
  const reduced = $derived(prefersReducedMotion.current);

  /** How far the sheet is pulled down from where it rests, in px. */
  let pulled = $state(0);
  let dragging = $state(false);
  let pointer: { id: number; startY: number; y: number; t: number } | undefined;
  /** px per ms, downward, at the last move: a flick closes it short of the
   *  distance a slow pull needs. */
  let speed = 0;

  /** The software keyboard's height, so a field stays in view above it
   *  (iOS has no API for it: the visual viewport shrinks instead). */
  let keyboard = $state(0);
  let viewportHeight = $state<number>();

  onMount(() => {
    // What had focus, to hand it back once the sheet is gone: while it is
    // still going down, the page under it is inert and won't take focus.
    const opener = document.activeElement;
    dialog?.showModal();
    const viewport = window.visualViewport;
    const update = () => {
      if (viewport === null) return;
      keyboard = Math.max(
        0,
        window.innerHeight - viewport.height - viewport.offsetTop,
      );
      viewportHeight = viewport.height;
    };
    update();
    viewport?.addEventListener("resize", update);
    viewport?.addEventListener("scroll", update);
    return () => {
      viewport?.removeEventListener("resize", update);
      viewport?.removeEventListener("scroll", update);
      const active = document.activeElement;
      if (
        opener instanceof HTMLElement &&
        opener.isConnected &&
        (active === null || active === document.body)
      ) {
        opener.focus({ preventScroll: true });
      }
    };
  });

  const onCancel = (event: Event) => {
    event.preventDefault();
    if (dismissable) onClose();
  };

  // The grabber and the strip it sits in follow the finger down; letting go
  // past a third of the sheet, or with a flick, puts it away from where it
  // is, otherwise it settles back. Upwards it gives a little and resists, as
  // a sheet at its full height does.
  const onPointerDown = (event: PointerEvent) => {
    if (!dismissable || event.button !== 0) return;
    pointer = {
      id: event.pointerId,
      startY: event.clientY,
      y: event.clientY,
      t: event.timeStamp,
    };
    speed = 0;
    dragging = true;
    (event.currentTarget as HTMLElement).setPointerCapture(event.pointerId);
  };
  const onPointerMove = (event: PointerEvent) => {
    if (pointer?.id !== event.pointerId) return;
    const dy = event.clientY - pointer.startY;
    pulled = dy >= 0 ? dy : -Math.sqrt(-dy) * 2;
    const dt = Math.max(1, event.timeStamp - pointer.t);
    speed = (event.clientY - pointer.y) / dt;
    pointer = { ...pointer, y: event.clientY, t: event.timeStamp };
  };
  const onPointerUp = (event: PointerEvent) => {
    if (pointer?.id !== event.pointerId) return;
    pointer = undefined;
    dragging = false;
    const height = panel?.offsetHeight ?? Infinity;
    if (pulled > height / 3 || (speed > 0.5 && pulled > 16)) {
      onClose();
    } else {
      pulled = 0;
    }
  };
</script>

<!-- Phone: a sheet from the bottom, as iOS has them, over everything. The
     page underneath dims; a tap on it, Escape or a swipe down on the
     grabber puts the sheet away, and the sheet goes down from wherever the
     finger left it.
     The dialog clips rather than hides what runs past it: coming in, the
     sheet starts a whole height below, and a hidden overflow could still be
     scrolled, by the browser bringing what it focuses into view, which
     would throw the dimmed page and the sheet up mid-flight. A drag on the
     dimmed page does nothing, so it can't scroll or bounce what's under.
     Opening, the sheet takes focus itself instead of handing it to its
     first control: when that's a field (the add-an-app search), a phone
     would bring up its keyboard over the sheet and shove it and the page
     up. Tab goes on from it to the first control. -->
<dialog
  bind:this={dialog}
  oncancel={onCancel}
  aria-labelledby={labelledBy}
  aria-label={label}
  class="fixed inset-0 m-0 h-dvh max-h-none w-full max-w-none overflow-clip bg-transparent p-0 outline-none backdrop:bg-transparent"
>
  <!-- svelte-ignore a11y_click_events_have_key_events, a11y_no_static_element_interactions -->
  <div
    class="bg-bg-overlay/80 absolute inset-0 touch-none"
    onclick={() => dismissable && onClose()}
    transition:fade|global={{ duration: reduced ? 0 : 200 }}
  ></div>
  <div
    bind:this={panel}
    autofocus
    tabindex="-1"
    class={[
      "absolute inset-x-0 flex flex-col overflow-hidden rounded-t-2xl bg-(--ii-floating) outline-none",
      !dragging &&
        "transition-[translate] duration-300 ease-(--ii-ease-out) motion-reduce:transition-none",
    ]}
    style:bottom={`${keyboard}px`}
    style:max-height={viewportHeight !== undefined
      ? `${viewportHeight - 32}px`
      : "calc(100dvh - 2rem)"}
    style:translate={`0 ${pulled}px`}
    in:fly|global={{
      y: "100%",
      duration: reduced ? 0 : 340,
      easing: quintOut,
      opacity: 1,
    }}
    out:fly|global={{
      y: "100%",
      duration: reduced ? 0 : 220,
      easing: cubicOut,
      opacity: 1,
    }}
  >
    <div
      aria-hidden="true"
      class={[
        "flex h-6 shrink-0 touch-none items-end justify-center",
        dismissable && "cursor-grab active:cursor-grabbing",
      ]}
      onpointerdown={onPointerDown}
      onpointermove={onPointerMove}
      onpointerup={onPointerUp}
      onpointercancel={onPointerUp}
    >
      <span class="bg-fg-quaternary/50 h-1.5 w-9 rounded-full"></span>
    </div>
    <div
      class="relative min-h-0 flex-1 touch-pan-y overflow-y-auto overscroll-contain px-4 pt-2 pb-[calc(1rem+env(safe-area-inset-bottom))]"
    >
      {@render children()}
      {#if showCloseButton}
        <button
          type="button"
          onclick={onClose}
          class="btn btn-tertiary btn-lg btn-icon absolute inset-e-2 top-0 z-2 rounded-full!"
        >
          <XIcon class="size-5" aria-hidden="true" />
          <span>Close</span>
        </button>
      {/if}
    </div>
  </div>
</dialog>
