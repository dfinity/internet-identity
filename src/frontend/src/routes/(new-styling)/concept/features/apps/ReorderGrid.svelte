<!--
  A grid of tiles that can be rearranged by dragging, the way a home screen is
  in its edit mode: the tile lifts and follows the pointer, the others step
  aside to make room, and the last row shows its free cells as places to drop.
  Escape, or a cancelled pointer, puts the tile back where it started.
-->
<script lang="ts" generics="T">
  import type { Snippet } from "svelte";
  import { flip } from "svelte/animate";
  import { quintOut } from "svelte/easing";
  import { prefersReducedMotion } from "svelte/motion";
  import { fade } from "svelte/transition";

  interface Props {
    items: T[];
    key: (item: T) => string;
    /** Rearranging is on: tiles can be dragged, and free cells show. */
    editing: boolean;
    /** Moves the item at `from` to `to`; the grid follows the new order. */
    onMove: (from: number, to: number) => void;
    /** Items that keep their place at the end, like the tile that adds an
     *  app: they can't be picked up, and nothing is dropped past them. They
     *  stay in the grid while rearranging, so it keeps its rows. */
    pinned?: (item: T) => boolean;
    tile: Snippet<[item: T, index: number]>;
    class?: string;
  }

  const {
    items,
    key,
    editing,
    onMove,
    pinned = () => false,
    tile,
    class: className,
  }: Props = $props();

  /** How many items can move: the pinned ones close the list. */
  const movable = $derived(items.filter((item) => !pinned(item)).length);

  interface Point {
    x: number;
    y: number;
  }
  interface Cell {
    left: number;
    top: number;
    right: number;
    bottom: number;
  }

  /** How far a press travels before it counts as a drag, not a click. */
  const DRAG_THRESHOLD = 6;
  /** The other tiles stepping aside. */
  const SHUFFLE = 260;
  /** The dropped tile easing into its cell. */
  const SETTLE = 220;

  const reduced = $derived(prefersReducedMotion.current);

  let list = $state<HTMLUListElement>();
  let columns = $state(1);

  let press:
    | {
        key: string;
        pointerId: number;
        at: Point;
        /** Escape put the tile back; the press ends with the release. */
        cancelled?: boolean;
      }
    | undefined;
  let drag = $state<{
    key: string;
    from: number;
    start: Point;
    pointer: Point;
  }>();
  /** Every cell's place on screen, taken when a drag starts. Cells don't move
   *  during a drag, only what's in them, so this stays true until the drop. */
  let cells: Cell[] = [];
  let settling = $state<string>();
  let settleTimer: ReturnType<typeof setTimeout> | undefined;

  // The row the grid ends on is filled with free cells, as many as it lacks.
  const freeCells = $derived(
    editing
      ? Array.from(
          { length: (columns - (items.length % columns)) % columns },
          (_, cell) => cell,
        )
      : [],
  );

  // Columns change with the width of the canvas, so they're read off the grid.
  $effect(() => {
    if (list === undefined) {
      return;
    }
    const node = list;
    const measure = () => {
      columns = Math.max(
        1,
        getComputedStyle(node)
          .gridTemplateColumns.split(" ")
          .filter((track) => track !== "").length,
      );
    };
    measure();
    const observer = new ResizeObserver(measure);
    observer.observe(node);
    return () => observer.disconnect();
  });

  const indexOf = (itemKey: string) =>
    items.findIndex((item) => key(item) === itemKey);

  // Layout offsets, not bounding boxes: a tile still stepping aside from the
  // last move must not shift where its cell is.
  const measureCells = (): Cell[] => {
    if (list === undefined) {
      return [];
    }
    const origin = list.getBoundingClientRect();
    return Array.from(list.children, (child) => {
      const cell = child as HTMLElement;
      const left = origin.left + cell.offsetLeft;
      const top = origin.top + cell.offsetTop;
      return {
        left,
        top,
        right: left + cell.offsetWidth,
        bottom: top + cell.offsetHeight,
      };
    });
  };

  /** Where the lifted tile is drawn: under the pointer, from whichever cell
   *  it occupies now. */
  const offset = $derived.by(() => {
    if (drag === undefined) {
      return undefined;
    }
    const home = cells[drag.from];
    const here = cells[indexOf(drag.key)];
    if (home === undefined || here === undefined) {
      return undefined;
    }
    return {
      x: drag.pointer.x - drag.start.x - (here.left - home.left),
      y: drag.pointer.y - drag.start.y - (here.top - home.top),
    };
  });

  let unwatchTarget = () => {};

  // A finger's events keep going to the element it came down on even after
  // that element has left the page, and a long press into managing replaces
  // the tile's link with a button. Detached, the events no longer reach the
  // window, so they're taken on the element itself until the press ends.
  const watchTarget = (target: EventTarget | null) => {
    unwatchTarget();
    if (!(target instanceof Element)) {
      return;
    }
    const node = target as HTMLElement;
    const move = (event: PointerEvent) => {
      if (!node.isConnected) {
        onPointerMove(event);
      }
    };
    const up = (event: PointerEvent) => {
      if (!node.isConnected) {
        finish(event, false);
      }
    };
    const cancel = (event: PointerEvent) => {
      if (!node.isConnected) {
        finish(event, true);
      }
    };
    const touchMove = (event: TouchEvent) => {
      if (!node.isConnected) {
        onTouchMove(event);
      }
    };
    node.addEventListener("pointermove", move);
    node.addEventListener("pointerup", up);
    node.addEventListener("pointercancel", cancel);
    node.addEventListener("touchmove", touchMove, { passive: false });
    unwatchTarget = () => {
      node.removeEventListener("pointermove", move);
      node.removeEventListener("pointerup", up);
      node.removeEventListener("pointercancel", cancel);
      node.removeEventListener("touchmove", touchMove);
      unwatchTarget = () => {};
    };
  };

  // Taken outside editing too: a tile held until the grid turns editable (a
  // long press into managing) can go straight on into a drag.
  const onPointerDown = (event: PointerEvent, itemKey: string) => {
    if (!event.isPrimary || event.button !== 0 || drag !== undefined) {
      return;
    }
    press = {
      key: itemKey,
      pointerId: event.pointerId,
      at: { x: event.clientX, y: event.clientY },
    };
    if (event.pointerType !== "mouse") {
      watchTarget(event.target);
    }
  };

  const settle = (itemKey: string) => {
    clearTimeout(settleTimer);
    settling = itemKey;
    settleTimer = setTimeout(() => (settling = undefined), SETTLE);
  };

  const putBack = () => {
    if (drag === undefined) {
      return;
    }
    const at = indexOf(drag.key);
    if (at !== drag.from) {
      onMove(at, drag.from);
    }
  };

  // The release after a drag would click the tile under it, which picks or
  // unpicks it. The click follows in the same task, so a zero timeout outlives
  // it and nothing later.
  const swallowClick = (event: MouseEvent) => {
    event.preventDefault();
    event.stopPropagation();
  };

  // Leaving editing drops any drag in progress.
  $effect(() => {
    if (!editing) {
      drag = undefined;
    }
  });

  const onPointerMove = (event: PointerEvent) => {
    if (
      !editing ||
      press === undefined ||
      press.cancelled === true ||
      event.pointerId !== press.pointerId
    ) {
      return;
    }
    const pointer = { x: event.clientX, y: event.clientY };
    if (drag === undefined) {
      if (
        Math.hypot(pointer.x - press.at.x, pointer.y - press.at.y) <
        DRAG_THRESHOLD
      ) {
        return;
      }
      cells = measureCells();
      drag = {
        key: press.key,
        from: indexOf(press.key),
        start: press.at,
        pointer,
      };
    } else {
      drag.pointer = pointer;
    }
    // Between cells, the tile keeps the place it has.
    const cell = cells.findIndex(
      ({ left, top, right, bottom }) =>
        pointer.x >= left &&
        pointer.x < right &&
        pointer.y >= top &&
        pointer.y < bottom,
    );
    if (cell < 0) {
      return;
    }
    // A free or pinned cell means the end of the grid: tiles never leave a
    // gap, and never pass what's pinned.
    const to = Math.min(cell, movable - 1);
    const from = indexOf(drag.key);
    if (to !== from) {
      onMove(from, to);
    }
  };

  const finish = (event: PointerEvent, cancelled: boolean) => {
    if (press === undefined || event.pointerId !== press.pointerId) {
      return;
    }
    const escaped = press.cancelled === true;
    press = undefined;
    unwatchTarget();
    if (!cancelled && (drag !== undefined || escaped)) {
      window.addEventListener("click", swallowClick, {
        capture: true,
        once: true,
      });
      setTimeout(() =>
        window.removeEventListener("click", swallowClick, { capture: true }),
      );
    }
    if (drag === undefined) {
      return;
    }
    if (cancelled) {
      putBack();
    }
    const dropped = drag.key;
    drag = undefined;
    settle(dropped);
  };
  const onPointerUp = (event: PointerEvent) => finish(event, false);
  const onPointerCancel = (event: PointerEvent) => finish(event, true);

  // Captured, so the page's own Escape doesn't also close its panel.
  const onKeyDown = (event: KeyboardEvent) => {
    if (event.key !== "Escape" || drag === undefined) {
      return;
    }
    event.preventDefault();
    putBack();
    const dropped = drag.key;
    if (press !== undefined) {
      press.cancelled = true;
    }
    drag = undefined;
    settle(dropped);
  };

  // A finger that was already down when editing began started under
  // `touch-action: auto`, so the page would scroll instead of the tile
  // moving. Holding still until then kept it from scrolling; this keeps it so.
  const onTouchMove = (event: TouchEvent) => {
    if (editing && press !== undefined && press.cancelled !== true) {
      event.preventDefault();
    }
  };

  $effect(() => {
    window.addEventListener("pointermove", onPointerMove);
    window.addEventListener("pointerup", onPointerUp);
    window.addEventListener("pointercancel", onPointerCancel);
    window.addEventListener("keydown", onKeyDown, true);
    window.addEventListener("touchmove", onTouchMove, { passive: false });
    return () => {
      window.removeEventListener("pointermove", onPointerMove);
      window.removeEventListener("pointerup", onPointerUp);
      window.removeEventListener("pointercancel", onPointerCancel);
      window.removeEventListener("keydown", onKeyDown, true);
      window.removeEventListener("touchmove", onTouchMove);
      unwatchTarget();
      press = undefined;
      drag = undefined;
    };
  });
</script>

<!-- While rearranging, a press on a tile is the drag's: no text selection, no
     native image drag, and on touch no scroll. -->
<ul
  bind:this={list}
  ondragstart={(event) => {
    if (editing) {
      event.preventDefault();
    }
  }}
  class={["relative", editing && "touch-none select-none", className]}
>
  {#each items as item, index (key(item))}
    <li
      animate:flip={{
        duration: reduced || drag?.key === key(item) ? 0 : SHUFFLE,
        easing: quintOut,
      }}
      data-dragging={drag?.key === key(item) ? "" : undefined}
      onpointerdown={(event) => {
        if (!pinned(item)) {
          onPointerDown(event, key(item));
        }
      }}
      class={[
        "relative",
        drag?.key === key(item) && [
          "z-10 cursor-grabbing",
          !reduced && "transition-[scale] duration-150 ease-out",
        ],
        settling === key(item) && [
          "z-10",
          !reduced &&
            "transition-[translate,scale] duration-220 ease-(--ii-ease-out)",
        ],
      ]}
      style:translate={drag?.key === key(item) && offset !== undefined
        ? `${offset.x}px ${offset.y}px`
        : undefined}
      style:scale={drag?.key === key(item) && !reduced ? "1.08" : undefined}
    >
      {@render tile(item, index)}
    </li>
  {/each}
  {#each freeCells as cell (cell)}
    <li
      aria-hidden="true"
      in:fade={{ duration: reduced ? 0 : 150 }}
      out:fade={{ duration: reduced ? 0 : 100 }}
      class="flex justify-center pt-4"
    >
      <!-- The same dashed frame as the tile that adds an app, which turns
           into the first of these while apps are arranged. -->
      <span
        class="border-border-primary size-(--ii-logo) rounded-2xl border-[1.5px] border-dashed"
      ></span>
    </li>
  {/each}
</ul>
