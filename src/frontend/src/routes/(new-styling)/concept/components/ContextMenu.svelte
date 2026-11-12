<!--
  A right-click menu for the concept, ported to Svelte from beUI's Animated
  Context Menu (https://beui.dev/components/motion/context-menu).
  MIT License, Copyright (c) 2026 Saurabh Chauhan
  (https://github.com/starc007/ui-components/blob/main/LICENSE).

  Kept from beUI: opening at the pointer on a right-click, a long press on touch
  or pen, or Shift+F10 and the ContextMenu key; arrow keys, Home/End, typeahead;
  a highlight that glides between rows. Changed: the menu opens the way the
  product's popovers do (see `menuIn` in ../motion.ts) instead of beUI's clip
  morph, grown from the point it was opened at, and it uses the product's tokens.
-->
<script lang="ts" module>
  import type { Component } from "svelte";

  export type ContextMenuItem = {
    kind?: "item";
    label: string;
    icon?: Component;
    /** Renders a link, so the browser opens it like any other link. */
    href?: string;
    tone?: "default" | "destructive";
    disabled?: boolean;
    onSelect?: () => void;
  };

  export type ContextMenuEntry = ContextMenuItem | { kind: "separator" };

  type TriggerEvent<E extends Event> = E & {
    currentTarget: EventTarget & HTMLElement;
  };

  /** Spread onto the element that opens the menu. */
  export type ContextMenuTriggerProps = {
    "aria-haspopup": "menu";
    "aria-expanded": boolean;
    "aria-controls": string | undefined;
    oncontextmenu: (event: TriggerEvent<MouseEvent>) => void;
    onkeydown: (event: TriggerEvent<KeyboardEvent>) => void;
    onpointerdown: (event: TriggerEvent<PointerEvent>) => void;
    onpointermove: (event: PointerEvent) => void;
    onpointerup: () => void;
    onpointercancel: () => void;
  };
</script>

<script lang="ts">
  import type { Snippet } from "svelte";
  import { prefersReducedMotion } from "svelte/motion";
  import { menuIn, menuOut } from "../motion";

  interface Props {
    items: ContextMenuEntry[];
    /** A quiet line over the items, e.g. the address a link goes to. */
    title?: string;
    /** The menu's accessible name. */
    label: string;
    /** Leaves the browser's own menu alone. */
    disabled?: boolean;
    trigger: Snippet<[props: ContextMenuTriggerProps, open: boolean]>;
  }

  const { items, title, label, disabled = false, trigger }: Props = $props();

  interface Point {
    x: number;
    y: number;
  }

  const VIEWPORT_PADDING = 8;
  const LONG_PRESS_DELAY = 520;
  const LONG_PRESS_TOLERANCE = 10;
  const TYPEAHEAD_RESET = 500;

  const menuId = $props.id();
  const reduced = $derived(prefersReducedMotion.current);

  let open = $state(false);
  /** Each opening renders a fresh menu, placed where that opening happened. */
  let invocation = $state(0);
  let point: Point = { x: 0, y: 0 };
  let triggerRef: HTMLElement | undefined;
  let menuRef = $state<HTMLElement>();
  const itemRefs: HTMLElement[] = $state([]);
  let active = $state<number>();
  /** The highlight jumps to the first row it lands on and glides after that. */
  let glide = $state(false);

  const clamp = (value: number, min: number, max: number) =>
    Math.max(min, Math.min(value, max));

  const openAt = (at: Point, from: HTMLElement) => {
    point = at;
    triggerRef = from;
    active = undefined;
    glide = false;
    invocation += 1;
    open = true;
  };

  const close = (returnFocus: boolean) => {
    if (!open) {
      return;
    }
    open = false;
    if (returnFocus) {
      triggerRef?.focus({ preventScroll: true });
    }
  };

  const enabledItems = (): HTMLElement[] =>
    menuRef === undefined
      ? []
      : Array.from(
          menuRef.querySelectorAll<HTMLElement>(
            "[data-menu-item]:not(:disabled)",
          ),
        );

  // --- Trigger ---

  let pressTimer: ReturnType<typeof setTimeout> | undefined;
  let pressOrigin: Point | undefined;
  /** A long press opened the menu during this press. */
  let pressOpened = false;
  let releasePress: (() => void) | undefined;

  const cancelPress = () => {
    clearTimeout(pressTimer);
    pressTimer = undefined;
    pressOrigin = undefined;
  };

  const endPress = () => {
    cancelPress();
    releasePress?.();
    releasePress = undefined;
  };

  // The platform runs its own long press on the same gesture (text selection,
  // the link preview on iOS) and would take it from us, so the press holds both
  // off for its own duration and no longer.
  const holdPlatformPress = (node: HTMLElement) => {
    const userSelect = node.style.userSelect;
    const webkitUserSelect = node.style.webkitUserSelect;
    const callout = node.style.getPropertyValue("-webkit-touch-callout");
    node.style.userSelect = "none";
    node.style.webkitUserSelect = "none";
    node.style.setProperty("-webkit-touch-callout", "none");
    return () => {
      node.style.userSelect = userSelect;
      node.style.webkitUserSelect = webkitUserSelect;
      node.style.setProperty("-webkit-touch-callout", callout);
    };
  };

  // Lifting the finger after a long press would still click the trigger and,
  // on a link, open it under the menu.
  const swallowClick = (event: MouseEvent) => {
    event.preventDefault();
    event.stopPropagation();
  };

  const onTriggerContextMenu = (event: TriggerEvent<MouseEvent>) => {
    if (disabled) {
      return;
    }
    event.preventDefault();
    // Android follows a long press with its own `contextmenu`.
    if (pressOpened) {
      return;
    }
    endPress();
    openAt({ x: event.clientX, y: event.clientY }, event.currentTarget);
  };

  const onTriggerKeydown = (event: TriggerEvent<KeyboardEvent>) => {
    if (disabled || event.defaultPrevented) {
      return;
    }
    if (
      event.key !== "ContextMenu" &&
      !(event.shiftKey && event.key === "F10")
    ) {
      return;
    }
    event.preventDefault();
    const rect = event.currentTarget.getBoundingClientRect();
    openAt(
      {
        x: rect.left + Math.min(24, rect.width / 2),
        y: rect.top + rect.height / 2,
      },
      event.currentTarget,
    );
  };

  const onTriggerPointerDown = (event: TriggerEvent<PointerEvent>) => {
    const node = event.currentTarget;
    node.removeEventListener("click", swallowClick, { capture: true });
    pressOpened = false;
    // A mouse has its right button; a finger or a pen holds instead.
    if (
      disabled ||
      event.defaultPrevented ||
      (event.pointerType !== "touch" && event.pointerType !== "pen")
    ) {
      return;
    }
    releasePress?.();
    releasePress = holdPlatformPress(node);
    const origin = { x: event.clientX, y: event.clientY };
    pressOrigin = origin;
    pressTimer = setTimeout(() => {
      pressTimer = undefined;
      pressOrigin = undefined;
      pressOpened = true;
      node.addEventListener("click", swallowClick, {
        capture: true,
        once: true,
      });
      openAt(origin, node);
    }, LONG_PRESS_DELAY);
  };

  const onTriggerPointerMove = (event: PointerEvent) => {
    if (
      pressOrigin !== undefined &&
      Math.hypot(event.clientX - pressOrigin.x, event.clientY - pressOrigin.y) >
        LONG_PRESS_TOLERANCE
    ) {
      cancelPress();
    }
  };

  const triggerProps = $derived<ContextMenuTriggerProps>({
    "aria-haspopup": "menu",
    "aria-expanded": open,
    "aria-controls": open ? menuId : undefined,
    oncontextmenu: onTriggerContextMenu,
    onkeydown: onTriggerKeydown,
    onpointerdown: onTriggerPointerDown,
    onpointermove: onTriggerPointerMove,
    onpointerup: endPress,
    onpointercancel: endPress,
  });

  $effect(() => endPress);

  // --- Menu ---

  /** Puts the menu at the point, kept inside the viewport, and grows it from
   *  there. Runs before the first paint, so it never shows anywhere else. */
  const place = (node: HTMLElement) => {
    node.showPopover();
    // Layout sizes: the opening scale is already on the menu at this point.
    const width = node.offsetWidth;
    const height = node.offsetHeight;
    const left = clamp(
      point.x,
      VIEWPORT_PADDING,
      window.innerWidth - width - VIEWPORT_PADDING,
    );
    const top = clamp(
      point.y,
      VIEWPORT_PADDING,
      window.innerHeight - height - VIEWPORT_PADDING,
    );
    node.style.left = `${left}px`;
    node.style.top = `${top}px`;
    node.style.transformOrigin = `${clamp(point.x - left, 12, Math.max(12, width - 12))}px ${clamp(point.y - top, 12, Math.max(12, height - 12))}px`;

    const frame = requestAnimationFrame(() =>
      enabledItems()[0]?.focus({ preventScroll: true }),
    );
    return () => cancelAnimationFrame(frame);
  };

  $effect(() => {
    if (!open) {
      return;
    }
    const onPointerDown = (event: PointerEvent) => {
      if (
        event.target instanceof Node &&
        menuRef?.contains(event.target) === true
      ) {
        return;
      }
      close(false);
    };
    const onViewportChange = () =>
      close(menuRef?.contains(document.activeElement) === true);
    window.addEventListener("pointerdown", onPointerDown, true);
    window.addEventListener("resize", onViewportChange);
    window.addEventListener("scroll", onViewportChange, true);
    return () => {
      window.removeEventListener("pointerdown", onPointerDown, true);
      window.removeEventListener("resize", onViewportChange);
      window.removeEventListener("scroll", onViewportChange, true);
    };
  });

  const activate = (index: number) => {
    glide = active !== undefined;
    active = index;
  };

  const select = (item: ContextMenuItem) => {
    close(true);
    item.onSelect?.();
  };

  let typeahead = "";
  let typeaheadTimer: ReturnType<typeof setTimeout> | undefined;

  const onMenuKeydown = (event: KeyboardEvent) => {
    const enabled = enabledItems();
    const current = enabled.findIndex(
      (item) => item === document.activeElement,
    );

    if (event.key === "Escape") {
      // Taken here, so the page's own Escape leaves its panel alone.
      event.preventDefault();
      close(true);
    } else if (event.key === "Tab") {
      // Focus moves on to what follows the tile, as from any menu.
      close(false);
    } else if (event.key === "ArrowDown" || event.key === "ArrowUp") {
      event.preventDefault();
      const step = event.key === "ArrowDown" ? 1 : -1;
      const next =
        current < 0 ? 0 : (current + step + enabled.length) % enabled.length;
      enabled[next]?.focus();
    } else if (event.key === "Home" || event.key === "End") {
      event.preventDefault();
      enabled[event.key === "Home" ? 0 : enabled.length - 1]?.focus();
    } else if (
      event.key === " " &&
      document.activeElement instanceof HTMLAnchorElement
    ) {
      // A button takes Space by itself; a link row needs it passed on.
      event.preventDefault();
      document.activeElement.click();
    } else if (
      event.key.length === 1 &&
      !event.ctrlKey &&
      !event.metaKey &&
      !event.altKey
    ) {
      typeahead += event.key.toLocaleLowerCase();
      clearTimeout(typeaheadTimer);
      typeaheadTimer = setTimeout(() => (typeahead = ""), TYPEAHEAD_RESET);
      enabled
        .find((item) =>
          (item.textContent ?? "")
            .trim()
            .toLocaleLowerCase()
            .startsWith(typeahead),
        )
        ?.focus();
    }
  };

  const ROW =
    "relative flex w-full items-center gap-2.5 rounded-lg px-2.5 py-2 text-start text-[13px] leading-[18px] outline-none select-none disabled:opacity-40";

  const activeItem = $derived(
    active === undefined ? undefined : itemRefs[active],
  );
  const activeTone = $derived.by(() => {
    const entry = active === undefined ? undefined : items[active];
    return entry !== undefined && entry.kind !== "separator"
      ? (entry.tone ?? "default")
      : "default";
  });
</script>

{@render trigger(triggerProps, open)}

{#snippet row(item: ContextMenuItem)}
  {#if item.icon !== undefined}
    {@const Icon = item.icon}
    <span
      aria-hidden="true"
      class={[
        "shrink-0 [&_svg]:size-4",
        item.tone === "destructive"
          ? "text-fg-error-primary"
          : "text-fg-quaternary dark:text-fg-tertiary",
      ]}
    >
      <Icon />
    </span>
  {/if}
  <span class="truncate">{item.label}</span>
{/snippet}

{#if open}
  {#key invocation}
    <div
      bind:this={menuRef}
      id={menuId}
      popover="manual"
      role="menu"
      tabindex="-1"
      aria-label={label}
      {@attach place}
      in:menuIn|global={{ reduced }}
      out:menuOut|global={{ reduced }}
      onkeydown={onMenuKeydown}
      oncontextmenu={(event) => event.preventDefault()}
      class="border-border-secondary bg-bg-primary_alt fixed inset-auto m-0 flex max-w-72 min-w-56 flex-col overflow-hidden rounded-xl border p-1.5 shadow-lg outline-none"
    >
      <!-- One highlight for the whole menu, so it can glide from row to row. -->
      <span
        aria-hidden="true"
        class={[
          "pointer-events-none absolute inset-x-1.5 top-0 rounded-lg",
          activeTone === "destructive"
            ? "bg-bg-error-primary"
            : "bg-(--ii-selected)",
          glide &&
            !reduced &&
            "transition-[translate,height,background-color] duration-200 ease-(--ii-ease-out)",
        ]}
        style:opacity={activeItem === undefined ? 0 : 1}
        style:translate={`0 ${activeItem?.offsetTop ?? 0}px`}
        style:height={`${activeItem?.offsetHeight ?? 0}px`}
      ></span>

      {#if title !== undefined}
        <div
          aria-hidden="true"
          class="text-text-tertiary truncate px-2.5 pt-1.5 pb-1 text-xs"
        >
          {title}
        </div>
      {/if}

      {#each items as entry, index (index)}
        {#if entry.kind === "separator"}
          <hr class="border-border-secondary -mx-1.5 my-1" />
        {:else if entry.href !== undefined}
          <a
            bind:this={itemRefs[index]}
            href={entry.href}
            target="_blank"
            rel="noopener noreferrer"
            role="menuitem"
            tabindex="-1"
            data-menu-item
            onfocus={() => activate(index)}
            onpointermove={(event) => {
              if (event.pointerType !== "touch") {
                event.currentTarget.focus({ preventScroll: true });
              }
            }}
            onclick={() => select(entry)}
            class={[
              ROW,
              entry.tone === "destructive"
                ? "text-text-error-primary"
                : "text-text-primary",
            ]}
          >
            {@render row(entry)}
          </a>
        {:else}
          <button
            bind:this={itemRefs[index]}
            type="button"
            role="menuitem"
            tabindex="-1"
            disabled={entry.disabled}
            data-menu-item
            onfocus={() => activate(index)}
            onpointermove={(event) => {
              if (event.pointerType !== "touch") {
                event.currentTarget.focus({ preventScroll: true });
              }
            }}
            onclick={() => select(entry)}
            class={[
              ROW,
              entry.tone === "destructive"
                ? "text-text-error-primary"
                : "text-text-primary",
            ]}
          >
            {@render row(entry)}
          </button>
        {/if}
      {/each}
    </div>
  {/key}
{/if}
