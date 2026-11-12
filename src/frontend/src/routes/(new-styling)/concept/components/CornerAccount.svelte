<script lang="ts">
  import { ChevronDownIcon, ChevronsUpDownIcon, XIcon } from "@lucide/svelte";
  import { tick, untrack } from "svelte";
  import IdentityAvatar from "$lib/components/ui/IdentityAvatar.svelte";
  import type { LastUsedIdentity } from "$lib/stores/last-used-identities.store";
  import type { Tone } from "../status";

  interface Props {
    /** Whether the panel beside the canvas is open. */
    open: boolean;
    identity: LastUsedIdentity;
    /** What waits on the user, for the dot and the line under the name. */
    status?: { sentence: string; tone: Tone };
    /** The closed account's outer size, so the canvas header keeps it room. */
    width?: number;
    height?: number;
    onOpen: () => void;
    onClose: () => void;
    /** The open account switches identity, as the panel's header did. */
    onSwitch: (anchor: HTMLElement) => void;
  }

  let {
    open,
    identity,
    status,
    width = $bindable(0),
    height = $bindable(0),
    onOpen,
    onClose,
    onSwitch,
  }: Props = $props();

  const name = $derived(identity.name ?? `${identity.identityNumber}`);

  // One account, two places, both in the top-right corner. Closed, it is a pill
  // in the canvas header; open, it is the first row of the panel on the page
  // beside the canvas. It moves between them while the panel slides in, so
  // the account never leaves the corner. The places come from the page's own
  // measures (+page.svelte):
  // - closed: 8px page padding + 1px frame + 24px header inset from the end,
  //   centered on the 64px header that starts below 9px of padding and frame;
  // - open: the panel's 16px end padding inside the 8px page padding, 4px
  //   below its top, 25rem wide less 24px + 16px of padding. The shell's 1px
  //   border sits outside both.
  const CLOSED_END = 33;
  const HEADER_MIDDLE = 41;
  const OPEN_END = 23;
  const OPEN_TOP = 11;
  const OPEN_WIDTH = 362;

  // The pill's size, taken only while closed: open, the row fills the panel.
  let rowWidth = $state(0);
  let rowHeight = $state(0);
  $effect(() => {
    const w = rowWidth;
    const h = rowHeight;
    if (w === 0 || untrack(() => open)) return;
    width = w + 2;
    height = h + 2;
  });

  // Focus stays on the account: opening keeps it on the same button, closing
  // brings it back from the panel, which goes inert.
  let main = $state<HTMLButtonElement>();
  let wasOpen = untrack(() => open);
  $effect(() => {
    if (open === wasOpen) return;
    wasOpen = open;
    if (!open) {
      void tick().then(() => {
        const active = document.activeElement;
        const canvas = document.querySelector("main");
        if (active === null || canvas === null || !canvas.contains(active)) {
          main?.focus({ preventScroll: true });
        }
      });
    }
  });

  const DOT: Record<Tone, string> = {
    risk: "bg-fg-error-primary",
    action: "bg-fg-warning-primary",
    queued: "bg-fg-warning-primary",
    pending: "bg-fg-tertiary",
    now: "bg-fg-primary",
    recent: "bg-fg-primary",
    tip: "bg-fg-quaternary",
    calm: "bg-fg-quaternary",
  };

  // What changes between the two places fades through a short blur; the
  // avatar and the name stay, and only travel.
  const SWAP =
    "min-w-0 transition-[opacity,filter,scale] duration-200 ease-out motion-reduce:transition-none";
  const GONE = "pointer-events-none opacity-0 blur-[3px]";
</script>

<div
  class={[
    "fixed z-30 overflow-hidden border border-transparent",
    "transition-[top,inset-inline-end,width,border-radius,box-shadow,background-color] ease-(--ii-ease-out) motion-reduce:transition-none",
    open ? "ii-surface-1" : "ii-surface-6",
  ]}
  style:top={`${open ? OPEN_TOP : HEADER_MIDDLE - height / 2}px`}
  style:inset-inline-end={`${open ? OPEN_END : CLOSED_END}px`}
  style:width={width > 0 ? `${open ? OPEN_WIDTH : width}px` : undefined}
  style:border-radius={open ? "1rem" : `${height / 2}px`}
  style:box-shadow={open ? "none" : "var(--ii-shadow-6)"}
  style:transition-duration={open ? "340ms" : "280ms"}
>
  <div
    bind:offsetWidth={rowWidth}
    bind:offsetHeight={rowHeight}
    class={["flex items-center", open ? "w-full gap-1" : "w-max"]}
  >
    <button
      bind:this={main}
      type="button"
      onclick={() => (open ? main !== undefined && onSwitch(main) : onOpen())}
      aria-label={open
        ? `${name}, identity ${identity.identityNumber}. Switch identity`
        : `${name}${status !== undefined ? `: ${status.sentence}` : ""}. Open identity`}
      aria-expanded={open ? undefined : false}
      class={[
        "group/identity focus-visible:ring-focus-ring flex min-w-0 flex-1 items-center gap-3 text-start outline-none focus-visible:ring-2 focus-visible:ring-inset",
        "transition-[padding,scale] duration-300 ease-(--ii-ease-out) motion-reduce:transition-none",
        open
          ? "rounded-2xl px-0 py-2"
          : "ii-hover rounded-full p-1.5 active:scale-[0.97] motion-reduce:active:scale-100",
      ]}
    >
      <span class="relative shrink-0">
        <IdentityAvatar {identity} size="sm" />
        {#if status !== undefined}
          <span
            aria-hidden="true"
            class={[
              "absolute -end-0.5 -top-0.5 size-3 rounded-full ring-2 ring-(color:--ii-surface-6) transition-[opacity,scale] duration-200",
              DOT[status.tone],
              open && "scale-50 opacity-0",
            ]}
          ></span>
        {/if}
      </span>
      <!-- The panel's two sizes and two weights in both places: a 14px medium
           label over a 13px regular line, the same as every row below. -->
      <span class={["flex min-w-0 flex-1 flex-col", !open && "max-w-48"]}>
        <span class="text-text-primary truncate text-sm font-medium">
          {name}
        </span>
        <span
          class="grid text-[13px] leading-[18px] *:col-start-1 *:row-start-1"
        >
          <span class={[SWAP, "text-text-tertiary truncate", open && GONE]}>
            {status?.sentence ?? "All set"}
          </span>
          <span
            class={[
              SWAP,
              "text-text-tertiary truncate tabular-nums",
              !open && GONE,
            ]}
          >
            Identity {identity.identityNumber}
          </span>
        </span>
      </span>
      <!-- A chevron says the pill opens; open, it turns into the switch
           control, which lights up on hover as the panel's header did. -->
      <span
        aria-hidden="true"
        class="grid size-8 shrink-0 place-items-center *:col-start-1 *:row-start-1"
      >
        <ChevronDownIcon
          class={[
            "text-fg-quaternary size-4 transition-[opacity,scale] duration-200",
            open && "scale-50 opacity-0",
          ]}
        />
        <span
          class={[
            "text-fg-tertiary grid size-8 place-items-center rounded-full transition-[opacity,scale,background-color] duration-200 group-hover/identity:bg-(--ii-hover) group-active/identity:bg-(--ii-selected)",
            !open && "scale-50 opacity-0",
          ]}
        >
          <ChevronsUpDownIcon class="size-4" />
        </span>
      </span>
    </button>
    <!-- Close arrives with the panel, drawn like the switch control beside it. -->
    <button
      type="button"
      onclick={onClose}
      inert={!open}
      class={[
        "text-fg-tertiary hover:text-fg-secondary focus-visible:ring-focus-ring grid h-8 shrink-0 place-items-center overflow-hidden rounded-full outline-none hover:bg-(--ii-hover) focus-visible:ring-2 focus-visible:ring-inset active:bg-(--ii-selected)",
        "transition-[width,opacity] duration-300 ease-(--ii-ease-out) motion-reduce:transition-none",
        open ? "w-8 opacity-100" : "w-0 opacity-0",
      ]}
    >
      <XIcon class="size-4 shrink-0" aria-hidden="true" />
      <span class="sr-only">Close</span>
    </button>
  </div>
</div>
