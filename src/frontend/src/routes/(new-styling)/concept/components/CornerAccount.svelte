<script lang="ts">
  import { tick, untrack } from "svelte";
  import IdentityAvatar from "$lib/components/ui/IdentityAvatar.svelte";
  import type { LastUsedIdentity } from "$lib/stores/last-used-identities.store";

  interface Props {
    /** Whether the panel beside the canvas is open. */
    open: boolean;
    identity: LastUsedIdentity;
    /** The account's size, so the canvas header keeps it room. */
    width?: number;
    height?: number;
    onOpen: () => void;
    /** The open account switches identity, as the panel's header did. */
    onSwitch: (anchor: HTMLElement) => void;
  }

  let {
    open,
    identity,
    width = $bindable(0),
    height = $bindable(0),
    onOpen,
    onSwitch,
  }: Props = $props();

  const name = $derived(identity.name ?? `${identity.identityNumber}`);

  // The avatar is the anchor: it keeps to the top-right corner, and the name
  // reads toward it from the left. Closed, it sits in the canvas header; open,
  // in the panel's first row, which ends almost where the header did, so the
  // account barely moves while the canvas slides out from under it. The
  // places come from the page's measures (+page.svelte), to the avatar's edge
  // and middle; the button's 4px end inset comes off the end distance:
  // - closed: 8px page padding + 1px frame + 24px header inset, and the
  //   middle of the 64px header that starts below those 9px;
  // - open: 8px page padding + 16px panel padding, and the middle of the
  //   panel's 56px first row, which starts at the page padding.
  const CLOSED_END = 33 - 4;
  const CLOSED_MIDDLE = 9 + 32;
  const OPEN_END = 24 - 4;
  const OPEN_MIDDLE = 8 + 28;

  let button = $state<HTMLButtonElement>();

  // Focus comes back to the account when the panel closes and goes inert.
  let wasOpen = untrack(() => open);
  $effect(() => {
    if (open === wasOpen) return;
    wasOpen = open;
    if (!open) {
      void tick().then(() => {
        const active = document.activeElement;
        const canvas = document.querySelector("main");
        if (active === null || canvas === null || !canvas.contains(active)) {
          button?.focus({ preventScroll: true });
        }
      });
    }
  });
</script>

<div
  class="fixed z-30 -translate-y-1/2 transition-[top,inset-inline-end] duration-300 ease-(--ii-ease-out) motion-reduce:transition-none"
  style:top={`${open ? OPEN_MIDDLE : CLOSED_MIDDLE}px`}
  style:inset-inline-end={`${open ? OPEN_END : CLOSED_END}px`}
>
  <button
    bind:this={button}
    bind:offsetWidth={width}
    bind:offsetHeight={height}
    type="button"
    onclick={() => (open ? button !== undefined && onSwitch(button) : onOpen())}
    aria-label={open
      ? `${name}, identity ${identity.identityNumber}. Switch identity`
      : `${name}. Open identity`}
    aria-expanded={open}
    class="ii-hover focus-visible:ring-focus-ring flex items-center gap-2.5 rounded-full py-1 ps-3 pe-1 outline-none focus-visible:ring-2 active:bg-(--ii-selected)"
  >
    <!-- Read toward the avatar, so both lines end at it. Open, the identity
         number unfolds under the name; closed, it takes no room at all. -->
    <span class="flex min-w-0 flex-col items-end text-end">
      <span class="text-text-primary max-w-40 truncate text-sm font-medium">
        {name}
      </span>
      <span
        class={[
          "grid transition-[grid-template-rows,opacity] duration-300 ease-(--ii-ease-out) motion-reduce:transition-none",
          open ? "grid-rows-[1fr] opacity-100" : "grid-rows-[0fr] opacity-0",
        ]}
      >
        <span
          class={[
            "text-text-tertiary min-h-0 overflow-hidden text-[13px] leading-[18px] whitespace-nowrap tabular-nums",
            !open && "w-0",
          ]}
        >
          Identity {identity.identityNumber}
        </span>
      </span>
    </span>
    <IdentityAvatar {identity} size="sm" />
  </button>
</div>
