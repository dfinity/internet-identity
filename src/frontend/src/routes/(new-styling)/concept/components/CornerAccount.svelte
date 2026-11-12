<script lang="ts">
  import { tick, untrack } from "svelte";
  import IdentityAvatar from "$lib/components/ui/IdentityAvatar.svelte";
  import type { LastUsedIdentity } from "$lib/stores/last-used-identities.store";
  import { SWITCH_LIST_ID } from "./IdentityCard.svelte";

  interface Props {
    /** Whether the panel beside the canvas is open. */
    open: boolean;
    identity: LastUsedIdentity;
    /** The account's size, so the canvas header keeps it room. */
    width?: number;
    height?: number;
    /** Whether the open panel lists the identities to switch to. */
    switching: boolean;
    onOpen: () => void;
    /** The open account lists the identities to switch to, in the panel. */
    onSwitch: () => void;
  }

  let {
    open,
    identity,
    width = $bindable(0),
    height = $bindable(0),
    switching,
    onOpen,
    onSwitch,
  }: Props = $props();

  const name = $derived(identity.name ?? `${identity.identityNumber}`);

  // The avatar is the anchor: it keeps to the top-right corner, and the name
  // reads toward it from the left. Closed, it sits in the canvas header; open,
  // in the panel's first row, which runs on the header's own line. So the
  // account is the same in both places, a name and an avatar that never
  // change size, and all it does is slide to the panel's edge while the canvas
  // leaves from under it: nothing in it moves up or down. The places come from
  // the page's measures (+page.svelte), to the avatar's edge and middle; the
  // button's 4px end inset comes off the end distance:
  // - closed: 8px page padding + 1px frame + 24px header inset;
  // - open: 8px page padding + 16px panel padding;
  // - both: the middle of a 64px row that starts 9px down, below the page
  //   padding and the canvas's frame.
  const CLOSED_END = 33 - 4;
  const OPEN_END = 24 - 4;
  const MIDDLE = 9 + 32;

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
  class="fixed z-30 -translate-y-1/2 transition-[inset-inline-end] duration-300 ease-(--ii-ease-out) motion-reduce:transition-none"
  style:top={`${MIDDLE}px`}
  style:inset-inline-end={`${open ? OPEN_END : CLOSED_END}px`}
>
  <button
    bind:this={button}
    bind:offsetWidth={width}
    bind:offsetHeight={height}
    type="button"
    onclick={() => (open ? onSwitch() : onOpen())}
    aria-label={open ? `${name}. Switch identity` : `${name}. Open identity`}
    aria-expanded={open ? switching : false}
    aria-controls={open && switching ? SWITCH_LIST_ID : undefined}
    class="ii-hover focus-visible:ring-focus-ring flex items-center gap-2.5 rounded-full py-1 ps-3 pe-1 outline-none focus-visible:ring-2 active:bg-(--ii-selected)"
  >
    <!-- Read toward the avatar, so the name ends at it. The identity's number
         is a row of the panel (IdentityCard), not a second line here. -->
    <span class="text-text-primary max-w-40 truncate text-sm font-medium">
      {name}
    </span>
    <IdentityAvatar {identity} size="sm" />
  </button>
</div>
