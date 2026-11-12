<script lang="ts">
  import { ChevronDownIcon } from "@lucide/svelte";
  import { tick, untrack, type Snippet } from "svelte";
  import { quintOut } from "svelte/easing";
  import { prefersReducedMotion, Tween } from "svelte/motion";
  import IdentityAvatar from "$lib/components/ui/IdentityAvatar.svelte";
  import type { LastUsedIdentity } from "$lib/stores/last-used-identities.store";
  import type { Tone } from "../status";
  import { islandIn, islandOut } from "../motion";
  import { provideSurface } from "./Elevated.svelte";

  interface Props {
    open: boolean;
    identity: LastUsedIdentity;
    /** What waits on the user, for the dot and the line under the name. */
    status?: { sentence: string; tone: Tone };
    /** Only the avatar, for a phone's header. */
    compact: boolean;
    onOpen: () => void;
    /** The panel, as it shows once the account opens. */
    children: Snippet;
  }

  const { open, identity, status, compact, onOpen, children }: Props = $props();

  // Inside, the panel lies on the page surface, as it does beside the canvas.
  provideSurface(1);

  const name = $derived(identity.name ?? `${identity.identityNumber}`);
  const reduced = $derived(prefersReducedMotion.current);

  // The island's morph, moved to the corner: the shell follows its content's
  // measured size, pinned at the top end, so it grows down and toward the
  // middle. Opening takes a little longer than closing, as entrances do.
  let contentWidth = $state(0);
  let contentHeight = $state(0);
  const width = new Tween(0, { easing: quintOut });
  const height = new Tween(0, { easing: quintOut });
  $effect(() => {
    // +2 for the shell's 1px transparent border (kept for forced colors).
    const w = contentWidth + 2;
    const h = contentHeight + 2;
    if (contentWidth === 0) return;
    const first = untrack(() => width.current === 0);
    const duration = first || reduced ? 0 : open ? 400 : 300;
    void width.set(w, { duration });
    void height.set(h, { duration });
  });

  // The header keeps the closed account's room, so nothing beside it moves
  // while the shell grows over the page.
  let pillWidth = $state(0);
  let pillHeight = $state(0);
  let slotWidth = $state(0);
  let slotHeight = $state(0);
  $effect(() => {
    if (pillWidth > 0) slotWidth = pillWidth + 2;
    if (pillHeight > 0) slotHeight = pillHeight + 2;
  });

  // Focus goes into the panel when it opens and back to the account after.
  let trigger = $state<HTMLElement>();
  let sheet = $state<HTMLElement>();
  let wasOpen = untrack(() => open);
  $effect(() => {
    if (open === wasOpen) return;
    wasOpen = open;
    const opening = open;
    void tick().then(() =>
      (opening ? sheet : trigger)?.focus({ preventScroll: true }),
    );
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
</script>

<div
  class="relative shrink-0"
  style:width={slotWidth > 0 ? `${slotWidth}px` : undefined}
  style:height={slotHeight > 0 ? `${slotHeight}px` : undefined}
>
  <div
    class={[
      "ii-elevation-6 absolute end-0 top-0 z-30 overflow-hidden",
      "transition-[border-radius,background-color] duration-300 ease-(--ii-ease-out) motion-reduce:transition-none",
      open ? "ii-surface-1" : "ii-surface-6",
    ]}
    style:width={width.current > 0 ? `${width.current}px` : undefined}
    style:height={height.current > 0 ? `${height.current}px` : undefined}
    style:border-radius={open ? "1.5rem" : `${(pillHeight + 2) / 2}px`}
  >
    {#key open}
      <div
        bind:offsetWidth={contentWidth}
        bind:offsetHeight={contentHeight}
        in:islandIn={{ reduced }}
        out:islandOut={{ reduced }}
        class="absolute end-0 top-0"
      >
        {#if open}
          <div
            bind:this={sheet}
            role="dialog"
            aria-label="Identity"
            tabindex="-1"
            class="max-h-[calc(100dvh-3rem)] w-[25rem] max-w-[calc(100vw-2rem)] overflow-y-auto overscroll-contain p-4 outline-none"
          >
            {@render children()}
          </div>
        {:else}
          <button
            bind:this={trigger}
            bind:offsetWidth={pillWidth}
            bind:offsetHeight={pillHeight}
            type="button"
            onclick={onOpen}
            aria-haspopup="dialog"
            aria-expanded="false"
            aria-label={`${name}${status !== undefined ? `: ${status.sentence}` : ""}. Open identity`}
            class={[
              "ii-hover focus-visible:ring-focus-ring flex items-center gap-2.5 rounded-full p-1.5 text-start whitespace-nowrap outline-none focus-visible:ring-2",
              "transition-[scale] duration-160 ease-out active:scale-[0.97] motion-reduce:active:scale-100",
              !compact && "pe-3",
            ]}
          >
            <span class="relative shrink-0">
              <IdentityAvatar {identity} size="sm" />
              {#if status !== undefined}
                <span
                  aria-hidden="true"
                  class={[
                    "absolute -end-0.5 -top-0.5 size-3 rounded-full ring-2 ring-(color:--ii-surface-6)",
                    DOT[status.tone],
                  ]}
                ></span>
              {/if}
            </span>
            {#if !compact}
              <span class="flex max-w-48 min-w-0 flex-col">
                <span class="text-text-primary truncate text-sm font-semibold">
                  {name}
                </span>
                <span class="text-text-tertiary truncate text-xs">
                  {status?.sentence ?? "All set"}
                </span>
              </span>
              <ChevronDownIcon
                aria-hidden="true"
                class="text-fg-quaternary size-4 shrink-0"
              />
            {/if}
          </button>
        {/if}
      </div>
    {/key}
  </div>
</div>
