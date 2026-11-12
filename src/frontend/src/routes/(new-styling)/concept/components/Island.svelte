<script lang="ts" module>
  export type IslandView = "calm" | "lead" | "select";
</script>

<script lang="ts">
  import { ChevronUpIcon } from "@lucide/svelte";
  import { untrack } from "svelte";
  import { quintOut } from "svelte/easing";
  import { prefersReducedMotion, Tween } from "svelte/motion";
  import IdentityAvatar from "$lib/components/ui/IdentityAvatar.svelte";
  import type { LastUsedIdentity } from "$lib/stores/last-used-identities.store";
  import type { LeadAction, Tone } from "../status";
  import { islandIn, islandOut, shellIn, shellOut } from "../motion";

  interface Props {
    view: IslandView;
    identity: LastUsedIdentity;
    /** Calm view: the one thing worth a glance, or nothing. */
    note?: { sentence: string; tone: Tone };
    /** Lead view: the loud card, told the same way the panel tells it. */
    lead?: { title: string; sentence: string; tone: Tone; action: LeadAction };
    selectedCount: number;
    onOpen: () => void;
    onAction: () => void;
    onDetails: () => void;
    onDone: () => void;
  }

  const {
    view,
    identity,
    note,
    lead,
    selectedCount,
    onOpen,
    onAction,
    onDetails,
    onDone,
  }: Props = $props();

  const name = $derived(identity.name ?? `${identity.identityNumber}`);
  const reduced = $derived(prefersReducedMotion.current);

  // The shell follows its content's measured size. A tween rather than a spring:
  // the change comes from a click or a status, not a throw, so it lands without
  // overshoot, and a new target picks up from wherever the shell is.
  let contentWidth = $state(0);
  let contentHeight = $state(0);
  const width = new Tween(0, { duration: 320, easing: quintOut });
  const height = new Tween(0, { duration: 320, easing: quintOut });
  $effect(() => {
    // +2 for the shell's 1px transparent border (kept for forced colors).
    const w = contentWidth + 2;
    const h = contentHeight + 2;
    if (contentWidth === 0) return;
    const first = untrack(() => width.current === 0);
    const duration = first || reduced ? 0 : 320;
    void width.set(w, { duration });
    void height.set(h, { duration });
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
  const PRESS =
    "transition-[scale] duration-160 ease-out active:scale-[0.97] motion-reduce:active:scale-100";
</script>

<div
  in:shellIn={{ reduced }}
  out:shellOut={{ reduced }}
  class="pointer-events-none fixed inset-x-0 bottom-[calc(1rem+env(safe-area-inset-bottom))] z-20 flex justify-center px-3"
>
  <div
    role="region"
    aria-label="Identity"
    aria-live="polite"
    class="ii-surface-6 ii-elevation-6 pointer-events-auto relative overflow-hidden rounded-[1.75rem]"
    style:width={width.current > 0 ? `${width.current}px` : undefined}
    style:height={height.current > 0 ? `${height.current}px` : undefined}
  >
    {#key view}
      <div
        bind:offsetWidth={contentWidth}
        bind:offsetHeight={contentHeight}
        in:islandIn={{ reduced }}
        out:islandOut={{ reduced }}
        class="absolute start-0 top-0 flex w-max max-w-[calc(100vw-1.5rem-2px)] items-center gap-1 p-1.5"
      >
        {#if view === "select"}
          <span
            class="text-text-primary ps-3.5 pe-2 text-sm font-semibold whitespace-nowrap tabular-nums"
          >
            {selectedCount === 0
              ? "Select apps"
              : selectedCount === 1
                ? "1 app selected"
                : `${selectedCount} apps selected`}
          </span>
          <button
            type="button"
            onclick={onDetails}
            disabled={selectedCount === 0}
            class={["btn btn-secondary btn-sm rounded-full", PRESS]}
          >
            Details
          </button>
          <button
            type="button"
            onclick={onDone}
            class={["btn btn-primary btn-sm rounded-full", PRESS]}
          >
            Done
          </button>
        {:else}
          <button
            type="button"
            onclick={onOpen}
            aria-haspopup="dialog"
            aria-label={view === "lead" && lead !== undefined
              ? `${name}: ${lead.title}, ${lead.sentence}. Open identity`
              : `${name}${note !== undefined ? `: ${note.sentence}` : ""}. Open identity`}
            class={[
              "ii-hover focus-visible:ring-focus-ring flex min-w-0 items-center gap-2.5 rounded-[1.3125rem] py-1 ps-1 pe-3 text-start outline-none focus-visible:ring-2",
              PRESS,
            ]}
          >
            <span class="relative shrink-0">
              <IdentityAvatar {identity} size="sm" />
              {#if view === "lead" && lead !== undefined}
                <span
                  aria-hidden="true"
                  class={[
                    "absolute -end-0.5 -top-0.5 size-3 rounded-full ring-2 ring-(color:--ii-surface-6)",
                    DOT[lead.tone],
                  ]}
                ></span>
              {/if}
            </span>
            {#if view === "lead" && lead !== undefined}
              <span class="flex max-w-60 min-w-0 flex-col">
                <span class="text-text-tertiary text-xs">{lead.title}</span>
                <span
                  class={[
                    "text-sm font-semibold text-pretty",
                    lead.tone === "risk"
                      ? "text-text-error-primary"
                      : "text-warning-700 dark:text-text-warning-primary",
                  ]}
                >
                  {lead.sentence}
                </span>
              </span>
            {:else}
              <span class="flex max-w-60 min-w-0 flex-col">
                <span class="text-text-primary truncate text-sm font-semibold">
                  {name}
                </span>
                <span
                  class="text-text-tertiary flex min-w-0 items-center gap-1.5 text-xs"
                >
                  {#if note !== undefined}
                    <span
                      aria-hidden="true"
                      class={["size-1.5 shrink-0 rounded-full", DOT[note.tone]]}
                    ></span>
                    <span class="truncate">{note.sentence}</span>
                  {:else}
                    All set
                  {/if}
                </span>
              </span>
              <ChevronUpIcon
                aria-hidden="true"
                class="text-fg-quaternary size-4 shrink-0"
              />
            {/if}
          </button>
          {#if view === "lead" && lead !== undefined}
            <button
              type="button"
              onclick={onAction}
              class={["btn btn-primary btn-sm shrink-0 rounded-full", PRESS]}
            >
              {lead.action.label}
            </button>
          {/if}
        {/if}
      </div>
    {/key}
  </div>
</div>
