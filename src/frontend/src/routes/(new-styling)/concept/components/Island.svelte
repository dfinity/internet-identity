<script lang="ts" module>
  export type IslandView = "calm" | "lead" | "select";
</script>

<script lang="ts">
  import {
    BellIcon,
    BellOffIcon,
    ChevronUpIcon,
    Trash2Icon,
    XIcon,
  } from "@lucide/svelte";
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
    /** Select view: whether any picked app may notify, so the one button
     *  mutes them all, or, where none may, unmutes them. */
    anyNotifying: boolean;
    onOpen: () => void;
    onAction: () => void;
    onDetails: () => void;
    onSetNotifications: (allowed: boolean) => void;
    onRemove: () => void;
    onDone: () => void;
  }

  const {
    view,
    identity,
    note,
    lead,
    selectedCount,
    anyNotifying,
    onOpen,
    onAction,
    onDetails,
    onSetNotifications,
    onRemove,
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
    warning: "bg-fg-warning-primary",
    info: "bg-fg-primary",
    neutral: "bg-fg-quaternary",
  };
  const PRESS =
    "transition-[scale] duration-160 ease-out active:scale-[0.97] motion-reduce:active:scale-100";
  // A phone keeps the actions' glyphs and lets their words go to a screen
  // reader, as the toolbar under a photo selection does.
  const ACTION = "btn btn-tertiary btn-sm rounded-full max-sm:w-9 max-sm:px-0";
  const ACTION_LABEL = "max-sm:sr-only";
</script>

<div
  in:shellIn={{ reduced }}
  out:shellOut={{ reduced }}
  class="pointer-events-none fixed inset-x-0 bottom-[calc(1rem+env(safe-area-inset-bottom))] z-20 flex justify-center px-3"
>
  <div
    role="region"
    aria-label={view === "select" ? "Selected apps" : "Identity"}
    aria-live="polite"
    class="ii-elevation-6 pointer-events-auto relative overflow-hidden rounded-[1.75rem] bg-(--ii-floating)"
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
          <!-- A selection bar: the count, what can be done to the picked apps
               right here, and a close. The count opens them in the panel, as
               the island's other views open it from their left end. -->
          {#if selectedCount === 0}
            <span
              class="text-text-primary ps-3.5 pe-2 text-sm font-semibold whitespace-nowrap"
            >
              Select apps
            </span>
          {:else}
            <button
              type="button"
              onclick={onDetails}
              aria-haspopup="dialog"
              aria-label={`${selectedCount} selected. Show details`}
              class={[
                "ii-hover text-text-primary focus-visible:ring-focus-ring flex h-9 items-center gap-1 rounded-full ps-3.5 pe-2.5 text-sm font-semibold whitespace-nowrap tabular-nums outline-none focus-visible:ring-2",
                PRESS,
              ]}
            >
              {selectedCount} selected
              <ChevronUpIcon
                aria-hidden="true"
                class="text-fg-quaternary size-4 shrink-0"
              />
            </button>
            <button
              type="button"
              onclick={() => onSetNotifications(!anyNotifying)}
              class={[ACTION, PRESS]}
            >
              {#if anyNotifying}
                <BellOffIcon aria-hidden="true" class="size-4" />
                <span class={ACTION_LABEL}>Mute</span>
              {:else}
                <BellIcon aria-hidden="true" class="size-4" />
                <span class={ACTION_LABEL}>Unmute</span>
              {/if}
            </button>
            <button
              type="button"
              onclick={onRemove}
              class={[ACTION, "btn-danger", PRESS]}
            >
              <Trash2Icon aria-hidden="true" class="size-4" />
              <span class={ACTION_LABEL}>Remove</span>
            </button>
          {/if}
          <button
            type="button"
            onclick={onDone}
            aria-label="Done"
            class={[
              "btn btn-tertiary btn-sm btn-icon text-fg-tertiary rounded-full",
              PRESS,
            ]}
          >
            <XIcon aria-hidden="true" class="size-4" />
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
                    "absolute -end-0.5 -top-0.5 size-3 rounded-full ring-2 ring-(color:--ii-floating)",
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
