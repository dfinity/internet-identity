<script lang="ts">
  import {
    EllipsisIcon,
    HouseIcon,
    LogOutIcon,
    PaletteIcon,
  } from "@lucide/svelte";
  import { tick, type Snippet } from "svelte";
  import type { ClassValue } from "svelte/elements";
  import Sheet from "../../foundation/frame/Sheet.svelte";

  interface Props {
    /** Change colour was picked; the button is where the colours hang from. */
    onColour: (button: HTMLElement) => void;
    onStartPage: () => void;
    onSignOut: () => void;
    /** What the button shows in place of "⋯", e.g. the identity's avatar,
     *  with the name it goes by and its own look. */
    trigger?: { content: Snippet; label: string; class: ClassValue };
    /** Over the rows, as an account menu leads with its accounts: the
     *  identities to switch to. It gets `close`, so a pick puts the sheet
     *  away. */
    head?: Snippet<[close: () => void]>;
  }

  const { onColour, onStartPage, onSignOut, trigger, head }: Props = $props();

  let button = $state<HTMLButtonElement>();
  let open = $state(false);
  const titleId = $props.id();

  /** Puts the sheet away and hands focus back to the button it came from,
   *  then does what was picked. */
  const close = async (then?: () => void) => {
    open = false;
    await tick();
    button?.focus({ preventScroll: true });
    then?.();
  };

  /** A row, as the desktop identity card's settings: its glyph in the
   *  avatars' column, so the lines start where theirs do. */
  const ROW =
    "ii-hover text-text-primary flex w-full items-center gap-3 px-4 py-3 text-start text-sm font-medium outline-none focus-visible:ring-2 focus-visible:ring-inset focus-visible:ring-focus-ring";
  const GLYPH = "text-fg-tertiary grid size-8 shrink-0 place-items-center";
</script>

<!-- Everything about the identity itself that isn't security: the other
     identities, its colour, the start page and signing out, behind one
     button. On a phone it comes up as a sheet from the bottom (Misha), as
     the sections do, swiped away by its grabber. -->
<button
  bind:this={button}
  type="button"
  onclick={() => (open = true)}
  aria-label={trigger?.label ?? "More for this identity"}
  aria-haspopup="dialog"
  aria-expanded={open}
  class={trigger?.class ?? [
    "text-fg-tertiary hover:text-fg-secondary focus-visible:outline-focus-ring grid size-9 shrink-0 place-items-center rounded-xl transition-colors duration-150 hover:bg-(--ii-hover) focus-visible:outline-2",
    open && "text-fg-secondary bg-(--ii-selected)",
  ]}
>
  {#if trigger !== undefined}
    {@render trigger.content()}
  {:else}
    <EllipsisIcon class="size-4.5" aria-hidden="true" />
  {/if}
</button>
{#if open}
  <Sheet onClose={() => close()} aria-labelledby={titleId}>
    <h2 id={titleId} class="sr-only">Identities and settings</h2>
    <!-- The rows run to the sheet's edges, their padding its own. -->
    <div class="-mx-4 flex flex-col">
      {#if head !== undefined}
        {@render head(() => close())}
      {/if}
      <ul
        aria-label="Settings"
        class="ii-rows flex flex-col [--ii-rows-inset:3.75rem]"
      >
        <li>
          <button
            type="button"
            onclick={() =>
              close(() => {
                if (button !== undefined) onColour(button);
              })}
            class={ROW}
          >
            <span aria-hidden="true" class={GLYPH}>
              <PaletteIcon class="size-4" />
            </span>
            Change colour
          </button>
        </li>
        <li>
          <button type="button" onclick={() => close(onStartPage)} class={ROW}>
            <span aria-hidden="true" class={GLYPH}>
              <HouseIcon class="size-4" />
            </span>
            Make id.ai your start page
          </button>
        </li>
        <li>
          <button type="button" onclick={() => close(onSignOut)} class={ROW}>
            <span aria-hidden="true" class={GLYPH}>
              <LogOutIcon class="size-4 rtl:-scale-x-100" />
            </span>
            Sign out
          </button>
        </li>
      </ul>
    </div>
  </Sheet>
{/if}
