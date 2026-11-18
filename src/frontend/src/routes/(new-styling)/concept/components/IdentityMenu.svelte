<script lang="ts">
  import {
    EllipsisIcon,
    HouseIcon,
    LogOutIcon,
    PaletteIcon,
  } from "@lucide/svelte";
  import Popup from "./Popup.svelte";

  interface Props {
    /** Change colour was picked; the button is where the menu hung from. */
    onColour: (button: HTMLElement) => void;
    onStartPage: () => void;
    onSignOut: () => void;
  }

  const { onColour, onStartPage, onSignOut }: Props = $props();

  let button = $state<HTMLButtonElement>();
  let open = $state(false);
  const close = () => (open = false);

  /** A menu's row: in a menu the lit row is the focus, so no ring. */
  const MENU_ROW =
    "ii-hover text-text-primary flex w-full items-center gap-2.5 rounded-lg px-2.5 py-2 text-start text-sm outline-none";
</script>

<!-- Everything about the identity itself that isn't security: its colour, the
     start page and signing out, out of the way behind one ghost button. -->
<button
  bind:this={button}
  type="button"
  onclick={() => (open = true)}
  aria-label="More for this identity"
  aria-haspopup="menu"
  aria-expanded={open}
  class={[
    "text-fg-tertiary hover:text-fg-secondary focus-visible:outline-focus-ring grid size-9 shrink-0 place-items-center rounded-xl transition-colors duration-150 hover:bg-(--ii-hover) focus-visible:outline-2",
    open && "text-fg-secondary bg-(--ii-selected)",
  ]}
>
  <EllipsisIcon class="size-4.5" aria-hidden="true" />
</button>
{#if open && button !== undefined}
  <!-- Rendered where the button is, so the menu stands two steps above the
       surface it opens over. -->
  <Popup
    anchor={button}
    onClose={close}
    direction="down"
    align="end"
    distance="0.375rem"
    responsive={false}
    class="!w-max"
  >
    <div class="flex flex-col p-1" role="menu" aria-label="Identity">
      <button
        type="button"
        role="menuitem"
        onclick={() => {
          close();
          if (button !== undefined) onColour(button);
        }}
        class={MENU_ROW}
      >
        <PaletteIcon class="text-fg-quaternary size-4" aria-hidden="true" />
        Change colour
      </button>
      <button
        type="button"
        role="menuitem"
        onclick={() => {
          close();
          onStartPage();
        }}
        class={MENU_ROW}
      >
        <HouseIcon class="text-fg-quaternary size-4" aria-hidden="true" />
        Make id.ai your start page
      </button>
      <hr class="border-border-tertiary -mx-1 my-1" />
      <button
        type="button"
        role="menuitem"
        onclick={() => {
          close();
          onSignOut();
        }}
        class={MENU_ROW}
      >
        <LogOutIcon
          class="text-fg-quaternary size-4 rtl:-scale-x-100"
          aria-hidden="true"
        />
        Sign out
      </button>
    </div>
  </Popup>
{/if}
