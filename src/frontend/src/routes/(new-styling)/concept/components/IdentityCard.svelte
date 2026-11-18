<script lang="ts" module>
  /** The list of identities to switch to, for whatever else opens it. */
  export const SWITCH_LIST_ID = "identity-switch-list";
</script>

<script lang="ts">
  import { EllipsisIcon, HouseIcon, LogOutIcon, PaletteIcon } from "@lucide/svelte";
  import type { Snippet } from "svelte";
  import { quintOut } from "svelte/easing";
  import { prefersReducedMotion } from "svelte/motion";
  import { slide } from "svelte/transition";
  import IdentityAvatar from "$lib/components/ui/IdentityAvatar.svelte";
  import type { LastUsedIdentity } from "$lib/stores/last-used-identities.store";
  import {
    identityInitial,
    type IdentityPalette,
  } from "$lib/utils/identityColors";
  import ColourStrip from "./ColourStrip.svelte";
  import Elevated, { useSurface } from "./Elevated.svelte";
  import IdentityList from "./IdentityList.svelte";
  import IdentityStack from "./IdentityStack.svelte";
  import Popup from "./Popup.svelte";

  interface Props {
    identity: LastUsedIdentity;
    /** The device's other identities, to switch to. */
    others: LastUsedIdentity[];
    /** Whether the other identities are listed. */
    switching?: boolean;
    onSwitch: (identityNumber: bigint) => void;
    onAdd: () => void;
    /** What this browser signed in with, said under the name. */
    signedInWith: string;
    /** The colours the user picked for an identity, over the ones its
     *  number gives. */
    paletteOf?: (one: LastUsedIdentity) => IdentityPalette | undefined;
    /** Where the identity's colours stand in IDENTITY_PALETTES, and a new
     *  pick. Only the colour can change: passkeys have no way to rename. */
    colour: number;
    onColour: (paletteIndex: number) => void;
    /** The card's menu: the start page and signing out, with the colour. */
    onStartPage: () => void;
    onSignOut: () => void;
    /** Something wrong with this identity, told inside its own card, under
     *  its name, so it's never unclear whose it is. */
    alert?: Snippet;
  }

  let {
    identity,
    others,
    switching = $bindable(false),
    onSwitch,
    onAdd,
    signedInWith,
    paletteOf,
    colour,
    onColour,
    onStartPage,
    onSignOut,
    alert,
  }: Props = $props();

  let menuAnchor = $state<HTMLElement>();
  const closeMenu = () => (menuAnchor = undefined);
  /** The avatar, which the colour row hangs from while it's open. */
  let avatarButton = $state<HTMLButtonElement>();
  let colourAnchor = $state<HTMLElement>();

  const nameOf = (one: LastUsedIdentity): string =>
    one.name ?? `${one.identityNumber}`;

  // The card is one step above what it stands on (Elevated, below), and the
  // avatars across from the name cut themselves out of it.
  const cardSurface = `var(--ii-surface-${Math.min(useSurface() + 1, 8)})`;

  /** A menu's row: in a menu the lit row is the focus, so no ring. */
  const MENU_ROW =
    "ii-hover text-text-primary flex w-full items-center gap-2.5 rounded-lg px-2.5 py-2 text-start text-sm outline-none";
</script>

<!-- Whose panel this is, as an account switcher heads its menu: the identity
     itself, large, with its name and how it signed in (never its number,
     which is legacy). Across from it, the other identities as small
     overlapping avatars, the way X shows the accounts you can switch to; a
     tap lists them all in the card itself, pushing the panel down, so
     nothing floats over the panel. -->
<Elevated
  offset={1}
  as="ul"
  class="ii-rows flex flex-col overflow-hidden rounded-2xl"
>
  <li class="flex items-center gap-3 ps-4 pe-2.5 py-4">
    <!-- The avatar is the colour's own control: a tap opens the row of
         colours under it, the way Thomas sketched it. -->
    <button
      bind:this={avatarButton}
      type="button"
      onclick={() => (colourAnchor = avatarButton)}
      aria-label="Change colour"
      aria-haspopup="true"
      aria-expanded={colourAnchor !== undefined}
      title="Change colour"
      class="focus-visible:outline-focus-ring shrink-0 rounded-full transition-[scale] duration-160 ease-out focus-visible:outline-2 focus-visible:outline-offset-2 active:scale-[0.96] motion-reduce:active:scale-100"
    >
      <IdentityAvatar {identity} palette={paletteOf?.(identity)} size="lg" />
    </button>
    {#if colourAnchor !== undefined}
      <ColourStrip
        anchor={colourAnchor}
        initial={identityInitial(identity.name)}
        current={colour}
        onPick={onColour}
        onClose={() => (colourAnchor = undefined)}
      />
    {/if}
    <div class="flex min-w-0 flex-1 flex-col items-start">
      <span
        class="text-text-primary w-full truncate text-base leading-6 font-semibold"
      >
        {nameOf(identity)}
      </span>
      <span class="text-text-tertiary w-full truncate text-sm leading-6">
        {signedInWith}
      </span>
    </div>
    <IdentityStack
      {others}
      expanded={switching}
      controls={SWITCH_LIST_ID}
      onToggle={() => (switching = !switching)}
      {paletteOf}
      backdrop={cardSurface}
    />
    <!-- Everything about the identity itself that isn't security: its colour,
         the start page and signing out, out of the way behind one ghost
         button. -->
    <button
      type="button"
      onclick={(event) => (menuAnchor = event.currentTarget)}
      aria-label="More for this identity"
      aria-haspopup="menu"
      aria-expanded={menuAnchor !== undefined}
      class={[
        "text-fg-tertiary hover:text-fg-secondary focus-visible:outline-focus-ring -ms-1 grid size-9 shrink-0 place-items-center rounded-xl transition-colors duration-150 hover:bg-(--ii-hover) focus-visible:outline-2",
        menuAnchor !== undefined && "text-fg-secondary bg-(--ii-selected)",
      ]}
    >
      <EllipsisIcon class="size-4.5" aria-hidden="true" />
    </button>
    {#if menuAnchor !== undefined}
      <!-- Inside the card, so the menu stands two steps above the card it
           opens over, not above the panel. -->
      <Popup
        anchor={menuAnchor}
        onClose={closeMenu}
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
              closeMenu();
              colourAnchor = avatarButton;
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
              closeMenu();
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
              closeMenu();
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
  </li>
  {#if alert !== undefined}
    <!-- Its own padding comes with what it says, so with nothing to say it
         takes no room and draws no line. -->
    <li class="before:!hidden">
      {@render alert()}
    </li>
  {/if}
  {#if switching}
    <li
      transition:slide={{
        duration: prefersReducedMotion.current ? 0 : 240,
        easing: quintOut,
      }}
    >
      <IdentityList
        id={SWITCH_LIST_ID}
        {identity}
        {signedInWith}
        {others}
        {onSwitch}
        {onAdd}
        {paletteOf}
      />
    </li>
  {/if}
</Elevated>
