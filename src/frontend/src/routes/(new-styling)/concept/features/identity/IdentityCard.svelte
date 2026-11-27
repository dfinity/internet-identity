<script lang="ts" module>
  /** What the card opens: the identities to switch to and its settings. */
  export const SWITCH_LIST_ID = "identity-switch-list";
</script>

<script lang="ts">
  import type { Snippet } from "svelte";
  import { ChevronDownIcon, HouseIcon, LogOutIcon } from "@lucide/svelte";
  import { prefersReducedMotion } from "svelte/motion";
  import IdentityAvatar from "$lib/components/ui/IdentityAvatar.svelte";
  import type { LastUsedIdentity } from "$lib/stores/last-used-identities.store";
  import {
    identityInitial,
    type IdentityPalette,
  } from "$lib/utils/identityColors";
  import {
    grow,
    revealIn,
    revealOut,
    travel,
  } from "../../foundation/motion/motion";
  import ColourStrip from "./ColourStrip.svelte";
  import Elevated, {
    surfaceColor,
    useSurface,
  } from "../../foundation/surfaces/Elevated.svelte";
  import IdentityList from "./IdentityList.svelte";
  import IdentityStack from "./IdentityStack.svelte";

  interface Props {
    identity: LastUsedIdentity;
    /** The device's other identities, to switch to. */
    others: LastUsedIdentity[];
    /** Whether the card is open on the identities and its settings. */
    switching?: boolean;
    onSwitch: (identityNumber: bigint) => void;
    onAdd: () => void;
    /** What this browser signed in with, under the name. Access counts the
     *  ways in ("3 ways to sign in"); this names the one in use (Thomas). */
    signedInWith: string;
    /** The colours the user picked for an identity, over the ones its
     *  number gives. */
    paletteOf?: (one: LastUsedIdentity) => IdentityPalette | undefined;
    /** Where the identity's colours stand in IDENTITY_PALETTES, and a new
     *  pick. Only the colour can change: passkeys have no way to rename. */
    colour: number;
    onColour: (paletteIndex: number) => void;
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

  /** The avatar, which the colour row hangs from while it's open. */
  let avatarButton = $state<HTMLButtonElement>();
  let colourAnchor = $state<HTMLElement>();

  const nameOf = (one: LastUsedIdentity): string =>
    one.name ?? `${one.identityNumber}`;

  /** Each other identity's avatar travels from the stack to its row in the
   *  list as the card opens, and back as it closes (Thomas). */
  const avatars = travel();

  // The card is one step above what it stands on (Elevated, below), and the
  // avatars across from the name cut themselves out of it.
  const cardSurface = surfaceColor(useSurface() + 1);

  const ROW =
    "ii-hover text-text-primary flex w-full items-center gap-3 px-4 py-2.5 text-start text-sm font-medium outline-none focus-visible:ring-2 focus-visible:ring-inset focus-visible:ring-focus-ring";
</script>

<!-- Whose panel this is: the identity with its name and what this browser
     signed in with under it, and across from it the other identities as
     smaller overlapping avatars, the way X shows the accounts you can switch
     to. One compact card, the same over every view
     of the panel (the overview, a section, an app's details, managing apps),
     so the head of the panel never changes as what's under it does. The
     card does one thing (Thomas): a tap anywhere on it opens it on the other
     identities and its settings, and closes it again. Only the avatar is a
     control of its own, for the colour: a tap opens the row of colours under
     it, the way Thomas sketched it. Open, the card pushes the panel down, so
     nothing floats over the panel. No line parts the name from what opens
     under it: the list hangs from it. -->
<Elevated as="ul" offset={1} class="flex flex-col overflow-hidden rounded-2xl">
  <!-- On the cards' own lines (Thomas): the avatar stands on their 16px
       edge at their icons' 36px, so the name starts where their titles and
       their rows' names do. -->
  <li class="relative flex items-center gap-3 px-4 py-2.5">
    <button
      type="button"
      onclick={() => (switching = !switching)}
      aria-expanded={switching}
      aria-controls={SWITCH_LIST_ID}
      aria-label={`${nameOf(identity)}: identities and settings`}
      class="ii-hover focus-visible:ring-focus-ring absolute inset-0 outline-none focus-visible:ring-2 focus-visible:ring-inset"
    ></button>
    <button
      bind:this={avatarButton}
      type="button"
      onclick={() => (colourAnchor = avatarButton)}
      aria-label="Change colour"
      aria-haspopup="true"
      aria-expanded={colourAnchor !== undefined}
      title="Change colour"
      class="focus-visible:outline-focus-ring relative shrink-0 rounded-full transition-[scale] duration-160 ease-out focus-visible:outline-2 focus-visible:outline-offset-2 active:scale-[0.96] motion-reduce:active:scale-100 [&>div>div]:size-9"
    >
      <IdentityAvatar {identity} palette={paletteOf?.(identity)} size="sm" />
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
    <span class="pointer-events-none relative flex min-w-0 flex-1 flex-col">
      <span class="text-text-primary truncate text-sm font-semibold">
        {nameOf(identity)}
      </span>
      <span class="text-text-tertiary truncate text-[13px] leading-[18px]">
        {signedInWith}
      </span>
    </span>
    <span class="pointer-events-none relative flex items-center gap-1">
      <!-- Open, the list below shows the others, so the stack leaves for
           it: each identity is on screen once. -->
      {#if !switching}
        <IdentityStack
          {others}
          {paletteOf}
          backdrop={cardSurface}
          travel={avatars}
        />
      {/if}
      <ChevronDownIcon
        aria-hidden="true"
        class={[
          "text-fg-quaternary size-4 shrink-0 transition-transform duration-200 ease-out motion-reduce:transition-none",
          switching && "rotate-180",
        ]}
      />
    </span>
  </li>
  {#if alert !== undefined}
    <!-- Its own padding comes with what it says, so with nothing to say it
         takes no room. -->
    <li>
      {@render alert()}
    </li>
  {/if}
  {#if switching}
    <!-- The head above stays the current identity and is the list's first
         row, so the list holds only the others (Thomas: each identity once).
         It opens without clipping, so the avatars can come down into it;
         the card clips instead. As it grows, its rows fade in one by one
         behind its edge, the way a panel card's section fades in as the card
         grows, while the avatars land whole (`revealIn`). -->
    <li
      id={SWITCH_LIST_ID}
      transition:grow={{ reduced: prefersReducedMotion.current }}
    >
      <IdentityList
        id={`${SWITCH_LIST_ID}-identities`}
        {identity}
        {signedInWith}
        {others}
        {onSwitch}
        {onAdd}
        {paletteOf}
        withCurrent={false}
        size="md"
        travel={avatars}
        reveal
      />
      <!-- The identity's settings under its list, as the phone's menu has
           them; the colour is the avatar's own. They come in after its
           rows, as one group, their line with them. -->
      <ul
        aria-label="Settings"
        class="ii-rows ii-reveal border-border-tertiary flex flex-col border-t py-1 [--ii-rows-inset:4rem]"
        in:revealIn={{
          index: others.length + 1,
          reduced: prefersReducedMotion.current,
        }}
        out:revealOut={{ reduced: prefersReducedMotion.current }}
      >
        <li>
          <button type="button" onclick={onStartPage} class={ROW}>
            <span
              aria-hidden="true"
              class="text-fg-tertiary grid size-9 shrink-0 place-items-center"
            >
              <HouseIcon class="size-4" />
            </span>
            Make id.ai your start page
          </button>
        </li>
        <li>
          <button type="button" onclick={onSignOut} class={ROW}>
            <span
              aria-hidden="true"
              class="text-fg-tertiary grid size-9 shrink-0 place-items-center"
            >
              <LogOutIcon class="size-4 rtl:-scale-x-100" />
            </span>
            Sign out
          </button>
        </li>
      </ul>
    </li>
  {/if}
</Elevated>
