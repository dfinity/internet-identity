<script lang="ts" module>
  /** What the card opens: the identities to switch to and its settings. */
  export const SWITCH_LIST_ID = "identity-switch-list";
</script>

<script lang="ts">
  import type { Snippet } from "svelte";
  import { ChevronDownIcon, HouseIcon, LogOutIcon } from "@lucide/svelte";
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

  interface Props {
    identity: LastUsedIdentity;
    /** The device's other identities, to switch to. */
    others: LastUsedIdentity[];
    /** Whether the card is open on the identities and its settings. */
    switching?: boolean;
    onSwitch: (identityNumber: bigint) => void;
    onAdd: () => void;
    /** What this browser signed in with, for the current identity's row in
     *  the open list. The card itself doesn't say it: Access does. */
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

  // The card is one step above what it stands on (Elevated, below), and the
  // avatars across from the name cut themselves out of it.
  const cardSurface = `var(--ii-surface-${Math.min(useSurface() + 1, 8)})`;

  const ROW =
    "ii-hover text-text-primary flex w-full items-center gap-3 px-4 py-2.5 text-start text-sm font-medium outline-none focus-visible:ring-2 focus-visible:ring-inset focus-visible:ring-focus-ring";
</script>

<!-- Whose panel this is: the identity, large, with its name, and across from
     it the other identities as small overlapping avatars, the way X shows
     the accounts you can switch to. The card does one thing (Thomas): a tap
     anywhere on it opens it on every identity and its settings, and closes
     it again. Only the avatar is a control of its own, for the colour: a tap
     opens the row of colours under it, the way Thomas sketched it. Open, the
     card pushes the panel down, so nothing floats over the panel. No line
     parts the name from what opens under it: the list hangs from it. -->
<Elevated as="ul" offset={1} class="flex flex-col overflow-hidden rounded-2xl">
  <li class="relative flex items-center gap-3 py-4 ps-4 pe-3">
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
      class="focus-visible:outline-focus-ring relative shrink-0 rounded-full transition-[scale] duration-160 ease-out focus-visible:outline-2 focus-visible:outline-offset-2 active:scale-[0.96] motion-reduce:active:scale-100"
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
    <span
      class="text-text-primary pointer-events-none relative min-w-0 flex-1 truncate text-base leading-6 font-semibold"
    >
      {nameOf(identity)}
    </span>
    <span class="pointer-events-none relative flex items-center gap-1">
      <IdentityStack {others} {paletteOf} backdrop={cardSurface} />
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
    <li
      id={SWITCH_LIST_ID}
      transition:slide={{
        duration: prefersReducedMotion.current ? 0 : 240,
        easing: quintOut,
      }}
    >
      <IdentityList
        id={`${SWITCH_LIST_ID}-identities`}
        {identity}
        {signedInWith}
        {others}
        {onSwitch}
        {onAdd}
        {paletteOf}
      />
      <!-- The identity's settings under its list, as the phone's menu has
           them; the colour is the avatar's own. -->
      <ul
        aria-label="Settings"
        class="ii-rows border-border-tertiary flex flex-col border-t py-1 [--ii-rows-inset:3.75rem]"
      >
        <li>
          <button type="button" onclick={onStartPage} class={ROW}>
            <span
              aria-hidden="true"
              class="text-fg-tertiary grid size-8 shrink-0 place-items-center"
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
              class="text-fg-tertiary grid size-8 shrink-0 place-items-center"
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
