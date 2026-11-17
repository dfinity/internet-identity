<script lang="ts" module>
  /** The list of identities to switch to, for whatever else opens it. */
  export const SWITCH_LIST_ID = "identity-switch-list";
</script>

<script lang="ts">
  import {
    EllipsisIcon,
    HouseIcon,
    LogOutIcon,
    PaletteIcon,
    PlusIcon,
    UserIcon,
    ChevronDownIcon,
  } from "@lucide/svelte";
  import type { Snippet } from "svelte";
  import { quintOut } from "svelte/easing";
  import { prefersReducedMotion } from "svelte/motion";
  import { slide } from "svelte/transition";
  import IdentityAvatar from "$lib/components/ui/IdentityAvatar.svelte";
  import type { LastUsedIdentity } from "$lib/stores/last-used-identities.store";
  import {
    identityInitial,
    identityPalette,
    type IdentityPalette,
  } from "$lib/utils/identityColors";
  import { getMetadataString } from "$lib/utils/openID";
  import Elevated, { useSurface } from "./Elevated.svelte";
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
    /** The colours the user picked, over the ones the number gives. */
    palette?: IdentityPalette;
    /** The card's menu: the name and colour, the start page, signing out. */
    onCustomize: () => void;
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
    palette,
    onCustomize,
    onStartPage,
    onSignOut,
    alert,
  }: Props = $props();

  let menuAnchor = $state<HTMLElement>();
  const closeMenu = () => (menuAnchor = undefined);

  const nameOf = (one: LastUsedIdentity): string =>
    one.name ?? `${one.identityNumber}`;

  /** What an identity signs in with, as the product's switcher says it. */
  const methodOf = ({ authMethod }: LastUsedIdentity): string => {
    if ("openid" in authMethod && authMethod.openid.metadata !== undefined) {
      return (
        getMetadataString(authMethod.openid.metadata, "email") ?? "Hidden email"
      );
    }
    if ("sso" in authMethod) {
      const { sso } = authMethod;
      return sso.email ?? sso.name ?? sso.domain;
    }
    return "Passkey";
  };

  /** The row's avatars show the first few; the rest are a count. */
  const PREVIEW = 3;
  // The card is one step above what it stands on (Elevated, below), and the
  // avatars in the preview cut themselves out of it.
  const cardSurface = `var(--ii-surface-${Math.min(useSurface() + 1, 8)})`;

  const ROW =
    "ii-hover focus-visible:ring-focus-ring flex w-full items-center text-start outline-none focus-visible:ring-2 focus-visible:ring-inset";
  /** A menu's row: in a menu the lit row is the focus, so no ring. */
  const MENU_ROW =
    "ii-hover text-text-primary flex w-full items-center gap-2.5 rounded-lg px-2.5 py-2 text-start text-sm outline-none";
</script>

<!-- Whose panel this is, as an account switcher heads its menu (Google's
     accounts, a workspace or an organization): the identity itself, large,
     with its name and how it signed in (never its number, which is legacy),
     and under it the way to another one. The other
     identities show as a row of avatars, so the row says who there is at a
     glance; they open in the card itself and push the panel down, so nothing
     floats over the panel. -->
<Elevated
  offset={1}
  as="ul"
  class="ii-rows flex flex-col overflow-hidden rounded-2xl"
>
  <li class="flex items-center gap-3 ps-4 pe-2.5 pt-4 pb-3.5">
    <IdentityAvatar {identity} {palette} size="lg" />
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
    <!-- Everything about the identity itself that isn't security: its name
         and colour, and signing out, out of the way behind one ghost button. -->
    <button
      type="button"
      onclick={(event) => (menuAnchor = event.currentTarget)}
      aria-label="More for this identity"
      aria-haspopup="menu"
      aria-expanded={menuAnchor !== undefined}
      class={[
        "text-fg-tertiary hover:text-fg-secondary focus-visible:outline-focus-ring grid size-9 shrink-0 place-items-center self-start rounded-xl transition-colors duration-150 hover:bg-(--ii-hover) focus-visible:outline-2",
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
              onCustomize();
            }}
            class={MENU_ROW}
          >
            <PaletteIcon class="text-fg-quaternary size-4" aria-hidden="true" />
            Customize name and colour
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
    <li class="px-2 pb-2">
      {@render alert()}
    </li>
  {/if}
  {#if others.length === 0}
    <li>
      <button type="button" onclick={onAdd} class={[ROW, "gap-2 px-4 py-3"]}>
        <span class="text-text-primary min-w-0 flex-1 text-sm font-medium">
          Add identity
        </span>
        <PlusIcon
          class="text-fg-quaternary size-4 shrink-0"
          aria-hidden="true"
        />
      </button>
    </li>
  {:else}
    <li>
      <!-- Closed, the row shows who there is to switch to, as overlapping
           avatars in their own colours; open, the list below says it. -->
      <button
        type="button"
        onclick={() => (switching = !switching)}
        aria-expanded={switching}
        aria-controls={SWITCH_LIST_ID}
        class={[ROW, "gap-2 px-4 py-3"]}
      >
        <span class="text-text-primary min-w-0 flex-1 text-sm font-medium">
          Switch identity
        </span>
        <span
          aria-hidden="true"
          class={[
            "flex shrink-0 items-center transition-opacity duration-150 ease-out motion-reduce:transition-none",
            switching && "opacity-0",
          ]}
          style:--ii-stack-cut={cardSurface}
        >
          {#each others.slice(0, PREVIEW) as other, index (other.identityNumber)}
            {@const palette = identityPalette(other.identityNumber)}
            {@const initial = identityInitial(other.name)}
            <!-- Later avatars tuck under earlier ones, each cut out of the
                 card by a ring, so the first reads as the front. -->
            <span
              class={[
                "relative grid size-6 place-items-center rounded-full text-[11px] leading-none font-semibold ring-2 ring-(color:--ii-stack-cut) outline-1 -outline-offset-1 outline-black/8 dark:outline-white/12",
                index > 0 && "-ms-1.5",
              ]}
              style:background-color={palette.background}
              style:color={palette.foreground}
              style:z-index={PREVIEW - index}
            >
              {#if initial !== undefined}
                {initial}
              {:else}
                <UserIcon class="size-3.5" />
              {/if}
            </span>
          {/each}
          {#if others.length > PREVIEW}
            <span
              class="bg-bg-secondary text-text-tertiary relative -ms-1.5 grid h-6 min-w-6 place-items-center rounded-full px-1 text-[11px] leading-none font-medium tabular-nums ring-2 ring-(color:--ii-stack-cut)"
            >
              +{others.length - PREVIEW}
            </span>
          {/if}
        </span>
        <ChevronDownIcon
          aria-hidden="true"
          class={[
            "text-fg-quaternary size-4 shrink-0 transition-transform duration-200 ease-(--ii-ease-out) motion-reduce:transition-none",
            switching && "rotate-180",
          ]}
        />
      </button>
    </li>
    {#if switching}
      <li
        transition:slide={{
          duration: prefersReducedMotion.current ? 0 : 240,
          easing: quintOut,
        }}
      >
        <!-- An avatar is 32px, so a line here starts 60px in. -->
        <ul
          id={SWITCH_LIST_ID}
          aria-label="Switch identity"
          class="ii-rows flex flex-col [--ii-rows-inset:3.75rem]"
        >
          {#each others as other (other.identityNumber)}
            <li>
              <button
                type="button"
                onclick={() => onSwitch(other.identityNumber)}
                class={[ROW, "gap-3 px-4 py-2.5"]}
              >
                <IdentityAvatar identity={other} size="sm" />
                <span class="flex min-w-0 flex-1 flex-col">
                  <span class="text-text-primary truncate text-sm font-medium">
                    {nameOf(other)}
                  </span>
                  <span
                    class="text-text-tertiary truncate text-[13px] leading-[18px]"
                  >
                    {methodOf(other)}
                  </span>
                </span>
              </button>
            </li>
          {/each}
          <li>
            <button
              type="button"
              onclick={onAdd}
              class={[ROW, "gap-3 px-4 py-2.5"]}
            >
              <span
                aria-hidden="true"
                class="border-border-secondary text-fg-tertiary grid size-8 shrink-0 place-items-center rounded-full border border-dashed"
              >
                <PlusIcon class="size-4" />
              </span>
              <span class="text-text-primary text-sm font-medium">
                Add identity
              </span>
            </button>
          </li>
        </ul>
      </li>
    {/if}
  {/if}
</Elevated>
