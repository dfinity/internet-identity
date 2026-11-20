<script lang="ts">
  import { PlusIcon, UserIcon } from "@lucide/svelte";
  import { still, type Travel } from "../../foundation/motion/motion";
  import type { LastUsedIdentity } from "$lib/stores/last-used-identities.store";
  import {
    identityInitial,
    identityPalette,
    type IdentityPalette,
  } from "$lib/utils/identityColors";

  interface Props {
    /** The device's other identities, to switch to. */
    others: LastUsedIdentity[];
    /** A crossfade pair (svelte/transition) the list's rows share: an avatar
     *  leaves the stack for its row as the list opens, and comes back. */
    travel?: Travel;
    /** Whether the list it opens is showing. */
    expanded?: boolean;
    /** The list's id, for `aria-controls`. */
    controls?: string;
    /** Without it the stack is only a picture: the card around it is the
     *  button that opens the list. */
    onToggle?: () => void;
    /** The colours picked for an identity, over the ones its number gives. */
    paletteOf?: (one: LastUsedIdentity) => IdentityPalette | undefined;
    /** What the avatars cut themselves out of: the surface the stack stands on. */
    backdrop: string;
  }

  const {
    others,
    travel,
    expanded = false,
    controls,
    onToggle,
    paletteOf,
    backdrop,
  }: Props = $props();

  // svelte-ignore state_referenced_locally
  const [travelOut, travelIn] = travel ?? [still, still];

  /** The first few show; the rest are a count. */
  const PREVIEW = 3;
</script>

<!-- The other identities, small and overlapping, across from the current one,
     as X shows the accounts you can switch to beside your own. A tap opens
     the list of all of them. With none yet, the same spot adds one. Only a
     picture where the whole card around it opens the list. -->
{#if onToggle !== undefined}
  <button
    type="button"
    onclick={onToggle}
    aria-expanded={expanded}
    aria-controls={controls}
    aria-label={others.length === 0
      ? "Identities"
      : `Switch identity, ${others.length} more`}
    title={others.length === 0 ? "Identities" : "Switch identity"}
    class={[
      "focus-visible:outline-focus-ring flex shrink-0 items-center rounded-full p-1 transition-colors duration-150 hover:bg-(--ii-hover) focus-visible:outline-2",
      expanded && "bg-(--ii-selected)",
    ]}
    style:--ii-stack-cut={backdrop}
  >
    {@render avatars()}
  </button>
{:else}
  <span
    aria-hidden="true"
    class="flex shrink-0 items-center p-1"
    style:--ii-stack-cut={backdrop}
  >
    {@render avatars()}
  </span>
{/if}

{#snippet avatars()}
  {#if others.length === 0}
    <span
      aria-hidden="true"
      class="border-border-secondary text-fg-tertiary grid size-6 place-items-center rounded-full border border-dashed"
    >
      <PlusIcon class="size-3" />
    </span>
  {:else}
    {#each others.slice(0, PREVIEW) as other, index (other.identityNumber)}
      {@const palette =
        paletteOf?.(other) ?? identityPalette(other.identityNumber)}
      {@const initial = identityInitial(other.name)}
      <!-- Later avatars tuck under earlier ones, each cut out of the card by
           a ring, so the first reads as the front. -->
      <span
        aria-hidden="true"
        class={[
          "relative grid size-6 place-items-center rounded-full text-[0.6875rem] leading-none font-semibold ring-2 ring-(color:--ii-stack-cut) outline-1 -outline-offset-1 outline-black/8 dark:outline-white/12",
          index > 0 && "-ms-1.5",
        ]}
        style:background-color={palette.background}
        style:color={palette.foreground}
        style:z-index={PREVIEW - index}
        in:travelIn|global={{ key: other.identityNumber }}
        out:travelOut|global={{ key: other.identityNumber }}
      >
        {#if initial !== undefined}
          {initial}
        {:else}
          <UserIcon class="size-3" />
        {/if}
      </span>
    {/each}
    {#if others.length > PREVIEW}
      <!-- The hover overlay laid over the card's own colour, opaque so the
           avatar it overlaps doesn't show through: a tint, never a fill
           darker than the card. -->
      <span
        aria-hidden="true"
        class="text-text-tertiary relative -ms-1.5 grid h-6 min-w-6 place-items-center rounded-full bg-(--ii-stack-cut) bg-[linear-gradient(var(--ii-hover),var(--ii-hover))] px-1 text-[0.6875rem] leading-none font-medium tabular-nums ring-2 ring-(color:--ii-stack-cut)"
      >
        +{others.length - PREVIEW}
      </span>
    {/if}
  {/if}
{/snippet}
