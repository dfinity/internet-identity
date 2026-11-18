<script lang="ts">
  import { PlusIcon, UserIcon } from "@lucide/svelte";
  import type { LastUsedIdentity } from "$lib/stores/last-used-identities.store";
  import {
    identityInitial,
    identityPalette,
    type IdentityPalette,
  } from "$lib/utils/identityColors";

  interface Props {
    /** The device's other identities, to switch to. */
    others: LastUsedIdentity[];
    /** Whether the list it opens is showing. */
    expanded: boolean;
    /** The list's id, for `aria-controls`. */
    controls: string;
    onToggle: () => void;
    /** The colours picked for an identity, over the ones its number gives. */
    paletteOf?: (one: LastUsedIdentity) => IdentityPalette | undefined;
    /** What the avatars cut themselves out of: the surface the stack stands on. */
    backdrop: string;
  }

  const {
    others,
    expanded,
    controls,
    onToggle,
    paletteOf,
    backdrop,
  }: Props = $props();

  /** The first few show; the rest are a count. */
  const PREVIEW = 3;
</script>

<!-- The other identities, small and overlapping, across from the current one,
     as X shows the accounts you can switch to beside your own. A tap opens
     the list of all of them. With none yet, the same spot adds one. -->
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
  {#if others.length === 0}
    <span
      aria-hidden="true"
      class="border-border-secondary text-fg-tertiary grid size-7 place-items-center rounded-full border border-dashed"
    >
      <PlusIcon class="size-3.5" />
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
          "relative grid size-7 place-items-center rounded-full text-xs leading-none font-semibold ring-2 ring-(color:--ii-stack-cut) outline-1 -outline-offset-1 outline-black/8 dark:outline-white/12",
          index > 0 && "-ms-2",
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
        aria-hidden="true"
        class="bg-bg-secondary text-text-tertiary relative -ms-2 grid h-7 min-w-7 place-items-center rounded-full px-1 text-xs leading-none font-medium tabular-nums ring-2 ring-(color:--ii-stack-cut)"
      >
        +{others.length - PREVIEW}
      </span>
    {/if}
  {/if}
</button>
