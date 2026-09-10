<script lang="ts">
  import type { BrowserDescription } from "$lib/generated/internet_identity_types";
  import Badge from "$lib/components/ui/Badge.svelte";
  import { t } from "$lib/stores/locale.store";
  import { brandIconOf, brandNameOf } from "../browsers";

  type Action = "sign-out" | "signing-out" | "signed-out" | "none";

  interface Props {
    description: BrowserDescription;
    /** Already formatted: a relative time, or "Now" for the browser reading the page. */
    lastUsed: string;
    /** Already formatted: a short date. */
    firstSeen: string;
    /** Marks the browser this page is being read from, wherever it falls in the list. */
    isCurrent?: boolean;
    action: Action;
    onSignOut?: () => void;
  }

  const {
    description,
    lastUsed,
    firstSeen,
    isCurrent = false,
    action,
    onSignOut,
  }: Props = $props();

  const brandIcon = $derived(brandIconOf(description));
  const dimmed = $derived(action === "signed-out");
</script>

{#snippet actionControl()}
  {#if action === "sign-out"}
    <button class="btn btn-secondary btn-sm" onclick={onSignOut}>
      {$t`Sign out`}
    </button>
  {:else if action === "signing-out"}
    <span class="text-text-tertiary text-sm">{$t`Signing out…`}</span>
  {:else if action === "signed-out"}
    <span class="text-text-tertiary text-sm">{$t`Signed out`}</span>
  {/if}
{/snippet}

{#snippet meta(label: string, value: string)}
  <!-- Dissolved when the row is a line, so the label and value become a column of the
       list's own grid. A two-cell block on its own line when the row is a block. -->
  <div class="contents @2xl/list:flex @2xl/list:flex-col @2xl/list:gap-1">
    <span class="text-text-tertiary text-xs font-semibold">{label}</span>
    <span class="text-text-primary text-xs @2xl/list:whitespace-nowrap"
      >{value}</span
    >
  </div>
{/snippet}

<!--
  One grid at both widths, taking its columns from the list so that every row's meta and
  action line up whatever they contain. What the breakpoint changes is where the pieces
  sit in it: on one line when the row is a line, and identity above meta with the action
  beside both when the row is a block.

  It switches on the card's width rather than the page's: this list sits in a settings
  pane that is narrow at any viewport, so a page breakpoint left the wide layout in a
  column too small for it, truncating the name and printing the meta over the badge.
-->
<div
  class="col-span-4 grid grid-cols-subgrid gap-y-1.5 py-2.5 @2xl/list:items-center @2xl/list:gap-y-0"
>
  <!-- Takes the action's column too when there is no action, which is every row that
       carries the badge: a row shows one of the badge, the button or the label, never a
       combination. Without that the name gives up the action column's width to hold
       nothing, and its badge wraps under it on a narrow card. The column keeps its width
       either way, because the rows that do have an action size it. -->
  <div
    class="{action === 'none'
      ? 'col-span-4'
      : 'col-span-3'} flex min-w-0 flex-row items-center gap-3 @2xl/list:col-span-1"
  >
    <span class="flex size-5 shrink-0 items-center justify-center">
      {#if brandIcon !== undefined}
        <!-- Not dimmed with the rest of a signed-out row: a brand mark is what the
             browser is, not what state it is in, and fading it reads as an image that
             failed to load. The name and the "Signed out" label carry the state. -->
        <img src={brandIcon} alt="" class="size-5" />
      {/if}
    </span>

    <span class="flex min-w-0 flex-wrap items-center gap-2">
      <span
        class="truncate text-sm font-semibold {dimmed
          ? 'text-text-tertiary'
          : 'text-text-primary'}"
      >
        {brandNameOf(description)}
      </span>
      {#if isCurrent}
        <Badge color="success" size="sm" dot class="flex-none"
          >{$t`This browser`}</Badge
        >
      {/if}
    </span>
  </div>

  <!-- A line of its own below the identity when the row is a block; dissolved into the
       row's own line above the breakpoint, where each pair takes a column. -->
  <div
    class="col-span-3 row-start-2 grid grid-cols-[auto_auto] items-baseline justify-start gap-x-1.5 gap-y-2 @2xl/list:contents {dimmed
      ? 'opacity-70'
      : ''}"
  >
    {@render meta($t`Last used`, lastUsed)}
    {@render meta($t`First seen`, firstSeen)}
  </div>

  <!-- On the name's line and centred on it, because the browser is what it acts on.
       `h-9` is the button's own height, held whatever the slot contains: without it a
       row showing "Signed out" would stand shorter than one offering a button, and the
       list would breathe unevenly. Rendered even when empty for the same reason, and so
       the column keeps its width. -->
  {#if action !== "none"}
    <span class="col-start-4 row-start-1 flex h-9 items-center justify-center">
      {@render actionControl()}
    </span>
  {/if}
</div>
