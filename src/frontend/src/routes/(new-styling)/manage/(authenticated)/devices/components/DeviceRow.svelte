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

<!-- Rendered in both layouts, which place it differently: beside the name when the row
     is a block, at the end of the line when it is a line. -->
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

<!--
  Two layouts rather than one that bends, and they switch on the card's width rather than
  the page's: this list sits in a settings pane that is narrow at any viewport, so a page
  breakpoint left the wide layout in a column too small for it, truncating the name and
  printing the meta over the badge.

  Wide, the row is a line: identity, two fixed meta columns, then the action. Narrow,
  those columns have nowhere to go, so the row becomes a block — identity and action on
  the first line, the meta beneath them.
-->
<div
  class="@container/row flex flex-col gap-2 px-4 py-3 @md/row:flex-row @md/row:items-center @md/row:gap-3"
>
  <div class="flex flex-row items-center gap-3">
    <span class="flex size-5 shrink-0 items-center justify-center">
      {#if brandIcon !== undefined}
        <img
          src={brandIcon}
          alt=""
          class="size-5 {dimmed ? 'opacity-50' : ''}"
        />
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

    <!-- Narrow, the action sits up here beside the name rather than taking a line of its
         own below the meta, which is what made the row five lines tall. Wide, it moves
         to the end of the line and this copy is gone. -->
    <span
      class="ms-auto flex shrink-0 items-center @md/row:hidden {action ===
      'none'
        ? 'hidden'
        : ''}"
    >
      {@render actionControl()}
    </span>
  </div>

  <!-- Two columns narrow, so the labels and their values line up down the list whatever
       their length; fixed columns wide, where the row has room for them. Indented to the
       brand name, past the icon. -->
  <div
    class="ms-8 grid grid-cols-2 gap-4 @md/row:ms-0 @md/row:flex @md/row:shrink-0 @md/row:flex-row"
  >
    <span
      class="flex flex-col gap-1 @md/row:w-26 @md/row:shrink-0 @md/row:whitespace-nowrap"
    >
      <span class="text-text-tertiary text-xs font-semibold"
        >{$t`Last used`}</span
      >
      <span class="text-text-primary text-xs">{lastUsed}</span>
    </span>
    <span
      class="flex flex-col gap-1 @md/row:w-18 @md/row:shrink-0 @md/row:whitespace-nowrap"
    >
      <span class="text-text-tertiary text-xs font-semibold"
        >{$t`First seen`}</span
      >
      <span class="text-text-primary text-xs">{firstSeen}</span>
    </span>
  </div>

  <!-- Wide only. Kept as an empty column even with nothing in it, so the meta of a row
       without a button stays aligned with the rows that have one. -->
  <span
    class="hidden @md/row:flex @md/row:w-21 @md/row:shrink-0 @md/row:items-center @md/row:justify-end"
  >
    {@render actionControl()}
  </span>
</div>
