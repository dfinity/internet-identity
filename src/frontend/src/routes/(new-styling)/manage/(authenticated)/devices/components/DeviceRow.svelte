<script lang="ts">
  import type { BrowserDescription } from "$lib/generated/internet_identity_types";
  import Badge from "$lib/components/ui/Badge.svelte";
  import { t } from "$lib/stores/locale.store";
  import { brandIconOf, brandNameOf } from "../browsers";

  type Action = "sign-out" | "signing-out" | "signed-out" | "none";

  interface Props {
    description: BrowserDescription;
    /** Already formatted: a relative time, or "Right now" for the browser reading the
     *  page. */
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
  <!-- Dimmed on the text rather than the wrapper: one wrapper or the other is
       `display: contents` at any width, and a box-less element paints no opacity. -->
  <div
    class="contents @min-[620px]/list:flex @min-[620px]/list:flex-col @min-[620px]/list:gap-1"
  >
    <span
      class="text-text-tertiary text-xs font-semibold {dimmed
        ? 'opacity-70'
        : ''}">{label}</span
    >
    <span
      class="text-text-primary text-xs @min-[620px]/list:whitespace-nowrap {dimmed
        ? 'opacity-70'
        : ''}">{value}</span
    >
  </div>
{/snippet}

<!-- Switches on the card's width, not the page's: this list sits in a settings pane
     that is narrow at any viewport. -->
<div
  class="col-span-4 grid grid-cols-subgrid gap-y-1.5 py-2.5 @min-[620px]/list:items-center @min-[620px]/list:gap-y-0 @min-[620px]/list:py-4"
>
  <!-- Spans the action's column when there is no action, so the badge does not wrap
       under the name to leave an empty column beside it. -->
  <div
    class="{action === 'none'
      ? 'col-span-4'
      : 'col-span-3'} flex min-h-9 min-w-0 flex-row items-center gap-3 @min-[620px]/list:col-span-1 @min-[620px]/list:ps-6"
  >
    <span class="flex size-5 shrink-0 items-center justify-center">
      {#if brandIcon !== undefined}
        <!-- Not dimmed with the rest of a signed-out row: a faded brand mark reads as
             an image that failed to load. -->
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

  <div
    class="col-span-4 row-start-2 grid grid-cols-[auto_auto] items-baseline justify-start gap-x-1.5 gap-y-2 @min-[620px]/list:contents"
  >
    {@render meta($t`Last used`, lastUsed)}
    {@render meta($t`First seen`, firstSeen)}
  </div>

  <!-- `h-9` is the button's own height, and the name's line carries the same floor, so
       every row stands equally tall whatever it holds. -->
  {#if action !== "none"}
    <span class="col-start-4 row-start-1 flex h-9 items-center justify-center">
      {@render actionControl()}
    </span>
  {/if}
</div>
