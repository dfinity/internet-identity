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

<!--
  Two layouts rather than one that bends. Above `sm` the row is a line: identity, then
  two fixed meta columns, then the action. Below it those fixed columns have nowhere to
  go — the name and its badge need two lines and grow straight into them — so the row
  becomes a block instead: identity, then the meta as a grid, then the action across the
  full width.
-->
<div class="flex flex-col gap-3 px-4 py-3 sm:flex-row sm:items-center sm:gap-3">
  <div class="flex min-w-0 flex-1 flex-row items-center gap-3">
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
  </div>

  <!-- A grid below `sm` so the two values line up under one another whatever their
       length; fixed columns above it, where the row has the width for them. Indented to
       the brand name, past the icon. -->
  <div
    class="ml-8 grid grid-cols-2 gap-4 sm:ml-0 sm:flex sm:shrink-0 sm:flex-row"
  >
    <span class="flex flex-col gap-1 sm:w-26 sm:shrink-0 sm:whitespace-nowrap">
      <span class="text-text-tertiary text-xs font-semibold"
        >{$t`Last used`}</span
      >
      <span class="text-text-primary text-xs">{lastUsed}</span>
    </span>
    <span class="flex flex-col gap-1 sm:w-18 sm:shrink-0 sm:whitespace-nowrap">
      <span class="text-text-tertiary text-xs font-semibold"
        >{$t`First seen`}</span
      >
      <span class="text-text-primary text-xs">{firstSeen}</span>
    </span>
  </div>

  <!-- Kept as an empty column above `sm` even with nothing in it, so the meta stays
       aligned with the rows that do carry a button; dropped entirely below, where it
       would only be blank space. -->
  <span
    class="{action === 'none'
      ? 'hidden sm:flex'
      : 'flex'} ml-8 sm:ml-0 sm:w-21 sm:shrink-0 sm:items-center sm:justify-end"
  >
    {#if action === "sign-out"}
      <button
        class="btn btn-secondary btn-sm w-full sm:w-auto"
        onclick={onSignOut}
      >
        {$t`Sign out`}
      </button>
    {:else if action === "signing-out"}
      <span class="text-text-tertiary text-sm">{$t`Signing out…`}</span>
    {:else if action === "signed-out"}
      <span class="text-text-tertiary text-sm">{$t`Signed out`}</span>
    {/if}
  </span>
</div>
