<script lang="ts">
  import type { BrowserDescription } from "$lib/generated/internet_identity_types";
  import Badge from "$lib/components/ui/Badge.svelte";
  import { t } from "$lib/stores/locale.store";
  import { brandIconOf, brandNameOf } from "../browsers";

  type Action = "sign-out" | "signing-out" | "signed-out" | "none";

  interface Props {
    description: BrowserDescription;
    /** Already formatted: a relative time, or "Never" for a browser with no record. */
    lastUsed: string;
    /** Already formatted: a short date, or "Now" for a browser with no record. */
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

<div
  class="flex flex-row items-start gap-3 py-3 pr-5 pl-4 sm:items-center sm:gap-3"
>
  <span class="flex size-5 shrink-0 items-center justify-center">
    {#if brandIcon !== undefined}
      <img src={brandIcon} alt="" class="size-5 {dimmed ? 'opacity-50' : ''}" />
    {/if}
  </span>

  <!-- Below sm the meta drops under the name: the fixed columns and the button leave a
       375px screen no room for a name beside them. -->
  <div class="flex min-w-0 flex-1 flex-col gap-1 sm:flex-row sm:items-center">
    <div class="flex min-w-0 flex-1 flex-col gap-1">
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

      <span class="flex flex-row gap-4 sm:hidden">
        <span class="flex w-26 shrink-0 flex-col gap-1">
          <span class="text-text-tertiary text-xs font-semibold"
            >{$t`Last used`}</span
          >
          <span class="text-text-primary text-xs">{lastUsed}</span>
        </span>
        <span class="flex w-18 shrink-0 flex-col gap-1">
          <span class="text-text-tertiary text-xs font-semibold"
            >{$t`First seen`}</span
          >
          <span class="text-text-primary text-xs">{firstSeen}</span>
        </span>
      </span>
    </div>

    <span class="hidden shrink-0 flex-row gap-4 sm:flex">
      <span class="flex w-26 shrink-0 flex-col gap-1 whitespace-nowrap">
        <span class="text-text-tertiary text-xs font-semibold"
          >{$t`Last used`}</span
        >
        <span class="text-text-primary text-xs">{lastUsed}</span>
      </span>
      <span class="flex w-18 shrink-0 flex-col gap-1 whitespace-nowrap">
        <span class="text-text-tertiary text-xs font-semibold"
          >{$t`First seen`}</span
        >
        <span class="text-text-primary text-xs">{firstSeen}</span>
      </span>
    </span>
  </div>

  <span class="flex w-21 shrink-0 items-center justify-end">
    {#if action === "sign-out"}
      <button class="btn btn-secondary btn-sm" onclick={onSignOut}>
        {$t`Sign out`}
      </button>
    {:else if action === "signing-out"}
      <span class="text-text-tertiary text-sm">{$t`Signing out…`}</span>
    {:else if action === "signed-out"}
      <span class="text-text-tertiary text-sm">{$t`Signed out`}</span>
    {/if}
  </span>
</div>
