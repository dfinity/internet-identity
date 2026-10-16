<script lang="ts">
  import {
    BellIcon,
    BellOffIcon,
    ChevronRightIcon,
    ExternalLinkIcon,
  } from "@lucide/svelte";
  import { formatRelative, t } from "$lib/stores/locale.store";
  import AppSummary from "./AppSummary.svelte";
  import { appNameStore, appUrl } from "../apps";

  interface Props {
    origin: string;
    /** Whether the app may notify. None may while this Internet Identity does not
     *  notify. */
    allowed: boolean;
    /** When the app last notified, in milliseconds, or `undefined` where it never has. */
    lastNotifiedMillis: number | undefined;
    /** Off where this Internet Identity notifies for no app at all. */
    showNotifications: boolean;
    onManage: () => void;
  }

  const {
    origin,
    allowed,
    lastNotifiedMillis,
    showNotifications,
    onManage,
  }: Props = $props();

  const nameStore = $derived(appNameStore(origin));
  const name = $derived($nameStore);
</script>

<a
  href={appUrl(origin)}
  target="_blank"
  rel="noopener noreferrer"
  aria-label={$t`Open ${name}`}
  class="bg-bg-secondary hover:bg-bg-tertiary focus-visible:bg-bg-tertiary grid grid-cols-[auto_minmax(0,1fr)_auto] items-center gap-x-3.5 py-4 ps-4 pe-3 outline-none"
>
  <AppSummary {origin} />
  <span
    aria-hidden="true"
    class="text-fg-tertiary inline-flex size-9 items-center justify-center"
  >
    <ExternalLinkIcon class="size-5 shrink-0" />
  </span>
</a>
<button
  onclick={onManage}
  aria-label={$t`Manage ${name}`}
  class="bg-bg-primary border-border-tertiary hover:bg-bg-primary_hover focus-visible:bg-bg-primary_hover flex min-h-13 cursor-pointer items-center gap-3 border-t py-2 ps-4 pe-3 text-start outline-none"
>
  <span
    class="flex min-w-0 flex-1 items-center gap-2 overflow-hidden text-[13px] leading-[18px] whitespace-nowrap"
  >
    {#if showNotifications && allowed}
      <span class="text-text-secondary flex items-center gap-1.5">
        <BellIcon class="text-fg-secondary size-3.5 shrink-0" />
        {#if lastNotifiedMillis !== undefined}
          {$formatRelative(new Date(lastNotifiedMillis), { style: "long" })}
        {:else}
          {$t`None yet`}
        {/if}
      </span>
    {:else if showNotifications}
      <span class="text-text-tertiary flex items-center gap-1.5">
        <BellOffIcon class="text-fg-quaternary size-3.5 shrink-0" />
        {$t`Not allowed`}
      </span>
    {/if}
  </span>
  <span
    class="text-text-tertiary inline-flex shrink-0 items-center gap-0.5 text-[13px] leading-[18px] font-semibold whitespace-nowrap"
  >
    {$t`Manage`}
    <ChevronRightIcon class="text-fg-tertiary size-4" />
  </span>
</button>
