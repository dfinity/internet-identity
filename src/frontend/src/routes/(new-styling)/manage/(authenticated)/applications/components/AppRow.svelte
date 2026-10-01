<script lang="ts">
  import { ChevronRightIcon } from "@lucide/svelte";
  import { getAppMetadataStore } from "$lib/stores/app-metadata.store";
  import { t } from "$lib/stores/locale.store";
  import { originLabel } from "$lib/utils/urlUtils";
  import AppLogo from "./AppLogo.svelte";

  interface Props {
    /** The origin the app's identity is derived for: what its metadata is published
     *  on and its consent is keyed by. */
    origin: string;
    /** Where the user signs in to it from, which is what they know it by. */
    displayOrigin: string;
    /** Already formatted: a relative time. */
    lastVisited: string;
    /** Whether the app may notify, or `undefined` while that is still being read and
     *  for an app this Internet Identity does not notify for. */
    allowed: boolean | undefined;
    /** Keeps the notifications column, empty or not, so the rows line up. */
    showNotifications: boolean;
    onOpen: () => void;
  }

  const {
    origin,
    displayOrigin,
    lastVisited,
    allowed,
    showNotifications,
    onOpen,
  }: Props = $props();

  // As the sign-in screens ask, so an app whose curated entry names only the origin
  // it is used on still resolves.
  const metadataStore = $derived(getAppMetadataStore(origin, displayOrigin));
  const metadata = $derived($metadataStore);
  const label = $derived(originLabel(displayOrigin));
</script>

<!-- Switches on the card's width, not the page's, as the devices list does. -->
<button
  onclick={onOpen}
  class="hover:bg-bg-primary_hover focus-visible:bg-bg-primary_hover grid w-full grid-cols-[auto_minmax(0,1fr)_auto] items-center gap-x-4 gap-y-2 p-4 text-start outline-none @min-[560px]/apps:grid-cols-[auto_minmax(0,1fr)_auto_auto]"
>
  <AppLogo logo={metadata.logo} size="md" />

  <span class="flex min-w-0 flex-col">
    <span class="text-text-primary truncate text-sm font-semibold">
      {metadata.name ?? label}
    </span>
    <span class="text-text-tertiary truncate text-sm">{label}</span>
  </span>

  <!-- Fixed tracks, so each column starts at the same place on every row. -->
  <span
    class="col-start-2 row-start-2 grid grid-cols-2 gap-x-4 @min-[560px]/apps:col-start-3 @min-[560px]/apps:row-start-1 @min-[560px]/apps:me-6 @min-[560px]/apps:grid-cols-[6.5rem_7rem] @min-[560px]/apps:gap-x-6"
  >
    <span
      class="flex min-w-0 flex-col gap-1 @min-[560px]/apps:whitespace-nowrap"
    >
      <span class="text-text-tertiary text-xs font-semibold">
        {$t`Last visited`}
      </span>
      <span class="text-text-primary text-xs">{lastVisited}</span>
    </span>
    {#if showNotifications}
      <span
        class="flex min-w-0 flex-col gap-1 @min-[560px]/apps:whitespace-nowrap"
      >
        {#if allowed !== undefined}
          <span class="text-text-tertiary text-xs font-semibold">
            {$t`Notifications`}
          </span>
          <span
            class={[
              "flex items-center gap-1.5 text-xs",
              allowed ? "text-text-primary" : "text-text-tertiary",
            ]}
          >
            <span
              aria-hidden="true"
              class={[
                "size-1.5 shrink-0 rounded-full",
                allowed ? "bg-fg-success-primary" : "bg-fg-quaternary",
              ]}
            ></span>
            {allowed ? $t`Allowed` : $t`Not allowed`}
          </span>
        {/if}
      </span>
    {/if}
  </span>

  <ChevronRightIcon
    aria-hidden="true"
    class="text-fg-tertiary col-start-3 row-span-2 row-start-1 size-5 @min-[560px]/apps:col-start-4 @min-[560px]/apps:row-span-1 rtl:-scale-x-100"
  />
</button>
