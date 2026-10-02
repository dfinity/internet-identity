<script lang="ts">
  import { Settings2Icon } from "@lucide/svelte";
  import { getAppMetadataStore } from "$lib/stores/app-metadata.store";
  import { formatRelative, t } from "$lib/stores/locale.store";
  import { originLabel } from "$lib/utils/urlUtils";
  import AppLogo from "./AppLogo.svelte";

  interface Props {
    /** The origin the app's identity is derived for: what its metadata is published
     *  on and its consent is keyed by. */
    origin: string;
    /** The latest sign-in at the app, in milliseconds. */
    lastUsedMillis: number;
    /** Whether the app may notify. An app this Internet Identity does not notify for
     *  may not. */
    allowed: boolean;
    /** When the app last notified, in milliseconds, or `undefined` where it never has. */
    lastNotifiedMillis: number | undefined;
    /** Off where this Internet Identity notifies for no app at all. */
    showNotifications: boolean;
    onManage: () => void;
  }

  const {
    origin,
    lastUsedMillis,
    allowed,
    lastNotifiedMillis,
    showNotifications,
    onManage,
  }: Props = $props();

  const metadataStore = $derived(getAppMetadataStore(origin));
  const metadata = $derived($metadataStore);
  const label = $derived(originLabel(origin));
  const name = $derived(metadata.name ?? label);
</script>

<!-- Switches on the card's width, not the page's, as the devices list does. The link's
     hit area is stretched over the whole row, so the settings button sits above it
     instead of inside it. -->
<div
  class="has-[a:hover]:bg-bg-primary_hover has-[a:focus-visible]:bg-bg-primary_hover relative grid w-full grid-cols-[auto_minmax(0,1fr)_auto] items-center gap-x-4 gap-y-2 p-4 @min-[560px]/apps:grid-cols-[auto_minmax(0,1fr)_auto_auto]"
>
  <AppLogo logo={metadata.logo} size="md" />

  <span class="flex min-w-0 flex-col">
    <a
      href={origin}
      target="_blank"
      rel="noopener noreferrer"
      class="text-text-primary truncate text-sm font-semibold outline-none after:absolute after:inset-0"
    >
      {name}
    </a>
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
      <span class="text-text-primary text-xs">
        {$formatRelative(new Date(lastUsedMillis), { style: "long" })}
      </span>
    </span>
    {#if showNotifications}
      <span
        class="flex min-w-0 flex-col gap-1 @min-[560px]/apps:whitespace-nowrap"
      >
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
          {#if !allowed}
            {$t`Not allowed`}
          {:else if lastNotifiedMillis !== undefined}
            {$formatRelative(new Date(lastNotifiedMillis), { style: "long" })}
          {:else}
            {$t`None yet`}
          {/if}
        </span>
      </span>
    {/if}
  </span>

  <button
    onclick={onManage}
    class="btn btn-tertiary btn-sm btn-icon relative col-start-3 row-start-1 @min-[560px]/apps:col-start-4"
    aria-label={$t`Manage ${name}`}
  >
    <Settings2Icon class="size-5" />
  </button>
</div>
