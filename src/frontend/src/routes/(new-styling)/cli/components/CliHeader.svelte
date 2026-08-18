<script lang="ts">
  import { ArrowRightIcon, GlobeIcon, TerminalIcon } from "@lucide/svelte";
  import Badge from "$lib/components/ui/Badge.svelte";
  import Ellipsis from "$lib/components/utils/Ellipsis.svelte";
  import { readable } from "svelte/store";
  import {
    getAppMetadataStore,
    type AppMetadata,
  } from "$lib/stores/app-metadata.store";
  import { originLabel } from "$lib/utils/urlUtils";

  interface Props {
    /** Hostname of the app the CLI is being authorized for, or undefined for
     *  generic mode (CLI signs into II itself). */
    appOrigin?: string;
  }

  const { appOrigin }: Props = $props();

  const emptyMetadataStore = readable<AppMetadata>({});
  const metadataStore = $derived(
    appOrigin !== undefined
      ? getAppMetadataStore(appOrigin)
      : emptyMetadataStore,
  );
  const app = $derived($metadataStore);
  // A logo that fails to decode falls back to the default icon instead of a
  // broken image; keyed by value so a later (valid) logo still renders.
  let failedLogo = $state<string>();
  const logo = $derived(app.logo !== failedLogo ? app.logo : undefined);
  const hostname = $derived(
    appOrigin !== undefined ? originLabel(appOrigin) : undefined,
  );
</script>

<div class="flex flex-1 flex-col items-center justify-center gap-6 py-5">
  {#if appOrigin !== undefined}
    <div class="flex items-center gap-3">
      <div
        class={[
          "flex shrink-0 items-center justify-center overflow-hidden rounded-2xl",
          logo === undefined &&
            "border-border-tertiary text-fg-primary bg-bg-primary border",
        ]}
      >
        {#if logo !== undefined}
          <img
            src={logo}
            alt={`${app.name ?? hostname} logo`}
            class="h-20 max-w-50 object-contain"
            onerror={() => (failedLogo = app.logo)}
          />
        {:else}
          <div
            class="flex size-20 items-center justify-center"
            aria-hidden="true"
          >
            <GlobeIcon class="size-6" />
          </div>
        {/if}
      </div>
      <ArrowRightIcon class="text-fg-quaternary size-5 shrink-0" />
      <div
        class="border-border-tertiary text-fg-primary bg-bg-primary flex size-20 shrink-0 items-center justify-center rounded-2xl border"
        aria-hidden="true"
      >
        <TerminalIcon class="size-8" />
      </div>
    </div>
    {#if hostname !== undefined}
      <Badge size="sm" class="max-w-[75%]">
        <Ellipsis text={hostname} position="middle" />
      </Badge>
    {/if}
  {:else}
    <div
      class="border-border-tertiary text-fg-primary bg-bg-primary flex size-20 shrink-0 items-center justify-center rounded-2xl border"
      aria-hidden="true"
    >
      <TerminalIcon class="size-9" />
    </div>
  {/if}
</div>
