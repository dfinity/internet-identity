<script lang="ts">
  import { ArrowRightIcon, GlobeIcon, TerminalIcon } from "@lucide/svelte";
  import Badge from "$lib/components/ui/Badge.svelte";
  import Ellipsis from "$lib/components/utils/Ellipsis.svelte";
  import { getDapps } from "$lib/legacy/flows/dappsExplorer/dapps";

  interface Props {
    /** Hostname of the dapp the CLI is being authorized for, or undefined for
     *  generic mode (CLI signs into II itself). */
    dappOrigin?: string;
  }

  const { dappOrigin }: Props = $props();

  const dapps = getDapps();
  const dapp = $derived(
    dappOrigin !== undefined
      ? dapps.find((d) => d.hasOrigin(dappOrigin))
      : undefined,
  );
  const hostname = $derived(
    dappOrigin !== undefined ? new URL(dappOrigin).hostname : undefined,
  );
</script>

<div class="flex flex-1 flex-col items-center justify-center gap-6 py-5">
  {#if dappOrigin !== undefined}
    <div class="flex items-center gap-3">
      <div
        class={[
          "flex shrink-0 items-center justify-center overflow-hidden rounded-2xl",
          dapp?.logoSrc === undefined &&
            "border-border-tertiary text-fg-primary bg-bg-primary border",
        ]}
      >
        {#if dapp?.logoSrc !== undefined}
          <img
            src={dapp.logoSrc}
            alt={`${dapp.name} logo`}
            class="h-20 max-w-50 object-contain"
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
