<script lang="ts">
  import { isNullish, nonNullish } from "@dfinity/utils";
  import Badge from "$lib/components/ui/Badge.svelte";
  import Ellipsis from "$lib/components/utils/Ellipsis.svelte";
  import { getDapps } from "$lib/legacy/flows/dappsExplorer/dapps.js";
  import type { HTMLAttributes } from "svelte/elements";
  import { GlobeIcon } from "@lucide/svelte";

  type Props = HTMLAttributes<HTMLDivElement> & {
    origin: string;
  };

  const { class: className, origin, ...props }: Props = $props();

  const hostname = $derived(new URL(origin).hostname);
  const dapps = getDapps();
  const dapp = $derived(dapps.find((dapp) => dapp.hasOrigin(origin)));
</script>

<div
  {...props}
  class={[
    "flex flex-1 flex-col items-center justify-center gap-6 py-5",
    className,
  ]}
>
  <div
    class={[
      "flex shrink-0 items-center justify-center overflow-hidden rounded-2xl",
      isNullish(dapp?.logoSrc) &&
        "border-border-tertiary text-fg-primary bg-bg-primary border",
    ]}
  >
    {#if nonNullish(dapp?.logoSrc)}
      <img
        src={dapp?.logoSrc}
        alt={`${dapp?.name ?? origin} logo`}
        class="h-16 max-w-50 object-contain"
      />
    {:else}
      <div class="flex size-16 items-center justify-center" aria-hidden="true">
        <GlobeIcon class="size-6" />
      </div>
    {/if}
  </div>
  <Badge size="sm" class="max-w-[75%]">
    <Ellipsis text={hostname} position="middle" />
  </Badge>
</div>
