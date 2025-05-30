<script lang="ts">
  import { isNullish, nonNullish } from "@dfinity/utils";
  import Badge from "$lib/components/ui/Badge.svelte";
  import Ellipsis from "$lib/components/utils/Ellipsis.svelte";
  import { getDapps } from "$lib/flows/dappsExplorer/dapps.js";
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

<div {...props} class={["flex flex-1 flex-col", className]}>
  <div
    class="flex flex-1 flex-col items-center justify-center pb-3 [@media(min-height:648px)]:py-8"
  >
    <div
      aria-hidden="true"
      class={[
        "mb-4 flex shrink-0 items-center justify-center overflow-hidden rounded-2xl",
        isNullish(dapp?.logoSrc) &&
          "border-border-tertiary text-fg-primary bg-bg-primary border",
      ]}
    >
      {#if nonNullish(dapp?.logoSrc)}
        <img src={dapp?.logoSrc} alt="" class="h-16 max-w-24 object-contain" />
      {:else}
        <div class="flex size-16 items-center justify-center">
          <GlobeIcon size="1.5rem" />
        </div>
      {/if}
    </div>
    <Badge size="sm" class="max-w-[75%]">
      <Ellipsis text={hostname} position="middle" />
    </Badge>
  </div>
  <h1 class="text-text-primary mb-2 self-start text-2xl font-medium">
    Sign in
  </h1>
  <p class="text-text-secondary self-start text-sm">
    {#if nonNullish(dapp?.name)}
      <span>to <b>{dapp.name}</b></span>
    {/if}
    <span>with Internet Identity</span>
  </p>
</div>
