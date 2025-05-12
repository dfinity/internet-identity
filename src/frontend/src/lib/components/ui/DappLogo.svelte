<script lang="ts">
  import type { HTMLAttributes } from "svelte/elements";
  import { GlobeIcon } from "@lucide/svelte";
  import { isNullish, nonNullish } from "@dfinity/utils";
  import { getDapps } from "$lib/flows/dappsExplorer/dapps";

  type Props = Omit<HTMLAttributes<HTMLDivElement>, "children"> & {
    origin: string;
  };

  const { class: className, origin, ...props }: Props = $props();

  const dapps = getDapps();
  const img = $derived(dapps.find((dapp) => dapp.hasOrigin(origin))?.logoSrc);
</script>

<div
  aria-hidden="true"
  {...props}
  class={[
    "flex h-16 min-w-16 shrink-0 items-center justify-center overflow-hidden",
    isNullish(img) ? "rounded-full" : "rounded-2xl",
    isNullish(img) && "border",
    isNullish(img) && "border-gray-light-200 text-gray-light-900 bg-white",
    isNullish(img) &&
      "dark:border-gray-light-700 dark:bg-gray-dark-950 dark:text-gray-dark-25",
    className,
  ]}
>
  {#if nonNullish(img)}
    <img src={img} alt="" class="h-16" />
  {:else}
    <GlobeIcon size="1.5rem" />
  {/if}
</div>
