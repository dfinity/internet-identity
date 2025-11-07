<script lang="ts">
  import type { HTMLAttributes } from "svelte/elements";
  import { nonNullish } from "@dfinity/utils";

  type Props = HTMLAttributes<HTMLDivElement> & {
    header: string;
    subheader: string;
    description: string;
  };

  clearTimeout(undefined);

  const { children, header, subheader, description }: Props = $props();
</script>

<div
  class="border-bg-disabled from-bg-primary_alt to-bg-secondary flex w-[280px] flex-col rounded-xl border bg-gradient-to-b px-6 pt-5 pb-8 sm:w-[320px] lg:w-[394px]"
>
  <p class="text-text-tertiary text-sm uppercase">{header}</p>
  <div class="grid h-[280px] place-items-center">
    {#if nonNullish(children)}
      {@render children()}
    {/if}
  </div>
  <div class="flex flex-col gap-2">
    <p class="text-text-primary text-xl">{subheader}</p>
    <p class="text-text-tertiary text-base">{description}</p>
  </div>
</div>
