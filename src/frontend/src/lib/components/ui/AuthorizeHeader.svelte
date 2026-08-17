<script lang="ts">
  import Badge from "$lib/components/ui/Badge.svelte";
  import Ellipsis from "$lib/components/utils/Ellipsis.svelte";
  import { getAppMetadataStore } from "$lib/stores/app-metadata.store";
  import type { HTMLAttributes } from "svelte/elements";
  import { GlobeIcon } from "@lucide/svelte";

  type Props = HTMLAttributes<HTMLDivElement> & {
    origin: string;
  };

  const { class: className, origin, ...props }: Props = $props();

  const hostname = $derived(new URL(origin).hostname);
  // App-provided (permissionless) display metadata; the hostname badge below
  // stays visible regardless, as the trust anchor the user can verify.
  const metadataStore = $derived(getAppMetadataStore(origin));
  const metadata = $derived($metadataStore);
  // A logo that fails to decode falls back to the default icon instead of a
  // broken image; keyed by value so a later (valid) logo still renders.
  let failedLogo = $state<string>();
  const logo = $derived(
    metadata.logo !== failedLogo ? metadata.logo : undefined,
  );
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
      logo === undefined &&
        "border-border-tertiary text-fg-primary bg-bg-primary border",
    ]}
  >
    {#if logo !== undefined}
      <img
        src={logo}
        alt={`${metadata.name ?? hostname} logo`}
        class={["h-20 max-w-50 object-contain"]}
        onerror={() => (failedLogo = metadata.logo)}
      />
    {:else}
      <div class="flex size-20 items-center justify-center" aria-hidden="true">
        <GlobeIcon class="size-6" />
      </div>
    {/if}
  </div>
  <div class="flex max-w-full flex-col items-center gap-2">
    <Badge size="sm" class="max-w-[75%]">
      <Ellipsis text={hostname} position="middle" />
    </Badge>
    {#if metadata.description !== undefined}
      <p
        class="text-text-tertiary line-clamp-2 max-w-[85%] text-center text-sm text-balance"
      >
        {metadata.description}
      </p>
    {/if}
  </div>
</div>
