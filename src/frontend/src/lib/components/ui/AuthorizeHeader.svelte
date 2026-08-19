<script lang="ts">
  import Badge from "$lib/components/ui/Badge.svelte";
  import Ellipsis from "$lib/components/utils/Ellipsis.svelte";
  import { getAppMetadataStore } from "$lib/stores/app-metadata.store";
  import { originLabel } from "$lib/utils/urlUtils";
  import type { HTMLAttributes } from "svelte/elements";
  import { GlobeIcon } from "@lucide/svelte";

  type Props = HTMLAttributes<HTMLDivElement> & {
    /** Origin displayed to the user: the app they are signing in from. */
    origin: string;
    /** Origin the app's display metadata is published on — the origin its
     *  identity is derived for, i.e. its validated derivation origin when the
     *  request carries one. Defaults to `origin`, which is the same origin
     *  whenever the app doesn't use a derivation origin. */
    metadataOrigin?: string;
  };

  const {
    class: className,
    origin,
    metadataOrigin,
    ...props
  }: Props = $props();

  // Shown as the trust anchor for the app-provided metadata below, so it
  // keeps any scheme/port that distinguishes this origin from another.
  const hostname = $derived(originLabel(origin));
  // App-provided (permissionless) display metadata; the origin badge below
  // stays visible regardless, as the value the user can verify. A derivation
  // origin publishes on behalf of its alternative origins, which it has
  // certified as its own via `/.well-known/ii-alternative-origins`.
  const metadataStore = $derived(
    getAppMetadataStore(metadataOrigin ?? origin, origin),
  );
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
      <!-- `dir="auto"` resolves the direction from the text itself and isolates
           it from the rest of the screen, so an RTL description reads correctly
           and can't reorder anything around it. -->
      <p
        dir="auto"
        class="text-text-tertiary line-clamp-2 max-w-[85%] text-center text-sm text-balance"
      >
        {metadata.description}
      </p>
    {/if}
  </div>
</div>
