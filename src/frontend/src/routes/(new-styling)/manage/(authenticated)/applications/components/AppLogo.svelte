<script lang="ts">
  import { GlobeIcon } from "@lucide/svelte";

  interface Props {
    /** A ready `<img src>`, or `undefined` for an app that publishes none. */
    logo?: string;
    size: "md" | "lg";
  }

  const { logo, size }: Props = $props();

  // A logo that fails to decode falls back to the globe instead of a broken image;
  // keyed by value so a later, valid logo still renders.
  let failedLogo = $state<string>();
  const shown = $derived(logo !== failedLogo ? logo : undefined);
</script>

<!-- Decorative: the app's name sits right beside it. -->
<span
  aria-hidden="true"
  class={[
    "border-border-secondary flex shrink-0 items-center justify-center overflow-hidden border",
    shown !== undefined ? "bg-white" : "bg-bg-primary text-fg-tertiary",
    { md: "size-10 rounded-lg", lg: "size-12 rounded-xl" }[size],
  ]}
>
  {#if shown !== undefined}
    <img
      src={shown}
      alt=""
      class="size-full object-contain"
      onerror={() => (failedLogo = shown)}
    />
  {:else}
    <GlobeIcon class={{ md: "size-5", lg: "size-6" }[size]} />
  {/if}
</span>
