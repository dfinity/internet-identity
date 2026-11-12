<script lang="ts">
  /**
   * An editorial banner, after the App Store's: a line on what it's for, a
   * headline, the app's own icon large as the art, and the app's lockup with
   * Open. One flat color and nothing behind the icon, so a banner stays as
   * quiet as the shelves around it while the pages are swiped. The whole
   * card opens the app.
   */
  import AppLogo from "../../manage/(authenticated)/applications/components/AppLogo.svelte";
  import type { ExploreBannerItem } from "../mockData";

  interface Props {
    item: ExploreBannerItem;
  }

  const { item }: Props = $props();
</script>

<a
  href={item.app.url}
  target="_blank"
  rel="noopener noreferrer"
  style:background={item.background}
  class={[
    "group focus-visible:outline-focus-ring relative isolate grid min-h-[30rem] gap-6 overflow-hidden rounded-[1.75rem] p-6 focus-visible:outline-2 focus-visible:outline-offset-2",
    "grid-rows-[auto_1fr_auto] [grid-template-areas:'text'_'art'_'lockup']",
    "@2xl:min-h-[21rem] @2xl:grid-cols-[1fr_auto] @2xl:grid-rows-[1fr_auto] @2xl:p-9 @2xl:[grid-template-areas:'text_art'_'lockup_art']",
    item.dark ? "text-white" : "text-neutral-950",
  ]}
>
  <div class="[grid-area:text]">
    <p class="text-xs font-semibold tracking-[0.08em] uppercase opacity-70">
      {item.eyebrow}
    </p>
    <h3
      class="mt-2 max-w-[18ch] text-[1.75rem] leading-[1.1] font-semibold tracking-tight text-balance @2xl:text-[2.5rem]"
    >
      {item.title}
    </h3>
  </div>

  <!-- The art: the app's icon, large. -->
  <div
    aria-hidden="true"
    class="grid place-items-center [grid-area:art] @2xl:pe-6"
  >
    <img
      src={item.app.logo}
      alt=""
      class={[
        "size-40 rounded-[22%] ring-1 transition-transform duration-500 ease-(--ii-ease-out) group-hover:scale-[1.03] motion-reduce:transition-none @2xl:size-56",
        item.dark ? "ring-white/10" : "ring-black/5",
      ]}
    />
  </div>

  <div class="flex items-center gap-3 [grid-area:lockup]">
    <AppLogo logo={item.app.logo} size="lg" />
    <span class="flex min-w-0 flex-col">
      <span class="text-sm font-semibold">{item.app.name}</span>
      {#if item.app.oneLiner !== undefined}
        <span class="line-clamp-2 max-w-[34ch] text-xs opacity-70">
          {item.app.oneLiner}
        </span>
      {/if}
    </span>
    <span
      aria-hidden="true"
      class={[
        "ms-auto shrink-0 rounded-full px-4 py-1.5 text-sm font-semibold @2xl:ms-4",
        item.dark ? "bg-white text-neutral-950" : "bg-neutral-950 text-white",
      ]}
    >
      Open
    </span>
  </div>
</a>
