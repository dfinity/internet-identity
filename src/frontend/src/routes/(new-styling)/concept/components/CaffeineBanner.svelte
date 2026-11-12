<script lang="ts">
  /**
   * Caffeine's banner, in Caffeine's own look (caffeine.ai): the sky
   * gradient of its "all-in-one platform" section, its wordmark, its
   * eyebrow and headline set as it sets them (Source Serif 4, regular,
   * tight), and its black "Start building" pill. The art is a prompt as its
   * builder takes one. Lays itself out by its own width, so the same banner
   * fills a row of Explore and a tile of Create.
   */
  import { ArrowUpIcon } from "@lucide/svelte";
  import type { ClassValue } from "svelte/elements";
  import caffeineLogo from "$lib/icons/caffeine_logo.svg?raw";
  import type { FeaturedApp } from "../mockData";
  import "../brands/brands.css";

  interface Props {
    app: FeaturedApp;
    class?: ClassValue;
  }

  const { app, class: className }: Props = $props();

  // caffeine.ai's section background, as its stylesheet has it.
  const SKY = [
    "radial-gradient(80% 60% at 78% 12%, rgb(156 196 230) 0%, rgb(156 196 230 / 0) 55%)",
    "radial-gradient(70% 70% at 18% 90%, rgb(47 111 176) 0%, rgb(47 111 176 / 0) 60%)",
    "linear-gradient(155deg, rgb(91 151 201) 0%, rgb(46 106 166) 48%, rgb(31 79 134) 100%)",
  ].join(", ");

  // The wordmark, from the app's logo (white letters on a black square), so
  // it can take the banner's color.
  const WORDMARK = [...caffeineLogo.matchAll(/<path d="([^"]+)"/g)].map(
    (match) => match[1],
  );
</script>

<a
  href={app.url}
  target="_blank"
  rel="noopener noreferrer"
  style:background={SKY}
  class={[
    "group @container relative isolate flex flex-col overflow-hidden rounded-[1.75rem] text-white",
    "focus-visible:outline-focus-ring focus-visible:outline-2 focus-visible:outline-offset-2",
    className,
  ]}
>
  <div
    class="grid flex-1 content-between gap-10 p-6 @xl:p-9 @3xl:grid-cols-[minmax(0,1fr)_minmax(0,22rem)] @3xl:content-center @3xl:items-center @3xl:gap-12"
  >
    <div class="flex flex-col">
      <svg
        viewBox="15 52 98 19"
        role="img"
        aria-label={app.name}
        class="h-[1.125rem] w-auto self-start"
      >
        {#each WORDMARK as d (d)}
          <path {d} fill="currentColor" />
        {/each}
      </svg>
      <p class="mt-10 text-[11px] tracking-[0.09em] text-white/80 uppercase">
        Build sovereign apps and websites on ICP
      </p>
      <!-- Broken where caffeine.ai breaks it, once there's room for two. -->
      <h3
        class="caffeine-serif mt-3 text-[2rem] leading-[1.05] tracking-[-0.04em] text-balance @xl:text-[2.75rem]"
      >
        Chat to build apps <span class="@xl:block"
          >on the Internet Computer.</span
        >
      </h3>
      <p
        class="mt-4 max-w-[40ch] text-[15px] leading-6 tracking-[-0.02em] text-pretty text-white/75"
      >
        Every app is sovereign, tamperproof and unstoppable.
      </p>
      <span
        aria-hidden="true"
        class="mt-8 w-fit rounded-full bg-[#0c0a09] px-5 py-2.5 text-[15px] tracking-[-0.03em] text-[#f5f5f4] transition-colors duration-150 group-hover:bg-[#292524]"
      >
        Start building
      </span>
    </div>

    <!-- The art: a prompt in the builder, on Caffeine's warm white. -->
    <div
      aria-hidden="true"
      class="rounded-2xl bg-[#fafaf9] p-4 text-[#0c0a09] shadow-[0_24px_48px_-16px_rgb(16_42_77/0.55)] transition-transform duration-500 ease-(--ii-ease-out) group-hover:-translate-y-1 motion-reduce:transition-none"
    >
      <p class="text-[15px] leading-6 tracking-[-0.02em]">
        A leave tracker for my team, with manager approvals<span class="caret"
        ></span>
      </p>
      <div class="mt-6 flex items-center justify-between gap-3">
        <span class="text-xs tracking-[-0.01em] text-[#78716c]">
          Builds it on the Internet Computer
        </span>
        <span
          class="grid size-8 shrink-0 place-items-center rounded-full bg-[#0c0a09] text-[#f5f5f4]"
        >
          <ArrowUpIcon class="size-4" />
        </span>
      </div>
    </div>
  </div>
</a>

<style>
  .caffeine-serif {
    font-family: "Source Serif 4", Georgia, "Times New Roman", serif;
    font-weight: 400;
  }

  /* The caret, as if the prompt were still being typed. */
  .caret {
    display: inline-block;
    width: 1.5px;
    height: 1.1em;
    margin-inline-start: 2px;
    vertical-align: -0.2em;
    background: currentColor;
    animation: caret-blink 1.1s steps(1) infinite;
  }
  @keyframes caret-blink {
    50% {
      opacity: 0;
    }
  }
  @media (prefers-reduced-motion: reduce) {
    .caret {
      animation: none;
    }
  }
</style>
