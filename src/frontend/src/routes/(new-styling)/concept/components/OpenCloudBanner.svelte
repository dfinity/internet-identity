<script lang="ts">
  /**
   * Open Cloud's banner, in Open Cloud's own look (opencloud.org): warm paper
   * ruled into a 24px grid, a numbered mono eyebrow, a Newsreader headline
   * with its second sentence in italic, a teal button with square corners,
   * and its demo engine, seven nodes from seven providers with two of them
   * down and the engine still running. Lays itself out by its own width, so
   * the same banner fills a row of Explore and of Create.
   */
  import { ArrowRightIcon } from "@lucide/svelte";
  import type { ClassValue } from "svelte/elements";
  import type { FeaturedApp } from "../mockData";
  import "../brands/brands.css";

  interface Props {
    app: FeaturedApp;
    class?: ClassValue;
  }

  const { app, class: className }: Props = $props();

  // opencloud.org's paper and rules.
  const PAPER = "#f8f6f1";
  const GRID = [
    "linear-gradient(to right, rgb(0 0 0 / 0.035) 1px, transparent 1px)",
    "linear-gradient(rgb(0 0 0 / 0.035) 1px, transparent 1px)",
  ].join(", ");

  /** The demo engine on opencloud.org, as it shows it. Illustrative. */
  const NODES: {
    code: string;
    city: string;
    provider: string;
    offline?: boolean;
  }[] = [
    { code: "US", city: "Ashburn", provider: "Flexential" },
    { code: "BR", city: "São Paulo", provider: "AWS" },
    { code: "DE", city: "Frankfurt", provider: "Hetzner", offline: true },
    { code: "ZA", city: "Johannesburg", provider: "Teraco" },
    { code: "IN", city: "Mumbai", provider: "Azure" },
    { code: "SG", city: "Singapore", provider: "Racks Central", offline: true },
    { code: "AU", city: "Sydney", provider: "NEXTDC" },
  ];
  const online = NODES.filter((node) => node.offline !== true).length;
</script>

<a
  href="https://opencloud.org/#early-access"
  target="_blank"
  rel="noopener noreferrer"
  style:background-color={PAPER}
  style:background-image={GRID}
  style:background-size="24px 24px"
  class={[
    "group @container relative isolate flex flex-col overflow-hidden rounded-[1.75rem] text-[#0f172a] shadow-[inset_0_0_0_1px_#e7e3da]",
    "focus-visible:outline-focus-ring focus-visible:outline-2 focus-visible:outline-offset-2",
    className,
  ]}
>
  <div class="flex flex-col gap-8 p-6 @xl:p-9">
    <div
      class="grid gap-6 @3xl:grid-cols-[minmax(0,1fr)_auto] @3xl:items-end @3xl:gap-10"
    >
      <div class="flex flex-col">
        <span class="flex items-center gap-2.5">
          <img src={app.logo} alt="" class="size-6" />
          <span class="text-[15px] font-semibold tracking-[-0.01em]">
            Open Cloud <span class="font-normal text-[#868078]">on</span> ICP
          </span>
        </span>
        <p
          class="oc-mono mt-10 text-[11px] tracking-[0.14em] text-[#868078] uppercase"
        >
          <span class="text-[#a8482b]">01</span>
          <span class="mx-1.5">|</span>
          Sovereign cloud engines
        </p>
        <h3
          class="oc-serif mt-3 text-[2rem] leading-[1.03] tracking-[-0.035em] text-balance @xl:text-[2.75rem]"
        >
          Nodes can fail. <em>Cloud engines can’t.</em>
        </h3>
        <p
          class="mt-4 max-w-[48ch] text-[15px] leading-6 text-pretty text-[#475569]"
        >
          Cloud engines combine hardware in locations you choose, from providers
          you choose.
        </p>
      </div>
      <span
        aria-hidden="true"
        class="inline-flex w-fit items-center gap-2 rounded-[4px] bg-[#0b5e5c] px-5 py-3 text-[15px] font-medium text-white transition-colors duration-150 group-hover:bg-[#084846]"
      >
        Apply for early access
        <ArrowRightIcon
          class="size-4 transition-transform duration-200 ease-(--ii-ease-out) group-hover:translate-x-0.5 motion-reduce:transition-none rtl:-scale-x-100"
        />
      </span>
    </div>

    <!-- The art: the engine, as opencloud.org shows it. -->
    <div
      aria-hidden="true"
      class="rounded-lg bg-white/90 p-4 shadow-[0_0_0_1px_#e7e3da,0_1px_2px_rgb(32_29_23/0.05),0_6px_16px_rgb(32_29_23/0.05)] @xl:p-5"
    >
      <div class="flex flex-wrap items-start justify-between gap-x-4 gap-y-2">
        <div>
          <p class="oc-serif text-xl leading-7 tracking-[-0.025em]">
            cloud-engine-global
          </p>
          <p class="text-[11.5px] text-[#868078]">
            <b class="oc-serif text-[17px] text-[#0b5e5c]">Nano</b> ·
            <b class="oc-serif text-[17px] text-[#0b5e5c]">{online}</b>
            of {NODES.length} nodes live ·
            <b class="oc-serif text-[17px] text-[#0b5e5c]">{NODES.length}</b>
            providers
          </p>
        </div>
        <span
          class="oc-mono inline-flex items-center gap-1.5 rounded-full bg-[#e4efed] px-2.5 py-1 text-[9px] tracking-[0.12em] text-[#084846] uppercase shadow-[inset_0_0_0_1px_#bfd8d5]"
        >
          <i class="size-1.5 rounded-full bg-[#0b5e5c]"></i>
          Running
        </span>
      </div>
      <ul class="mt-4 grid grid-cols-7 gap-1.5 @4xl:gap-2">
        {#each NODES as node (node.code)}
          {@const off = node.offline === true}
          <li
            class={[
              "flex min-w-0 flex-col rounded-[4px] px-2 pt-2 pb-2 @4xl:px-[11px] @4xl:pt-2 @4xl:pb-[9px]",
              off
                ? "bg-[#fbf1ee] shadow-[inset_0_0_0_1px_#efcfc6]"
                : "bg-white shadow-[inset_0_0_0_1px_#e7e3da]",
            ]}
          >
            <span class="flex items-center justify-between gap-1">
              <span
                class={[
                  "oc-mono text-[10px] tracking-[0.14em]",
                  off ? "text-[#a8482b]" : "text-[#868078]",
                ]}
              >
                {node.code}
              </span>
              <i
                class={[
                  "size-1.5 shrink-0 rounded-full",
                  off ? "bg-[#d0543c]" : "bg-[#3f8f6d]",
                ]}
              ></i>
            </span>
            <!-- Room for the city only where a card is wide enough. -->
            <span class="hidden min-w-0 flex-col @4xl:flex">
              <strong
                class={[
                  "oc-serif mt-1.5 truncate text-sm font-normal tracking-[-0.02em]",
                  off && "text-[#a8482b]",
                ]}
              >
                {node.city}
              </strong>
              <span
                class={[
                  "truncate text-[11px]",
                  off ? "text-[#a8482b]" : "text-[#475569]",
                ]}
              >
                {node.provider}
              </span>
              <span
                class={[
                  "mt-2 border-t pt-1.5 text-[10px]",
                  off
                    ? "border-[#efcfc6] text-[#a8482b]"
                    : "border-[#e7e3da] text-[#868078]",
                ]}
              >
                {off ? "Offline" : "Online"}
              </span>
            </span>
          </li>
        {/each}
      </ul>
    </div>
  </div>
</a>

<style>
  .oc-serif {
    font-family: Newsreader, Georgia, ui-serif, serif;
    font-weight: 400;
  }
  .oc-mono {
    font-family: "JetBrains Mono", ui-monospace, monospace;
  }
</style>
