<script lang="ts">
  /**
   * The one place Explore says you can make an app of your own, as a banner
   * among the others: Caffeine leads, as it needs nothing but words, and the
   * ways that take code sit under it as a row of links. It comes third, after
   * the apps, since Explore is first for what you can use.
   */
  import { ArrowUpIcon, ArrowUpRightIcon } from "@lucide/svelte";
  import AppLogo from "../../manage/(authenticated)/applications/components/AppLogo.svelte";
  import type { FeaturedApp } from "../mockData";

  interface Props {
    app: FeaturedApp;
  }

  const { app }: Props = $props();

  /** The ways that take code, each to DFINITY's own page for it. */
  const WAYS: { title: string; detail: string; url: string }[] = [
    {
      title: "With your AI agent",
      detail: "Claude Code, Cursor or Codex, with DFINITY's skills",
      url: "https://docs.internetcomputer.org/guides/ai-coding-agents/",
    },
    {
      title: "On your computer",
      detail: "The icp command line, in Motoko or Rust",
      url: "https://docs.internetcomputer.org/getting-started/quickstart/",
    },
    {
      title: "In your browser",
      detail: "ICP Ninja, with nothing to install",
      url: "https://icp.ninja",
    },
  ];

  /** As light as Open Cloud's banner, in the hue of Caffeine's own sky. */
  const BACKGROUND = "#d6e9fe";
  const FOCUS =
    "focus-visible:outline-focus-ring focus-visible:outline-2 focus-visible:outline-offset-2";
</script>

<section
  aria-label="Build your own app"
  style:background={BACKGROUND}
  class="flex flex-col gap-2 rounded-[1.75rem] p-2 text-neutral-950 [color-scheme:light]"
>
  <a
    href={app.url}
    target="_blank"
    rel="noopener noreferrer"
    class={[
      "group grid gap-6 rounded-[1.375rem] p-4 @2xl:grid-cols-[1fr_auto] @2xl:p-7",
      FOCUS,
    ]}
  >
    <div class="flex min-w-0 flex-col">
      <p class="text-xs font-semibold tracking-[0.08em] uppercase opacity-70">
        Build your own
      </p>
      <h3
        class="mt-2 max-w-[18ch] text-[1.75rem] leading-[1.1] font-semibold tracking-tight text-balance @2xl:text-[2.5rem]"
      >
        Describe an app. Caffeine builds it.
      </h3>
      <p class="mt-3 max-w-[44ch] text-sm text-pretty opacity-75">
        It writes the app and puts it online, with sign-in by Internet Identity
        built in.
      </p>

      <!-- The art: a prompt as Caffeine takes one, in one line. -->
      <div
        aria-hidden="true"
        class="mt-6 flex max-w-xl items-center gap-3 rounded-3xl bg-white py-2 ps-4 pe-2 shadow-[0_0_0_1px_rgb(0_0_0/0.05),0_1px_2px_rgb(0_0_0/0.05)] transition-transform duration-500 ease-(--ii-ease-out) group-hover:-translate-y-1 motion-reduce:transition-none @2xl:ps-5"
      >
        <!-- The caret stays with the last word, never on a line of its own. -->
        <p class="min-w-0 flex-1 text-sm leading-6 @2xl:text-[15px]">
          A booking page for my yoga <span class="whitespace-nowrap"
            >studio<span class="caret"></span></span
          >
        </p>
        <span
          class="grid size-8 shrink-0 place-items-center rounded-full bg-neutral-950 text-white"
        >
          <ArrowUpIcon class="size-4" />
        </span>
      </div>
    </div>

    <div
      class="flex items-center gap-3 @2xl:flex-col @2xl:items-end @2xl:justify-end"
    >
      <span class="flex min-w-0 items-center gap-3">
        <AppLogo logo={app.logo} size="lg" />
        <span class="flex min-w-0 flex-col">
          <span class="text-sm font-semibold">{app.name}</span>
          <span class="text-xs opacity-70">caffeine.ai</span>
        </span>
      </span>
      <span
        aria-hidden="true"
        class="ms-auto shrink-0 rounded-full bg-neutral-950 px-4 py-1.5 text-sm font-semibold text-white @2xl:ms-0"
      >
        Start building
      </span>
    </div>
  </a>

  <!-- The ways that take code: one row of links on a lighter plate. -->
  <div class="flex flex-col gap-1 rounded-[1.375rem] bg-white/55 p-2">
    <p class="px-3 pt-2 text-xs font-medium opacity-70">
      Or write the code yourself
    </p>
    <ul class="grid @2xl:grid-cols-3">
      {#each WAYS as way (way.url)}
        <li>
          <a
            href={way.url}
            target="_blank"
            rel="noopener noreferrer"
            class={[
              "group flex h-full items-start gap-2 rounded-2xl px-3 py-2.5 transition-colors duration-150 hover:bg-white/80 active:bg-white motion-reduce:transition-none",
              FOCUS,
            ]}
          >
            <span class="flex min-w-0 flex-1 flex-col gap-0.5">
              <span class="text-sm font-semibold">{way.title}</span>
              <span class="text-xs text-pretty opacity-70">{way.detail}</span>
            </span>
            <ArrowUpRightIcon
              class="mt-0.5 size-4 shrink-0 opacity-40 transition-[opacity,translate] duration-200 ease-(--ii-ease-out) group-hover:translate-x-0.5 group-hover:-translate-y-0.5 group-hover:opacity-80 motion-reduce:transition-none rtl:-scale-x-100"
              aria-hidden="true"
            />
          </a>
        </li>
      {/each}
    </ul>
  </div>
</section>

<style>
  /* The caret in Caffeine's prompt, as if it were still being typed. */
  .caret {
    display: inline-block;
    width: 2px;
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
