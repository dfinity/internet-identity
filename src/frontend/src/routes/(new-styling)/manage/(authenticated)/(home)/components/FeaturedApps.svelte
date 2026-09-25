<script lang="ts">
  import { MediaQuery } from "svelte/reactivity";
  import {
    ArrowUpRightIcon,
    ChevronDownIcon,
    ChevronUpIcon,
  } from "@lucide/svelte";
  import { t } from "$lib/stores/locale.store";
  import type { KnownDapp } from "$lib/legacy/flows/dappsExplorer/dapps";

  const { apps }: { apps: KnownDapp[] } = $props();

  const DWELL_MS = 6000;
  const FADE_MS = 260;

  const reducedMotion = new MediaQuery("(prefers-reduced-motion: reduce)");
  const autoAdvance = $derived(apps.length > 1 && !reducedMotion.current);

  let activeIndex = $state(0);
  let fading = $state(false);
  let direction = $state<1 | -1>(1);
  let paused = $state(false);

  const activeApp = $derived(apps[Math.min(activeIndex, apps.length - 1)]);

  let stopped = false;
  let dwellTimer: ReturnType<typeof setTimeout> | undefined;
  let fadeTimer: ReturnType<typeof setTimeout> | undefined;
  let dueAt = 0;
  let remainingOnPause = 0;

  const scheduleAdvance = (delayMs: number) => {
    if (stopped) {
      return;
    }
    clearTimeout(dwellTimer);
    dueAt = Date.now() + delayMs;
    dwellTimer = setTimeout(() => showRelative(1), delayMs);
  };

  const restartDwell = () => {
    if (autoAdvance && !paused) {
      scheduleAdvance(DWELL_MS);
    }
  };

  const showApp = (index: number, dir: 1 | -1) => {
    if (stopped || fading || index === activeIndex) {
      return;
    }
    if (reducedMotion.current) {
      activeIndex = index;
      restartDwell();
      return;
    }
    direction = dir;
    fading = true;
    fadeTimer = setTimeout(() => {
      activeIndex = index;
      fading = false;
      restartDwell();
    }, FADE_MS);
  };

  const showRelative = (delta: 1 | -1) => {
    showApp((activeIndex + delta + apps.length) % apps.length, delta);
  };

  const pause = () => {
    if (paused) {
      return;
    }
    paused = true;
    remainingOnPause = Math.max(0, dueAt - Date.now());
    clearTimeout(dwellTimer);
  };

  const resume = () => {
    if (!paused) {
      return;
    }
    paused = false;
    if (autoAdvance) {
      scheduleAdvance(remainingOnPause > 0 ? remainingOnPause : DWELL_MS);
    }
  };

  const handleFocusOut = (event: FocusEvent) => {
    if (
      event.relatedTarget instanceof Node &&
      event.currentTarget instanceof Node &&
      event.currentTarget.contains(event.relatedTarget)
    ) {
      return;
    }
    resume();
  };

  $effect(() => {
    if (autoAdvance) {
      scheduleAdvance(DWELL_MS);
      return () => clearTimeout(dwellTimer);
    }
  });

  $effect(() => () => {
    stopped = true;
    clearTimeout(dwellTimer);
    clearTimeout(fadeTimer);
  });

  let listViewport = $state<HTMLDivElement>();
  let rowElements: HTMLButtonElement[] = $state([]);

  $effect(() => {
    const viewport = listViewport;
    const row = rowElements[activeIndex];
    if (viewport === undefined || row === undefined) {
      return;
    }
    viewport.scrollTo({
      top: row.offsetTop - (viewport.clientHeight - row.offsetHeight) / 2,
      behavior: reducedMotion.current ? "auto" : "smooth",
    });
  });
</script>

<section class="@container mt-12 flex flex-col gap-3.5">
  <h2 class="text-text-primary text-base font-medium tracking-tight">
    {$t`Featured apps`}
  </h2>
  <div
    role="group"
    aria-label={$t`Featured apps`}
    onmouseenter={pause}
    onmouseleave={resume}
    onfocusin={pause}
    onfocusout={handleFocusOut}
    class="flex flex-col gap-5 @3xl:flex-row @3xl:items-stretch"
  >
    <div
      class="order-2 flex min-w-0 flex-col gap-3 @3xl:order-1 @3xl:max-w-105 @3xl:grow @3xl:basis-75"
    >
      <div
        bind:this={listViewport}
        class="relative max-h-72.5 [scrollbar-width:none] overflow-y-auto [&::-webkit-scrollbar]:hidden"
      >
        <div class="flex flex-col gap-1">
          {#each apps as app, index (app.website)}
            {@const isActive = index === activeIndex}
            <button
              type="button"
              bind:this={rowElements[index]}
              onclick={() => showApp(index, index > activeIndex ? 1 : -1)}
              aria-current={isActive ? "true" : undefined}
              class={[
                "flex h-13.5 shrink-0 items-center gap-3.5 rounded-lg px-3.5 text-start transition-colors outline-none",
                "focus-visible:ring-focus-ring focus-visible:ring-2 focus-visible:ring-inset",
                isActive ? "bg-bg-active" : "hover:bg-bg-active",
              ]}
            >
              <img
                src={app.logoSrc}
                alt=""
                width="34"
                height="34"
                class={[
                  "block size-8.5 shrink-0 rounded-lg transition-opacity",
                  isActive ? "opacity-100" : "opacity-55",
                ]}
              />
              <span class="flex min-w-0 flex-col gap-0.5">
                <span
                  class={[
                    "truncate text-sm font-semibold",
                    isActive ? "text-text-primary" : "text-text-secondary",
                  ]}
                >
                  {app.name}
                </span>
                {#if app.oneLiner !== undefined}
                  <span class="text-text-tertiary truncate text-xs">
                    {app.oneLiner}
                  </span>
                {/if}
              </span>
            </button>
          {/each}
        </div>
      </div>
      {#if apps.length > 1}
        <div class="flex gap-1.5 px-3.5">
          <button
            type="button"
            onclick={() => showRelative(-1)}
            aria-label={$t`Previous app`}
            class="btn btn-secondary btn-icon h-8"
          >
            <ChevronUpIcon class="size-4" />
          </button>
          <button
            type="button"
            onclick={() => showRelative(1)}
            aria-label={$t`Next app`}
            class="btn btn-secondary btn-icon h-8"
          >
            <ChevronDownIcon class="size-4" />
          </button>
        </div>
      {/if}
    </div>
    <div
      class="bg-bg-primary_alt border-border-secondary relative order-1 flex min-h-83.5 min-w-0 flex-col justify-between overflow-hidden rounded-xl border p-7 shadow-xs @3xl:order-2 @3xl:grow-2 @3xl:basis-110"
    >
      <div
        class="flex flex-col gap-5 transition-[opacity,transform] duration-260 ease-in-out"
        style:opacity={fading ? 0 : 1}
        style:transform="translateY({fading
          ? direction > 0
            ? -14
            : 14
          : 0}px)"
      >
        <img
          src={activeApp.logoSrc}
          alt=""
          width="56"
          height="56"
          class="block size-14 rounded-xl transition-transform duration-320 ease-in-out"
          style:transform={fading
            ? `translateY(${direction > 0 ? -10 : 10}px) scale(0.96)`
            : "translateY(0) scale(1)"}
        />
        <div class="flex flex-col gap-2.5">
          <h3 class="text-text-primary text-2xl font-medium tracking-tight">
            {activeApp.name}
          </h3>
          {#if activeApp.oneLiner !== undefined}
            <p
              class="text-text-tertiary max-w-md text-sm leading-relaxed text-pretty"
            >
              {activeApp.oneLiner}
            </p>
          {/if}
        </div>
      </div>
      <div class="mt-6 flex items-center">
        <a
          href={activeApp.website}
          target="_blank"
          rel="noopener noreferrer"
          class="btn btn-primary"
        >
          {$t`Open app`}
          <ArrowUpRightIcon class="size-4" />
        </a>
      </div>
      {#if autoAdvance}
        <div class="bg-border-secondary absolute inset-x-0 bottom-0 h-0.5">
          {#key activeIndex}
            <div
              class={[
                "progress h-full transition-colors",
                paused ? "bg-fg-quaternary" : "bg-fg-primary",
              ]}
              style:animation-duration="{DWELL_MS}ms"
              style:animation-play-state={paused ? "paused" : "running"}
            ></div>
          {/key}
        </div>
      {/if}
    </div>
  </div>
</section>

<style>
  .progress {
    width: 0;
    animation-name: progress;
    animation-timing-function: linear;
    animation-fill-mode: forwards;
  }

  @keyframes progress {
    to {
      width: 100%;
    }
  }
</style>
