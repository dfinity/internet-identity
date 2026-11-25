<!--
  The landing playground (`/concept/landing`): Internet Identity's first
  page, where you pick an identity or make one, in six layouts on the
  concept's surfaces, type and components. See README.md.

  Three ways to look: the page in this window; the page on a phone (a frame
  390 wide, the same page inside); all six at once. The frames are this
  page with `frame=1`, which drops the controls and takes its choices from
  the window around it.
-->
<script lang="ts">
  import "../foundation/surfaces/surfaces.css";
  import { replaceState } from "$app/navigation";
  import { page } from "$app/state";
  import { FlaskConicalIcon } from "@lucide/svelte";
  import { onMount, tick, untrack } from "svelte";
  import { prefersReducedMotion } from "svelte/motion";
  import { fade } from "svelte/transition";
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import { arrive } from "../foundation/motion/motion";
  import { ConceptTheme } from "../mock/theme.svelte";
  import Centered from "./layouts/Centered.svelte";
  import Immersive from "./layouts/Immersive.svelte";
  import Profiles from "./layouts/Profiles.svelte";
  import Sentence from "./layouts/Sentence.svelte";
  import Split from "./layouts/Split.svelte";
  import Welcome from "./layouts/Welcome.svelte";
  import SignInMethods from "./parts/SignInMethods.svelte";
  import Thumbnail from "./parts/Thumbnail.svelte";
  import Toolbar from "./parts/Toolbar.svelte";
  import {
    BRANDS,
    DEFAULT_TEXTURE,
    LAYOUTS,
    Playground,
    STATES,
    TONES,
    type LayoutId,
  } from "./playground.svelte";

  const p = new Playground();
  p.load(new URLSearchParams(window.location.search));

  const theme = new ConceptTheme();

  const LAYOUT_OF = {
    centered: Centered,
    split: Split,
    profiles: Profiles,
    welcome: Welcome,
    sentence: Sentence,
    immersive: Immersive,
  };
  const Layout = $derived(LAYOUT_OF[p.layout]);
  const current = $derived(LAYOUTS.find((one) => one.id === p.layout));

  /** Where the flask is while the controls are folded: in the footer of
   *  a layout that has room for it (Profiles), else floating in the middle
   *  of the header, where the controls open. */
  const labInFooter = $derived(
    p.view === "page" && current?.labInFooter === true,
  );

  // --- The frames: a phone, and the overview ---

  /** What a frame shows of the window's choices: its own layout, and in
   *  the overview each other layout with the texture it starts with. */
  const frameQuery = (layout: LayoutId = p.layout): string =>
    p.query({
      frame: "1",
      view: "page",
      layout,
      ...(layout === p.layout ? {} : { texture: DEFAULT_TEXTURE[layout] }),
    });

  /** Each frame opens on the choices of the moment it opens, and then gets
   *  every change sent, without reloading. */
  let phoneFrame = $state<HTMLIFrameElement>();
  let thumbFrames = $state<(HTMLIFrameElement | undefined)[]>([]);
  const phoneSrc = $derived(
    p.view === "phone"
      ? `${window.location.pathname}${untrack(() => frameQuery())}`
      : "",
  );
  const overviewSrc = $derived(
    p.view === "all"
      ? untrack(() =>
          LAYOUTS.map(
            (one) => `${window.location.pathname}${frameQuery(one.id)}`,
          ),
        )
      : [],
  );

  const send = (frame: HTMLIFrameElement | undefined) => {
    const layout = frame?.dataset.layout as LayoutId | undefined;
    frame?.contentWindow?.postMessage(
      { type: "ii-landing", query: frameQuery(layout ?? p.layout) },
      window.location.origin,
    );
  };

  $effect(() => {
    void p.query();
    send(phoneFrame);
    thumbFrames.forEach(send);
  });

  onMount(() => {
    if (!p.framed) {
      return;
    }
    // Inside a frame: take the window's choices, and its theme.
    const onMessage = (event: MessageEvent) => {
      if (
        event.origin === window.location.origin &&
        typeof event.data === "object" &&
        event.data?.type === "ii-landing"
      ) {
        p.load(new URLSearchParams(event.data.query as string));
      }
    };
    const onStorage = (event: StorageEvent) => {
      if (event.key === "ii-concept-theme") {
        const system = matchMedia("(prefers-color-scheme: dark)").matches
          ? "dark"
          : "light";
        theme.set(
          event.newValue === "light" || event.newValue === "dark"
            ? event.newValue
            : system,
        );
      }
    };
    window.addEventListener("message", onMessage);
    window.addEventListener("storage", onStorage);
    return () => {
      window.removeEventListener("message", onMessage);
      window.removeEventListener("storage", onStorage);
    };
  });

  // The address keeps every choice, so a link opens the same page.
  $effect(() => {
    const query = p.query();
    if (p.framed) {
      return;
    }
    void tick().then(() => replaceState(query, page.state));
  });

  // --- Keys ---

  const cycle = <T extends string>(
    options: readonly { id: T }[],
    value: T,
  ): T => {
    const index = options.findIndex((one) => one.id === value);
    return options[(index + 1) % options.length].id;
  };

  /** Runs a key's choice; whether the key was one. */
  const press = (pressed: string): boolean => {
    const key = pressed.toLowerCase();
    const state = STATES[Number(pressed) - 1];
    if (pressed === "ArrowRight") {
      p.stepLayout(1);
    } else if (pressed === "ArrowLeft") {
      p.stepLayout(-1);
    } else if (state !== undefined) {
      p.setState(state.id);
    } else if (key === "l") {
      p.lastUsed = !p.lastUsed;
    } else if (key === "t") {
      p.stepTexture();
    } else if (key === "b") {
      p.brand = cycle(BRANDS, p.brand);
    } else if (key === "c") {
      p.tone = cycle(TONES, p.tone);
    } else if (key === "d") {
      theme.set(theme.current === "dark" ? "light" : "dark");
    } else if (key === "w") {
      p.view = "page";
    } else if (key === "p") {
      p.view = p.view === "phone" ? "page" : "phone";
    } else if (key === "o") {
      p.view = p.view === "all" ? "page" : "all";
    } else if (key === "h") {
      p.controls = !p.controls;
    } else if (key === "escape" && p.editing) {
      p.editing = false;
    } else {
      return false;
    }
    return true;
  };

  const onKey = (event: KeyboardEvent) => {
    if (
      p.methodsOpen ||
      event.metaKey ||
      event.ctrlKey ||
      event.altKey ||
      (event.target instanceof HTMLElement &&
        event.target.closest("input, textarea, [contenteditable]") !== null)
    ) {
      return;
    }
    if (p.framed) {
      // A frame keeps the focus once clicked: its keys are the window's.
      window.parent.postMessage(
        { type: "ii-landing-key", key: event.key },
        window.location.origin,
      );
      return;
    }
    if (press(event.key)) {
      event.preventDefault();
    }
  };

  // The window hears the keys pressed inside its frames.
  onMount(() => {
    if (p.framed) {
      return;
    }
    const onFrameKey = (event: MessageEvent) => {
      if (
        event.origin === window.location.origin &&
        typeof event.data === "object" &&
        event.data?.type === "ii-landing-key" &&
        typeof event.data.key === "string"
      ) {
        press(event.data.key as string);
      }
    };
    window.addEventListener("message", onFrameKey);
    return () => window.removeEventListener("message", onFrameKey);
  });

  /** Add identity opens on signing in, whatever it was left on. */
  let methodsMode = $state<"signin" | "signup">("signin");
  $effect(() => {
    if (!p.methodsOpen) {
      methodsMode = "signin";
    }
  });

  // A layout or a state arriving: its blocks fade in a step apart.
  const visit = $derived(`${p.layout}:${p.state}`);
</script>

<svelte:window onkeydown={onKey} />

<div class="concept min-h-dvh">
  {#if p.view === "page" || p.framed}
    {#key visit}
      <div
        use:arrive={{
          targets: "[data-arrive]",
          step: 70,
          reduced: prefersReducedMotion.current,
        }}
      >
        <Layout {p} />
      </div>
    {/key}
  {:else if p.view === "phone"}
    <div
      class="flex min-h-dvh flex-col items-center justify-center gap-6 bg-(--ii-page) px-4 pt-24 pb-10"
    >
      <iframe
        bind:this={phoneFrame}
        onload={() => send(phoneFrame)}
        title={`${current?.label} on a phone`}
        src={phoneSrc}
        class="h-[min(844px,calc(100dvh-11rem))] w-[390px] shrink-0 rounded-[2.75rem] border-[6px] border-(--ii-floating) shadow-(--ii-shadow-6)"
      ></iframe>
      <p
        class="text-text-tertiary max-w-[24rem] text-center text-sm text-pretty"
      >
        <span class="text-text-primary font-medium">{current?.label}.</span>
        {current?.note}
      </p>
    </div>
  {:else}
    <div class="min-h-dvh bg-(--ii-page) px-4 pt-24 pb-12 md:px-8">
      <ul
        class="mx-auto grid max-w-[96rem] grid-cols-1 gap-x-6 gap-y-10 md:grid-cols-2 xl:grid-cols-3"
      >
        {#each LAYOUTS as option, index (option.id)}
          <li>
            <button
              type="button"
              onclick={() => {
                p.setLayout(option.id);
                p.view = "page";
              }}
              class="group focus-visible:ring-focus-ring flex w-full flex-col gap-3 rounded-2xl text-start outline-none focus-visible:ring-2 focus-visible:ring-offset-4"
            >
              <Thumbnail
                src={overviewSrc[index]}
                layout={option.id}
                title={option.label}
                bind:frame={thumbFrames[index]}
                onload={() => send(thumbFrames[index])}
              />
              <span class="flex flex-col gap-1 px-1">
                <span class="text-text-primary text-sm font-medium">
                  <span class="text-text-tertiary me-1.5 tabular-nums">
                    {index + 1}
                  </span>{option.label}
                </span>
                <span
                  class="text-text-tertiary text-[13px] leading-[18px] text-pretty"
                >
                  {option.note}
                </span>
              </span>
            </button>
          </li>
        {/each}
      </ul>
    </div>
  {/if}

  {#if !p.framed}
    {#if !p.controls && !labInFooter}
      <button
        type="button"
        title="Show the controls (H)"
        aria-label="Show the controls"
        onclick={() => (p.controls = true)}
        transition:fade={{ duration: 150 }}
        class="text-fg-quaternary hover:text-fg-tertiary focus-visible:outline-focus-ring fixed start-1/2 top-3.5 z-30 grid size-9 -translate-x-1/2 place-items-center rounded-full bg-(--ii-floating) shadow-(--ii-shadow-4) transition-colors duration-150 focus-visible:outline-2 md:top-6.5 rtl:translate-x-1/2"
      >
        <FlaskConicalIcon class="size-4" aria-hidden="true" />
      </button>
    {:else if p.controls}
      <div
        class="pointer-events-none fixed inset-x-0 top-3 z-30 flex justify-center px-2"
        transition:fade={{ duration: 150 }}
      >
        <div class="pointer-events-auto">
          <Toolbar {p} {theme} onHide={() => (p.controls = false)} />
        </div>
      </div>
    {/if}
  {/if}

  {#if p.methodsOpen}
    <Dialog onClose={() => (p.methodsOpen = false)}>
      <div class="flex flex-col gap-1 pb-6">
        <h2 class="text-text-primary text-xl font-semibold tracking-tight">
          {methodsMode === "signin" ? "Add an identity" : "Create an identity"}
        </h2>
        <p class="text-text-tertiary text-sm">
          {methodsMode === "signin"
            ? "Sign in to one you already have."
            : "With a passkey or an account you already have."}
        </p>
      </div>
      <SignInMethods onPick={p.useMethod} bind:mode={methodsMode} />
    </Dialog>
  {/if}
</div>
