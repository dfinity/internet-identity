<!--
  6 · Immersive. The texture is the whole page and the statement stands on
  it; the identities float in a dock at the bottom, where a thumb reaches,
  like the concept's island. A line over the dock says what a tap does, and
  follows the pointer from avatar to avatar. The identity used last has a
  dot under it, as a running app has in the Dock. In "Identity colour" the
  page takes the colours of the identity pointed at.
-->
<script lang="ts">
  import { ArrowRightIcon, PlusIcon } from "@lucide/svelte";
  import { quintOut } from "svelte/easing";
  import { prefersReducedMotion } from "svelte/motion";
  import { fade, scale } from "svelte/transition";
  import Logo from "$lib/components/ui/Logo.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import { FLOATING } from "../../foundation/surfaces/Elevated.svelte";
  import SurfaceLevel from "../../foundation/surfaces/SurfaceLevel.svelte";
  import { nameOf } from "../mock";
  import type { Playground } from "../playground.svelte";
  import Backdrop from "../parts/Backdrop.svelte";
  import BigAvatar from "../parts/BigAvatar.svelte";
  import Brand from "../parts/Brand.svelte";
  import ProviderLogo, { type Provider } from "../parts/ProviderLogo.svelte";

  const { p }: { p: Playground } = $props();

  const empty = $derived(p.identities.length === 0);
  const palette = $derived(p.tone === "identity" ? p.focusPalette : undefined);
  const pointed = $derived(
    p.identities.find((one) => one.identityNumber === p.hovered),
  );
  const caption = $derived(
    empty
      ? "Sign in, or create an identity"
      : pointed !== undefined
        ? `Continue as ${nameOf(pointed)}`
        : p.identities.length === 1
          ? `Continue as ${nameOf(p.identities[0])}`
          : "Choose an identity",
  );

  const WAYS: { id: Provider; label: string }[] = [
    { id: "google", label: "Google" },
    { id: "apple", label: "Apple" },
    { id: "microsoft", label: "Microsoft" },
    { id: "sso", label: "SSO" },
  ];

  const ICON =
    "focus-visible:ring-focus-ring relative grid size-12 shrink-0 place-items-center rounded-full outline-none transition-[translate,scale] duration-220 ease-(--ii-ease-out) hover:-translate-y-1.5 hover:scale-110 focus-visible:-translate-y-1.5 focus-visible:scale-110 focus-visible:ring-2 active:scale-100 motion-reduce:transition-none motion-reduce:hover:translate-y-0 motion-reduce:hover:scale-100";
</script>

<div
  class="relative isolate flex min-h-dvh flex-col overflow-hidden bg-(--ii-page) transition-[background-color,color] duration-300 ease-(--ii-ease-out) motion-reduce:transition-none"
  style={palette === undefined
    ? undefined
    : `background-color: ${palette.background}; color: ${palette.foreground}`}
>
  <Backdrop
    texture={p.texture}
    ink={palette?.foreground}
    class={[
      "-z-1 [mask-image:radial-gradient(ellipse_60%_50%_at_26%_42%,transparent_35%,black_95%)]",
      palette === undefined
        ? "text-fg-quaternary opacity-55 dark:opacity-75"
        : "opacity-30",
    ]}
  />
  <header class="flex h-16 shrink-0 items-center px-6 md:px-10">
    <Logo
      class={["h-5.5", palette === undefined && "text-fg-primary"]}
      aria-label="Internet Identity"
    />
  </header>

  <main
    class="pointer-events-none flex flex-1 flex-col justify-center px-6 pb-44 md:px-10 lg:px-16"
  >
    <div data-arrive class="pointer-events-auto max-w-[52rem]">
      <Brand brand={p.brand} tinted={palette !== undefined} scale="hero" />
    </div>
  </main>

  <!-- The dock: what floats over the page (SURFACES.md). -->
  <div
    data-arrive
    class="fixed inset-x-0 bottom-0 z-10 flex flex-col items-center gap-3 px-3 pb-[max(1.25rem,env(safe-area-inset-bottom))]"
  >
    {#key caption}
      <p
        class="text-text-primary rounded-full bg-(--ii-floating) px-3.5 py-1.5 text-sm font-medium shadow-(--ii-shadow-4)"
        in:fade={{ duration: prefersReducedMotion.current ? 0 : 160 }}
      >
        {caption}
      </p>
    {/key}
    <SurfaceLevel level={FLOATING}>
      <div
        class="ii-elevation-6 flex max-w-full items-end gap-2 overflow-x-auto rounded-[1.75rem] bg-(--ii-floating) p-2"
      >
        {#if empty}
          <button
            type="button"
            onclick={() => p.useMethod("Continue with passkey")}
            class="btn btn-primary h-12 shrink-0 gap-2 rounded-full px-5 font-medium transition-[scale] duration-160 ease-out active:scale-[0.97]"
          >
            <ProviderLogo provider="passkey" />
            Continue with passkey
          </button>
          <span
            aria-hidden="true"
            class="bg-border-secondary mx-1 h-8 w-px shrink-0 self-center"
          ></span>
          {#each WAYS as way (way.id)}
            <button
              type="button"
              onclick={() => p.useMethod(`Continue with ${way.label}`)}
              aria-label={`Continue with ${way.label}`}
              title={way.label}
              class={[ICON, "ii-hover bg-(--ii-hover)"]}
            >
              <ProviderLogo provider={way.id} class="text-fg-primary size-5" />
            </button>
          {/each}
        {:else}
          {#each p.identities as one (one.identityNumber)}
            {@const recent =
              p.lastUsed &&
              p.identities.length > 1 &&
              p.recent?.identityNumber === one.identityNumber}
            <span
              class="relative flex shrink-0 flex-col items-center"
              out:scale={{
                duration: prefersReducedMotion.current ? 0 : 160,
                start: 0.6,
                easing: quintOut,
              }}
            >
              <button
                type="button"
                onclick={() => p.signIn(one)}
                onpointerenter={() => (p.hovered = one.identityNumber)}
                onpointerleave={() => (p.hovered = undefined)}
                onfocus={() => (p.hovered = one.identityNumber)}
                onblur={() => (p.hovered = undefined)}
                aria-label={`Continue as ${nameOf(one)}`}
                aria-busy={p.pending === one.identityNumber}
                class={ICON}
              >
                <BigAvatar identity={one} size={48} />
                {#if p.pending === one.identityNumber}
                  <span
                    class="absolute -inset-1"
                    transition:fade={{ duration: 150 }}
                  >
                    <ProgressRing class="text-fg-primary size-full" />
                  </span>
                {/if}
              </button>
              <!-- Last used: the Dock's dot under a running app. -->
              <span
                aria-hidden="true"
                class={[
                  "bg-fg-tertiary mt-1 size-1 rounded-full transition-opacity duration-200",
                  recent ? "opacity-100" : "opacity-0",
                ]}
              ></span>
              {#if recent}
                <span class="sr-only">Last used</span>
              {/if}
            </span>
          {/each}
          <span
            aria-hidden="true"
            class="bg-border-secondary mx-1 mb-3 h-8 w-px shrink-0 self-end"
          ></span>
          <span class="flex shrink-0 flex-col items-center">
            <button
              type="button"
              onclick={() => (p.methodsOpen = true)}
              aria-label="Add identity"
              title="Add identity"
              class={[
                ICON,
                "border-border-secondary text-fg-tertiary border border-dashed",
              ]}
            >
              <PlusIcon class="size-5" />
            </button>
            <span class="mt-1 size-1" aria-hidden="true"></span>
          </span>
          {#if p.identities.length === 1}
            <button
              type="button"
              onclick={() => p.signIn(p.identities[0])}
              class="btn btn-primary mb-2 h-12 shrink-0 gap-2 rounded-full px-5 font-medium transition-[scale] duration-160 ease-out active:scale-[0.97]"
            >
              Continue
              <ArrowRightIcon class="size-4 rtl:-scale-x-100" />
            </button>
          {/if}
        {/if}
      </div>
    </SurfaceLevel>
  </div>
</div>
