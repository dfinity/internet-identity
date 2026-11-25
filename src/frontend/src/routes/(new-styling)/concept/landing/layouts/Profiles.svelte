<!--
  1 · Profiles. The identities as large tiles in the dashboard's frame, the
  way My apps shows apps: the colour and the initial tell them apart before
  the name is read, and a new one is a dashed slot at the end. Edit puts a
  remove mark on each, as a home screen does.
-->
<script lang="ts">
  import { MinusIcon, PlusIcon } from "@lucide/svelte";
  import { quintOut } from "svelte/easing";
  import { prefersReducedMotion } from "svelte/motion";
  import { fade, scale } from "svelte/transition";
  import Footer from "$lib/components/layout/Footer.svelte";
  import Logo from "$lib/components/ui/Logo.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import Elevated from "../../foundation/surfaces/Elevated.svelte";
  import { methodOf, nameOf } from "../mock";
  import type { Playground } from "../playground.svelte";
  import Backdrop from "../parts/Backdrop.svelte";
  import BigAvatar from "../parts/BigAvatar.svelte";
  import EditToggle from "../parts/EditToggle.svelte";
  import LastUsed from "../parts/LastUsed.svelte";
  import SignInMethods from "../parts/SignInMethods.svelte";

  const { p }: { p: Playground } = $props();

  const empty = $derived(p.identities.length === 0);
  const single = $derived(p.identities.length === 1);

  const TILE =
    "group flex w-32 flex-col items-center gap-3 rounded-3xl px-2 pt-4 pb-3 outline-none focus-visible:ring-2 focus-visible:ring-focus-ring transition-[scale] duration-160 ease-out active:scale-[0.97] motion-reduce:active:scale-100 sm:w-36";
  const LIFT =
    "relative transition-[scale] duration-220 ease-(--ii-ease-out) group-hover:scale-106 group-focus-visible:scale-106 motion-reduce:transition-none";
</script>

<div class="flex min-h-dvh flex-col bg-(--ii-page) md:p-3">
  <Elevated
    offset={2}
    class="relative isolate flex min-h-dvh flex-1 flex-col overflow-hidden max-md:rounded-none max-md:border-0 max-md:shadow-none md:min-h-[calc(100dvh-1.5rem)] md:rounded-[1.25rem]"
  >
    <Backdrop
      texture={p.texture}
      ink={p.focusPalette?.background}
      class="-z-1 [mask-image:radial-gradient(ellipse_at_center,transparent_30%,black_85%)] opacity-40"
    />
    <header
      class="flex h-16 shrink-0 items-center justify-between px-5 md:px-8"
    >
      <Logo class="text-fg-primary h-5.5" aria-label="Internet Identity" />
      {#if !empty}
        <EditToggle {p} />
      {/if}
    </header>

    <main
      class="flex flex-1 flex-col items-center justify-center px-4 pt-6 pb-12"
    >
      {#if empty}
        <div data-arrive class="mb-8 flex" aria-hidden="true">
          {#each [0, 1, 2] as slot (slot)}
            <span
              class={[
                "border-border-primary grid size-18 place-items-center rounded-full border-[1.5px] border-dashed",
                slot > 0 && "-ms-5",
              ]}
              style:opacity={1 - slot * 0.3}
            ></span>
          {/each}
        </div>
      {/if}
      <div data-arrive class="text-center">
        <h1
          class="text-text-primary text-2xl leading-8 font-semibold tracking-tight md:text-[2rem] md:leading-10"
        >
          {empty
            ? "Sign in to Internet Identity"
            : single
              ? "Welcome back"
              : "Choose an identity"}
        </h1>
        <p class="text-text-tertiary mt-2 text-base">
          {empty
            ? "No identities on this device yet."
            : "Identities on this device"}
        </p>
      </div>

      {#if empty}
        <div data-arrive class="mt-10 w-full max-w-[25rem]">
          <SignInMethods onPick={p.useMethod} />
        </div>
      {:else}
        <ul
          data-arrive
          aria-label="Identities on this device"
          class="mt-10 flex max-w-[60rem] flex-wrap justify-center gap-x-2 gap-y-4 md:gap-x-4"
        >
          {#each p.identities as one (one.identityNumber)}
            {@const pending = p.pending === one.identityNumber}
            {@const recent =
              p.lastUsed &&
              p.identities.length > 1 &&
              p.recent?.identityNumber === one.identityNumber}
            <li
              class="relative"
              out:scale={{
                duration: prefersReducedMotion.current ? 0 : 180,
                start: 0.9,
                easing: quintOut,
              }}
            >
              <button
                type="button"
                class={TILE}
                onclick={() => p.signIn(one)}
                onpointerenter={() => (p.hovered = one.identityNumber)}
                onpointerleave={() => (p.hovered = undefined)}
                onfocus={() => (p.hovered = one.identityNumber)}
                onblur={() => (p.hovered = undefined)}
                disabled={p.editing}
                aria-busy={pending}
              >
                <span class={LIFT}>
                  <BigAvatar identity={one} size={88} badge />
                  {#if pending}
                    <span
                      class="absolute -inset-2"
                      transition:fade={{ duration: 150 }}
                    >
                      <ProgressRing
                        class="text-fg-primary size-full"
                        strokeWidth={2}
                      />
                    </span>
                  {/if}
                </span>
                <span class="flex max-w-full flex-col items-center gap-1">
                  <span
                    class="text-text-primary max-w-full truncate text-base font-medium"
                  >
                    {nameOf(one)}
                  </span>
                  {#if recent}
                    <LastUsed />
                  {:else}
                    <span class="text-text-tertiary h-5 text-xs leading-5">
                      {methodOf(one)}
                    </span>
                  {/if}
                </span>
              </button>
              {#if p.editing}
                <button
                  type="button"
                  onclick={() => p.remove(one)}
                  aria-label={`Remove ${nameOf(one)} from this device`}
                  title="Remove from this device"
                  class="text-fg-primary focus-visible:ring-focus-ring absolute start-4 top-3 grid size-7 place-items-center rounded-full bg-(--ii-top) shadow-(--ii-shadow-3) outline-none focus-visible:ring-2 sm:start-6"
                  transition:scale={{
                    duration: prefersReducedMotion.current ? 0 : 160,
                    start: 0.6,
                    easing: quintOut,
                  }}
                >
                  <MinusIcon class="size-4" strokeWidth={2.5} />
                </button>
              {/if}
            </li>
          {/each}
          {#if !p.editing}
            <li>
              <button
                type="button"
                class={TILE}
                onclick={() => (p.methodsOpen = true)}
              >
                <span
                  class={[
                    LIFT,
                    "border-border-primary text-fg-tertiary grid size-22 place-items-center rounded-full border-[1.5px] border-dashed",
                  ]}
                >
                  <PlusIcon class="size-6" />
                </span>
                <span class="flex flex-col items-center gap-1">
                  <span class="text-text-primary text-base font-medium">
                    Add identity
                  </span>
                  <span class="text-text-tertiary h-5 text-xs leading-5">
                    Passkey or account
                  </span>
                </span>
              </button>
            </li>
          {/if}
        </ul>
      {/if}
    </main>
    <Footer />
  </Elevated>
</div>
