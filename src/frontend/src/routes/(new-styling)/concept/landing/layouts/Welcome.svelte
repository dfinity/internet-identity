<!--
  4 · Welcome back. The identity used last is the page: the concept's
  greeting, its avatar and name as large as a title, one Continue. The
  others wait in the stack the dashboard's identity card uses, and open into
  its list (`IdentityStack`, `IdentityList`, the avatars travelling between
  them). A texture, if picked, takes the identity's colour on the far side.
-->
<script lang="ts">
  import { ArrowRightIcon, ChevronDownIcon, PlusIcon } from "@lucide/svelte";
  import Footer from "$lib/components/layout/Footer.svelte";
  import Logo from "$lib/components/ui/Logo.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import IdentityList from "../../features/identity/IdentityList.svelte";
  import IdentityStack from "../../features/identity/IdentityStack.svelte";
  import { travel } from "../../foundation/motion/motion";
  import Elevated from "../../foundation/surfaces/Elevated.svelte";
  import { agoOf, greeting, methodOf, nameOf } from "../mock";
  import type { Playground } from "../playground.svelte";
  import Backdrop from "../parts/Backdrop.svelte";
  import BigAvatar from "../parts/BigAvatar.svelte";
  import LastUsed from "../parts/LastUsed.svelte";
  import SignInMethods from "../parts/SignInMethods.svelte";

  const { p }: { p: Playground } = $props();

  const pair = travel();
  let expanded = $state(false);

  const hero = $derived(p.recent);
  const others = $derived(p.identities.slice(1));
  const pending = $derived(
    hero !== undefined && p.pending === hero.identityNumber,
  );

  const PRESS =
    "transition-[scale] duration-160 ease-out active:scale-[0.97] motion-reduce:active:scale-100";
</script>

<div class="flex min-h-dvh flex-col bg-(--ii-page) md:p-3">
  <Elevated
    offset={2}
    class="relative isolate flex min-h-dvh flex-1 flex-col overflow-hidden max-md:rounded-none max-md:border-0 max-md:shadow-none md:min-h-[calc(100dvh-1.5rem)] md:rounded-[1.25rem]"
  >
    <Backdrop
      texture={p.texture}
      ink={p.focusPalette?.background}
      class="-z-1 [mask-image:linear-gradient(to_right,transparent_35%,black_90%)] opacity-70 max-md:[mask-image:linear-gradient(to_top,black,transparent_45%)]"
    />
    <header class="flex h-16 shrink-0 items-center px-5 md:px-8">
      <Logo class="text-fg-primary h-5.5" aria-label="Internet Identity" />
    </header>

    <!-- From a fixed line, not centred: the list opening below only pushes
         down, and the greeting stays where it was. -->
    <main
      class="flex flex-1 flex-col px-6 pt-10 pb-16 md:px-16 md:pt-[max(4rem,19vh)] lg:px-24"
    >
      {#if hero === undefined}
        <div class="max-w-[34rem]">
          <p data-arrive class="text-text-tertiary text-lg md:text-xl">
            {greeting()}.
          </p>
          <h1
            data-arrive
            class="text-text-primary mt-3 text-[2.75rem] leading-[1.02] font-semibold tracking-[-0.04em] text-balance md:text-[4.5rem]"
          >
            Let's get you signed in.
          </h1>
          <div data-arrive class="mt-10 max-w-[25rem]">
            <SignInMethods onPick={p.useMethod} />
          </div>
        </div>
      {:else}
        <div class="flex max-w-[44rem] min-w-0 flex-col">
          <p data-arrive class="text-text-tertiary text-lg md:text-xl">
            {greeting()},
          </p>
          <div
            data-arrive
            class="mt-4 flex min-w-0 items-center gap-4 md:mt-5 md:gap-6"
          >
            <BigAvatar identity={hero} size={64} badge class="md:hidden" />
            <BigAvatar identity={hero} size={96} badge class="max-md:hidden" />
            <h1
              class="text-text-primary min-w-0 truncate pb-[0.08em] text-[3.25rem] leading-[1.05] font-semibold tracking-[-0.045em] md:text-[5.5rem]"
            >
              {nameOf(hero)}
            </h1>
          </div>
          <p
            data-arrive
            class="text-text-tertiary mt-4 flex flex-wrap items-center gap-x-2 gap-y-1 text-base"
          >
            <span>{methodOf(hero)}</span>
            {#if p.lastUsed && others.length > 0}
              <LastUsed />
            {:else}
              <span aria-hidden="true">·</span>
              <span>used {agoOf(hero)}</span>
            {/if}
          </p>
          <div data-arrive class="mt-8">
            <button
              type="button"
              onclick={() => p.signIn(hero)}
              aria-busy={pending}
              class={[
                "btn btn-primary btn-xl h-14 gap-2 rounded-full px-7 font-medium",
                PRESS,
              ]}
            >
              Continue
              {#if pending}
                <ProgressRing class="size-4" />
              {:else}
                <ArrowRightIcon class="size-4 rtl:-scale-x-100" />
              {/if}
            </button>
          </div>

          <div data-arrive class="mt-14 flex flex-col items-start">
            {#if others.length > 0}
              <!-- As on the dashboard's identity card, each identity is on
                   screen once: opening the list, the avatars leave the
                   stack for their rows. -->
              <button
                type="button"
                onclick={() => (expanded = !expanded)}
                aria-expanded={expanded}
                aria-controls="welcome-others"
                class="focus-visible:ring-focus-ring -ms-3 flex h-10 items-center gap-2 rounded-full ps-3 pe-2.5 transition-colors duration-150 outline-none hover:bg-(--ii-hover) focus-visible:ring-2"
              >
                <span class="text-text-tertiary text-sm">
                  Not {nameOf(hero)}?
                </span>
                {#if !expanded}
                  <IdentityStack
                    {others}
                    travel={pair}
                    backdrop="var(--ii-canvas)"
                  />
                {/if}
                <ChevronDownIcon
                  aria-hidden="true"
                  class={[
                    "text-fg-quaternary size-4 transition-transform duration-200 ease-out motion-reduce:transition-none",
                    expanded && "rotate-180",
                  ]}
                />
              </button>
              {#if expanded}
                <Elevated
                  offset={1}
                  class="mt-3 w-full max-w-[22rem] overflow-hidden rounded-2xl"
                >
                  <IdentityList
                    id="welcome-others"
                    identity={hero}
                    signedInWith={methodOf(hero)}
                    {others}
                    withCurrent={false}
                    travel={pair}
                    reveal
                    onSwitch={(identityNumber) => {
                      const one = others.find(
                        (other) => other.identityNumber === identityNumber,
                      );
                      if (one !== undefined) {
                        p.signIn(one);
                      }
                    }}
                    onAdd={() => (p.methodsOpen = true)}
                  />
                </Elevated>
              {/if}
            {:else}
              <button
                type="button"
                onclick={() => (p.methodsOpen = true)}
                class="btn btn-tertiary text-text-secondary -ms-3 gap-2 rounded-full font-medium"
              >
                <PlusIcon class="size-4" />
                Use another identity
              </button>
            {/if}
          </div>
        </div>
      {/if}
    </main>
    <Footer />
  </Elevated>
</div>
