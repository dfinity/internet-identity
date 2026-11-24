<!--
  5 · Sentence. The choice is one sentence set large, and the identities are
  its words: "Continue as Thomas, Work or Testing." Each name is a button
  in its avatar's colour. Editorial and quick to read while the list is
  short; "Many" shows where it stops working. With no identity the sentence
  names the ways in instead.
-->
<script lang="ts">
  import { UserIcon, XIcon } from "@lucide/svelte";
  import Footer from "$lib/components/layout/Footer.svelte";
  import Logo from "$lib/components/ui/Logo.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import { identityInitial, identityPalette } from "$lib/utils/identityColors";
  import { nameOf } from "../mock";
  import type { Playground } from "../playground.svelte";
  import Backdrop from "../parts/Backdrop.svelte";
  import EditToggle from "../parts/EditToggle.svelte";
  import LastUsed from "../parts/LastUsed.svelte";
  import ProviderLogo, { type Provider } from "../parts/ProviderLogo.svelte";

  const { p }: { p: Playground } = $props();

  const empty = $derived(p.identities.length === 0);

  /** ", " between words, " or " before the last. */
  const joint = (index: number, count: number): string =>
    index === count - 1 ? "" : index === count - 2 ? " or " : ", ";

  const WAYS: { id: Provider; label: string }[] = [
    { id: "passkey", label: "a passkey" },
    { id: "google", label: "Google" },
    { id: "apple", label: "Apple" },
    { id: "microsoft", label: "Microsoft" },
  ];

  /** An inline-block, so it stands on the sentence's baseline, its text
   *  the line's own size. */
  const CHIP =
    "relative inline-block rounded-full bg-(--ii-hover) py-[0.04em] ps-[0.12em] pe-[0.4em] align-baseline leading-[1.12] whitespace-nowrap outline-none transition-[background-color,scale] duration-160 ease-out hover:bg-(--ii-selected) focus-visible:ring-[3px] focus-visible:ring-focus-ring active:scale-[0.97] motion-reduce:active:scale-100";
  /** The avatar or mark at the start of a chip: as tall as a capital, on
   *  the line; the word space after it is the gap. */
  const MARK =
    "inline-grid size-[0.74em] place-items-center rounded-full align-[-0.04em]";
</script>

<div class="relative isolate flex min-h-dvh flex-col bg-(--ii-canvas)">
  <Backdrop
    texture={p.texture}
    ink={p.focusPalette?.background}
    class="text-fg-quaternary -z-1 [mask-image:linear-gradient(to_top,black,transparent_55%)] opacity-60"
  />
  <header
    class="flex h-16 shrink-0 items-center justify-between px-6 md:px-12 lg:px-20"
  >
    <Logo class="text-fg-primary h-5.5" aria-label="Internet Identity" />
    {#if !empty}
      <EditToggle {p} />
    {/if}
  </header>

  <main
    class="flex flex-1 flex-col justify-center px-6 pt-8 pb-16 md:px-12 lg:px-20"
  >
    <h1
      data-arrive
      class="text-text-primary max-w-[16em] text-[2.25rem] leading-[1.32] font-semibold tracking-[-0.035em] text-pretty md:text-[3.5rem] xl:text-[4.25rem]"
    >
      {#if empty}
        Sign in with
        {#each WAYS as way, index (way.id)}
          <button
            type="button"
            class={CHIP}
            onclick={() => p.useMethod(`Continue with ${way.label}`)}
          >
            <span class={[MARK, "bg-(--ii-top) shadow-(--ii-shadow-2)"]}>
              <ProviderLogo
                provider={way.id}
                class="text-fg-primary size-[0.4em]!"
              />
            </span>
            {way.label}</button
          >{joint(index, WAYS.length)}
        {/each}.
      {:else}
        Continue as
        {#each p.identities as one, index (one.identityNumber)}
          {@const palette = identityPalette(one.identityNumber)}
          {@const initial = identityInitial(one.name)}
          {@const pending = p.pending === one.identityNumber}
          {@const recent =
            p.lastUsed &&
            p.identities.length > 1 &&
            p.recent?.identityNumber === one.identityNumber}
          <span class="whitespace-nowrap"
            ><button
              type="button"
              class={CHIP}
              onclick={() => p.signIn(one)}
              onpointerenter={() => (p.hovered = one.identityNumber)}
              onpointerleave={() => (p.hovered = undefined)}
              onfocus={() => (p.hovered = one.identityNumber)}
              onblur={() => (p.hovered = undefined)}
              disabled={p.editing}
              aria-busy={pending}
            >
              <span
                aria-hidden="true"
                class={[
                  MARK,
                  "outline-1 -outline-offset-1 outline-black/8 dark:outline-white/12",
                ]}
                style:background-color={palette.background}
                style:color={palette.foreground}
              >
                {#if pending}
                  <ProgressRing class="size-[0.5em]" />
                {:else if initial !== undefined}
                  <span class="text-[0.36em] leading-none tracking-normal">
                    {initial}
                  </span>
                {:else}
                  <UserIcon class="size-[0.45em]" />
                {/if}
              </span>
              {nameOf(one)}
              {#if recent}
                <LastUsed
                  class="text-text-tertiary ms-[0.15em] align-[0.35em] tracking-normal"
                />
              {/if}
            </button>{#if p.editing}<button
                type="button"
                onclick={() => p.remove(one)}
                aria-label={`Remove ${nameOf(one)} from this device`}
                title="Remove from this device"
                class="text-fg-tertiary hover:text-fg-primary focus-visible:ring-focus-ring ms-[0.1em] inline-grid size-[0.7em] place-items-center rounded-full bg-(--ii-hover) align-[0.05em] outline-none focus-visible:ring-2"
                ><XIcon class="size-[0.4em]" strokeWidth={2.5} /></button
              >{/if}</span
          >{joint(index, p.identities.length)}
        {/each}.
      {/if}
    </h1>
    <p
      data-arrive
      class="text-text-tertiary mt-8 max-w-[36rem] text-lg text-pretty md:text-xl"
    >
      {#if empty}
        No identity yet? The same choices create one. At work, use your
        organization's
        <button
          type="button"
          class="text-text-primary font-medium underline decoration-1 underline-offset-4 hover:decoration-2"
          onclick={() => p.useMethod("Continue with SSO")}
          >single sign-on</button
        >.
      {:else}
        Or
        <button
          type="button"
          class="text-text-primary font-medium underline decoration-1 underline-offset-4 hover:decoration-2"
          onclick={() => (p.methodsOpen = true)}>add another identity</button
        >.
      {/if}
    </p>
  </main>
  <Footer class="md:px-12 lg:px-20" />
</div>
