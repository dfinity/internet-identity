<!--
  2 · Centered. One column in the middle of the window, a card holding the
  identities, the way most sign-in pages choose an account. A texture, if
  one is picked, stays faint behind it and fades out towards the card.
-->
<script lang="ts">
  import Footer from "$lib/components/layout/Footer.svelte";
  import Logo from "$lib/components/ui/Logo.svelte";
  import Elevated from "../../foundation/surfaces/Elevated.svelte";
  import type { Playground } from "../playground.svelte";
  import Backdrop from "../parts/Backdrop.svelte";
  import IdentityRows from "../parts/IdentityRows.svelte";
  import SignInMethods from "../parts/SignInMethods.svelte";
  import EditToggle from "../parts/EditToggle.svelte";

  const { p }: { p: Playground } = $props();

  const empty = $derived(p.identities.length === 0);
  const single = $derived(p.identities.length === 1);
</script>

<div class="relative isolate flex min-h-dvh flex-col bg-(--ii-page)">
  <Backdrop
    texture={p.texture}
    class="text-fg-quaternary -z-1 [mask-image:radial-gradient(ellipse_at_center,transparent_22%,black_75%)] opacity-50"
  />
  <main
    class="flex flex-1 flex-col items-center justify-center px-4 pt-16 pb-10"
  >
    <div class="flex w-full max-w-[25rem] flex-col items-stretch">
      <div data-arrive class="flex flex-col items-center text-center">
        <Logo class="text-fg-primary mb-8 h-6" aria-label="Internet Identity" />
        <h1
          class="text-text-primary text-2xl leading-8 font-semibold tracking-tight text-balance"
        >
          {empty
            ? "Sign in to Internet Identity"
            : single
              ? "Welcome back"
              : "Choose an identity"}
        </h1>
        <p class="text-text-tertiary mt-2 text-sm text-balance">
          {empty
            ? "Use a passkey, or an account you already have. New here? The same buttons create your identity."
            : "to continue to Internet Identity"}
        </p>
      </div>

      <div data-arrive class="mt-8">
        {#if empty}
          <Elevated offset={2} class="rounded-3xl p-5">
            <SignInMethods onPick={p.useMethod} />
          </Elevated>
        {:else}
          <Elevated offset={2} class="overflow-hidden rounded-3xl">
            <IdentityRows {p} />
          </Elevated>
        {/if}
      </div>
      {#if !empty}
        <div data-arrive class="mt-3 flex justify-center">
          <EditToggle {p} />
        </div>
      {/if}
    </div>
  </main>
  <Footer />
</div>
