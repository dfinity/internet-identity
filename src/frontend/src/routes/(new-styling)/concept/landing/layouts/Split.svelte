<!--
  3 · Split. Half the window to choose an identity, half to the brand: a
  framed panel, as the dashboard frames its canvas, with a texture and what
  Internet Identity stands for. In "Identity colour" the panel takes the
  colours of the identity pointed at (the most recent one otherwise), so
  the brand side answers the choice. On a phone the panel is a band over
  the list.
-->
<script lang="ts">
  import Footer from "$lib/components/layout/Footer.svelte";
  import Logo from "$lib/components/ui/Logo.svelte";
  import Elevated from "../../foundation/surfaces/Elevated.svelte";
  import type { Playground } from "../playground.svelte";
  import Backdrop from "../parts/Backdrop.svelte";
  import Brand from "../parts/Brand.svelte";
  import EditToggle from "../parts/EditToggle.svelte";
  import IdentityRows from "../parts/IdentityRows.svelte";
  import SignInMethods from "../parts/SignInMethods.svelte";

  const { p }: { p: Playground } = $props();

  const empty = $derived(p.identities.length === 0);
  const single = $derived(p.identities.length === 1);
  const palette = $derived(p.tone === "identity" ? p.focusPalette : undefined);
</script>

<div class="flex min-h-dvh bg-(--ii-page) max-md:flex-col">
  <section class="flex min-w-0 flex-col md:min-h-dvh md:basis-1/2">
    <header
      class="flex h-16 shrink-0 items-center px-6 max-md:hidden md:px-10 lg:px-14"
    >
      <Logo class="text-fg-primary h-5.5" aria-label="Internet Identity" />
    </header>
    <div
      class="flex flex-1 flex-col justify-center px-5 py-8 md:px-10 md:py-10 lg:px-14"
    >
      <div class="w-full max-w-[26rem]">
        <div data-arrive>
          <h1
            class="text-text-primary text-2xl leading-8 font-semibold tracking-tight text-balance md:text-[1.75rem] md:leading-9"
          >
            {empty
              ? "Sign in to Internet Identity"
              : single
                ? "Welcome back"
                : "Choose an identity"}
          </h1>
          <p class="text-text-tertiary mt-2 text-base text-balance">
            {empty
              ? "Use a passkey, or an account you already have. New here? The same buttons create your identity."
              : "to continue to Internet Identity"}
          </p>
        </div>
        <div data-arrive class="mt-8">
          {#if empty}
            <SignInMethods onPick={p.useMethod} />
          {:else}
            <Elevated offset={2} class="overflow-hidden rounded-3xl">
              <IdentityRows {p} />
            </Elevated>
            <EditToggle {p} class="-ms-3 mt-3" />
          {/if}
        </div>
      </div>
    </div>
    <Footer class="max-md:hidden md:px-10 lg:px-14" />
  </section>

  <aside
    data-arrive
    class="p-2 max-md:order-first max-md:h-[min(42svh,22rem)] md:sticky md:top-0 md:h-dvh md:basis-1/2 md:p-3 md:ps-0"
  >
    <Elevated
      offset={2}
      class="relative isolate h-full overflow-hidden rounded-[1.25rem] transition-[background-color,color] duration-300 ease-(--ii-ease-out) motion-reduce:transition-none"
      style={palette === undefined
        ? undefined
        : `background-color: ${palette.background}; color: ${palette.foreground}`}
    >
      <Backdrop
        texture={p.texture}
        ink={palette?.foreground}
        class={[
          "-z-1 [mask-image:linear-gradient(to_bottom,black_25%,transparent_80%)] max-md:[mask-image:linear-gradient(to_bottom,black,transparent_85%)]",
          palette === undefined
            ? "text-fg-quaternary opacity-50 dark:opacity-75"
            : "opacity-30",
        ]}
      />
      <!-- A phone: the logo and a smaller line over the band. -->
      <div class="flex h-full flex-col justify-between p-5 md:hidden">
        <Logo
          class={["h-5 self-start", palette === undefined && "text-fg-primary"]}
          aria-label="Internet Identity"
        />
        <Brand brand={p.brand} tinted={palette !== undefined} scale="band" />
      </div>
      <div class="flex h-full flex-col justify-end p-10 max-md:hidden lg:p-14">
        <Brand brand={p.brand} tinted={palette !== undefined} />
      </div>
    </Elevated>
  </aside>
  <Footer class="md:hidden" />
</div>
