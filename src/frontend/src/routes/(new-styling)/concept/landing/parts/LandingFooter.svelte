<!--
  The product's footer (`Footer`) for a layout that puts About in its
  header: the language, Support and Source code, and before the copyright
  the flask that opens the playground's controls (not in a phone's or the
  overview's frame, which have none). It stays on the start side, clear of
  the badge Netlify puts in the bottom corner of the published concept.
  Same type and spacing as the product's, so the two read alike.
-->
<script lang="ts">
  import { FlaskConicalIcon } from "@lucide/svelte";
  import type { ClassValue } from "svelte/elements";
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import ChooseLanguage from "$lib/components/views/ChooseLanguage.svelte";
  import { SOURCE_CODE_URL, SUPPORT_URL } from "$lib/config";
  import { locales, localeStore } from "$lib/stores/locale.store";
  import type { Playground } from "../playground.svelte";

  const { p, class: className }: { p: Playground; class?: ClassValue } =
    $props();

  let languageOpen = $state(false);
</script>

<footer
  class={[
    "flex h-14 items-center justify-between px-4 py-3 md:px-6 lg:px-8",
    className,
  ]}
>
  <div class="flex items-center gap-3">
    {#if !p.framed}
      <button
        type="button"
        onclick={() => (p.controls = !p.controls)}
        aria-expanded={p.controls}
        aria-label="Playground controls"
        title={`${p.controls ? "Hide" : "Show"} the controls (H)`}
        class={[
          "focus-visible:ring-focus-ring hover:text-fg-primary -m-1.5 grid size-7 place-items-center rounded-full transition-colors duration-150 outline-none hover:bg-(--ii-hover) focus-visible:ring-2",
          p.controls ? "text-fg-primary" : "text-fg-quaternary",
        ]}
      >
        <FlaskConicalIcon class="size-4" aria-hidden="true" />
      </button>
    {/if}
    <span class="text-text-tertiary text-xs font-medium">
      © Internet Identity
    </span>
  </div>
  <nav class="text-text-primary flex items-center gap-4 text-xs font-semibold">
    <button
      type="button"
      onclick={() => (languageOpen = true)}
      class="uppercase outline-0 focus-visible:underline"
    >
      {$localeStore}
    </button>
    <a
      href={SUPPORT_URL}
      target="_blank"
      rel="noopener"
      class="outline-0 focus-visible:underline"
    >
      Support
    </a>
    <a
      href={SOURCE_CODE_URL}
      target="_blank"
      rel="noopener"
      class="outline-0 focus-visible:underline"
    >
      Source code
    </a>
  </nav>
</footer>

{#if languageOpen}
  <Dialog onClose={() => (languageOpen = false)}>
    <ChooseLanguage
      locales={$locales}
      value={$localeStore}
      onChange={(value) => {
        languageOpen = false;
        void localeStore.setOrReset(value);
      }}
    />
  </Dialog>
{/if}
