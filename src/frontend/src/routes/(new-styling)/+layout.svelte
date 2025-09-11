<script lang="ts">
  // Enable new styles only in the new layout pages.
  import style from "$lib/app.css?url";
  import WaveCanvas from "$lib/components/backgrounds/WaveCanvas.svelte";
  import Toaster from "$lib/components/utils/Toaster.svelte";
  import type { LayoutProps } from "./$types";
  import { loadTranslations, locale } from "$lib/utils/translations";
  import { onMount } from "svelte";

  const { children }: LayoutProps = $props();

  onMount(async () => {
    const initLocale = "en";

    await loadTranslations(initLocale, "t");
  });
</script>

<svelte:head>
  <link rel="stylesheet" href={style} />
</svelte:head>

{@render children()}
<WaveCanvas />
<Toaster />
