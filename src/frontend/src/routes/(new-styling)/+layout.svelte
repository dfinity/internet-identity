<script lang="ts">
  // Enable new styles only in the new layout pages.
  import style from "$lib/app.css?url";
  import WaveCanvas from "$lib/components/backgrounds/WaveCanvas.svelte";
  import Toaster from "$lib/components/utils/Toaster.svelte";
  import type { LayoutProps } from "./$types";

  const { children }: LayoutProps = $props();
</script>

<svelte:head>
  <link rel="stylesheet" href={style} />
</svelte:head>

{@render children()}
<!-- Hide global wave canvas on mobile -->
<div class="max-md:hidden">
  <WaveCanvas />
</div>
<Toaster />
