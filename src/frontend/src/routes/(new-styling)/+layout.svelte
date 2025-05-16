<script lang="ts">
  // Enable new styles only in the new layout pages.
  import style from "$lib/app.css?url";
  import Toaster from "$lib/components/utils/Toaster.svelte";
  import type { LayoutProps } from "./$types";

  const { children }: LayoutProps = $props();
</script>

<svelte:head>
  <link rel="stylesheet" href={style} />
</svelte:head>

{@render children()}
<Toaster />
