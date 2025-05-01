<script lang="ts">
  // Enable new styles only in the new layout pages.
  import "$lib/app.css";
  import { Toaster } from "@skeletonlabs/skeleton-svelte";
  import { toaster } from "$lib/utils/toaster";
  import { LayoutProps } from "./$types";

  const { children }: LayoutProps = $props();
</script>

{@render children()}
<Toaster {toaster}></Toaster>
