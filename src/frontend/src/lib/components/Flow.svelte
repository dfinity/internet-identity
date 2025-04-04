<script lang="ts" generics="T extends unknown[], S extends unknown">
  import { onMount } from "svelte";
  import "$lib/legacy/styles/main.css";

  interface Props {
    promise: (...args: T) => Promise<S>;
    args: T;
    onResolve?: (result: S) => void;
  }

  const { promise, args, onResolve }: Props = $props();
  onMount(async () => {
    const value = await promise(...args);
    onResolve?.(value);
  });
</script>

<main id="pageContent" aria-live="polite"></main>
<div id="loaderContainer"></div>
