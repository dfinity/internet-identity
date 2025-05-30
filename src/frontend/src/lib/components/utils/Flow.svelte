<script lang="ts" generics="T extends unknown[], S extends unknown">
  import { onMount } from "svelte";
  import style from "$lib/legacy/styles/main.css?url";

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

<svelte:head>
  <link rel="stylesheet" href={style} />
</svelte:head>

<main id="pageContent" aria-live="polite"></main>
<div id="loaderContainer"></div>
