<script lang="ts">
  import { fade } from "svelte/transition";
  import { tick } from "svelte";
  import { onMount } from "svelte";

  export let texts: string[] = [];
  export let duration: number = 1000;
  export let delay: number = 2000;
  export let className: string = "";
  export let containerClass: string = "";

  let index = 0;
  let currentText = texts[0];

  const next = async () => {
    if (index < texts.length - 1) {
      await tick();
      index += 1;
      currentText = texts[index];
    }
  };

  let interval: NodeJS.Timeout;

  onMount(() => {
    if (texts.length <= 1) return;
    interval = setInterval(next, delay + duration);
    return () => clearInterval(interval);
  });
</script>

<div class={`relative ${containerClass}`}>
  {#key currentText}
    <p transition:fade={{ duration }} class={`absolute ${className}`}>
      {currentText}
    </p>
  {/key}
</div>
