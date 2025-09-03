<script lang="ts">
  import { fade } from "svelte/transition";
  import { tick } from "svelte";
  import { onMount } from "svelte";

  const {
    texts = [],
    duration = 1000,
    delay = 2000,
    textClass = "",
    containerClass = "",
  } = $props();

  let index = $state(0);
  let currentText = $state(texts[0]);

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
    <p transition:fade={{ duration }} class={`absolute ${textClass}`}>
      {currentText}
    </p>
  {/key}
</div>
