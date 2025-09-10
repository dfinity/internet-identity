<script lang="ts">
  import { fade } from "svelte/transition";
  import { tick } from "svelte";
  import { onMount } from "svelte";

  const {
    texts = [],
    duration = 1000,
    delayBetween = 2000,
    startDelay = 0,
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

    const start = setTimeout(() => {
      interval = setInterval(next, delayBetween + duration);
      next();
    }, startDelay);

    return () => {
      clearTimeout(start);
      clearInterval(interval);
    };
  });
</script>

<div class={`relative ${containerClass}`}>
  {#key currentText}
    <p transition:fade={{ duration }} class={`absolute ${textClass}`}>
      {currentText}
    </p>
  {/key}
</div>
