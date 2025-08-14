<script lang="ts">
  import FlairCanvas from "./FlairCanvas.svelte";
  import { isMobile } from "$lib/state/UI/isMobile";
  import { onMount, onDestroy } from "svelte";
  import {
    registerAnimationTrigger,
    unregisterAnimationTrigger,
  } from "$lib/utils/animation-dispatcher";

  let { triggerAnimation = $bindable() } = $props();

  onMount(() => {
    if (triggerAnimation) {
      registerAnimationTrigger(triggerAnimation);
    }
  });

  onDestroy(() => {
    unregisterAnimationTrigger();
  });
</script>

{#if !$isMobile}
  <FlairCanvas
    spacing="medium"
    aspect="ultrawide"
    dotSize="small"
    vignette="none"
    visibility="maskwave"
    maskWaveRampIn={0.001}
    maskWaveRampOut={0.12}
    maskWaveThickness="large"
    maskWaveMinValue={0}
    maskWaveSpeedMultiplier={4}
    enableRandomPointSize
    enableRandomOpacity={false}
    pointSizeNoiseScale="medium"
    pointSizeNoiseMultiplier="medium"
    springOrTween={{
      type: "spring",
      stiffness: "medium",
      dampening: "medium",
    }}
    bind:triggerAnimation
  />
{/if}
