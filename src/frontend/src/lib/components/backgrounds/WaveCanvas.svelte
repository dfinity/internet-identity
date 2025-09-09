<script lang="ts">
  import FlairCanvas from "./FlairCanvas.svelte";
  import { isMobile } from "$lib/state/UI/isMobile";
  import { onMount, onDestroy } from "svelte";
  import {
    registerAnimationTrigger,
    unregisterAnimationTrigger,
  } from "$lib/utils/animation-dispatcher";

  let {
    triggerAnimation = $bindable(),
    clearAnimation = $bindable<() => void>(),
  } = $props();

  onMount(() => {
    if (triggerAnimation) {
      registerAnimationTrigger(triggerAnimation, clearAnimation);
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
    maskWaveRampOut={0.5}
    maskWaveThickness="large"
    maskWaveMinValue={0}
    maskWaveSpeedMultiplier={2}
    maskWavePauseValue={0.25}
    maskWaveOneWay={true}
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
    bind:clearAnimation
  />
{/if}
