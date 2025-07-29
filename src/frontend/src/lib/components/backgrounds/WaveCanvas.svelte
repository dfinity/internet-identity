<script lang="ts">
  import FlairCanvas from "./FlairCanvas.svelte";
  import { FLAIR } from "$lib/state/featureFlags";
  import { isMobile } from "$lib/state/UI/isMobile";
  import { onMount, onDestroy } from "svelte";
  import {
    registerWaveAnimationTrigger,
    unregisterWaveAnimationTrigger,
  } from "$lib/utils/wave-animation";

  let { triggerAnimation = $bindable() } = $props();

  onMount(() => {
    if (triggerAnimation) {
      registerWaveAnimationTrigger(triggerAnimation);
    }
  });

  onDestroy(() => {
    console.log("Unregistering wave animation trigger");
    unregisterWaveAnimationTrigger();
  });
</script>

{#if $FLAIR && !$isMobile}
  <FlairCanvas
    spacing="medium"
    aspect="ultrawide"
    dotSize={1.2}
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
