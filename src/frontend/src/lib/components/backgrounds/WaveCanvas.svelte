<script lang="ts">
  import FlairCanvas from "./FlairCanvas.svelte";
  import { FLAIR } from "$lib/state/featureFlags";
  import { isMobile } from "$lib/state/UI/isMobile";

  let { triggerAnimation = $bindable() } = $props();
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
