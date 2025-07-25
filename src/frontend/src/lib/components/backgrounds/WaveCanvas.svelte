<script lang="ts">
  import FlairCanvas from "./FlairCanvas.svelte";
  import { FLAIR } from "$lib/state/featureFlags";

  let { triggerAnimation = $bindable() } = $props();
</script>

{#if $FLAIR}
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
    customColorMode="all"
    customColor="#ECFF57"
    bind:triggerAnimation
  />
{/if}
