<script lang="ts">
  import FlairCanvas from "./FlairCanvas.svelte";

  let { triggerAnimation = $bindable() } = $props();
</script>

<!-- missing: mask wave speed multiplier -->
<FlairCanvas
  spacing="medium"
  aspect="ultrawide"
  dotSize={1.1}
  vignette="none"
  maskWaveRampIn={0.001}
  maskWaveRampOut={0.12}
  maskWaveThickness="large"
  maskWaveMinValue={0}
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
  bind:triggerAnimation
/>
