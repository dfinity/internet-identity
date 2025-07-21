<script lang="ts">
  import FlairBox, {
    type FlairAnimationOptions,
  } from "$lib/components/backgrounds/FlairBox.svelte";
  import { onDestroy, onMount } from "svelte";

  let flairBoxRef = $state();
  let animTrig = $state<(opts: FlairAnimationOptions) => void>();

  // Animation interval (still runs every 3s)
  let interval = $state<ReturnType<typeof setInterval>>();
  onMount(() => {
    interval = setInterval(() => {
      if (animTrig) animTrig(animOptions);
    }, 3000);
  });
  onDestroy(() => clearInterval(interval));

  // Dropdown state
  let showDropdown = $state(false);

  // Animation options state
  let animOptions = $state<FlairAnimationOptions>({
    location: "center",
    target: ["motion", "scale", "opacity"],
    motionType: "omni",
    speed: "slow",
    intensity: "light",
    size: "medium",
  });

  // FlairBox props state
  let flairBoxProps = $state({
    spacing: "medium",
    aspect: "ultrawide",
    dotSize: "small",
    vignette: "none",
    bgType: "dots",
  });

  // Whenever animOptions change, trigger animation immediately
  // $effect(() => {
  //   if (animTrig) animTrig(animOptions);
  // });

  // Helper for multi-select
  function toggleTarget(val: "motion" | "scale" | "opacity") {
    if (animOptions.target.includes(val)) {
      animOptions = {
        ...animOptions,
        target: animOptions.target.filter((t) => t !== val),
      };
    } else {
      animOptions = { ...animOptions, target: [...animOptions.target, val] };
    }
  }
</script>

<!-- Toggle Dropdown Button -->
<button
  onclick={() => (showDropdown = !showDropdown)}
  style="position: absolute; z-index: 10; top: 1rem; right: 1rem; background: white; border-radius: 0.5rem; padding: 0.5rem; box-shadow: 0 2px 8px rgba(0,0,0,0.08);"
>
  ⚙️
</button>

<!-- Dropdown Panel -->
{#if showDropdown}
  <div
    style="
      position: absolute;
      top: 3.5rem;
      right: 1rem;
      z-index: 20;
      background: white;
      border-radius: 0.75rem;
      box-shadow: 0 2px 16px rgba(0,0,0,0.12);
      padding: 1rem;
      min-width: 220px;
      font-size: 0.95rem;
    "
  >
    <div
      style="display: flex; justify-content: space-between; align-items: center;"
    >
      <strong>FlairBox Controls</strong>
      <button
        onclick={() => (showDropdown = false)}
        style="background: none; border: none; font-size: 1.2em;">✕</button
      >
    </div>
    <hr style="margin: 0.5em 0 1em 0;" />

    <div style="display: flex; flex-direction: column; gap: 0.5em;">
      <!-- FlairBox Props -->
      <label>
        Spacing:
        <select bind:value={flairBoxProps.spacing}>
          <option value="small">small</option>
          <option value="medium">medium</option>
          <option value="large">large</option>
        </select>
      </label>
      <label>
        Aspect:
        <select bind:value={flairBoxProps.aspect}>
          <option value="square">square</option>
          <option value="wide">wide</option>
          <option value="ultrawide">ultrawide</option>
        </select>
      </label>
      <label>
        Dot Size:
        <select bind:value={flairBoxProps.dotSize}>
          <option value="small">small</option>
          <option value="medium">medium</option>
          <option value="large">large</option>
        </select>
      </label>
      <label>
        Vignette:
        <select bind:value={flairBoxProps.vignette}>
          <option value="none">none</option>
          <option value="center">center</option>
          <option value="top">top</option>
          <option value="bottom">bottom</option>
          <option value="left">left</option>
          <option value="right">right</option>
        </select>
      </label>
      <label>
        BG Type:
        <select bind:value={flairBoxProps.bgType}>
          <option value="dots">dots</option>
          <option value="grid">grid</option>
          <option value="noisedots">noisedots</option>
        </select>
      </label>
    </div>

    <hr style="margin: 1em 0;" />
    <strong>Impulse Controls</strong>
    <div style="display: flex; flex-direction: column; gap: 0.5em;">
      <!-- Animation Options -->
      <label>
        Location:
        <select bind:value={animOptions.location}>
          <option value="center">center</option>
          <option value="top">top</option>
          <option value="bottom">bottom</option>
          <option value="left">left</option>
          <option value="right">right</option>
          <option value="topLeft">topLeft</option>
          <option value="topRight">topRight</option>
          <option value="bottomLeft">bottomLeft</option>
          <option value="bottomRight">bottomRight</option>
        </select>
      </label>
      <label>
        Motion Type:
        <select bind:value={animOptions.motionType}>
          <option value="omni">omni</option>
          <option value="xy">xy</option>
          <option value="yx">yx</option>
          <option value="up">up</option>
          <option value="down">down</option>
          <option value="left">left</option>
          <option value="right">right</option>
          <option value="cw">cw</option>
          <option value="ccw">ccw</option>
        </select>
      </label>
      <label>
        Speed:
        <select bind:value={animOptions.speed}>
          <option value="slow">slow</option>
          <option value="medium">medium</option>
          <option value="fast">fast</option>
        </select>
      </label>
      <label>
        Intensity:
        <select bind:value={animOptions.intensity}>
          <option value="light">light</option>
          <option value="medium">medium</option>
          <option value="strong">strong</option>
        </select>
      </label>
      <label>
        Size:
        <select bind:value={animOptions.size}>
          <option value="small">small</option>
          <option value="medium">medium</option>
          <option value="large">large</option>
        </select>
      </label>
      <label>
        Target (currently only motion is functional):
        <div style="display: flex; gap: 0.5em;">
          <label
            ><input
              type="checkbox"
              checked={animOptions.target.includes("motion")}
              onchange={() => toggleTarget("motion")}
            /> motion</label
          >
          <label
            ><input
              type="checkbox"
              checked={animOptions.target.includes("scale")}
              onchange={() => toggleTarget("scale")}
            /> scale</label
          >
          <label
            ><input
              type="checkbox"
              checked={animOptions.target.includes("opacity")}
              onchange={() => toggleTarget("opacity")}
            /> opacity</label
          >
        </div>
      </label>
    </div>
  </div>
{/if}

<FlairBox
  bind:this={flairBoxRef}
  spacing={flairBoxProps.spacing}
  aspect={flairBoxProps.aspect}
  dotSize={flairBoxProps.dotSize}
  vignette={flairBoxProps.vignette}
  bgType={flairBoxProps.bgType}
  bind:triggerAnimation={animTrig}
/>
