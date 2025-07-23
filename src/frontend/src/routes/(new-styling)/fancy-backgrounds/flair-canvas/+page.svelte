<script lang="ts">
  import type { FlairAnimationOptions } from "$lib/components/backgrounds/FlairCanvas";
  import FlairCanvas from "$lib/components/backgrounds/FlairCanvas.svelte";
  import { onDestroy, onMount } from "svelte";
  import * as easingFunctions from "svelte/easing";
  import type {
    FlairAnimationOptions as FlairAnimationOptionsBase,
    FlairCanvasProps as FlairBoxPropsBase,
  } from "$lib/components/backgrounds/FlairCanvas";
  import Panel from "$lib/components/ui/Panel.svelte";
  import ButtonCard from "$lib/components/ui/ButtonCard.svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import Logo from "$lib/components/ui/Logo.svelte";

  let flairCanvasRef = $state();
  let animTrig = $state<(opts: FlairAnimationOptions) => void>();

  // Animation interval for demo purposes
  let interval = $state<ReturnType<typeof setInterval>>();
  onMount(() => {
    if (animTrig)
      animTrig({
        ...(animOptions as FlairAnimationOptions),
        impulseEasing: customImpulseEasing,
      });
    interval = setInterval(() => {
      if (animTrig)
        animTrig({
          ...(animOptions as FlairAnimationOptions),
          impulseEasing: customImpulseEasing,
        });
    }, 3000);
  });
  onDestroy(() => clearInterval(interval));

  let showDropdown = $state(false);

  type CustomOr<T extends string | number> = T | "custom";

  // UI-extended FlairBoxProps
  interface FlairBoxPropsUI
    extends Omit<
      FlairBoxPropsBase,
      "spacing" | "aspect" | "dotSize" | "springOrTween"
    > {
    spacing?: CustomOr<"small" | "medium" | "large" | number>;
    aspect?: CustomOr<"square" | "wide" | "ultrawide" | number>;
    dotSize?: CustomOr<"small" | "medium" | "large" | number>;
    springOrTween?:
      | {
          type: "spring";
          stiffness: CustomOr<"low" | "medium" | "high" | number>;
          dampening: CustomOr<"low" | "medium" | "high" | number>;
        }
      | {
          type: "tween";
          duration: CustomOr<"short" | "medium" | "long" | number>;
          easing: string; // UI uses string, you can map to EasingFunction later
        };
    display: "bgOnly" | "behindBox" | "insideBox";
  }

  // UI-extended FlairAnimationOptions
  interface FlairAnimationOptionsUI
    extends Omit<
      FlairAnimationOptionsBase,
      "speed" | "intensity" | "size" | "location"
    > {
    speed: CustomOr<"slow" | "medium" | "fast" | number>;
    intensity: CustomOr<"light" | "medium" | "strong" | number>;
    size: CustomOr<"small" | "medium" | "large" | number>;
    location:
      | "top"
      | "left"
      | "right"
      | "bottom"
      | "topLeft"
      | "topRight"
      | "bottomLeft"
      | "bottomRight"
      | "center"
      | { x: number; y: number }
      | "custom";
  }

  // Animation options state
  let animOptions = $state<FlairAnimationOptionsUI>({
    location: "center",
    target: ["motion", "scale", "opacity"],
    motionType: "omni",
    speed: "medium",
    intensity: "strong",
    size: "large",
    nImpulses: "double",
  });

  // FlairCanvas props state
  let flairBoxProps = $state<FlairBoxPropsUI>({
    spacing: "medium",
    aspect: "ultrawide",
    dotSize: "small",
    vignette: "none",
    bgType: "dots",
    springOrTween: { type: "spring", stiffness: "medium", dampening: "medium" },
    visibility: "always",
    hoverAction: "none",
    display: "behindBox",
    noiseTimeScale: 1500,
    enableRandomOpacity: false,
    opacityNoiseScale: "medium",
    opacityNoiseMultiplier: "medium",
    enableRandomPointSize: false,
    pointSizeNoiseMultiplier: "medium",
    pointSizeNoiseScale: "medium",

    backgroundClasses: "",
    foregroundClasses: "",
  });

  const easingOptions = Object.keys(easingFunctions);

  // Add state for custom values
  let customSpacing = $state<number>(32);
  let customAspect = $state<number>(2.39);
  let customDotSize = $state<number>(4);
  let customSpringStiffness = $state<number>(100);
  let customSpringDampening = $state<number>(10);
  let customTweenDuration = $state<number>(500);
  let customEasing = $state<string>("linear");
  let customSpeed = $state<number>(1);
  let customIntensity = $state<number>(1);
  let customSize = $state<number>(1);
  let customLocation = $state<{ x: number; y: number }>({ x: 0.5, y: 0.5 });
  let springOrTweenType = $state<"spring" | "tween">("spring");
  let customImpulseEasing = $state<keyof typeof easingFunctions>("cubicIn");

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
        min-width: 260px;
        font-size: 0.95rem;
      "
    class="max-h-[85vh] overflow-scroll"
  >
    <div
      style="display: flex; justify-content: space-between; align-items: center;"
    >
      <strong>FlairCanvas Controls</strong>
      <button
        onclick={() => (showDropdown = false)}
        style="background: none; border: none; font-size: 1.2em;">✕</button
      >
    </div>
    <hr style="margin: 0.5em 0 1em 0;" />

    <div style="display: flex; flex-direction: column; gap: 0.5em;">
      <!-- New FlairCanvas Props -->
      <label>
        Visibility:
        <select bind:value={flairBoxProps.visibility}>
          <option value="always">always</option>
          <option value="moving">moving</option>
          <option value="maskwave">maskwave</option>
        </select>
      </label>
      <label>
        Hover Action:
        <select bind:value={flairBoxProps.hoverAction}>
          <option value="intense">intense</option>
          <option value="minimal">minimal</option>
          <option value="none">none</option>
        </select>
      </label>
      <label>
        Display:
        <select bind:value={flairBoxProps.display}>
          <option value="bgOnly">bgOnly</option>
          <option value="behindBox">behindBox</option>
          <option value="insideBox">insideBox</option>
        </select>
      </label>

      <!-- <label>
        Background Classes:
        <input
          type="text"
          bind:value={flairBoxProps.backgroundClasses}
          placeholder="(optional)"
        />
      </label>
      <label>
        Foreground Classes:
        <input
          type="text"
          bind:value={flairBoxProps.foregroundClasses}
          placeholder="(optional)"
        />
      </label> -->

      <!-- Enum-or-number fields with custom option -->
      <label>
        Spacing:
        <select bind:value={flairBoxProps.spacing}>
          <option value="small">small</option>
          <option value="medium">medium</option>
          <option value="large">large</option>
          <option value="custom">Custom</option>
        </select>
        {#if flairBoxProps.spacing === "custom"}
          <input
            type="number"
            bind:value={customSpacing}
            onchange={() => (flairBoxProps.spacing = customSpacing)}
            min="1"
          />
        {/if}
      </label>
      <label>
        Aspect:
        <select bind:value={flairBoxProps.aspect}>
          <option value="square">square</option>
          <option value="wide">wide</option>
          <option value="ultrawide">ultrawide</option>
          <option value="custom">Custom</option>
        </select>
        {#if flairBoxProps.aspect === "custom"}
          <input
            type="number"
            step="0.01"
            bind:value={customAspect}
            onchange={() => (flairBoxProps.aspect = customAspect)}
            min="0.1"
          />
        {/if}
      </label>
      <label>
        Dot Size:
        <select bind:value={flairBoxProps.dotSize}>
          <option value="small">small</option>
          <option value="medium">medium</option>
          <option value="large">large</option>
          <option value="custom">Custom</option>
        </select>
        {#if flairBoxProps.dotSize === "custom"}
          <input
            type="number"
            bind:value={customDotSize}
            onchange={() => (flairBoxProps.dotSize = customDotSize)}
            min="1"
          />
        {/if}
      </label>
      <!-- Vignette control -->
      <label>
        Vignette:
        <select bind:value={flairBoxProps.vignette}>
          <option value="none">none</option>
          <option value="left">left</option>
          <option value="right">right</option>
          <option value="center">center</option>
          <option value="top">top</option>
          <option value="bottom">bottom</option>
        </select>
      </label>
      <!-- MaskWave controls -->
      <label>
        Mask Wave Ramp In:
        <input
          type="number"
          bind:value={flairBoxProps.maskWaveRampIn}
          min="0"
          step="any"
          placeholder="(optional)"
        />
      </label>
      <label>
        Mask Wave Ramp Out:
        <input
          type="number"
          bind:value={flairBoxProps.maskWaveRampOut}
          min="0"
          step="any"
          placeholder="(optional)"
        />
      </label>
      <label>
        Mask Wave Thickness:
        <select bind:value={flairBoxProps.maskWaveThickness}>
          <option value="large">Large</option>
          <option value="medium">Medium</option>
          <option value="small">Small</option>
          <option value={0}>Custom</option>
        </select>
        {#if typeof flairBoxProps.maskWaveThickness === "number"}
          <input
            type="number"
            bind:value={flairBoxProps.maskWaveThickness}
            min="0"
            step="any"
          />
        {/if}
      </label>
      <label>
        Mask Wave Min Value:
        <input
          type="number"
          bind:value={flairBoxProps.maskWaveMinValue}
          min="0"
          step="any"
          placeholder="(optional)"
        />
      </label>
      <!-- Randomization control -->
      <label>
        <input
          type="checkbox"
          bind:checked={flairBoxProps.enableRandomOpacity}
        />
        Enable Random Opacity
      </label>
      {#if flairBoxProps.enableRandomOpacity}
        <div>
          <label>
            Opacity Noise Scale:
            <select bind:value={flairBoxProps.opacityNoiseScale}>
              <option value="large">Large</option>
              <option value="medium">Medium</option>
              <option value="small">Small</option>
              <option value={0}>Custom</option>
            </select>
            {#if typeof flairBoxProps.opacityNoiseScale === "number"}
              <input
                type="number"
                bind:value={flairBoxProps.opacityNoiseScale}
                min="0"
                step="any"
              />
            {/if}
          </label>
          <label>
            Opacity Noise Multiplier:
            <select bind:value={flairBoxProps.opacityNoiseMultiplier}>
              <option value="large">Large</option>
              <option value="medium">Medium</option>
              <option value="small">Small</option>
              <option value={0}>Custom</option>
            </select>
            {#if typeof flairBoxProps.opacityNoiseMultiplier === "number"}
              <input
                type="number"
                bind:value={flairBoxProps.opacityNoiseMultiplier}
                min="0"
                step="any"
              />
            {/if}
          </label>
        </div>
      {/if}

      <label>
        <input
          type="checkbox"
          bind:checked={flairBoxProps.enableRandomPointSize}
        />
        Enable Random Point Size
      </label>
      {#if flairBoxProps.enableRandomPointSize}
        <div>
          <label>
            Point Size Noise Scale:
            <select bind:value={flairBoxProps.pointSizeNoiseScale}>
              <option value="large">Large</option>
              <option value="medium">Medium</option>
              <option value="small">Small</option>
              <option value={0}>Custom</option>
            </select>
            {#if typeof flairBoxProps.pointSizeNoiseScale === "number"}
              <input
                type="number"
                bind:value={flairBoxProps.pointSizeNoiseScale}
                min="0"
                step="any"
              />
            {/if}
          </label>
          <label>
            Point Size Noise Multiplier:
            <select bind:value={flairBoxProps.pointSizeNoiseMultiplier}>
              <option value="large">Large</option>
              <option value="medium">Medium</option>
              <option value="small">Small</option>
              <option value={0}>Custom</option>
            </select>
            {#if typeof flairBoxProps.pointSizeNoiseMultiplier === "number"}
              <input
                type="number"
                bind:value={flairBoxProps.pointSizeNoiseMultiplier}
                min="0"
                step="any"
              />
            {/if}
          </label>
        </div>
      {/if}

      <!-- springOrTween -->
      <label>
        Spring/Tween:
        <select
          bind:value={springOrTweenType}
          onchange={() => {
            if (springOrTweenType === "spring") {
              flairBoxProps.springOrTween = {
                type: "spring",
                stiffness: "medium",
                dampening: "medium",
              };
            } else {
              flairBoxProps.springOrTween = {
                type: "tween",
                duration: "medium",
                easing: "linear",
              };
            }
          }}
        >
          <option value="spring">spring</option>
          <option value="tween">tween</option>
        </select>
        {#if flairBoxProps.springOrTween?.type === "spring"}
          <div style="margin-left:1em;">
            <!-- <label>
              Stiffness:
              <select bind:value={flairBoxProps.springOrTween.stiffness}>
                <option value="low">low</option>
                <option value="medium">medium</option>
                <option value="high">high</option>
                <option value="custom">Custom</option>
              </select>
              {#if flairBoxProps.springOrTween.stiffness === "custom"}
                <input
                  type="number"
                  bind:value={customSpringStiffness}
                  onchange={() => {
                    if (
                      !flairBoxProps.springOrTween ||
                      flairBoxProps.springOrTween.type === "tween"
                    )
                      return;
                    flairBoxProps.springOrTween.stiffness =
                      customSpringStiffness;
                  }}
                  min="1"
                />
              {/if}
            </label>
            <label>
              Dampening:
              <select bind:value={flairBoxProps.springOrTween.dampening}>
                <option value="low">low</option>
                <option value="medium">medium</option>
                <option value="high">high</option>
                <option value="custom">Custom</option>
              </select>
              {#if flairBoxProps.springOrTween.dampening === "custom"}
                <input
                  type="number"
                  bind:value={customSpringDampening}
                  onchange={() => {
                    if (
                      !flairBoxProps.springOrTween ||
                      flairBoxProps.springOrTween.type === "tween"
                    )
                      return;
                    flairBoxProps.springOrTween.dampening =
                      customSpringDampening;
                  }}
                  min="1"
                />
              {/if}
            </label> -->
          </div>
        {:else if flairBoxProps.springOrTween?.type === "tween"}
          <div style="margin-left:1em;">
            <label>
              Duration:
              <select bind:value={flairBoxProps.springOrTween.duration}>
                <option value="short">short</option>
                <option value="medium">medium</option>
                <option value="long">long</option>
                <option value="custom">Custom</option>
              </select>
              {#if flairBoxProps.springOrTween.duration === "custom"}
                <input
                  type="number"
                  bind:value={customTweenDuration}
                  onchange={() => {
                    if (
                      !flairBoxProps.springOrTween ||
                      flairBoxProps.springOrTween.type === "spring"
                    )
                      return;
                    flairBoxProps.springOrTween.duration = customTweenDuration;
                  }}
                  min="1"
                />
              {/if}
            </label>
            <label>
              Easing:
              <select bind:value={flairBoxProps.springOrTween.easing}>
                {#each easingOptions as option}
                  <option value={option}>{option}</option>
                {/each}
              </select>
            </label>
          </div>
        {/if}
      </label>
      <strong>Animation Controls</strong>
      <hr />
      <!-- Animation Options: allow custom numbers and custom location -->
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
          <option value="custom">Custom (x, y)</option>
        </select>
        {#if animOptions.location === "custom"}
          <input
            type="number"
            step="0.01"
            min="0"
            max="1"
            bind:value={customLocation.x}
            placeholder="x (0-1)"
          />
          <input
            type="number"
            step="0.01"
            min="0"
            max="1"
            bind:value={customLocation.y}
            placeholder="y (0-1)"
          />
          <button
            onclick={() =>
              (animOptions.location = {
                x: customLocation.x,
                y: customLocation.y,
              })}>Set</button
          >
        {/if}
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
          <option value="perlin">perlin</option>
        </select>
      </label>
      <label>
        Speed:
        <select bind:value={animOptions.speed}>
          <option value="slow">slow</option>
          <option value="medium">medium</option>
          <option value="fast">fast</option>
          <option value="custom">Custom</option>
        </select>
        {#if animOptions.speed === "custom"}
          <input
            type="number"
            bind:value={customSpeed}
            onchange={() => (animOptions.speed = customSpeed)}
            min="0.01"
          />
        {/if}
      </label>
      <label>
        Intensity:
        <select bind:value={animOptions.intensity}>
          <option value="light">light</option>
          <option value="medium">medium</option>
          <option value="strong">strong</option>
          <option value="custom">Custom</option>
        </select>
        {#if animOptions.intensity === "custom"}
          <input
            type="number"
            bind:value={customIntensity}
            onchange={() => (animOptions.intensity = customIntensity)}
            min="0.01"
          />
        {/if}
      </label>
      <label>
        Size:
        <select bind:value={animOptions.size}>
          <option value="small">small</option>
          <option value="medium">medium</option>
          <option value="large">large</option>
          <option value="custom">Custom</option>
        </select>
        {#if animOptions.size === "custom"}
          <input
            type="number"
            bind:value={customSize}
            onchange={() => (animOptions.size = customSize)}
            min="0.01"
          />
        {/if}
      </label>
      <label>
        Number of impulses:
        <select bind:value={animOptions.nImpulses}>
          <option value="single">single</option>
          <option value="double">double</option>
        </select>
      </label>
      <label>
        Impulse Easing:
        <select bind:value={customImpulseEasing}>
          {#each easingOptions as option}
            <option value={option}>{option}</option>
          {/each}
        </select>
      </label>
      <!-- <label>
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
      </label> -->
    </div>
  </div>
{/if}

{#snippet flair()}
  <FlairCanvas
    bind:this={flairCanvasRef}
    spacing={flairBoxProps.spacing as "small" | "medium" | "large"}
    aspect={flairBoxProps.aspect as "square" | "wide" | "ultrawide"}
    dotSize={flairBoxProps.dotSize as "small" | "medium" | "large"}
    vignette={flairBoxProps.vignette as
      | "none"
      | "left"
      | "right"
      | "center"
      | "top"
      | "bottom"}
    hoverAction={flairBoxProps.hoverAction}
    visibility={flairBoxProps.visibility}
    bgType={flairBoxProps.bgType as "dots" | "grid" | "noisedots"}
    noiseTimeScale={1500}
    enableRandomOpacity={flairBoxProps.enableRandomOpacity}
    enableRandomPointSize={flairBoxProps.enableRandomPointSize}
    opacityNoiseScale={flairBoxProps.opacityNoiseScale}
    opacityNoiseMultiplier={flairBoxProps.opacityNoiseMultiplier}
    pointSizeNoiseScale={flairBoxProps.pointSizeNoiseScale}
    pointSizeNoiseMultiplier={flairBoxProps.pointSizeNoiseMultiplier}
    springOrTween={flairBoxProps.springOrTween?.type === "spring"
      ? {
          type: "spring",
          stiffness:
            flairBoxProps.springOrTween.stiffness === "custom"
              ? customSpringStiffness
              : flairBoxProps.springOrTween.stiffness,
          dampening:
            flairBoxProps.springOrTween.dampening === "custom"
              ? customSpringDampening
              : flairBoxProps.springOrTween.dampening,
        }
      : flairBoxProps.springOrTween?.type === "tween"
        ? {
            type: "tween",
            duration:
              flairBoxProps.springOrTween.duration === "custom"
                ? customTweenDuration
                : flairBoxProps.springOrTween.duration,
            easing: flairBoxProps.springOrTween
              .easing as keyof typeof easingFunctions,
          }
        : undefined}
    maskWaveRampIn={flairBoxProps.maskWaveRampIn}
    maskWaveRampOut={flairBoxProps.maskWaveRampOut}
    maskWaveThickness={flairBoxProps.maskWaveThickness}
    maskWaveMinValue={flairBoxProps.maskWaveMinValue}
    bind:triggerAnimation={animTrig}
  />
{/snippet}

{#if flairBoxProps.display === "bgOnly"}
  {@render flair()}
{:else if flairBoxProps.display === "behindBox"}
  {@render flair()}
  <div class="fixed flex h-screen w-screen items-center justify-center">
    <Panel class="max-w-96 p-4">
      <h1 class="text-text-brand-primary text-2xl">This is a Panel!</h1>
      <p class="text-text-brand-secondary mb-4">
        Lorem Ipsum is simply dummy text of the printing and typesetting
        industry. Lorem Ipsum has been the industry's standard dummy text ever
        since the 1500s, when an unknown printer took a galley of type and
        scrambled it to make a type specimen book.
      </p>
      <Button class="mb-2 w-full">Cool</Button>
      <Button class="mb-2 w-full" variant="secondary">Yes</Button>
      <Button class="w-full" variant="tertiary">Omg</Button>
    </Panel>
  </div>
{:else if flairBoxProps.display === "insideBox"}
  <div class="fixed flex h-screen w-screen items-center justify-center">
    <Panel class="relative min-h-44 max-w-96 overflow-hidden">
      {@render flair()}
      <div class="relative p-4">
        <h1 class="text-text-brand-primary text-2xl">This is a Panel!</h1>
        <p class="text-text-brand-secondary mb-4">
          Lorem Ipsum is simply dummy text of the printing and typesetting
          industry. Lorem Ipsum has been the industry's standard dummy text ever
          since the 1500s, when an unknown printer took a galley of type and
          scrambled it to make a type specimen book.
        </p>
        <Button class="mb-2 w-full">Cool</Button>
        <Button class="mb-2 w-full" variant="secondary">Yes</Button>
        <Button class="w-full" variant="tertiary">Omg</Button>
      </div>
    </Panel>
  </div>
{/if}
