<script lang="ts">
  import { Spring } from "svelte/motion";
  import {
    createXYSprings,
    leftPos,
    topPos,
    createContinuousWave,
    createDirectionalImpulse,
    createImpulse,
    resetNodes,
    createRotationalImpulse,
  } from "$lib/utils/UI/backgrounds/waveBackground";
  let backgroundRef = $state<HTMLDivElement>();

  let {
    xSpacing = 50,
    ySpacing = 50,
    showControls = false,
  }: {
    xSpacing?: number;
    ySpacing?: number;
    showControls?: boolean;
  } = $props();

  // Reactive parameters that can be controlled by sliders
  let stiffness = $state(0.1);
  let damping = $state(0.5);
  let mouseRadius = $state(200);
  let mouseScalar = $state(0.1);
  let clickRadius = $state(600);
  let impulseScalar = $state(0.3);
  let waveSpeed = $state(1.5);
  let impulseDuration = $state(80);

  let pointerX = $state<number>(0);
  let pointerY = $state<number>(0);
  let pointerInside = $state<boolean>(false);

  let lastPointerX = $state<number | null>(null);
  let lastPointerY = $state<number | null>(null);
  let lastPointerTime = $state<number | null>(null);

  let innerHeight = $state<number>();
  let innerWidth = $state<number>();

  let motionAxis = $state<"xy" | "x" | "y">("xy");
  let impulseCombo = $state<"off" | "xFirst" | "yFirst">("off");

  let heldDirections = new Set<"up" | "down" | "left" | "right">();
  let rHeld = $state<boolean>(false);

  let xPositions = $derived<number[]>(
    innerWidth
      ? Array.from(
          { length: Math.max(1, Math.floor((innerWidth - 1) / xSpacing) + 1) },
          (_, i) => i * xSpacing,
        )
      : [0],
  );

  let yPositions = $derived<number[]>(
    innerHeight
      ? Array.from(
          { length: Math.max(1, Math.floor((innerHeight - 1) / ySpacing) + 1) },
          (_, i) => i * ySpacing,
        )
      : [0],
  );

  let gridWidth = $derived<number>(
    xPositions.length > 1 ? (xPositions.length - 1) * xSpacing : 0,
  );
  let gridHeight = $derived<number>(
    yPositions.length > 1 ? (yPositions.length - 1) * ySpacing : 0,
  );

  let offsetX = $derived<number>(
    innerWidth !== undefined ? (innerWidth - gridWidth) / 2 : 0,
  );
  let offsetY = $derived<number>(
    innerHeight !== undefined ? (innerHeight - gridHeight) / 2 : 0,
  );

  let springs: Spring<{ x: number; y: number }>[][] = $state([[]]);

  $effect(() => {
    if (xPositions.length > 0 && yPositions.length > 0) {
      createSpringsLocal(xPositions.length, yPositions.length);
    }
  });

  const createSpringsLocal = (xCount: number, yCount: number) => {
    springs = createXYSprings(xCount, yCount, Spring, stiffness, damping);
  };

  const handlePointerMove = (e: PointerEvent) => {
    const now = performance.now();
    const prevX = lastPointerX;
    const prevY = lastPointerY;
    const prevTime = lastPointerTime;

    pointerX = e.clientX;
    pointerY = e.clientY;

    let speed = 0;
    if (
      prevX !== null &&
      prevY !== null &&
      prevTime !== null &&
      now > prevTime
    ) {
      const dx = pointerX - prevX;
      const dy = pointerY - prevY;
      const dt = now - prevTime;
      const dist = Math.sqrt(dx * dx + dy * dy);
      speed = dist / dt; // px per ms
    }

    // Store for next event
    lastPointerX = pointerX;
    lastPointerY = pointerY;
    lastPointerTime = now;

    // Scale the mouseScalar by speed, clamp for sanity
    const minScalar = 0;
    const maxScalar = 1.0;
    const dynamicScalar = Math.min(
      maxScalar,
      Math.max(minScalar, mouseScalar * speed),
    );

    if (pointerInside) {
      createContinuousWave(
        pointerX,
        pointerY,
        xPositions,
        yPositions,
        offsetX,
        offsetY,
        springs,
        xSpacing,
        ySpacing,
        mouseRadius * 0.7 + mouseRadius * dynamicScalar,
        dynamicScalar, // use dynamic scalar
        waveSpeed,
        impulseDuration,
        motionAxis,
      );
    }
  };

  const handlePointerEnter = () => {
    pointerInside = true;
  };

  const handlePointerDown = (e: PointerEvent) => {
    const direction = heldDirections.values().next().value;
    if (rHeld) {
      // Randomly pick cw or ccw
      const rotDir = Math.random() < 0.5 ? "cw" : "ccw";
      createRotationalImpulse(
        e.clientX,
        e.clientY,
        xPositions,
        yPositions,
        offsetX,
        offsetY,
        springs,
        clickRadius,
        impulseScalar * 0.8,
        waveSpeed,
        impulseDuration,
        rotDir,
      );
      setTimeout(() => {
        createRotationalImpulse(
          e.clientX,
          e.clientY,
          xPositions,
          yPositions,
          offsetX,
          offsetY,
          springs,
          clickRadius * 1.3,
          impulseScalar * 0.6,
          waveSpeed / 2,
          impulseDuration,
          rotDir === "ccw" ? "cw" : "ccw",
        );
      }, 250);
    } else if (heldDirections.size === 0 || !direction) {
      createImpulse(
        e.clientX,
        e.clientY,
        xPositions,
        yPositions,
        offsetX,
        offsetY,
        springs,
        clickRadius,
        impulseScalar,
        waveSpeed,
        impulseDuration,
        impulseCombo === "off" ? "xy" : impulseCombo === "xFirst" ? "x" : "y",
      );
      setTimeout(() => {
        createImpulse(
          e.clientX,
          e.clientY,
          xPositions,
          yPositions,
          offsetX,
          offsetY,
          springs,
          clickRadius * 1.3,
          impulseScalar * 0.8,
          waveSpeed / 2,
          impulseDuration,
          impulseCombo === "off" ? "xy" : impulseCombo === "xFirst" ? "y" : "x",
        );
      }, 250);
    } else {
      // For simplicity, just use the first held direction
      createDirectionalImpulse(
        e.clientX,
        e.clientY,
        xPositions,
        yPositions,
        offsetX,
        offsetY,
        springs,
        clickRadius,
        impulseScalar,
        waveSpeed,
        impulseDuration,
        direction,
        impulseCombo === "off" ? "xy" : impulseCombo === "xFirst" ? "x" : "y",
      );
      setTimeout(
        () => {
          createDirectionalImpulse(
            e.clientX,
            e.clientY,
            xPositions,
            yPositions,
            offsetX,
            offsetY,
            springs,
            clickRadius * 1.3,
            impulseScalar * 0.8,
            waveSpeed / 2,
            impulseDuration,
            direction,
            impulseCombo === "off"
              ? "xy"
              : impulseCombo === "xFirst"
                ? "y"
                : "x",
          );
        },
        impulseCombo === "off" ? 350 : 250,
      );
    }
  };

  const handleReset = () => {
    pointerInside = false;
    resetNodes(springs);
  };

  const toggleControls = () => {
    showControls = !showControls;
  };

  function handleKeyDown(e: KeyboardEvent) {
    switch (e.key) {
      case "ArrowUp":
        heldDirections.add("up");
        break;
      case "ArrowDown":
        heldDirections.add("down");
        break;
      case "ArrowLeft":
        heldDirections.add("left");
        break;
      case "ArrowRight":
        heldDirections.add("right");
        break;
      case "r":
        rHeld = true;
        break;
    }
  }

  function handleKeyUp(e: KeyboardEvent) {
    switch (e.key) {
      case "ArrowUp":
        heldDirections.delete("up");
        break;
      case "ArrowDown":
        heldDirections.delete("down");
        break;
      case "ArrowLeft":
        heldDirections.delete("left");
        break;
      case "ArrowRight":
        heldDirections.delete("right");
        break;
      case "r":
        rHeld = false;
        break;
    }
  }
</script>

<div
  class="bg-bg-brand-primary fixed h-screen w-screen select-none"
  aria-hidden="true"
  bind:this={backgroundRef}
  onpointerleave={handleReset}
  onpointerdown={handlePointerDown}
  onpointermove={handlePointerMove}
  onpointerenter={handlePointerEnter}
  style="pointer-events: auto;"
>
  <svg
    width={innerWidth}
    height={innerHeight}
    style="display: block; width: 100vw; height: 100vh; pointer-events: none; position: absolute; top: 0; left: 0;"
  >
    {#each xPositions as xPos, xIndex}
      {#each yPositions as yPos, yIndex}
        <circle
          cx={leftPos(xPos, xIndex, yIndex, offsetX, springs)}
          cy={topPos(yPos, xIndex, yIndex, offsetY, springs)}
          r="2"
          fill="var(--fg-brand-primary, #fff)"
        />
      {/each}
    {/each}
  </svg>
</div>

<h1
  class="text-text-brand-primary bold fixed bottom-3 w-full text-center text-3xl"
>
  Hold directional key to constrain impulses to single direction. Hold r for
  rotational impulses.
</h1>

<!-- Control Panel -->
{#if showControls}
  <div
    class="fixed top-4 right-4 z-50 max-w-sm rounded-lg bg-white/90 p-4 shadow-lg backdrop-blur-sm"
  >
    <div class="mb-4 flex items-center justify-between">
      <h3 class="text-lg font-semibold text-gray-800">Wave Controls</h3>
      <button
        onclick={toggleControls}
        class="text-gray-500 hover:text-gray-700"
      >
        ✕
      </button>
    </div>

    <div class="space-y-4">
      <!-- Grid Spacing -->
      <div>
        <label class="mb-1 block text-sm font-medium text-gray-700">
          X Spacing: {xSpacing}px
        </label>
        <input
          type="range"
          min="20"
          max="100"
          step="5"
          bind:value={xSpacing}
          class="h-2 w-full cursor-pointer appearance-none rounded-lg bg-gray-200"
        />
      </div>

      <div>
        <label class="mb-1 block text-sm font-medium text-gray-700">
          Y Spacing: {ySpacing}px
        </label>
        <input
          type="range"
          min="20"
          max="100"
          step="5"
          bind:value={ySpacing}
          class="h-2 w-full cursor-pointer appearance-none rounded-lg bg-gray-200"
        />
      </div>

      <!-- Spring Physics -->
      <div>
        <label class="mb-1 block text-sm font-medium text-gray-700">
          Stiffness: {stiffness.toFixed(2)}
        </label>
        <input
          type="range"
          min="0.01"
          max="0.5"
          step="0.01"
          bind:value={stiffness}
          class="h-2 w-full cursor-pointer appearance-none rounded-lg bg-gray-200"
        />
      </div>

      <div>
        <label class="mb-1 block text-sm font-medium text-gray-700">
          Damping: {damping.toFixed(2)}
        </label>
        <input
          type="range"
          min="0.1"
          max="1.0"
          step="0.05"
          bind:value={damping}
          class="h-2 w-full cursor-pointer appearance-none rounded-lg bg-gray-200"
        />
      </div>

      <!-- Mouse Interaction -->
      <div>
        <label class="mb-1 block text-sm font-medium text-gray-700">
          Mouse Radius: {mouseRadius}px
        </label>
        <input
          type="range"
          min="50"
          max="400"
          step="10"
          bind:value={mouseRadius}
          class="h-2 w-full cursor-pointer appearance-none rounded-lg bg-gray-200"
        />
      </div>

      <div>
        <label class="mb-1 block text-sm font-medium text-gray-700">
          Mouse Strength: {mouseScalar.toFixed(2)}
        </label>
        <input
          type="range"
          min="0.1"
          max="1.0"
          step="0.05"
          bind:value={mouseScalar}
          class="h-2 w-full cursor-pointer appearance-none rounded-lg bg-gray-200"
        />
      </div>

      <!-- Click Interaction -->
      <div>
        <label class="mb-1 block text-sm font-medium text-gray-700">
          Click Radius: {clickRadius}px
        </label>
        <input
          type="range"
          min="200"
          max="1000"
          step="50"
          bind:value={clickRadius}
          class="h-2 w-full cursor-pointer appearance-none rounded-lg bg-gray-200"
        />
      </div>

      <div>
        <label class="mb-1 block text-sm font-medium text-gray-700">
          Click Strength: {impulseScalar.toFixed(2)}
        </label>
        <input
          type="range"
          min="0.1"
          max="1.0"
          step="0.05"
          bind:value={impulseScalar}
          class="h-2 w-full cursor-pointer appearance-none rounded-lg bg-gray-200"
        />
      </div>

      <!-- Wave Properties -->
      <div>
        <label class="mb-1 block text-sm font-medium text-gray-700">
          Wave Speed: {waveSpeed.toFixed(1)}ms/px
        </label>
        <input
          type="range"
          min="0.5"
          max="5.0"
          step="0.1"
          bind:value={waveSpeed}
          class="h-2 w-full cursor-pointer appearance-none rounded-lg bg-gray-200"
        />
      </div>

      <div>
        <label class="mb-1 block text-sm font-medium text-gray-700">
          Impulse Duration: {impulseDuration}ms
        </label>
        <input
          type="range"
          min="20"
          max="200"
          step="10"
          bind:value={impulseDuration}
          class="h-2 w-full cursor-pointer appearance-none rounded-lg bg-gray-200"
        />
      </div>

      <div>
        <label class="mb-1 block text-sm font-medium text-gray-700">
          Motion Axis:
        </label>
        <div class="flex gap-2">
          <label>
            <input type="radio" bind:group={motionAxis} value="xy" /> XY
          </label>
          <label>
            <input type="radio" bind:group={motionAxis} value="x" /> X Only
          </label>
          <label>
            <input type="radio" bind:group={motionAxis} value="y" /> Y Only
          </label>
        </div>
      </div>
      <div>
        <label class="mb-1 block text-sm font-medium text-gray-700">
          Impulse Combo:
        </label>
        <div class="flex gap-2">
          <label>
            <input type="radio" bind:group={impulseCombo} value="off" /> off
          </label>
          <label>
            <input type="radio" bind:group={impulseCombo} value="xFirst" /> x first
          </label>
          <label>
            <input type="radio" bind:group={impulseCombo} value="yFirst" /> y first
          </label>
        </div>
      </div>

      <!-- Reset Button -->
      <button
        onclick={handleReset}
        class="w-full rounded-lg bg-blue-500 px-4 py-2 text-white transition-colors hover:bg-blue-600"
      >
        Reset Grid
      </button>
    </div>
  </div>
{/if}

<!-- Toggle Controls Button -->
<button
  onclick={toggleControls}
  class="fixed top-4 right-4 z-50 rounded-lg bg-white/90 p-2 shadow-lg backdrop-blur-sm transition-colors hover:bg-white"
  class:hidden={showControls}
>
  ⚙️
</button>

<svelte:window
  bind:innerHeight
  bind:innerWidth
  onpointermove={handlePointerMove}
  onkeydown={handleKeyDown}
  onkeyup={handleKeyUp}
/>
