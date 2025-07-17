<script lang="ts">
  import { Spring } from "svelte/motion";
  import {
    createSprings,
    leftPos,
    topPos,
    createContinuousWave,
    createImpulse,
    resetNodes,
  } from "$lib/utils/UI/backgrounds/waveBackground";
  let backgroundRef = $state<HTMLDivElement>();
  import { onMount } from "svelte";

  let {
    xSpacing = 50,
    ySpacing = 50,
    continuousRepel = false,
    showControls = false,
  }: {
    xSpacing?: number;
    ySpacing?: number;
    continuousRepel?: boolean;
    showControls?: boolean;
  } = $props();

  // Reactive parameters that can be controlled by sliders
  let stiffness = $state(0.1);
  let damping = $state(0.5);
  let mouseRadius = $state(200);
  let mouseScalar = $state(0.3);
  let clickRadius = $state(600);
  let impulseScalar = $state(0.3);
  let waveSpeed = $state(1.5);
  let impulseDuration = $state(80);

  let pointerX = $state<number>(0);
  let pointerY = $state<number>(0);
  let pointerInside = $state<boolean>(false);

  let innerHeight = $state<number>();
  let innerWidth = $state<number>();

  let xPositions = $derived<number[]>(
    innerWidth
      ? Array.from(
          { length: Math.floor(Math.abs(innerWidth / xSpacing)) },
          (_, i) => i * xSpacing,
        )
      : [0],
  );
  let yPositions = $derived<number[]>(
    innerHeight
      ? Array.from(
          { length: Math.floor(Math.abs(innerHeight / ySpacing)) },
          (_, i) => i * xSpacing,
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

  onMount(() => {
    if (continuousRepel) animateRepel();
  });

  $effect(() => {
    if (xPositions.length > 0 && yPositions.length > 0) {
      createSpringsLocal(xPositions.length, yPositions.length);
    }
  });

  const createSpringsLocal = (xCount: number, yCount: number) => {
    springs = createSprings(xCount, yCount, Spring, stiffness, damping);
  };

  const handlePointerMove = (e: PointerEvent) => {
    pointerX = e.clientX;
    pointerY = e.clientY;
    if (!continuousRepel && pointerInside) {
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
        mouseRadius,
        mouseScalar,
        waveSpeed,
        impulseDuration,
      );
    }
  };

  const handlePointerEnter = () => {
    pointerInside = true;
  };

  const handlePointerDown = (e: PointerEvent) => {
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
    );
  };

  function animateRepel() {
    if (
      continuousRepel &&
      pointerInside &&
      pointerX !== null &&
      pointerY !== null
    ) {
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
        mouseRadius,
        mouseScalar,
        waveSpeed,
        impulseDuration,
      );
      requestAnimationFrame(animateRepel);
    }
  }

  const handleReset = () => {
    pointerInside = false;
    resetNodes(springs);
  };

  const toggleControls = () => {
    showControls = !showControls;
  };
</script>

<div
  class="bg-bg-brand-primary fixed h-screen w-screen select-none"
  aria-hidden="true"
  bind:this={backgroundRef}
  onpointerleave={handleReset}
  onpointerdown={handlePointerDown}
  onpointermove={handlePointerMove}
  onpointerenter={handlePointerEnter}
>
  {#each xPositions as xPos, xIndex}
    {#each yPositions as yPos, yIndex}
      <div
        class="bg-bg-brand-solid fixed h-1 w-1 -translate-1/2 rounded-full"
        style="left: {leftPos(
          xPos,
          xIndex,
          yIndex,
          offsetX,
          springs,
        )}px; top: {topPos(yPos, xIndex, yIndex, offsetY, springs)}px; "
      ></div>
    {/each}
  {/each}
</div>

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

      <!-- Toggle Continuous Repel -->
      <div class="flex items-center space-x-2">
        <input
          type="checkbox"
          id="continuousRepel"
          bind:checked={continuousRepel}
          class="h-4 w-4 rounded border-gray-300 bg-gray-100 text-blue-600 focus:ring-blue-500"
        />
        <label for="continuousRepel" class="text-sm font-medium text-gray-700">
          Continuous Repel
        </label>
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
/>
