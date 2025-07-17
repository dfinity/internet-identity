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
  }: {
    xSpacing?: number;
    ySpacing?: number;
  } = $props();

  const STIFFNESS = 0.1;
  const DAMPING = 0.5;
  const MOUSE_RADIUS = 200;
  const MOUSE_SCALAR = 0.3;

  const CLICK_RADIUS = 600;
  const IMPULSE_SCALAR = 0.3; // You can tweak this for effect
  const WAVE_SPEED = 1.5; // ms per px, lower = faster wave
  const IMPULSE_DURATION = 80; // ms the impulse lasts

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
    animateRepel();
  });

  const createSpringsLocal = (xCount: number, yCount: number) => {
    springs = createSprings(xCount, yCount, Spring, STIFFNESS, DAMPING);
  };

  $effect(() => {
    if (xPositions.length > 0 && yPositions.length > 0) {
      createSpringsLocal(xPositions.length, yPositions.length);
    }
  });

  const handlePointerMove = (e: PointerEvent) => {
    pointerX = e.clientX;
    pointerY = e.clientY;
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
      CLICK_RADIUS,
      IMPULSE_SCALAR,
      WAVE_SPEED,
      IMPULSE_DURATION,
    );
  };

  function animateRepel() {
    if (pointerInside && pointerX !== null && pointerY !== null) {
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
        MOUSE_RADIUS,
        MOUSE_SCALAR,
        WAVE_SPEED,
        IMPULSE_DURATION,
      );
    }
    requestAnimationFrame(animateRepel);
  }

  const handleReset = () => {
    pointerInside = false;
    resetNodes(springs);
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

<svelte:window
  bind:innerHeight
  bind:innerWidth
  onpointermove={handlePointerMove}
/>
