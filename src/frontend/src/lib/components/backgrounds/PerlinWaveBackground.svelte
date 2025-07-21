<script lang="ts">
  import { onMount } from "svelte";
  import { PerlinNoise3D } from "$lib/utils/UI/backgrounds/perlinNoise3d";
  import { Tween } from "svelte/motion";
  import { cubicIn, cubicInOut, cubicOut, quadOut } from "svelte/easing";

  const noise = new PerlinNoise3D();
  noise.noiseSeed(0);

  let {
    xSpacing = 30,
    ySpacing = 20,
    showControls = false,
  }: {
    xSpacing?: number;
    ySpacing?: number;
    showControls?: boolean;
  } = $props();

  let pointerX = $state<number>(0);
  let pointerY = $state<number>(0);
  let pointerInside = $state<boolean>(false);

  let lastPointerX = $state<number | null>(null);
  let lastPointerY = $state<number | null>(null);
  let lastPointerTime = $state<number | null>(null);

  let noiseScale = $state<number>(0.2);
  let timeScale = $state<number>(700);

  let innerHeight = $state<number>();
  let innerWidth = $state<number>();

  let rippleDuration = $state(5000);

  type Ripple = {
    tween: Tween<number>;
    x: number;
    y: number;
    id: number;
  };

  let ripples = $state<Ripple[]>([]);

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

  // --- Animation state ---
  let time = $state<number>(0);

  function animate(now: number) {
    time = now / timeScale; // seconds
    requestAnimationFrame(animate);
  }

  const handleClick = (e: PointerEvent) => {
    createRipple(e);
    setTimeout(() => {
      createRipple(e);
    }, 450);
  };

  const createRipple = (e: PointerEvent) => {
    const id = Date.now();
    const newRipple: Ripple = {
      tween: new Tween(0, {
        duration: rippleDuration,
        easing: quadOut,
      }),
      x: e.clientX,
      y: e.clientY,
      id,
    };

    newRipple.tween.target = 1;

    ripples.push(newRipple);
    setTimeout(() => {
      ripples = ripples.filter((ripple) => ripple.id !== id);
    }, rippleDuration);
  };

  onMount(() => {
    requestAnimationFrame(animate);
  });
</script>

<div class="fixed h-screen w-screen" onclick={handleClick}>
  <svg
    class="bg-bg-brand-primary fixed h-screen w-screen select-none"
    aria-hidden="true"
    width={innerWidth}
    height={innerHeight}
    style="top:0; left:0; position:fixed; pointer-events:none;"
  >
    <defs>
      <filter id="blur" x="-20%" y="-20%" width="140%" height="140%">
        <feGaussianBlur stdDeviation="30" />
      </filter>
    </defs>
    <mask id="wave-mask">
      <rect width="100%" height="100%" fill="black" />
      {#each ripples as ripple}
        <circle
          cx={ripple.x}
          cy={ripple.y}
          r={ripple.tween.current * 700}
          stroke-width={40 + 200 * ripple.tween.current}
          stroke={`hsla(0,0%,100%,${100 - ripple.tween.current * 100}%)`}
          filter="url(#blur)"
        />
      {/each}
    </mask>
    <g mask="url(#wave-mask)">
      {#each xPositions as xPos}
        {#each yPositions as yPos}
          <circle
            cx={xPos + offsetX}
            cy={yPos + offsetY}
            r={`${2.5 * noise.get(xPos * noiseScale, yPos * noiseScale, time)}`}
            fill={`hsla(70,100%,50%,${noise.get(xPos * noiseScale, yPos * noiseScale, time) * 150}%)`}
          />
        {/each}
      {/each}
    </g>
  </svg>
</div>

<h1 class="text-text-brand-primary fixed bottom-3 w-full text-center text-3xl">
  Click anywhere to make waves!
</h1>

<svelte:window bind:innerHeight bind:innerWidth />
