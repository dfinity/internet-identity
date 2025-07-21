<script lang="ts">
  import { PerlinNoise3D } from "$lib/utils/UI/backgrounds/perlinNoise3d";
  import {
    createDirectionalImpulse,
    createImpulse,
    createRotationalImpulse,
    createXYSprings,
    getImpulseLocation,
    getVignetteConfig,
    gridPath,
    leftPos,
    resetNodes,
    topPos,
  } from "$lib/utils/UI/backgrounds/waveBackground";
  import { quadOut } from "svelte/easing";
  import { Spring, Tween } from "svelte/motion";

  export interface FlairAnimationOptions {
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
      | { x: number; y: number };
    target: ("motion" | "scale" | "opacity")[];
    motionType:
      | "omni"
      | "xy"
      | "yx"
      | "up"
      | "down"
      | "left"
      | "right"
      | "cw"
      | "ccw";
    speed: "slow" | "medium" | "fast";
    intensity: "light" | "medium" | "strong";
    size: "large" | "medium" | "small";
  }

  export interface FlairBoxProps {
    bgType?: "dots" | "grid" | "noisedots";
    spacing?: "large" | "medium" | "small";
    aspect?: "square" | "wide" | "ultrawide";
    visibility?: "always" | "animation";
    dotSize?: "large" | "medium" | "small";
    vignette?: "center" | "top" | "left" | "right" | "bottom" | "none";
    hoverAction?: "intense" | "minimal" | "none";
    springOrTween?: "spring" | "tween";
    backgroundClasses?: string;
    foregroundClasses?: string;
    triggerAnimation?: (opts: FlairAnimationOptions) => void;
  }

  let {
    bgType = "dots",
    spacing = "medium",
    aspect = "wide",
    hoverAction = "minimal",
    visibility = "always",
    dotSize = "medium",
    vignette = "center",
    springOrTween = "spring",
    backgroundClasses,
    foregroundClasses,
    triggerAnimation = $bindable(),
  }: FlairBoxProps = $props();

  let backgroundRef = $state<HTMLDivElement>();

  let pointerX = $state<number>(0);
  let pointerY = $state<number>(0);
  let lastPointerX = $state<number>(0);
  let lastPointerY = $state<number>(0);
  let lastPointerTime = $state<number>(0);
  let pointerSpeed = $state<number>(0);
  let pointerInside = $state<boolean>(false);

  let stiffness = $state(0.1);
  let damping = $state(0.5);
  let rippleRadius = $state(300);
  let impulseScalar = $state(0.2);
  let waveSpeed = $state(1.5);
  let impulseDuration = $state(80);

  let noiseScale = $state<number>(0.2);
  let timeScale = $state<number>(1500);

  type Ripple = {
    tween: Tween<number>;
    x: number;
    y: number;
    id: number;
  };

  let ripples = $state<Ripple[]>([]);

  // --- Animation state ---
  let time = $state<number>(0);

  function animateNoise(now: number) {
    if (bgType === "noisedots") {
      time = now / timeScale; // seconds
      requestAnimationFrame(animateNoise);
    }
  }

  let innerHeight = $state<number>();
  let innerWidth = $state<number>();

  const noise = new PerlinNoise3D();
  noise.noiseSeed(0);

  const spacingTable = {
    large: 100,
    medium: 50,
    small: 25,
  };

  const aspectTable = {
    square: 1,
    wide: 1.6,
    ultrawide: 2.8,
  };

  const dotSizeTable = {
    large: 3,
    medium: 2,
    small: 1,
  };

  const speedTable = {
    fast: 0.3,
    medium: 1,
    slow: 2,
  };

  const intensityTable = {
    light: 0.3,
    medium: 1,
    strong: 2,
  };

  const rippleSizeTable = {
    small: 0.3,
    medium: 1,
    large: 2,
  };

  let xSpacing = $derived(spacingTable[spacing]);
  let ySpacing = $derived(spacingTable[spacing] / aspectTable[aspect]);

  let xPositions = $derived<number[]>(
    innerWidth
      ? Array.from(
          { length: Math.max(1, Math.floor((innerWidth - 1) / xSpacing) + 2) }, // +2 instead of +1
          (_, i) => i * xSpacing,
        )
      : [0],
  );

  let yPositions = $derived<number[]>(
    innerHeight
      ? Array.from(
          { length: Math.max(1, Math.floor((innerHeight - 1) / ySpacing) + 2) }, // +2 instead of +1
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

  let springs: (
    | Spring<{ x: number; y: number }>
    | Tween<{ x: number; y: number }>
  )[][] = $state([[]]);

  let vignetteConfig = $derived(
    getVignetteConfig(vignette, innerHeight, innerWidth),
  );

  triggerAnimation = (opts) => {
    const { location, target, motionType, speed, intensity, size } = opts;

    if (target.includes("motion") && innerWidth && innerHeight) {
      const { x, y } = getImpulseLocation(location, innerWidth, innerHeight);

      if (motionType === "cw" || motionType === "ccw") {
        createRotationalImpulse(
          x,
          y,
          xPositions,
          yPositions,
          offsetX,
          offsetY,
          springs,
          rippleRadius * rippleSizeTable[size],
          impulseScalar * intensityTable[intensity],
          waveSpeed * speedTable[speed],
          impulseDuration * speedTable[speed],
          motionType,
        );
        createRipple(x, y);
        return;
      }
      if (
        motionType === "down" ||
        motionType === "up" ||
        motionType === "left" ||
        motionType === "right"
      ) {
        createDirectionalImpulse(
          x,
          y,
          xPositions,
          yPositions,
          offsetX,
          offsetY,
          springs,
          rippleRadius * rippleSizeTable[size],
          impulseScalar * intensityTable[intensity],
          waveSpeed * speedTable[speed],
          impulseDuration * speedTable[speed],
          motionType,
        );
        createRipple(x, y);
        return;
      }

      createImpulse(
        x,
        y,
        xPositions,
        yPositions,
        offsetX,
        offsetY,
        springs,
        rippleRadius * rippleSizeTable[size],
        impulseScalar * intensityTable[intensity],
        waveSpeed * speedTable[speed],
        impulseDuration * speedTable[speed],
        motionType === "omni" ? "omni" : motionType === "xy" ? "x" : "y",
      );
      createRipple(x, y);

      setTimeout(() => {
        createImpulse(
          x,
          y,
          xPositions,
          yPositions,
          offsetX,
          offsetY,
          springs,
          rippleRadius * 1.2 * rippleSizeTable[size],
          impulseScalar * 0.6 * intensityTable[intensity],
          (waveSpeed / 2) * speedTable[speed],
          impulseDuration * speedTable[speed],
          motionType === "omni" ? "omni" : motionType === "xy" ? "y" : "x",
        );
        createRipple(x, y);
      }, 250 * speedTable[speed]);
    }
  };

  const createSpringsLocal = (xCount: number, yCount: number) => {
    springs = createXYSprings(
      xCount,
      yCount,
      springOrTween === "spring" ? Spring : Tween,
      stiffness,
      damping,
    );
  };

  const handleReset = () => {
    pointerInside = false;
    resetNodes(springs);
  };

  const handlePointerMove = (e: PointerEvent) => {
    const now = performance.now();
    const prevX = lastPointerX;
    const prevY = lastPointerY;
    const prevTime = lastPointerTime;

    pointerX = e.clientX;
    pointerY = e.clientY;

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
      pointerSpeed = dist / dt; // px per ms
    }
  };

  const createRipple = (x: number, y: number) => {
    const id = Date.now();
    const newRipple: Ripple = {
      tween: new Tween(0, {
        duration: impulseDuration,
        easing: quadOut,
      }),
      x,
      y,
      id,
    };

    newRipple.tween.target = 1;

    ripples.push(newRipple);
    setTimeout(() => {
      ripples = ripples.filter((ripple) => ripple.id !== id);
    }, impulseDuration);
  };

  $effect(() => {
    if (xPositions.length > 0 && yPositions.length > 0) {
      createSpringsLocal(xPositions.length, yPositions.length);
    }
  });

  $effect(() => {
    if (bgType === "noisedots") {
      requestAnimationFrame(animateNoise);
    }
  });
</script>

<div
  class="bg-bg-brand-primary fixed h-screen w-screen select-none"
  aria-hidden="true"
  bind:this={backgroundRef}
  onpointerleave={handleReset}
  style="pointer-events: auto;"
>
  <svg
    width={innerWidth}
    height={innerHeight}
    style="display: block; width: 100vw; height: 100vh; pointer-events: none; position: absolute; top: 0; left: 0;"
  >
    <defs>
      <filter id="blur" x="-20%" y="-20%" width="140%" height="140%">
        <feGaussianBlur stdDeviation="40" />
      </filter>
      {#if vignette !== "none"}
        <mask id="vignette-mask">
          <rect width="100%" height="100%" fill="black" />
          <ellipse
            cx={vignetteConfig.cx}
            cy={vignetteConfig.cy}
            rx={vignetteConfig.rx}
            ry={vignetteConfig.ry}
            fill="white"
            filter="url(#blur)"
          />
        </mask>
      {/if}
      {#if visibility === "animation"}
        <mask id="wave-mask">
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
      {/if}
    </defs>
    <g mask={vignette !== "none" ? "url(#vignette-mask)" : undefined}>
      <g mask={visibility === "animation" ? "url(#wave-mask)" : undefined}>
        {#if bgType === "dots"}
          {#each xPositions as xPos, xIndex}
            {#each yPositions as yPos, yIndex}
              <circle
                cx={leftPos(xPos, xIndex, yIndex, offsetX, springs)}
                cy={topPos(yPos, xIndex, yIndex, offsetY, springs)}
                r={dotSizeTable[dotSize]}
                fill={"var(--fg-brand-primary, #fff)"}
              />
            {/each}
          {/each}
        {:else if bgType === "noisedots"}
          {#each xPositions as xPos, xIndex}
            {#each yPositions as yPos, yIndex}
              <circle
                cx={leftPos(xPos, xIndex, yIndex, offsetX, springs)}
                cy={topPos(yPos, xIndex, yIndex, offsetY, springs)}
                r={`${dotSizeTable[dotSize] * noise.get(xPos * noiseScale, yPos * noiseScale, time)}`}
                fill={`hsla(70,100%,50%,${noise.get(xPos * noiseScale, yPos * noiseScale, time) * 150}%)`}
              />
            {/each}
          {/each}
        {:else if bgType === "grid"}
          <!-- Horizontal curves -->
          {#each yPositions as yPos, yIndex}
            {#each xPositions.slice(0, -1) as xPos, xIndex}
              <path
                d={gridPath(
                  leftPos(xPos, xIndex, yIndex, offsetX, springs),
                  topPos(yPos, xIndex, yIndex, offsetY, springs),
                  leftPos(
                    xPositions[xIndex + 1],
                    xIndex + 1,
                    yIndex,
                    offsetX,
                    springs,
                  ),
                  topPos(yPos, xIndex + 1, yIndex, offsetY, springs),
                )}
                stroke="var(--fg-brand-primary, #fff)"
                stroke-width={dotSizeTable[dotSize]}
                fill="none"
              />
            {/each}
          {/each}

          <!-- Vertical curves -->
          {#each xPositions as xPos, xIndex}
            {#each yPositions.slice(0, -1) as yPos, yIndex}
              <path
                d={gridPath(
                  leftPos(xPos, xIndex, yIndex, offsetX, springs),
                  topPos(yPos, xIndex, yIndex, offsetY, springs),
                  leftPos(xPos, xIndex, yIndex + 1, offsetX, springs),
                  topPos(
                    yPositions[yIndex + 1],
                    xIndex,
                    yIndex + 1,
                    offsetY,
                    springs,
                  ),
                )}
                stroke="var(--fg-brand-primary, #fff)"
                stroke-width={dotSizeTable[dotSize]}
                fill="none"
              />
            {/each}
          {/each}
        {/if}
      </g>
    </g>
  </svg>
</div>

<svelte:window
  bind:innerHeight
  bind:innerWidth
  onpointermove={handlePointerMove}
/>
