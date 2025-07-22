<script lang="ts">
  import { PerlinNoise3D } from "$lib/utils/UI/backgrounds/perlinNoise3d";
  import {
    createContinuousWave,
    createDirectionalImpulse,
    createImpulse,
    createRotationalImpulse,
    createXYSprings,
    drawNodes,
    drawVignetteMask,
    getImpulseLocation,
    getVignetteConfig,
    gridPath,
    leftPos,
    resetNodes,
    topPos,
  } from "$lib/utils/UI/backgrounds/waveBackground";
  import { quadOut } from "svelte/easing";
  import { Spring, Tween } from "svelte/motion";
  import type { FlairCanvasProps } from "./FlairCanvas";
  import { onMount } from "svelte";

  let {
    bgType = "dots",
    spacing = "medium",
    aspect = "wide",
    hoverAction = "minimal",
    visibility = "always",
    dotSize = "medium",
    vignette = "center",
    springOrTween = {
      type: "spring",
      stiffness: "medium",
      dampening: "medium",
    },
    backgroundClasses,
    foregroundClasses,
    triggerAnimation = $bindable(),
  }: FlairCanvasProps = $props();

  let backgroundRef = $state<HTMLDivElement>();
  let canvasRef = $state<HTMLCanvasElement>();
  let ctx = $state<CanvasRenderingContext2D | null>(null);
  let dpr = typeof window !== "undefined" ? window.devicePixelRatio : 1;

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

  let clientHeight = $state<number>();
  let clientWidth = $state<number>();

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

  let xSpacing = $derived(
    typeof spacing === "number" ? spacing : spacingTable[spacing],
  );
  let ySpacing = $derived(
    typeof spacing === "number"
      ? spacing
      : spacingTable[spacing] /
          (typeof aspect === "number" ? aspect : aspectTable[aspect]),
  );

  let xPositions = $derived<number[]>(
    clientWidth
      ? Array.from(
          { length: Math.max(1, Math.floor((clientWidth - 1) / xSpacing) + 2) }, // +2 instead of +1
          (_, i) => i * xSpacing,
        )
      : [0],
  );

  let yPositions = $derived<number[]>(
    clientHeight
      ? Array.from(
          {
            length: Math.max(1, Math.floor((clientHeight - 1) / ySpacing) + 2),
          }, // +2 instead of +1
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
    clientWidth !== undefined ? (clientWidth - gridWidth) / 2 : 0,
  );
  let offsetY = $derived<number>(
    clientHeight !== undefined ? (clientHeight - gridHeight) / 2 : 0,
  );

  let springs: (
    | Spring<{ x: number; y: number }>
    | Tween<{ x: number; y: number }>
  )[][] = $state([[]]);

  let vignetteConfig = $derived(
    getVignetteConfig(vignette, clientHeight, clientWidth),
  );

  triggerAnimation = (opts) => {
    const {
      location,
      target,
      motionType,
      speed,
      intensity,
      size,
      nImpulses,
      impulseEasing,
    } = opts;

    if (target.includes("motion") && clientWidth && clientHeight) {
      const { x, y } = getImpulseLocation(location, clientWidth, clientHeight);

      if (motionType === "cw" || motionType === "ccw") {
        createRotationalImpulse(
          x,
          y,
          xPositions,
          yPositions,
          offsetX,
          offsetY,
          springs,
          rippleRadius *
            (typeof size === "number" ? size : rippleSizeTable[size]),
          impulseScalar *
            (typeof intensity === "number"
              ? intensity
              : intensityTable[intensity]),
          waveSpeed * (typeof speed === "number" ? speed : speedTable[speed]),
          impulseDuration *
            (typeof speed === "number" ? speed : speedTable[speed]),
          motionType,
          impulseEasing,
        );
        createRipple(x, y);
        if (nImpulses === "double") {
          setTimeout(() => {
            createRotationalImpulse(
              x,
              y,
              xPositions,
              yPositions,
              offsetX,
              offsetY,
              springs,
              rippleRadius *
                1.2 *
                (typeof size === "number" ? size : rippleSizeTable[size]),
              impulseScalar *
                0.6 *
                (typeof intensity === "number"
                  ? intensity
                  : intensityTable[intensity]),
              waveSpeed *
                (typeof speed === "number" ? speed : speedTable[speed]),
              impulseDuration *
                (typeof speed === "number" ? speed : speedTable[speed]),
              motionType === "cw" ? "ccw" : "cw",
              impulseEasing,
            );
            createRipple(x, y);
          }, 250);
        }
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
          rippleRadius *
            (typeof size === "number" ? size : rippleSizeTable[size]),
          impulseScalar *
            (typeof intensity === "number"
              ? intensity
              : intensityTable[intensity]),
          waveSpeed * (typeof speed === "number" ? speed : speedTable[speed]),
          impulseDuration *
            (typeof speed === "number" ? speed : speedTable[speed]),
          motionType,
          "xy",
          impulseEasing,
        );
        createRipple(x, y);
        if (nImpulses === "double") {
          setTimeout(() => {
            createDirectionalImpulse(
              x,
              y,
              xPositions,
              yPositions,
              offsetX,
              offsetY,
              springs,
              rippleRadius *
                1.2 *
                (typeof size === "number" ? size : rippleSizeTable[size]),
              impulseScalar *
                0.6 *
                (typeof intensity === "number"
                  ? intensity
                  : intensityTable[intensity]),
              (waveSpeed / 2) *
                (typeof speed === "number" ? speed : speedTable[speed]),
              impulseDuration *
                (typeof speed === "number" ? speed : speedTable[speed]),
              motionType,
              "xy",
              impulseEasing,
            );
            createRipple(x, y);
          }, 250);
        }
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
        rippleRadius *
          (typeof size === "number" ? size : rippleSizeTable[size]),
        impulseScalar *
          (typeof intensity === "number"
            ? intensity
            : intensityTable[intensity]),
        waveSpeed * (typeof speed === "number" ? speed : speedTable[speed]),
        impulseDuration *
          (typeof speed === "number" ? speed : speedTable[speed]),
        motionType === "omni" ? "omni" : motionType === "xy" ? "x" : "y",
        impulseEasing,
      );
      createRipple(x, y);

      if (nImpulses === "double") {
        setTimeout(
          () => {
            createImpulse(
              x,
              y,
              xPositions,
              yPositions,
              offsetX,
              offsetY,
              springs,
              rippleRadius *
                1.2 *
                (typeof size === "number" ? size : rippleSizeTable[size]),
              impulseScalar *
                0.6 *
                (typeof intensity === "number"
                  ? intensity
                  : intensityTable[intensity]),
              (waveSpeed / 2) *
                (typeof speed === "number" ? speed : speedTable[speed]),
              impulseDuration *
                (typeof speed === "number" ? speed : speedTable[speed]),
              motionType === "omni" ? "omni" : motionType === "xy" ? "y" : "x",
              impulseEasing,
            );
            createRipple(x, y);
          },
          250 * (typeof speed === "number" ? speed : speedTable[speed]),
        );
      }
    }
  };

  const createSpringsLocal = (xCount: number, yCount: number) => {
    springs = createXYSprings(
      xCount,
      yCount,
      springOrTween.type === "spring" ? Spring : Tween,
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

    if (now - prevTime > 60 && hoverAction !== "none") {
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
        rippleRadius / 2,
        hoverAction === "intense" ? 0.4 : 0.1,
        waveSpeed,
        impulseDuration,
        "xy",
      );
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

  let lastRenderTime = 0; // Add this at the top-level script

  const render = (now: number) => {
    const FRAME_DURATION = 1000 / 60; // ~16.67ms for 60fps

    if (now - lastRenderTime < FRAME_DURATION) {
      requestAnimationFrame(render);
      return;
    }
    lastRenderTime = now;

    if (!ctx || !canvasRef) {
      requestAnimationFrame(render);
      return;
    }

    // Clear the canvas
    ctx.clearRect(0, 0, canvasRef.width, canvasRef.height);
    drawNodes(
      xPositions,
      yPositions,
      offsetX,
      offsetY,
      springs,
      typeof dotSize === "number" ? dotSize : dotSizeTable[dotSize],
      ctx,
    );

    if (
      vignette !== "none" &&
      clientHeight !== undefined &&
      clientWidth !== undefined
    ) {
      drawVignetteMask(clientWidth, clientHeight, vignetteConfig, ctx);
    }

    requestAnimationFrame(render);
  };

  onMount(() => {
    if (canvasRef) {
      ctx = canvasRef.getContext("2d");
    }
    requestAnimationFrame(render);
  });

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

  $effect(() => {
    if (canvasRef && ctx && clientWidth && clientHeight) {
      dpr = window.devicePixelRatio || 1;
      canvasRef.width = clientWidth * dpr;
      canvasRef.height = clientHeight * dpr;
      canvasRef.style.width = clientWidth + "px";
      canvasRef.style.height = clientHeight + "px";
      ctx.setTransform(1, 0, 0, 1, 0, 0); // Reset any existing transforms
      ctx.scale(dpr, dpr);
    }
  });
</script>

<div
  class="absolute inset-0 h-full w-full select-none"
  aria-hidden="true"
  bind:this={backgroundRef}
  onpointerleave={handleReset}
  bind:clientHeight
  bind:clientWidth
  onpointermove={handlePointerMove}
>
  <canvas
    class="block h-full w-full"
    bind:this={canvasRef}
    height={clientHeight}
    width={clientWidth}
  ></canvas>
</div>
