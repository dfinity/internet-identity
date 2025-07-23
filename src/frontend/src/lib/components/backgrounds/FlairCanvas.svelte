<script lang="ts">
  import { PerlinNoise3D } from "$lib/utils/UI/backgrounds/perlinNoise3d";
  import {
    createContinuousWave,
    createDirectionalImpulse,
    createImpulse,
    createOpacityWave,
    createPerlinImpulse,
    createRotationalImpulse,
    createXYNodeMotions,
    createXYSprings,
    drawMovingRingMask,
    drawNodes,
    drawVignetteMask,
    getHypotenuse,
    getImpulseLocation,
    getVignetteConfig,
    gridPath,
    leftPos,
    resetNodes,
    topPos,
  } from "$lib/utils/UI/backgrounds/waveBackground";
  import { quadOut } from "svelte/easing";
  import { Spring, Tween } from "svelte/motion";
  import type { FlairCanvasProps, NodeMotion } from "./FlairCanvas";
  import { onMount } from "svelte";
  import * as easingFunctions from "svelte/easing";

  let {
    bgType = "dots",
    spacing = "medium",
    aspect = "wide",
    hoverAction = "none",
    visibility = "always",
    dotSize = "medium",
    vignette = "center",
    springOrTween = {
      type: "spring",
      stiffness: "medium",
      dampening: "medium",
    },
    noiseTimeScale = 1500,
    enableRandomOpacity = false,
    opacityNoiseScale = 0.01,
    opacityNoiseMultiplier = 1,
    enableRandomPointSize = false,
    pointSizeNoiseScale = 0.01,
    pointSizeNoiseMultiplier = 1,
    maskWaveMinValue,
    maskWaveThickness,
    maskWaveRampIn,
    maskWaveRampOut,
    customColor,
    customColorMode,
    backgroundClasses,
    foregroundClasses,
    triggerAnimation = $bindable(),
  }: FlairCanvasProps = $props();

  let backgroundRef = $state<HTMLDivElement>();
  let canvasRef = $state<HTMLCanvasElement>();
  let ctx = $state<CanvasRenderingContext2D | null>(null);
  let dpr = typeof window !== "undefined" ? window.devicePixelRatio : 1;
  let canvasGlobalOpacity = new Tween(100); // Workaround for ugly resize behavior
  let resizeTimeout = $state<ReturnType<typeof setTimeout>>();
  let observer = $state<ResizeObserver>();

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

  let opacityWaveDuration = $state(500);

  let opacityWaveMotion = new Tween(0, {
    easing: easingFunctions.linear,
    duration: opacityWaveDuration,
  });

  let motionNoiseScale = $state<number>(0.01);

  // --- Animation state ---
  let time = $state<number>(0);

  function animateNoise(now: number) {
    time =
      now /
      (typeof noiseTimeScale === "number"
        ? noiseTimeScale
        : noiseTimeScaleTable[noiseTimeScale]); // seconds
    // requestAnimationFrame(animateNoise);
  }

  let clientHeight = $state<number>();
  let clientWidth = $state<number>();

  const motionNoise = new PerlinNoise3D();
  motionNoise.noiseSeed(0);

  const opacityNoise = new PerlinNoise3D();
  opacityNoise.noiseSeed(1);

  const pointSizeNoise = new PerlinNoise3D();
  pointSizeNoise.noiseSeed(2);

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
    large: 3,
  };

  const stiffnessTable = {
    light: 0.1,
    medium: 0.5,
    strong: 1,
  };

  const dampeningTable = {
    light: 0.1,
    medium: 0.5,
    strong: 1,
  };

  const noiseTimeScaleTable = {
    fast: 1000,
    medium: 1500,
    slow: 2000,
  };

  const opacityNoiseScaleTable = {
    large: 0.11,
    medium: 0.01,
    small: 0.005,
  };

  const opacityNoiseMultiplierTable = {
    large: 1,
    medium: 0.7,
    small: 0.3,
  };

  const pointSizeNoiseScaleTable = {
    large: 0.01,
    medium: 0.005,
    small: 0.001,
  };

  const pointSizeNoiseMultiplierTable = {
    large: 5,
    medium: 1.5,
    small: 1.1,
  };

  const maskWaveThicknessTable = {
    large: 0.8,
    medium: 0.4,
    small: 0.2,
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

  let springs: NodeMotion[][] = $state([[]]);

  let vignetteConfig = $derived(
    getVignetteConfig(vignette, clientHeight, clientWidth),
  );

  triggerAnimation = async (opts) => {
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

    let easingFunction = impulseEasing
      ? easingFunctions[impulseEasing]
      : undefined;

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
          easingFunction,
        );

        if (nImpulses === "double") {
          setTimeout(
            () => {
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
                easingFunction,
              );
            },
            250 * (typeof speed === "number" ? speed : speedTable[speed]),
          );
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
          easingFunction,
        );

        if (nImpulses === "double") {
          setTimeout(
            () => {
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
                easingFunction,
              );
            },
            250 * (typeof speed === "number" ? speed : speedTable[speed]),
          );
        }
        return;
      }

      if (motionType === "perlin") {
        createPerlinImpulse(
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
          waveSpeed *
            0.75 *
            (typeof speed === "number" ? speed : speedTable[speed]),
          impulseDuration *
            (typeof speed === "number" ? speed : speedTable[speed]),
          "omni",
          motionNoise,
          0.01,
          1,
          easingFunction,
        );

        if (nImpulses === "double") {
          setTimeout(
            () => {
              createPerlinImpulse(
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
                "omni",
                motionNoise,
                0.01,
                1,
                easingFunction,
              );
            },
            250 * (typeof speed === "number" ? speed : speedTable[speed]),
          );
        }
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
        easingFunction,
      );

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
              easingFunction,
            );
          },
          250 * (typeof speed === "number" ? speed : speedTable[speed]),
        );
      }

      if (visibility === "maskwave") {
        if (impulseEasing) {
          opacityWaveMotion = new Tween(0, {
            easing: easingFunctions[impulseEasing],
          });
        }
        createOpacityWave(
          opacityWaveMotion,
          rippleRadius *
            waveSpeed *
            (typeof speed === "number" ? speed : speedTable[speed]),
          0,
        );
      }
    }
  };

  const createSpringsLocal = (xCount: number, yCount: number) => {
    springs = createXYNodeMotions(
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
      if (hoverAction === "minimal") {
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
          rippleRadius * 0.618,
          0.06,
          waveSpeed * 3,
          impulseDuration,
          "xy",
        );
      } else if (hoverAction === "intense") {
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
          rippleRadius * 0.618,
          0.3,
          waveSpeed * 3,
          impulseDuration,
          "xy",
        );
      }
    }
  };

  let lastRenderTime = 0; // Add this at the top-level script

  const render = (now: number) => {
    const FRAME_DURATION = 1000 / 60; // ~16.67ms for 60fps
    const deltaTime = now - lastRenderTime;

    if (deltaTime < FRAME_DURATION) {
      requestAnimationFrame(render);
      return;
    }
    lastRenderTime = now;

    if (!ctx || !canvasRef) {
      requestAnimationFrame(render);
      return;
    }

    for (let i = 0; i < springs.length; i++) {
      for (let j = 0; j < springs[i].length; j++) {
        const node = springs[i][j];
        const { x, y } = node.motion.current;
        const dx = x - node.prev.x;
        const dy = y - node.prev.y;
        node.speed = Math.sqrt(dx * dx + dy * dy) / deltaTime;
        node.prev = { x, y };
      }
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
      visibility,
      ctx,
      customColor,
      customColorMode,
      enableRandomOpacity
        ? {
            noise: opacityNoise,
            noiseScale:
              typeof opacityNoiseScale === "number"
                ? opacityNoiseScale
                : opacityNoiseScaleTable[opacityNoiseScale],
            multiplier:
              typeof opacityNoiseMultiplier === "number"
                ? opacityNoiseMultiplier
                : opacityNoiseMultiplierTable[opacityNoiseMultiplier],
            noiseTime: time,
          }
        : null,
      enableRandomPointSize
        ? {
            noise: pointSizeNoise,
            noiseScale:
              typeof pointSizeNoiseScale === "number"
                ? pointSizeNoiseScale
                : pointSizeNoiseScaleTable[pointSizeNoiseScale],
            multiplier:
              typeof pointSizeNoiseMultiplier === "number"
                ? pointSizeNoiseMultiplier
                : pointSizeNoiseMultiplierTable[pointSizeNoiseMultiplier],
            noiseTime: time,
          }
        : null,
    );

    if (
      vignette !== "none" &&
      clientHeight !== undefined &&
      clientWidth !== undefined
    ) {
      drawVignetteMask(clientWidth, clientHeight, vignetteConfig, ctx);
    }

    if (
      visibility === "maskwave" &&
      !!clientWidth &&
      !!clientHeight &&
      maskWaveThickness !== undefined
    ) {
      drawMovingRingMask(
        clientWidth,
        clientHeight,
        opacityWaveMotion.current,
        getHypotenuse(clientHeight, clientWidth) *
          (typeof maskWaveThickness === "number"
            ? maskWaveThickness
            : maskWaveThicknessTable[maskWaveThickness]),
        ctx,
        maskWaveRampIn,
        maskWaveRampOut,
        maskWaveMinValue,
      );
    }

    animateNoise(now);

    requestAnimationFrame(render);
  };

  const handleGlobalPointerMove = (e: PointerEvent) => {
    if (!backgroundRef) return;
    const rect = backgroundRef.getBoundingClientRect();
    if (
      e.clientX >= rect.left &&
      e.clientX <= rect.right &&
      e.clientY >= rect.top &&
      e.clientY <= rect.bottom
    ) {
      handlePointerMove(e);
    }
  };

  const handleResize = () => {
    clearTimeout(resizeTimeout);
    canvasGlobalOpacity.set(0, { duration: 0 });
    resizeTimeout = setTimeout(() => {
      canvasGlobalOpacity.set(100, { duration: 1000 });
    }, 10);
  };

  onMount(() => {
    observer = new ResizeObserver(handleResize);
    if (backgroundRef) {
      observer.observe(backgroundRef);
    }
  });

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
  class="absolute inset-0 -z-50 h-full w-full select-none"
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
    style={`opacity: ${canvasGlobalOpacity.current}%`}
  ></canvas>
</div>

<svelte:window onpointermove={handleGlobalPointerMove} />
