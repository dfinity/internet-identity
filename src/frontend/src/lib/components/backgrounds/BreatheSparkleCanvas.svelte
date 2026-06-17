<script lang="ts">
  import { PerlinNoise3D } from "$lib/utils/UI/backgrounds/perlinNoise3d";
  import { onMount } from "svelte";

  interface Props {
    class?: string;
    fadeCenterHold?: string;
  }
  const { class: className = "", fadeCenterHold }: Props = $props();

  const maskStyle = $derived(
    fadeCenterHold !== undefined
      ? (() => {
          const grad = `radial-gradient(ellipse closest-side at center, transparent 0%, transparent ${fadeCenterHold}, black 100%)`;
          return `mask-image: ${grad}; -webkit-mask-image: ${grad};`;
        })()
      : "",
  );

  let canvasEl: HTMLCanvasElement | undefined = $state();

  const GAP = 28;
  const BASE_ALPHA = 0.05;
  const BREATHE_AMP = 0.16;
  const BREATHE_RADIUS_AMP = 0.25;
  const BREATHE_FREQ = 0.7;
  const BREATHE_PHASE_NOISE_SCALE = 0.003;
  const BREATHE_PHASE_TIME_SCALE = 0.08;
  const BREATHE_PHASE_RANGE = Math.PI * 2;
  const SPARKLE_TIME_SCALE = 0.34;
  const SPARKLE_THRESHOLD = 0.64;
  const SPARKLE_RING_ALPHA = 0.34;
  const SPARKLE_RING_BASE = 1.6;
  const SPARKLE_RING_GAIN = 1.0;
  const HALO_ALPHA_THRESHOLD = 0.25;
  const HALO_RADIUS_OFFSET = 2.6;

  type Dot = { x: number; y: number };

  onMount(() => {
    const canvas = canvasEl;
    if (!canvas) return;
    const ctx = canvas.getContext("2d");
    if (!ctx) return;

    const dpr = Math.min(window.devicePixelRatio ?? 1, 2);
    const sparkleNoise = new PerlinNoise3D();
    sparkleNoise.noiseSeed(2);
    const breatheNoise = new PerlinNoise3D();
    breatheNoise.noiseSeed(11);

    let dots: Dot[] = [];
    let lastW = -1;
    let lastH = -1;

    const build = () => {
      const w = canvas.clientWidth;
      const h = canvas.clientHeight;
      canvas.width = Math.max(1, Math.floor(w * dpr));
      canvas.height = Math.max(1, Math.floor(h * dpr));
      ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
      const cols = Math.ceil(w / GAP) + 1;
      const rows = Math.ceil(h / GAP) + 1;
      const offX = (w - (cols - 1) * GAP) / 2;
      const offY = (h - (rows - 1) * GAP) / 2;
      dots = [];
      for (let r = 0; r < rows; r++) {
        for (let c = 0; c < cols; c++) {
          dots.push({ x: offX + c * GAP, y: offY + r * GAP });
        }
      }
      lastW = w;
      lastH = h;
    };

    let raf = 0;
    let start = 0;

    const tick = (now: number) => {
      if (start === 0) start = now;
      const t = (now - start) / 1000;
      const w = canvas.clientWidth;
      const h = canvas.clientHeight;
      if (w === 0 || h === 0) {
        raf = requestAnimationFrame(tick);
        return;
      }
      if (w !== lastW || h !== lastH) build();
      ctx.clearRect(0, 0, w, h);

      const zt = t * SPARKLE_TIME_SCALE;
      const breatheZ = t * BREATHE_PHASE_TIME_SCALE;

      for (const d of dots) {
        const phase =
          breatheNoise.get(
            d.x * BREATHE_PHASE_NOISE_SCALE,
            d.y * BREATHE_PHASE_NOISE_SCALE,
            breatheZ,
          ) * BREATHE_PHASE_RANGE;
        const pulse = 0.5 + 0.5 * Math.sin(t * BREATHE_FREQ + phase);
        const a = BASE_ALPHA + pulse * BREATHE_AMP;
        const radius = 1 + pulse * BREATHE_RADIUS_AMP;

        const sN = sparkleNoise.get(d.x, d.y, zt);
        const s = Math.max(0, sN - SPARKLE_THRESHOLD) / (1 - SPARKLE_THRESHOLD);

        ctx.beginPath();
        ctx.fillStyle = `rgba(255,255,255,${a})`;
        ctx.arc(d.x, d.y, radius, 0, Math.PI * 2);
        ctx.fill();

        if (a > HALO_ALPHA_THRESHOLD) {
          ctx.beginPath();
          ctx.fillStyle = `rgba(255,255,255,${(a - HALO_ALPHA_THRESHOLD) * 0.22})`;
          ctx.arc(d.x, d.y, radius + HALO_RADIUS_OFFSET, 0, Math.PI * 2);
          ctx.fill();
        }

        if (s > 0) {
          const ringAlpha = Math.min(1, s) * SPARKLE_RING_ALPHA;
          const ringR = SPARKLE_RING_BASE + s * SPARKLE_RING_GAIN;
          ctx.beginPath();
          ctx.lineWidth = 1;
          ctx.strokeStyle = `rgba(255,255,255,${ringAlpha})`;
          ctx.arc(d.x, d.y, ringR, 0, Math.PI * 2);
          ctx.stroke();
        }
      }

      raf = requestAnimationFrame(tick);
    };

    build();
    raf = requestAnimationFrame(tick);

    const onResize = () => build();
    window.addEventListener("resize", onResize);

    return () => {
      cancelAnimationFrame(raf);
      window.removeEventListener("resize", onResize);
    };
  });
</script>

<div
  class={["absolute inset-0 -z-50 w-full select-none", className]}
  style={maskStyle}
  aria-hidden="true"
>
  <canvas bind:this={canvasEl} class="block h-full w-full"></canvas>
</div>
