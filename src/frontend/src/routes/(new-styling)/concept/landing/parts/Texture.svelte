<!--
  One interactive texture (textures.ts) filling the box it's put in. It
  follows the pointer anywhere over its box, even through the words laid on
  top, and settles back when the pointer leaves. Without a hover (a phone),
  or when asked to, it moves by itself along a slow figure, as textures.dev
  does on touch screens. Under reduced motion it snaps and doesn't drift.

  The ink is the `color` of the canvas: a text colour class, so it follows
  the theme, or a colour given, which it moves to over 300ms as a surface's
  background would.
-->
<script lang="ts">
  import { onMount } from "svelte";
  import { prefersReducedMotion } from "svelte/motion";
  import type { ClassValue } from "svelte/elements";
  import { TEXTURE_KINDS, type Pointer, type TextureKind } from "./textures";

  interface Props {
    kind: TextureKind;
    /** A colour for the marks; by default the canvas's text colour. */
    ink?: string;
    /** Moves by itself even where a pointer could hover. */
    ambient?: boolean;
    class?: ClassValue;
  }

  const { kind, ink, ambient = false, class: className }: Props = $props();

  let host = $state<HTMLDivElement>();
  let canvas = $state<HTMLCanvasElement>();
  /** Paints for a while from now, set once the canvas is up. */
  let follow: (() => void) | undefined;

  // A new ink eases in through the canvas's `color`: paint every frame
  // until it has.
  $effect(() => {
    void ink;
    void ambient;
    follow?.();
  });

  onMount(() => {
    if (host === undefined || canvas === undefined) {
      return;
    }
    const box = host;
    const surface = canvas;
    const ctx = surface.getContext("2d");
    if (ctx === null) {
      return;
    }
    const texture = TEXTURE_KINDS[kind]();
    const noHover = matchMedia("(hover: none)");
    let width = 0;
    let height = 0;
    let pointer: Pointer;
    let frame: number | undefined;
    let last = 0;
    let started = performance.now();
    let colour = "";
    /** Paints every frame until then, while the ink eases to a new one. */
    let holdUntil = 0;

    const drifting = () =>
      (ambient || noHover.matches) && !prefersReducedMotion.current;

    /** A slow Lissajous figure over the middle of the box. */
    const drift = (now: number): Pointer => {
      const t = (now - started) / 1000;
      return {
        x: width * (0.5 + 0.32 * Math.sin(t * 0.43)),
        y: height * (0.5 + 0.3 * Math.sin(t * 0.61 + 1.2)),
      };
    };

    const readInk = () => getComputedStyle(surface).color;
    /** Device pixels to a CSS pixel, 1 where the browser doesn't say. */
    const ratio = () =>
      window.devicePixelRatio > 0 ? window.devicePixelRatio : 1;

    const paint = () => {
      const dpr = ratio();
      ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
      ctx.clearRect(0, 0, width, height);
      colour = readInk();
      texture.draw(ctx, colour);
    };

    const tick = (now: number) => {
      frame = undefined;
      const dt = Math.min((now - last) / 1000, 1 / 20);
      last = now;
      const target = drifting() ? drift(now) : pointer;
      const moving = prefersReducedMotion.current
        ? (texture.step(10, target), false)
        : texture.step(dt, target);
      paint();
      if (moving || drifting() || now < holdUntil) {
        frame = requestAnimationFrame(tick);
      }
    };

    const wake = () => {
      if (frame === undefined) {
        last = performance.now();
        frame = requestAnimationFrame(tick);
      }
    };

    const resize = () => {
      const rect = box.getBoundingClientRect();
      width = Math.max(1, rect.width);
      height = Math.max(1, rect.height);
      const dpr = ratio();
      surface.width = Math.round(width * dpr);
      surface.height = Math.round(height * dpr);
      texture.resize(width, height);
      paint();
      wake();
    };

    const onMove = (event: PointerEvent) => {
      if (event.pointerType === "touch") {
        return;
      }
      const rect = box.getBoundingClientRect();
      const x = event.clientX - rect.left;
      const y = event.clientY - rect.top;
      const inside = x >= 0 && y >= 0 && x <= rect.width && y <= rect.height;
      const next = inside ? { x, y } : undefined;
      if (next === undefined && pointer === undefined) {
        return;
      }
      pointer = next;
      wake();
    };
    const onLeave = () => {
      pointer = undefined;
      wake();
    };

    // The theme can change under a still texture: look at the ink now and
    // then, and paint again if it did.
    const watchInk = setInterval(() => {
      if (frame === undefined && readInk() !== colour) {
        paint();
      }
    }, 400);

    follow = () => {
      holdUntil = performance.now() + 450;
      wake();
    };

    const observer = new ResizeObserver(resize);
    observer.observe(box);
    window.addEventListener("pointermove", onMove, { passive: true });
    document.documentElement.addEventListener("pointerleave", onLeave);
    started = performance.now();
    resize();

    return () => {
      follow = undefined;
      observer.disconnect();
      clearInterval(watchInk);
      window.removeEventListener("pointermove", onMove);
      document.documentElement.removeEventListener("pointerleave", onLeave);
      if (frame !== undefined) {
        cancelAnimationFrame(frame);
      }
    };
  });
</script>

<div
  bind:this={host}
  aria-hidden="true"
  class={["pointer-events-none absolute inset-0 overflow-hidden", className]}
>
  <canvas
    bind:this={canvas}
    class="block size-full transition-[color] duration-300 ease-(--ii-ease-out) motion-reduce:transition-none"
    style:color={ink}
  ></canvas>
</div>
