/**
 * Interactive textures for the landing playground, drawn on a canvas.
 *
 * After the free set of textures.dev (https://textures.dev, MIT): magnet,
 * watch, twist, blinds, cells and swell, rebuilt here from their descriptions
 * rather than installed, so the concept takes no new dependency. Each is a
 * field of marks in the ink colour that answers the pointer and settles back
 * when it leaves. Positions are CSS pixels; the canvas scales for the screen.
 */

export type Pointer = { x: number; y: number } | undefined;

export interface Texture {
  /** Lays the marks out for a canvas this size. */
  resize(width: number, height: number): void;
  /** Moves every mark `dt` seconds on towards where the pointer wants it.
   *  Returns whether anything is still moving. */
  step(dt: number, pointer: Pointer): boolean;
  draw(ctx: CanvasRenderingContext2D, ink: string): void;
}

export type TextureKind =
  "magnet" | "watch" | "twist" | "blinds" | "cells" | "swell";

const clamp = (value: number, min: number, max: number): number =>
  Math.min(Math.max(value, min), max);

/** 0 below `edge0`, 1 above `edge1`, smooth between. */
const smoothstep = (edge0: number, edge1: number, x: number): number => {
  const t = clamp((x - edge0) / (edge1 - edge0), 0, 1);
  return t * t * (3 - 2 * t);
};

/** Exponential approach: how much of the way to go after `dt` with a time
 *  constant `tau`. Frame-rate independent. */
const toward = (value: number, target: number, dt: number, tau: number) =>
  target + (value - target) * Math.exp(-dt / tau);

/** The shortest turn from `from` to `to`, for lines (period π). */
const lineTurn = (from: number, to: number): number => {
  let delta = (to - from) % Math.PI;
  if (delta > Math.PI / 2) delta -= Math.PI;
  if (delta < -Math.PI / 2) delta += Math.PI;
  return delta;
};

const EPSILON = 0.01;

/** Cells on a grid of `pitch`, centred in the canvas. */
const grid = (width: number, height: number, pitch: number) => {
  const cols = Math.max(1, Math.floor(width / pitch));
  const rows = Math.max(1, Math.floor(height / pitch));
  return {
    cols,
    rows,
    x0: (width - (cols - 1) * pitch) / 2,
    y0: (height - (rows - 1) * pitch) / 2,
  };
};

/* ---------------------------------------------------------------------- */
/* Magnet: dots nearest the pointer snap into a solid block, nearby ones   */
/* lean in, all on springs.                                                */
/* ---------------------------------------------------------------------- */

const magnet = (): Texture => {
  const PITCH = 14;
  const DOT = 3.6;
  const PACK = 3.7;
  const REACH = 11;
  const PULL = 5.5;
  const ZETA = 0.66;
  let cols = 0;
  let rows = 0;
  let x0 = 0;
  let y0 = 0;
  let home = new Float32Array(0);
  let pos = new Float32Array(0);
  let vel = new Float32Array(0);
  let stiff = new Float32Array(0);

  return {
    resize(width, height) {
      ({ cols, rows, x0, y0 } = grid(width, height, PITCH));
      const n = cols * rows;
      home = new Float32Array(n * 2);
      pos = new Float32Array(n * 2);
      vel = new Float32Array(n * 2);
      stiff = new Float32Array(n);
      for (let j = 0; j < rows; j++) {
        for (let i = 0; i < cols; i++) {
          const k = j * cols + i;
          home[k * 2] = pos[k * 2] = x0 + i * PITCH;
          home[k * 2 + 1] = pos[k * 2 + 1] = y0 + j * PITCH;
          stiff[k] = 760 * (1 + (Math.random() * 2 - 1) * 0.35);
        }
      }
    },
    step(dt, pointer) {
      let ci = -99;
      let cj = -99;
      if (pointer !== undefined) {
        ci = clamp(Math.round((pointer.x - x0) / PITCH), 2, cols - 3);
        cj = clamp(Math.round((pointer.y - y0) / PITCH), 2, rows - 3);
      }
      const sub = Math.max(1, Math.ceil(dt * 240));
      const h = dt / sub;
      let moving = false;
      for (let j = 0; j < rows; j++) {
        for (let i = 0; i < cols; i++) {
          const k = j * cols + i;
          let tx = home[k * 2];
          let ty = home[k * 2 + 1];
          if (pointer !== undefined) {
            const di = i - ci;
            const dj = j - cj;
            if (Math.abs(di) <= 2 && Math.abs(dj) <= 2) {
              tx = pointer.x + di * PACK;
              ty = pointer.y + dj * PACK;
            } else {
              const d = Math.hypot(di, dj);
              if (d < REACH) {
                const pull = PULL * smoothstep(0, 1, (REACH - d) / (REACH - 2));
                tx -= (di / d) * pull;
                ty -= (dj / d) * pull;
              }
            }
          }
          const k2 = k * 2;
          const dx = pos[k2] - tx;
          const dy = pos[k2 + 1] - ty;
          if (
            Math.abs(dx) < EPSILON &&
            Math.abs(dy) < EPSILON &&
            Math.abs(vel[k2]) < EPSILON &&
            Math.abs(vel[k2 + 1]) < EPSILON
          ) {
            pos[k2] = tx;
            pos[k2 + 1] = ty;
            vel[k2] = vel[k2 + 1] = 0;
            continue;
          }
          moving = true;
          const s = stiff[k];
          const c = 2 * ZETA * Math.sqrt(s);
          for (let n = 0; n < sub; n++) {
            vel[k2] += (-s * (pos[k2] - tx) - c * vel[k2]) * h;
            vel[k2 + 1] += (-s * (pos[k2 + 1] - ty) - c * vel[k2 + 1]) * h;
            pos[k2] += vel[k2] * h;
            pos[k2 + 1] += vel[k2 + 1] * h;
          }
        }
      }
      return moving;
    },
    draw(ctx, ink) {
      ctx.fillStyle = ink;
      ctx.beginPath();
      const r = DOT / 2;
      for (let k = 0; k < cols * rows; k++) {
        const x = pos[k * 2];
        const y = pos[k * 2 + 1];
        ctx.moveTo(x + r, y);
        ctx.arc(x, y, r, 0, Math.PI * 2);
      }
      ctx.fill();
    },
  };
};

/* ---------------------------------------------------------------------- */
/* Watch: each cell's dot swells under the pointer and turns to it, like  */
/* a field of eyes.                                                        */
/* ---------------------------------------------------------------------- */

const watch = (): Texture => {
  const PITCH = 26;
  const CELL = 23.6;
  const REST = 3.8;
  const REACH = 14;
  let cols = 0;
  let rows = 0;
  let x0 = 0;
  let y0 = 0;
  let size = new Float32Array(0);
  let shift = new Float32Array(0);

  return {
    resize(width, height) {
      ({ cols, rows, x0, y0 } = grid(width, height, PITCH));
      size = new Float32Array(cols * rows).fill(REST);
      shift = new Float32Array(cols * rows * 2);
    },
    step(dt, pointer) {
      let moving = false;
      for (let j = 0; j < rows; j++) {
        for (let i = 0; i < cols; i++) {
          const k = j * cols + i;
          const cx = x0 + i * PITCH;
          const cy = y0 + j * PITCH;
          let targetSize = REST;
          let sx = 0;
          let sy = 0;
          let tau = 0.14;
          if (pointer !== undefined) {
            const dx = pointer.x - cx;
            const dy = pointer.y - cy;
            const dist = Math.hypot(dx, dy);
            const d = dist / PITCH;
            targetSize = REST + (CELL - REST) * Math.max(0, 1 - d / 9) ** 2.2;
            const fade = d < REACH ? 1 : Math.max(0, 1 - (d - REACH) / 3);
            const room = ((CELL - targetSize) / 2) * 0.85 * fade;
            if (dist > 0.001) {
              sx = (dx / dist) * Math.min(room, dist);
              sy = (dy / dist) * Math.min(room, dist);
            }
            tau = 0.045 + (0.14 - 0.045) * Math.min(d / REACH, 1);
          }
          const nextSize = toward(size[k], targetSize, dt, tau);
          const nx = toward(shift[k * 2], sx, dt, tau);
          const ny = toward(shift[k * 2 + 1], sy, dt, tau);
          if (
            Math.abs(nextSize - targetSize) > EPSILON ||
            Math.abs(nx - sx) > EPSILON ||
            Math.abs(ny - sy) > EPSILON
          ) {
            moving = true;
          }
          size[k] = nextSize;
          shift[k * 2] = nx;
          shift[k * 2 + 1] = ny;
        }
      }
      return moving;
    },
    draw(ctx, ink) {
      // The sockets, faint.
      ctx.fillStyle = ink;
      ctx.globalAlpha = 0.1;
      ctx.beginPath();
      for (let j = 0; j < rows; j++) {
        for (let i = 0; i < cols; i++) {
          ctx.roundRect(
            x0 + i * PITCH - CELL / 2,
            y0 + j * PITCH - CELL / 2,
            CELL,
            CELL,
            CELL * 0.3,
          );
        }
      }
      ctx.fill();
      ctx.globalAlpha = 1;
      ctx.beginPath();
      for (let j = 0; j < rows; j++) {
        for (let i = 0; i < cols; i++) {
          const k = j * cols + i;
          const r = size[k] / 2;
          const x = x0 + i * PITCH + shift[k * 2];
          const y = y0 + j * PITCH + shift[k * 2 + 1];
          ctx.moveTo(x + r, y);
          ctx.arc(x, y, r, 0, Math.PI * 2);
        }
      }
      ctx.fill();
    },
  };
};

/* ---------------------------------------------------------------------- */
/* Twist: every bar turns to point at the pointer, the near ones first.    */
/* ---------------------------------------------------------------------- */

const twist = (): Texture => {
  const PITCH = 22;
  const LENGTH = 11;
  const REST = Math.PI / 4;
  let cols = 0;
  let rows = 0;
  let x0 = 0;
  let y0 = 0;
  let angle = new Float32Array(0);
  let grow = new Float32Array(0);

  return {
    resize(width, height) {
      ({ cols, rows, x0, y0 } = grid(width, height, PITCH));
      angle = new Float32Array(cols * rows).fill(REST);
      grow = new Float32Array(cols * rows);
    },
    step(dt, pointer) {
      let moving = false;
      for (let j = 0; j < rows; j++) {
        for (let i = 0; i < cols; i++) {
          const k = j * cols + i;
          const cx = x0 + i * PITCH;
          const cy = y0 + j * PITCH;
          let target = REST;
          let targetGrow = 0;
          let tau = 0.35;
          if (pointer !== undefined) {
            const dx = pointer.x - cx;
            const dy = pointer.y - cy;
            const dist = Math.hypot(dx, dy);
            target = Math.atan2(dy, dx);
            // Close in, the bars lengthen; right under it they shrink away.
            targetGrow =
              smoothstep(240, 40, dist) * 0.9 - smoothstep(28, 0, dist) * 1.2;
            tau = 0.05 + dist / 1800;
          }
          const turn = lineTurn(angle[k], target);
          const next = target - turn * Math.exp(-dt / tau);
          const nextGrow = toward(grow[k], targetGrow, dt, tau);
          if (
            Math.abs(lineTurn(next, target)) > 0.002 ||
            Math.abs(nextGrow - targetGrow) > EPSILON
          ) {
            moving = true;
          }
          angle[k] = next;
          grow[k] = nextGrow;
        }
      }
      return moving;
    },
    draw(ctx, ink) {
      ctx.strokeStyle = ink;
      ctx.lineWidth = 2;
      ctx.lineCap = "round";
      ctx.beginPath();
      for (let j = 0; j < rows; j++) {
        for (let i = 0; i < cols; i++) {
          const k = j * cols + i;
          const half = Math.max(0, (LENGTH / 2) * (1 + grow[k]));
          const cx = x0 + i * PITCH;
          const cy = y0 + j * PITCH;
          const ux = Math.cos(angle[k]) * half;
          const uy = Math.sin(angle[k]) * half;
          ctx.moveTo(cx - ux, cy - uy);
          ctx.lineTo(cx + ux, cy + uy);
        }
      }
      ctx.stroke();
    },
  };
};

/* ---------------------------------------------------------------------- */
/* Blinds: thin vertical stripes twist face-on around the pointer, like   */
/* ribbons turning.                                                        */
/* ---------------------------------------------------------------------- */

const blinds = (): Texture => {
  const PITCH = 26;
  const THIN = 1.25;
  const WIDE = 24;
  const SPAN = 170;
  const REACH = 11;
  const SAMPLES = 40;
  let count = 0;
  let x0 = 0;
  let top = 0;
  let bottom = 0;
  let open = new Float32Array(0);
  let focusY = 0;

  return {
    resize(width, height) {
      count = Math.max(1, Math.floor(width / PITCH));
      x0 = (width - (count - 1) * PITCH) / 2;
      top = 0;
      bottom = height;
      open = new Float32Array(count);
      focusY = height / 2;
    },
    step(dt, pointer) {
      let moving = false;
      if (pointer !== undefined) {
        focusY = pointer.y;
      }
      for (let i = 0; i < count; i++) {
        let target = 0;
        let tau = 0.2;
        if (pointer !== undefined) {
          const u = Math.abs(x0 + i * PITCH - pointer.x) / PITCH;
          target = smoothstep(0, 1, 1 - u / REACH);
          tau = 0.03 + (0.14 - 0.03) * Math.min(u / REACH, 1);
        }
        const next = toward(open[i], target, dt, tau);
        if (Math.abs(next - target) > 0.001) {
          moving = true;
        }
        open[i] = next;
      }
      return moving;
    },
    draw(ctx, ink) {
      ctx.fillStyle = ink;
      ctx.beginPath();
      for (let i = 0; i < count; i++) {
        const cx = x0 + i * PITCH;
        if (open[i] < 0.002) {
          ctx.rect(cx - THIN / 2, top, THIN, bottom - top);
          continue;
        }
        const halfAt = (y: number) => {
          const dy = (y - focusY) / SPAN;
          const theta = open[i] * (Math.PI / 2) * Math.exp(-dy * dy);
          return (THIN + (WIDE - THIN) * Math.sin(theta)) / 2;
        };
        ctx.moveTo(cx - halfAt(top), top);
        for (let s = 1; s <= SAMPLES; s++) {
          const y = top + ((bottom - top) * s) / SAMPLES;
          ctx.lineTo(cx - halfAt(y), y);
        }
        for (let s = SAMPLES; s >= 0; s--) {
          const y = top + ((bottom - top) * s) / SAMPLES;
          ctx.lineTo(cx + halfAt(y), y);
        }
        ctx.closePath();
      }
      ctx.fill();
    },
  };
};

/* ---------------------------------------------------------------------- */
/* Cells: each cell holds a small square; near the pointer the squares    */
/* swell to fill their cells in a soft dome.                               */
/* ---------------------------------------------------------------------- */

const cells = (): Texture => {
  const PITCH = 18;
  const FULL = 15;
  const REST = 3;
  const RADIUS = 150;
  let cols = 0;
  let rows = 0;
  let x0 = 0;
  let y0 = 0;
  let size = new Float32Array(0);

  return {
    resize(width, height) {
      ({ cols, rows, x0, y0 } = grid(width, height, PITCH));
      size = new Float32Array(cols * rows).fill(REST);
    },
    step(dt, pointer) {
      let moving = false;
      for (let j = 0; j < rows; j++) {
        for (let i = 0; i < cols; i++) {
          const k = j * cols + i;
          let target = REST;
          let tau = 0.18;
          if (pointer !== undefined) {
            const dist = Math.hypot(
              pointer.x - (x0 + i * PITCH),
              pointer.y - (y0 + j * PITCH),
            );
            const dome = Math.sqrt(Math.max(0, 1 - (dist / RADIUS) ** 2));
            target = REST + (FULL - REST) * dome;
            tau = 0.05 + dist / 2400;
          }
          const next = toward(size[k], target, dt, tau);
          if (Math.abs(next - target) > EPSILON) {
            moving = true;
          }
          size[k] = next;
        }
      }
      return moving;
    },
    draw(ctx, ink) {
      ctx.fillStyle = ink;
      ctx.beginPath();
      for (let j = 0; j < rows; j++) {
        for (let i = 0; i < cols; i++) {
          const s = size[j * cols + i];
          ctx.roundRect(
            x0 + i * PITCH - s / 2,
            y0 + j * PITCH - s / 2,
            s,
            s,
            Math.min(2, s / 3),
          );
        }
      }
      ctx.fill();
    },
  };
};

/* ---------------------------------------------------------------------- */
/* Swell: dashes under the pointer swell to four times their thickness    */
/* and push their neighbours aside.                                        */
/* ---------------------------------------------------------------------- */

const swell = (): Texture => {
  const ROW = 11;
  const DASH = 16;
  const GAP = 8;
  const THIN = 1.5;
  const SIGMA_X = 120;
  const SIGMA_Y = 56;
  let cols = 0;
  let rows = 0;
  let x0 = 0;
  let y0 = 0;
  let amount = new Float32Array(0);
  let focus = { x: 0, y: 0 };

  return {
    resize(width, height) {
      cols = Math.ceil(width / (DASH + GAP)) + 2;
      rows = Math.max(1, Math.floor(height / ROW));
      x0 = -(DASH + GAP);
      y0 = (height - (rows - 1) * ROW) / 2;
      amount = new Float32Array(cols * rows);
      focus = { x: width / 2, y: height / 2 };
    },
    step(dt, pointer) {
      let moving = false;
      if (pointer !== undefined) {
        focus = pointer;
      }
      for (let j = 0; j < rows; j++) {
        const offset = j % 2 === 0 ? 0 : (DASH + GAP) / 2;
        for (let i = 0; i < cols; i++) {
          const k = j * cols + i;
          let target = 0;
          if (pointer !== undefined) {
            const dx = x0 + offset + i * (DASH + GAP) + DASH / 2 - pointer.x;
            const dy = y0 + j * ROW - pointer.y;
            target = Math.exp(
              -(dx * dx) / (2 * SIGMA_X * SIGMA_X) -
                (dy * dy) / (2 * SIGMA_Y * SIGMA_Y),
            );
          }
          const next = toward(amount[k], target, dt, pointer ? 0.08 : 0.22);
          if (Math.abs(next - target) > 0.002) {
            moving = true;
          }
          amount[k] = next;
        }
      }
      return moving;
    },
    draw(ctx, ink) {
      ctx.fillStyle = ink;
      ctx.beginPath();
      for (let j = 0; j < rows; j++) {
        const offset = j % 2 === 0 ? 0 : (DASH + GAP) / 2;
        for (let i = 0; i < cols; i++) {
          const k = j * cols + i;
          const a = amount[k];
          const thick = THIN * (1 + 3.5 * a);
          const x = x0 + offset + i * (DASH + GAP);
          const baseY = y0 + j * ROW;
          // Pushed away from the pointer's row, as far as the swell needs.
          const away = Math.sign(baseY - focus.y);
          const push =
            away *
            a *
            6 *
            Math.min(1, Math.abs(baseY - focus.y) / (SIGMA_Y * 0.6));
          ctx.roundRect(x, baseY + push - thick / 2, DASH, thick, thick / 2);
        }
      }
      ctx.fill();
    },
  };
};

export const TEXTURE_KINDS: Record<TextureKind, () => Texture> = {
  magnet,
  watch,
  twist,
  blinds,
  cells,
  swell,
};
