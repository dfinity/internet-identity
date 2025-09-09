import type {} from "$lib/components/backgrounds/FlairCanvas.d.ts";
import { quadInOut } from "svelte/easing";
import { Spring, Tween } from "svelte/motion";
import { EasingFunction } from "svelte/transition";
import { PerlinNoise3D } from "./perlinNoise3d";
import {
  type FlairCanvasProps,
  NodeMotion,
  type FlairAnimationOptions,
} from "$lib/components/backgrounds/FlairCanvas";
import { nonNullish } from "@dfinity/utils";
import { tick } from "svelte";

////// ANIMATION //////
export function createXYSprings(
  xCount: number,
  yCount: number,
  SpringCtor: typeof Spring | typeof Tween,
  stiffness: number,
  damping: number,
) {
  if (SpringCtor instanceof Spring) {
    return Array.from({ length: xCount }, (_) =>
      Array.from(
        { length: yCount },
        () => new SpringCtor({ x: 0, y: 0 }, { stiffness, damping }),
      ),
    );
  } else {
    return Array.from({ length: xCount }, (_) =>
      Array.from(
        { length: yCount },
        () =>
          new SpringCtor({ x: 0, y: 0 }, { duration: 200, easing: quadInOut }),
      ),
    );
  }
}

export function createXYNodeMotions(
  xCount: number,
  yCount: number,
  SpringCtor: typeof Spring | typeof Tween,
  stiffness: number,
  damping: number,
): NodeMotion[][] {
  if (SpringCtor instanceof Spring) {
    return Array.from({ length: xCount }, (_) =>
      Array.from({ length: yCount }, () => {
        return {
          motion: new SpringCtor({ x: 0, y: 0 }, { stiffness, damping }),
          prev: { x: 0, y: 0 },
          speed: 0,
        };
      }),
    );
  } else {
    return Array.from({ length: xCount }, (_) =>
      Array.from({ length: yCount }, () => {
        return {
          motion: new SpringCtor(
            { x: 0, y: 0 },
            { duration: 200, easing: quadInOut },
          ),
          prev: { x: 0, y: 0 },
          speed: 0,
        };
      }),
    );
  }
}

export function createScalarSprings(
  xCount: number,
  yCount: number,
  stiffness: number,
  damping: number,
) {
  return Array.from({ length: xCount }, (_) =>
    Array.from({ length: yCount }, () => new Spring(0, { stiffness, damping })),
  );
}

export function leftPos(
  xPos: number,
  xIndex: number,
  yIndex: number,
  offsetX: number,
  springs: NodeMotion[][] | undefined,
) {
  if (
    springs === undefined ||
    springs[xIndex] === undefined ||
    springs[xIndex][yIndex] === undefined
  )
    return xPos + offsetX;
  return xPos + offsetX + springs[xIndex][yIndex].motion.current.x;
}

export function topPos(
  yPos: number,
  xIndex: number,
  yIndex: number,
  offsetY: number,
  springs: NodeMotion[][] | undefined,
) {
  if (
    springs === undefined ||
    springs[xIndex] === undefined ||
    springs[xIndex][yIndex] === undefined
  )
    return yPos + offsetY;
  return yPos + offsetY + springs[xIndex][yIndex].motion.current.y;
}

export function waveScale(
  xIndex: number,
  yIndex: number,
  springs: (Spring<number> | Tween<number> | undefined)[][] | undefined,
) {
  if (
    springs === undefined ||
    springs[xIndex] === undefined ||
    springs[xIndex][yIndex] === undefined
  )
    return 1;

  return 1 + springs[xIndex][yIndex].current * 0.01;
}

export function createContinuousWave(
  mouseX: number,
  mouseY: number,
  xPositions: number[],
  yPositions: number[],
  offsetX: number,
  offsetY: number,
  springs: NodeMotion[][],
  xSpacing: number,
  ySpacing: number,
  mouseRadius: number,
  mouseScalar: number,
  waveSpeed: number,
  impulseDuration: number,
  axis: "xy" | "x" | "y" = "xy",
) {
  const minX = Math.max(
    0,
    Math.floor((mouseX - offsetX - mouseRadius) / xSpacing),
  );
  const maxX = Math.min(
    xPositions.length - 1,
    Math.ceil((mouseX - offsetX + mouseRadius) / xSpacing),
  );
  const minY = Math.max(
    0,
    Math.floor((mouseY - offsetY - mouseRadius) / ySpacing),
  );
  const maxY = Math.min(
    yPositions.length - 1,
    Math.ceil((mouseY - offsetY + mouseRadius) / ySpacing),
  );

  for (let xIndex = minX; xIndex <= maxX; xIndex++) {
    for (let yIndex = minY; yIndex <= maxY; yIndex++) {
      const nodeX = xPositions[xIndex] + offsetX;
      const nodeY = yPositions[yIndex] + offsetY;
      const dx = nodeX - mouseX;
      const dy = nodeY - mouseY;
      const dist = Math.sqrt(dx * dx + dy * dy);

      if (dist < mouseRadius) {
        const delay = dist * waveSpeed;
        const falloff = Math.pow(1 - dist / mouseRadius, 2);
        const strength = falloff * mouseScalar * mouseRadius;

        setTimeout(() => {
          if (strength > 0.01) {
            let xVal = (dx / dist) * strength;
            let yVal = (dy / dist) * strength;
            if (axis === "x") yVal = 0;
            if (axis === "y") xVal = 0;
            springs[xIndex][yIndex].motion.target = { x: xVal, y: yVal };

            setTimeout(() => {
              springs[xIndex][yIndex].motion.target = { x: 0, y: 0 };
            }, impulseDuration);
          }
        }, delay);
      } else {
        springs[xIndex][yIndex].motion.target = { x: 0, y: 0 };
      }
    }
  }
}

export function createContinuousScalarWave(
  mouseX: number,
  mouseY: number,
  xPositions: number[],
  yPositions: number[],
  offsetX: number,
  offsetY: number,
  springs: Spring<number>[][] | Tween<number>[][],
  xSpacing: number,
  ySpacing: number,
  mouseRadius: number,
  mouseScalar: number,
  waveSpeed: number,
  impulseDuration: number,
) {
  const minX = Math.max(
    0,
    Math.floor((mouseX - offsetX - mouseRadius) / xSpacing),
  );
  const maxX = Math.min(
    xPositions.length - 1,
    Math.ceil((mouseX - offsetX + mouseRadius) / xSpacing),
  );
  const minY = Math.max(
    0,
    Math.floor((mouseY - offsetY - mouseRadius) / ySpacing),
  );
  const maxY = Math.min(
    yPositions.length - 1,
    Math.ceil((mouseY - offsetY + mouseRadius) / ySpacing),
  );

  for (let xIndex = minX; xIndex <= maxX; xIndex++) {
    for (let yIndex = minY; yIndex <= maxY; yIndex++) {
      const nodeX = xPositions[xIndex] + offsetX;
      const nodeY = yPositions[yIndex] + offsetY;
      const dx = nodeX - mouseX;
      const dy = nodeY - mouseY;
      const dist = Math.sqrt(dx * dx + dy * dy);

      if (dist < mouseRadius) {
        const delay = dist * waveSpeed;
        const falloff = Math.pow(1 - dist / mouseRadius, 2);
        const strength = falloff * mouseScalar * mouseRadius;

        setTimeout(() => {
          if (strength > 0.01) {
            springs[xIndex][yIndex].target = dist * strength;
            setTimeout(() => {
              springs[xIndex][yIndex].target = 0;
            }, impulseDuration);
          }
        }, delay);
      } else {
        springs[xIndex][yIndex].target = 0;
      }
    }
  }
}

export async function createImpulse(
  x: number,
  y: number,
  xPositions: number[],
  yPositions: number[],
  offsetX: number,
  offsetY: number,
  springs: NodeMotion[][],
  clickRadius: number,
  impulseScalar: number,
  waveSpeed: number,
  impulseDuration: number,
  direction: "omni" | "x" | "y",
  impulseEasing?: EasingFunction,
): Promise<void> {
  const promises: Promise<void>[] = [];

  for (let xIndex = 0; xIndex < xPositions.length; xIndex++) {
    for (let yIndex = 0; yIndex < yPositions.length; yIndex++) {
      const nodeX = xPositions[xIndex] + offsetX;
      const nodeY = yPositions[yIndex] + offsetY;
      const dx = nodeX - x;
      const dy = nodeY - y;
      const dist = Math.sqrt(dx * dx + dy * dy);

      if (dist < 1e-6) continue;

      const falloff = Math.max(0, 1 - dist / clickRadius);
      const strength = Math.pow(falloff, 2) * impulseScalar * clickRadius;

      // Easing for the delay
      const t = Math.min(dist / clickRadius, 1); // normalized distance [0,1]
      const eased = impulseEasing ? impulseEasing(t) : t; // or any easing function you prefer
      const delay = eased * clickRadius * waveSpeed; // or: delay = eased * dist * waveSpeed;

      const promise = new Promise<void>((resolve) => {
        setTimeout(() => {
          if (strength > 0.01) {
            let xVal = (dx / dist) * strength;
            let yVal = (dy / dist) * strength;
            if (direction === "x") yVal = 0;
            if (direction === "y") xVal = 0;
            springs[xIndex][yIndex].motion.target = { x: xVal, y: yVal };

            setTimeout(() => {
              springs[xIndex][yIndex].motion.target = { x: 0, y: 0 };
              resolve();
            }, impulseDuration);
          } else {
            resolve();
          }
        }, delay);
      });

      promises.push(promise);
    }
  }

  // Wait for all animations to complete
  await Promise.all(promises);
}

export async function createOneWayImpulse(
  x: number,
  y: number,
  xPositions: number[],
  yPositions: number[],
  offsetX: number,
  offsetY: number,
  springs: NodeMotion[][],
  clickRadius: number,
  impulseScalar: number,
  waveSpeed: number,
  direction: "omni" | "x" | "y",
  impulseEasing?: EasingFunction,
): Promise<void> {
  const promises: Promise<void>[] = [];

  for (let xIndex = 0; xIndex < xPositions.length; xIndex++) {
    for (let yIndex = 0; yIndex < yPositions.length; yIndex++) {
      const nodeX = xPositions[xIndex] + offsetX;
      const nodeY = yPositions[yIndex] + offsetY;
      const dx = nodeX - x;
      const dy = nodeY - y;
      const dist = Math.sqrt(dx * dx + dy * dy);

      if (dist < 1e-6) continue;

      const falloff = Math.max(0, 1 - dist / clickRadius);
      const strength = Math.pow(falloff, 2) * impulseScalar * clickRadius;

      // Easing for the delay
      const t = Math.min(dist / clickRadius, 1); // normalized distance [0,1]
      const eased = impulseEasing ? impulseEasing(t) : t; // or any easing function you prefer
      const delay = eased * clickRadius * waveSpeed; // or: delay = eased * dist * waveSpeed;

      const promise = new Promise<void>((resolve) => {
        setTimeout(() => {
          if (strength > 0.01) {
            let xVal = (dx / dist) * strength;
            let yVal = (dy / dist) * strength;
            if (direction === "x") yVal = 0;
            if (direction === "y") xVal = 0;
            springs[xIndex][yIndex].motion.target = { x: xVal, y: yVal };
            resolve();
          } else {
            resolve();
          }
        }, delay);
      });

      promises.push(promise);
    }
  }

  // Wait for all animations to complete
  await Promise.all(promises);
}

export async function createPerlinImpulse(
  x: number,
  y: number,
  xPositions: number[],
  yPositions: number[],
  offsetX: number,
  offsetY: number,
  springs: NodeMotion[][],
  clickRadius: number,
  impulseScalar: number,
  waveSpeed: number,
  impulseDuration: number,
  direction: "omni" | "x" | "y",
  perlin: PerlinNoise3D,
  perlinScale: number = 0.01, // scale for perlin input, tweak as needed
  perlinZ: number = 0, // optional z value for animation
  impulseEasing?: EasingFunction,
): Promise<void> {
  const promises: Promise<void>[] = [];

  for (let xIndex = 0; xIndex < xPositions.length; xIndex++) {
    for (let yIndex = 0; yIndex < yPositions.length; yIndex++) {
      const nodeX = xPositions[xIndex] + offsetX;
      const nodeY = yPositions[yIndex] + offsetY;
      const dx = nodeX - x;
      const dy = nodeY - y;
      const dist = Math.sqrt(dx * dx + dy * dy);

      if (dist < 1e-6) continue;

      const falloff = Math.max(0, 1 - dist / clickRadius);
      const strength = Math.pow(falloff, 2) * impulseScalar * clickRadius;

      // Perlin noise value for this node's position
      const perlinValue = perlin.get(
        nodeX * perlinScale,
        nodeY * perlinScale,
        perlinZ,
      );
      // Map perlin [0,1] to [-1,1]
      const perlinMapped = perlinValue - 0.5;

      // Easing for the delay
      const t = Math.min(dist / clickRadius, 1);
      const eased = impulseEasing ? impulseEasing(t) : t;
      const delay = eased * clickRadius * waveSpeed;

      const promise = new Promise<void>((resolve) => {
        setTimeout(() => {
          // Only apply if strength is significant
          if (Math.abs(strength * perlinMapped) > 0.01) {
            // Outward if perlinMapped > 0, inward if < 0
            let xVal = (dx / dist) * strength * perlinMapped * 30;
            let yVal = (dy / dist) * strength * perlinMapped * 30;
            if (direction === "x") yVal = 0;
            if (direction === "y") xVal = 0;
            springs[xIndex][yIndex].motion.target = { x: xVal, y: yVal };
            setTimeout(() => {
              springs[xIndex][yIndex].motion.target = { x: 0, y: 0 };
              resolve();
            }, impulseDuration);
          } else {
            resolve();
          }
        }, delay);
      });

      promises.push(promise);
    }
  }

  // Wait for all animations to complete
  await Promise.all(promises);
}

export async function createRotationalImpulse(
  x: number,
  y: number,
  xPositions: number[],
  yPositions: number[],
  offsetX: number,
  offsetY: number,
  springs: NodeMotion[][],
  clickRadius: number,
  impulseScalar: number,
  waveSpeed: number,
  impulseDuration: number,
  direction: "cw" | "ccw" = "cw", // clockwise or counterclockwise
  impulseEasing?: EasingFunction,
): Promise<void> {
  const promises: Promise<void>[] = [];

  for (let xIndex = 0; xIndex < xPositions.length; xIndex++) {
    for (let yIndex = 0; yIndex < yPositions.length; yIndex++) {
      const nodeX = xPositions[xIndex] + offsetX;
      const nodeY = yPositions[yIndex] + offsetY;
      const dx = nodeX - x;
      const dy = nodeY - y;
      const dist = Math.sqrt(dx * dx + dy * dy);

      const falloff = Math.max(0, 1 - dist / clickRadius);
      const strength = Math.pow(falloff, 2) * impulseScalar * clickRadius;
      // Easing for the delay
      const t = Math.min(dist / clickRadius, 1); // normalized distance [0,1]
      const eased = impulseEasing ? impulseEasing(t) : t;
      const delay = eased * clickRadius * waveSpeed;

      const promise = new Promise<void>((resolve) => {
        setTimeout(() => {
          if (strength > 0.01 && dist > 0.0001) {
            // Perpendicular vector: (-dy, dx) for ccw, (dy, -dx) for cw
            let perpX, perpY;
            if (direction === "cw") {
              perpX = dy / dist;
              perpY = -dx / dist;
            } else {
              perpX = -dy / dist;
              perpY = dx / dist;
            }
            springs[xIndex][yIndex].motion.target = {
              x: perpX * strength,
              y: perpY * strength,
            };
            setTimeout(() => {
              springs[xIndex][yIndex].motion.target = { x: 0, y: 0 };
              resolve();
            }, impulseDuration);
          } else {
            resolve();
          }
        }, delay);
      });

      promises.push(promise);
    }
  }

  // Wait for all animations to complete
  await Promise.all(promises);
}

export async function createDirectionalImpulse(
  mouseX: number,
  mouseY: number,
  xPositions: number[],
  yPositions: number[],
  offsetX: number,
  offsetY: number,
  springs: NodeMotion[][],
  clickRadius: number,
  impulseScalar: number,
  waveSpeed: number,
  impulseDuration: number,
  direction: "up" | "down" | "left" | "right",
  axis: "xy" | "x" | "y" = "xy",
  impulseEasing?: EasingFunction,
): Promise<void> {
  const promises: Promise<void>[] = [];

  for (let xIndex = 0; xIndex < xPositions.length; xIndex++) {
    for (let yIndex = 0; yIndex < yPositions.length; yIndex++) {
      const nodeX = xPositions[xIndex] + offsetX;
      const nodeY = yPositions[yIndex] + offsetY;
      const dx = nodeX - mouseX;
      const dy = nodeY - mouseY;
      const dist = Math.sqrt(dx * dx + dy * dy);

      // Directional filtering and scaling
      let directionalScale = 0;
      switch (direction) {
        case "up":
          if (dy < 0) directionalScale = -dy / dist; // sin(theta), negative dy is up
          break;
        case "down":
          if (dy > 0) directionalScale = dy / dist; // sin(theta), positive dy is down
          break;
        case "left":
          if (dx < 0) directionalScale = -dx / dist; // cos(theta), negative dx is left
          break;
        case "right":
          if (dx > 0) directionalScale = dx / dist; // cos(theta), positive dx is right
          break;
      }

      // Only apply impulse if in the correct direction and not at the mouse position
      if (directionalScale > 0 && dist > 0.0001) {
        const falloff = Math.max(0, 1 - dist / clickRadius);
        const strength =
          Math.pow(falloff, 2) * impulseScalar * clickRadius * directionalScale;
        // Easing for the delay
        const t = Math.min(dist / clickRadius, 1); // normalized distance [0,1]
        const eased = impulseEasing ? impulseEasing(t) : t;
        const delay = eased * clickRadius * waveSpeed;

        const promise = new Promise<void>((resolve) => {
          setTimeout(() => {
            if (strength > 0.01) {
              let xVal = (dx / dist) * strength;
              let yVal = (dy / dist) * strength;
              if (axis === "x") yVal = 0;
              if (axis === "y") xVal = 0;
              springs[xIndex][yIndex].motion.target = { x: xVal, y: yVal };
              setTimeout(() => {
                springs[xIndex][yIndex].motion.target = { x: 0, y: 0 };
                resolve();
              }, impulseDuration);
            } else {
              resolve();
            }
          }, delay);
        });

        promises.push(promise);
      }
    }
  }

  // Wait for all animations to complete
  await Promise.all(promises);
}

export function createScalarImpulse(
  mouseX: number,
  mouseY: number,
  xPositions: number[],
  yPositions: number[],
  offsetX: number,
  offsetY: number,
  springs: Spring<number>[][] | Tween<number>[][],
  clickRadius: number,
  impulseScalar: number,
  waveSpeed: number,
  impulseDuration: number,
) {
  for (let xIndex = 0; xIndex < xPositions.length; xIndex++) {
    for (let yIndex = 0; yIndex < yPositions.length; yIndex++) {
      const nodeX = xPositions[xIndex] + offsetX;
      const nodeY = yPositions[yIndex] + offsetY;
      const dx = nodeX - mouseX;
      const dy = nodeY - mouseY;
      const dist = Math.sqrt(dx * dx + dy * dy);

      const falloff = Math.max(0, 1 - dist / clickRadius);
      const strength = Math.pow(falloff, 2) * impulseScalar * clickRadius;
      const delay = dist * waveSpeed;

      setTimeout(() => {
        if (strength > 0.01) {
          springs[xIndex][yIndex].target = dist * strength;
          setTimeout(() => {
            springs[xIndex][yIndex].target = 0;
          }, impulseDuration);
        }
      }, delay);
    }
  }
}

export function resetNodes(springs: NodeMotion[][]) {
  for (let x = 0; x < springs.length; x++) {
    for (let y = 0; y < springs[x].length; y++) {
      springs[x][y].motion.target = { x: 0, y: 0 };
    }
  }
}

export function resetScalarNodes(
  springs: Spring<number>[][] | Tween<number>[][],
) {
  for (let x = 0; x < springs.length; x++) {
    for (let y = 0; y < springs[x].length; y++) {
      springs[x][y].target = 0;
    }
  }
}

export function gridPath(x1: number, y1: number, x2: number, y2: number) {
  const cx = (x1 + x2) / 2;
  const cy = (y1 + y2) / 2;

  return `M ${x1} ${y1} C ${cx} ${cy}, ${cx} ${cy}, ${x2} ${y2}`;
}

export function getImpulseLocation(
  location: FlairAnimationOptions["location"],
  innerWidth: number,
  innerHeight: number,
) {
  switch (location) {
    case "top":
      return { x: innerWidth / 2, y: 0 };
    case "left":
      return { x: 0, y: innerHeight / 2 };
    case "right":
      return { x: innerWidth, y: innerHeight / 2 };
    case "bottom":
      return { x: innerWidth / 2, y: innerHeight };
    case "topLeft":
      return { x: 0, y: 0 };
    case "topRight":
      return { x: innerWidth, y: 0 };
    case "bottomLeft":
      return { x: 0, y: innerHeight };
    case "bottomRight":
      return { x: innerWidth, y: innerHeight };
    case "center":
      return { x: innerWidth / 2, y: innerHeight / 2 };
    default:
      if (
        typeof location === "object" &&
        location !== null &&
        typeof location.x === "number" &&
        typeof location.y === "number"
      ) {
        return location;
      }
      return { x: innerWidth / 2, y: innerHeight / 2 };
  }
}

////// SVG //////

export function getVignetteConfig(
  vignette: FlairCanvasProps["vignette"],
  innerHeight: number | undefined,
  innerWidth: number | undefined,
) {
  // Margin from the edge of the viewport
  const margin = 0;
  // How much smaller the vignette is compared to the viewport
  const scale = 0.5;

  if (innerHeight === undefined || innerWidth === undefined) {
    return { cx: 0, cy: 0, rx: 0, ry: 0 };
  }

  const w =
    vignette === "top" || vignette === "bottom" ? innerWidth * 2 : innerWidth;
  const h =
    vignette === "left" || vignette === "right" ? innerHeight * 2 : innerHeight;
  let rx = (w * scale) / 2;
  let ry = (h * scale) / 2;

  let cx = innerWidth / 2;
  let cy = innerHeight / 2;

  switch (vignette) {
    case "top":
      cy = 0;

      break;
    case "bottom":
      cy = h - margin;

      break;
    case "left":
      cx = margin;

      break;
    case "right":
      cx = w - margin;

      break;
    case "center":
      rx = w * scale;
      ry = h * scale;
      break;
    default:
      // already centered
      break;
  }

  return { cx, cy, rx, ry };
}

///////// CANVAS ////////

export function drawNodes(
  xPositions: number[],
  yPositions: number[],
  offsetX: number,
  offsetY: number,
  springs: NodeMotion[][],
  radius: number,
  visibility: FlairCanvasProps["visibility"],
  ctx: CanvasRenderingContext2D,
  customHexColor?: string,
  customColorMode?: "all" | "moving",
  opacityNoise?: {
    noise: PerlinNoise3D;
    noiseScale: number;
    multiplier: number;
    noiseTime: number;
  } | null,
  pointSizeNoise?: {
    noise: PerlinNoise3D;
    noiseScale: number;
    multiplier: number;
    noiseTime: number;
  } | null,
) {
  const body = document.querySelector("body");
  if (!body) return;
  const color =
    nonNullish(customHexColor) &&
    customHexColor.length > 0 &&
    customColorMode === "all"
      ? customHexColor
      : getComputedStyle(body).getPropertyValue("--fg-primary").trim();
  xPositions.forEach((xPos, xIndex) => {
    yPositions.forEach((yPos, yIndex) => {
      ctx.beginPath();
      if (pointSizeNoise) {
        const pointSizeScalar =
          pointSizeNoise.noise.get(xPos, yPos, pointSizeNoise.noiseTime) *
          pointSizeNoise.multiplier;

        ctx.ellipse(
          leftPos(xPos, xIndex, yIndex, offsetX, springs),
          topPos(yPos, xIndex, yIndex, offsetY, springs),
          radius * pointSizeScalar,
          radius * pointSizeScalar,
          0,
          0,
          360,
          false,
        );
      } else {
        ctx.ellipse(
          leftPos(xPos, xIndex, yIndex, offsetX, springs),
          topPos(yPos, xIndex, yIndex, offsetY, springs),
          radius,
          radius,
          0,
          0,
          360,
          false,
        );
      }
      ctx.fillStyle = color;

      if (visibility === "moving") {
        const speed = springs[xIndex][yIndex].speed;
        ctx.fillStyle = hexToRgba(
          ctx.fillStyle,
          speed !== undefined ? speed : 0,
        );
      }
      if (customColorMode === "moving" && visibility !== "moving") {
        const speed = springs[xIndex][yIndex].speed ?? 0;
        ctx.fillStyle = interpolateColor(
          color,
          customHexColor ?? "#000000",
          Math.min(speed, 1),
        );
      }
      if (opacityNoise) {
        const opacityScalar =
          opacityNoise.noise.get(
            xPos * opacityNoise.noiseScale,
            yPos * opacityNoise.noiseScale,
            opacityNoise.noiseTime,
          ) * opacityNoise.multiplier;
        if (
          typeof ctx.fillStyle === "string" &&
          ctx.fillStyle.startsWith("rgba")
        ) {
          // Extract rgba values
          const match = ctx.fillStyle.match(
            /rgba\(\s*(\d+),\s*(\d+),\s*(\d+),\s*([0-9.]+)\s*\)/,
          );
          if (match) {
            const [, r, g, b, a] = match;
            const newAlpha = Math.max(
              0,
              Math.min(1, parseFloat(a) * opacityScalar),
            );
            ctx.fillStyle = `rgba(${r},${g},${b},${newAlpha})`;
          }
        } else {
          ctx.fillStyle = hexToRgba(ctx.fillStyle as string, opacityScalar);
        }
      }
      ctx.fill();
      ctx.closePath();
    });
  });
}

export function drawVignetteMask(
  width: number,
  height: number,
  vignetteConfig: { cx: number; cy: number; rx: number; ry: number },
  ctx: CanvasRenderingContext2D,
) {
  // Create a radial gradient for the vignette
  const { cx, cy, rx, ry } = vignetteConfig;

  // Calculate center in pixels
  const centerX = cx;
  const centerY = cy;
  // Use the larger of rx/ry for the gradient radius
  const maxRadius = Math.max(rx, ry);

  // Create a radial gradient
  const gradient = ctx.createRadialGradient(
    centerX,
    centerY,
    0, // inner circle (center, radius 0)
    centerX,
    centerY,
    maxRadius, // outer circle (center, max radius)
  );
  gradient.addColorStop(0, "rgba(0,0,0,0)"); // fully transparent at center
  gradient.addColorStop(0.5, "rgba(0,0,0,0)");
  gradient.addColorStop(0.7, "rgba(0,0,0,0.2)");
  gradient.addColorStop(1, "rgba(0,0,0,1)"); // mostly opaque at edge

  // Optionally, blur the gradient by drawing to an offscreen canvas and using ctx.filter = 'blur(...)'
  // For most use cases, the gradient alone is enough.

  // Draw the gradient over the canvas
  ctx.save();
  ctx.globalCompositeOperation = "destination-out";
  ctx.fillStyle = gradient;
  ctx.fillRect(0, 0, width, height);
  ctx.restore();
}

export async function createTweenedWave(
  motionController: Tween<number>,
  duration: number,
  from = 0,
): Promise<void> {
  await motionController.set(from, { duration: 0 });
  await tick();
  await motionController.set(1, { duration });
}

export async function createPausedTweenedWave(
  motionController: Tween<number>,
  duration: number,
  from = 0,
  pauseValue: number,
): Promise<void> {
  await motionController.set(from, { duration: 0 });
  await tick();
  await motionController.set(pauseValue, { duration });
}

export function drawMovingRingMask(
  width: number,
  height: number,
  progress: number, // 0..1
  thickness: number,
  ctx: CanvasRenderingContext2D,
  rampIn: number = 0.1, // e.g. 0.1 means ramp up over first 10% of progress
  rampOut: number = 0.9, // e.g. 0.9 means ramp down over last 10% of progress
  minValue: number = 0, // so we don't start in dead center
) {
  // Clamp rampIn and rampOut
  rampIn = Math.max(minValue, Math.min(1, rampIn));
  rampOut = Math.max(0, Math.min(1, rampOut));
  if (rampOut < rampIn) rampOut = rampIn;

  // Calculate overall opacity based on ramps
  let opacity = 1;
  if (progress < minValue + rampIn && minValue + rampIn > minValue) {
    opacity = progress / (rampIn + minValue);
  } else if (progress > rampOut && rampOut < 1) {
    opacity = 1 - (progress - rampOut) / (1 - rampOut);
  }
  opacity = Math.max(0, Math.min(1, opacity));

  // Center of the canvas
  const cx = width / 2;
  const cy = height / 2;
  // Maximum possible radius (to the closest edge)
  const maxRadius = getHypotenuse(width, height);

  // Clamp progress to [0, 1]
  const t = Math.max(0, Math.min(1, progress));

  // The radius of the center of the ring
  const ringRadius = t * maxRadius + minValue;

  // The inner and outer radii of the ring
  const innerRadius = Math.max(0, ringRadius);
  const outerRadius = ringRadius + thickness;

  // Create a radial gradient for the ring
  const gradient = ctx.createRadialGradient(
    cx,
    cy,
    innerRadius,
    cx,
    cy,
    outerRadius,
  );
  // Transparent inside the inner radius
  gradient.addColorStop(0, "rgba(0,0,0,1)");
  // Start of the ring
  gradient.addColorStop(innerRadius / outerRadius, "rgba(0,0,0,1)");
  // Middle of the ring (opaque, with ramped opacity)
  gradient.addColorStop(
    (innerRadius + (outerRadius - innerRadius) * 0.5) / outerRadius,
    `rgba(0,0,0,${1 - opacity})`,
  );
  // End of the ring (fade out)
  gradient.addColorStop(1, "rgba(0,0,0,1)");

  ctx.save();
  ctx.globalCompositeOperation = "destination-out";
  ctx.fillStyle = gradient;
  ctx.fillRect(0, 0, width, height);
  ctx.restore();
}

function hexToRgba(hex: string, alpha: number): string {
  // Remove leading #
  hex = hex.replace(/^#/, "");
  // Handle shorthand hex (#abc)
  if (hex.length === 3) {
    hex = hex
      .split("")
      .map((x) => x + x)
      .join("");
  }
  const num = parseInt(hex, 16);
  const r = (num >> 16) & 255;
  const g = (num >> 8) & 255;
  const b = num & 255;
  return `rgba(${r}, ${g}, ${b}, ${alpha})`;
}

export function getHypotenuse(x: number, y: number) {
  return Math.sqrt((x / 2) ** 2 + (y / 2) ** 2);
}

function interpolateColor(
  color1: string,
  color2: string,
  factor: number,
): string {
  // Parse both colors to RGB
  const rgb1 = parseColor(color1);
  const rgb2 = parseColor(color2);
  // Interpolate each channel
  const r = Math.round(rgb1.r + (rgb2.r - rgb1.r) * factor);
  const g = Math.round(rgb1.g + (rgb2.g - rgb1.g) * factor);
  const b = Math.round(rgb1.b + (rgb2.b - rgb1.b) * factor);
  const a = rgb1.a + (rgb2.a - rgb1.a) * factor;
  return `rgba(${r},${g},${b},${a})`;
}

// Helper to parse hex or rgba color strings
function parseColor(color: string): {
  r: number;
  g: number;
  b: number;
  a: number;
} {
  color = color.trim();
  if (color.startsWith("#")) {
    // Hex to RGBA
    let hex = color.slice(1);
    if (hex.length === 3)
      hex = hex
        .split("")
        .map((x) => x + x)
        .join("");
    const num = parseInt(hex, 16);
    return {
      r: (num >> 16) & 255,
      g: (num >> 8) & 255,
      b: num & 255,
      a: 1,
    };
  } else if (color.startsWith("rgba")) {
    const match = color.match(
      /rgba\(\s*(\d+),\s*(\d+),\s*(\d+),\s*([0-9.]+)\s*\)/,
    );
    if (match) {
      return {
        r: parseInt(match[1]),
        g: parseInt(match[2]),
        b: parseInt(match[3]),
        a: parseFloat(match[4]),
      };
    }
  } else if (color.startsWith("rgb")) {
    const match = color.match(/rgb\(\s*(\d+),\s*(\d+),\s*(\d+)\s*\)/);
    if (match) {
      return {
        r: parseInt(match[1]),
        g: parseInt(match[2]),
        b: parseInt(match[3]),
        a: 1,
      };
    }
  }
  // Fallback: black
  return { r: 0, g: 0, b: 0, a: 1 };
}

export const clearWave = async (
  motionController: Tween<number>,
  springs: NodeMotion[][],
): Promise<void> => {
  await motionController.set(0, { duration: 0 });
  resetNodes(springs);
};
