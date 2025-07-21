import {
  FlairAnimationOptions,
  FlairBoxProps,
} from "$lib/components/backgrounds/FlairBox.svelte";
import { quadInOut } from "svelte/easing";
import { Spring, Tween } from "svelte/motion";

export function createXYSprings(
  xCount: number,
  yCount: number,
  SpringCtor: typeof Spring | typeof Tween,
  stiffness: number,
  damping: number,
) {
  if (SpringCtor instanceof Spring) {
    return Array.from({ length: xCount }, (_, x) =>
      Array.from(
        { length: yCount },
        () => new SpringCtor({ x: 0, y: 0 }, { stiffness, damping }),
      ),
    );
  } else {
    return Array.from({ length: xCount }, (_, x) =>
      Array.from(
        { length: yCount },
        () =>
          new SpringCtor({ x: 0, y: 0 }, { duration: 200, easing: quadInOut }),
      ),
    );
  }
}

export function createScalarSprings(
  xCount: number,
  yCount: number,
  stiffness: number,
  damping: number,
) {
  return Array.from({ length: xCount }, (_, x) =>
    Array.from({ length: yCount }, () => new Spring(0, { stiffness, damping })),
  );
}

export function leftPos(
  xPos: number,
  xIndex: number,
  yIndex: number,
  offsetX: number,
  springs:
    | (
        | Spring<{ x: number; y: number }>
        | Tween<{ x: number; y: number }>
        | undefined
      )[][]
    | undefined,
) {
  if (
    springs === undefined ||
    springs[xIndex] === undefined ||
    springs[xIndex][yIndex] === undefined
  )
    return xPos + offsetX;
  return xPos + offsetX + springs[xIndex][yIndex].current.x;
}

export function topPos(
  yPos: number,
  xIndex: number,
  yIndex: number,
  offsetY: number,
  springs:
    | (
        | Spring<{ x: number; y: number }>
        | Tween<{ x: number; y: number }>
        | undefined
      )[][]
    | undefined,
) {
  if (
    springs === undefined ||
    springs[xIndex] === undefined ||
    springs[xIndex][yIndex] === undefined
  )
    return yPos + offsetY;
  return yPos + offsetY + springs[xIndex][yIndex].current.y;
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
  springs: (
    | Spring<{ x: number; y: number }>
    | Tween<{ x: number; y: number }>
  )[][],
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
            springs[xIndex][yIndex].target = { x: xVal, y: yVal };

            setTimeout(() => {
              springs[xIndex][yIndex].target = { x: 0, y: 0 };
            }, impulseDuration);
          }
        }, delay);
      } else {
        springs[xIndex][yIndex].target = { x: 0, y: 0 };
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

export function createImpulse(
  x: number,
  y: number,
  xPositions: number[],
  yPositions: number[],
  offsetX: number,
  offsetY: number,
  springs: (
    | Spring<{ x: number; y: number }>
    | Tween<{ x: number; y: number }>
  )[][],
  clickRadius: number,
  impulseScalar: number,
  waveSpeed: number,
  impulseDuration: number,
  direction: "omni" | "x" | "y",
) {
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
      const delay = dist * waveSpeed;

      setTimeout(() => {
        if (strength > 0.01) {
          let xVal = (dx / dist) * strength;
          let yVal = (dy / dist) * strength;
          if (direction === "x") yVal = 0;
          if (direction === "y") xVal = 0;
          springs[xIndex][yIndex].target = { x: xVal, y: yVal };
          setTimeout(() => {
            springs[xIndex][yIndex].target = { x: 0, y: 0 };
          }, impulseDuration);
        }
      }, delay);
    }
  }
}

export function createRotationalImpulse(
  x: number,
  y: number,
  xPositions: number[],
  yPositions: number[],
  offsetX: number,
  offsetY: number,
  springs: (
    | Spring<{ x: number; y: number }>
    | Tween<{ x: number; y: number }>
  )[][],
  clickRadius: number,
  impulseScalar: number,
  waveSpeed: number,
  impulseDuration: number,
  direction: "cw" | "ccw" = "cw", // clockwise or counterclockwise
) {
  for (let xIndex = 0; xIndex < xPositions.length; xIndex++) {
    for (let yIndex = 0; yIndex < yPositions.length; yIndex++) {
      const nodeX = xPositions[xIndex] + offsetX;
      const nodeY = yPositions[yIndex] + offsetY;
      const dx = nodeX - x;
      const dy = nodeY - y;
      const dist = Math.sqrt(dx * dx + dy * dy);

      const falloff = Math.max(0, 1 - dist / clickRadius);
      const strength = Math.pow(falloff, 2) * impulseScalar * clickRadius;
      const delay = dist * waveSpeed;

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
          springs[xIndex][yIndex].target = {
            x: perpX * strength,
            y: perpY * strength,
          };
          setTimeout(() => {
            springs[xIndex][yIndex].target = { x: 0, y: 0 };
          }, impulseDuration);
        }
      }, delay);
    }
  }
}

export function createDirectionalImpulse(
  mouseX: number,
  mouseY: number,
  xPositions: number[],
  yPositions: number[],
  offsetX: number,
  offsetY: number,
  springs: (
    | Spring<{ x: number; y: number }>
    | Tween<{ x: number; y: number }>
  )[][],
  clickRadius: number,
  impulseScalar: number,
  waveSpeed: number,
  impulseDuration: number,
  direction: "up" | "down" | "left" | "right",
  axis: "xy" | "x" | "y" = "xy",
) {
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
        const delay = dist * waveSpeed;

        setTimeout(() => {
          if (strength > 0.01) {
            let xVal = (dx / dist) * strength;
            let yVal = (dy / dist) * strength;
            if (axis === "x") yVal = 0;
            if (axis === "y") xVal = 0;
            springs[xIndex][yIndex].target = { x: xVal, y: yVal };
            setTimeout(() => {
              springs[xIndex][yIndex].target = { x: 0, y: 0 };
            }, impulseDuration);
          }
        }, delay);
      }
    }
  }
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

export function resetNodes(
  springs: (
    | Spring<{ x: number; y: number }>
    | Tween<{ x: number; y: number }>
  )[][],
) {
  for (let x = 0; x < springs.length; x++) {
    for (let y = 0; y < springs[x].length; y++) {
      springs[x][y].target = { x: 0, y: 0 };
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

export function getVignetteConfig(
  vignette: FlairBoxProps["vignette"],
  innerHeight: number | undefined,
  innerWidth: number | undefined,
) {
  // Margin from the edge of the viewport
  const margin = 0;
  // How much smaller the vignette is compared to the viewport
  const scale = 0.9;

  if (innerHeight === undefined || innerWidth === undefined) {
    return { cx: 0, cy: 0, rx: 0, ry: 0 };
  }

  const w =
    vignette === "top" || vignette === "bottom" ? innerWidth * 2 : innerWidth;
  const h =
    vignette === "left" || vignette === "right" ? innerHeight * 2 : innerHeight;
  const rx = (w * scale) / 2;
  const ry = (h * scale) / 2;

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
    default:
      // already centered
      break;
  }

  return { cx, cy, rx, ry };
}
