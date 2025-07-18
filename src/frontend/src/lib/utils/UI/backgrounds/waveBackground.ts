import { Spring } from "svelte/motion";

export function createXYSprings(
  xCount: number,
  yCount: number,
  SpringCtor: typeof Spring,
  stiffness: number,
  damping: number,
) {
  return Array.from({ length: xCount }, (_, x) =>
    Array.from(
      { length: yCount },
      () => new SpringCtor({ x: 0, y: 0 }, { stiffness, damping }),
    ),
  );
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
  springs: (Spring<{ x: number; y: number }> | undefined)[][] | undefined,
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
  springs: (Spring<{ x: number; y: number }> | undefined)[][] | undefined,
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
  springs: (Spring<number> | undefined)[][] | undefined,
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
  springs: Spring<{ x: number; y: number }>[][],
  xSpacing: number,
  ySpacing: number,
  MOUSE_RADIUS: number,
  MOUSE_SCALAR: number,
  WAVE_SPEED: number,
  IMPULSE_DURATION: number,
  axis: "xy" | "x" | "y" = "xy",
) {
  const minX = Math.max(
    0,
    Math.floor((mouseX - offsetX - MOUSE_RADIUS) / xSpacing),
  );
  const maxX = Math.min(
    xPositions.length - 1,
    Math.ceil((mouseX - offsetX + MOUSE_RADIUS) / xSpacing),
  );
  const minY = Math.max(
    0,
    Math.floor((mouseY - offsetY - MOUSE_RADIUS) / ySpacing),
  );
  const maxY = Math.min(
    yPositions.length - 1,
    Math.ceil((mouseY - offsetY + MOUSE_RADIUS) / ySpacing),
  );

  for (let xIndex = minX; xIndex <= maxX; xIndex++) {
    for (let yIndex = minY; yIndex <= maxY; yIndex++) {
      const nodeX = xPositions[xIndex] + offsetX;
      const nodeY = yPositions[yIndex] + offsetY;
      const dx = nodeX - mouseX;
      const dy = nodeY - mouseY;
      const dist = Math.sqrt(dx * dx + dy * dy);

      if (dist < MOUSE_RADIUS) {
        const delay = dist * WAVE_SPEED;
        const falloff = Math.pow(1 - dist / MOUSE_RADIUS, 2);
        const strength = falloff * MOUSE_SCALAR * MOUSE_RADIUS;

        setTimeout(() => {
          if (strength > 0.01) {
            let xVal = (dx / dist) * strength;
            let yVal = (dy / dist) * strength;
            if (axis === "x") yVal = 0;
            if (axis === "y") xVal = 0;
            springs[xIndex][yIndex].target = { x: xVal, y: yVal };

            setTimeout(() => {
              springs[xIndex][yIndex].target = { x: 0, y: 0 };
            }, IMPULSE_DURATION);
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
  springs: Spring<number>[][],
  xSpacing: number,
  ySpacing: number,
  MOUSE_RADIUS: number,
  MOUSE_SCALAR: number,
  WAVE_SPEED: number,
  IMPULSE_DURATION: number,
) {
  const minX = Math.max(
    0,
    Math.floor((mouseX - offsetX - MOUSE_RADIUS) / xSpacing),
  );
  const maxX = Math.min(
    xPositions.length - 1,
    Math.ceil((mouseX - offsetX + MOUSE_RADIUS) / xSpacing),
  );
  const minY = Math.max(
    0,
    Math.floor((mouseY - offsetY - MOUSE_RADIUS) / ySpacing),
  );
  const maxY = Math.min(
    yPositions.length - 1,
    Math.ceil((mouseY - offsetY + MOUSE_RADIUS) / ySpacing),
  );

  for (let xIndex = minX; xIndex <= maxX; xIndex++) {
    for (let yIndex = minY; yIndex <= maxY; yIndex++) {
      const nodeX = xPositions[xIndex] + offsetX;
      const nodeY = yPositions[yIndex] + offsetY;
      const dx = nodeX - mouseX;
      const dy = nodeY - mouseY;
      const dist = Math.sqrt(dx * dx + dy * dy);

      if (dist < MOUSE_RADIUS) {
        const delay = dist * WAVE_SPEED;
        const falloff = Math.pow(1 - dist / MOUSE_RADIUS, 2);
        const strength = falloff * MOUSE_SCALAR * MOUSE_RADIUS;

        setTimeout(() => {
          if (strength > 0.01) {
            springs[xIndex][yIndex].target = dist * strength;
            setTimeout(() => {
              springs[xIndex][yIndex].target = 0;
            }, IMPULSE_DURATION);
          }
        }, delay);
      } else {
        springs[xIndex][yIndex].target = 0;
      }
    }
  }
}

export function createImpulse(
  mouseX: number,
  mouseY: number,
  xPositions: number[],
  yPositions: number[],
  offsetX: number,
  offsetY: number,
  springs: Spring<{ x: number; y: number }>[][],
  CLICK_RADIUS: number,
  IMPULSE_SCALAR: number,
  WAVE_SPEED: number,
  IMPULSE_DURATION: number,
  axis: "xy" | "x" | "y" = "xy",
) {
  for (let xIndex = 0; xIndex < xPositions.length; xIndex++) {
    for (let yIndex = 0; yIndex < yPositions.length; yIndex++) {
      const nodeX = xPositions[xIndex] + offsetX;
      const nodeY = yPositions[yIndex] + offsetY;
      const dx = nodeX - mouseX;
      const dy = nodeY - mouseY;
      const dist = Math.sqrt(dx * dx + dy * dy);

      const falloff = Math.max(0, 1 - dist / CLICK_RADIUS);
      const strength = Math.pow(falloff, 2) * IMPULSE_SCALAR * CLICK_RADIUS;
      const delay = dist * WAVE_SPEED;

      setTimeout(() => {
        if (strength > 0.01) {
          let xVal = (dx / dist) * strength;
          let yVal = (dy / dist) * strength;
          if (axis === "x") yVal = 0;
          if (axis === "y") xVal = 0;
          springs[xIndex][yIndex].target = { x: xVal, y: yVal };
          setTimeout(() => {
            springs[xIndex][yIndex].target = { x: 0, y: 0 };
          }, IMPULSE_DURATION);
        }
      }, delay);
    }
  }
}

export function createRotationalImpulse(
  mouseX: number,
  mouseY: number,
  xPositions: number[],
  yPositions: number[],
  offsetX: number,
  offsetY: number,
  springs: Spring<{ x: number; y: number }>[][],
  CLICK_RADIUS: number,
  IMPULSE_SCALAR: number,
  WAVE_SPEED: number,
  IMPULSE_DURATION: number,
  direction: "cw" | "ccw" = "cw", // clockwise or counterclockwise
) {
  for (let xIndex = 0; xIndex < xPositions.length; xIndex++) {
    for (let yIndex = 0; yIndex < yPositions.length; yIndex++) {
      const nodeX = xPositions[xIndex] + offsetX;
      const nodeY = yPositions[yIndex] + offsetY;
      const dx = nodeX - mouseX;
      const dy = nodeY - mouseY;
      const dist = Math.sqrt(dx * dx + dy * dy);

      const falloff = Math.max(0, 1 - dist / CLICK_RADIUS);
      const strength = Math.pow(falloff, 2) * IMPULSE_SCALAR * CLICK_RADIUS;
      const delay = dist * WAVE_SPEED;

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
          }, IMPULSE_DURATION);
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
  springs: Spring<{ x: number; y: number }>[][],
  CLICK_RADIUS: number,
  IMPULSE_SCALAR: number,
  WAVE_SPEED: number,
  IMPULSE_DURATION: number,
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
        const falloff = Math.max(0, 1 - dist / CLICK_RADIUS);
        const strength =
          Math.pow(falloff, 2) *
          IMPULSE_SCALAR *
          CLICK_RADIUS *
          directionalScale;
        const delay = dist * WAVE_SPEED;

        setTimeout(() => {
          if (strength > 0.01) {
            let xVal = (dx / dist) * strength;
            let yVal = (dy / dist) * strength;
            if (axis === "x") yVal = 0;
            if (axis === "y") xVal = 0;
            springs[xIndex][yIndex].target = { x: xVal, y: yVal };
            setTimeout(() => {
              springs[xIndex][yIndex].target = { x: 0, y: 0 };
            }, IMPULSE_DURATION);
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
  springs: Spring<number>[][],
  CLICK_RADIUS: number,
  IMPULSE_SCALAR: number,
  WAVE_SPEED: number,
  IMPULSE_DURATION: number,
) {
  for (let xIndex = 0; xIndex < xPositions.length; xIndex++) {
    for (let yIndex = 0; yIndex < yPositions.length; yIndex++) {
      const nodeX = xPositions[xIndex] + offsetX;
      const nodeY = yPositions[yIndex] + offsetY;
      const dx = nodeX - mouseX;
      const dy = nodeY - mouseY;
      const dist = Math.sqrt(dx * dx + dy * dy);

      const falloff = Math.max(0, 1 - dist / CLICK_RADIUS);
      const strength = Math.pow(falloff, 2) * IMPULSE_SCALAR * CLICK_RADIUS;
      const delay = dist * WAVE_SPEED;

      setTimeout(() => {
        if (strength > 0.01) {
          springs[xIndex][yIndex].target = dist * strength;
          setTimeout(() => {
            springs[xIndex][yIndex].target = 0;
          }, IMPULSE_DURATION);
        }
      }, delay);
    }
  }
}

export function resetNodes(springs: Spring<{ x: number; y: number }>[][]) {
  for (let x = 0; x < springs.length; x++) {
    for (let y = 0; y < springs[x].length; y++) {
      springs[x][y].target = { x: 0, y: 0 };
    }
  }
}

export function resetScalarNodes(springs: Spring<number>[][]) {
  for (let x = 0; x < springs.length; x++) {
    for (let y = 0; y < springs[x].length; y++) {
      springs[x][y].target = 0;
    }
  }
}
