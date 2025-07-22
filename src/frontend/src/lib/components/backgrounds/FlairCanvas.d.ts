import type { EasingFunction } from "svelte/transition";

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
    | "ccw"
    | "perlin";
  speed: "slow" | "medium" | "fast" | number;
  intensity: "light" | "medium" | "strong" | number;
  size: "large" | "medium" | "small" | number;
}

export interface FlairBoxProps {
  bgType?: "dots" | "grid" | "noisedots";
  spacing?: "large" | "medium" | "small" | number;
  aspect?: "square" | "wide" | "ultrawide" | number;
  visibility?: "always" | "animation";
  dotSize?: "large" | "medium" | "small" | number;
  vignette?: "center" | "top" | "left" | "right" | "bottom" | "none";
  hoverAction?: "intense" | "minimal" | "none";
  springOrTween?:
    | {
        type: "spring";
        stiffness: "low" | "medium" | "high" | number;
        dampening: "low" | "medium" | "high" | number;
      }
    | {
        type: "tween";
        duration: "short" | "medium" | "long" | number;
        easing: EasingFunction;
      };
  display: "bgOnly" | "behindBox" | "insideBox";
  colorScheme: undefined | "dark" | "light";
  backgroundClasses?: string;
  foregroundClasses?: string;
  triggerAnimation?: (opts: FlairAnimationOptions) => void;
}
