import * as easingFunctions from "svelte/easing";

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
  nImpulses: "single" | "double";
  impulseEasing?: keyof typeof easingFunctions;
  // When true, omni-direction impulses (and the second impulse of
  // `nImpulses: "double"`) are added on top of the dot's current
  // spring target instead of replacing it. Used together with the
  // `repel` hover mode so a click wave starts from the displaced
  // positions and returns to them rather than snapping through the
  // neutral grid.
  additive?: boolean;
}

export interface FlairCanvasProps {
  bgType?: "dots" | "grid" | "noisedots";
  spacing?: "large" | "medium" | "small" | number;
  aspect?: "square" | "wide" | "ultrawide" | number;
  visibility?: "always" | "moving" | "maskwave" | "pausedmaskwave";
  dotSize?: "large" | "medium" | "small" | number;
  vignette?: "center" | "top" | "left" | "right" | "bottom" | "none";
  hoverAction?: "intense" | "minimal" | "repel" | "none";
  springOrTween?:
    | {
        type: "spring";
        stiffness: "low" | "medium" | "high" | number;
        dampening: "low" | "medium" | "high" | number;
      }
    | {
        type: "tween";
        duration: "short" | "medium" | "long" | number;
        easing: keyof typeof easingFunctions;
      };
  noiseTimeScale?: "fast" | "medium" | "slow" | number;
  enableRandomOpacity?: boolean;
  opacityNoiseScale?: "large" | "medium" | "small" | number;
  opacityNoiseMultiplier?: "large" | "medium" | "small" | number;
  enableRandomPointSize?: boolean;
  pointSizeNoiseScale?: "large" | "medium" | "small" | number;
  pointSizeNoiseMultiplier?: "large" | "medium" | "small" | number;
  maskWaveRampIn?: number;
  maskWaveRampOut?: number;
  maskWaveThickness?: "large" | "medium" | "small" | number;
  maskWaveMinValue?: number;
  maskWaveSpeedMultiplier?: number;
  maskWavePauseValue?: number | null;
  maskWaveOneWay?: boolean;
  customColor?: string;
  customColorMode?: "all" | "moving";
  containerHeight?: string;
  triggerAnimation?: (opts: FlairAnimationOptions) => Promise<void>;
  clearAnimation?: () => void;
}

export interface NodeMotion {
  motion: Spring<{ x: number; y: number }> | Tween<{ x: number; y: number }>;
  prev: { x: number; y: number };
  speed: number;
}
