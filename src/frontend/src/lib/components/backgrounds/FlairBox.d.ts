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
    | "ccw";
  speed: "slow" | "medium" | "fast";
  intensity: "light" | "medium" | "strong";
  size: "large" | "medium" | "small";
}

export interface FlairBoxProps {
  bgType?: "dots" | "grid" | "noisedots";
  spacing?: "large" | "medium" | "small";
  aspect?: "square" | "wide" | "ultrawide";
  visibility?: "always" | "animation";
  dotSize?: "large" | "medium" | "small";
  vignette?: "center" | "top" | "left" | "right" | "bottom" | "none";
  hoverAction?: "intense" | "minimal" | "none";
  springOrTween?: "spring" | "tween";
  backgroundClasses?: string;
  foregroundClasses?: string;
  triggerAnimation?: (opts: FlairAnimationOptions) => void;
}
