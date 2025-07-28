import { DROP_WAVE_ANIMATION } from "$lib/components/backgrounds/constants";
import type { FlairAnimationOptions } from "$lib/components/backgrounds/FlairCanvas";

declare global {
  interface Window {
    triggerWaveCanvas?: (opts: FlairAnimationOptions) => void;
  }
}

export async function dropWaveAnimation() {
  await window.triggerWaveCanvas?.(DROP_WAVE_ANIMATION);
}
