import { DROP_WAVE_ANIMATION } from "$lib/components/backgrounds/constants";
import type { FlairAnimationOptions } from "$lib/components/backgrounds/FlairCanvas";

declare global {
  interface Window {
    triggerFlairCanvas?: (opts: FlairAnimationOptions) => void;
  }
}

export async function dropWaveAnimation() {
  await window.triggerFlairCanvas?.(DROP_WAVE_ANIMATION);
}
