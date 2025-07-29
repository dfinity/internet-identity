import { DROP_WAVE_ANIMATION } from "$lib/components/backgrounds/constants";
import type { FlairAnimationOptions } from "$lib/components/backgrounds/FlairCanvas";

type TriggerFunction = (opts: FlairAnimationOptions) => Promise<void>;

class WaveAnimationStore {
  private triggerFunction: TriggerFunction | null = null;

  /**
   * Register a trigger function from a WaveCanvas component
   * @param triggerFn The trigger function that handles wave animations
   */
  registerTrigger(triggerFn: TriggerFunction): void {
    this.triggerFunction = triggerFn;
  }

  /**
   * Unregister the trigger function when component unmounts
   */
  unregisterTrigger(): void {
    this.triggerFunction = null;
  }

  /**
   * Trigger the drop wave animation
   * @returns Promise that resolves when the animation completes
   */
  async dropWaveAnimation(): Promise<void> {
    if (this.triggerFunction) {
      console.log("Triggering drop wave animation");
      await this.triggerFunction(DROP_WAVE_ANIMATION);
      console.log("Drop wave animation completed");
    }
    // If no trigger function is registered, resolve immediately (graceful degradation)
  }
}

const waveAnimationStore = new WaveAnimationStore();

export const dropWaveAnimation = (): Promise<void> =>
  waveAnimationStore.dropWaveAnimation();

export const registerWaveAnimationTrigger = (
  triggerFn: TriggerFunction,
): void => waveAnimationStore.registerTrigger(triggerFn);

export const unregisterWaveAnimationTrigger = (): void =>
  waveAnimationStore.unregisterTrigger();
