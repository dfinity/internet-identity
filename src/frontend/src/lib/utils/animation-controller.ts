import { DROP_WAVE_ANIMATION } from "$lib/components/backgrounds/constants";
import type { FlairAnimationOptions } from "$lib/components/backgrounds/FlairCanvas";

type TriggerFunction = (opts: FlairAnimationOptions) => Promise<void>;

class AnimationDispatcher {
  #triggerFunction: TriggerFunction | null = null;

  /**
   * Register a trigger function from a WaveCanvas component
   * @param triggerFn The trigger function that handles wave animations
   */
  registerTrigger(triggerFn: TriggerFunction): void {
    this.#triggerFunction = triggerFn;
  }

  /**
   * Unregister the trigger function when component unmounts
   */
  unregisterTrigger(): void {
    this.#triggerFunction = null;
  }

  /**
   * Trigger the drop wave animation
   * @returns Promise that resolves when the animation completes
   */
  async dropWaveAnimation(): Promise<void> {
    if (this.#triggerFunction) {
      await this.#triggerFunction(DROP_WAVE_ANIMATION);
    }
    // If no trigger function is registered, resolve immediately (graceful degradation)
  }
}

const animationDispatcher = new AnimationDispatcher();

// Public API
export const triggerDropWaveAnimation = (): Promise<void> =>
  animationDispatcher.dropWaveAnimation();

export const registerAnimationTrigger = (triggerFn: TriggerFunction): void =>
  animationDispatcher.registerTrigger(triggerFn);

export const unregisterAnimationTrigger = (): void =>
  animationDispatcher.unregisterTrigger();
