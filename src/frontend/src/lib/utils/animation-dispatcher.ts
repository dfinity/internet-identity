import { DROP_WAVE_ANIMATION } from "$lib/components/backgrounds/constants";
import type { FlairAnimationOptions } from "$lib/components/backgrounds/FlairCanvas";

type TriggerFunction = (opts: FlairAnimationOptions) => Promise<void>;

class AnimationDispatcher {
  #triggerFunction: TriggerFunction | null = null;
  #clearFunction: (() => Promise<void>) | null = null;
  #animationQueue: Array<() => Promise<void>> = [];
  #isAnimating: boolean = false;

  /**
   * Register a trigger function from a WaveCanvas component
   * @param triggerFn The trigger function that handles wave animations
   */
  registerTrigger(
    triggerFn: TriggerFunction,
    clearFn?: () => Promise<void>,
  ): void {
    this.#triggerFunction = triggerFn;
    this.#clearFunction = clearFn ?? null;
  }

  /**
   * Unregister the trigger function when component unmounts
   */
  unregisterTrigger(): void {
    this.#triggerFunction = null;
    // Execute any pending animations to resolve their promises (graceful degradation)
    const pendingAnimations = [...this.#animationQueue];
    this.#animationQueue = [];
    this.#isAnimating = false;

    // Resolve all pending animations immediately
    pendingAnimations.forEach((animation) => {
      void animation();
    });
  }

  /**
   * Process the animation queue sequentially
   */
  async #processQueue(): Promise<void> {
    if (this.#isAnimating || this.#animationQueue.length === 0) {
      return;
    }

    this.#isAnimating = true;
    while (this.#animationQueue.length > 0) {
      const animation = this.#animationQueue.shift()!;
      try {
        await animation();
      } catch (error) {
        // Continue processing queue even if one animation fails
        console.warn("Animation failed:", error);
      }
    }
    this.#isAnimating = false;
  }

  /**
   * Trigger the drop wave animation
   * @returns Promise that resolves when the animation completes
   */
  dropWaveAnimation(): Promise<void> {
    return new Promise((resolve) => {
      this.#animationQueue.push(async () => {
        try {
          if (this.#triggerFunction) {
            await this.#triggerFunction(DROP_WAVE_ANIMATION);
          }
        } catch (error) {
          console.warn("Animation failed:", error);
        } finally {
          resolve();
        }
      });

      void this.#processQueue();
    });
  }

  async clearWaveAnimation(): Promise<void> {
    await this.#clearFunction?.(); // call visual reset
  }
}

const animationDispatcher = new AnimationDispatcher();

// Public API
export const triggerDropWaveAnimation = (): Promise<void> =>
  animationDispatcher.dropWaveAnimation();

export const clearDropWaveAnimation = (): Promise<void> =>
  animationDispatcher.clearWaveAnimation();

export const registerAnimationTrigger = (
  triggerFn: TriggerFunction,
  clearFn?: () => Promise<void>,
): void => animationDispatcher.registerTrigger(triggerFn, clearFn);

export const unregisterAnimationTrigger = (): void =>
  animationDispatcher.unregisterTrigger();
