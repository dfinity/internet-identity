const DECAY_DURATION = 220;

interface HoldControllerOptions {
  getDuration: () => number;
  onProgress: (value: number) => void;
  onComplete: () => void;
}

export class HoldController {
  readonly #getDuration: () => number;
  readonly #onProgress: (value: number) => void;
  readonly #onComplete: () => void;

  #holding = false;
  #rafId: number | undefined;
  #startTime: number | undefined;
  #currentProgress = 0;
  #decayStartTime: number | undefined;
  #decayStartProgress = 0;

  constructor(options: HoldControllerOptions) {
    this.#getDuration = options.getDuration;
    this.#onProgress = options.onProgress;
    this.#onComplete = options.onComplete;
  }

  start = () => {
    if (this.#holding) return;
    this.#holding = true;
    this.#decayStartTime = undefined;
    this.#cancelRaf();

    const duration = this.#getDuration();
    this.#startTime = performance.now() - this.#currentProgress * duration;

    const tick = (now: number) => {
      const elapsed = now - (this.#startTime ?? now);
      this.#currentProgress = Math.min(elapsed / this.#getDuration(), 1);
      this.#onProgress(this.#currentProgress);
      if (this.#currentProgress < 1) {
        this.#rafId = requestAnimationFrame(tick);
      } else {
        this.#currentProgress = 1;
        this.#holding = false;
        this.#onComplete();
      }
    };
    this.#rafId = requestAnimationFrame(tick);
  };

  cancel = () => {
    if (!this.#holding) return;
    this.#holding = false;
    this.#cancelRaf();

    this.#decayStartProgress = this.#currentProgress;
    this.#decayStartTime = undefined;

    const decay = (now: number) => {
      if (this.#decayStartTime === undefined) this.#decayStartTime = now;
      const elapsed = now - this.#decayStartTime;
      const fraction = Math.min(elapsed / DECAY_DURATION, 1);
      this.#currentProgress = this.#decayStartProgress * (1 - fraction);
      this.#onProgress(this.#currentProgress);
      if (fraction < 1) {
        this.#rafId = requestAnimationFrame(decay);
      } else {
        this.#currentProgress = 0;
      }
    };
    this.#rafId = requestAnimationFrame(decay);
  };

  reset = () => {
    this.#cancelRaf();
    this.#holding = false;
    this.#currentProgress = 0;
    this.#decayStartTime = undefined;
    this.#decayStartProgress = 0;
    this.#onProgress(0);
  };

  dispose = () => {
    this.#cancelRaf();
  };

  #cancelRaf = () => {
    if (this.#rafId !== undefined) {
      cancelAnimationFrame(this.#rafId);
      this.#rafId = undefined;
    }
  };
}
