import { analytics } from "./analytics";
import { trackWindowSession } from "../trackWindowSession";

export class Funnel<T extends Record<string, string>> {
  #name: string;
  #cleanupSession?: () => void;
  #startTimestamp?: number;

  constructor(name: string) {
    this.#name = name;
  }

  init(): void {
    this.#startTimestamp = Date.now();
    analytics.event("start-" + this.#name);

    // Start window session tracking
    this.#cleanupSession = trackWindowSession({
      onEnterSession: () => {
        analytics.event(`start-${this.#name}-window-session-enter`);
      },
      onLeaveSession: () => {
        analytics.event(`start-${this.#name}-window-session-leave`);
      },
    });
  }

  close(): void {
    if (this.#cleanupSession) {
      this.#cleanupSession();
      this.#cleanupSession = undefined;
    }

    // Only track duration if we have a valid startTimestamp
    if (
      this.#startTimestamp !== undefined &&
      !Number.isNaN(this.#startTimestamp) &&
      this.#startTimestamp > 0
    ) {
      const durationMs = Date.now() - this.#startTimestamp;
      const durationSec = durationMs / 1000;
      analytics.event(`end-${this.#name}`, {
        [`duration-${this.#name}`]: durationSec,
      });
      this.#startTimestamp = undefined;
    }
  }

  trigger(event: T[keyof T]): void {
    analytics.event(event);
  }
}
