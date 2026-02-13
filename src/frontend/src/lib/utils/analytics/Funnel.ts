import { analytics } from "./analytics";
import { trackWindowSession } from "../trackWindowSession";

export class Funnel<T extends Record<string, string>> {
  #name: string;
  #prefixEvents: boolean;
  #cleanupSession?: () => void;
  #startTimestamp?: number;
  #properties?: Record<string, string | number>;

  constructor(name: string, prefixEvents = false) {
    this.#name = name;
    this.#prefixEvents = prefixEvents;
  }

  init(properties?: Record<string, string | number>): void {
    this.#startTimestamp = Date.now();
    this.#properties = properties;
    analytics.event("start-" + this.#name, this.#properties);

    // Start window session tracking
    this.#cleanupSession = trackWindowSession({
      onEnterSession: () => {
        analytics.event(
          `start-${this.#name}-window-session-enter`,
          this.#properties,
        );
      },
      onLeaveSession: () => {
        analytics.event(
          `start-${this.#name}-window-session-leave`,
          this.#properties,
        );
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
      const eventProperties = {
        ...(this.#properties || {}),
        [`duration-${this.#name}`]: durationSec,
      };
      analytics.event(`end-${this.#name}`, eventProperties);
      this.#startTimestamp = undefined;
    }
  }

  addProperties(additionalProperties: Record<string, string | number>): void {
    this.#properties = {
      ...(this.#properties ?? {}),
      ...additionalProperties,
    };
  }

  trigger(
    event: T[keyof T],
    additionalProperties?: Record<string, string | number>,
  ): void {
    const eventProperties = {
      ...(this.#properties ?? {}),
      ...(additionalProperties ?? {}),
    };

    analytics.event(
      this.#prefixEvents ? `${this.#name}--${event}` : event,
      Object.keys(eventProperties).length > 0 ? eventProperties : undefined,
    );
  }
}
