import { analytics } from "./analytics";
import { trackWindowSession } from "./trackWindowSession";

export class Funnel<T extends { [K in keyof T]: string }> {
  private readonly name: string;
  private readonly events: T;
  private cleanupSession?: () => void;

  constructor(name: string, events: T) {
    this.name = name;
    this.events = events;
  }

  init(): void {
    analytics.event("start-" + this.name);

    // Start window session tracking
    this.cleanupSession = trackWindowSession({
      onEnterSession: () => {
        analytics.event(`start-${this.name}-window-session-enter`);
      },
      onLeaveSession: () => {
        analytics.event(`start-${this.name}-window-session-leave`);
      },
    });
  }

  close(): void {
    if (this.cleanupSession) {
      this.cleanupSession();
      this.cleanupSession = undefined;
    }
  }

  trigger(event: T[keyof T]): void {
    analytics.event(event);
  }
}
