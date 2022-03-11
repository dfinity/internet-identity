import { render } from "lit-html";

/**
 * Countdown implementation which calls the supplied update function approximately every second until the endTimestamp is reached.
 * When the endTimestamp is reached, the timeout callback supplied to the start method is called once.
 * Stopping the countdown does not call the timeout callback.
 */
export class Countdown {
  private interval = 1000;
  private timeoutHandle: number | undefined;
  private expected;
  private readonly endTimestamp;
  private timeoutFunc: (() => Promise<void>) | undefined;

  /**
   * Constructs a new Countdown. To start the countdown, {@link Countdown.start} has to be called.
   * @param updateFunc Callback to be called approximately every second until endTimestamp is reached.
   * @param endTimestamp timestamp (in nanoseconds) when the countdown should end.
   */
  constructor(private updateFunc: () => void, endTimestamp: bigint) {
    this.expected = Date.now() + this.interval;
    this.endTimestamp = Number(endTimestamp / BigInt("1000000"));
  }

  /**
   * Starts the countdown. When the timout is reached, the given callback is called (if any).
   * @param timeoutFunc Optional callback function to be called when the timeout is reached.
   */
  public start(timeoutFunc?: () => Promise<void>): void {
    this.timeoutFunc = timeoutFunc;
    this.timeoutHandle = window.setTimeout(() => this.step()); // execute the first step immediately
  }

  /**
   * Stops the countdown. When the timout is reached, the given callback is called (if any).
   */
  public stop(): void {
    if (this.timeoutHandle !== undefined) {
      window.clearTimeout(this.timeoutHandle);
    }
  }

  private async step() {
    const now = Date.now();
    if (now >= this.endTimestamp) {
      if (this.timeoutFunc) {
        await this.timeoutFunc();
      }
      return;
    }
    const drift = now - this.expected;
    this.updateFunc();
    this.expected += this.interval;
    this.timeoutHandle = window.setTimeout(
      () => this.step(),
      Math.max(0, this.interval - drift)
    );
  }
}

/**
 * Sets up a countdown ending at endTimestamp updating the timerElement every second.
 * Note: this function will not yet start the countdown. To {@link Countdown.start} the countdown start() has to be called on the returned object.
 * @param endTimestamp timestamp (in nanoseconds) when the countdown should end
 * @param timerElement element to update with the remaining time in mm:ss format.
 * @return the initialized {@link Countdown}
 */
export const setupCountdown = (
  endTimestamp: bigint,
  timerElement: HTMLElement
): Countdown => {
  return new Countdown(
    () => render(formatRemainingTime(endTimestamp), timerElement),
    endTimestamp
  );
};

export function formatRemainingTime(endTimestamp: bigint): string {
  const [minRemaining, secondsRemaining] = calculateTimeRemaining(endTimestamp);
  return `${minRemaining}:${secondsRemaining}`;
}

const calculateTimeRemaining = (
  expirationTimestamp: bigint
): [string, string] => {
  const now = new Date().getTime();
  const diffSeconds =
    (Number(expirationTimestamp / BigInt("1000000")) - now) / 1000;
  return [
    Math.floor(diffSeconds / 60).toString(),
    Math.floor(diffSeconds % 60)
      .toString()
      .padStart(2, "0"),
  ];
};
