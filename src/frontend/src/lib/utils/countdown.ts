import { delayMillis } from "./utils";

/// A one shot countdown timer that emits the remaining time as a formatted string ("mm:ss")
// through an AsyncIterable.
//
// The countdown cannot be restarted.
export class AsyncCountdown<A> {
  // Create a countdown from nanoseconds
  static fromNanos<A>(nanos: bigint): AsyncCountdown<A> {
    return new AsyncCountdown(Number(nanos / BigInt(1e9)));
  }

  // The value set by the countdown on timeout
  static readonly timeout: unique symbol = Symbol("timeout");

  // The number of seconds elapsed since the epoch
  static seconds() {
    return new Date().getTime() / 1000;
  }

  // Whether the countdown was stopped explicitely.
  // XXX: Some assumptions are made in the code that the countdown is never restarted.
  private stopped: boolean;

  // A promise that resolves once the countdown has stopped
  private promise: Promise<A | typeof AsyncCountdown.timeout>;
  private resolve?: (a: A | typeof AsyncCountdown.timeout) => void;

  // when it should stop (seconds since epoch)
  constructor(private expirationSeconds: number) {
    this.stopped = this.pastExpiration();
    this.promise = new Promise((resolve) => {
      this.resolve = resolve;
    });
  }

  // Stop the countdown explicitly, setting a result
  stop(a: A) {
    this.__stop(a);
  }

  // Stop the countdown, effectively settings the result.
  // Can be called many times but only the first result will be taken into account.
  private __stop(a: A | typeof AsyncCountdown.timeout) {
    this.stopped = true;
    this.resolve?.(a);
  }

  // Number of seconds remaining
  remainingSeconds(): number {
    return Math.max(0, this.expirationSeconds - AsyncCountdown.seconds());
  }

  // The remaining time, formatted as "mm:ss". This will yield approximately
  // once a second.
  async *remainingFormattedAsync(): AsyncIterable<string> {
    for (;;) {
      const remaining = this.remainingSeconds();

      // Yield the time, as long as the countdown has not stopped.
      // NOTE: a '0' _will_ be yielded before the countdown stop.
      yield prettifySeconds(remaining);

      if (remaining <= 0) {
        this.__stop(AsyncCountdown.timeout);
      }

      if (this.hasStopped()) {
        break;
      }

      // Wait for at least the delay until the next second
      // (fractional part of remaining time in seconds)
      await delayMillis((remaining % 1) * 1000);
    }
  }

  // Returns true if the countdown has stopped, either because the time is up
  // or because `.stop()` was called explicitly.
  // can assume: if stopped, won't restart
  hasStopped(): boolean {
    return this.stopped || this.pastExpiration(); // assupmtion: don't go back in time
  }

  // Wait until the result is available (or the 'timeout' value in case of timeout)
  wait(): Promise<A | typeof AsyncCountdown.timeout> {
    return this.promise;
  }

  // Returns true if the time is past the countdown's expiration
  pastExpiration() {
    return AsyncCountdown.seconds() > this.expirationSeconds;
  }
}

// Prettify the number into a string: "mm:ss"
function prettifySeconds(a: number): string {
  const mins = Math.floor(a / 60)
    .toString()
    .padStart(2, "0");
  const secs = Math.floor(a % 60)
    .toString()
    .padStart(2, "0");
  return `${mins}:${secs}`;
}
