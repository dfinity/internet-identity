// Turns an 'unknown' into a string, if possible, otherwise use the default
// `def` parameter.
import { isNullish, nonNullish } from "@dfinity/utils";

export function unknownToString(obj: unknown, def: string): string {
  // Only booleans, numbers and strings _may_ not be objects, so first we try
  // Object's toString, and if not we go through the remaining types.
  if (obj instanceof Object) {
    return obj.toString();
  } else if (typeof obj === "string") {
    return obj;
  } else if (typeof obj === "number") {
    return obj.toString();
  } else if (typeof obj === "boolean") {
    return obj.toString();
  }

  // Only "null" and "undefined" do not have 'toString', though typescript
  // doesn't know that.
  return def;
}

// Helper to gain access to the event's target
export const withInputElement = <E extends Event>(
  evnt: E,
  f: (evnt: E, element: HTMLInputElement) => void
): void => {
  const element = evnt.currentTarget;
  if (!(element instanceof HTMLInputElement)) {
    return;
  }

  return f(evnt, element);
};

/** Try to read unknown data as a record */
export function unknownToRecord(
  msg: unknown
): Record<string, unknown> | undefined {
  if (typeof msg !== "object") {
    return undefined;
  }

  if (msg === null) {
    return undefined;
  }

  // Some extra conversions to take typescript by the hand
  // eslint-disable-next-line
  const tmp: {} = msg;
  const obj: Record<string, unknown> = tmp;
  return obj;
}

export type NonEmptyArray<T> = [T, ...T[]];

export function isNonEmptyArray<T>(
  original: T[]
): original is NonEmptyArray<T> {
  return original.length >= 1;
}

export function asNonEmptyArray<T>(
  original: T[]
): NonEmptyArray<T> | undefined {
  const arr: T[] = [...original];

  const first = arr.shift();

  if (isNullish(first)) {
    return undefined;
  }

  return [first, ...arr];
}

// Returns true if we're in Safari or iOS (although technically iOS only has
// Safari)
export function iOSOrSafari(): boolean {
  // List of values of navigator.userAgent, navigator.platform and
  // navigator.userAgentData by device so far (note: navigator.platform is
  // deprecated but navigator.userAgentdata is not implemented in many
  // browsers):
  //
  // iPhone 12 Mini, iOS 15.0.2
  //
  // Safari
  // navigator.userAgentData: undefined
  // navigator.platform: "iPhone"
  // navigator.userAgent: "Mozilla/5.0 (iPhone; CPU iPhone OS 15_0_2 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/15.0 Mobile/15E148 Safari/604.1"
  //
  //
  // MacBook Pro Intel, MacOS Big Sur 11.6
  //
  // Safari
  // navigator.userAgentData: undefined
  // navigator.platform: "MacIntel"
  // navigator.userAgent: "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/15.0 Safari/605.1.15"
  //
  // Chrome
  // navigator.userAgentData.plaftorm: "macOS"
  // navigator.platform: "MacIntel"
  // navigator.userAgent: "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/95.0.4638.69 Safari/537.36"
  //
  // Firefox
  // navigator.userAgentData: undefined
  // navigator.platform: "MacIntel"
  // navigator.userAgent: "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:93.0) Gecko/20100101 Firefox/93.0"
  //
  //
  // MacBook Air M1, MacOS Big Sur 11.6
  //
  // Safari
  // navigator.userAgentData: undefined
  // navigator.platform: "MacIntel" // yes, I double checked
  // navigator.userAgent: "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/15.0 Safari/605.1.15"
  //
  // Firefox
  // navigator.userAgentData: undefined
  // navigator.platform: "MacIntel" // yes, I double checked
  //
  // iPad Pro, iPadOS 15.0.2
  //
  // Safari
  // navigator.userAgentData: undefined
  // navigator.platform: "iPad"
  // navigator.userAgent: "Mozilla/5.0 (iPad; CPU OS 15_0_2 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/15.0 Mobile/15E148 Safari/604.1"

  // For details, see https://stackoverflow.com/a/23522755/2716377
  return /^((?!chrome|android).)*safari/i.test(navigator.userAgent);
}

/* A function that can never be called. Can be used to prove that all type alternatives have been exhausted. */
export function unreachable(_: never, reason?: string): never {
  throw new Error(`Unexpected error ${reason ?? ""}`);
}

/* A version of 'unreachable' that doesn't throw an error but allows execution to continue */
export function unreachableLax(_: never) {
  /* */
}

/* Wrap an unknown value as an error and try to extract a string from it */
export function wrapError(err: unknown): string {
  const unknownError = "unknown error";

  if (err instanceof Error) {
    return err.message;
  }

  if (typeof err === "string") {
    return err;
  }

  return unknownError;
}

/** A channel (Chan) between two execution environments.
 * Values can be sent (`send()`) and received (`recv()`) asynchronously
 * on the other end.
 */
export class Chan<A> implements AsyncIterable<A> {
  /* The `recv` function will read values both from a blocking `snd` function
   * and from a buffer. We always _first_ write to `snd` and _then_ write
   * to `buffer` and _first_ read from the buffer and _then_ read from `snd`
   * to maintain a correct ordering.
   *
   * `snd` is a set by `recv` as `resolve` from a promise that `recv` blocks
   * on.
   */

  // Write to `recv`'s blocking promise
  private snd?: (value: A) => void;

  // Buffer where values are stored in between direct writes
  // to the promise
  private buffer: A[] = [];

  // A list of other channels to which we forward (`send()`) the values
  // sent to us
  private listeners: ((a: A) => void)[] = [];

  public latest: A;

  // Constructor with latest which is "initial" and then latest
  constructor(initial: A) {
    this.latest = initial;
  }

  send(a: A): void {
    if (nonNullish(this.snd)) {
      this.snd(a);
      // After the promise was resolved, set as undefined so that
      // future `send`s go to the buffer.
      this.snd = undefined;
    } else {
      this.buffer.push(a);
    }

    // Finally, broadcast to all listeners
    this.listeners.forEach((listener) => listener(a));

    // and set as latest
    this.latest = a;
  }

  // Receive all values sent to this `Chan`. Note that this effectively
  // consumes the values: if you need to read the value from different
  // places use `.values()` instead.
  protected async *recv(): AsyncIterable<A> {
    yield this.latest;

    // Forever loop, yielding entire buffers and then blocking
    // on `snd` (which prevents hot looping)
    while (true) {
      // Yield the buffer first
      while (this.buffer.length >= 1) {
        // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
        yield this.buffer.shift()!;
      }

      // then block and yield a value when received
      yield await new Promise((resolve: (value: A) => void) => {
        this.snd = resolve;
      });
    }
  }

  // Signal to `map` that the element should remain unchanged
  static readonly unchanged = Symbol("unchanged");

  // Return a new Chan mapped with `f`.
  // In the simplest case, a mapping function is provided.
  // For advanced cases, the mapping function may return 'Chan.unchanged' signalling
  // that the element shouldn't be changed, in which case a default (initial) value
  // also needs to be provided.
  map<B>(
    opts: ((a: A) => B) | { f: (a: A) => B | typeof Chan.unchanged; def: B }
  ): Chan<B> {
    const { handleValue, latest } = this.__handleMapOpts(opts);

    // Create a chan that automatically maps the value
    const input = new Chan<B>(latest);
    this.listeners.push((value: A) =>
      handleValue({ send: (a: B) => input.send(a), value })
    );

    return input;
  }

  // Zip two Chans together, where the resulting Chan includes updates
  // from both Chans.
  zip<B>(chanB: Chan<B>): Chan<[A, B]> {
    // eslint-disable-next-line
    const chanA = this; // for clarity/symmetry below

    const zipped = new Chan<[A, B]>([chanA.latest, chanB.latest]);

    chanA.listeners.push((value: A) => zipped.send([value, chanB.latest]));
    chanB.listeners.push((value: B) => zipped.send([chanA.latest, value]));

    return zipped;
  }

  // How the mapped chan should handle the value
  protected __handleMapOpts<B>(
    opts: ((a: A) => B) | { f: (a: A) => B | typeof Chan.unchanged; def: B }
  ): {
    handleValue: (arg: { send: (b: B) => void; value: A }) => void;
    latest: B;
  } {
    if (typeof opts === "function") {
      // Case of a simple mapper
      const f = opts;
      return {
        handleValue: ({ send, value }) => send(f(value)),
        latest: f(this.latest),
      };
    }

    // Advanced case with "unchanged" handling, where sending is skipped on "unchanged" (and initial/latest value may
    // be set to "def")
    const result = opts.f(this.latest);

    return {
      handleValue: ({ send, value }) => {
        const result = opts.f(value);
        if (result !== Chan.unchanged) {
          send(result);
        }
      },
      latest: result === Chan.unchanged ? opts.def : result,
    };
  }

  // Read all the values sent to this `Chan`.
  values(): AsyncIterable<A> {
    const dup = this.map((x) => x);
    return dup.recv();
  }

  // When used directly as an async iterator, return values()
  [Symbol.asyncIterator](): AsyncIterator<A> {
    return this.values()[Symbol.asyncIterator]();
  }
}

/** Return a random string of size 10
 *
 * NOTE: this is not a very robust random, so do not use this for
 * anything requiring anything resembling true randomness.
 * */
export function randomString(): string {
  return (Math.random() + 1).toString(36).substring(2);
}

// Create a promise that will resolve _after_ this amount of milliseconds.
export function delayMillis(millis: number) {
  return new Promise<void>((resolve) => {
    setTimeout(() => resolve(), millis);
  });
}

// Return a shuffled version of the array. Adapted from https://stackoverflow.com/a/12646864 to
// avoid shuffling in place.
export function shuffleArray<T>(array_: T[]): T[] {
  const array = [...array_];
  for (let i = array.length - 1; i > 0; i--) {
    const j = Math.floor(Math.random() * (i + 1));
    [array[i], array[j]] = [array[j], array[i]];
  }
  return array;
}

// Omit specified functions parameters, for instance OmitParams<..., "foo" | "bar">
// will transform
//  f: (a: { foo, bar, baz }) => void
// into
//  f: (a: { baz }) => void
//
// eslint-disable-next-line
export type OmitParams<T extends (arg: any) => any, A extends string> = (
  a: Omit<Parameters<T>[0], A>
) => ReturnType<T>;

// Zip two arrays together
export const zip = <A, B>(a: A[], b: B[]): [A, B][] =>
  Array.from(Array(Math.min(b.length, a.length)), (_, i) => [a[i], b[i]]);
