// A `hasOwnProperty` that produces evidence for the typechecker
export function hasOwnProperty<
  X extends Record<string, unknown>,
  Y extends PropertyKey
>(obj: X, prop: Y): obj is X & Record<Y, unknown> {
  return Object.prototype.hasOwnProperty.call(obj, prop);
}

// A `hasOwnProperty` variant that checks that the property is a number
// and produces evidence for the typechecked
export function hasNumberProperty<
  X extends Record<string, unknown>,
  Y extends PropertyKey
>(obj: X, prop: Y): obj is X & Record<Y, number> {
  return hasOwnProperty(obj, prop) && typeof obj[prop] === "number";
}

// Turns an 'unknown' into a string, if possible, otherwise use the default
// `def` parameter.
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

  if (first === undefined) {
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
export class Chan<A> {
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

  send(a: A): void {
    if (this.snd !== undefined) {
      this.snd(a);
      // After the promise was resolved, set as undefined so that
      // future `send`s go to the buffer.
      this.snd = undefined;
    } else {
      this.buffer.push(a);
    }
  }

  async *recv(): AsyncIterable<A> {
    // Forever loop, yielding entire buffers and then blocking
    // on `snd` (which prevents hot looping)
    while (true) {
      // Yield the buffer first
      while (true) {
        const val = this.buffer.shift();
        if (val === undefined) {
          break;
        }
        yield val;
      }

      // then block and yield a value when received
      yield await new Promise((resolve: (value: A) => void) => {
        this.snd = resolve;
      });
    }
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
