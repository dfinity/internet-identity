import type { SignedDelegation } from "$lib/generated/internet_identity_types";
import { Signature } from "@icp-sdk/core/agent";
import { type Readable } from "svelte/store";
import {
  Delegation,
  SignedDelegation as FrontendSignedDelegation,
} from "@icp-sdk/core/identity";

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

/** Collect information helpful to diagnose errors */
export async function diagnosticInfo(): Promise<string> {
  return `user-agent: "${
    navigator.userAgent
  }", is platform auth available: ${await window?.PublicKeyCredential?.isUserVerifyingPlatformAuthenticatorAvailable()}`;
}

export type NonEmptyArray<T> = [T, ...T[]];

export function isNonEmptyArray<T>(
  original: T[],
): original is NonEmptyArray<T> {
  return original.length >= 1;
}

export function asNonEmptyArray<T>(
  original: T[],
): NonEmptyArray<T> | undefined {
  const arr: T[] = [...original];

  const first = arr.shift();

  if (first === undefined) {
    return undefined;
  }

  return [first, ...arr];
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
    if (this.snd !== undefined) {
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
    opts: ((a: A) => B) | { f: (a: A) => B | typeof Chan.unchanged; def: B },
  ): Chan<B> {
    const { handleValue, latest } = this.__handleMapOpts(opts);

    // Create a chan that automatically maps the value
    const input = new Chan<B>(latest);
    this.listeners.push((value: A) =>
      handleValue({ send: (a: B) => input.send(a), value }),
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
    opts: ((a: A) => B) | { f: (a: A) => B | typeof Chan.unchanged; def: B },
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

// Create a promise that will resolve _after_ this amount of milliseconds.
export function waitFor(duration: number) {
  return new Promise<void>((resolve) => {
    setTimeout(() => resolve(), duration);
  });
}

// Converts a union type to an intersection type to extract shared keys
export type UnionToIntersection<U> = (
  U extends unknown ? (k: U) => void : never
) extends (k: infer I) => void
  ? I
  : never;

// Utility to wrap Rust canister enum error into a JS error that can be thrown
export class CanisterError<T extends Record<string, unknown>> extends Error {
  readonly #value: T;

  constructor(value: T) {
    super();
    Object.setPrototypeOf(this, CanisterError.prototype);
    this.#value = value;
  }

  get type(): keyof UnionToIntersection<T> {
    return Object.keys(this.#value)[0] as keyof UnionToIntersection<T>;
  }

  value<S extends keyof UnionToIntersection<T>>(
    type: S,
  ): UnionToIntersection<T>[S] {
    return this.#value[type as keyof T] as UnionToIntersection<T>[S];
  }
}

export const isCanisterError = <T extends Record<string, unknown>>(
  error: unknown,
): error is CanisterError<T> => {
  return error instanceof CanisterError;
};

export const throwCanisterError = <
  T extends { Ok: unknown } | { Err: Record<string, unknown> | null },
  S = T extends { Ok: infer Ok } ? Ok : never,
>(
  response: T,
): Promise<S> => {
  if ("Err" in response) {
    if (response.Err === null) {
      throw new Error("Unexpected error occurred");
    }
    throw new CanisterError(response.Err);
  }
  return response.Ok as Promise<S>;
};

// Helper that normalizes ArrayBuffer-like inputs.
// Given a Uint8Array or an ArrayBuffer, always return an ArrayBuffer corresponding
// exactly to the view's bytes (respects byteOffset/byteLength for typed arrays).
export const bufFromBufLike = (
  bufLike: ArrayBuffer | Uint8Array,
): ArrayBuffer => {
  if (bufLike instanceof ArrayBuffer) {
    return bufLike;
  }
  // bufLike is a Uint8Array view into an ArrayBuffer; slice to get the exact range
  return bufLike.buffer.slice(
    bufLike.byteOffset,
    bufLike.byteOffset + bufLike.byteLength,
  ) as ArrayBuffer;
};

export const fromBase64 = (base64: string): Uint8Array => {
  if (
    "fromBase64" in Uint8Array &&
    typeof Uint8Array.fromBase64 === "function"
  ) {
    return Uint8Array.fromBase64(base64);
  }
  if (typeof globalThis.atob !== "undefined") {
    return Uint8Array.from(globalThis.atob(base64), (m) => m.charCodeAt(0));
  }
  throw Error("Could not decode base64 string");
};

export const fromBase64URL = (base64Url: string): Uint8Array =>
  fromBase64(
    base64Url
      .replace(/-/g, "+")
      .replace(/_/g, "/")
      .padEnd(Math.ceil(base64Url.length / 4) * 4, "="),
  );

export const toBase64 = (bytes: Uint8Array): string => {
  if ("toBase64" in bytes && typeof bytes.toBase64 === "function") {
    return bytes.toBase64();
  }
  if (typeof globalThis.btoa !== "undefined") {
    let binary = "";
    for (let i = 0; i < bytes.byteLength; i++) {
      binary += String.fromCharCode(bytes[i]);
    }
    return btoa(binary);
  }
  throw Error("Could not encode base64 string");
};

export const toBase64URL = (bytes: Uint8Array): string =>
  toBase64(bytes).replace(/\+/g, "-").replace(/\//g, "_").replace(/=+$/, "");

export const fromHex = (hex: string): Uint8Array => {
  if ("fromHex" in Uint8Array && typeof Uint8Array.fromHex === "function") {
    return Uint8Array.fromHex(hex);
  }
  if (hex.length % 2 !== 0) {
    throw new Error("Invalid hex string");
  }
  return new Uint8Array(
    Array.from({ length: hex.length / 2 }, (_, i) =>
      parseInt(hex.slice(i * 2, i * 2 + 2), 16),
    ),
  );
};

export const toHex = (bytes: Uint8Array): string => {
  if ("toHex" in bytes && typeof bytes.toHex === "function") {
    return bytes.toHex();
  }
  return Array.from(bytes)
    .map((b) => b.toString(16).padStart(2, "0"))
    .join("");
};

// Utility to transform the signed delegation received from the backend into one that the frontend DelegationChain understands.
export const transformSignedDelegation = (
  signed_delegation: SignedDelegation,
): FrontendSignedDelegation => {
  return {
    delegation: new Delegation(
      Uint8Array.from(signed_delegation.delegation.pubkey),
      signed_delegation.delegation.expiration,
      undefined,
    ),
    signature: Uint8Array.from(
      signed_delegation.signature,
    ) as unknown as Signature,
  };
};

export const retryFor = async <T>(
  attempts: number,
  fn: () => Promise<T>,
): Promise<T> => {
  let lastError: unknown;
  for (let i = 0; i < attempts; i++) {
    try {
      await waitFor(i * 1000); // Linear backoff
      return await fn();
    } catch (error) {
      lastError = error;
    }
  }
  throw lastError;
};

/**
 * Generate secure random id
 * @param length How long the generated id should be
 * @param dictionary Characters to use to generate id (defaults to base62)
 */
export const secureRandomId = (
  length: number,
  dictionary = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ",
) => {
  const chars: string[] = [];
  while (chars.length < length) {
    // Randomly pick character from dictionary through rejection sampling
    const value = crypto.getRandomValues(new Uint8Array(1))[0];
    if (value >= 256 - (256 % dictionary.length)) {
      continue;
    }
    chars.push(dictionary[value % dictionary.length]);
  }
  return chars.join("");
};

/**
 * Resolves when the store value satisfies the condition.
 * The condition maps the store value to `R | undefined` — returning `undefined`
 * means "keep waiting", any other value resolves the promise.
 * If no condition is provided, resolves when the store value is not `undefined`.
 */
export function waitForStore<T>(store: Readable<T | undefined>): Promise<T>;
export function waitForStore<T, R>(
  store: Readable<T>,
  condition: (value: T) => R | undefined,
): Promise<R>;
export function waitForStore<T, R>(
  store: Readable<T>,
  condition: (value: T) => R | undefined = (value) => value as R | undefined,
): Promise<R> {
  return new Promise((resolve) => {
    const unsubscribe = store.subscribe((value) => {
      const result = condition(value);
      if (result !== undefined) {
        queueMicrotask(() => unsubscribe());
        resolve(result);
      }
    });
  });
}
