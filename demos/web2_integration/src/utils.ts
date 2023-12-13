// Turns an 'unknown' into a string, if possible, otherwise use the default
// `def` parameter.
import { isNullish, nonNullish } from "@dfinity/utils";
import { render, type TemplateResult } from "lit-html";
import type { Ref } from "lit-html/directives/ref.js";

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
  private snd: ((value: A) => void) | undefined;

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

// Read a "lit-html" ref, showing an error message (in the console) in case the
// element is not available.
export function withRef<A, B>(ref: Ref<A>, f: (val: A) => B): B | undefined {
  const value = ref.value;

  if (isNullish(value)) {
    alert(
      "Internet Identity: Tried to access a DOM element that doesn't exist, this is a bug",
    );
    return;
  } else {
    return f(value);
  }
}

/* A wrapper for lit-html's render, rendering a page to the "pageContent" element */
export function renderPage<
  T extends (props: Parameters<T>[0]) => TemplateResult,
>(template: T): (props: Parameters<T>[0], container?: HTMLElement) => void {
  return (props, container) => {
    const contain =
      container ?? (document.getElementById("pageContent") as HTMLElement);
    render(template(props), contain);
  };
}
