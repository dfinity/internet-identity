/**
 * A queue that runs what it is handed one at a time, in the order it was handed them.
 *
 * Each call answers with a promise for its own run: it settles when that run settles,
 * and rejects if that run throws. A rejection reaches only its own caller — the queue
 * carries on, so one failed run does not strand what is queued behind it.
 *
 * Each queue is its own, so callers that serialise for different reasons do not wait
 * on each other. Why a caller needs one belongs at the caller.
 */
export const promiseQueue = (): (<T>(run: () => Promise<T>) => Promise<T>) => {
  let tail: Promise<unknown> = Promise.resolve();
  return <T>(run: () => Promise<T>): Promise<T> => {
    const next = tail.then(run);
    tail = next.catch(() => undefined);
    return next;
  };
};
