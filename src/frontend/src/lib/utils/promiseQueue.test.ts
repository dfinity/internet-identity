import { describe, expect, it } from "vitest";
import { promiseQueue } from "$lib/utils/promiseQueue";

describe("promiseQueue", () => {
  it("runs what it is handed one at a time, in order", async () => {
    const order: string[] = [];
    const settle: (() => void)[] = [];
    const run = (name: string) => () =>
      new Promise<void>((resolve) => {
        order.push(`start ${name}`);
        settle.push(() => {
          order.push(`end ${name}`);
          resolve();
        });
      });
    const enqueue = promiseQueue();

    const first = enqueue(run("first"));
    const second = enqueue(run("second"));

    // The chain starts its next link in a microtask, so let those run first.
    await new Promise((resolve) => setTimeout(resolve, 0));
    expect(order).toEqual(["start first"]);
    settle[0]();
    await first;
    await new Promise((resolve) => setTimeout(resolve, 0));
    settle[1]();
    await second;

    expect(order).toEqual([
      "start first",
      "end first",
      "start second",
      "end second",
    ]);
  });

  it("does not let a failed run hold up the next", async () => {
    const enqueue = promiseQueue();
    const failed = enqueue(() => Promise.reject(new Error("no")));

    await expect(failed).rejects.toThrow("no");
    await expect(enqueue(() => Promise.resolve())).resolves.toBeUndefined();
  });

  it("answers each caller with its own run's value", async () => {
    const enqueue = promiseQueue();

    await expect(
      Promise.all([
        enqueue(() => Promise.resolve("first")),
        enqueue(() => Promise.resolve(2)),
      ]),
    ).resolves.toEqual(["first", 2]);
  });

  it("keeps each queue to itself", async () => {
    const order: string[] = [];
    const held = new Promise<void>(() => undefined);
    const one = promiseQueue();
    const other = promiseQueue();

    const record = (what: string) => () => {
      order.push(what);
      return Promise.resolve();
    };
    void one(() => held);
    void one(record("behind the held one"));
    await other(record("other queue"));

    expect(order).toEqual(["other queue"]);
  });
});
