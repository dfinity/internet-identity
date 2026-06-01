import { describe, it, expect, vi, beforeEach, afterEach } from "vitest";
import { HoldController } from "./holdController";

const FRAME_MS = 16;

let pending = new Map<number, (time: number) => void>();
let nextRafId = 1;
let rafTime = 0;

const advanceTime = (ms: number) => {
  const end = rafTime + ms;
  while (rafTime < end) {
    if (pending.size === 0) {
      rafTime = end;
      break;
    }
    rafTime = Math.min(rafTime + FRAME_MS, end);
    const due = Array.from(pending.entries());
    pending = new Map();
    for (const [, cb] of due) cb(rafTime);
  }
};

beforeEach(() => {
  pending = new Map();
  nextRafId = 1;
  rafTime = 0;
  vi.spyOn(globalThis, "requestAnimationFrame").mockImplementation((cb) => {
    const id = nextRafId++;
    pending.set(id, cb);
    return id;
  });
  vi.spyOn(globalThis, "cancelAnimationFrame").mockImplementation((id) => {
    pending.delete(id);
  });
  vi.spyOn(performance, "now").mockImplementation(() => rafTime);
});

afterEach(() => {
  vi.restoreAllMocks();
});

describe("HoldController", () => {
  it("completes when held for the full duration", () => {
    const onComplete = vi.fn();
    let currentProgress = 0;
    const controller = new HoldController({
      getDuration: () => 1500,
      onProgress: (v) => (currentProgress = v),
      onComplete,
    });

    controller.start();
    advanceTime(1500);

    expect(onComplete).toHaveBeenCalledTimes(1);
    expect(currentProgress).toBe(1);
  });

  it("does not complete when released before duration", () => {
    const onComplete = vi.fn();
    let currentProgress = 0;
    const controller = new HoldController({
      getDuration: () => 1500,
      onProgress: (v) => (currentProgress = v),
      onComplete,
    });

    controller.start();
    advanceTime(500);
    controller.cancel();

    expect(onComplete).not.toHaveBeenCalled();
    expect(currentProgress).toBeGreaterThan(0);
    expect(currentProgress).toBeLessThan(1);
  });

  it("decays progress back to 0 after cancel", () => {
    const onComplete = vi.fn();
    let currentProgress = 0;
    const controller = new HoldController({
      getDuration: () => 1500,
      onProgress: (v) => (currentProgress = v),
      onComplete,
    });

    controller.start();
    advanceTime(750);
    controller.cancel();
    // Extra cushion past the 220ms decay window so the final frame lands
    // after fraction=1 and progress settles at 0.
    advanceTime(320);

    expect(onComplete).not.toHaveBeenCalled();
    expect(currentProgress).toBe(0);
  });

  it("cancel is a no-op when not holding", () => {
    const controller = new HoldController({
      getDuration: () => 1500,
      onProgress: () => {},
      onComplete: vi.fn(),
    });

    expect(() => controller.cancel()).not.toThrow();
  });

  it("dispose stops any in-flight animation", () => {
    const onComplete = vi.fn();
    let progressCalls = 0;
    const controller = new HoldController({
      getDuration: () => 1500,
      onProgress: () => progressCalls++,
      onComplete,
    });

    controller.start();
    advanceTime(500);
    const before = progressCalls;
    controller.dispose();
    advanceTime(1000);

    expect(progressCalls).toBe(before);
    expect(onComplete).not.toHaveBeenCalled();
  });

  it("reset clears progress and stops the animation", () => {
    const onComplete = vi.fn();
    let currentProgress = 0;
    const controller = new HoldController({
      getDuration: () => 1500,
      onProgress: (v) => (currentProgress = v),
      onComplete,
    });

    controller.start();
    advanceTime(500);
    expect(currentProgress).toBeGreaterThan(0);

    controller.reset();
    expect(currentProgress).toBe(0);

    advanceTime(2000);
    expect(currentProgress).toBe(0);
    expect(onComplete).not.toHaveBeenCalled();
  });

  it("progress is clamped to 1 even when overdue", () => {
    let lastProgress = 0;
    const controller = new HoldController({
      getDuration: () => 1500,
      onProgress: (v) => (lastProgress = v),
      onComplete: vi.fn(),
    });

    controller.start();
    advanceTime(3000);

    expect(lastProgress).toBeLessThanOrEqual(1);
  });

  it("can be started again after a successful completion", () => {
    const onComplete = vi.fn();
    const controller = new HoldController({
      getDuration: () => 1500,
      onProgress: () => {},
      onComplete,
    });

    controller.start();
    advanceTime(1500);
    expect(onComplete).toHaveBeenCalledTimes(1);

    controller.start();
    advanceTime(1500);
    expect(onComplete).toHaveBeenCalledTimes(2);
  });
});
