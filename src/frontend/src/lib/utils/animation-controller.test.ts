import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import {
  triggerDropWaveAnimation,
  registerAnimationTrigger,
  unregisterAnimationTrigger,
} from "./animation-controller";
import { DROP_WAVE_ANIMATION } from "$lib/components/backgrounds/constants";
import type { FlairAnimationOptions } from "$lib/components/backgrounds/FlairCanvas";

describe("AnimationDispatcher", () => {
  let mockTriggerFunction: ReturnType<typeof vi.fn>;
  let animationCallOrder: number[];
  let animationCounter: number;

  beforeEach(() => {
    animationCallOrder = [];
    animationCounter = 0;

    // Create a mock trigger function that tracks call order and timing
    mockTriggerFunction = vi
      .fn()
      .mockImplementation(async (_opts: FlairAnimationOptions) => {
        const callId = ++animationCounter;
        animationCallOrder.push(callId);

        // Simulate animation duration
        await new Promise((resolve) => setTimeout(resolve, 100));

        return Promise.resolve();
      });
    unregisterAnimationTrigger();
    vi.clearAllMocks();
    vi.useRealTimers();
  });

  describe("Basic Functionality", () => {
    it("should register and call trigger function", async () => {
      registerAnimationTrigger(mockTriggerFunction);

      await triggerDropWaveAnimation();

      expect(mockTriggerFunction).toHaveBeenCalledTimes(1);
      expect(mockTriggerFunction).toHaveBeenCalledWith(DROP_WAVE_ANIMATION);
    });

    it("should handle graceful degradation when no trigger function is registered", async () => {
      // Don't register any trigger function
      await expect(triggerDropWaveAnimation()).resolves.toBeUndefined();
    });

    it("should unregister trigger function", async () => {
      registerAnimationTrigger(mockTriggerFunction);
      unregisterAnimationTrigger();

      await triggerDropWaveAnimation();

      expect(mockTriggerFunction).not.toHaveBeenCalled();
    });
  });

  describe("Queue Behavior", () => {
    beforeEach(() => {
      registerAnimationTrigger(mockTriggerFunction);
    });

    it("should execute single animation immediately", async () => {
      await triggerDropWaveAnimation();

      expect(mockTriggerFunction).toHaveBeenCalledTimes(1);
      expect(animationCallOrder).toEqual([1]);
    });

    it("should queue multiple animations and execute them sequentially", async () => {
      // Trigger multiple animations rapidly
      const promises = [
        triggerDropWaveAnimation(),
        triggerDropWaveAnimation(),
        triggerDropWaveAnimation(),
      ];

      await Promise.all(promises);

      expect(mockTriggerFunction).toHaveBeenCalledTimes(3);
      expect(animationCallOrder).toEqual([1, 2, 3]);
    });

    it("should maintain FIFO order for queued animations", async () => {
      vi.useFakeTimers();

      // Create a longer-running mock to better test queuing
      mockTriggerFunction.mockImplementation(async () => {
        const callId = ++animationCounter;
        animationCallOrder.push(callId);
        await new Promise((resolve) => setTimeout(resolve, 1000));
      });

      // Trigger animations rapidly
      const promise1 = triggerDropWaveAnimation();
      const promise2 = triggerDropWaveAnimation();
      const promise3 = triggerDropWaveAnimation();

      // First animation should start immediately
      await vi.advanceTimersByTimeAsync(0);
      expect(animationCallOrder).toEqual([1]);

      // Complete first animation, second should start
      await vi.advanceTimersByTimeAsync(1000);
      expect(animationCallOrder).toEqual([1, 2]);

      // Complete second animation, third should start
      await vi.advanceTimersByTimeAsync(1000);
      expect(animationCallOrder).toEqual([1, 2, 3]);

      // Complete third animation
      await vi.advanceTimersByTimeAsync(1000);

      await Promise.all([promise1, promise2, promise3]);
      expect(mockTriggerFunction).toHaveBeenCalledTimes(3);
    });

    it("should handle animations added while queue is processing", async () => {
      vi.useFakeTimers();

      mockTriggerFunction.mockImplementation(async () => {
        const callId = ++animationCounter;
        animationCallOrder.push(callId);
        await new Promise((resolve) => setTimeout(resolve, 500));
      });

      // Start first animation
      const promise1 = triggerDropWaveAnimation();
      await vi.advanceTimersByTimeAsync(0);
      expect(animationCallOrder).toEqual([1]);

      // Add second animation while first is running
      await vi.advanceTimersByTimeAsync(250);
      const promise2 = triggerDropWaveAnimation();

      // First animation should still be running
      expect(animationCallOrder).toEqual([1]);

      // Complete first animation, second should start
      await vi.advanceTimersByTimeAsync(250);
      expect(animationCallOrder).toEqual([1, 2]);

      // Complete second animation
      await vi.advanceTimersByTimeAsync(500);

      await Promise.all([promise1, promise2]);
      expect(mockTriggerFunction).toHaveBeenCalledTimes(2);
    });
  });

  describe("Promise Resolution", () => {
    beforeEach(() => {
      registerAnimationTrigger(mockTriggerFunction);
    });

    it("should resolve promises in correct order", async () => {
      const resolveOrder: number[] = [];

      const promise1 = triggerDropWaveAnimation().then(() =>
        resolveOrder.push(1),
      );
      const promise2 = triggerDropWaveAnimation().then(() =>
        resolveOrder.push(2),
      );
      const promise3 = triggerDropWaveAnimation().then(() =>
        resolveOrder.push(3),
      );

      await Promise.all([promise1, promise2, promise3]);

      expect(resolveOrder).toEqual([1, 2, 3]);
    });

    it("should resolve individual promises when their animations complete", async () => {
      vi.useFakeTimers();

      mockTriggerFunction.mockImplementation(async () => {
        const callId = ++animationCounter;
        animationCallOrder.push(callId);
        await new Promise((resolve) => setTimeout(resolve, 1000));
      });

      let promise1Resolved = false;
      let promise2Resolved = false;

      const promise1 = triggerDropWaveAnimation().then(() => {
        promise1Resolved = true;
      });
      const promise2 = triggerDropWaveAnimation().then(() => {
        promise2Resolved = true;
      });

      // First animation starts
      await vi.advanceTimersByTimeAsync(0);
      expect(promise1Resolved).toBe(false);
      expect(promise2Resolved).toBe(false);

      // First animation completes
      await vi.advanceTimersByTimeAsync(1000);
      await Promise.resolve(); // Allow promise to resolve
      expect(promise1Resolved).toBe(true);
      expect(promise2Resolved).toBe(false);

      // Second animation completes
      await vi.advanceTimersByTimeAsync(1000);
      await Promise.resolve(); // Allow promise to resolve
      expect(promise2Resolved).toBe(true);

      await Promise.all([promise1, promise2]);
    });
  });

  describe("Edge Cases", () => {
    it.only("should clear queue when unregistering trigger", async () => {
      vi.useFakeTimers();

      // Create a longer-running mock to test queue clearing
      mockTriggerFunction.mockImplementation(async () => {
        const callId = ++animationCounter;
        animationCallOrder.push(callId);
        await new Promise((resolve) => setTimeout(resolve, 1000));
      });

      registerAnimationTrigger(mockTriggerFunction);

      // Queue some animations
      const promise1 = triggerDropWaveAnimation();
      const promise2 = triggerDropWaveAnimation();

      // Let first animation start
      await vi.advanceTimersByTimeAsync(0);
      expect(animationCallOrder).toEqual([1]);

      // Unregister before first animation completes
      unregisterAnimationTrigger();

      // Complete the first animation and wait for promises
      await vi.advanceTimersByTimeAsync(2000);
      await Promise.all([promise1, promise2]);

      // Only the first animation should have been called (it was already running)
      // The second should have been cleared from the queue
      expect(mockTriggerFunction).toHaveBeenCalledTimes(1);
      expect(animationCallOrder).toEqual([1]);
    });

    it("should handle trigger function that throws an error", async () => {
      const consoleWarnSpy = vi
        .spyOn(console, "warn")
        .mockImplementation(() => {});
      const errorTrigger = vi
        .fn()
        .mockRejectedValue(new Error("Animation failed"));
      registerAnimationTrigger(errorTrigger);

      // Animation should still resolve even if trigger function fails
      await expect(triggerDropWaveAnimation()).resolves.toBeUndefined();
      expect(errorTrigger).toHaveBeenCalledTimes(1);
      expect(consoleWarnSpy).toHaveBeenCalledWith(
        "Animation failed:",
        expect.any(Error),
      );

      consoleWarnSpy.mockRestore();
    });

    it("should continue processing queue even if one animation fails", async () => {
      const consoleWarnSpy = vi
        .spyOn(console, "warn")
        .mockImplementation(() => {});
      let callCount = 0;
      const flakyTrigger = vi.fn().mockImplementation(() => {
        callCount++;
        if (callCount === 2) {
          throw new Error("Second animation failed");
        }
        animationCallOrder.push(callCount);
      });

      registerAnimationTrigger(flakyTrigger);

      const promises = [
        triggerDropWaveAnimation(),
        triggerDropWaveAnimation(),
        triggerDropWaveAnimation(),
      ];

      await Promise.all(promises);

      expect(flakyTrigger).toHaveBeenCalledTimes(3);
      expect(animationCallOrder).toEqual([1, 3]); // Second call failed, but third succeeded
      expect(consoleWarnSpy).toHaveBeenCalledWith(
        "Animation failed:",
        expect.any(Error),
      );

      consoleWarnSpy.mockRestore();
    });

    it("should handle rapid registration and unregistration", async () => {
      registerAnimationTrigger(mockTriggerFunction);
      const promise1 = triggerDropWaveAnimation();

      // Wait a bit to let the first animation potentially start
      await new Promise<void>((resolve) => setTimeout(resolve, 10));

      unregisterAnimationTrigger();
      registerAnimationTrigger(mockTriggerFunction);

      const promise2 = triggerDropWaveAnimation();

      await Promise.all([promise1, promise2]);

      // The first animation should have been called before unregistering
      // The second animation should be called after re-registering
      expect(mockTriggerFunction).toHaveBeenCalledTimes(2);
    });
  });
});
