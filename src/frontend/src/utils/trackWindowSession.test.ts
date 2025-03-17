import {
  cleanUpWindowSessionTrackers,
  trackWindowSession,
} from "./trackWindowSession";

// Change jest to vitest
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

// Mock document.visibilityState
const mockVisibilityState = (state: "hidden" | "visible") => {
  Object.defineProperty(document, "visibilityState", {
    configurable: true,
    get: () => state,
  });
};

describe("trackWindowSession", () => {
  let onLeaveSession: ReturnType<typeof vi.fn>;
  let onEnterSession: ReturnType<typeof vi.fn>;

  beforeEach(() => {
    onLeaveSession = vi.fn();
    onEnterSession = vi.fn();
  });

  afterEach(() => {
    cleanUpWindowSessionTrackers();
  });

  it("should call onLeaveSession when the window is hidden", () => {
    trackWindowSession({ onLeaveSession, onEnterSession });
    mockVisibilityState("hidden");
    document.dispatchEvent(new Event("visibilitychange"));
    expect(onLeaveSession).toHaveBeenCalled();
  });

  it("should call onEnterSession when the window becomes visible", () => {
    trackWindowSession({ onLeaveSession, onEnterSession });
    mockVisibilityState("visible");
    document.dispatchEvent(new Event("visibilitychange"));
    expect(onEnterSession).toHaveBeenCalled();
  });

  it("should remove the event listener when removeWindowSessionTracker is called", () => {
    trackWindowSession({ onLeaveSession, onEnterSession });
    mockVisibilityState("hidden");
    document.dispatchEvent(new Event("visibilitychange"));
    expect(onLeaveSession).toHaveBeenCalled();
    cleanUpWindowSessionTrackers();
    mockVisibilityState("hidden");
    document.dispatchEvent(new Event("visibilitychange"));
    expect(onLeaveSession).not.toHaveBeenCalledTimes(2);
    mockVisibilityState("visible");
    document.dispatchEvent(new Event("visibilitychange"));
    expect(onEnterSession).not.toHaveBeenCalled();
  });

  it("should remove only the specific listener when the cleanup function is called", () => {
    const cleanup = trackWindowSession({ onLeaveSession, onEnterSession });

    // Create a second set of listeners
    const onLeaveSession2 = vi.fn();
    const onEnterSession2 = vi.fn();
    trackWindowSession({
      onLeaveSession: onLeaveSession2,
      onEnterSession: onEnterSession2,
    });

    // Trigger events, both listeners should respond
    mockVisibilityState("hidden");
    document.dispatchEvent(new Event("visibilitychange"));
    expect(onLeaveSession).toHaveBeenCalledTimes(1);
    expect(onLeaveSession2).toHaveBeenCalledTimes(1);

    // Clean up the first listener
    cleanup();

    // Only the second listener should respond now
    mockVisibilityState("hidden");
    document.dispatchEvent(new Event("visibilitychange"));
    expect(onLeaveSession).toHaveBeenCalledTimes(1); // Count should still be 1
    expect(onLeaveSession2).toHaveBeenCalledTimes(2); // Count should increase to 2
  });

  it("should support multiple listeners that can be independently removed and added", () => {
    // Create three sets of listeners
    const onLeaveSession1 = vi.fn();
    const onEnterSession1 = vi.fn();
    const cleanup1 = trackWindowSession({
      onLeaveSession: onLeaveSession1,
      onEnterSession: onEnterSession1,
    });

    const onLeaveSession2 = vi.fn();
    const onEnterSession2 = vi.fn();
    const cleanup2 = trackWindowSession({
      onLeaveSession: onLeaveSession2,
      onEnterSession: onEnterSession2,
    });

    const onLeaveSession3 = vi.fn();
    const onEnterSession3 = vi.fn();
    const cleanup3 = trackWindowSession({
      onLeaveSession: onLeaveSession3,
      onEnterSession: onEnterSession3,
    });

    // Test all three listeners respond
    mockVisibilityState("visible");
    document.dispatchEvent(new Event("visibilitychange"));
    expect(onEnterSession1).toHaveBeenCalledTimes(1);
    expect(onEnterSession2).toHaveBeenCalledTimes(1);
    expect(onEnterSession3).toHaveBeenCalledTimes(1);

    // Remove the third listener first
    cleanup2();

    const onLeaveSession4 = vi.fn();
    const onEnterSession4 = vi.fn();
    const cleanup4 = trackWindowSession({
      onLeaveSession: onLeaveSession4,
      onEnterSession: onEnterSession4,
    });

    // Test that only listeners 1 and 2 respond
    mockVisibilityState("visible");
    document.dispatchEvent(new Event("visibilitychange"));
    expect(onEnterSession1).toHaveBeenCalledTimes(2);
    expect(onEnterSession2).toHaveBeenCalledTimes(1); // Should remain at 1
    expect(onEnterSession3).toHaveBeenCalledTimes(2);
    expect(onEnterSession4).toHaveBeenCalledTimes(1); // New listener should respond

    // Remove the first listener
    cleanup1();

    // Test that only listener 2 responds
    mockVisibilityState("visible");
    document.dispatchEvent(new Event("visibilitychange"));
    expect(onEnterSession1).toHaveBeenCalledTimes(2); // Should remain at 2
    expect(onEnterSession2).toHaveBeenCalledTimes(1); // Still 1
    expect(onEnterSession3).toHaveBeenCalledTimes(3);
    expect(onEnterSession4).toHaveBeenCalledTimes(2);

    cleanup3();

    mockVisibilityState("visible");
    document.dispatchEvent(new Event("visibilitychange"));
    expect(onEnterSession1).toHaveBeenCalledTimes(2); // Still 2
    expect(onEnterSession2).toHaveBeenCalledTimes(1); // Still 1
    expect(onEnterSession3).toHaveBeenCalledTimes(3); // Should remain at 3
    expect(onEnterSession4).toHaveBeenCalledTimes(3);

    // Remove the last remaining listener
    cleanup4();

    // Test that no listeners respond
    mockVisibilityState("visible");
    document.dispatchEvent(new Event("visibilitychange"));
    expect(onEnterSession1).toHaveBeenCalledTimes(2); // Still 2
    expect(onEnterSession2).toHaveBeenCalledTimes(1); // Still 1
    expect(onEnterSession3).toHaveBeenCalledTimes(3); // Still 3
    expect(onEnterSession4).toHaveBeenCalledTimes(3); // Still 3
  });
});
