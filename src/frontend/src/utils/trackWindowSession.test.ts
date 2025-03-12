import {
  removeWindowSessionTracker,
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
    removeWindowSessionTracker();
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
    removeWindowSessionTracker();
    mockVisibilityState("hidden");
    document.dispatchEvent(new Event("visibilitychange"));
    expect(onLeaveSession).not.toHaveBeenCalledTimes(2);
    mockVisibilityState("visible");
    document.dispatchEvent(new Event("visibilitychange"));
    expect(onEnterSession).not.toHaveBeenCalled();
  });
});
