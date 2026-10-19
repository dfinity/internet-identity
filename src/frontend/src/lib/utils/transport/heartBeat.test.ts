import { describe, it, expect, vi, beforeEach, afterEach } from "vitest";
import { HeartbeatServer } from "./heartBeat";

/**
 * A stand-in for the signer `window`: records the `message` listeners the
 * server registers and lets a test deliver events to them, so a client's
 * `icrc29_status` polls can be simulated without a real cross-window channel.
 */
const makeWindow = () => {
  const listeners = new Set<(event: MessageEvent) => void>();
  return {
    opener: null,
    addEventListener: (type: string, fn: (event: MessageEvent) => void) => {
      if (type === "message") {
        listeners.add(fn);
      }
    },
    removeEventListener: (_type: string, fn: (event: MessageEvent) => void) => {
      listeners.delete(fn);
    },
    deliver: (event: Partial<MessageEvent>) => {
      [...listeners].forEach((fn) => fn(event as MessageEvent));
    },
  };
};

const ORIGIN = "https://rp.test";

// A client poll: an `icrc29_status` request from the relying party window.
const statusRequest = (
  id: string,
  source: { postMessage: unknown },
): Partial<MessageEvent> => ({
  data: { jsonrpc: "2.0", id, method: "icrc29_status" },
  origin: ORIGIN,
  source: source as MessageEventSource,
});

describe("HeartbeatServer.setPending", () => {
  afterEach(() => {
    vi.useRealTimers();
  });

  const establish = () => {
    const window = makeWindow();
    const source = { postMessage: vi.fn() };
    const onDisconnect = vi.fn();
    const server = new HeartbeatServer({
      status: "ready",
      onEstablish: vi.fn(),
      onEstablishTimeout: vi.fn(),
      onDisconnect,
      // The mock stands in for a `Window`; only the message-listener surface and
      // `opener` are exercised.
      window: window as unknown as Window,
    });
    // The client's first poll establishes the channel and is answered "ready".
    window.deliver(statusRequest("1", source));
    expect(source.postMessage).toHaveBeenLastCalledWith(
      { jsonrpc: "2.0", id: "1", result: "ready" },
      ORIGIN,
    );
    return { window, source, server, onDisconnect };
  };

  it("resolves once the client has been answered with pending", async () => {
    const { window, source, server } = establish();

    let resolved = false;
    const pending = server.setPending().then(() => {
      resolved = true;
    });

    // Not yet: the client hasn't polled since the switch, so it can't know to
    // wait rather than tear the channel down.
    await Promise.resolve();
    expect(resolved).toBe(false);

    // The next poll is answered "pending" — and only then does the wait resolve.
    window.deliver(statusRequest("2", source));
    expect(source.postMessage).toHaveBeenLastCalledWith(
      { jsonrpc: "2.0", id: "2", result: "pending" },
      ORIGIN,
    );
    await pending;
    expect(resolved).toBe(true);
  });

  it("resolves on a timeout fallback when the client stops polling", async () => {
    vi.useFakeTimers();
    const { server } = establish();

    let resolved = false;
    const pending = server.setPending().then(() => {
      resolved = true;
    });

    // No further polls arrive, but the wait must not block the redirect forever.
    await vi.advanceTimersByTimeAsync(1000);
    await pending;
    expect(resolved).toBe(true);
  });

  it("keeps answering later polls with pending after the switch", () => {
    const { window, source, server } = establish();
    void server.setPending();

    window.deliver(statusRequest("2", source));
    window.deliver(statusRequest("3", source));
    expect(source.postMessage).toHaveBeenLastCalledWith(
      { jsonrpc: "2.0", id: "3", result: "pending" },
      ORIGIN,
    );
  });
});

describe("HeartbeatServer.changeStatus", () => {
  let window: ReturnType<typeof makeWindow>;
  let source: { postMessage: ReturnType<typeof vi.fn> };

  beforeEach(() => {
    window = makeWindow();
    source = { postMessage: vi.fn() };
  });

  it("switches the status answered to subsequent polls", () => {
    const server = new HeartbeatServer({
      status: "ready",
      onEstablish: vi.fn(),
      onEstablishTimeout: vi.fn(),
      onDisconnect: vi.fn(),
      window: window as unknown as Window,
    });
    window.deliver(statusRequest("1", source));

    server.changeStatus("pending");
    window.deliver(statusRequest("2", source));
    expect(source.postMessage).toHaveBeenLastCalledWith(
      { jsonrpc: "2.0", id: "2", result: "pending" },
      ORIGIN,
    );
  });
});
