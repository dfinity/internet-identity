import { describe, it, expect, vi } from "vitest";
// The exact source the canister serves and hashes. `?raw` gives the file
// bytes, so this test runs the same script the browser does.
import callbackScript from "./callback_script.js?raw";

interface FakeBroadcastChannel {
  name: string;
  posted: unknown[];
  closed: boolean;
}

/**
 * Run the served callback IIFE with injected browser globals, exactly as the
 * canister's `/callback` page runs it. Building a `Function` from the source
 * also throws on any syntax error in the script — the regression that an
 * escaped-quote backslash guard (`"\"`) would reintroduce.
 */
const runCallbackScript = ({
  payload,
  returnPath,
  authorizeState,
  opener,
  origin = "https://id.ai",
}: {
  payload: unknown;
  returnPath?: string;
  authorizeState?: string;
  opener?: {
    postMessage: (data: unknown, targetOrigin: string) => void;
  } | null;
  origin?: string;
}) => {
  const store = new Map<string, string>();
  store.set("callback-payload-json", JSON.stringify(payload));
  if (returnPath !== undefined) store.set("ii-openid-return-path", returnPath);
  if (authorizeState !== undefined)
    store.set("ii-openid-authorize-state", authorizeState);

  const sessionStorage = {
    getItem: (key: string) => (store.has(key) ? store.get(key)! : null),
    setItem: (key: string, value: string) => store.set(key, value),
  };

  const channels: FakeBroadcastChannel[] = [];
  class BroadcastChannel {
    #record: FakeBroadcastChannel;
    constructor(name: string) {
      this.#record = { name, posted: [], closed: false };
      channels.push(this.#record);
    }
    postMessage(data: unknown) {
      this.#record.posted.push(data);
    }
    close() {
      this.#record.closed = true;
    }
  }

  const windowMock = {
    opener: opener === undefined ? null : opener,
    location: { replace: vi.fn<(url: string) => void>(), origin },
    close: vi.fn(),
  };

  const documentMock = {
    getElementById: (id: string) =>
      id === "callback-payload"
        ? { textContent: store.get("callback-payload-json")! }
        : null,
  };

  const fn = new Function(
    "window",
    "document",
    "sessionStorage",
    "BroadcastChannel",
    callbackScript,
  );
  fn(windowMock, documentMock, sessionStorage, BroadcastChannel);

  return { windowMock, channels, store };
};

describe("callback_script", () => {
  const payload = { id_token: "a.b.c", state: "xyz" };

  it("is a self-contained classic script (no module syntax)", () => {
    expect(callbackScript).not.toMatch(/^\s*(import|export)\b/m);
    expect(callbackScript).toContain('BroadcastChannel("redirect_callback")');
  });

  it("parses without a syntax error", () => {
    // Guards the escaped-backslash footgun: a malformed guard string makes
    // `new Function` throw at construction.
    expect(() =>
      runCallbackScript({ payload, returnPath: "/manage" }),
    ).not.toThrow();
  });

  describe("same-tab flow", () => {
    it("stashes the payload and navigates to the return path", () => {
      const { windowMock, channels, store } = runCallbackScript({
        payload,
        returnPath: "/manage?x=1",
      });
      expect(store.get("ii-openid-callback-data")).toBe(
        JSON.stringify(payload),
      );
      expect(windowMock.location.replace).toHaveBeenCalledWith("/manage?x=1");
      expect(windowMock.close).not.toHaveBeenCalled();
      expect(channels).toHaveLength(0);
    });

    it("falls back to the authorize resume path when only the 1-click marker is set", () => {
      const { windowMock, store } = runCallbackScript({
        payload,
        authorizeState: "state-value",
      });
      expect(store.get("ii-openid-callback-data")).toBe(
        JSON.stringify(payload),
      );
      expect(windowMock.location.replace).toHaveBeenCalledWith(
        "/authorize?flow=openid-resume",
      );
    });

    it("honours a bare root path", () => {
      const { windowMock } = runCallbackScript({ payload, returnPath: "/" });
      expect(windowMock.location.replace).toHaveBeenCalledWith("/");
    });
  });

  describe("open-redirect guard", () => {
    it.each([
      ["protocol-relative //", "//evil.com"],
      ["backslash /\\", "/\\evil.com"],
      ["no leading slash", "evil.com"],
      ["absolute URL", "https://evil.com"],
    ])(
      "rejects %s and takes the popup branch instead",
      (_label, returnPath) => {
        const { windowMock, channels } = runCallbackScript({
          payload,
          returnPath,
          opener: { postMessage: vi.fn() },
        });
        expect(windowMock.location.replace).not.toHaveBeenCalled();
        // Routed to the popup branch: the payload goes out over the channel.
        expect(channels).toHaveLength(1);
        expect(channels[0].posted).toEqual([payload]);
      },
    );
  });

  describe("popup flow", () => {
    it("posts on the channel, to the opener, and closes", () => {
      const openerPost = vi.fn();
      const { windowMock, channels } = runCallbackScript({
        payload,
        opener: { postMessage: openerPost },
      });
      expect(channels).toHaveLength(1);
      expect(channels[0].name).toBe("redirect_callback");
      expect(channels[0].posted).toEqual([payload]);
      expect(channels[0].closed).toBe(true);
      expect(openerPost).toHaveBeenCalledWith(payload, "https://id.ai");
      expect(windowMock.close).toHaveBeenCalled();
    });

    it("does not post to a null opener but still delivers over the channel", () => {
      const { windowMock, channels } = runCallbackScript({
        payload,
        opener: null,
      });
      expect(channels[0].posted).toEqual([payload]);
      expect(channels[0].closed).toBe(true);
      expect(windowMock.close).toHaveBeenCalled();
    });
  });
});
