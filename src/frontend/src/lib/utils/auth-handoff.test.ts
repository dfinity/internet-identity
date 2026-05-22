import {
  DelegationChain,
  DelegationIdentity,
  ECDSAKeyIdentity,
} from "@icp-sdk/core/identity";
import { Principal } from "@icp-sdk/core/principal";
import { describe, expect, it, vi, afterEach } from "vitest";
import {
  deserializeAuth,
  receiveAuthFromOpener,
  sendAuthToOpenedTab,
  serializeAuth,
} from "./auth-handoff";
import type { Authenticated } from "$lib/stores/authentication.store";

type AuthWithoutAgentActor = Omit<
  Authenticated,
  "agent" | "actor" | "salt" | "nonce"
>;

async function makeIdentity(): Promise<{
  inner: ECDSAKeyIdentity;
  chain: DelegationChain;
  identity: DelegationIdentity;
}> {
  const signer = await ECDSAKeyIdentity.generate({ extractable: true });
  const session = await ECDSAKeyIdentity.generate({ extractable: true });
  const chain = await DelegationChain.create(
    signer,
    session.getPublicKey(),
    new Date(Date.now() + 30 * 60 * 1000),
  );
  const identity = DelegationIdentity.fromDelegation(session, chain);
  return { inner: session, chain, identity };
}

describe("serializeAuth / deserializeAuth round-trip", () => {
  it("passkey authMethod — credentialId bytes preserved", async () => {
    const { identity } = await makeIdentity();
    const credentialId = new Uint8Array([1, 2, 3, 4, 5, 6]);
    const auth: AuthWithoutAgentActor = {
      identityNumber: BigInt(12345),
      identity,
      authMethod: { passkey: { credentialId } },
    };

    const payload = await serializeAuth(auth);
    const restored = await deserializeAuth(payload);

    expect(restored).not.toBeNull();
    expect(restored!.authMethod).toEqual({ passkey: { credentialId } });
  });

  it("openid authMethod — iss and sub preserved", async () => {
    const { identity } = await makeIdentity();
    const auth: AuthWithoutAgentActor = {
      identityNumber: BigInt(99),
      identity,
      authMethod: {
        openid: { iss: "https://accounts.google.com", sub: "12345678" },
      },
    };

    const payload = await serializeAuth(auth);
    const restored = await deserializeAuth(payload);

    expect(restored).not.toBeNull();
    expect(restored!.authMethod).toEqual({
      openid: { iss: "https://accounts.google.com", sub: "12345678" },
    });
  });

  it("recoveryPhrase authMethod — principal preserved", async () => {
    const { identity } = await makeIdentity();
    const principal = Principal.fromText("2vxsx-fae");
    const auth: AuthWithoutAgentActor = {
      identityNumber: BigInt(1),
      identity,
      authMethod: { recoveryPhrase: { principal } },
    };

    const payload = await serializeAuth(auth);
    const restored = await deserializeAuth(payload);

    expect(restored).not.toBeNull();
    expect(restored!.authMethod).toEqual({ recoveryPhrase: { principal } });
  });

  it("emailRecovery authMethod — principal preserved", async () => {
    const { identity } = await makeIdentity();
    const principal = Principal.fromText("2vxsx-fae");
    const auth: AuthWithoutAgentActor = {
      identityNumber: BigInt(1),
      identity,
      authMethod: { emailRecovery: { principal } },
    };

    const payload = await serializeAuth(auth);
    const restored = await deserializeAuth(payload);

    expect(restored).not.toBeNull();
    expect(restored!.authMethod).toEqual({ emailRecovery: { principal } });
  });

  it("DelegationChain publicKey survives serialize → deserialize", async () => {
    const { chain, identity } = await makeIdentity();
    const auth: AuthWithoutAgentActor = {
      identityNumber: BigInt(7),
      identity,
      authMethod: {
        openid: { iss: "https://appleid.apple.com", sub: "abc" },
      },
    };

    const payload = await serializeAuth(auth);
    const restored = await deserializeAuth(payload);

    expect(restored).not.toBeNull();
    const restoredChain = restored!.identity.getDelegation();
    expect(Array.from(restoredChain.publicKey)).toEqual(
      Array.from(chain.publicKey),
    );
  });

  it("restored identity has the same getPrincipal() as the original", async () => {
    const { identity } = await makeIdentity();
    const auth: AuthWithoutAgentActor = {
      identityNumber: BigInt(42),
      identity,
      authMethod: {
        openid: { iss: "https://accounts.google.com", sub: "xyz" },
      },
    };

    const payload = await serializeAuth(auth);
    const restored = await deserializeAuth(payload);

    expect(restored).not.toBeNull();
    expect(restored!.identity.getPrincipal().toText()).toBe(
      identity.getPrincipal().toText(),
    );
  });

  it("identityNumber preserved for a large bigint", async () => {
    const { identity } = await makeIdentity();
    const bigNumber = BigInt("9999999999999999");
    const auth: AuthWithoutAgentActor = {
      identityNumber: bigNumber,
      identity,
      authMethod: {
        openid: { iss: "https://accounts.google.com", sub: "big" },
      },
    };

    const payload = await serializeAuth(auth);
    const restored = await deserializeAuth(payload);

    expect(restored).not.toBeNull();
    expect(restored!.identityNumber).toBe(bigNumber);
  });

  it("deserializeAuth returns null for an expired delegation", async () => {
    const signer = await ECDSAKeyIdentity.generate({ extractable: true });
    const session = await ECDSAKeyIdentity.generate({ extractable: true });
    const chain = await DelegationChain.create(
      signer,
      session.getPublicKey(),
      new Date(Date.now() - 1000),
    );
    const identity = DelegationIdentity.fromDelegation(session, chain);
    const auth: AuthWithoutAgentActor = {
      identityNumber: BigInt(1),
      identity,
      authMethod: {
        openid: { iss: "https://accounts.google.com", sub: "x" },
      },
    };

    const payload = await serializeAuth(auth);
    const restored = await deserializeAuth(payload);

    expect(restored).toBeNull();
  });
});

describe("receiveAuthFromOpener", () => {
  afterEach(() => {
    vi.restoreAllMocks();
  });

  it("returns null after timeout when no message arrives", async () => {
    const mockOpener = { closed: false, postMessage: vi.fn() };
    vi.stubGlobal("window", {
      ...window,
      opener: mockOpener,
      addEventListener: window.addEventListener.bind(window),
      removeEventListener: window.removeEventListener.bind(window),
      location: { origin: "https://id.ai" },
    });

    const result = await receiveAuthFromOpener({ timeoutMs: 50 });
    expect(result).toBeNull();
  }, 1000);

  it("returns null immediately when window.opener is null", async () => {
    vi.stubGlobal("window", {
      ...window,
      opener: null,
    });

    const result = await receiveAuthFromOpener({ timeoutMs: 50 });
    expect(result).toBeNull();
  });

  it("returns null immediately when window.opener is closed", async () => {
    vi.stubGlobal("window", {
      ...window,
      opener: { closed: true, postMessage: vi.fn() },
    });

    const result = await receiveAuthFromOpener({ timeoutMs: 50 });
    expect(result).toBeNull();
  });

  it("ignores messages with wrong event.data.type and stays pending until timeout", async () => {
    const messageListeners: ((e: MessageEvent) => void)[] = [];
    const mockOpener = { closed: false, postMessage: vi.fn() };

    vi.stubGlobal("window", {
      ...window,
      opener: mockOpener,
      location: { origin: "https://id.ai" },
      addEventListener: (
        type: string,
        listener: EventListenerOrEventListenerObject,
      ) => {
        if (type === "message") {
          messageListeners.push(listener as (e: MessageEvent) => void);
        }
      },
      removeEventListener: vi.fn(),
    });

    const resultPromise = receiveAuthFromOpener({ timeoutMs: 100 });

    // Fire a message with wrong type
    const wrongEvent = new MessageEvent("message", {
      data: { type: "something-else" },
      origin: "https://id.ai",
      source: mockOpener as unknown as Window,
    });
    for (const listener of messageListeners) {
      listener(wrongEvent);
    }

    const result = await resultPromise;
    expect(result).toBeNull();
  }, 1000);

  it("ignores messages with wrong event.origin and stays pending until timeout", async () => {
    const messageListeners: ((e: MessageEvent) => void)[] = [];
    const mockOpener = { closed: false, postMessage: vi.fn() };

    vi.stubGlobal("window", {
      ...window,
      opener: mockOpener,
      location: { origin: "https://id.ai" },
      addEventListener: (
        type: string,
        listener: EventListenerOrEventListenerObject,
      ) => {
        if (type === "message") {
          messageListeners.push(listener as (e: MessageEvent) => void);
        }
      },
      removeEventListener: vi.fn(),
    });

    const resultPromise = receiveAuthFromOpener({ timeoutMs: 100 });

    const wrongOriginEvent = new MessageEvent("message", {
      data: { type: "ii-handoff:auth" },
      origin: "https://evil.example.com",
      source: mockOpener as unknown as Window,
    });
    for (const listener of messageListeners) {
      listener(wrongOriginEvent);
    }

    const result = await resultPromise;
    expect(result).toBeNull();
  }, 1000);

  it("ignores messages from wrong source", async () => {
    const messageListeners: ((e: MessageEvent) => void)[] = [];
    const mockOpener = { closed: false, postMessage: vi.fn() };
    const wrongSource = { closed: false };

    vi.stubGlobal("window", {
      ...window,
      opener: mockOpener,
      location: { origin: "https://id.ai" },
      addEventListener: (
        type: string,
        listener: EventListenerOrEventListenerObject,
      ) => {
        if (type === "message") {
          messageListeners.push(listener as (e: MessageEvent) => void);
        }
      },
      removeEventListener: vi.fn(),
    });

    const resultPromise = receiveAuthFromOpener({ timeoutMs: 100 });

    const wrongSourceEvent = new MessageEvent("message", {
      data: { type: "ii-handoff:auth" },
      origin: "https://id.ai",
      source: wrongSource as unknown as Window,
    });
    for (const listener of messageListeners) {
      listener(wrongSourceEvent);
    }

    const result = await resultPromise;
    expect(result).toBeNull();
  }, 1000);
});

describe("sendAuthToOpenedTab", () => {
  afterEach(() => {
    vi.restoreAllMocks();
  });

  it("synchronous cancel() during in-flight serialize prevents postMessage", async () => {
    const { identity } = await makeIdentity();
    const auth: AuthWithoutAgentActor = {
      identityNumber: BigInt(2),
      identity,
      authMethod: {
        openid: { iss: "https://accounts.google.com", sub: "sync-cancel" },
      },
    };

    const messageListeners: ((e: MessageEvent) => void)[] = [];
    const targetWindow = { postMessage: vi.fn(), closed: false };

    vi.stubGlobal("window", {
      ...window,
      location: { origin: "https://id.ai" },
      addEventListener: (
        type: string,
        listener: EventListenerOrEventListenerObject,
      ) => {
        if (type === "message") {
          messageListeners.push(listener as (e: MessageEvent) => void);
        }
      },
      removeEventListener: vi.fn(),
    });

    const { cancel } = sendAuthToOpenedTab(
      targetWindow as unknown as Window,
      auth,
    );

    // Cancel SYNCHRONOUSLY — payloadPromise (serializeAuth) is still in-flight.
    // The listener is already installed by this point (race fix), so a "ready"
    // message could arrive and trigger the listener; the `cancelled` guard
    // inside the listener must prevent the postMessage.
    cancel();

    // Now fire "ready" — the listener may still be invoked because cleanup
    // races with cancel. The `cancelled` flag is what prevents the post.
    const readyEvent = new MessageEvent("message", {
      data: { type: "ii-handoff:ready" },
      origin: "https://id.ai",
      source: targetWindow as unknown as Window,
    });
    for (const listener of messageListeners) {
      listener(readyEvent);
    }

    // Wait for any pending microtasks (serializeAuth + the await inside listener)
    await new Promise((resolve) => setTimeout(resolve, 30));

    expect(targetWindow.postMessage).not.toHaveBeenCalled();
  }, 1000);

  it("cancel() removes listener before ready arrives — subsequent ready does not trigger a post", async () => {
    const { identity } = await makeIdentity();
    const auth: AuthWithoutAgentActor = {
      identityNumber: BigInt(1),
      identity,
      authMethod: {
        openid: { iss: "https://accounts.google.com", sub: "cancel-test" },
      },
    };

    const messageListeners: ((e: MessageEvent) => void)[] = [];
    const targetWindow = { postMessage: vi.fn(), closed: false };

    vi.stubGlobal("window", {
      ...window,
      location: { origin: "https://id.ai" },
      addEventListener: (
        type: string,
        listener: EventListenerOrEventListenerObject,
      ) => {
        if (type === "message") {
          messageListeners.push(listener as (e: MessageEvent) => void);
        }
      },
      removeEventListener: vi.fn(),
    });

    const { cancel } = sendAuthToOpenedTab(
      targetWindow as unknown as Window,
      auth,
    );

    // Wait a tick for async serializeAuth to complete and listener to be installed
    await new Promise((resolve) => setTimeout(resolve, 10));

    cancel();

    // Simulate "ready" arriving after cancel
    const readyEvent = new MessageEvent("message", {
      data: { type: "ii-handoff:ready" },
      origin: "https://id.ai",
      source: targetWindow as unknown as Window,
    });
    for (const listener of messageListeners) {
      listener(readyEvent);
    }

    // Give it a tick to process
    await new Promise((resolve) => setTimeout(resolve, 10));

    expect(targetWindow.postMessage).not.toHaveBeenCalled();
  }, 1000);
});
