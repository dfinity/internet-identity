import {
  DelegationChain,
  DelegationIdentity,
  ECDSAKeyIdentity,
  isDelegationValid,
} from "@icp-sdk/core/identity";
import { Principal } from "@icp-sdk/core/principal";
import { describe, expect, it, vi, afterEach } from "vitest";
import { receiveAuthFromOpener, sendAuthToOpenedTab } from "./auth-handoff";
import { toBase64 } from "./utils";

const stubReceiveWindow = (params: {
  opener: { closed: boolean; postMessage: ReturnType<typeof vi.fn> } | null;
  hash?: string;
  messageListeners?: ((e: MessageEvent) => void)[];
  replaceState?: ReturnType<typeof vi.fn>;
}) => {
  const { opener, hash = "#handoff", messageListeners, replaceState } = params;
  const locationStub = {
    origin: "https://id.ai",
    hash,
    pathname: "/manage",
    search: "",
  };
  vi.stubGlobal("location", locationStub);
  vi.stubGlobal("history", { replaceState: replaceState ?? vi.fn() });
  vi.stubGlobal("window", {
    ...window,
    opener,
    location: locationStub,
    addEventListener:
      messageListeners !== undefined
        ? (type: string, listener: EventListenerOrEventListenerObject) => {
            if (type === "message") {
              messageListeners.push(listener as (e: MessageEvent) => void);
            }
          }
        : window.addEventListener.bind(window),
    removeEventListener: vi.fn(),
  });
};

import type { Authenticated } from "$lib/stores/authentication.store";

type AuthWithoutAgentActor = Omit<
  Authenticated,
  "agent" | "actor" | "salt" | "nonce"
>;

async function makeOpenerAuth(
  authMethod?: AuthWithoutAgentActor["authMethod"],
): Promise<AuthWithoutAgentActor> {
  const root = await ECDSAKeyIdentity.generate({ extractable: false });
  const session = await ECDSAKeyIdentity.generate({ extractable: false });
  const chain = await DelegationChain.create(
    root,
    session.getPublicKey(),
    new Date(Date.now() + 30 * 60 * 1000),
  );
  const identity = DelegationIdentity.fromDelegation(session, chain);
  return {
    identityNumber: BigInt(42),
    identity,
    authMethod: authMethod ?? {
      openid: { iss: "https://accounts.google.com", sub: "u1" },
    },
  };
}

async function makeReceiverKey(): Promise<{
  innerKey: ECDSAKeyIdentity;
  publicKeyDer: string;
}> {
  const innerKey = await ECDSAKeyIdentity.generate({ extractable: false });
  const publicKeyDer = toBase64(innerKey.getPublicKey().toDer());
  return { innerKey, publicKeyDer };
}

function stubSenderWindow(
  messageListeners: ((e: MessageEvent) => void)[],
): void {
  vi.stubGlobal("location", { origin: "https://id.ai" });
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
}

async function fireReadyAndWait(
  messageListeners: ((e: MessageEvent) => void)[],
  targetWindow: { postMessage: ReturnType<typeof vi.fn> },
  publicKeyDer: string,
): Promise<void> {
  const readyEvent = new MessageEvent("message", {
    data: { type: "ii-handoff:ready", publicKeyDer },
    origin: "https://id.ai",
    source: targetWindow as unknown as Window,
  });
  for (const listener of messageListeners) {
    listener(readyEvent);
  }
  await vi.waitUntil(() => targetWindow.postMessage.mock.calls.length > 0, {
    timeout: 1500,
  });
}

describe("sendAuthToOpenedTab / receiveAuthFromOpener — delegation protocol", () => {
  afterEach(() => {
    vi.restoreAllMocks();
  });

  it("round-trip with passkey authMethod — receiver builds valid DelegationIdentity", async () => {
    const credentialId = new Uint8Array([1, 2, 3, 4, 5]);
    const auth = await makeOpenerAuth({ passkey: { credentialId } });
    const { innerKey, publicKeyDer } = await makeReceiverKey();

    const messageListeners: ((e: MessageEvent) => void)[] = [];
    const targetWindow = { postMessage: vi.fn(), closed: false };
    stubSenderWindow(messageListeners);

    sendAuthToOpenedTab(targetWindow as unknown as Window, auth);
    await fireReadyAndWait(messageListeners, targetWindow, publicKeyDer);

    const [payload] = targetWindow.postMessage.mock.calls[0];
    expect(payload.type).toBe("ii-handoff:auth");

    const newChain = DelegationChain.fromJSON(JSON.parse(payload.chainJson));
    expect(isDelegationValid(newChain)).toBe(true);

    const receiverIdentity = DelegationIdentity.fromDelegation(
      innerKey,
      newChain,
    );
    expect(receiverIdentity.getPrincipal().toText()).toBe(
      auth.identity.getPrincipal().toText(),
    );
    expect(payload.authMethod).toEqual({
      kind: "passkey",
      credentialId: expect.any(String),
    });
    expect(payload.identityNumber).toBe("42");
  }, 2000);

  it("round-trip with openid authMethod — principal preserved", async () => {
    const auth = await makeOpenerAuth({
      openid: { iss: "https://accounts.google.com", sub: "u1" },
    });
    const { innerKey, publicKeyDer } = await makeReceiverKey();

    const messageListeners: ((e: MessageEvent) => void)[] = [];
    const targetWindow = { postMessage: vi.fn(), closed: false };
    stubSenderWindow(messageListeners);

    sendAuthToOpenedTab(targetWindow as unknown as Window, auth);
    await fireReadyAndWait(messageListeners, targetWindow, publicKeyDer);

    const [payload] = targetWindow.postMessage.mock.calls[0];
    const newChain = DelegationChain.fromJSON(JSON.parse(payload.chainJson));
    const receiverIdentity = DelegationIdentity.fromDelegation(
      innerKey,
      newChain,
    );
    expect(receiverIdentity.getPrincipal().toText()).toBe(
      auth.identity.getPrincipal().toText(),
    );
    expect(payload.authMethod).toEqual({
      kind: "openid",
      iss: "https://accounts.google.com",
      sub: "u1",
    });
  }, 2000);

  it("round-trip with recoveryPhrase authMethod", async () => {
    const auth = await makeOpenerAuth({
      recoveryPhrase: { principal: Principal.fromText("2vxsx-fae") },
    });
    const { publicKeyDer } = await makeReceiverKey();

    const messageListeners: ((e: MessageEvent) => void)[] = [];
    const targetWindow = { postMessage: vi.fn(), closed: false };
    stubSenderWindow(messageListeners);

    sendAuthToOpenedTab(targetWindow as unknown as Window, auth);
    await fireReadyAndWait(messageListeners, targetWindow, publicKeyDer);

    const [payload] = targetWindow.postMessage.mock.calls[0];
    expect(payload.authMethod).toEqual({
      kind: "recoveryPhrase",
      principal: "2vxsx-fae",
    });
  }, 2000);

  it("round-trip with emailRecovery authMethod", async () => {
    const auth = await makeOpenerAuth({
      emailRecovery: { principal: Principal.fromText("2vxsx-fae") },
    });
    const { publicKeyDer } = await makeReceiverKey();

    const messageListeners: ((e: MessageEvent) => void)[] = [];
    const targetWindow = { postMessage: vi.fn(), closed: false };
    stubSenderWindow(messageListeners);

    sendAuthToOpenedTab(targetWindow as unknown as Window, auth);
    await fireReadyAndWait(messageListeners, targetWindow, publicKeyDer);

    const [payload] = targetWindow.postMessage.mock.calls[0];
    expect(payload.authMethod).toEqual({
      kind: "emailRecovery",
      principal: "2vxsx-fae",
    });
  }, 2000);

  it("new chain expiration is within 30 minutes from now", async () => {
    const auth = await makeOpenerAuth();
    const { publicKeyDer } = await makeReceiverKey();
    const before = Date.now();

    const messageListeners: ((e: MessageEvent) => void)[] = [];
    const targetWindow = { postMessage: vi.fn(), closed: false };
    stubSenderWindow(messageListeners);

    sendAuthToOpenedTab(targetWindow as unknown as Window, auth);
    await fireReadyAndWait(messageListeners, targetWindow, publicKeyDer);

    const [payload] = targetWindow.postMessage.mock.calls[0];
    const newChain = DelegationChain.fromJSON(JSON.parse(payload.chainJson));

    const lastDelegation =
      newChain.delegations[newChain.delegations.length - 1];
    const expirationMs = Number(
      lastDelegation.delegation.expiration / BigInt(1_000_000),
    );
    expect(expirationMs).toBeGreaterThan(before);
    expect(expirationMs).toBeLessThanOrEqual(before + 30 * 60 * 1000 + 1000);
  }, 2000);

  it("expired root delegation — receiver gets an invalid chain", async () => {
    const root = await ECDSAKeyIdentity.generate({ extractable: false });
    const session = await ECDSAKeyIdentity.generate({ extractable: false });
    const expiredChain = await DelegationChain.create(
      root,
      session.getPublicKey(),
      new Date(Date.now() - 1000),
    );
    const identity = DelegationIdentity.fromDelegation(session, expiredChain);
    const auth: AuthWithoutAgentActor = {
      identityNumber: BigInt(1),
      identity,
      authMethod: {
        openid: { iss: "https://accounts.google.com", sub: "expired" },
      },
    };
    const { publicKeyDer } = await makeReceiverKey();

    const messageListeners: ((e: MessageEvent) => void)[] = [];
    const targetWindow = { postMessage: vi.fn(), closed: false };
    stubSenderWindow(messageListeners);

    sendAuthToOpenedTab(targetWindow as unknown as Window, auth);

    const readyEvent = new MessageEvent("message", {
      data: { type: "ii-handoff:ready", publicKeyDer },
      origin: "https://id.ai",
      source: targetWindow as unknown as Window,
    });
    for (const listener of messageListeners) {
      listener(readyEvent);
    }

    await new Promise((resolve) => setTimeout(resolve, 200));

    if (targetWindow.postMessage.mock.calls.length > 0) {
      const [payload] = targetWindow.postMessage.mock.calls[0];
      const newChain = DelegationChain.fromJSON(JSON.parse(payload.chainJson));
      expect(isDelegationValid(newChain)).toBe(false);
    }
  }, 2000);

  it("cancel before ready arrives — no auth message posted", async () => {
    const auth = await makeOpenerAuth();

    const messageListeners: ((e: MessageEvent) => void)[] = [];
    const targetWindow = { postMessage: vi.fn(), closed: false };
    stubSenderWindow(messageListeners);

    const { cancel } = sendAuthToOpenedTab(
      targetWindow as unknown as Window,
      auth,
    );

    cancel();

    const { publicKeyDer } = await makeReceiverKey();
    const readyEvent = new MessageEvent("message", {
      data: { type: "ii-handoff:ready", publicKeyDer },
      origin: "https://id.ai",
      source: targetWindow as unknown as Window,
    });
    for (const listener of messageListeners) {
      listener(readyEvent);
    }

    await new Promise((resolve) => setTimeout(resolve, 200));

    expect(targetWindow.postMessage).not.toHaveBeenCalled();
  }, 2000);

  it("ignores ready messages from wrong source", async () => {
    const auth = await makeOpenerAuth();
    const { publicKeyDer } = await makeReceiverKey();

    const messageListeners: ((e: MessageEvent) => void)[] = [];
    const targetWindow = { postMessage: vi.fn(), closed: false };
    const wrongSource = { postMessage: vi.fn(), closed: false };
    stubSenderWindow(messageListeners);

    sendAuthToOpenedTab(targetWindow as unknown as Window, auth, 100);

    const wrongSourceReady = new MessageEvent("message", {
      data: { type: "ii-handoff:ready", publicKeyDer },
      origin: "https://id.ai",
      source: wrongSource as unknown as Window,
    });
    for (const listener of messageListeners) {
      listener(wrongSourceReady);
    }

    await new Promise((resolve) => setTimeout(resolve, 150));

    expect(targetWindow.postMessage).not.toHaveBeenCalled();
  }, 2000);

  it("ignores ready messages with wrong origin", async () => {
    const auth = await makeOpenerAuth();
    const { publicKeyDer } = await makeReceiverKey();

    const messageListeners: ((e: MessageEvent) => void)[] = [];
    const targetWindow = { postMessage: vi.fn(), closed: false };
    stubSenderWindow(messageListeners);

    sendAuthToOpenedTab(targetWindow as unknown as Window, auth, 100);

    const wrongOriginReady = new MessageEvent("message", {
      data: { type: "ii-handoff:ready", publicKeyDer },
      origin: "https://evil.example.com",
      source: targetWindow as unknown as Window,
    });
    for (const listener of messageListeners) {
      listener(wrongOriginReady);
    }

    await new Promise((resolve) => setTimeout(resolve, 150));

    expect(targetWindow.postMessage).not.toHaveBeenCalled();
  }, 2000);
});

describe("receiveAuthFromOpener", () => {
  afterEach(() => {
    vi.restoreAllMocks();
  });

  it("returns null after timeout when no message arrives", async () => {
    stubReceiveWindow({ opener: { closed: false, postMessage: vi.fn() } });

    const result = await receiveAuthFromOpener({ timeoutMs: 50 });
    expect(result).toBeNull();
  }, 1000);

  it("returns null immediately when window.opener is null", async () => {
    stubReceiveWindow({ opener: null });

    const result = await receiveAuthFromOpener({ timeoutMs: 50 });
    expect(result).toBeNull();
  });

  it("returns null immediately when window.opener is closed", async () => {
    stubReceiveWindow({ opener: { closed: true, postMessage: vi.fn() } });

    const result = await receiveAuthFromOpener({ timeoutMs: 50 });
    expect(result).toBeNull();
  });

  it("returns null immediately when URL hash has no handoff marker", async () => {
    stubReceiveWindow({
      opener: { closed: false, postMessage: vi.fn() },
      hash: "",
    });

    const result = await receiveAuthFromOpener({ timeoutMs: 50 });
    expect(result).toBeNull();
  });

  it("eagerly strips the handoff marker from the URL fragment", () => {
    const replaceState = vi.fn();
    stubReceiveWindow({
      opener: { closed: false, postMessage: vi.fn() },
      hash: "#handoff",
      replaceState,
    });

    void receiveAuthFromOpener({ timeoutMs: 50 });

    expect(replaceState).toHaveBeenCalledWith(null, "", "/manage");
  });

  it("preserves other hash params when stripping the handoff marker", () => {
    const replaceState = vi.fn();
    stubReceiveWindow({
      opener: { closed: false, postMessage: vi.fn() },
      hash: "#handoff&other=keep",
      replaceState,
    });

    void receiveAuthFromOpener({ timeoutMs: 50 });

    expect(replaceState).toHaveBeenCalledWith(null, "", "/manage#other=keep");
  });

  it("ignores messages with wrong event.data.type", async () => {
    const messageListeners: ((e: MessageEvent) => void)[] = [];
    const mockOpener = { closed: false, postMessage: vi.fn() };
    stubReceiveWindow({ opener: mockOpener, messageListeners });

    const resultPromise = receiveAuthFromOpener({ timeoutMs: 100 });

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

  it("ignores messages with wrong event.origin", async () => {
    const messageListeners: ((e: MessageEvent) => void)[] = [];
    const mockOpener = { closed: false, postMessage: vi.fn() };
    stubReceiveWindow({ opener: mockOpener, messageListeners });

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
    stubReceiveWindow({ opener: mockOpener, messageListeners });

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

  it("posts a ready message with publicKeyDer to opener", async () => {
    const mockOpener = { closed: false, postMessage: vi.fn() };
    stubReceiveWindow({ opener: mockOpener });

    void receiveAuthFromOpener({ timeoutMs: 200 });

    await vi.waitUntil(() => mockOpener.postMessage.mock.calls.length > 0, {
      timeout: 500,
    });

    expect(mockOpener.postMessage).toHaveBeenCalledWith(
      expect.objectContaining({
        type: "ii-handoff:ready",
        publicKeyDer: expect.any(String),
      }),
      expect.any(String),
    );
  }, 1000);

  it("returns null when the received delegation chain is already expired", async () => {
    const root = await ECDSAKeyIdentity.generate({ extractable: false });
    const session = await ECDSAKeyIdentity.generate({ extractable: false });
    const expiredChain = await DelegationChain.create(
      root,
      session.getPublicKey(),
      new Date(Date.now() - 1000),
    );

    const messageListeners: ((e: MessageEvent) => void)[] = [];
    const mockOpener = { closed: false, postMessage: vi.fn() };
    stubReceiveWindow({ opener: mockOpener, messageListeners });

    const resultPromise = receiveAuthFromOpener({ timeoutMs: 500 });

    await vi.waitUntil(() => mockOpener.postMessage.mock.calls.length > 0, {
      timeout: 500,
    });

    const authEvent = new MessageEvent("message", {
      data: {
        type: "ii-handoff:auth",
        identityNumber: "1",
        chainJson: JSON.stringify(expiredChain.toJSON()),
        authMethod: {
          kind: "openid",
          iss: "https://accounts.google.com",
          sub: "x",
        },
      },
      origin: "https://id.ai",
      source: mockOpener as unknown as Window,
    });
    for (const listener of messageListeners) {
      listener(authEvent);
    }

    const result = await resultPromise;
    expect(result).toBeNull();
  }, 2000);

  it("auth message arrives before localInnerKey is set — resolves to null", async () => {
    let resolveKeyGeneration!: (key: ECDSAKeyIdentity) => void;
    const deferredKey = new Promise<ECDSAKeyIdentity>((resolve) => {
      resolveKeyGeneration = resolve;
    });
    vi.spyOn(ECDSAKeyIdentity, "generate").mockReturnValueOnce(deferredKey);

    const messageListeners: ((e: MessageEvent) => void)[] = [];
    const mockOpener = { closed: false, postMessage: vi.fn() };
    stubReceiveWindow({ opener: mockOpener, messageListeners });

    const resultPromise = receiveAuthFromOpener({ timeoutMs: 500 });

    const root = await ECDSAKeyIdentity.generate({ extractable: false });
    const session = await ECDSAKeyIdentity.generate({ extractable: false });
    const chain = await DelegationChain.create(
      root,
      session.getPublicKey(),
      new Date(Date.now() + 30 * 60 * 1000),
    );

    const authEvent = new MessageEvent("message", {
      data: {
        type: "ii-handoff:auth",
        identityNumber: "42",
        chainJson: JSON.stringify(chain.toJSON()),
        authMethod: {
          kind: "openid",
          iss: "https://accounts.google.com",
          sub: "race-test",
        },
      },
      origin: "https://id.ai",
      source: mockOpener as unknown as Window,
    });
    for (const listener of messageListeners) {
      listener(authEvent);
    }

    const result = await resultPromise;
    expect(result).toBeNull();

    const cleanupKey = await ECDSAKeyIdentity.generate({ extractable: false });
    resolveKeyGeneration(cleanupKey);
  }, 3000);
});
