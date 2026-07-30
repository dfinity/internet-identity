import {
  describe,
  it,
  expect,
  beforeAll,
  beforeEach,
  afterEach,
  vi,
} from "vitest";
import { IDBFactory } from "fake-indexeddb";
import {
  createStore,
  clear as idbClear,
  entries as idbEntries,
} from "idb-keyval";
import { DelegationChain, Ed25519KeyIdentity } from "@icp-sdk/core/identity";
import type { JsonRequest, JsonResponse } from "./utils";
import { DelegationParamsCodec, DelegationResultSchema } from "./utils";
import {
  interceptDelegation,
  UrlChannel,
  UrlTransport,
  UrlTransportUnsupportedError,
  type DelegationInterceptor,
} from "./url";

// The loopback (`http`) secure-context allowance is gated on the `dev_csp`
// canister config — only the dev CSP permits the loopback allow-list fetch — so
// mock the frontend config and flip `dev_csp` per test (reset to off, i.e.
// production, in `beforeEach`).
const { mockFrontendCanisterConfig } = vi.hoisted(() => ({
  mockFrontendCanisterConfig: { dev_csp: [] as [] | [boolean] },
}));
vi.mock("$lib/globals", () => ({
  frontendCanisterConfig: mockFrontendCanisterConfig,
}));

// Same store the transport keeps its ephemeral keys in — inspected/reset here.
const EPHEMERAL_KEY_STORE = createStore("ii-icrc167-ephemeral-keys", "keys");

const CALLBACK = "https://relying-party.example/callback";
const ORIGIN = "https://relying-party.example";

const bytesEqual = (a: Uint8Array, b: Uint8Array): boolean =>
  a.length === b.length && a.every((value, index) => value === b[index]);

const delegationRequest = (sessionKey: Ed25519KeyIdentity): JsonRequest => ({
  jsonrpc: "2.0",
  id: 1,
  method: "icrc34_delegation",
  params: DelegationParamsCodec.encode({
    publicKey: sessionKey.getPublicKey(),
  }),
});

// `interceptDelegation` is the security-critical part: it must never let the
// canister sign a delegation directly against the RP-supplied key, and must
// hand the RP a chain that ends at that key. These tests exercise it end to end
// with real keys and a real delegation chain.
describe("interceptDelegation", () => {
  it("passes non-delegation requests through untouched", async () => {
    const request: JsonRequest = {
      jsonrpc: "2.0",
      id: 7,
      method: "icrc25_supported_standards",
    };
    const interceptor = await interceptDelegation(request);

    expect(interceptor.request).toEqual(request);
    const response: JsonResponse = { jsonrpc: "2.0", id: 7, result: { ok: 1 } };
    expect(await interceptor.finalize(response)).toEqual(response);
  });

  it("swaps the RP session key for a fresh intermediate key on the way in", async () => {
    const rp = Ed25519KeyIdentity.generate();
    const rpKey = new Uint8Array(rp.getPublicKey().toDer());

    const interceptor = await interceptDelegation(delegationRequest(rp));

    const rewritten = DelegationParamsCodec.parse(interceptor.request.params);
    const intermediateKey = new Uint8Array(rewritten.publicKey.toDer());
    expect(bytesEqual(intermediateKey, rpKey)).toBe(false);
  });

  it("extends the returned chain to the RP session key, keeping the canister hop on the intermediate key", async () => {
    const rp = Ed25519KeyIdentity.generate();
    const rpKey = new Uint8Array(rp.getPublicKey().toDer());
    const user = Ed25519KeyIdentity.generate();
    const userKey = new Uint8Array(user.getPublicKey().toDer());

    const interceptor = await interceptDelegation(delegationRequest(rp));
    const intermediateKey = new Uint8Array(
      DelegationParamsCodec.parse(interceptor.request.params).publicKey.toDer(),
    );

    // What the canister would sign: user -> intermediate (the rewritten key).
    const canisterChain = await DelegationChain.create(
      user,
      { toDer: () => intermediateKey },
      new Date(Date.now() + 3_600_000),
    );
    const finalized = await interceptor.finalize({
      jsonrpc: "2.0",
      id: 1,
      result: DelegationResultSchema.encode(canisterChain),
    });

    if (!("result" in finalized)) {
      throw new Error("expected a result response");
    }
    const chain = DelegationResultSchema.parse(finalized.result);

    // Two hops: user -> intermediate (canister-signed) -> RP session key (signed here).
    expect(chain.delegations).toHaveLength(2);
    expect(bytesEqual(new Uint8Array(chain.publicKey), userKey)).toBe(true);
    // The certified (on-chain) hop targets the intermediate key, never the RP key.
    expect(
      bytesEqual(
        new Uint8Array(chain.delegations[0].delegation.pubkey),
        intermediateKey,
      ),
    ).toBe(true);
    // The final hop, assembled only here, targets the RP session key.
    expect(
      bytesEqual(new Uint8Array(chain.delegations[1].delegation.pubkey), rpKey),
    ).toBe(true);
  });

  it("passes an error response through without extending a chain", async () => {
    const rp = Ed25519KeyIdentity.generate();
    const interceptor = await interceptDelegation(delegationRequest(rp));
    const error: JsonResponse = {
      jsonrpc: "2.0",
      id: 1,
      error: { code: 1000, message: "rejected" },
    };
    expect(await interceptor.finalize(error)).toEqual(error);
  });
});

const passthrough = (request: JsonRequest): DelegationInterceptor => ({
  request,
  finalize: (response) => Promise.resolve(response),
});

// Location/history stubs — the transport reads the request from the current
// URL hash and delivers the response by navigating the tab.
let assignMock: ReturnType<typeof vi.fn>;

const installLocation = (hash: string): void => {
  assignMock = vi.fn();
  Object.defineProperty(window, "location", {
    configurable: true,
    writable: true,
    value: {
      href: `http://localhost:3000/authorize${hash}`,
      origin: "http://localhost:3000",
      pathname: "/authorize",
      search: "",
      hash,
      assign: assignMock,
    },
  });
};

beforeAll(() => {
  global.indexedDB = new IDBFactory();
});

beforeEach(async () => {
  sessionStorage.clear();
  await idbClear(EPHEMERAL_KEY_STORE);
  vi.spyOn(window.history, "replaceState").mockImplementation(() => undefined);
  // Default to production (dev CSP off); the loopback test opts in explicitly.
  mockFrontendCanisterConfig.dev_csp = [];
});

afterEach(() => {
  vi.restoreAllMocks();
  vi.unstubAllGlobals();
});

describe("UrlChannel", () => {
  beforeEach(() => installLocation("#"));

  const newChannel = (batch: boolean, requests: JsonRequest[]): UrlChannel =>
    new UrlChannel({
      origin: ORIGIN,
      callback: CALLBACK,
      state: "state-123",
      batch,
      interceptors: requests.map(passthrough),
    });

  it("replays the request to a subscribing handler", () => {
    const request: JsonRequest = { jsonrpc: "2.0", id: 1, method: "m" };
    const channel = newChannel(false, [request]);
    const seen: JsonRequest[] = [];
    channel.addEventListener("request", (r) => seen.push(r));
    expect(seen).toEqual([request]);
  });

  it("delivers a single response to the callback fragment", async () => {
    const channel = newChannel(false, [{ jsonrpc: "2.0", id: 1, method: "m" }]);
    const response: JsonResponse = { jsonrpc: "2.0", id: 1, result: { a: 1 } };

    await channel.send(response);

    expect(assignMock).toHaveBeenCalledOnce();
    const url = new URL(assignMock.mock.calls[0][0]);
    expect(`${url.origin}${url.pathname}`).toBe(CALLBACK);
    const fragment = new URLSearchParams(url.hash.slice(1));
    expect(fragment.get("state")).toBe("state-123");
    // A single (non-batch) request is answered with a single response object.
    expect(JSON.parse(fragment.get("message") ?? "")).toEqual(response);
  });

  it("coalesces a batch into a single redirect once every response is in", async () => {
    const channel = newChannel(true, [
      { jsonrpc: "2.0", id: 1, method: "a" },
      { jsonrpc: "2.0", id: 2, method: "b" },
    ]);
    const respA: JsonResponse = { jsonrpc: "2.0", id: 1, result: "A" };
    const respB: JsonResponse = { jsonrpc: "2.0", id: 2, result: "B" };

    await channel.send(respB); // out of order; still waits for both
    expect(assignMock).not.toHaveBeenCalled();
    await channel.send(respA);

    expect(assignMock).toHaveBeenCalledOnce();
    const fragment = new URLSearchParams(
      new URL(assignMock.mock.calls[0][0]).hash.slice(1),
    );
    // Batch responses are an array in request order.
    expect(JSON.parse(fragment.get("message") ?? "")).toEqual([respA, respB]);
  });

  it("redirects on the first error, filling unanswered batch requests with a generic error", async () => {
    const channel = newChannel(true, [
      { jsonrpc: "2.0", id: 1, method: "a" },
      { jsonrpc: "2.0", id: 2, method: "b" },
      { jsonrpc: "2.0", id: 3, method: "c" },
    ]);

    await channel.send({ jsonrpc: "2.0", id: 1, result: "A" });
    expect(assignMock).not.toHaveBeenCalled();
    // Request 2 errors → abort the batch and redirect now, without waiting for 3.
    await channel.send({
      jsonrpc: "2.0",
      id: 2,
      error: { code: 4000, message: "boom" },
    });

    expect(assignMock).toHaveBeenCalledOnce();
    const message = JSON.parse(
      new URLSearchParams(
        new URL(assignMock.mock.calls[0][0]).hash.slice(1),
      ).get("message") ?? "",
    );
    expect(message[0]).toEqual({ jsonrpc: "2.0", id: 1, result: "A" });
    expect(message[1]).toEqual({
      jsonrpc: "2.0",
      id: 2,
      error: { code: 4000, message: "boom" },
    });
    // The unanswered request gets a generic ICRC-25 error, not a hung batch.
    expect(message[2].id).toBe(3);
    expect(message[2].error.code).toBe(1000); // GENERIC_ERROR_CODE
  });

  it("ignores stray responses after delivery", async () => {
    const channel = newChannel(false, [{ jsonrpc: "2.0", id: 1, method: "m" }]);
    await channel.send({ jsonrpc: "2.0", id: 1, result: 1 });
    await channel.send({ jsonrpc: "2.0", id: 1, result: 2 });
    expect(assignMock).toHaveBeenCalledOnce();
  });

  it("rejects send on a closed channel", async () => {
    const channel = newChannel(false, [{ jsonrpc: "2.0", id: 1, method: "m" }]);
    await channel.close();
    await expect(
      channel.send({ jsonrpc: "2.0", id: 1, result: 1 }),
    ).rejects.toThrow();
  });
});

describe("UrlTransport.establishChannel", () => {
  const stubAllowList = (callbacks: string[]): void => {
    vi.stubGlobal(
      "fetch",
      vi.fn(() =>
        Promise.resolve(
          new Response(JSON.stringify({ callbacks }), {
            status: 200,
            headers: { "content-type": "application/json" },
          }),
        ),
      ),
    );
  };

  const hashFor = (message: unknown): string => {
    const params = new URLSearchParams({
      message: JSON.stringify(message),
      callback: CALLBACK,
      state: "state-123",
    });
    return `#${params.toString()}`;
  };

  it("rejects when the hash carries no ICRC-167 request", async () => {
    installLocation("#");
    await expect(new UrlTransport().establishChannel()).rejects.toBeInstanceOf(
      UrlTransportUnsupportedError,
    );
  });

  it("establishes a channel scoped to the callback origin when the callback is declared", async () => {
    const request: JsonRequest = {
      jsonrpc: "2.0",
      id: 1,
      method: "icrc25_supported_standards",
    };
    installLocation(hashFor(request));
    stubAllowList([CALLBACK]);

    const channel = await new UrlTransport().establishChannel();

    expect(channel.origin).toBe(ORIGIN);
    const seen: JsonRequest[] = [];
    channel.addEventListener("request", (r) => seen.push(r));
    expect(seen).toEqual([request]);
  });

  it("rejects when the callback is not declared in the allow-list", async () => {
    installLocation(hashFor({ jsonrpc: "2.0", id: 1, method: "m" }));
    stubAllowList([]); // callback absent

    await expect(new UrlTransport().establishChannel()).rejects.toThrow();
  });

  it.each([
    "javascript:alert(1)",
    "data:text/html,x",
    "http://evil.example/cb", // plain http on a remote host is not a secure context
  ])(
    "rejects a non-secure-context callback (%s) before any navigation or fetch",
    async (callback) => {
      const params = new URLSearchParams({
        message: JSON.stringify({ jsonrpc: "2.0", id: 1, method: "m" }),
        callback,
        state: "state-123",
      });
      installLocation(`#${params.toString()}`);

      await expect(new UrlTransport().establishChannel()).rejects.toThrow();
      expect(assignMock).not.toHaveBeenCalled();
    },
  );

  it.each([
    "https://relying-party.example/callback?next=/home", // query
    "https://relying-party.example/callback#frag", // fragment
    "https://user:pass@relying-party.example/callback", // credentials
  ])(
    "rejects a callback that is not protocol + host + path only (%s)",
    async (callback) => {
      const params = new URLSearchParams({
        message: JSON.stringify({ jsonrpc: "2.0", id: 1, method: "m" }),
        callback,
        state: "state-123",
      });
      installLocation(`#${params.toString()}`);

      await expect(new UrlTransport().establishChannel()).rejects.toThrow();
      expect(assignMock).not.toHaveBeenCalled();
    },
  );

  it("allows an http callback on loopback when the dev CSP is enabled", async () => {
    mockFrontendCanisterConfig.dev_csp = [true];
    const callback = "http://localhost:8080/callback";
    const params = new URLSearchParams({
      message: JSON.stringify({
        jsonrpc: "2.0",
        id: 1,
        method: "icrc25_supported_standards",
      }),
      callback,
      state: "state-123",
    });
    installLocation(`#${params.toString()}`);
    stubAllowList([callback]);

    const channel = await new UrlTransport().establishChannel();
    expect(channel.origin).toBe("http://localhost:8080");
  });

  it("rejects an http loopback callback when the dev CSP is disabled (production)", async () => {
    // dev_csp defaults to off in beforeEach: II's production CSP would block
    // the loopback allow-list fetch, so the callback is not a secure context.
    const params = new URLSearchParams({
      message: JSON.stringify({ jsonrpc: "2.0", id: 1, method: "m" }),
      callback: "http://localhost:8080/callback",
      state: "state-123",
    });
    installLocation(`#${params.toString()}`);

    await expect(new UrlTransport().establishChannel()).rejects.toThrow();
    expect(assignMock).not.toHaveBeenCalled();
  });

  it("rejects a request without an id (a notification cannot be answered)", async () => {
    installLocation(hashFor({ jsonrpc: "2.0", method: "m" }));
    stubAllowList([CALLBACK]);

    await expect(new UrlTransport().establishChannel()).rejects.toThrow();
    expect(assignMock).not.toHaveBeenCalled();
  });

  it("rejects a message larger than the cap before parsing", async () => {
    const params = new URLSearchParams({
      message: "x".repeat(17 * 1024),
      callback: CALLBACK,
      state: "state-123",
    });
    installLocation(`#${params.toString()}`);

    await expect(new UrlTransport().establishChannel()).rejects.toThrow();
    expect(assignMock).not.toHaveBeenCalled();
  });

  it("rejects a batch with too many requests", async () => {
    const batch = Array.from({ length: 11 }, (_, i) => ({
      jsonrpc: "2.0",
      id: i,
      method: "m",
    }));
    installLocation(hashFor(batch));

    await expect(new UrlTransport().establishChannel()).rejects.toThrow();
    expect(assignMock).not.toHaveBeenCalled();
  });

  it("rejects a batch with duplicate request ids", async () => {
    const batch = [
      { jsonrpc: "2.0", id: 1, method: "a" },
      { jsonrpc: "2.0", id: 1, method: "b" },
    ];
    installLocation(hashFor(batch));

    await expect(new UrlTransport().establishChannel()).rejects.toThrow();
    expect(assignMock).not.toHaveBeenCalled();
  });

  it("journals distinct ephemeral keys for numeric and string ids that stringify alike", async () => {
    const batch = [1, "1"].map((id) => ({
      jsonrpc: "2.0",
      id,
      method: "icrc34_delegation",
      params: DelegationParamsCodec.encode({
        publicKey: Ed25519KeyIdentity.generate().getPublicKey(),
      }),
    }));
    installLocation(hashFor(batch));
    stubAllowList([CALLBACK]);

    await new UrlTransport().establishChannel();

    const flow = JSON.parse(
      sessionStorage.getItem("ii-icrc167-url-flow") ?? "",
    );
    // The numeric `1` and string `"1"` are journaled under distinct type-tagged
    // keys, so neither delegation's ephemeral key overwrites the other's.
    expect(Object.keys(flow.ephemeralKeyIds)).toHaveLength(2);
    expect(new Set(Object.values(flow.ephemeralKeyIds)).size).toBe(2);
  });

  const STORAGE_KEY = "ii-icrc167-url-flow";
  // Resume-mode options as the authorize page would supply them: the stored
  // flow's origin and its resume token (read back from storage, as the page
  // reads the stashed token it captured from the channel before the redirect).
  const resumeOpts = (): { allowedOrigin: string; resumeToken: string } => {
    const flow = JSON.parse(
      sessionStorage.getItem(STORAGE_KEY) ?? "{}",
    ) as Partial<{ origin: string; resumeToken: string }>;
    return {
      allowedOrigin: flow.origin ?? ORIGIN,
      resumeToken: flow.resumeToken ?? "",
    };
  };
  const messageFrom = (): unknown =>
    JSON.parse(
      new URLSearchParams(
        new URL(assignMock.mock.calls[0][0]).hash.slice(1),
      ).get("message") ?? "",
    );

  it("persists a fresh flow and resumes it from storage on a hashless load", async () => {
    const request: JsonRequest = {
      jsonrpc: "2.0",
      id: 1,
      method: "icrc25_supported_standards",
    };
    installLocation(hashFor(request));
    stubAllowList([CALLBACK]);
    await new UrlTransport().establishChannel();
    expect(sessionStorage.getItem(STORAGE_KEY)).not.toBeNull();

    // The return load (e.g. after an OpenID hop): no hash, no allow-list fetch.
    installLocation("#");
    const resumed = await new UrlTransport().establishChannel(resumeOpts());
    expect(resumed.origin).toBe(ORIGIN);
    const seen: JsonRequest[] = [];
    resumed.addEventListener("request", (r) => seen.push(r));
    expect(seen).toEqual([request]);

    await resumed.send({ jsonrpc: "2.0", id: 1, result: { ok: 1 } });
    expect(assignMock).toHaveBeenCalledOnce();
    expect(sessionStorage.getItem(STORAGE_KEY)).toBeNull(); // cleared on delivery
  });

  it("reuses the persisted ephemeral key across a resume so the second hop targets the RP key", async () => {
    const rp = Ed25519KeyIdentity.generate();
    const rpKey = new Uint8Array(rp.getPublicKey().toDer());
    const user = Ed25519KeyIdentity.generate();

    installLocation(hashFor(delegationRequest(rp)));
    stubAllowList([CALLBACK]);
    await new UrlTransport().establishChannel(); // generates + persists ephemeral

    installLocation("#");
    const resumed = await new UrlTransport().establishChannel(resumeOpts());
    const seen: JsonRequest[] = [];
    resumed.addEventListener("request", (r) => seen.push(r));
    const ephemeralKey = new Uint8Array(
      DelegationParamsCodec.parse(seen[0].params).publicKey.toDer(),
    );

    const canisterChain = await DelegationChain.create(
      user,
      { toDer: () => ephemeralKey },
      new Date(Date.now() + 3_600_000),
    );
    await resumed.send({
      jsonrpc: "2.0",
      id: 1,
      result: DelegationResultSchema.encode(canisterChain),
    });

    const response = messageFrom() as { result: unknown };
    const chain = DelegationResultSchema.parse(response.result);
    expect(chain.delegations).toHaveLength(2);
    expect(
      bytesEqual(new Uint8Array(chain.delegations[1].delegation.pubkey), rpKey),
    ).toBe(true);
  });

  it("journals only the ephemeral key id — the key pair stays non-extractable in IndexedDB, never in sessionStorage", async () => {
    const rp = Ed25519KeyIdentity.generate();
    installLocation(hashFor(delegationRequest(rp)));
    stubAllowList([CALLBACK]);
    await new UrlTransport().establishChannel();

    // The journaled flow references the key by id and carries no key material.
    const flowJson = sessionStorage.getItem(STORAGE_KEY) ?? "";
    const flow = JSON.parse(flowJson) as {
      ephemeralKeyIds: Record<string, string>;
    };
    expect(Object.values(flow.ephemeralKeyIds)).toHaveLength(1);
    expect(flowJson).not.toMatch(/[Jj]wk|privateKey|"d":/);

    // The key pair itself is a single non-extractable CryptoKeyPair in IndexedDB.
    const entries = await idbEntries<string, { keyPair: CryptoKeyPair }>(
      EPHEMERAL_KEY_STORE,
    );
    expect(entries).toHaveLength(1);
    expect(entries[0][1].keyPair.privateKey.extractable).toBe(false);
  });

  it("resumes a partially-answered batch, re-emitting only the unanswered request", async () => {
    const batch: JsonRequest[] = [
      { jsonrpc: "2.0", id: 1, method: "a" },
      { jsonrpc: "2.0", id: 2, method: "b" },
    ];
    installLocation(hashFor(batch));
    stubAllowList([CALLBACK]);
    const first = await new UrlTransport().establishChannel();
    await first.send({ jsonrpc: "2.0", id: 1, result: "A" });
    expect(assignMock).not.toHaveBeenCalled(); // batch not complete, no redirect

    // Redirect, then resume: request 1's response is restored from storage.
    installLocation("#");
    const resumed = await new UrlTransport().establishChannel(resumeOpts());
    const seen: JsonRequest[] = [];
    resumed.addEventListener("request", (r) => seen.push(r));
    expect(seen.map((r) => r.id)).toEqual([2]); // only the unanswered one

    await resumed.send({ jsonrpc: "2.0", id: 2, result: "B" });
    expect(assignMock).toHaveBeenCalledOnce();
    expect(messageFrom()).toEqual([
      { jsonrpc: "2.0", id: 1, result: "A" },
      { jsonrpc: "2.0", id: 2, result: "B" },
    ]);
  });

  it("ignores and clears an expired stored flow", async () => {
    sessionStorage.setItem(
      STORAGE_KEY,
      JSON.stringify({
        origin: ORIGIN,
        callback: CALLBACK,
        state: "s",
        batch: false,
        timestamp: 0, // long expired
        requests: [{ jsonrpc: "2.0", id: 1, method: "m" }],
        ephemeralKeyIds: {},
        responses: {},
      }),
    );
    installLocation("#");

    await expect(new UrlTransport().establishChannel()).rejects.toBeInstanceOf(
      UrlTransportUnsupportedError,
    );
    expect(sessionStorage.getItem(STORAGE_KEY)).toBeNull();
  });

  it("fails to resume a delegation flow whose ephemeral key was never journaled", async () => {
    // A stored flow with a delegation request but no journaled key (corruption
    // / tampering) must fail rather than pass the delegation through
    // un-intercepted — the canister would then certify to the RP-supplied key.
    const rp = Ed25519KeyIdentity.generate();
    sessionStorage.setItem(
      STORAGE_KEY,
      JSON.stringify({
        origin: ORIGIN,
        callback: CALLBACK,
        state: "s",
        batch: false,
        timestamp: Date.now(),
        resumeToken: "tok-1",
        requests: [delegationRequest(rp)],
        ephemeralKeyIds: {},
        responses: {},
      }),
    );
    installLocation("#");

    await expect(
      new UrlTransport().establishChannel({
        allowedOrigin: ORIGIN,
        resumeToken: "tok-1",
      }),
    ).rejects.toThrow(/no ephemeral key journaled/);
  });

  it("rejects a malformed stored flow instead of trusting it", async () => {
    sessionStorage.setItem(STORAGE_KEY, JSON.stringify({ origin: ORIGIN }));
    installLocation("#");

    await expect(
      new UrlTransport().establishChannel({ allowedOrigin: ORIGIN }),
    ).rejects.toBeInstanceOf(UrlTransportUnsupportedError);
    expect(sessionStorage.getItem(STORAGE_KEY)).toBeNull();
  });

  it("resumes only when the resume token (and origin) prove ownership of the flow", async () => {
    // Persist a fresh flow, then simulate the hashless return load.
    const persistFlow = async () => {
      installLocation(hashFor({ jsonrpc: "2.0", id: 1, method: "m" }));
      stubAllowList([CALLBACK]);
      await new UrlTransport().establishChannel();
      installLocation("#");
      expect(sessionStorage.getItem(STORAGE_KEY)).not.toBeNull();
    };

    // A normal load (no resume options) is not a resume for us: decline and
    // drop the abandoned flow rather than pick it up.
    await persistFlow();
    await expect(new UrlTransport().establishChannel()).rejects.toBeInstanceOf(
      UrlTransportUnsupportedError,
    );
    expect(sessionStorage.getItem(STORAGE_KEY)).toBeNull();

    // A resume for a different origin declines (a load for another app carries
    // that app's origin, so a stray copy of this flow can't be resumed there).
    await persistFlow();
    await expect(
      new UrlTransport().establishChannel({
        allowedOrigin: "https://other.example",
        resumeToken: resumeOpts().resumeToken,
      }),
    ).rejects.toBeInstanceOf(UrlTransportUnsupportedError);

    // The origin matches but the resume token is another channel's: declines —
    // this is what closes the same-origin case (e.g. a sibling flow's establish
    // can't resume a lingering flow it doesn't own).
    await persistFlow();
    await expect(
      new UrlTransport().establishChannel({
        allowedOrigin: ORIGIN,
        resumeToken: "some-other-channels-token",
      }),
    ).rejects.toBeInstanceOf(UrlTransportUnsupportedError);

    // Origin and token both prove ownership: resumes.
    await persistFlow();
    const resumed = await new UrlTransport().establishChannel(resumeOpts());
    expect(resumed.origin).toBe(ORIGIN);
  });

  it("clears the persisted flow when the channel is closed without delivering", async () => {
    installLocation(hashFor({ jsonrpc: "2.0", id: 1, method: "m" }));
    stubAllowList([CALLBACK]);
    const channel = await new UrlTransport().establishChannel();
    expect(sessionStorage.getItem(STORAGE_KEY)).not.toBeNull();

    await channel.close();
    expect(sessionStorage.getItem(STORAGE_KEY)).toBeNull();
  });
});
