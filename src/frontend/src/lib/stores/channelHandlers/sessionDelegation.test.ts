import { beforeEach, describe, expect, it, vi } from "vitest";
import "fake-indexeddb/auto";

const ORIGIN = "https://app.example.com";

vi.mock("$lib/globals", async () => {
  const { Principal } = await import("@icp-sdk/core/principal");
  return {
    canisterId: Principal.fromText("rwlgt-iiaaa-aaaaa-aaaaa-cai"),
    backendCanisterConfig: { openid_configs: [] },
    frontendCanisterConfig: { related_origins: [], dev_csp: [] },
  };
});
vi.mock("$lib/utils/validateDerivationOrigin", () => ({
  validateDerivationOrigin: vi.fn(() => Promise.resolve({ result: "valid" })),
}));

const setRequestContext = vi.fn();

const IDENTITY = BigInt(10_000);
const prepareAccountSession = vi.fn();
const getAccountSession = vi.fn();

vi.mock("$lib/stores/authorization.store", () => ({
  authorizationStore: {
    setRequestContext: (...args: unknown[]) => setRequestContext(...args),
  },
  // A store that already holds its value, which is what `waitForStore` waits for.
  // Inlined rather than shared, because `vi.mock` is hoisted above anything declared
  // here.
  authorizedStore: {
    subscribe: (run: (value: unknown) => void) => {
      run({
        accessLevel: "full-access",
        maxTimeToLive: undefined,
        accountNumberPromise: Promise.resolve(undefined),
      });
      return () => {};
    },
  },
}));
vi.mock("$lib/stores/authentication.store", () => ({
  authenticationStore: {
    subscribe: (run: (value: unknown) => void) => {
      run({
        identityNumber: BigInt(10_000),
        authMethod: { passkey: {} },
        actor: {
          prepare_account_session: (...args: unknown[]) =>
            prepareAccountSession(...args),
          get_account_session: (...args: unknown[]) =>
            getAccountSession(...args),
        },
      });
      return () => {};
    },
  },
}));

import {
  asBrowserKeyError,
  handleSessionDelegationRequest,
} from "./sessionDelegation";
import { StaleBrowserKeyError } from "$lib/stores/browser-key.store";
import { CanisterError } from "$lib/utils/utils";
import {
  appSessionsForOrigin,
  purgeAppSessions,
} from "$lib/stores/app-session.store";
import { ECDSAKeyIdentity } from "@icp-sdk/core/identity";
import { Principal } from "@icp-sdk/core/principal";
import { Base64ToBytesCodec } from "$lib/utils/transport/utils";

const channelWith = () => {
  const sent: unknown[] = [];
  return {
    channel: {
      origin: ORIGIN,
      closed: false,
      resumeToken: "token",
      addEventListener: () => () => {},
      send: (response: unknown) => {
        sent.push(response);
        return Promise.resolve();
      },
      close: async () => {},
    },
    sent,
  };
};

describe("ii_session_delegation", () => {
  beforeEach(async () => {
    setRequestContext.mockClear();
    await purgeAppSessions(BigInt(10_000));
    await purgeAppSessions(BigInt(10_001));
  });

  it("ignores a request for another method", async () => {
    const { channel, sent } = channelWith();
    const onError = vi.fn();

    await handleSessionDelegationRequest(
      channel,
      onError,
    )({
      jsonrpc: "2.0",
      id: 1,
      method: "icrc34_delegation",
    });

    expect(sent).toEqual([]);
    expect(onError).not.toHaveBeenCalled();
  });

  it("rejects params that carry no session key", async () => {
    const { channel, sent } = channelWith();
    const onError = vi.fn();

    await handleSessionDelegationRequest(
      channel,
      onError,
    )({
      jsonrpc: "2.0",
      id: 1,
      method: "ii_session_delegation",
      params: {},
    });

    expect(sent).toHaveLength(1);
    expect(sent[0]).toMatchObject({ id: 1, error: { code: -32602 } });
    expect(onError).toHaveBeenCalledWith("invalid-request");
  });

  /// A duration `BigInt` cannot read throws out of `safeParse`, which sits above the
  /// handler's `try`, so the app would be told nothing at all. The rest `BigInt` reads
  /// happily as something else: `""` and `" "` are `0n`, `"+1"` is `1n`, `"0x10"` is
  /// `16n`, and the nat64 bounds reject none of them — so the canister would clamp a
  /// number the app never meant to send.
  it.each(["not a number", "", " ", "+1", "0x10", "-1"])(
    "rejects %o as a duration",
    async (maxTimeToLive) => {
      const { channel, sent } = channelWith();
      const onError = vi.fn();

      await handleSessionDelegationRequest(
        channel,
        onError,
      )({
        jsonrpc: "2.0",
        id: 1,
        method: "ii_session_delegation",
        params: {
          sessionPublicKey: btoa("an app key"),
          maxTimeToLive,
        },
      });

      expect(sent).toHaveLength(1);
      expect(sent[0]).toMatchObject({ id: 1, error: { code: -32602 } });
      expect(onError).toHaveBeenCalledWith("invalid-request");
    },
  );

  /// The whole ceremony, which nothing else here reaches: what the canister is asked
  /// for, what is kept, and what the app is handed back.
  it("mints a session and answers with a chain the app can use", async () => {
    const { channel, sent } = channelWith();
    const appKey = await ECDSAKeyIdentity.generate({ extractable: false });
    const appPublicKey = new Uint8Array(appKey.getPublicKey().toDer());
    const expiration = BigInt(Date.now() + 60 * 60 * 1000) * BigInt(1_000_000);

    prepareAccountSession.mockImplementation(({ session_key }) =>
      Promise.resolve({
        Ok: {
          user_key: session_key,
          expiration,
          session_id: BigInt(77),
          browser_id: 3,
          account_principal: Principal.anonymous(),
        },
      }),
    );
    getAccountSession.mockImplementation(({ session_key }) =>
      Promise.resolve({
        Ok: {
          signed_delegation: {
            // As the canister answers since the session credential was scoped: the
            // targets are part of what it signed, so a chain rebuilt without them is
            // refused by the replica.
            delegation: {
              pubkey: session_key,
              expiration,
              targets: [[Principal.fromText("rwlgt-iiaaa-aaaaa-aaaaa-cai")]],
            },
            // At least 32 bytes: the chain's own parser refuses anything shorter.
            signature: new Uint8Array(64).fill(7),
          },
        },
      }),
    );

    await handleSessionDelegationRequest(
      channel,
      vi.fn(),
    )({
      jsonrpc: "2.0",
      id: 1,
      method: "ii_session_delegation",
      params: { sessionPublicKey: Base64ToBytesCodec.encode(appPublicKey) },
    });

    // Asked for what the request and the consent said, at this origin.
    expect(prepareAccountSession).toHaveBeenCalledWith(
      expect.objectContaining({
        identity_number: IDENTITY,
        origin: ORIGIN,
        account_number: [],
      }),
    );

    // Kept, so a later silent re-auth resumes rather than signing in again — and kept
    // against II's own key, never the app's.
    const [stored] = await appSessionsForOrigin(ORIGIN);
    expect(stored.record.sessionId).toBe(BigInt(77));
    expect(stored.identityNumber).toBe(IDENTITY);

    // Answered, and the chain ends at the app's key rather than at what the canister
    // signed: the hop only II can make is what makes the on-chain half unusable alone.
    expect(sent).toHaveLength(1);
    expect(sent[0]).toMatchObject({ id: 1 });
    const result = (
      sent[0] as {
        result: {
          publicKey: string;
          signerDelegation: { delegation: { targets?: string[] } }[];
        };
      }
    ).result;
    expect(result.publicKey).toEqual(expect.any(String));

    // The hop the canister signed keeps its targets. Dropping them leaves a delegation
    // that hashes to nothing in the signature tree, and every call the app makes with
    // this chain comes back "Invalid canister signature".
    expect(result.signerDelegation[0].delegation.targets).toEqual([
      "rwlgt-iiaaa-aaaaa-aaaaa-cai",
    ]);
  });
});

describe("asBrowserKeyError", () => {
  it("names a retired browser key so the key store can promote its successor", () => {
    const stale = asBrowserKeyError(
      new CanisterError({ StaleBrowserKey: null }),
    );

    expect(stale).toBeInstanceOf(StaleBrowserKeyError);
  });

  it("leaves every other canister error alone", () => {
    const other = new CanisterError({ NoSuchAccount: null });

    expect(asBrowserKeyError(other)).toBe(other);
  });

  it("leaves a transport failure alone", () => {
    const network = new Error("network");

    expect(asBrowserKeyError(network)).toBe(network);
  });
});
