import { describe, it, expect, beforeEach } from "vitest";
import { Delegation, ECDSAKeyIdentity } from "@icp-sdk/core/identity";
import type { Signature } from "@icp-sdk/core/agent";
import { endRedirectSession, REDIRECT_SESSION_STORAGE_KEY } from "./legacy";
import type { AuthResponse } from "./utils";

// `endRedirectSession` reconstructs the inner delegation a second time (see
// `legacy.ts`), independently of the `AuthResponseCodec` / `DelegationResultSchema`
// round-trip tests in `utils.test.ts`. Its output is posted to the relying party
// as a raw `postMessage`, bypassing those codecs entirely, so a regression here
// (e.g. dropping the `permissions` constructor arg) would pass every other test
// while silently stripping the restriction on the legacy redirect flow.
describe("endRedirectSession", () => {
  const authOrigin = "https://relying-party.example";

  beforeEach(() => {
    sessionStorage.clear();
  });

  const setUpRedirectSession = async () => {
    const sessionKeyPair = await crypto.subtle.generateKey(
      { name: "ECDSA", namedCurve: "P-256" },
      true,
      ["sign", "verify"],
    );
    const privateJwk = await crypto.subtle.exportKey(
      "jwk",
      sessionKeyPair.privateKey,
    );
    const publicJwk = await crypto.subtle.exportKey(
      "jwk",
      sessionKeyPair.publicKey,
    );
    const sessionIdentity = await ECDSAKeyIdentity.fromKeyPair({
      privateKey: sessionKeyPair.privateKey,
      publicKey: sessionKeyPair.publicKey,
    });
    const sessionPublicKeyDer = new Uint8Array(
      sessionIdentity.getPublicKey().toDer(),
    );

    // The dapp's final session public key: opaque bytes as far as
    // `endRedirectSession` is concerned (only re-wrapped, never verified).
    const authPublicKey = new Uint8Array(32).fill(7);

    sessionStorage.setItem(
      REDIRECT_SESSION_STORAGE_KEY,
      JSON.stringify({
        privateJwk,
        publicJwk,
        authOrigin,
        authPublicKey: bytesToBase64(authPublicKey),
        timestamp: Date.now(),
      }),
    );

    return { sessionPublicKeyDer };
  };

  function bytesToBase64(bytes: Uint8Array): string {
    let binary = "";
    for (const b of bytes) binary += String.fromCharCode(b);
    return btoa(binary);
  }

  it("preserves a queries-only delegation's permissions through to the posted response", async () => {
    const { sessionPublicKeyDer } = await setUpRedirectSession();

    const authResponse: AuthResponse = {
      kind: "authorize-client-success",
      delegations: [
        {
          delegation: new Delegation(
            sessionPublicKeyDer,
            BigInt(Date.now() + 3_600_000) * BigInt(1_000_000),
            undefined,
            "queries",
          ),
          signature: new Uint8Array(64) as unknown as Signature,
        },
      ],
      userPublicKey: new Uint8Array(32).fill(1),
      authnMethod: "passkey",
    };

    const result = await endRedirectSession(authOrigin, authResponse);

    if (result.kind !== "authorize-client-success") {
      throw new Error("expected success response");
    }
    // The reconstructed inner delegation is first in the chain; the newly
    // minted outer (session) delegation is appended after it.
    expect(result.delegations[0].delegation.permissions).toBe("queries");
  });

  it("leaves an unrestricted delegation without permissions", async () => {
    const { sessionPublicKeyDer } = await setUpRedirectSession();

    const authResponse: AuthResponse = {
      kind: "authorize-client-success",
      delegations: [
        {
          delegation: new Delegation(
            sessionPublicKeyDer,
            BigInt(Date.now() + 3_600_000) * BigInt(1_000_000),
          ),
          signature: new Uint8Array(64) as unknown as Signature,
        },
      ],
      userPublicKey: new Uint8Array(32).fill(1),
      authnMethod: "passkey",
    };

    const result = await endRedirectSession(authOrigin, authResponse);

    if (result.kind !== "authorize-client-success") {
      throw new Error("expected success response");
    }
    expect(result.delegations[0].delegation.permissions).toBeUndefined();
  });
});
