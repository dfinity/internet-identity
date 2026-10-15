import { describe, expect, it } from "vitest";
import { decodeLinkToken, encodeLinkToken } from "./linkToken";

const token = {
  identityNumber: BigInt(10_772),
  expiresAtNs: BigInt("1790000000000000000"),
  signature: new Uint8Array([0, 1, 250, 255, 128, 64]),
};

describe("link token", () => {
  it("survives the trip through the URL", () => {
    expect(decodeLinkToken(`#${encodeLinkToken(token)}`)).toEqual(token);
  });

  it("reads a fragment written without its hash", () => {
    expect(decodeLinkToken(encodeLinkToken(token))).toEqual(token);
  });

  /** A launch with nothing to claim with says so rather than guessing: the app shows the
   *  install steps again instead of calling the canister with a token it invented. */
  it.each([["#"], [""], ["#anchor=1"], ["#anchor=1&exp=2"], ["#not-a-token"]])(
    "reads no token from %o",
    (fragment) => {
      expect(decodeLinkToken(fragment)).toBeUndefined();
    },
  );

  it("reads no token from a signature that is not base64url", () => {
    expect(decodeLinkToken("#anchor=1&exp=2&sig=!!!!")).toBeUndefined();
  });

  it("reads no token from an anchor that is not a number", () => {
    expect(decodeLinkToken("#anchor=x&exp=2&sig=AAE")).toBeUndefined();
  });

  /** The signature is bytes, and a URL carries text: `+` and `/` from plain base64 would
   *  not survive a fragment, so the encoding has to be the URL-safe alphabet. */
  it("encodes the signature without characters a URL would change", () => {
    const encoded = encodeLinkToken({
      ...token,
      signature: new Uint8Array([251, 255, 190, 255]),
    });

    expect(encoded).not.toContain("+");
    expect(encoded).not.toContain("%2F");
  });
});
