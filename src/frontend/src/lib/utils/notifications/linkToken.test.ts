import { describe, expect, it } from "vitest";
import { LINK_KEY_BYTES } from "./linkKey";
import { decodeLinkToken, encodeLinkToken } from "./linkToken";

/** The whole byte range, so a value a URL would change is caught. */
const linkKey = Uint8Array.from(
  { length: LINK_KEY_BYTES },
  (_, index) => index * 2,
);

const token = {
  identityNumber: BigInt(10_772),
  linkKey,
};

describe("link token", () => {
  it("survives the trip through the URL", () => {
    expect(decodeLinkToken(`#${encodeLinkToken(token)}`)).toEqual({
      kind: "token",
      token,
    });
  });

  it("reads a fragment written without its hash", () => {
    expect(decodeLinkToken(encodeLinkToken(token))).toEqual({
      kind: "token",
      token,
    });
  });

  it.each([["#"], [""], ["#not-a-token"]])(
    "reads no token from %o",
    (fragment) => {
      expect(decodeLinkToken(fragment)).toEqual({ kind: "no-token" });
    },
  );

  it.each([["#anchor=1"], ["#key=AAE"]])(
    "reads a damaged token from %o",
    (fragment) => {
      expect(decodeLinkToken(fragment)).toEqual({
        kind: "unreadable",
        reason: "the token is missing a field",
      });
    },
  );

  it("reports a key that is not base64url", () => {
    expect(decodeLinkToken("#anchor=1&key=!!!!")).toMatchObject({
      kind: "unreadable",
    });
  });

  it("reports an anchor that is not a number", () => {
    const encoded = encodeLinkToken({ ...token, identityNumber: BigInt(1) });

    expect(
      decodeLinkToken(encoded.replace("anchor=1", "anchor=x")),
    ).toMatchObject({ kind: "unreadable" });
  });

  it("reports a key that is not the length a link key is", () => {
    const short = encodeLinkToken({
      ...token,
      linkKey: new Uint8Array([0, 1, 250]),
    });

    expect(decodeLinkToken(short)).toEqual({
      kind: "unreadable",
      reason: `the key is 3 bytes, not ${LINK_KEY_BYTES}`,
    });
  });

  it("encodes the key without characters a URL would change", () => {
    const encoded = encodeLinkToken({
      ...token,
      linkKey: new Uint8Array(LINK_KEY_BYTES).fill(251),
    });

    expect(encoded).not.toContain("+");
    expect(encoded).not.toContain("%2F");
  });
});
