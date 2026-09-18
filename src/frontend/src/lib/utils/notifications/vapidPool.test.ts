import { describe, expect, it } from "vitest";
import { fromBase64URL } from "$lib/utils/utils";
import {
  JWT_POOL_SIZE,
  generateVapidKeypair,
  signJwtPool,
  windowPayloadB64,
} from "./vapidPool";

const decode = (b64url: string): string =>
  new TextDecoder().decode(fromBase64URL(b64url));

describe("windowPayloadB64", () => {
  it("emits the agreed claims, keys ordered aud/exp/sub", () => {
    expect(
      decode(windowPayloadB64("https://relay.example", BigInt(0), 0)),
    ).toBe('{"aud":"https://relay.example","exp":86400,"sub":"https://id.ai"}');
  });

  it("sets exp to the window end in seconds", () => {
    // window 2 ends 3 * 24h after issue.
    const issuedAtNs =
      BigInt(1_000) * BigInt(24 * 60 * 60) * BigInt(1_000_000_000);
    const payload = decode(
      windowPayloadB64("https://relay.example", issuedAtNs, 2),
    );
    expect(payload).toContain(
      `"exp":${(BigInt(1_000) + BigInt(3)) * BigInt(24 * 60 * 60)}`,
    );
  });

  it("escapes the relay origin like JSON", () => {
    expect(decode(windowPayloadB64('https://a"b', BigInt(0), 0))).toContain(
      '"aud":"https://a\\"b"',
    );
  });
});

describe("generateVapidKeypair + signJwtPool", () => {
  it("produces a 65-byte public key and a full pool of 64-byte signatures", async () => {
    const { publicKeyRaw, privateKey } = await generateVapidKeypair();
    expect(publicKeyRaw).toHaveLength(65);
    expect(publicKeyRaw[0]).toBe(0x04); // uncompressed point

    const signatures = await signJwtPool(
      privateKey,
      "https://relay.example",
      BigInt(42),
    );
    expect(signatures).toHaveLength(JWT_POOL_SIZE);
    expect(signatures.every((sig) => sig.length === 64)).toBe(true);
  });
});
