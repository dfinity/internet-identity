import { describe, expect, it } from "vitest";
import { notificationAppLinkMessage } from "./browser-key.store";

describe("notificationAppLinkMessage", () => {
  /**
   * The exact bytes a link token is signed over, for anchor 10000 expiring at 5000.
   *
   * Asserted identically in `browser_key.rs`. This browser signs the message and the
   * canister rebuilds it, so the two layouts have to agree byte for byte: pinning the
   * bytes on both sides turns a change to either into a failing test rather than tokens
   * the canister quietly refuses.
   */
  it("is the bytes the canister rebuilds", () => {
    const hex = [...notificationAppLinkMessage(BigInt(10_000), BigInt(5_000))]
      .map((byte) => byte.toString(16).padStart(2, "0"))
      .join("");

    expect(hex).toBe(
      "69692d6e6f74696669636174696f6e2d6170702d6c696e6b00000000000027100000000000001388",
    );
  });

  it("covers the anchor number, so a token cannot move to another identity", () => {
    expect(notificationAppLinkMessage(BigInt(1), BigInt(5_000))).not.toEqual(
      notificationAppLinkMessage(BigInt(2), BigInt(5_000)),
    );
  });

  it("covers the expiry, so a holder cannot give it longer to live", () => {
    expect(notificationAppLinkMessage(BigInt(1), BigInt(5_000))).not.toEqual(
      notificationAppLinkMessage(BigInt(1), BigInt(6_000)),
    );
  });
});
