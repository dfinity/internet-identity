import { describe, expect, it } from "vitest";
import { isNotifiableOrigin } from "./notifiableOrigin";

describe("isNotifiableOrigin", () => {
  it("accepts a bare https origin, with or without a port", () => {
    expect(isNotifiableOrigin("https://app.example")).toBe(true);
    expect(isNotifiableOrigin("https://app.example:8443")).toBe(true);
  });

  /// Each is a spelling the canister refuses, so asking for it would spend a browser
  /// permission prompt on a grant that cannot land.
  it("refuses anything the canister will not key consent by", () => {
    for (const origin of [
      "http://app.example",
      "https://app.example/",
      "https://app.example/path",
      "https://app.example?q=1",
      "https://app.example#frag",
      "https://app.example:443",
      "https://user@app.example",
      "chrome-extension://abcdef",
      "app.example",
      "",
    ]) {
      expect(isNotifiableOrigin(origin), origin).toBe(false);
    }
  });
});
