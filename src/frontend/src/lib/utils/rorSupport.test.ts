import { userSupportsWebauthRoR } from "./rorSupport";

describe("rorSupport", () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  describe("userSupportsWebauthRoR", () => {
    it("should return true if the user agent supports Webauthn with Related Origin Requests and the credential.get function is not monkey patched", () => {
      vi.stubGlobal("navigator", {
        userAgent:
          "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/18.0 Safari/605.1.15",
        credentials: {
          get: {
            toString: () => "function get() { [native code] }",
          },
        },
      });
      expect(userSupportsWebauthRoR()).toBe(true);
    });

    it("should return false if the user agent does not support Webauthn with Related Origin Requests", () => {
      vi.stubGlobal("navigator", {
        userAgent:
          "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/17.0 Safari/605.1.15",
        credentials: {
          get: {
            toString: () => "function get() { [native code] }",
          },
        },
      });
      expect(userSupportsWebauthRoR()).toBe(false);
    });

    it("should return false if the credential.get function is monkey patched", () => {
      vi.stubGlobal("navigator", {
        userAgent:
          "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/18.0 Safari/605.1.15",
        credentials: {
          get: {
            toString: () => "function get() { [non-native code] }",
          },
        },
      });
      expect(userSupportsWebauthRoR()).toBe(false);
    });
  });
});
