import { describe, expect, it } from "vitest";
import { isSameOrigin, originLabel } from "./urlUtils";

describe("urlUtils", () => {
  describe("isSameOrigin", () => {
    it("should return true for identical URLs", () => {
      expect(isSameOrigin("https://example.com", "https://example.com")).toBe(
        true,
      );
    });

    it("should return true for URLs with the same origin but different paths", () => {
      expect(
        isSameOrigin("https://example.com/path1", "https://example.com/path2"),
      ).toBe(true);
    });

    it("should return true for URLs with the same origin but different query parameters", () => {
      expect(
        isSameOrigin(
          "https://example.com?param1=value1",
          "https://example.com?param2=value2",
        ),
      ).toBe(true);
    });

    it("should return true for URLs with the same origin but different hash fragments", () => {
      expect(
        isSameOrigin(
          "https://example.com#section1",
          "https://example.com#section2",
        ),
      ).toBe(true);
    });

    it("should return false for URLs with different protocols", () => {
      expect(isSameOrigin("https://example.com", "http://example.com")).toBe(
        false,
      );
    });

    it("should return false for URLs with different hostnames", () => {
      expect(isSameOrigin("https://example.com", "https://example.org")).toBe(
        false,
      );
    });

    it("should return false for URLs with different ports", () => {
      expect(
        isSameOrigin("https://example.com:443", "https://example.com:8080"),
      ).toBe(false);
    });

    it("should return false for URLs with different subdomains", () => {
      expect(
        isSameOrigin("https://sub1.example.com", "https://sub2.example.com"),
      ).toBe(false);
    });

    it("should handle URLs with default ports correctly", () => {
      expect(
        isSameOrigin("https://example.com", "https://example.com:443"),
      ).toBe(true);
      expect(isSameOrigin("http://example.com", "http://example.com:80")).toBe(
        true,
      );
    });

    it("should handle invalid URLs by falling back to string comparison", () => {
      expect(isSameOrigin("invalid-url", "invalid-url")).toBe(true);
      expect(isSameOrigin("invalid-url", "another-invalid-url")).toBe(false);
    });
  });

  describe("originLabel", () => {
    it("should collapse ordinary https origins to their hostname", () => {
      expect(originLabel("https://example.com")).toBe("example.com");
      expect(originLabel("https://example.com:443")).toBe("example.com");
      expect(originLabel("https://sub.example.com/path")).toBe(
        "sub.example.com",
      );
    });

    it("should keep a non-default port, which distinguishes the origin", () => {
      // https://example.com and https://example.com:8443 are different
      // origins deriving different principals, so they must not share a label.
      expect(originLabel("https://example.com:8443")).toBe(
        "https://example.com:8443",
      );
      expect(originLabel("https://example.com:8443")).not.toBe(
        originLabel("https://example.com"),
      );
    });

    it("should keep a non-https scheme rather than hide it", () => {
      expect(originLabel("http://example.com")).toBe("http://example.com");
      expect(originLabel("http://localhost:5173")).toBe(
        "http://localhost:5173",
      );
    });

    it("should show unparseable values verbatim", () => {
      expect(originLabel("not-an-origin")).toBe("not-an-origin");
    });
  });
});
