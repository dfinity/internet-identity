import { describe, expect, it } from "vitest";
import { isSameOrigin, toRelative } from "./urlUtils";

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

  describe("toRelative", () => {
    describe("absolute URLs", () => {
      it("returns path, query, and hash for absolute URL", () => {
        expect(
          toRelative("https://example.com/hello/world?abc=1&xyz=2#hello"),
        ).toBe("/hello/world?abc=1&xyz=2#hello");
      });

      it("returns '/' for absolute URL without path (root)", () => {
        expect(toRelative("https://example.com")).toBe("/");
      });

      it("preserves query and hash for origin-only absolute URL", () => {
        expect(toRelative("https://example.com?x=1#y")).toBe("/?x=1#y");
      });

      it("resolves protocol-relative URLs to path only", () => {
        expect(toRelative("//example.com/path/to")).toBe("/path/to");
      });
    });

    describe("relative inputs", () => {
      it("returns input unchanged when already relative with leading slash", () => {
        expect(toRelative("/hello/world?abc=1&xyz=2#hello")).toBe(
          "/hello/world?abc=1&xyz=2#hello",
        );
      });

      it("normalizes relative path without leading slash", () => {
        expect(toRelative("hello/world")).toBe("/hello/world");
      });

      it("anchors hash-only input to root", () => {
        expect(toRelative("#section")).toBe("/#section");
      });
    });

    describe("invalid inputs", () => {
      it("returns null for invalid URL strings", () => {
        // Missing hostname with explicit protocol is invalid even with a base
        expect(toRelative("http://")).toBeNull();
      });
    });
  });
});
