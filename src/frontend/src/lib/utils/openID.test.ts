import { describe, it, expect } from "vitest";
import { issuerMatches } from "./openID";

describe("issuerMatches", () => {
  it("returns true for exact match without placeholders", () => {
    expect(
      issuerMatches(
        "https://accounts.google.com",
        "https://accounts.google.com",
      ),
    ).toBe(true);
  });

  it("returns false for non-matching exact strings", () => {
    expect(
      issuerMatches(
        "https://accounts.google.com",
        "https://accounts.example.com",
      ),
    ).toBe(false);
  });

  it("matches a single placeholder as one path segment", () => {
    expect(
      issuerMatches(
        "https://login.microsoftonline.com/{tid}/v2.0",
        "https://login.microsoftonline.com/4a435c5e-6451-4c1a-a81f-ab9666b6de8f/v2.0",
      ),
    ).toBe(true);
  });

  it("does not allow placeholder to match across path segments", () => {
    expect(
      issuerMatches(
        "https://login.microsoftonline.com/{tid}/v2.0",
        "https://login.microsoftonline.com/foo/bar/v2.0",
      ),
    ).toBe(false);
  });

  it("matches multiple placeholders each to a single segment", () => {
    expect(
      issuerMatches(
        "https://example.com/{a}/{b}/end",
        "https://example.com/one/two/end",
      ),
    ).toBe(true);
    expect(
      issuerMatches(
        "https://example.com/{a}/{b}/end",
        "https://example.com/one/two/extra/end",
      ),
    ).toBe(false);
  });

  it("treats unmatched braces literally (no regex)", () => {
    // Only opening brace, no closing brace -> literal comparison
    const pattern = "https://example.com/{tid";
    expect(issuerMatches(pattern, pattern)).toBe(true);
    expect(issuerMatches(pattern, "https://example.com/anything"))
      .toBe(false);
  });

  it("escapes regex special characters in literals", () => {
    expect(
      issuerMatches(
        "https://example.com/v2.0",
        "https://example.com/v2.0",
      ),
    ).toBe(true);
    expect(
      issuerMatches(
        "https://example.com/v2.0",
        "https://example.com/v20",
      ),
    ).toBe(false);

    // Placeholder next to a dot
    expect(
      issuerMatches(
        "https://example.com/v{ver}.0",
        "https://example.com/v123.0",
      ),
    ).toBe(true);
    expect(
      issuerMatches(
        "https://example.com/v{ver}.0",
        "https://example.com/v/123.0",
      ),
    ).toBe(false);
  });
});
