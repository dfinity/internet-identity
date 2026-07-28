import { describe, it, expect } from "vitest";
import type { McpConfig } from "$lib/utils/mcpConfig";
import { trustedUrl } from "./utils";

const OFFICIAL = "https://official-mcp.id.ai/mcp";
const CUSTOM = "https://mcp.acme.com/mcp";

/** A stored config. `undefined` stands for an identity that never wrote one. */
const cfg = (enabled: boolean, url: string | undefined): McpConfig => ({
  enabled,
  url,
});

describe("trustedUrl", () => {
  it("falls back to the official connector when no custom URL is set", () => {
    expect(trustedUrl(cfg(true, undefined), OFFICIAL)).toBe(OFFICIAL);
  });

  it("prefers the custom connector over the official one", () => {
    expect(trustedUrl(cfg(true, CUSTOM), OFFICIAL)).toBe(CUSTOM);
  });

  it("trusts nothing while the feature is disabled", () => {
    expect(trustedUrl(cfg(false, undefined), OFFICIAL)).toBeUndefined();
    expect(trustedUrl(cfg(false, CUSTOM), OFFICIAL)).toBeUndefined();
  });

  it("trusts nothing when enabled with no custom and no official connector", () => {
    expect(trustedUrl(cfg(true, undefined), undefined)).toBeUndefined();
  });
});
