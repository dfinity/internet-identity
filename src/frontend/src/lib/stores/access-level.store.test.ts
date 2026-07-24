import { describe, it, expect, beforeEach, vi } from "vitest";
import { accessLevelStore } from "./access-level.store";

vi.mock("$app/environment", () => ({ browser: true }));

const anchorA = BigInt("111");
const anchorB = BigInt("222");

describe("accessLevelStore", () => {
  beforeEach(() => {
    localStorage.clear();
    accessLevelStore.reset();
  });

  it("returns undefined for a (flow, anchor) with no stored choice (first-time)", () => {
    expect(accessLevelStore.getPreference("continue", anchorA)).toBeUndefined();
    expect(accessLevelStore.getPreference("cli", anchorA)).toBeUndefined();
    expect(accessLevelStore.getPreference("mcp", anchorA)).toBeUndefined();
  });

  it("round-trips a stored preference", () => {
    accessLevelStore.setPreference("continue", anchorA, "read-only");
    expect(accessLevelStore.getPreference("continue", anchorA)).toBe(
      "read-only",
    );
    accessLevelStore.setPreference("continue", anchorA, "full-access");
    expect(accessLevelStore.getPreference("continue", anchorA)).toBe(
      "full-access",
    );
  });

  it("keeps preferences independent across flows for the same anchor", () => {
    accessLevelStore.setPreference("continue", anchorA, "full-access");
    accessLevelStore.setPreference("cli", anchorA, "read-only");
    expect(accessLevelStore.getPreference("continue", anchorA)).toBe(
      "full-access",
    );
    expect(accessLevelStore.getPreference("cli", anchorA)).toBe("read-only");
    expect(accessLevelStore.getPreference("mcp", anchorA)).toBeUndefined();
  });

  it("keeps preferences independent across anchors for the same flow (shared-device safety)", () => {
    accessLevelStore.setPreference("mcp", anchorA, "read-only");
    expect(accessLevelStore.getPreference("mcp", anchorB)).toBeUndefined();
    accessLevelStore.setPreference("mcp", anchorB, "full-access");
    expect(accessLevelStore.getPreference("mcp", anchorA)).toBe("read-only");
    expect(accessLevelStore.getPreference("mcp", anchorB)).toBe("full-access");
  });

  it("persists across reads via localStorage", () => {
    accessLevelStore.setPreference("mcp", anchorA, "read-only");
    const raw = localStorage.getItem("ii-access-level");
    expect(raw).not.toBeNull();
    const parsed = JSON.parse(raw!);
    // Stored shape: { data: { <flow>: { <anchor>: level } }, version: 1 }
    expect(parsed.data.mcp[anchorA.toString()]).toBe("read-only");
  });
});
