import { describe, expect, it } from "vitest";
import { buildDiagnosticsBlob } from "./diagnostics";
import type { EmailChallengeDiagnostics } from "$lib/generated/internet_identity_types";

// 1_700_000_000 s since epoch → 2023-11-14T22:13:20Z. Stored in ns.
const CREATED_AT_NS = BigInt("1700000000000000000");

describe("buildDiagnosticsBlob", () => {
  it("includes the gateway message_id, reason code and DoH path", () => {
    const diag: EmailChallengeDiagnostics = {
      message_id: ["<gw-123@gateway.example>"],
      reason_code: "Failed:AddressMismatch",
      verification_path: { Doh: null },
      created_at: CREATED_AT_NS,
    };
    const blob = buildDiagnosticsBlob(diag);
    expect(blob).toContain("message-id: <gw-123@gateway.example>");
    expect(blob).toContain("reason:     Failed:AddressMismatch");
    expect(blob).toContain("path:       doh");
    expect(blob).toContain("time:       2023-11-14T");
  });

  it("renders an absent message_id and the DNSSEC path", () => {
    const diag: EmailChallengeDiagnostics = {
      message_id: [],
      reason_code: "Pending",
      verification_path: { Dnssec: null },
      created_at: CREATED_AT_NS,
    };
    const blob = buildDiagnosticsBlob(diag);
    expect(blob).toContain("message-id: —");
    expect(blob).toContain("path:       dnssec");
  });

  it("falls back to FE-only fields when there is no canister record", () => {
    const blob = buildDiagnosticsBlob(undefined);
    expect(blob).toContain("message-id: —");
    expect(blob).toContain("reason:     —");
    expect(blob).toContain("path:       —");
    expect(blob).toContain("time:       —");
  });
});
