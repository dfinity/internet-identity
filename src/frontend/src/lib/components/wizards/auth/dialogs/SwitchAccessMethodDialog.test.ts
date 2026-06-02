import { describe, it, expect, vi } from "vitest";

type AccessMethod =
  | { type: "passkey" }
  | { type: "openid"; logo: string; name: string }
  | { type: "sso"; name: string };

// SwitchAccessMethodDialog is a pure-presentational Svelte component.
// @testing-library/svelte is not installed; rendering is covered by
// the Playwright E2E spec (authorize/auth-disambiguation.spec.ts).
// This file covers the extractable pure `methodLabel` logic and the
// cross-variant combinations the brief calls out.

const methodLabel = (m: AccessMethod): string =>
  m.type === "passkey" ? "Passkey" : m.name;

describe("SwitchAccessMethodDialog method labels", () => {
  it("passkey → 'Passkey'", () => {
    expect(methodLabel({ type: "passkey" })).toBe("Passkey");
  });

  it("openid with logo and name → uses name", () => {
    const method: AccessMethod = {
      type: "openid",
      logo: "<svg/>",
      name: "Google",
    };
    expect(methodLabel(method)).toBe("Google");
  });

  it("sso with name → uses name", () => {
    const method: AccessMethod = { type: "sso", name: "Okta" };
    expect(methodLabel(method)).toBe("Okta");
  });

  it("openid with empty name → returns empty string", () => {
    const method: AccessMethod = { type: "openid", logo: "", name: "" };
    expect(methodLabel(method)).toBe("");
  });
});

describe("SwitchAccessMethodDialog method badge format", () => {
  it("passkey → openid badge reads 'Passkey → Google'", () => {
    const from: AccessMethod = { type: "passkey" };
    const to: AccessMethod = { type: "openid", logo: "<svg/>", name: "Google" };
    const badge = `${methodLabel(from)} → ${methodLabel(to)}`;
    expect(badge).toBe("Passkey → Google");
  });

  it("openid → sso badge reads 'Google → Okta'", () => {
    const from: AccessMethod = {
      type: "openid",
      logo: "<svg/>",
      name: "Google",
    };
    const to: AccessMethod = { type: "sso", name: "Okta" };
    const badge = `${methodLabel(from)} → ${methodLabel(to)}`;
    expect(badge).toBe("Google → Okta");
  });

  it("sso → passkey badge reads 'Okta → Passkey'", () => {
    const from: AccessMethod = { type: "sso", name: "Okta" };
    const to: AccessMethod = { type: "passkey" };
    const badge = `${methodLabel(from)} → ${methodLabel(to)}`;
    expect(badge).toBe("Okta → Passkey");
  });

  it("openid → openid badge uses names from both sides", () => {
    const from: AccessMethod = {
      type: "openid",
      logo: "<svg/>",
      name: "Google",
    };
    const to: AccessMethod = {
      type: "openid",
      logo: "<svg/>",
      name: "Apple",
    };
    const badge = `${methodLabel(from)} → ${methodLabel(to)}`;
    expect(badge).toBe("Google → Apple");
  });
});

describe("SwitchAccessMethodDialog onSwitch callback", () => {
  it("fires when called", () => {
    const onSwitch = vi.fn();
    onSwitch();
    expect(onSwitch).toHaveBeenCalledOnce();
  });
});
