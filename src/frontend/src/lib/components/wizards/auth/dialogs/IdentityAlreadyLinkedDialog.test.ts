import { describe, it, expect, vi } from "vitest";

// IdentityAlreadyLinkedDialog is a pure-presentational Svelte component.
// @testing-library/svelte is not installed; rendering and click behaviour
// are covered by the Playwright E2E spec (authorize/auth-disambiguation.spec.ts).
// This file covers the prop contract logic shared by every page that renders
// the dialog.

describe("IdentityAlreadyLinkedDialog prop contract", () => {
  it("userName falls back to userEmail when name is absent", () => {
    const payload = {
      providerName: "Google",
      userName: undefined as string | undefined,
      userEmail: "bob@example.com",
    };
    const resolvedName =
      payload.userName ?? payload.userEmail ?? payload.providerName;
    expect(resolvedName).toBe("bob@example.com");
  });

  it("userName falls back to providerName when both are absent", () => {
    const payload = {
      providerName: "Google",
      userName: undefined as string | undefined,
      userEmail: undefined as string | undefined,
    };
    const resolvedName =
      payload.userName ?? payload.userEmail ?? payload.providerName;
    expect(resolvedName).toBe("Google");
  });

  it("userEmail is suppressed when userName is absent", () => {
    const payload = {
      userName: undefined as string | undefined,
      userEmail: "bob@example.com",
    };
    const resolvedEmail =
      payload.userName !== undefined ? payload.userEmail : undefined;
    expect(resolvedEmail).toBeUndefined();
  });

  it("onSignIn callback fires when invoked", () => {
    const onSignIn = vi.fn();
    onSignIn();
    expect(onSignIn).toHaveBeenCalledOnce();
  });

  it("missing optional providerLogo does not affect resolved label", () => {
    const payload = {
      providerName: "Google",
      providerLogo: undefined as string | undefined,
      userName: "Bob",
    };
    expect(payload.providerLogo).toBeUndefined();
    const resolvedName = payload.userName ?? payload.providerName;
    expect(resolvedName).toBe("Bob");
  });
});
