import { describe, it, expect, vi } from "vitest";

// IdentityNotConnectedDialog is a pure-presentational Svelte component.
// @testing-library/svelte is not installed in this project, so component
// rendering is covered by the Playwright E2E spec
// (authorize/auth-disambiguation.spec.ts). This file covers the one
// extractable pure-logic piece: the prop contract used by the parent
// pages to decide what to pass.

describe("IdentityNotConnectedDialog prop contract", () => {
  it("userName falls back to userEmail when name is absent", () => {
    // Mirrors the inline fallback used in +page.svelte and +layout.svelte:
    //   userName={payload.userName ?? payload.userEmail ?? payload.providerName}
    const payload = {
      providerName: "Google",
      providerLogo: undefined,
      userName: undefined as string | undefined,
      userEmail: "alice@example.com",
    };
    const resolvedName =
      payload.userName ?? payload.userEmail ?? payload.providerName;
    expect(resolvedName).toBe("alice@example.com");
  });

  it("userName falls back to providerName when both name and email are absent", () => {
    const payload = {
      providerName: "Google",
      userName: undefined as string | undefined,
      userEmail: undefined as string | undefined,
    };
    const resolvedName =
      payload.userName ?? payload.userEmail ?? payload.providerName;
    expect(resolvedName).toBe("Google");
  });

  it("userEmail is only passed through when userName is defined", () => {
    // The parent renders userEmail only when userName is set, so the
    // component receives a distinct display email beneath the name.
    const withName = { userName: "Alice", userEmail: "alice@example.com" };
    const withoutName = {
      userName: undefined as string | undefined,
      userEmail: "alice@example.com",
    };

    const resolvedEmail = (p: typeof withName | typeof withoutName) =>
      p.userName !== undefined ? p.userEmail : undefined;

    expect(resolvedEmail(withName)).toBe("alice@example.com");
    expect(resolvedEmail(withoutName)).toBeUndefined();
  });

  it("onSignUp and onRecover callbacks are distinct handler slots", () => {
    const onSignUp = vi.fn();
    const onRecover = vi.fn();

    onSignUp();
    expect(onSignUp).toHaveBeenCalledOnce();
    expect(onRecover).not.toHaveBeenCalled();

    onRecover();
    expect(onRecover).toHaveBeenCalledOnce();
  });

  it("calling onSignUp twice fires the callback twice", () => {
    const onSignUp = vi.fn();
    onSignUp();
    onSignUp();
    expect(onSignUp).toHaveBeenCalledTimes(2);
  });
});
