import { expect } from "@playwright/test";
import { test } from "../../fixtures";
import { authorize } from "../../utils";

test.describe("Re-issue a delegation with ?prompt=none", () => {
  // The principal the priming sign-in produced. A silent re-issue has to
  // reproduce it exactly: the delegation is derived from the same account seed
  // either way, so a different principal would mean the wrong account.
  let expectedPrincipal: string;

  // A normal sign-in first. That ceremony is what leaves Internet Identity
  // holding a delegation for this app, so the round under test has something to
  // re-issue. Runs before the `authorizePage` fixture performs that round.
  test.beforeEach(async ({ page, identities, signInWithIdentity }) => {
    expectedPrincipal = await authorize(page, async (authPage) => {
      await signInWithIdentity(authPage, identities[0].identityNumber);
      await authPage
        .getByRole("button", { name: "Continue", exact: true })
        .click();
    });
  });

  // Read here rather than in the test body: `authorizedPrincipal` resolves only
  // once the flow has finished, and the flow is held open for the duration of
  // the body by the `authorizePage` fixture.
  test.afterEach(({ authorizedPrincipal }) => {
    expect(authorizedPrincipal?.toText()).toBe(expectedPrincipal);
  });

  test.describe("over the window transport", () => {
    test.use({
      authorizeConfig: { protocol: "icrc25", prompt: "none" },
    });

    test("re-issues without interaction", ({ authorizePage }) => {
      // Deliberately empty: under `prompt=none` there is no passkey, no virtual
      // authenticator and no Continue button, because Internet Identity answers
      // without rendering anything. Reaching the afterEach with the same
      // principal is the assertion.
      expect(authorizePage).toBeDefined();
    });
  });

  test.describe("over the redirect transport", () => {
    test.use({
      authorizeConfig: {
        protocol: "icrc25",
        prompt: "none",
        transport: "redirect",
      },
    });

    test("re-issues without interaction", ({ authorizePage }) => {
      // The delegation is stored against the effective origin, not the transport
      // that fetched it, so a sign-in over the window flow is re-issuable over
      // the redirect flow.
      expect(authorizePage).toBeDefined();
    });
  });
});
