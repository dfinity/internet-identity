import { expect } from "@playwright/test";
import { test } from "../../fixtures";
import { authorize, addVirtualAuthenticator, II_URL } from "../../utils";
import { DEFAULT_OPENID_PORT } from "../../fixtures/openid";

const DEFAULT_USER_NAME = "John Doe";

// AuthWizardView heading and subtitle (new-user path at /authorize)
test.describe("AuthWizardView heading and subtitle", () => {
  test("new user sees 'Sign in to Internet Identity' heading and dapp subtitle", async ({
    page,
  }) => {
    await authorize(page, async (authPage) => {
      await expect(
        authPage.getByRole("heading", {
          name: "Sign in to Internet Identity",
        }),
      ).toBeVisible();
      await expect(authPage.getByText("to continue to")).toBeVisible();

      await addVirtualAuthenticator(authPage);
      await authPage
        .getByRole("button", { name: "Sign up", exact: true })
        .click();
      await authPage
        .getByRole("button", { name: "Sign up with passkey" })
        .click();
      await authPage.getByLabel("Identity name").fill(DEFAULT_USER_NAME);
      await authPage.getByRole("button", { name: "Create identity" }).click();
      await authPage
        .getByRole("button", { name: "Continue", exact: true })
        .click();
    });
  });

  test("new user can toggle to sign-up modal from the picker footer 'Sign up' CTA", async ({
    page,
  }) => {
    await authorize(page, async (authPage) => {
      await expect(
        authPage.getByRole("button", { name: "Sign up", exact: true }),
      ).toBeVisible();

      await authPage
        .getByRole("button", { name: "Sign up", exact: true })
        .click();
      await expect(authPage.getByRole("dialog")).toBeVisible();

      // Sign-up dialog must NOT show the "Already have an identity?" footer
      await expect(
        authPage.getByText("Already have an identity?"),
      ).toBeHidden();

      await authPage.getByRole("button", { name: "Close" }).click();
      await expect(authPage.getByRole("dialog")).toBeHidden();
      await expect(
        authPage.getByRole("heading", {
          name: "Sign in to Internet Identity",
        }),
      ).toBeVisible();

      await addVirtualAuthenticator(authPage);
      await authPage
        .getByRole("button", { name: "Sign up", exact: true })
        .click();
      await authPage
        .getByRole("button", { name: "Sign up with passkey" })
        .click();
      await authPage.getByLabel("Identity name").fill(DEFAULT_USER_NAME);
      await authPage.getByRole("button", { name: "Create identity" }).click();
      await authPage
        .getByRole("button", { name: "Continue", exact: true })
        .click();
    });
  });
});

test.describe("ContinueView heading and subtitle — returning user", () => {
  test("returning user sees 'Continue to <dapp>' heading and 'with your Internet Identity' subtitle", async ({
    page,
    identities,
    addAuthenticatorForIdentity,
    signInWithIdentity,
  }) => {
    await page.goto(II_URL);
    await signInWithIdentity(page, identities[0].identityNumber);
    await page.waitForURL(II_URL + "/manage");

    await authorize(page, async (authPage) => {
      await expect(
        authPage.getByRole("heading", { name: /^Continue to / }),
      ).toBeVisible();
      await expect(
        authPage.getByText("with your Internet Identity"),
      ).toBeVisible();
      await addAuthenticatorForIdentity(authPage, identities[0].identityNumber);
      await authPage
        .getByRole("button", { name: "Continue", exact: true })
        .click();
    });
  });
});

// IdentityNotConnectedDialog — OIDC sign-in for a user without an II
test.describe("IdentityNotConnectedDialog at /authorize", () => {
  test.use({
    openIdConfig: {
      defaultPort: DEFAULT_OPENID_PORT,
      createUsers: [
        {
          claims: {
            name: DEFAULT_USER_NAME,
            email: "john.doe@example.com",
          },
        },
      ],
    },
  });

  test("OIDC sign-in for unknown user shows IdentityNotConnected dialog; dismiss returns to picker", async ({
    page,
    signInWithOpenId,
    openIdUsers,
  }) => {
    await authorize(page, async (authPage) => {
      const popupPromise = authPage.context().waitForEvent("page");
      await authPage
        .getByRole("button", { name: openIdUsers[0].issuer.name })
        .click();
      const popup = await popupPromise;
      const closePromise = popup.waitForEvent("close", { timeout: 15_000 });
      await signInWithOpenId(popup, openIdUsers[0].id);
      await closePromise;

      await expect(
        authPage.getByRole("heading", { name: "Create your Identity" }),
      ).toBeVisible();
      await expect(authPage.getByText("Not connected yet")).toBeVisible();

      await authPage.getByRole("button", { name: "Close" }).click();
      await expect(
        authPage.getByRole("heading", { name: "Create your Identity" }),
      ).toBeHidden();
      await expect(
        authPage.getByRole("heading", {
          name: "Sign in to Internet Identity",
        }),
      ).toBeVisible();

      await addVirtualAuthenticator(authPage);
      await authPage
        .getByRole("button", { name: "Sign up", exact: true })
        .click();
      await authPage
        .getByRole("button", { name: "Sign up with passkey" })
        .click();
      await authPage.getByLabel("Identity name").fill(DEFAULT_USER_NAME);
      await authPage.getByRole("button", { name: "Create identity" }).click();
      await authPage
        .getByRole("button", { name: "Continue", exact: true })
        .click();
    });
  });

  test("clicking Sign up inside IdentityNotConnectedDialog completes registration", async ({
    page,
    signInWithOpenId,
    openIdUsers,
  }) => {
    await authorize(page, async (authPage) => {
      const popupPromise = authPage.context().waitForEvent("page");
      await authPage
        .getByRole("button", { name: openIdUsers[0].issuer.name })
        .click();
      const popup = await popupPromise;
      const closePromise = popup.waitForEvent("close", { timeout: 15_000 });
      await signInWithOpenId(popup, openIdUsers[0].id);
      await closePromise;

      await expect(
        authPage.getByRole("heading", { name: "Create your Identity" }),
      ).toBeVisible();
      await authPage
        .getByRole("dialog")
        .getByRole("button", { name: "Sign up", exact: true })
        .click();

      await authPage
        .getByRole("button", { name: "Continue", exact: true })
        .click();
    });
  });
});

// OIDC sign-in for a returning user at /authorize — no disambiguation dialog
test.describe("OIDC sign-in for existing II at /authorize", () => {
  const name = DEFAULT_USER_NAME;

  test.use({
    openIdConfig: {
      defaultPort: DEFAULT_OPENID_PORT,
      createUsers: [{ claims: { name } }],
    },
  });

  test("OIDC sign-in with known identity proceeds without disambiguation dialog", async ({
    page,
    managePage,
    signInWithOpenId,
    openIdUsers,
  }) => {
    // Register the user first
    await page.goto(II_URL);
    const signUpPopupPromise = page.context().waitForEvent("page");
    await page
      .getByRole("button", { name: openIdUsers[0].issuer.name })
      .click();
    const signUpPopup = await signUpPopupPromise;
    const signUpClosePromise = signUpPopup.waitForEvent("close", {
      timeout: 15_000,
    });
    await signInWithOpenId(signUpPopup, openIdUsers[0].id);
    await signUpClosePromise;
    // Fresh OIDC user on the homepage surfaces IdentityNotConnectedDialog —
    // confirm sign-up to land on /manage.
    await page
      .getByRole("dialog")
      .getByRole("button", { name: "Sign up" })
      .click();
    await page.waitForURL(II_URL + "/manage");
    await managePage.signOut((c) => c.keepIdentity());
    await page.context().clearCookies();

    // Sign in via /authorize — known (iss, sub) goes straight to ContinueView.
    // keepIdentity() left the last-used entry in localStorage, so the picker
    // is skipped. ContinueView still calls authLastUsedFlow.authenticate on
    // Continue click, which opens an OIDC popup (mediation: "optional").
    await authorize(page, async (authPage) => {
      await expect(
        authPage.getByRole("heading", { name: /^Continue to/ }),
      ).toBeVisible();
      await expect(
        authPage.getByRole("heading", { name: "Create your Identity" }),
      ).toBeHidden();
      await expect(
        authPage.getByRole("heading", { name: "Already connected" }),
      ).toBeHidden();

      const signInPopupPromise = authPage.context().waitForEvent("page");
      await authPage
        .getByRole("button", { name: "Continue", exact: true })
        .click();
      const signInPopup = await signInPopupPromise;
      const signInClosePromise = signInPopup.waitForEvent("close", {
        timeout: 15_000,
      });
      await signInWithOpenId(signInPopup, openIdUsers[0].id);
      await signInClosePromise;
    });
  });
});

// IdentityAlreadyLinkedDialog — OIDC sign-up where provider is already linked
test.describe("IdentityAlreadyLinkedDialog at /authorize (sign-up path)", () => {
  const name = DEFAULT_USER_NAME;

  test.use({
    openIdConfig: {
      defaultPort: DEFAULT_OPENID_PORT,
      createUsers: [
        {
          claims: { name, email: "john.doe@example.com" },
        },
      ],
    },
  });

  test("OIDC sign-up with already-linked provider shows IdentityAlreadyLinked dialog; Sign in proceeds", async ({
    page,
    signInWithOpenId,
    openIdUsers,
    managePage,
  }) => {
    await page.goto(II_URL);
    const signUpPopupPromise = page.context().waitForEvent("page");
    await page
      .getByRole("button", { name: openIdUsers[0].issuer.name })
      .click();
    const signUpPopup = await signUpPopupPromise;
    const signUpClosePromise = signUpPopup.waitForEvent("close", {
      timeout: 15_000,
    });
    await signInWithOpenId(signUpPopup, openIdUsers[0].id);
    await signUpClosePromise;
    // Fresh OIDC user on the homepage surfaces IdentityNotConnectedDialog
    // — confirm sign-up to land on /manage.
    await page
      .getByRole("dialog")
      .getByRole("button", { name: "Sign up" })
      .click();
    await page.waitForURL(II_URL + "/manage");
    await managePage.signOut((c) => c.removeFromDevice());

    await authorize(page, async (authPage) => {
      await authPage
        .getByRole("button", { name: "Sign up", exact: true })
        .click();
      await expect(authPage.getByRole("dialog")).toBeVisible();

      await authPage.context().clearCookies();
      const popupPromise = authPage.context().waitForEvent("page");
      await authPage
        .getByRole("dialog")
        .getByRole("button", { name: openIdUsers[0].issuer.name })
        .click();
      const popup = await popupPromise;
      const closePromise = popup.waitForEvent("close", { timeout: 15_000 });
      await signInWithOpenId(popup, openIdUsers[0].id);
      await closePromise;

      await expect(
        authPage.getByRole("heading", { name: "Already connected" }),
      ).toBeVisible();
      await expect(
        authPage.getByText("Connected", { exact: true }),
      ).toBeVisible();

      await authPage
        .getByRole("button", { name: "Sign in", exact: true })
        .click();
      await authPage
        .getByRole("button", { name: "Continue", exact: true })
        .click();
    });
  });

  test("dismissing IdentityAlreadyLinkedDialog stays in sign-up modal; no auto-sign-in", async ({
    page,
    signInWithOpenId,
    openIdUsers,
    managePage,
  }) => {
    await page.goto(II_URL);
    const signUpPopupPromise = page.context().waitForEvent("page");
    await page
      .getByRole("button", { name: openIdUsers[0].issuer.name })
      .click();
    const signUpPopup = await signUpPopupPromise;
    const signUpClosePromise = signUpPopup.waitForEvent("close", {
      timeout: 15_000,
    });
    await signInWithOpenId(signUpPopup, openIdUsers[0].id);
    await signUpClosePromise;
    // Fresh OIDC user on the homepage surfaces IdentityNotConnectedDialog
    // — confirm sign-up to land on /manage.
    await page
      .getByRole("dialog")
      .getByRole("button", { name: "Sign up" })
      .click();
    await page.waitForURL(II_URL + "/manage");
    await managePage.signOut((c) => c.removeFromDevice());

    await authorize(page, async (authPage) => {
      await authPage
        .getByRole("button", { name: "Sign up", exact: true })
        .click();

      await authPage.context().clearCookies();
      const popupPromise = authPage.context().waitForEvent("page");
      await authPage
        .getByRole("dialog")
        .getByRole("button", { name: openIdUsers[0].issuer.name })
        .click();
      const popup = await popupPromise;
      const closePromise = popup.waitForEvent("close", { timeout: 15_000 });
      await signInWithOpenId(popup, openIdUsers[0].id);
      await closePromise;

      await expect(
        authPage.getByRole("heading", { name: "Already connected" }),
      ).toBeVisible();

      // Dismiss the AlreadyLinked dialog via the wizard's Close button —
      // the stacked dialog gets `data-testid="auth-wizard-dialog"` so the
      // locator targets the inner dialog unambiguously.
      await authPage
        .getByTestId("auth-wizard-dialog")
        .getByRole("button", { name: "Close" })
        .click();
      await expect(
        authPage.getByRole("heading", { name: "Already connected" }),
      ).toBeHidden();
      // The sign-up modal (parent dialog) is still visible; user can
      // proceed with passkey sign-up instead of auto-signing-in.
      await expect(
        authPage.getByRole("button", { name: "Sign up with passkey" }),
      ).toBeVisible();

      await addVirtualAuthenticator(authPage);
      await authPage
        .getByRole("button", { name: "Sign up with passkey" })
        .click();
      await authPage.getByLabel("Identity name").fill("New User");
      await authPage.getByRole("button", { name: "Create identity" }).click();
      await authPage.getByRole("button", { name: "Close" }).click();
      await authPage
        .getByRole("button", { name: "Continue", exact: true })
        .click();
    });
  });
});
