import { expect } from "@playwright/test";
import { test } from "../../fixtures";
import { authorize, II_URL, TEST_APP_URL } from "../../utils";

test.describe("Manage your Internet Identity from authorize popover", () => {
  test("postMessage handoff — manage tab lands on /manage without sign-in dialog", async ({
    page,
    identities,
    addAuthenticatorForIdentity,
    signInWithIdentity,
  }) => {
    // Establish a last-used identity by completing an authorize flow first.
    await page.goto(TEST_APP_URL);
    await page.getByRole("textbox", { name: "Identity Provider" }).fill(II_URL);

    await authorize(page, async (authPage) => {
      await addAuthenticatorForIdentity(authPage, identities[0].identityNumber);
      await signInWithIdentity(authPage, identities[0].identityNumber);
      await authPage
        .getByRole("button", { name: "Continue", exact: true })
        .click();
    });

    // The test app page is back in focus after authorize() completes.
    // Now open a second II popup by clicking "Sign In" again. This popup will
    // have the last-used identity in local storage so the header avatar/popover
    // is rendered.
    const secondPopupPromise = page.context().waitForEvent("page");
    await page.getByRole("button", { name: "Sign In" }).click();
    const secondPopup = await secondPopupPromise;

    // Add authenticator (webauthn) to the second popup.
    await addAuthenticatorForIdentity(
      secondPopup,
      identities[0].identityNumber,
    );

    // Wait for the avatar button (signals welcome-back state with a cached
    // identity in local storage).
    const avatarBtn = secondPopup.getByRole("button", {
      name: "Switch identity",
    });
    await expect(avatarBtn).toBeVisible({ timeout: 10_000 });

    // Open the identity popover.
    await avatarBtn.click();

    // Click "Manage your Internet Identity" — signs in via the virtual
    // authenticator and surfaces the "You're signed in" confirmation. The
    // dialog's own button click then drives window.open with fresh
    // transient activation so the flow works on Safari too.
    await secondPopup
      .getByRole("button", { name: "Manage your Internet Identity" })
      .click();

    const managePagePromise = secondPopup.context().waitForEvent("page");
    await secondPopup.getByRole("button", { name: "Open manage" }).click();

    const managePage = await managePagePromise;
    await managePage.waitForURL("**/manage**", { timeout: 15_000 });

    // The manage page should show the welcome heading and NOT the sign-in
    // dialog — which would indicate the auth handoff succeeded.
    await expect(
      managePage.getByRole("heading", { name: "Welcome" }),
    ).toBeVisible({ timeout: 10_000 });
    await expect(
      managePage.getByRole("dialog", { name: "Add existing identity" }),
    ).toBeHidden();
  });

  // TODO: Simulate popup-block (window.open returns null) to exercise the
  // same-tab fallback path. Monkeypatching window.open via
  // page.addInitScript is straightforward, but the authorize layout is
  // loaded in a popup context itself, making reliable interception
  // non-trivial with this fixture setup. The fallback path is exercised by
  // the unit test in auth-handoff.test.ts and by the same-tab /manage
  // navigation guard already covered in the existing manage suite.
});
