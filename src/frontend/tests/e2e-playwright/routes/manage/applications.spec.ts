import type { BrowserContext, Page } from "@playwright/test";
import { expect } from "@playwright/test";
import { test } from "../../fixtures";
import { armPushNotifications } from "../../fixtures/notifications";
import type { TestApp } from "../../fixtures/testApp";
import { II_URL } from "../../utils";
import { continueAs } from "../authorize/app-sessions/helpers";

/**
 * The Applications page lists the apps the identity decided a permission for, today
 * whether they may notify, and nothing else. From it the user switches that permission
 * or removes the app, which signs it out.
 *
 * Every scenario signs in to the test app and answers its notification request first,
 * through the real consent screen, so what the page shows is what the canister holds.
 * The browser's push APIs are stubbed, as in the consent scenarios.
 */
test.describe("Applications page", () => {
  test.use({ authorizeConfig: { protocol: "icrc25" } });

  const signInToTestApp = async (
    testApp: TestApp,
    authenticate: (authPage: Page) => Promise<void>,
  ) => {
    await testApp.open();
    await testApp.signIn(authenticate);
    await testApp.waitUntilSignedIn();
  };

  const allowNotifications = async (
    testApp: TestApp,
    authenticate: (authPage: Page) => Promise<void>,
  ) => {
    await testApp.askToNotify(async (authPage: Page) => {
      await authenticate(authPage);
      await authPage
        .getByRole("button", { name: "Allow", exact: true })
        .click();
    });
    await testApp.expectMayNotify();
  };

  const openApplications = async (
    context: BrowserContext,
    identityNumber: bigint,
    signInWithIdentity: (page: Page, identityNumber: bigint) => Promise<void>,
  ): Promise<Page> => {
    const page = await context.newPage();
    await page.goto(`${II_URL}/manage/applications`);
    await signInWithIdentity(page, identityNumber);
    await expect(
      page.getByRole("heading", { name: "Applications", exact: true }),
    ).toBeVisible();
    return page;
  };

  /**
   * The page as the canister holds it, in a window of its own: the manage pages keep their
   * sign-in in memory only, so a reload lands on the sign-in screen instead.
   */
  let reopen: (page: Page) => Promise<Page>;
  test.beforeEach(({ context, identities, signInWithIdentity }) => {
    reopen = async (page: Page) => {
      await page.close();
      return openApplications(
        context,
        identities[0].identityNumber,
        signInWithIdentity,
      );
    };
  });

  /** One card per listed app, each with its settings behind this button. */
  const manageButtons = (page: Page) =>
    page.getByRole("button", { name: /^Manage / });

  const emptyList = (page: Page) =>
    page.getByText("No apps yet", { exact: true });

  test("an app the user only signed in to is not listed", async ({
    context,
    testApp,
    identities,
    signInWithIdentity,
  }) => {
    await armPushNotifications(context);
    const authenticate = continueAs(
      identities[0].identityNumber,
      signInWithIdentity,
    );
    await signInToTestApp(testApp, authenticate);

    const applications = await openApplications(
      context,
      identities[0].identityNumber,
      signInWithIdentity,
    );

    await expect(emptyList(applications)).toBeVisible();
    await expect(manageButtons(applications)).toHaveCount(0);
  });

  test("an allowed app is listed, and stays listed once switched off", async ({
    context,
    testApp,
    identities,
    signInWithIdentity,
  }) => {
    await armPushNotifications(context);
    const authenticate = continueAs(
      identities[0].identityNumber,
      signInWithIdentity,
    );
    await signInToTestApp(testApp, authenticate);
    await allowNotifications(testApp, authenticate);

    const applications = await openApplications(
      context,
      identities[0].identityNumber,
      signInWithIdentity,
    );
    await expect(manageButtons(applications)).toHaveCount(1);
    await expect(applications.getByText("None yet")).toBeVisible();

    await manageButtons(applications).click();
    const dialog = applications.getByRole("dialog");
    const notifications = dialog.getByRole("switch", {
      name: "Notifications",
    });
    await expect(notifications).toBeChecked();
    await notifications.click();
    await expect(notifications).not.toBeChecked();
    await applications.keyboard.press("Escape");
    await expect(applications.getByText("Not allowed")).toBeVisible();

    // What the canister holds, not what the page wrote ahead of the call.
    const reopened = await reopen(applications);
    await expect(manageButtons(reopened)).toHaveCount(1);
    await expect(reopened.getByText("Not allowed")).toBeVisible();

    await manageButtons(reopened).click();
    const switchAgain = reopened
      .getByRole("dialog")
      .getByRole("switch", { name: "Notifications" });
    await switchAgain.click();
    await expect(switchAgain).toBeChecked();
    await reopened.keyboard.press("Escape");

    const reopenedAgain = await reopen(reopened);
    await expect(reopenedAgain.getByText("None yet")).toBeVisible();
  });

  test("removing an app takes it off the list and signs it out", async ({
    context,
    testApp,
    identities,
    signInWithIdentity,
  }) => {
    await armPushNotifications(context);
    const authenticate = continueAs(
      identities[0].identityNumber,
      signInWithIdentity,
    );
    await signInToTestApp(testApp, authenticate);
    await allowNotifications(testApp, authenticate);

    const applications = await openApplications(
      context,
      identities[0].identityNumber,
      signInWithIdentity,
    );
    await manageButtons(applications).click();
    await applications
      .getByRole("dialog")
      .getByRole("button", { name: "Remove", exact: true })
      .click();
    await applications
      .getByRole("dialog")
      .getByRole("button", { name: "Remove app", exact: true })
      .click();

    await expect(applications.getByText(/has been removed\.$/)).toBeVisible();
    await expect(emptyList(applications)).toBeVisible();
    await expect(emptyList(await reopen(applications))).toBeVisible();

    // Ended for the app too, at its next refresh.
    await testApp.focus();
    await testApp.ageDelegation();
    await testApp.replaceDelegation();
    await testApp.expectSessionEnded();
  });

  test("apps selected on the list are removed together", async ({
    context,
    testApp,
    identities,
    signInWithIdentity,
  }) => {
    await armPushNotifications(context);
    const authenticate = continueAs(
      identities[0].identityNumber,
      signInWithIdentity,
    );
    await signInToTestApp(testApp, authenticate);
    await allowNotifications(testApp, authenticate);

    const applications = await openApplications(
      context,
      identities[0].identityNumber,
      signInWithIdentity,
    );
    await applications
      .getByRole("button", { name: "Remove apps", exact: true })
      .click();
    const toolbar = applications.getByRole("toolbar", { name: "Remove apps" });
    await expect(toolbar.getByText("Select apps to remove")).toBeVisible();
    await expect(
      toolbar.getByRole("button", { name: "Remove", exact: true }),
    ).toBeDisabled();

    await applications.getByRole("checkbox", { name: /^Select / }).click();
    await expect(toolbar.getByText("1 app selected")).toBeVisible();
    await toolbar.getByRole("button", { name: "Remove", exact: true }).click();
    await applications
      .getByRole("dialog")
      .getByRole("button", { name: "Remove app", exact: true })
      .click();

    await expect(emptyList(applications)).toBeVisible();
    await expect(toolbar).toBeHidden();
  });
});
