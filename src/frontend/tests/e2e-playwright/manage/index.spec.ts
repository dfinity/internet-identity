import { test } from "../fixtures";
import { expect } from "@playwright/test";
import { II_URL } from "../utils";

test.describe("Manage", () => {
  test.describe("can be accessed", () => {
    test.describe("with a single last used identity", () => {
      test.beforeEach(async ({ identity, managePage }) => {
        await managePage.goto();
        await identity.signIn();
      });

      test("to see home", async ({ identity, managePage }) => {
        await managePage.navigation((navigation) =>
          navigation.assertHome(identity.name),
        );
      });

      test("to sign out", async ({ managePage, page }) => {
        await managePage.signOut();
        await expect(page).toHaveURL(II_URL);
      });

      test("to sign in with another identity", async ({
        altIdentity,
        managePage,
      }) => {
        await altIdentity.signIn();
        await managePage.navigation((navigation) =>
          navigation.assertHome(altIdentity.name),
        );
      });
    });

    test.describe("with multiple last used identities", () => {
      // Sign in to add both identities to identity switcher
      test.beforeEach(async ({ identity, altIdentity, managePage }) => {
        await managePage.goto();
        await altIdentity.signIn();
        await identity.signIn(); // We're signed in with default initially
      });

      test("to switch between identities", async ({
        identity,
        altIdentity,
        managePage,
      }) => {
        await managePage.switchToIdentity(altIdentity);
        await managePage.navigation((navigation) =>
          navigation.assertHome(altIdentity.name),
        );
        await managePage.switchToIdentity(identity);
        await managePage.navigation((navigation) =>
          navigation.assertHome(identity.name),
        );
      });

      test("to sign out", async ({ managePage, page }) => {
        await managePage.signOut();
        await expect(page).toHaveURL(II_URL);
      });
    });
  });

  test.describe("can be navigated", () => {
    test.beforeEach(async ({ identity, managePage }) => {
      await managePage.goto();
      await identity.signIn();
    });

    // Verify that we can navigate back to home afterward
    test.afterEach(async ({ identity, managePage }) => {
      await managePage.navigation(async (navigation) => {
        await navigation.home();
        await navigation.assertHome(identity.name);
      });
    });

    test("to access methods", async ({ managePage }) => {
      await managePage.navigation(async (navigation) => {
        await navigation.accessMethods();
        await navigation.assertAccessMethods();
      });
    });

    test("to recovery phrase", async ({ managePage }) => {
      await managePage.navigation(async (navigation) => {
        await navigation.recoveryPhrase();
        await navigation.assertRecoveryPhrase();
      });
    });
  });
});
