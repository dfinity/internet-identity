import { test as base } from "./fixtures";
import { expect } from "@playwright/test";
import { generateMnemonic } from "$lib/utils/recoveryPhrase";
import { II_URL } from "./utils";

/**
 * Replace the last word with the first different word found,
 * compared to random shuffle, this guarantees an invalid checksum.
 *
 * @example changeLastWord(["abc", "abc", "xyz", "xyz"]) // ["abc", "abc", "xyz", "abc"]
 */
export const changeLastWord = (words: string[]) => {
  const invalidOrder = [...words];
  const checksumWord = invalidOrder[invalidOrder.length - 1];
  const differentWordIndex = invalidOrder.findIndex(
    (word) => word !== checksumWord,
  );
  invalidOrder[invalidOrder.length - 1] = invalidOrder[differentWordIndex];
  expect(invalidOrder).not.toEqual(words); // Smoke test
  return invalidOrder;
};

// Add words fixture to test so they can be passed around between blocks
const test = base.extend<{
  words: {
    current?: string[];
  };
}>({
  // Destructuring first argument is required in Playwright
  // eslint-disable-next-line no-empty-pattern
  words: ({}, use) => use({}),
});

test.describe("Recovery flow", () => {
  // Activate recovery phrase and goto recovery
  test.beforeEach(
    async ({ manageRecoveryPage, recoveryPage, identity, words }) => {
      await manageRecoveryPage.goto();
      await identity.signIn();
      await manageRecoveryPage.assertNotActivated();
      words.current = await manageRecoveryPage.activate(async (wizard) => {
        await wizard.acknowledge();
        const recoveryPhrase = await wizard.writeDown();
        await wizard.verifySelecting(recoveryPhrase);
        return recoveryPhrase;
      });
      await manageRecoveryPage.assertActivated();
      await recoveryPage.goto();
    },
  );

  test.describe("can be completed", () => {
    // Verify we've reached the dashboard and the page has loaded,
    // this indicates that the user has successfully authenticated.
    test.afterEach(async ({ page }) => {
      await page.waitForURL(II_URL + "/manage/access");
      await expect(
        page.getByRole("heading", { name: "Access methods" }),
      ).toBeVisible();
    });

    test("on first attempt", async ({ recoveryPage, words, identity }) => {
      await recoveryPage.start(async (wizard) => {
        await wizard.enterRecoveryPhrase(words.current!);
        await wizard.confirmFoundIdentity(identity.name);
      });
    });

    test("on retry after identity not found", async ({
      recoveryPage,
      words,
      identity,
    }) => {
      await recoveryPage.start(async (wizard) => {
        await wizard.enterRecoveryPhrase(generateMnemonic());
        await wizard.retryIdentityNotFound();
        await wizard.enterRecoveryPhrase(words.current!);
        await wizard.confirmFoundIdentity(identity.name);
      });
    });

    test("on retry after invalid phrase", async ({
      recoveryPage,
      words,
      identity,
    }) => {
      await recoveryPage.start(async (wizard) => {
        await wizard.enterRecoveryPhrase(changeLastWord(words.current!));
        await wizard.retryInvalid();
        await wizard.enterRecoveryPhrase(words.current!);
        await wizard.confirmFoundIdentity(identity.name);
      });
    });

    test("with a legacy identity", async ({
      recoveryPage,
      words,
      identity,
    }) => {
      // Remove name from identity
      const { actor, identityNumber } = await identity.createActor();
      await actor.identity_properties_replace(identityNumber, { name: [] });
      // Enter name during recovery
      await recoveryPage.start(async (wizard) => {
        await wizard.enterRecoveryPhrase(words.current!);
        await wizard.enterMissingName("Upgraded test user");
      });
    });
  });

  test("can be completed more than once", async ({
    page,
    recoveryPage,
    words,
    identity,
  }) => {
    // Recover with the same phrase twice to verify that
    // the phrase remains usable after it has been used.
    for (let i = 0; i < 2; i++) {
      await recoveryPage.goto();
      await recoveryPage.start(async (wizard) => {
        await wizard.enterRecoveryPhrase(words.current!);
        await wizard.confirmFoundIdentity(identity.name);
      });
      // Verify we've reached the dashboard and the page has loaded,
      // this indicates that the user has successfully authenticated.
      await page.waitForURL(II_URL + "/manage/access");
      await expect(
        page.getByRole("heading", { name: "Access methods" }),
      ).toBeVisible();
    }
  });

  test.describe("can be cancelled", () => {
    // Assert we're back on the login page and the recovery phrase is still valid
    test.afterEach(async ({ page, recoveryPage, words, identity }) => {
      await page.waitForURL(II_URL + "/login");
      await recoveryPage.goto();
      await recoveryPage.start(async (wizard) => {
        await wizard.enterRecoveryPhrase(words.current!);
        await wizard.confirmFoundIdentity(identity.name);
      });
      await page.waitForURL(II_URL + "/manage/access");
      await expect(
        page.getByRole("heading", { name: "Access methods" }),
      ).toBeVisible();
    });

    test("before starting", async ({ recoveryPage }) => {
      await recoveryPage.cancel();
    });

    test("before submission", async ({ recoveryPage }) => {
      await recoveryPage.start(async (wizard) => {
        await wizard.close();
      });
      await recoveryPage.cancel();
    });

    test("after found identity", async ({ recoveryPage, words }) => {
      await recoveryPage.start(async (wizard) => {
        await wizard.enterRecoveryPhrase(words.current!);
        await wizard.cancelFoundIdentity();
      });
      await recoveryPage.cancel();
    });

    test("after identity not found", async ({ recoveryPage }) => {
      await recoveryPage.start(async (wizard) => {
        await wizard.enterRecoveryPhrase(generateMnemonic());
        await wizard.cancelIdentityNotFound();
      });
      await recoveryPage.cancel();
    });

    test("after invalid phrase", async ({ recoveryPage, words }) => {
      await recoveryPage.start(async (wizard) => {
        await wizard.enterRecoveryPhrase(changeLastWord(words.current!));
        await wizard.cancelInvalid();
      });
      await recoveryPage.cancel();
    });

    test("after invalid phrase", async ({ recoveryPage, words }) => {
      await recoveryPage.start(async (wizard) => {
        await wizard.enterRecoveryPhrase(changeLastWord(words.current!));
        await wizard.cancelInvalid();
      });
      await recoveryPage.cancel();
    });

    test("before upgrading legacy identity", async ({
      recoveryPage,
      words,
      identity,
    }) => {
      // Remove name from identity
      const { actor, identityNumber } = await identity.createActor();
      await actor.identity_properties_replace(identityNumber, { name: [] });
      // Cancel instead of entering an identity name
      await recoveryPage.start(async (wizard) => {
        await wizard.enterRecoveryPhrase(words.current!);
        await wizard.cancelMissingName();
      });
      await recoveryPage.cancel();
      // Revert name removal from identity
      await actor.identity_properties_replace(identityNumber, {
        name: [identity.name],
      });
    });
  });
});
