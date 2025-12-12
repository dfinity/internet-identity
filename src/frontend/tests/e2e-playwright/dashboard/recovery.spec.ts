import { test as base } from "../fixtures";
import { expect } from "@playwright/test";
import {
  fromMnemonicWithoutValidation,
  generateMnemonic,
  IC_DERIVATION_PATH,
} from "$lib/utils/recoveryPhrase";
import { II_URL } from "../utils";

/**
 * Swap the first word around with the next different word found,
 * compared to random shuffle, this guarantees a different phrase.
 *
 * @example swapWordsAround(["abc", "abc", "xyz", "xyz"]) // ["xyz", "abc", "abc", "xyz"]
 */
const swapWordsAround = (words: string[]) => {
  const incorrectOrder = [...words];
  const firstWord = incorrectOrder[0];
  const differentWordIndex = incorrectOrder.findIndex(
    (word) => word !== firstWord,
  );
  incorrectOrder[0] = incorrectOrder[differentWordIndex];
  incorrectOrder[differentWordIndex] = firstWord;
  expect(incorrectOrder).not.toEqual(words); // Smoke test
  return incorrectOrder;
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

test.describe("Recovery phrase", () => {
  test.beforeEach(async ({ manageRecoveryPage, identity }) => {
    await manageRecoveryPage.goto();
    await identity.signIn();
    await manageRecoveryPage.assertNotActivated();
  });

  test.describe("can be activated", () => {
    test.afterEach(
      async ({ page, manageRecoveryPage, identity, words, recoveryPage }) => {
        await manageRecoveryPage.assertActivated();
        // Assert it's still activated after signing back in
        await identity.signOut();
        await manageRecoveryPage.goto();
        await identity.signIn();
        await manageRecoveryPage.assertActivated();
        // Verify we can recover using `words.current`
        await recoveryPage.goto();
        await recoveryPage.start(async (wizard) => {
          await wizard.enterRecoveryPhrase(words.current!);
          await wizard.confirmFoundIdentity(identity.name);
        });
        await page.waitForURL(II_URL + "/manage/access");
        await expect(
          page.getByRole("heading", { name: "Access methods" }),
        ).toBeVisible();
      },
    );

    test("on first attempt", async ({ manageRecoveryPage, words }) => {
      await manageRecoveryPage.activate(async (wizard) => {
        await wizard.acknowledge();
        words.current = await wizard.writeDown();
        await wizard.verifySelecting(words.current);
      });
    });

    test("on retry", async ({ manageRecoveryPage, words }) => {
      await manageRecoveryPage.activate(async (wizard) => {
        await wizard.acknowledge();
        words.current = await wizard.writeDown();
        await wizard.verifySelecting(swapWordsAround(words.current));
        await wizard.retry();
        const reminderWords = await wizard.writeDown();
        expect(reminderWords).toEqual(words.current);
        await wizard.verifySelecting(reminderWords);
      });
    });
  });

  test.describe("can be verified", () => {
    test.beforeEach(async ({ manageRecoveryPage, words }) => {
      words.current = await manageRecoveryPage.activate(async (wizard) => {
        await wizard.acknowledge();
        const words = await wizard.writeDown();
        await wizard.close();
        return words;
      });
      await manageRecoveryPage.assertNotVerified();
    });

    test.afterEach(
      async ({ page, manageRecoveryPage, identity, recoveryPage, words }) => {
        await manageRecoveryPage.assertActivated();
        // Assert it's still activated after signing back in
        await identity.signOut();
        await manageRecoveryPage.goto();
        await identity.signIn();
        await manageRecoveryPage.assertActivated();
        // Verify we can still recover using `words.current`
        await recoveryPage.goto();
        await recoveryPage.start(async (wizard) => {
          await wizard.enterRecoveryPhrase(words.current!);
          await wizard.confirmFoundIdentity(identity.name);
        });
        await page.waitForURL(II_URL + "/manage/access");
        await expect(
          page.getByRole("heading", { name: "Access methods" }),
        ).toBeVisible();
      },
    );

    test.describe("when still signed in", () => {
      test("on first attempt", async ({ manageRecoveryPage, words }) => {
        await manageRecoveryPage.verify(async (wizard) => {
          await wizard.verifySelecting(words.current!);
        });
      });

      test("on retry", async ({ manageRecoveryPage, words }) => {
        await manageRecoveryPage.verify(async (wizard) => {
          await wizard.verifySelecting(swapWordsAround(words.current!));
          await wizard.retry();
          const reminderWords = await wizard.writeDown();
          expect(reminderWords).toEqual(words.current);
          await wizard.verifySelecting(reminderWords);
        });
      });
    });

    test.describe("when coming back after sign out", () => {
      test.beforeEach(async ({ manageRecoveryPage, identity }) => {
        await identity.signOut();
        await manageRecoveryPage.goto();
        await identity.signIn();
      });

      test("on first attempt", async ({ manageRecoveryPage, words }) => {
        await manageRecoveryPage.verify(async (wizard) => {
          await wizard.verifyTyping(words.current!);
        });
      });

      test("on retry", async ({ manageRecoveryPage, words }) => {
        await manageRecoveryPage.verify(async (wizard) => {
          await wizard.verifyTyping(swapWordsAround(words.current!));
          await wizard.retry();
          await wizard.verifyTyping(words.current!);
        });
      });
    });

    test("when signed in (incorrectly) and then when coming back (correctly)", async ({
      manageRecoveryPage,
      identity,
      words,
    }) => {
      await manageRecoveryPage.verify(async (wizard) => {
        await wizard.verifySelecting(swapWordsAround(words.current!));
        await wizard.close();
      });
      await identity.signOut();
      await manageRecoveryPage.goto();
      await identity.signIn();
      await manageRecoveryPage.verify(async (wizard) => {
        await wizard.verifyTyping(words.current!);
      });
    });
  });

  test.describe("can be reset", () => {
    test.afterEach(
      async ({ page, manageRecoveryPage, identity, recoveryPage, words }) => {
        await manageRecoveryPage.assertActivated();
        // Assert it's still activated after signing back in
        await identity.signOut();
        await manageRecoveryPage.goto();
        await identity.signIn();
        await manageRecoveryPage.assertActivated();
        // Verify we can recover using the new recovery phrase (`words.current`)
        await recoveryPage.goto();
        await recoveryPage.start(async (wizard) => {
          await wizard.enterRecoveryPhrase(words.current!);
          await wizard.confirmFoundIdentity(identity.name);
        });
        await page.waitForURL(II_URL + "/manage/access");
        await expect(
          page.getByRole("heading", { name: "Access methods" }),
        ).toBeVisible();
      },
    );

    const scenarios: Array<{ label: string; setup: () => void }> = [
      {
        label: "when it is activated",
        setup: () =>
          test.beforeEach(async ({ manageRecoveryPage, words }) => {
            words.current = await manageRecoveryPage.activate(
              async (wizard) => {
                await wizard.acknowledge();
                const oldWords = await wizard.writeDown();
                await wizard.verifySelecting(oldWords);
                return oldWords;
              },
            );
            await manageRecoveryPage.assertActivated();
          }),
      },
      {
        label: "when it is not verified",
        setup: () =>
          test.beforeEach(async ({ manageRecoveryPage, words }) => {
            words.current = await manageRecoveryPage.activate(
              async (wizard) => {
                await wizard.acknowledge();
                const oldWords = await wizard.writeDown();
                await wizard.close();
                return oldWords;
              },
            );
            await manageRecoveryPage.assertNotVerified();
          }),
      },
    ];

    for (const scenario of scenarios) {
      test.describe(scenario.label, () => {
        scenario.setup();

        test("on first attempt", async ({ manageRecoveryPage, words }) => {
          words.current = await manageRecoveryPage.reset(async (wizard) => {
            await wizard.confirmReset();
            const newWords = await wizard.writeDown();
            expect(newWords).not.toEqual(words.current);
            await wizard.verifySelecting(newWords);
            return newWords;
          });
        });

        test("on retry", async ({ manageRecoveryPage, words }) => {
          words.current = await manageRecoveryPage.reset(async (wizard) => {
            await wizard.confirmReset();
            const newWords = await wizard.writeDown();
            expect(newWords).not.toEqual(words.current);
            await wizard.verifySelecting(swapWordsAround(newWords));
            await wizard.retry();
            const reminderWords = await wizard.writeDown();
            expect(reminderWords).toEqual(newWords);
            await wizard.verifySelecting(reminderWords);
            return reminderWords;
          });
        });
      });
    }

    test.describe("when it is locked (legacy)", () => {
      test.beforeEach(async ({ identity, manageRecoveryPage, words }) => {
        // Use an actor to create a locked recovery phrase
        // since this functionality is no longer available.
        const { actor, identityNumber } = await identity.createActor();
        words.current = generateMnemonic();
        const recoveryIdentity = await fromMnemonicWithoutValidation(
          words.current.join(" "),
          IC_DERIVATION_PATH,
        );
        await actor.authn_method_add(identityNumber, {
          metadata: [
            ["alias", { String: "Recovery phrase" }],
            ["usage", { String: "recovery_phrase" }],
          ],
          authn_method: {
            PubKey: {
              pubkey: new Uint8Array(recoveryIdentity.getPublicKey().derKey),
            },
          },
          security_settings: {
            protection: { Protected: null },
            purpose: { Recovery: null },
          },
          last_authentication: [],
        });
        // Revisit the page to see the changes
        await identity.signOut();
        await manageRecoveryPage.goto();
        await identity.signIn();
        await manageRecoveryPage.assertLocked();
      });

      test("on first attempt", async ({ manageRecoveryPage, words }) => {
        words.current = await manageRecoveryPage.unlockAndReset(
          async (wizard) => {
            await wizard.unlock(words.current!);
            await wizard.confirmReset();
            const newWords = await wizard.writeDown();
            expect(newWords).not.toEqual(words.current);
            await wizard.verifySelecting(newWords);
            return newWords;
          },
        );
      });

      test("on retry", async ({ manageRecoveryPage, words }) => {
        words.current = await manageRecoveryPage.unlockAndReset(
          async (wizard) => {
            await wizard.unlock(swapWordsAround(words.current!));
            await wizard.retry();
            await wizard.unlock(words.current!);
            await wizard.confirmReset();
            const newWords = await wizard.writeDown();
            expect(newWords).not.toEqual(words.current);
            await wizard.verifySelecting(newWords);
            return newWords;
          },
        );
      });
    });
  });

  test.describe("can be cancelled", () => {
    test("when activating", async ({ manageRecoveryPage }) => {
      await manageRecoveryPage.activate(async (wizard) => {
        await wizard.close();
      });
      await manageRecoveryPage.assertNotActivated();
    });

    test("when resetting", async ({
      page,
      manageRecoveryPage,
      recoveryPage,
      identity,
    }) => {
      const oldWords = await manageRecoveryPage.activate(async (wizard) => {
        await wizard.acknowledge();
        const words = await wizard.writeDown();
        await wizard.verifySelecting(words);
        return words;
      });
      await manageRecoveryPage.assertActivated();
      await manageRecoveryPage.reset(async (wizard) => {
        await wizard.cancelReset();
      });
      await manageRecoveryPage.assertActivated();
      //  Verify we can still recover using `oldWords`
      await recoveryPage.goto();
      await recoveryPage.start(async (wizard) => {
        await wizard.enterRecoveryPhrase(oldWords);
        await wizard.confirmFoundIdentity(identity.name);
      });
      await page.waitForURL(II_URL + "/manage/access");
      await expect(
        page.getByRole("heading", { name: "Access methods" }),
      ).toBeVisible();
    });
  });

  test.describe("is not verified", () => {
    test.afterEach(async ({ manageRecoveryPage, identity }) => {
      await manageRecoveryPage.assertNotVerified();
      // Assert it's still not verified after signing back in
      await identity.signOut();
      await manageRecoveryPage.goto();
      await identity.signIn();
      await manageRecoveryPage.assertNotVerified();
    });

    test.describe("when closed during activation", () => {
      test("before written down", async ({ manageRecoveryPage }) => {
        await manageRecoveryPage.activate(async (wizard) => {
          await wizard.acknowledge();
          await wizard.close();
        });
      });

      test("after written down", async ({ manageRecoveryPage }) => {
        await manageRecoveryPage.activate(async (wizard) => {
          await wizard.acknowledge();
          await wizard.writeDown();
          await wizard.close();
        });
      });

      test("before retry", async ({ manageRecoveryPage }) => {
        await manageRecoveryPage.activate(async (wizard) => {
          await wizard.acknowledge();
          const words = await wizard.writeDown();
          await wizard.verifySelecting(swapWordsAround(words));
          await wizard.close();
        });
      });

      test("after retry", async ({ manageRecoveryPage }) => {
        await manageRecoveryPage.activate(async (wizard) => {
          await wizard.acknowledge();
          const words = await wizard.writeDown();
          await wizard.verifySelecting(swapWordsAround(words));
          await wizard.retry();
          await wizard.close();
        });
      });
    });

    test.describe("when closed during reset", () => {
      test.beforeEach(async ({ manageRecoveryPage }) => {
        await manageRecoveryPage.activate(async (wizard) => {
          await wizard.acknowledge();
          const words = await wizard.writeDown();
          await wizard.verifySelecting(words);
        });
        await manageRecoveryPage.assertActivated();
      });

      test("before written down", async ({ manageRecoveryPage }) => {
        await manageRecoveryPage.reset(async (wizard) => {
          await wizard.confirmReset();
          await wizard.close();
        });
      });

      test("after written down", async ({ manageRecoveryPage }) => {
        await manageRecoveryPage.reset(async (wizard) => {
          await wizard.confirmReset();
          await wizard.writeDown();
          await wizard.close();
        });
      });

      test("before retry", async ({ manageRecoveryPage }) => {
        await manageRecoveryPage.reset(async (wizard) => {
          await wizard.confirmReset();
          const words = await wizard.writeDown();
          await wizard.verifySelecting(swapWordsAround(words));
          await wizard.close();
        });
      });

      test("after retry", async ({ manageRecoveryPage }) => {
        await manageRecoveryPage.reset(async (wizard) => {
          await wizard.confirmReset();
          const words = await wizard.writeDown();
          await wizard.verifySelecting(swapWordsAround(words));
          await wizard.retry();
          await wizard.close();
        });
      });
    });
  });
});
