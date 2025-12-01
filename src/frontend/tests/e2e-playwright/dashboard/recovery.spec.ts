import { test as base } from "../fixtures";
import { expect } from "@playwright/test";

/**
 * Swap the first word around with the next different word found,
 * compared to random shuffle, this guarantees a different phrase.
 */
const swapWordsAround = (words: string[]) => {
  const incorrectOrder = [...words];
  const firstWord = incorrectOrder[0];
  const differentWordIndex = incorrectOrder.findIndex(
    (word) => word !== firstWord,
  );
  incorrectOrder[0] = incorrectOrder[differentWordIndex];
  incorrectOrder[differentWordIndex] = firstWord;
  expect(incorrectOrder).not.toEqual(words);
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
    test.afterEach(async ({ manageRecoveryPage, identity }) => {
      await manageRecoveryPage.assertActivated();
      // Assert it's still activated after signing back in
      await identity.signOut();
      await manageRecoveryPage.goto();
      await identity.signIn();
      await manageRecoveryPage.assertActivated();
      // TODO: Verify we can recover using `words.current`
    });

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

    test.afterEach(async ({ manageRecoveryPage, identity }) => {
      await manageRecoveryPage.assertActivated();
      // Assert it's still activated after signing back in
      await identity.signOut();
      await manageRecoveryPage.goto();
      await identity.signIn();
      await manageRecoveryPage.assertActivated();
      // TODO: Verify we can recover using `words.current`
    });

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
  });

  test.describe("can be reset", () => {
    test.afterEach(async ({ manageRecoveryPage, identity }) => {
      await manageRecoveryPage.assertActivated();
      // Assert it's still activated after signing back in
      await identity.signOut();
      await manageRecoveryPage.goto();
      await identity.signIn();
      await manageRecoveryPage.assertActivated();
      // TODO: Verify we can recover using `words.current`
    });

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
  });

  test.describe("can be cancelled", () => {
    test("when activating", async ({ manageRecoveryPage }) => {
      await manageRecoveryPage.activate(async (wizard) => {
        await wizard.close();
      });
      await manageRecoveryPage.assertNotActivated();
    });

    test("when resetting", async ({ manageRecoveryPage }) => {
      // eslint-disable-next-line @typescript-eslint/no-unused-vars
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
      // TODO: Verify we can still recover using `oldWords`
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
