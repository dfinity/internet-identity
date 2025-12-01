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
  test.beforeEach(async ({ recoveryPhrasePage, identity }) => {
    await recoveryPhrasePage.goto();
    await identity.signIn();
    await recoveryPhrasePage.assertNotActivated();
  });

  test.describe("can be activated", () => {
    test.afterEach(async ({ recoveryPhrasePage, identity }) => {
      await recoveryPhrasePage.assertActivated();
      // Assert it's still activated after signing back in
      await identity.signOut();
      await recoveryPhrasePage.goto();
      await identity.signIn();
      await recoveryPhrasePage.assertActivated();
      // TODO: Verify we can recover using `words.current`
    });

    test("on first attempt", async ({ recoveryPhrasePage, words }) => {
      await recoveryPhrasePage.activate(async (wizard) => {
        await wizard.acknowledge();
        words.current = await wizard.writeDown();
        await wizard.verifySelecting(words.current);
      });
    });

    test("on retry", async ({ recoveryPhrasePage, words }) => {
      await recoveryPhrasePage.activate(async (wizard) => {
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
    test.beforeEach(async ({ recoveryPhrasePage, words }) => {
      words.current = await recoveryPhrasePage.activate(async (wizard) => {
        await wizard.acknowledge();
        const words = await wizard.writeDown();
        await wizard.close();
        return words;
      });
      await recoveryPhrasePage.assertNotVerified();
    });

    test.afterEach(async ({ recoveryPhrasePage, identity }) => {
      await recoveryPhrasePage.assertActivated();
      // Assert it's still activated after signing back in
      await identity.signOut();
      await recoveryPhrasePage.goto();
      await identity.signIn();
      await recoveryPhrasePage.assertActivated();
      // TODO: Verify we can recover using `words.current`
    });

    test.describe("when still signed in", () => {
      test("on first attempt", async ({ recoveryPhrasePage, words }) => {
        await recoveryPhrasePage.verify(async (wizard) => {
          await wizard.verifySelecting(words.current!);
        });
      });

      test("on retry", async ({ recoveryPhrasePage, words }) => {
        await recoveryPhrasePage.verify(async (wizard) => {
          await wizard.verifySelecting(swapWordsAround(words.current!));
          await wizard.retry();
          const reminderWords = await wizard.writeDown();
          expect(reminderWords).toEqual(words.current);
          await wizard.verifySelecting(reminderWords);
        });
      });
    });

    test.describe("when coming back after sign out", () => {
      test.beforeEach(async ({ recoveryPhrasePage, identity }) => {
        await identity.signOut();
        await recoveryPhrasePage.goto();
        await identity.signIn();
      });

      test("on first attempt", async ({ recoveryPhrasePage, words }) => {
        await recoveryPhrasePage.verify(async (wizard) => {
          await wizard.verifyTyping(words.current!);
        });
      });

      test("on retry", async ({ recoveryPhrasePage, words }) => {
        await recoveryPhrasePage.verify(async (wizard) => {
          await wizard.verifyTyping(swapWordsAround(words.current!));
          await wizard.retry();
          await wizard.verifyTyping(words.current!);
        });
      });
    });
  });

  test.describe("can be reset", () => {
    test.afterEach(async ({ recoveryPhrasePage, identity }) => {
      await recoveryPhrasePage.assertActivated();
      // Assert it's still activated after signing back in
      await identity.signOut();
      await recoveryPhrasePage.goto();
      await identity.signIn();
      await recoveryPhrasePage.assertActivated();
      // TODO: Verify we can recover using `words.current`
    });

    const scenarios: Array<{ label: string; setup: () => void }> = [
      {
        label: "when it is activated",
        setup: () =>
          test.beforeEach(async ({ recoveryPhrasePage, words }) => {
            words.current = await recoveryPhrasePage.activate(
              async (wizard) => {
                await wizard.acknowledge();
                const oldWords = await wizard.writeDown();
                await wizard.verifySelecting(oldWords);
                return oldWords;
              },
            );
            await recoveryPhrasePage.assertActivated();
          }),
      },
      {
        label: "when it is not verified",
        setup: () =>
          test.beforeEach(async ({ recoveryPhrasePage, words }) => {
            words.current = await recoveryPhrasePage.activate(
              async (wizard) => {
                await wizard.acknowledge();
                const oldWords = await wizard.writeDown();
                await wizard.close();
                return oldWords;
              },
            );
            await recoveryPhrasePage.assertNotVerified();
          }),
      },
    ];

    for (const scenario of scenarios) {
      test.describe(scenario.label, () => {
        scenario.setup();

        test("on first attempt", async ({ recoveryPhrasePage, words }) => {
          words.current = await recoveryPhrasePage.reset(async (wizard) => {
            await wizard.confirmReset();
            const newWords = await wizard.writeDown();
            expect(newWords).not.toEqual(words.current);
            await wizard.verifySelecting(newWords);
            return newWords;
          });
        });

        test("on retry", async ({ recoveryPhrasePage, words }) => {
          words.current = await recoveryPhrasePage.reset(async (wizard) => {
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
    test("when activating", async ({ recoveryPhrasePage }) => {
      await recoveryPhrasePage.activate(async (wizard) => {
        await wizard.close();
      });
      await recoveryPhrasePage.assertNotActivated();
    });

    test("when resetting", async ({ recoveryPhrasePage }) => {
      // eslint-disable-next-line @typescript-eslint/no-unused-vars
      const oldWords = await recoveryPhrasePage.activate(async (wizard) => {
        await wizard.acknowledge();
        const words = await wizard.writeDown();
        await wizard.verifySelecting(words);
        return words;
      });
      await recoveryPhrasePage.assertActivated();
      await recoveryPhrasePage.reset(async (wizard) => {
        await wizard.cancelReset();
      });
      await recoveryPhrasePage.assertActivated();
      // TODO: Verify we can still recover using `oldWords`
    });
  });

  test.describe("is not verified", () => {
    test.afterEach(async ({ recoveryPhrasePage, identity }) => {
      await recoveryPhrasePage.assertNotVerified();
      // Assert it's still not verified after signing back in
      await identity.signOut();
      await recoveryPhrasePage.goto();
      await identity.signIn();
      await recoveryPhrasePage.assertNotVerified();
    });

    test.describe("when closed during activation", () => {
      test("before written down", async ({ recoveryPhrasePage }) => {
        await recoveryPhrasePage.activate(async (wizard) => {
          await wizard.acknowledge();
          await wizard.close();
        });
      });

      test("after written down", async ({ recoveryPhrasePage }) => {
        await recoveryPhrasePage.activate(async (wizard) => {
          await wizard.acknowledge();
          await wizard.writeDown();
          await wizard.close();
        });
      });

      test("before retry", async ({ recoveryPhrasePage }) => {
        await recoveryPhrasePage.activate(async (wizard) => {
          await wizard.acknowledge();
          const words = await wizard.writeDown();
          await wizard.verifySelecting(swapWordsAround(words));
          await wizard.close();
        });
      });

      test("after retry", async ({ recoveryPhrasePage }) => {
        await recoveryPhrasePage.activate(async (wizard) => {
          await wizard.acknowledge();
          const words = await wizard.writeDown();
          await wizard.verifySelecting(swapWordsAround(words));
          await wizard.retry();
          await wizard.close();
        });
      });
    });

    test.describe("when closed during reset", () => {
      test.beforeEach(async ({ recoveryPhrasePage }) => {
        await recoveryPhrasePage.activate(async (wizard) => {
          await wizard.acknowledge();
          const words = await wizard.writeDown();
          await wizard.verifySelecting(words);
        });
        await recoveryPhrasePage.assertActivated();
      });

      test("before written down", async ({ recoveryPhrasePage }) => {
        await recoveryPhrasePage.reset(async (wizard) => {
          await wizard.confirmReset();
          await wizard.close();
        });
      });

      test("after written down", async ({ recoveryPhrasePage }) => {
        await recoveryPhrasePage.reset(async (wizard) => {
          await wizard.confirmReset();
          await wizard.writeDown();
          await wizard.close();
        });
      });

      test("before retry", async ({ recoveryPhrasePage }) => {
        await recoveryPhrasePage.reset(async (wizard) => {
          await wizard.confirmReset();
          const words = await wizard.writeDown();
          await wizard.verifySelecting(swapWordsAround(words));
          await wizard.close();
        });
      });

      test("after retry", async ({ recoveryPhrasePage }) => {
        await recoveryPhrasePage.reset(async (wizard) => {
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
