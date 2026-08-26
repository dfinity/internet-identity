import { expect } from "@playwright/test";
import { test } from "../../../fixtures";
import { continueAs, openSettings } from "./helpers";

/**
 * An identity keeps a bounded list of the browsers it is signed in from, so
 * signing in from one more than it holds has to drop one rather than refuse. This
 * is that boundary: the newest sign-in works, the list stays capped, and the
 * browser that was dropped loses its access.
 *
 * Runs CAP-6 of `docs/ongoing/session-test-scenarios.md`. The rest of that group
 * needs more apps than an identity keeps rows for, which is not something a
 * browser can reach.
 */
test.describe("using many apps and browsers", () => {
  test("signing in from more browsers than the list holds drops the oldest", async ({
    browser,
    openTestApp,
    identities,
    signInWithIdentity,
  }) => {
    test.setTimeout(15 * 60 * 1000);
    // A context is a separate browser as far as the identity is concerned, so the
    // 20-entry cap is reachable without 20 machines.
    const CAP = 20;
    const browsers = [];
    for (let index = 0; index < CAP + 2; index++) {
      const fresh = await browser.newContext({ ignoreHTTPSErrors: true });
      const app = openTestApp(await fresh.newPage());
      await app.open();
      await app.signIn(
        continueAs(identities[0].identityNumber, signInWithIdentity),
      );
      browsers.push({ fresh, app });
    }

    // DEV-14: reaching the limit drops the least recently used rather than
    // refusing, so the newest sign-in worked and the list is capped.
    const settings = await openSettings(
      browsers[browsers.length - 1].fresh,
      identities[0].identityNumber,
      signInWithIdentity,
    );
    const listed = await settings
      .getByRole("button", { name: "Sign out" })
      .count();
    expect(listed).toBeLessThanOrEqual(CAP);

    // DEV-15: dropping an entry ends that browser's sessions.
    const oldest = browsers[0].app;
    await oldest.focus();
    await oldest.ageDelegation();
    await oldest.replaceDelegation();
    await expect(oldest.state).toHaveText("no session", { timeout: 30_000 });

    for (const { fresh } of browsers) await fresh.close();
  });
});
