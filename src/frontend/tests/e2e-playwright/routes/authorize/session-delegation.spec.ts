import { expect } from "@playwright/test";
import { test } from "../../fixtures";
import { authorize, TEST_APP_URL, II_URL } from "../../utils";

test.describe("session delegation — multiple accounts without re-auth", () => {
  test("after sign-in, reloading authorize and toggling multiple accounts loads accounts without WebAuthn ceremony", async ({
    page,
    identities,
    addAuthenticatorForIdentity,
    signInWithIdentity,
  }) => {
    await authorize(page, async (authPage) => {
      await signInWithIdentity(authPage, identities[0].identityNumber);
      await authPage
        .getByRole("button", { name: "Continue", exact: true })
        .click();
    });

    const authPagePromise = page.context().waitForEvent("page");
    await page.goto(TEST_APP_URL);
    await page.getByRole("textbox", { name: "Identity Provider" }).fill(II_URL);
    await page.getByRole("button", { name: "Sign In" }).click();
    const authPage = await authPagePromise;

    await addAuthenticatorForIdentity(authPage, identities[0].identityNumber);

    const credentialPromise = authPage.waitForEvent("dialog").catch(() => null);

    await authPage
      .getByRole("switch", { name: "Enable multiple accounts" })
      .setChecked(true);

    await expect(
      authPage.getByRole("switch", { name: "Enable multiple accounts" }),
    ).toBeChecked();

    await expect(
      authPage.getByRole("button", { name: /Continue with/ }),
    ).toBeVisible();

    const credentialDialog = await Promise.race([
      credentialPromise,
      new Promise<null>((resolve) => setTimeout(() => resolve(null), 500)),
    ]);

    expect(
      credentialDialog,
      "no WebAuthn ceremony (dialog) should fire when toggling multiple accounts via session delegation",
    ).toBeNull();

    await authPage.close();
  });

  test("Continue action still triggers WebAuthn ceremony even with valid session delegation", async ({
    page,
    identities,
    addAuthenticatorForIdentity,
    signInWithIdentity,
  }) => {
    const expectedPrincipal = await authorize(page, async (authPage) => {
      await signInWithIdentity(authPage, identities[0].identityNumber);
      await authPage
        .getByRole("button", { name: "Continue", exact: true })
        .click();
    });

    // Return visit: a session delegation now exists. Clicking Continue must
    // still go through full auth (prepare_account_delegation is not opted
    // into the session scope), and yield the same principal as the first
    // full-auth authorization.
    const principal = await authorize(page, async (authPage) => {
      await addAuthenticatorForIdentity(authPage, identities[0].identityNumber);
      await authPage
        .getByRole("button", { name: "Continue", exact: true })
        .click();
    });

    expect(principal).toBe(expectedPrincipal);
  });

  test("expired session triggers WebAuthn ceremony when toggling multiple accounts", async ({
    page,
    identities,
    addAuthenticatorForIdentity,
    signInWithIdentity,
  }) => {
    // Initial sign-in: mints a session and persists it in II's IndexedDB.
    await authorize(page, async (authPage) => {
      await signInWithIdentity(authPage, identities[0].identityNumber);
      await authPage
        .getByRole("button", { name: "Continue", exact: true })
        .click();
    });

    // Simulate clock-moved-forward by rewriting the stored session's
    // `expiresAtMillis` to the past. The FE's expiry-margin check in
    // `actorForIdentity` should then short-circuit, purge the record,
    // and fall through to ceremony.
    const tamperPage = await page.context().newPage();
    await tamperPage.goto(II_URL);
    await tamperPage.evaluate(async (recordKey) => {
      await new Promise<void>((resolve, reject) => {
        const open = indexedDB.open("ii-session-delegations");
        open.onsuccess = () => {
          const db = open.result;
          const tx = db.transaction("keys", "readwrite");
          const store = tx.objectStore("keys");
          const getReq = store.get(recordKey);
          getReq.onsuccess = () => {
            const record = getReq.result;
            if (record === undefined) {
              db.close();
              reject(new Error("no session record to tamper with"));
              return;
            }
            record.expiresAtMillis = Date.now() - 1000;
            const putReq = store.put(record, recordKey);
            putReq.onsuccess = () => {
              db.close();
              resolve();
            };
            putReq.onerror = () => {
              db.close();
              reject(putReq.error);
            };
          };
          getReq.onerror = () => {
            db.close();
            reject(getReq.error);
          };
        };
        open.onerror = () => reject(open.error);
      });
    }, identities[0].identityNumber.toString());
    await tamperPage.close();

    // Return visit: the stored session is now "expired" from the FE's POV.
    // Toggling multiple accounts must trigger a WebAuthn ceremony because
    // `actorForIdentity` will return undefined and the handler falls through
    // to `authLastUsedFlow.authenticate(...)`.
    const authPagePromise = page.context().waitForEvent("page");
    await page.goto(TEST_APP_URL);
    await page.getByRole("textbox", { name: "Identity Provider" }).fill(II_URL);
    await page.getByRole("button", { name: "Sign In" }).click();
    const authPage = await authPagePromise;

    await addAuthenticatorForIdentity(authPage, identities[0].identityNumber);

    const credentialPromise = authPage.waitForEvent("dialog").catch(() => null);

    await authPage
      .getByRole("switch", { name: "Enable multiple accounts" })
      .setChecked(true);

    const credentialDialog = await Promise.race([
      credentialPromise,
      new Promise<null>((resolve) => setTimeout(() => resolve(null), 5000)),
    ]);

    expect(
      credentialDialog,
      "WebAuthn ceremony must fire when the session is expired",
    ).not.toBeNull();

    await authPage.close();
  });

  test("fresh browser context without IndexedDB requires WebAuthn to load accounts", async ({
    browser,
    identities: _identities,
  }) => {
    const freshContext = await browser.newContext();
    const freshPage = await freshContext.newPage();

    try {
      await freshPage.goto(TEST_APP_URL);
      await freshPage
        .getByRole("textbox", { name: "Identity Provider" })
        .fill(II_URL);

      const authPagePromise = freshContext.waitForEvent("page");
      await freshPage.getByRole("button", { name: "Sign In" }).click();
      const authPage = await authPagePromise;

      await expect(
        authPage.getByRole("switch", { name: "Enable multiple accounts" }),
      ).toBeHidden();
    } finally {
      await freshPage.close();
      await freshContext.close();
    }
  });
});
