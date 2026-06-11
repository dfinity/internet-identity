import { test } from "../fixtures";
import { expect } from "@playwright/test";
import {
  addVirtualAuthenticator,
  authorize,
  removeVirtualAuthenticator,
  addCredentialToVirtualAuthenticator,
  createActorForCredential,
  createLegacyCredential,
  swapToLegacyOnlyIdentity,
} from "../utils";

test("Can upgrade identity", async ({ page, identities }) => {
  const legacyCredential = await createLegacyCredential(page);

  const actor = await createActorForCredential(
    identities[0].host,
    identities[0].canisterId,
    identities[0].credentials[0],
  );
  await swapToLegacyOnlyIdentity(actor, identities[0], legacyCredential);

  // The dedicated upgrade panel on /authorize is gated by the
  // GUIDED_UPGRADE feature flag, which only auto-enables when the page
  // loads on a non-primary origin (legacy domain). Persisting the
  // override in localStorage beats the domain-based default and gives
  // every popup opened in this context a visible "Upgrade" entry.
  await page.context().addInitScript(() => {
    try {
      window.localStorage.setItem(
        "ii-localstorage-feature-flags__GUIDED_UPGRADE",
        JSON.stringify(true),
      );
    } catch {
      // localStorage may be locked in some test contexts.
    }
  });

  // Verify identity can be upgraded multiple times via the dapp-driven
  // /authorize flow — the new canonical entry point for legacy users.
  for (let attempt = 0; attempt < 3; attempt++) {
    await authorize(page, async (authPage) => {
      const authenticatorId = await addVirtualAuthenticator(authPage);
      await addCredentialToVirtualAuthenticator(
        authPage,
        authenticatorId,
        legacyCredential,
      );

      // First attempt: the panel renders expanded with a primary
      // "Upgrade your identity" button. After the first success the
      // panel collapses (state persisted to `ii-guided-upgrade-collapsed`
      // in localStorage) and the entry becomes the secondary "Upgrade"
      // link in the collapsed header.
      const upgradeButton =
        attempt === 0
          ? authPage.getByRole("button", { name: "Upgrade your identity" })
          : authPage.getByRole("button", { name: "Upgrade", exact: true });
      await upgradeButton.click();

      const dialog = authPage.getByRole("dialog");
      await dialog
        .getByPlaceholder("Internet Identity number")
        .fill(identities[0].identityNumber.toString());
      await dialog.getByRole("button", { name: "Continue" }).click();

      // On subsequent attempts the identity is already migrated; the
      // wizard surfaces the "already upgraded" view with an explicit
      // "Upgrade again" CTA.
      if (attempt > 0) {
        await expect(
          dialog.getByRole("heading", { name: "Identity already upgraded" }),
        ).toBeVisible();
        await dialog.getByRole("button", { name: "Upgrade again" }).click();
      }

      // Complete the upgrade. The wizard creates a fresh passkey in the
      // authenticator and routes to the success view.
      await dialog.getByLabel("Identity name").fill(identities[0].name);
      await dialog.getByRole("button", { name: "Upgrade identity" }).click();

      // Cleanup the popup's authenticator before driving the success
      // view forward — the next iteration spins up a fresh authenticator
      // and re-attaches the legacy credential alone.
      await removeVirtualAuthenticator(authPage, authenticatorId);

      // The success view auto-redirects to the dapp after a 5s
      // countdown; clicking is faster and matches a real user.
      await authPage.getByRole("button", { name: /Go to the app/ }).click();
    });
  }
});
