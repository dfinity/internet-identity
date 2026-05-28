import { expect } from "@playwright/test";
import { test } from "../../fixtures";
import {
  fromMnemonicWithoutValidation,
  generateMnemonic,
  IC_DERIVATION_PATH,
} from "$lib/utils/recoveryPhrase";
import { createActorForCredential, II_URL } from "../../utils";
import { DEFAULT_PASSKEY_NAME } from "../../fixtures/manageAccessPage";

test.describe("Sign-out confirmation targets the active identity", () => {
  test.use({
    identityConfig: {
      createIdentities: [{ name: "Identity X" }, { name: "Identity Y" }],
    },
  });

  test("removes the active identity after recovery + access-method switch", async ({
    page,
    identities,
    signInWithIdentity,
    addAuthenticatorForIdentity,
    managePage,
    manageAccessPage,
    recoveryPage,
  }) => {
    const [identityX, identityY] = identities;

    const actor = await createActorForCredential(
      identityY.host,
      identityY.canisterId,
      identityY.credentials[0],
    );
    const words = generateMnemonic();
    const recoveryIdentity = await fromMnemonicWithoutValidation(
      words.join(" "),
      IC_DERIVATION_PATH,
    );
    await actor.authn_method_add(identityY.identityNumber, {
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
        protection: { Unprotected: null },
        purpose: { Recovery: null },
      },
      last_authentication: [],
    });

    await page.goto(II_URL);
    await signInWithIdentity(page, identityX.identityNumber);
    await managePage.assertVisible();
    await managePage.signOut();

    await recoveryPage.goto();
    await recoveryPage.start(async (wizard) => {
      await wizard.enterRecoveryPhrase(words);
      await wizard.confirmFoundIdentity(identityY.name);
    });
    await manageAccessPage.assertVisible();

    await addAuthenticatorForIdentity(page, identityY.identityNumber);
    await manageAccessPage
      .findPasskey(DEFAULT_PASSKEY_NAME)
      .switch((dialog) => dialog.confirm());

    await managePage.signOut(async (confirmation) => {
      const dialog = page.getByRole("dialog");
      await expect(dialog.getByText(identityY.name)).toBeVisible();
      await expect(dialog.getByText(identityX.name)).toBeHidden();
      await confirmation.removeFromDevice();
    });

    const raw = await page.evaluate(() =>
      localStorage.getItem("ii-last-used-identities"),
    );
    expect(raw).not.toBeNull();
    const stored = JSON.parse(raw!);
    expect(stored.data).toHaveProperty(identityX.identityNumber.toString());
    expect(stored.data).not.toHaveProperty(identityY.identityNumber.toString());
  });
});
