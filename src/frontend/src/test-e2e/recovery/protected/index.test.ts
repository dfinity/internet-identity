import { FLOWS } from "../../flows";
import { addVirtualAuthenticator, runInBrowser } from "../../util";
import { MainView } from "../../views";

import { DEVICE_NAME1, II_URL, RECOVERY_PHRASE_NAME } from "../../constants";

test("Make recovery protected", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    await addVirtualAuthenticator(browser);
    await browser.url(II_URL);
    await FLOWS.registerNewIdentityWelcomeView(DEVICE_NAME1, browser);
    const mainView = new MainView(browser);
    await mainView.waitForDeviceDisplay(DEVICE_NAME1);
    const seedPhrase = await FLOWS.addRecoveryMechanismSeedPhrase(browser);
    await mainView.waitForDisplay();
    await mainView.assertDeviceUnprotected(RECOVERY_PHRASE_NAME);
    await mainView.protect(RECOVERY_PHRASE_NAME, seedPhrase);
    await mainView.assertDeviceProtected(RECOVERY_PHRASE_NAME);
  });
}, 300_000);

test("Make recovery unprotected", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    await addVirtualAuthenticator(browser);
    await browser.url(II_URL);
    await FLOWS.registerNewIdentityWelcomeView(DEVICE_NAME1, browser);
    const mainView = new MainView(browser);
    await mainView.waitForDeviceDisplay(DEVICE_NAME1);
    const seedPhrase = await FLOWS.addRecoveryMechanismSeedPhrase(browser);
    await mainView.waitForDisplay();
    await mainView.assertDeviceUnprotected(RECOVERY_PHRASE_NAME);
    await mainView.protect(RECOVERY_PHRASE_NAME, seedPhrase);
    await mainView.assertDeviceProtected(RECOVERY_PHRASE_NAME);

    await mainView.unprotect(RECOVERY_PHRASE_NAME, seedPhrase);
    await mainView.assertDeviceUnprotected(RECOVERY_PHRASE_NAME);
  });
}, 300_000);
