import { FLOWS } from "../../flows";
import { addVirtualAuthenticator, runInBrowser } from "../../util";
import { MainView, RecoverView } from "../../views";

import { DEVICE_NAME1, II_URL, RECOVERY_PHRASE_NAME } from "../../constants";

test("Reset protected recovery phrase", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    await addVirtualAuthenticator(browser);
    await browser.url(II_URL);
    await FLOWS.registerNewIdentityWelcomeView(DEVICE_NAME1, browser);
    const mainView = new MainView(browser);
    await mainView.waitForDeviceDisplay(DEVICE_NAME1);
    const seedPhrase = await FLOWS.addRecoveryMechanismSeedPhrase(browser);
    await mainView.waitForDisplay();

    await mainView.protect(RECOVERY_PHRASE_NAME, seedPhrase);

    await mainView.waitForDisplay();

    // Ensure the settings dropdown is in view
    await browser.execute("window.scrollTo(0, document.body.scrollHeight)");
    await mainView.reset(RECOVERY_PHRASE_NAME);
    await browser.acceptAlert();

    const recoveryView = new RecoverView(browser);
    await recoveryView.waitForSeedInputDisplay();
    await recoveryView.enterSeedPhrase(seedPhrase);
    await recoveryView.enterSeedPhraseContinue();

    const _seedPhrase = await FLOWS.readSeedPhrase(browser);

    await mainView.waitForDisplay();
  });
}, 300_000);

test("Reset protected recovery phrase, confirm with empty seed phrase", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    await addVirtualAuthenticator(browser);
    await browser.url(II_URL);
    await FLOWS.registerNewIdentityWelcomeView(DEVICE_NAME1, browser);
    const mainView = new MainView(browser);
    await mainView.waitForDeviceDisplay(DEVICE_NAME1);
    const seedPhrase = await FLOWS.addRecoveryMechanismSeedPhrase(browser);
    await mainView.waitForDisplay();

    await mainView.protect(RECOVERY_PHRASE_NAME, seedPhrase);
    await mainView.waitForDisplay();

    // Ensure the settings dropdown is in view
    await browser.execute("window.scrollTo(0, document.body.scrollHeight)");
    await mainView.reset(RECOVERY_PHRASE_NAME);
    await browser.acceptAlert();

    const recoveryView = new RecoverView(browser);
    await recoveryView.waitForSeedInputDisplay();
    await recoveryView.enterSeedPhrase("");
    await recoveryView.enterSeedPhraseContinue();
    await recoveryView.waitForInvalidSeedPhraseDisplay();
  });
}, 300_000);
