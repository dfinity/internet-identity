import {
  DeviceSettingsView,
  MainView,
  RecoverView,
  WelcomeView,
} from "./views";
import { FLOWS } from "./flows";
import { addVirtualAuthenticator, runInBrowser } from "./util";

import { II_URL, DEVICE_NAME1, RECOVERY_PHRASE_NAME } from "./constants";

test("Recover access, after registration", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    await addVirtualAuthenticator(browser);
    await browser.url(II_URL);
    const userNumber = await FLOWS.registerNewIdentityWelcomeView(
      DEVICE_NAME1,
      browser
    );
    const mainView = new MainView(browser);
    await mainView.waitForDeviceDisplay(DEVICE_NAME1);
    const seedPhrase = await FLOWS.addRecoveryMechanismSeedPhrase(browser);
    await mainView.waitForDisplay();
    await mainView.logout();

    const welcomeView = new WelcomeView(browser);
    await welcomeView.recover();
    const recoveryView = new RecoverView(browser);
    await recoveryView.waitForDisplay();
    await recoveryView.enterIdentityAnchor(userNumber);
    await recoveryView.continue();
    await recoveryView.waitForSeedInputDisplay();
    await recoveryView.enterSeedPhrase(seedPhrase);
    await recoveryView.enterSeedPhraseContinue();
    await mainView.waitForDeviceDisplay(DEVICE_NAME1);
  });
}, 300_000);

test("Remove unprotected recovery phrase", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    await addVirtualAuthenticator(browser);
    await browser.url(II_URL);
    await FLOWS.registerNewIdentityWelcomeView(DEVICE_NAME1, browser);
    const mainView = new MainView(browser);
    await mainView.waitForDeviceDisplay(DEVICE_NAME1);
    await FLOWS.addRecoveryMechanismSeedPhrase(browser);
    await mainView.waitForDisplay();

    await mainView.waitForDeviceDisplay(RECOVERY_PHRASE_NAME);
    await mainView.deviceSettings(RECOVERY_PHRASE_NAME);

    const settingsView = new DeviceSettingsView(browser);
    await settingsView.waitForDisplay();
    await settingsView.remove();
    await browser.acceptAlert();

    await mainView.waitForDeviceNotDisplay(RECOVERY_PHRASE_NAME);
  });
}, 300_000);

test("Make recovery protected", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    await addVirtualAuthenticator(browser);
    await browser.url(II_URL);
    await FLOWS.registerNewIdentityWelcomeView(DEVICE_NAME1, browser);
    const mainView = new MainView(browser);
    await mainView.waitForDeviceDisplay(DEVICE_NAME1);
    const seedPhrase = await FLOWS.addRecoveryMechanismSeedPhrase(browser);
    await mainView.waitForDisplay();

    await mainView.deviceSettings(RECOVERY_PHRASE_NAME);

    const settingsView = new DeviceSettingsView(browser);
    await settingsView.waitForDisplay();
    await settingsView.protect(seedPhrase);
  });
}, 300_000);

test("Remove protected recovery phrase", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    await addVirtualAuthenticator(browser);
    await browser.url(II_URL);
    await FLOWS.registerNewIdentityWelcomeView(DEVICE_NAME1, browser);
    const mainView = new MainView(browser);
    await mainView.waitForDeviceDisplay(DEVICE_NAME1);
    const seedPhrase = await FLOWS.addRecoveryMechanismSeedPhrase(browser);
    await mainView.waitForDisplay();

    await mainView.deviceSettings(RECOVERY_PHRASE_NAME);

    const settingsView = new DeviceSettingsView(browser);
    await settingsView.waitForDisplay();
    await settingsView.protect(seedPhrase);
    await settingsView.waitForDisplay();

    await settingsView.remove();
    await browser.acceptAlert();

    const recoveryView = new RecoverView(browser);
    await recoveryView.waitForSeedInputDisplay();
    await recoveryView.enterSeedPhrase(seedPhrase);
    await recoveryView.enterSeedPhraseContinue();
    await mainView.waitForDisplay();
    await mainView.waitForDeviceNotDisplay(RECOVERY_PHRASE_NAME);
  });
}, 300_000);

test("Remove protected recovery phrase, confirm with empty seed phrase", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    await addVirtualAuthenticator(browser);
    await browser.url(II_URL);
    await FLOWS.registerNewIdentityWelcomeView(DEVICE_NAME1, browser);
    const mainView = new MainView(browser);
    await mainView.waitForDeviceDisplay(DEVICE_NAME1);
    const seedPhrase = await FLOWS.addRecoveryMechanismSeedPhrase(browser);
    await mainView.waitForDisplay();

    await mainView.deviceSettings(RECOVERY_PHRASE_NAME);

    const settingsView = new DeviceSettingsView(browser);
    await settingsView.waitForDisplay();
    await settingsView.protect(seedPhrase);

    await settingsView.remove();
    await browser.acceptAlert();

    const recoveryView = new RecoverView(browser);
    await recoveryView.waitForSeedInputDisplay();
    await recoveryView.enterSeedPhrase("");
    await recoveryView.enterSeedPhraseContinue();
    await recoveryView.waitForInvalidSeedPhraseDisplay();
  });
}, 300_000);
