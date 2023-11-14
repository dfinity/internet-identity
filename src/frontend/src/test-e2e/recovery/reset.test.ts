import { FLOWS } from "../flows";
import { addVirtualAuthenticator, runInBrowser } from "../util";
import { MainView } from "../views";

import { DEVICE_NAME1, II_URL, RECOVERY_PHRASE_NAME } from "../constants";

test("Reset recovery phrase", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    await addVirtualAuthenticator(browser);
    await browser.url(II_URL);
    await FLOWS.registerNewIdentityWelcomeView(DEVICE_NAME1, browser);
    const mainView = new MainView(browser);
    await mainView.waitForDeviceDisplay(DEVICE_NAME1);
    await FLOWS.addRecoveryMechanismSeedPhrase(browser);
    await mainView.waitForDisplay();

    await mainView.waitForRecoveryDisplay(RECOVERY_PHRASE_NAME);

    // Ensure the settings dropdown is in view
    await browser.execute("window.scrollTo(0, document.body.scrollHeight)");
    await mainView.reset(RECOVERY_PHRASE_NAME);

    const _seedPhrase = await FLOWS.readSeedPhrase(browser);
    await mainView.waitForDisplay();
  });
}, 300_000);

test("Recover access, after reset", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    await addVirtualAuthenticator(browser);
    await browser.url(II_URL);
    const _userNumber = await FLOWS.registerNewIdentityWelcomeView(
      DEVICE_NAME1,
      browser
    );
    const mainView = new MainView(browser);
    await mainView.waitForDeviceDisplay(DEVICE_NAME1);

    // Create a phrase
    const _seedPhrase = await FLOWS.addRecoveryMechanismSeedPhrase(browser);
    await mainView.waitForDisplay();

    // Reset the phrase
    // Ensure the settings dropdown is in view
    await browser.execute("window.scrollTo(0, document.body.scrollHeight)");
    await mainView.reset(RECOVERY_PHRASE_NAME);

    const seedPhrase = await FLOWS.readSeedPhrase(browser);
    await mainView.waitForDisplay();

    // Logout
    await mainView.logout();

    // Recover with new phrase
    await FLOWS.recoverUsingSeedPhrase(browser, seedPhrase);
    await mainView.waitForDeviceDisplay(DEVICE_NAME1);
  });
}, 300_000);

test("Canceling reset keeps old phrase", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    await addVirtualAuthenticator(browser);
    await browser.url(II_URL);
    const _userNumber = await FLOWS.registerNewIdentityWelcomeView(
      DEVICE_NAME1,
      browser
    );
    const mainView = new MainView(browser);
    await mainView.waitForDeviceDisplay(DEVICE_NAME1);

    // Create a phrase
    const seedPhrase = await FLOWS.addRecoveryMechanismSeedPhrase(browser);
    await mainView.waitForDisplay();

    // Reset the phrase
    // Ensure the settings dropdown is in view
    await browser.execute("window.scrollTo(0, document.body.scrollHeight)");
    await mainView.reset(RECOVERY_PHRASE_NAME);

    // Instead of reading the new seed phrase, cancel the flow
    await browser.$(`button[data-action='cancel']`).click();
    await mainView.waitForDisplay();

    // Logout
    await mainView.logout();

    // Recover with old phrase
    await FLOWS.recoverUsingSeedPhrase(browser, seedPhrase);
    await mainView.waitForDeviceDisplay(DEVICE_NAME1);
  });
}, 300_000);

test("Reset unprotected recovery phrase, when authenticated with phrase", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    await addVirtualAuthenticator(browser);
    await browser.url(II_URL);
    const _userNumber = await FLOWS.registerNewIdentityWelcomeView(
      DEVICE_NAME1,
      browser
    );
    const mainView = new MainView(browser);
    await mainView.waitForDeviceDisplay(DEVICE_NAME1);
    const seedPhrase = await FLOWS.addRecoveryMechanismSeedPhrase(browser);
    await mainView.waitForDisplay();
    await mainView.logout();

    await FLOWS.recoverUsingSeedPhrase(browser, seedPhrase);
    await mainView.waitForDeviceDisplay(DEVICE_NAME1);

    // Ensure the settings dropdown is in view
    await browser.execute("window.scrollTo(0, document.body.scrollHeight)");
    await mainView.reset(RECOVERY_PHRASE_NAME);

    const _seedPhrase = await FLOWS.readSeedPhrase(browser);
    await mainView.waitForDisplay();
  });
}, 300_000);
