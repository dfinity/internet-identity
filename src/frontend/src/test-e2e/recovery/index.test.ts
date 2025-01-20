import { FLOWS } from "../flows";
import { addVirtualAuthenticator, runInBrowser } from "../util";
import { MainView } from "../views";

import { DEVICE_NAME1, II_URL } from "../constants";

test("Recover with phrase", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    await addVirtualAuthenticator(browser);
    await browser.url(II_URL);
    const _userNumber = await FLOWS.registerNewIdentityWelcomeView(browser);
    const mainView = new MainView(browser);
    await mainView.waitForDeviceDisplay(DEVICE_NAME1);
    const seedPhrase = await FLOWS.addRecoveryMechanismSeedPhrase(browser);
    await mainView.waitForDisplay();
    await mainView.logout();

    await FLOWS.recoverUsingSeedPhrase(browser, seedPhrase);
    await mainView.waitForDeviceDisplay(DEVICE_NAME1);
  });
}, 300_000);

test("Recover with device", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    await addVirtualAuthenticator(browser);
    await browser.url(II_URL);
    const _userNumber = await FLOWS.registerNewIdentityWelcomeView(browser);
    const mainView = new MainView(browser);
    await mainView.waitForDeviceDisplay(DEVICE_NAME1);
    await FLOWS.addRecoveryMechanismDevice(browser);
    await mainView.waitForDisplay();
    await mainView.logout();

    await FLOWS.recoverUsingDevice(browser);
    await mainView.waitForDisplay();
    await mainView.waitForDeviceDisplay(DEVICE_NAME1);
  });
}, 300_000);

test("Recover with both phrase and device", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    await addVirtualAuthenticator(browser);
    await browser.url(II_URL);
    const _userNumber = await FLOWS.registerNewIdentityWelcomeView(browser);
    const mainView = new MainView(browser);
    await mainView.waitForDeviceDisplay(DEVICE_NAME1);
    const seedPhrase = await FLOWS.addRecoveryMechanismSeedPhrase(browser);
    await mainView.waitForDisplay();
    await FLOWS.addRecoveryMechanismDevice(browser);
    await mainView.waitForDisplay();
    await mainView.logout();

    await FLOWS.recoverUsingSeedPhrase(browser, seedPhrase);
    await mainView.waitForDeviceDisplay(DEVICE_NAME1);

    await mainView.logout();

    await FLOWS.recoverUsingDevice(browser);
    await mainView.waitForDeviceDisplay(DEVICE_NAME1);
  });
}, 300_000);
