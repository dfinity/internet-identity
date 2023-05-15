import { FLOWS } from "./flows";
import { addVirtualAuthenticator, runInBrowser, switchToPopup } from "./util";
import { DemoAppView, MainView } from "./views";

import { DEVICE_NAME1, II_URL, TEST_APP_NICE_URL } from "./constants";

test("Device can be renamed", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    await addVirtualAuthenticator(browser);
    await browser.url(II_URL);
    await FLOWS.registerNewIdentityWelcomeView(DEVICE_NAME1, browser);
    const mainView = new MainView(browser);
    await mainView.waitForDeviceDisplay(DEVICE_NAME1);
    const newName = DEVICE_NAME1 + "-new";
    await mainView.rename(DEVICE_NAME1, newName);
    await mainView.waitForDisplay();
    await mainView.waitForDeviceDisplay(newName);
  });
}, 300_000);

test("Should show dapp logo for known dapp", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    const niceDemoAppView = new DemoAppView(browser);
    await niceDemoAppView.open(TEST_APP_NICE_URL, II_URL);
    await niceDemoAppView.waitForDisplay();
    await niceDemoAppView.signin();

    await switchToPopup(browser);
    await browser.$('[data-role="known-dapp-image"]').waitForExist();
  });
}, 300_000);
