import { FLOWS } from "../flows";
import { addVirtualAuthenticator, runInBrowser } from "../util";
import { MainView } from "../views";

import { DEVICE_NAME1, II_URL } from "../constants";

test("Recover with phrase", async () => {
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
  });
}, 300_000);
