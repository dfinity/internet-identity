import {
  addVirtualAuthenticator,
  createActor,
  runInBrowser,
  setDomainCompatibilityFeatureFlag,
} from "./util";

// Read canister ids from the corresponding dfx files.
// This assumes that they have been successfully dfx-deployed
import { FLOWS } from "$src/test-e2e/flows";
import { MainView } from "$src/test-e2e/views";
import { DEVICE_NAME1, II_URL } from "./constants";

test("Sign in on related origins", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    // Register on main (current) origin
    await addVirtualAuthenticator(browser);
    await browser.url(II_URL);
    const userNumber = await FLOWS.registerNewIdentityWelcomeView(browser);
    const mainView = new MainView(browser);
    await mainView.waitForDisplay();
    await mainView.logout();

    // Get related origins
    const actor = await createActor(browser);
    const config = await actor.config();
    const relatedOrigins = config.related_origins[0] ?? [II_URL];

    // Sign in on each related origin
    for (const relatedOrigin of relatedOrigins) {
      await browser.url(relatedOrigin);
      await setDomainCompatibilityFeatureFlag(browser, true);
      await FLOWS.loginExistingAuthenticateView(
        userNumber,
        DEVICE_NAME1,
        browser
      );
      await mainView.logout();
    }
  });
}, 300_000);
