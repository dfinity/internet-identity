import { FLOWS } from "./flows";
import {
  addVirtualAuthenticator,
  focusBrowser,
  removeVirtualAuthenticator,
  runInBrowser,
} from "./util";
import {
  AddDeviceSuccessView,
  AddIdentityAnchorView,
  AddRemoteDeviceInstructionsView,
  AddRemoteDeviceVerificationCodeView,
  MainView,
  NotInRegistrationModeView,
  VerifyRemoteDeviceView,
  WelcomeView,
} from "./views";

// Read canister ids from the corresponding dfx files.
// This assumes that they have been successfully dfx-deployed
import { readFileSync } from "fs";
import { DEVICE_NAME1, II_URL } from "./constants";
export const test_app_canister_ids = JSON.parse(
  readFileSync("./demos/test-app/.dfx/local/canister_ids.json", "utf-8")
);

const TEST_APP_CANISTER_ID = test_app_canister_ids.test_app.local;
const TEST_APP_CANONICAL_URL = `https://${TEST_APP_CANISTER_ID}.ic0.app`;
const TEST_APP_NICE_URL = "https://nice-name.com";

test("Add device", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    const firstAuthenticator = await addVirtualAuthenticator(browser);
    await browser.url(II_URL);
    const userNumber = await FLOWS.registerNewIdentityWelcomeView(
      DEVICE_NAME1,
      browser
    );

    const mainView = new MainView(browser);
    await mainView.waitForDeviceDisplay(DEVICE_NAME1);
    // We're removing the first authenticator here, because unfortunately we
    // can't tell Chrome to _actually_ use the second authenticator, which
    // leads to flaky tests otherwise.
    await removeVirtualAuthenticator(browser, firstAuthenticator);
    await addVirtualAuthenticator(browser);
    await FLOWS.addFidoDevice(browser);

    // home
    await mainView.waitForDisplay();
    // Expect a second device with the default name
    await mainView.waitForDeviceCount(DEVICE_NAME1, 2);

    await mainView.logout();
    await FLOWS.login(userNumber, DEVICE_NAME1, browser);
  });
}, 300_000);

test("Add remote device", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    await addVirtualAuthenticator(browser);
    await browser.url(II_URL);
    const userNumber = await FLOWS.registerNewIdentityWelcomeView(
      DEVICE_NAME1,
      browser
    );
    const mainView = new MainView(browser);
    await mainView.waitForDeviceDisplay(DEVICE_NAME1);
    await mainView.addAdditionalDevice();

    const addRemoteDeviceInstructionsView = new AddRemoteDeviceInstructionsView(
      browser
    );
    const addDeviceLink = await addRemoteDeviceInstructionsView.addDeviceLink();

    await runInBrowser(async (browser2: WebdriverIO.Browser) => {
      await addVirtualAuthenticator(browser2);
      await browser2.url(addDeviceLink);

      const verificationCodeView = new AddRemoteDeviceVerificationCodeView(
        browser2
      );
      await verificationCodeView.waitForDisplay();
      const code = await verificationCodeView.getVerificationCode();

      // browser 1 again
      await focusBrowser(browser);
      const verificationView = new VerifyRemoteDeviceView(browser);
      await verificationView.waitForDisplay();
      await verificationView.enterVerificationCode(code);

      // Verify success on Browser 1

      // success page
      const addDeviceSuccessView = new AddDeviceSuccessView(browser);
      await addDeviceSuccessView.waitForDisplay();
      await addDeviceSuccessView.continue();

      await mainView.waitForDisplay();
      // Expect a second device with the default name
      await mainView.waitForDeviceCount(DEVICE_NAME1, 2);

      // Verify success on Browser 2
      // browser 2 again
      await focusBrowser(browser2);

      // add authenticator because we will sign in on continue
      // await addVirtualAuthenticator(browser2);

      // success page
      const addDeviceSuccessView2 = new AddDeviceSuccessView(browser2);
      await addDeviceSuccessView2.waitForDisplay();
      await addDeviceSuccessView2.continue();

      // main page signed-in
      const mainView2 = new MainView(browser2);
      // Expect a second device with the default name
      await mainView.waitForDeviceCount(DEVICE_NAME1, 2);
    });
  });
}, 300_000);

test("Add remote device starting on new device", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    await addVirtualAuthenticator(browser);
    await browser.url(II_URL);
    const userNumber = await FLOWS.registerNewIdentityWelcomeView(
      DEVICE_NAME1,
      browser
    );
    const mainView = new MainView(browser);
    await mainView.waitForDeviceDisplay(DEVICE_NAME1);

    await runInBrowser(async (browser2: WebdriverIO.Browser) => {
      await addVirtualAuthenticator(browser2);
      await browser2.url(II_URL);
      const welcomeView2 = new WelcomeView(browser2);
      await welcomeView2.waitForDisplay();
      await welcomeView2.addDevice();
      const addIdentityAnchorView2 = new AddIdentityAnchorView(browser2);
      await addIdentityAnchorView2.waitForDisplay();
      await addIdentityAnchorView2.continue(userNumber);

      const notInRegistrationModeView = new NotInRegistrationModeView(browser2);
      await notInRegistrationModeView.waitForDisplay();

      // browser 1 again
      await focusBrowser(browser);
      await mainView.addAdditionalDevice();

      const addRemoteDeviceInstructionsView =
        new AddRemoteDeviceInstructionsView(browser);
      await addRemoteDeviceInstructionsView.waitForDisplay();

      // browser 2 again
      await focusBrowser(browser2);
      await notInRegistrationModeView.retry();
      const verificationCodeView = new AddRemoteDeviceVerificationCodeView(
        browser2
      );
      await verificationCodeView.waitForDisplay();
      const code = await verificationCodeView.getVerificationCode();

      // browser 1 again
      await focusBrowser(browser);
      const verificationView = new VerifyRemoteDeviceView(browser);
      await verificationView.waitForDisplay();
      await verificationView.enterVerificationCode(code);

      // success page
      const addDeviceSuccessView = new AddDeviceSuccessView(browser);
      await addDeviceSuccessView.waitForDisplay();
      await addDeviceSuccessView.continue();
    });

    await mainView.waitForDisplay();
    // Expect a second device with the default name
    await mainView.waitForDeviceCount(DEVICE_NAME1, 2);
  });
}, 300_000);
