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
  AuthenticateView,
  MainView,
  NotInRegistrationModeView,
  PromptDeviceTrustedView,
  VerifyRemoteDeviceView,
  WelcomeView,
} from "./views";

// Read canister ids from the corresponding dfx files.
// This assumes that they have been successfully dfx-deployed
import { DEVICE_NAME1, II_URL } from "./constants";

test("Add device", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    const firstAuthenticator = await addVirtualAuthenticator(browser);
    await browser.url(II_URL);
    const userNumber = await FLOWS.registerNewIdentityWelcomeView(browser);
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
    await FLOWS.loginAuthenticateView(userNumber, DEVICE_NAME1, browser);
  });
}, 300_000);

test("Add remote device", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    await addVirtualAuthenticator(browser);
    await browser.url(II_URL);
    const _userNumber = await FLOWS.registerNewIdentityWelcomeView(browser);
    const mainView = new MainView(browser);
    await mainView.waitForDeviceDisplay(DEVICE_NAME1);
    await mainView.addAdditionalDevice();

    const addRemoteDeviceInstructionsView = new AddRemoteDeviceInstructionsView(
      browser,
    );
    const addDeviceLink = await addRemoteDeviceInstructionsView.addDeviceLink();

    await runInBrowser(async (browser2: WebdriverIO.Browser) => {
      await addVirtualAuthenticator(browser2);
      await browser2.url(addDeviceLink);

      const promptDeviceTrustedView = new PromptDeviceTrustedView(browser2);
      await promptDeviceTrustedView.waitForDisplay();
      await promptDeviceTrustedView.confirmTrusted();

      const verificationCodeView = new AddRemoteDeviceVerificationCodeView(
        browser2,
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

      // Expect a second device with the default name
      await mainView.waitForDeviceCount(DEVICE_NAME1, 2);
    });
  });
}, 300_000);

test("Add remote device starting on new device", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    await addVirtualAuthenticator(browser);
    await browser.url(II_URL);
    const userNumber = await FLOWS.registerNewIdentityWelcomeView(browser);
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

      const promptDeviceTrustedView = new PromptDeviceTrustedView(browser2);
      await promptDeviceTrustedView.waitForDisplay();
      await promptDeviceTrustedView.confirmTrusted();

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
        browser2,
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

      // browser 2 again
      const addDeviceSuccessView2 = new AddDeviceSuccessView(browser2);
      await addDeviceSuccessView2.waitForDisplay();
      await addDeviceSuccessView2.continue();

      // make sure the browser now shows the sign-in screen with the user number
      // pre-filled
      await focusBrowser(browser2);
      const authView = new AuthenticateView(browser2);
      await authView.waitForDisplay();
      await authView.expectAnchor(userNumber);
    });

    await mainView.waitForDisplay();
    // Expect a second device with the default name
    await mainView.waitForDeviceCount(DEVICE_NAME1, 2);
  });
}, 300_000);
