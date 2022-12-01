import {
  AddDeviceAliasView,
  AddDeviceFlowSelectorView,
  AddIdentityAnchorView,
  AddRemoteDeviceAliasView,
  AddRemoteDeviceInstructionsView,
  AddRemoteDeviceVerificationCodeView,
  AuthenticateView,
  DemoAppView,
  MainView,
  NotInRegistrationModeView,
  RecoveryMethodSelectorView,
  SingleDeviceWarningView,
  VerifyRemoteDeviceView,
  WelcomeView,
} from "./views";
import { FLOWS } from "./flows";
import {
  addVirtualAuthenticator,
  addWebAuthnCredential,
  focusBrowser,
  getWebAuthnCredentials,
  originToRelyingPartyId,
  removeVirtualAuthenticator,
  runInBrowser,
  switchToPopup,
  waitToClose,
} from "./util";

// Read canister ids from the corresponding dfx files.
// This assumes that they have been successfully dfx-deployed
import test_app_canister_ids from "../../../../demos/test-app/.dfx/local/canister_ids.json";
import ii_canister_ids from "../../../../.dfx/local/canister_ids.json";

const TEST_APP_CANISTER_ID = test_app_canister_ids.test_app.local;
const TEST_APP_CANONICAL_URL = `https://${TEST_APP_CANISTER_ID}.ic0.app`;
const TEST_APP_NICE_URL = "https://nice-name.com";
const REPLICA_URL = "https://ic0.app";
const II_URL = "https://identity.ic0.app";
const FAQ_URL = `${II_URL}/faq`;
const ABOUT_URL = `${II_URL}/about`;

const DEVICE_NAME1 = "Virtual WebAuthn device";
const DEVICE_NAME2 = "Other WebAuthn device";

test("Register new identity and login with it", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    await browser.url(II_URL);
    const welcomeView = new WelcomeView(browser);
    await welcomeView.waitForDisplay();
    await welcomeView.register();
    await addVirtualAuthenticator(browser);
    await browser.url(II_URL);
    const userNumber = await FLOWS.registerNewIdentityWelcomeView(
      DEVICE_NAME1,
      browser
    );
    const mainView = new MainView(browser);
    await mainView.waitForDeviceDisplay(DEVICE_NAME1);
    await mainView.logout();
    await FLOWS.login(userNumber, DEVICE_NAME1, browser);
  });
}, 300_000);

test("Register new identity and add additional device", async () => {
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
    await mainView.addAdditionalDevice();

    const addDeviceFlowView = new AddDeviceFlowSelectorView(browser);
    await addDeviceFlowView.waitForDisplay();
    await addDeviceFlowView.selectLocalDevice();

    const addDeviceAliasView = new AddDeviceAliasView(browser);
    await addDeviceAliasView.waitForDisplay();
    await addDeviceAliasView.addAdditionalDevice(DEVICE_NAME2);
    await addDeviceAliasView.continue();

    await browser.pause(10_000);

    await mainView.waitForDeviceDisplay(DEVICE_NAME1);
    await mainView.waitForDeviceDisplay(DEVICE_NAME2);

    await mainView.logout();
    await FLOWS.login(userNumber, DEVICE_NAME1, browser);
  });
}, 300_000);

test("Register new identity and add additional remote device", async () => {
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

    const addDeviceFlowView = new AddDeviceFlowSelectorView(browser);
    await addDeviceFlowView.waitForDisplay();
    await addDeviceFlowView.selectRemoteDevice();

    await runInBrowser(async (browser2: WebdriverIO.Browser) => {
      await addVirtualAuthenticator(browser2);
      await browser2.url(II_URL);
      const welcomeView2 = new WelcomeView(browser2);
      await welcomeView2.waitForDisplay();
      await welcomeView2.addDevice();
      const addIdentityAnchorView2 = new AddIdentityAnchorView(browser2);
      await addIdentityAnchorView2.waitForDisplay();
      await addIdentityAnchorView2.continue(userNumber);
      const addRemoteDeviceView = new AddRemoteDeviceAliasView(browser2);
      await addRemoteDeviceView.waitForDisplay();
      await addRemoteDeviceView.selectAlias(DEVICE_NAME2);
      await addRemoteDeviceView.continue();

      const verificationCodeView =
        await new AddRemoteDeviceVerificationCodeView(browser2);
      await verificationCodeView.waitForDisplay();
      const code = await verificationCodeView.getVerificationCode();

      // browser 1 again
      await focusBrowser(browser);
      const verificationView = await new VerifyRemoteDeviceView(browser);
      await verificationView.waitForDisplay();
      await verificationView.enterVerificationCode(code);
      await verificationView.continue();
    });

    await mainView.waitForDisplay();
    await mainView.waitForDeviceDisplay(DEVICE_NAME2);
  });
}, 300_000);

test("Register new identity and add additional remote device starting on new device", async () => {
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
      const addRemoteDeviceView = new AddRemoteDeviceAliasView(browser2);
      await addRemoteDeviceView.waitForDisplay();
      await addRemoteDeviceView.selectAlias(DEVICE_NAME2);
      await addRemoteDeviceView.continue();

      const notInRegistrationModeView = new NotInRegistrationModeView(browser2);
      await notInRegistrationModeView.waitForDisplay();

      // browser 1 again
      await focusBrowser(browser);
      await mainView.addAdditionalDevice();
      const addDeviceFlowView = new AddDeviceFlowSelectorView(browser);
      await addDeviceFlowView.waitForDisplay();
      await addDeviceFlowView.selectRemoteDevice();

      const addRemoteDeviceInstructionsView =
        new AddRemoteDeviceInstructionsView(browser);
      await addRemoteDeviceInstructionsView.waitForDisplay();

      // browser 2 again
      await focusBrowser(browser2);
      await notInRegistrationModeView.retry();
      const verificationCodeView =
        await new AddRemoteDeviceVerificationCodeView(browser2);
      await verificationCodeView.waitForDisplay();
      const code = await verificationCodeView.getVerificationCode();

      // browser 1 again
      await focusBrowser(browser);
      const verificationView = await new VerifyRemoteDeviceView(browser);
      await verificationView.waitForDisplay();
      await verificationView.enterVerificationCode(code);
      await verificationView.continue();
    });

    await mainView.waitForDisplay();
    await mainView.waitForDeviceDisplay(DEVICE_NAME2);
  });
}, 300_000);

test("Log into client application, after registration", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    await addVirtualAuthenticator(browser);
    const demoAppView = new DemoAppView(browser);
    await demoAppView.open(TEST_APP_NICE_URL, II_URL);
    await demoAppView.waitForDisplay();
    expect(await demoAppView.getPrincipal()).toBe("2vxsx-fae");
    await demoAppView.signin();
    await switchToPopup(browser);
    await FLOWS.registerNewIdentityAuthenticateView(DEVICE_NAME1, browser);
    await waitToClose(browser);
    await demoAppView.waitForDisplay();
    const principal = await demoAppView.getPrincipal();
    expect(principal).not.toBe("2vxsx-fae");

    expect(await demoAppView.whoami(REPLICA_URL, TEST_APP_CANISTER_ID)).toBe(
      principal
    );

    // default value
    const exp = await browser.$("#expiration").getText();
    expect(Number(exp) / (8 * 60 * 60_000_000_000)).toBeCloseTo(1);
  });
}, 300_000);

test("Register first then log into client application", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    const authenticatorId1 = await addVirtualAuthenticator(browser);

    await browser.url(II_URL);
    const userNumber = await FLOWS.registerNewIdentityWelcomeView(
      DEVICE_NAME1,
      browser
    );

    const credentials = await getWebAuthnCredentials(browser, authenticatorId1);
    expect(credentials).toHaveLength(1);

    const demoAppView = new DemoAppView(browser);
    await demoAppView.open(TEST_APP_NICE_URL, II_URL);
    await demoAppView.waitForDisplay();
    expect(await demoAppView.getPrincipal()).toBe("2vxsx-fae");
    await demoAppView.signin();

    const authenticatorId2 = await switchToPopup(browser);
    await addWebAuthnCredential(
      browser,
      authenticatorId2,
      credentials[0],
      originToRelyingPartyId(II_URL)
    );

    const authenticateView = new AuthenticateView(browser);
    await authenticateView.waitForDisplay();
    await authenticateView.pickAnchor(userNumber);
    const recoveryMethodSelectorView = new RecoveryMethodSelectorView(browser);
    await recoveryMethodSelectorView.waitForDisplay();
    await recoveryMethodSelectorView.skipRecovery();
    const singleDeviceWarningView = new SingleDeviceWarningView(browser);
    await singleDeviceWarningView.waitForDisplay();
    await singleDeviceWarningView.remindLater();
    await waitToClose(browser);
    await demoAppView.waitForDisplay();
    const principal = await demoAppView.getPrincipal();
    expect(principal).not.toBe("2vxsx-fae");

    expect(await demoAppView.whoami(REPLICA_URL, TEST_APP_CANISTER_ID)).toBe(
      principal
    );

    // default value
    const exp = await browser.$("#expiration").getText();
    expect(Number(exp) / (8 * 60 * 60_000_000_000)).toBeCloseTo(1);
  });
}, 300_000);
