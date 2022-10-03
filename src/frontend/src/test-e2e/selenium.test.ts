import {
  AboutView,
  AddDeviceAliasView,
  AddDeviceFlowSelectorView,
  AddIdentityAnchorView,
  AddRemoteDeviceAliasView,
  AddRemoteDeviceInstructionsView,
  AddRemoteDeviceVerificationCodeView,
  AuthenticateView,
  CompatabilityNoticeView,
  DemoAppView,
  DeviceSettingsView,
  ErrorView,
  FAQView,
  MainView,
  NotInRegistrationModeView,
  RecoverView,
  RecoveryMethodSelectorView,
  RegisterView,
  SingleDeviceWarningView,
  VerifyRemoteDeviceView,
  WelcomeBackView,
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

test("Delegation maxTimeToLive: 1 min", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    await addVirtualAuthenticator(browser);
    const demoAppView = new DemoAppView(browser);
    await demoAppView.open(TEST_APP_NICE_URL, II_URL);
    await demoAppView.waitForDisplay();
    expect(await demoAppView.getPrincipal()).toBe("2vxsx-fae");
    await demoAppView.setMaxTimeToLive(BigInt(60_000_000_000));
    await demoAppView.signin();
    await switchToPopup(browser);
    await FLOWS.registerNewIdentityAuthenticateView(DEVICE_NAME1, browser);
    await waitToClose(browser);
    await demoAppView.waitForDisplay();
    expect(await demoAppView.getPrincipal()).not.toBe("2vxsx-fae");
    const exp = await browser.$("#expiration").getText();
    // compare only up to one decimal place for the 1min test
    expect(Number(exp) / 60_000_000_000).toBeCloseTo(1, 0);
  });
}, 300_000);

test("Delegation maxTimeToLive: 1 day", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    await addVirtualAuthenticator(browser);
    const demoAppView = new DemoAppView(browser);
    await demoAppView.open(TEST_APP_NICE_URL, II_URL);
    await demoAppView.waitForDisplay();
    expect(await demoAppView.getPrincipal()).toBe("2vxsx-fae");
    await demoAppView.setMaxTimeToLive(BigInt(86400_000_000_000));
    await demoAppView.signin();
    await switchToPopup(browser);
    await FLOWS.registerNewIdentityAuthenticateView(DEVICE_NAME1, browser);
    await waitToClose(browser);
    expect(await demoAppView.getPrincipal()).not.toBe("2vxsx-fae");
    const exp = await browser.$("#expiration").getText();
    expect(Number(exp) / 86400_000_000_000).toBeCloseTo(1);
  });
}, 300_000);

test("Delegation maxTimeToLive: 2 months", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    await addVirtualAuthenticator(browser);
    const demoAppView = new DemoAppView(browser);
    await demoAppView.open(TEST_APP_NICE_URL, II_URL);
    await demoAppView.waitForDisplay();
    expect(await demoAppView.getPrincipal()).toBe("2vxsx-fae");
    await demoAppView.setMaxTimeToLive(BigInt(5_184_000_000_000_000)); // 60 days
    await demoAppView.signin();
    await switchToPopup(browser);
    await FLOWS.registerNewIdentityAuthenticateView(DEVICE_NAME1, browser);
    await waitToClose(browser);
    expect(await demoAppView.getPrincipal()).not.toBe("2vxsx-fae");
    const exp = await browser.$("#expiration").getText();
    // NB: Max out at 30 days
    expect(Number(exp) / 2_592_000_000_000_000).toBeCloseTo(1);
  });
}, 300_000);

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

    const recoveryAlias = "Recovery phrase";
    await mainView.waitForDeviceDisplay(recoveryAlias);
    await mainView.deviceSettings(recoveryAlias);

    const settingsView = new DeviceSettingsView(browser);
    await settingsView.waitForDisplay();
    await settingsView.remove();
    await browser.acceptAlert();

    await mainView.waitForDeviceNotDisplay(recoveryAlias);
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

    const recoveryAlias = "Recovery phrase";
    await mainView.deviceSettings(recoveryAlias);

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

    const recoveryAlias = "Recovery phrase";
    await mainView.deviceSettings(recoveryAlias);

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
    await mainView.waitForDeviceNotDisplay(recoveryAlias);
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

    const recoveryAlias = "Recovery phrase";
    await mainView.deviceSettings(recoveryAlias);

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
    await authenticateView.expectPrefilledAnchorToBe(userNumber);
    await authenticateView.authenticate();
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

test("Authorize ready message should be sent immediately", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    await addVirtualAuthenticator(browser);
    const demoAppView = new DemoAppView(browser);
    await demoAppView.open(TEST_APP_NICE_URL, II_URL);
    await demoAppView.waitForDisplay();
    await demoAppView.openIiTab();
    await demoAppView.waitForNthMessage(1);
    expect(await demoAppView.getMessageText(1)).toContain("authorize-ready");
  });
}, 300_000);

test("Should ignore invalid data and allow second valid message", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    await addVirtualAuthenticator(browser);
    const demoAppView = new DemoAppView(browser);
    await demoAppView.open(TEST_APP_NICE_URL, II_URL);
    await demoAppView.waitForDisplay();
    await demoAppView.openIiTab();
    // switch back to demo app
    await browser.switchToWindow((await browser.getWindowHandles())[0]);
    await demoAppView.waitForNthMessage(1); // message 1: authorize-ready
    await demoAppView.sendInvalidData(); // message 2
    await demoAppView.sendValidMessage(); // message 3

    await switchToPopup(browser);
    await FLOWS.registerNewIdentityAuthenticateView(DEVICE_NAME1, browser);

    // switch back to demo app
    await browser.switchToWindow((await browser.getWindowHandles())[0]);

    await demoAppView.waitForNthMessage(4); // message 4: authorize-success
    const successMessage = await demoAppView.getMessageText(4);
    expect(successMessage).toContain("authorize-client-success");

    // Internet Identity default value (as opposed to agent-js)
    const exp = await browser.$("#expiration").getText();
    expect(Number(exp) / (30 * 60_000_000_000)).toBeCloseTo(1);
  });
}, 300_000);

test("Should issue the same principal to nice url and canonical url", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    const authenticatorId1 = await addVirtualAuthenticator(browser);
    await browser.url(II_URL);
    const userNumber = await FLOWS.registerNewIdentityWelcomeView(
      DEVICE_NAME1,
      browser
    );
    await FLOWS.addRecoveryMechanismSeedPhrase(browser);
    const credentials = await getWebAuthnCredentials(browser, authenticatorId1);
    expect(credentials).toHaveLength(1);

    const canonicalDemoAppView = new DemoAppView(browser);
    await canonicalDemoAppView.open(TEST_APP_CANONICAL_URL, II_URL);
    await canonicalDemoAppView.waitForDisplay();
    await canonicalDemoAppView.setDerivationOrigin(TEST_APP_CANONICAL_URL);
    expect(await canonicalDemoAppView.getPrincipal()).toBe("2vxsx-fae");
    await canonicalDemoAppView.signin();

    const authenticatorId2 = await switchToPopup(browser);
    await addWebAuthnCredential(
      browser,
      authenticatorId2,
      credentials[0],
      originToRelyingPartyId(II_URL)
    );
    let authenticateView = new AuthenticateView(browser);
    await authenticateView.waitForDisplay();
    await authenticateView.expectPrefilledAnchorToBe(userNumber);
    await authenticateView.authenticate();
    await waitToClose(browser);

    const principal1 = await canonicalDemoAppView.getPrincipal();

    const niceDemoAppView = new DemoAppView(browser);
    await niceDemoAppView.open(TEST_APP_NICE_URL, II_URL);
    await niceDemoAppView.waitForDisplay();
    await niceDemoAppView.updateAlternativeOrigins(
      REPLICA_URL,
      TEST_APP_CANISTER_ID,
      `{"alternativeOrigins":["${TEST_APP_NICE_URL}"]}`,
      "certified"
    );
    await niceDemoAppView.setDerivationOrigin(TEST_APP_CANONICAL_URL);
    expect(await niceDemoAppView.getPrincipal()).toBe("2vxsx-fae");
    await niceDemoAppView.signin();

    const authenticatorId3 = await switchToPopup(browser);
    await addWebAuthnCredential(
      browser,
      authenticatorId3,
      credentials[0],
      originToRelyingPartyId(II_URL)
    );
    authenticateView = new AuthenticateView(browser);
    await authenticateView.waitForDisplay();
    await authenticateView.expectPrefilledAnchorToBe(userNumber);
    await authenticateView.authenticate();
    await waitToClose(browser);

    const principal2 = await niceDemoAppView.getPrincipal();
    expect(principal1).toEqual(principal2);
  });
}, 300_000);

test("Should not issue delegation when derivationOrigin is missing from /.well-known/ii-alternative-origins", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    const authenticatorId1 = await addVirtualAuthenticator(browser);
    await browser.url(II_URL);
    const userNumber = await FLOWS.registerNewIdentityWelcomeView(
      DEVICE_NAME1,
      browser
    );
    await FLOWS.addRecoveryMechanismSeedPhrase(browser);
    const credentials = await getWebAuthnCredentials(browser, authenticatorId1);
    expect(credentials).toHaveLength(1);

    const niceDemoAppView = new DemoAppView(browser);
    await niceDemoAppView.open(TEST_APP_NICE_URL, II_URL);
    await niceDemoAppView.waitForDisplay();
    await niceDemoAppView.resetAlternativeOrigins(
      REPLICA_URL,
      TEST_APP_CANISTER_ID
    );
    await niceDemoAppView.setDerivationOrigin(TEST_APP_CANONICAL_URL);
    expect(await niceDemoAppView.getPrincipal()).toBe("2vxsx-fae");
    await niceDemoAppView.signin();

    const authenticatorId3 = await switchToPopup(browser);
    await addWebAuthnCredential(
      browser,
      authenticatorId3,
      credentials[0],
      originToRelyingPartyId(II_URL)
    );
    const errorView = new ErrorView(browser);
    await errorView.waitForDisplay();
    expect(await errorView.getErrorMessage()).toEqual(
      `"${TEST_APP_CANONICAL_URL}" is not a valid derivation origin for "https://nice-name.com"`
    );
    expect(await errorView.getErrorDetail()).toEqual(
      '"https://nice-name.com" is not listed in the list of allowed alternative origins. Allowed alternative origins:'
    );
  });
}, 300_000);

test("Should not issue delegation when derivationOrigin is malformed", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    const authenticatorId1 = await addVirtualAuthenticator(browser);
    await browser.url(II_URL);
    await FLOWS.registerNewIdentityWelcomeView(DEVICE_NAME1, browser);
    await FLOWS.addRecoveryMechanismSeedPhrase(browser);
    const credentials = await getWebAuthnCredentials(browser, authenticatorId1);
    expect(credentials).toHaveLength(1);

    const niceDemoAppView = new DemoAppView(browser);
    await niceDemoAppView.open(TEST_APP_NICE_URL, II_URL);
    await niceDemoAppView.waitForDisplay();
    await niceDemoAppView.resetAlternativeOrigins(
      REPLICA_URL,
      TEST_APP_CANISTER_ID
    );
    await niceDemoAppView.setDerivationOrigin(
      "https://some-random-disallowed-url.com"
    );
    expect(await niceDemoAppView.getPrincipal()).toBe("2vxsx-fae");
    await niceDemoAppView.signin();

    const authenticatorId3 = await switchToPopup(browser);
    await addWebAuthnCredential(
      browser,
      authenticatorId3,
      credentials[0],
      originToRelyingPartyId(II_URL)
    );
    const errorView = new ErrorView(browser);
    await errorView.waitForDisplay();
    expect(await errorView.getErrorMessage()).toEqual(
      '"https://some-random-disallowed-url.com" is not a valid derivation origin for "https://nice-name.com"'
    );
    expect(await errorView.getErrorDetail()).toEqual(
      "derivationOrigin does not match regex /^https:\\/\\/([\\w-]+)(?:\\.raw)?\\.ic0\\.app$/"
    );
  });
}, 300_000);

test("Should not issue delegation when /.well-known/ii-alternative-origins has too many entries", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    const authenticatorId1 = await addVirtualAuthenticator(browser);
    await browser.url(II_URL);
    await FLOWS.registerNewIdentityWelcomeView(DEVICE_NAME1, browser);
    await FLOWS.addRecoveryMechanismSeedPhrase(browser);
    const credentials = await getWebAuthnCredentials(browser, authenticatorId1);
    expect(credentials).toHaveLength(1);

    const niceDemoAppView = new DemoAppView(browser);
    await niceDemoAppView.open(TEST_APP_NICE_URL, II_URL);
    await niceDemoAppView.waitForDisplay();
    await niceDemoAppView.updateAlternativeOrigins(
      REPLICA_URL,
      TEST_APP_CANISTER_ID,
      '{"alternativeOrigins":["https://a0.com", "https://a1.com", "https://a2.com", "https://a3.com", "https://a4.com", "https://a5.com", "https://a6.com", "https://a7.com", "https://a8.com", "https://a9.com", "https://a10.com"]}',
      "certified"
    );
    await niceDemoAppView.setDerivationOrigin(TEST_APP_CANONICAL_URL);
    expect(await niceDemoAppView.getPrincipal()).toBe("2vxsx-fae");
    await niceDemoAppView.signin();

    const authenticatorId3 = await switchToPopup(browser);
    await addWebAuthnCredential(
      browser,
      authenticatorId3,
      credentials[0],
      originToRelyingPartyId(II_URL)
    );
    const errorView = new ErrorView(browser);
    await errorView.waitForDisplay();
    expect(await errorView.getErrorMessage()).toEqual(
      `"${TEST_APP_CANONICAL_URL}" is not a valid derivation origin for "https://nice-name.com"`
    );
    expect(await errorView.getErrorDetail()).toEqual(
      `Resource ${TEST_APP_CANONICAL_URL}/.well-known/ii-alternative-origins has too many entries: To prevent misuse at most 10 alternative origins are allowed.`
    );
  });
}, 300_000);

test("Should not follow redirect returned by /.well-known/ii-alternative-origins", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    const authenticatorId1 = await addVirtualAuthenticator(browser);
    await browser.url(II_URL);
    await FLOWS.registerNewIdentityWelcomeView(DEVICE_NAME1, browser);
    await FLOWS.addRecoveryMechanismSeedPhrase(browser);
    const credentials = await getWebAuthnCredentials(browser, authenticatorId1);
    expect(credentials).toHaveLength(1);

    const niceDemoAppView = new DemoAppView(browser);
    await niceDemoAppView.open(TEST_APP_NICE_URL, II_URL);
    await niceDemoAppView.waitForDisplay();
    await niceDemoAppView.updateAlternativeOrigins(
      REPLICA_URL,
      TEST_APP_CANISTER_ID,
      '{"alternativeOrigins":["https://evil.com"]}',
      "redirect"
    );
    await niceDemoAppView.setDerivationOrigin(TEST_APP_CANONICAL_URL);
    expect(await niceDemoAppView.getPrincipal()).toBe("2vxsx-fae");
    await niceDemoAppView.signin();

    const authenticatorId3 = await switchToPopup(browser);
    await addWebAuthnCredential(
      browser,
      authenticatorId3,
      credentials[0],
      originToRelyingPartyId(II_URL)
    );
    const errorView = new ErrorView(browser);
    await errorView.waitForDisplay();
    expect(await errorView.getErrorMessage()).toEqual(
      `"${TEST_APP_CANONICAL_URL}" is not a valid derivation origin for "https://nice-name.com"`
    );
    expect(await errorView.getErrorDetail()).toEqual(
      `An error occurred while validating the derivationOrigin "${TEST_APP_CANONICAL_URL}": Failed to fetch`
    );
  });
}, 300_000);

test("Should fetch /.well-known/ii-alternative-origins using the non-raw url", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    const authenticatorId1 = await addVirtualAuthenticator(browser);
    await browser.url(II_URL);
    const userNumber = await FLOWS.registerNewIdentityWelcomeView(
      DEVICE_NAME1,
      browser
    );
    await FLOWS.addRecoveryMechanismSeedPhrase(browser);
    const credentials = await getWebAuthnCredentials(browser, authenticatorId1);
    expect(credentials).toHaveLength(1);

    const niceDemoAppView = new DemoAppView(browser);
    await niceDemoAppView.open(TEST_APP_NICE_URL, II_URL);
    await niceDemoAppView.waitForDisplay();
    await niceDemoAppView.updateAlternativeOrigins(
      REPLICA_URL,
      TEST_APP_CANISTER_ID,
      `{"alternativeOrigins":["${TEST_APP_NICE_URL}"]}`,
      "certified"
    );
    await niceDemoAppView.setDerivationOrigin(
      `https://${TEST_APP_CANISTER_ID}.raw.ic0.app`
    );
    expect(await niceDemoAppView.getPrincipal()).toBe("2vxsx-fae");
    await niceDemoAppView.signin();

    const authenticatorId2 = await switchToPopup(browser);
    await addWebAuthnCredential(
      browser,
      authenticatorId2,
      credentials[0],
      originToRelyingPartyId(II_URL)
    );

    // Selenium has _no_ connectivity to the raw url
    await browser.execute(
      `console.log(await fetch("https://${TEST_APP_CANISTER_ID}.raw.ic0.app/.well-known/ii-alternative-origins"))`
    );
    let logs = (await browser.getLogs("browser")) as { message: string }[];
    expect(logs[logs.length - 1].message).toEqual(
      `https://${TEST_APP_CANISTER_ID}.raw.ic0.app/.well-known/ii-alternative-origins - Failed to load resource: the server responded with a status of 400 (Bad Request)`
    );

    // This works anyway --> fetched using non-raw
    const authenticateView = new AuthenticateView(browser);
    await authenticateView.waitForDisplay();
    await authenticateView.expectPrefilledAnchorToBe(userNumber);
    await authenticateView.authenticate();
    await waitToClose(browser);

    expect(await niceDemoAppView.getPrincipal()).not.toBe("2vxsx-fae");
  });
}, 300_000);
