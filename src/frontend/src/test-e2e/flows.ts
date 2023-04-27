import { II_URL } from "./constants";
import { addVirtualAuthenticator, focusBrowser } from "./util";
import {
  AddDeviceSuccessView,
  AddRemoteDeviceAliasView,
  AddRemoteDeviceInstructionsView,
  AddRemoteDeviceVerificationCodeView,
  AuthenticateView,
  MainView,
  RecoveryMethodSelectorView,
  RegisterView,
  SingleDeviceWarningView,
  VerifyRemoteDeviceView,
  WelcomeView,
} from "./views";

export const FLOWS = {
  register: async function (
    browser: WebdriverIO.Browser,
    deviceName: string
  ): Promise<string> {
    const registerView = new RegisterView(browser);
    await registerView.waitForDisplay();
    await registerView.enterAlias(deviceName);
    await registerView.create();
    await registerView.waitForRegisterConfirm();
    await registerView.confirmRegisterConfirm();
    await registerView.waitForIdentity();
    const userNumber = await registerView.registerGetIdentity();
    await registerView.registerConfirmIdentity();
    const recoveryMethodSelectorView = new RecoveryMethodSelectorView(browser);
    await recoveryMethodSelectorView.waitForDisplay();
    await recoveryMethodSelectorView.skipRecovery();
    const singleDeviceWarningView = new SingleDeviceWarningView(browser);
    await singleDeviceWarningView.waitForDisplay();
    await singleDeviceWarningView.remindLater();
    return userNumber;
  },
  registerNewIdentityWelcomeView: async (
    deviceName: string,
    browser: WebdriverIO.Browser
  ): Promise<string> => {
    const welcomeView = new WelcomeView(browser);
    await welcomeView.waitForDisplay();
    await welcomeView.register();
    return await FLOWS.register(browser, deviceName);
  },
  registerNewIdentityAuthenticateView: async (
    deviceName: string,
    browser: WebdriverIO.Browser
  ): Promise<string> => {
    const authenticateView = new AuthenticateView(browser);
    await authenticateView.waitForDisplay();
    await authenticateView.register();
    return await FLOWS.register(browser, deviceName);
  },
  login: async (
    userNumber: string,
    deviceName: string,
    browser: WebdriverIO.Browser
  ): Promise<void> => {
    const welcomeView = new WelcomeView(browser);
    await welcomeView.waitForDisplay();
    await welcomeView.login();
    await welcomeView.typeUserNumber(userNumber);
    await browser.$("button[data-action='continue']").click();
    // NOTE: depending on the browser, we issue different warnings. On Safari,
    // the warning comes before the recovery method selector. Since we only
    // test on Chrome we always expect the recovery selector first.
    const recoveryMethodSelectorView = new RecoveryMethodSelectorView(browser);
    await recoveryMethodSelectorView.waitForDisplay();
    await recoveryMethodSelectorView.skipRecovery();
    const singleDeviceWarningView = new SingleDeviceWarningView(browser);
    await singleDeviceWarningView.waitForDisplay();
    await singleDeviceWarningView.remindLater();
    const mainView = new MainView(browser);
    await mainView.waitForDeviceDisplay(deviceName);
  },
  addRecoveryMechanismSeedPhrase: async (
    browser: WebdriverIO.Browser
  ): Promise<string> => {
    const mainView = new MainView(browser);
    await mainView.waitForDisplay();
    await mainView.addRecovery();

    const recoveryMethodSelectorView = new RecoveryMethodSelectorView(browser);
    const seedPhrase = await recoveryMethodSelectorView.getSeedPhrase();
    await recoveryMethodSelectorView.acknowledgeCheckbox();
    await recoveryMethodSelectorView.seedPhraseContinue();
    await recoveryMethodSelectorView.seedPhraseFill();

    // Wait for the main view to be displayed again to ensure that the recovery
    // mechanism was added successfully.
    await mainView.waitForDisplay();

    return seedPhrase;
  },
  readSeedPhrase: async (browser: WebdriverIO.Browser): Promise<string> => {
    const recoveryMethodSelectorView = new RecoveryMethodSelectorView(browser);
    await recoveryMethodSelectorView.waitForSeedPhrase();
    const seedPhrase = await recoveryMethodSelectorView.getSeedPhrase();
    await recoveryMethodSelectorView.acknowledgeCheckbox();
    await recoveryMethodSelectorView.seedPhraseContinue();
    await recoveryMethodSelectorView.seedPhraseFill();

    return seedPhrase;
  },
};

export const initDeviceLink = async ({
  browser,
  deviceName,
}: {
  browser: WebdriverIO.Browser;
  deviceName: string;
}): Promise<{
  mainView: MainView;
  addDeviceLink: string;
}> => {
  await addVirtualAuthenticator(browser);
  await browser.url(II_URL);
  const userNumber = await FLOWS.registerNewIdentityWelcomeView(
    deviceName,
    browser
  );
  const mainView = new MainView(browser);
  await mainView.waitForDeviceDisplay(deviceName);
  await mainView.addAdditionalDevice();

  const addRemoteDeviceInstructionsView = new AddRemoteDeviceInstructionsView(
    browser
  );
  const addDeviceLink = await addRemoteDeviceInstructionsView.addDeviceLink();

  return {
    mainView,
    addDeviceLink,
  };
};
export const registerRemoteDevice = async ({
  browser1,
  browser2,
  addDeviceLink,
  deviceName,
}: {
  browser1: WebdriverIO.Browser;
  browser2: WebdriverIO.Browser;
  addDeviceLink: string;
  deviceName: string;
}) => {
  await addVirtualAuthenticator(browser2);
  await browser2.url(addDeviceLink);
  const addRemoteDeviceView = new AddRemoteDeviceAliasView(browser2);
  await addRemoteDeviceView.waitForDisplay();
  await addRemoteDeviceView.selectAlias(deviceName);
  await addRemoteDeviceView.continue();

  const verificationCodeView = new AddRemoteDeviceVerificationCodeView(
    browser2
  );
  await verificationCodeView.waitForDisplay();
  const code = await verificationCodeView.getVerificationCode();

  // browser 1 again
  await focusBrowser(browser1);
  const verificationView = new VerifyRemoteDeviceView(browser1);
  await verificationView.waitForDisplay();
  await verificationView.enterVerificationCode(code);
  await verificationView.continue();

  // success page
  const addDeviceSuccessView = new AddDeviceSuccessView(browser1);
  await addDeviceSuccessView.waitForDisplay();
  await addDeviceSuccessView.continue();
};
