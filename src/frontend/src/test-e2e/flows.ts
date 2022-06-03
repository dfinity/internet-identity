import {
  AuthenticateView,
  MainView,
  RecoveryMethodSelectorView,
  RegisterView,
  SingleDeviceWarningView,
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
    await authenticateView.expectAnchorInputField();
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
    await welcomeView.typeUserNumber(userNumber);
    await welcomeView.login();
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
    await recoveryMethodSelectorView.waitForDisplay();
    await recoveryMethodSelectorView.useSeedPhrase();
    await recoveryMethodSelectorView.waitForSeedPhrase();
    const seedPhrase = await recoveryMethodSelectorView.getSeedPhrase();
    await recoveryMethodSelectorView.copySeedPhrase();
    await recoveryMethodSelectorView.seedPhraseContinue();

    return seedPhrase;
  },
};
